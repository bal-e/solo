//! Type-checking for Solo.

use core::iter;
use core::num::NonZeroU32;

use thiserror::Error;
use unionfind::VecUnionFindByRank;

use crate::ast::{self, BinOp, UnaOp, Stored};
use crate::types as ext;

pub mod logic;
use self::logic::*;

pub mod types;
use self::types::*;

/// Type-checking information for a function.
pub struct Function<'ast> {
    /// The AST definition of the function.
    ast: &'ast ast::Function,

    /// The type of every expression in the function.
    types: Vec<ext::StreamType>,
}

impl<'ast> Function<'ast> {
    /// Get the type of an expression in the function.
    pub fn get_expr_type(&self, expr: &Stored<ast::Expr>) -> ext::StreamType {
        self.types
            .get((expr.ident - self.ast.expr_ids.start) as usize)
            .cloned()
            .unwrap()
    }
}

/// Type-check a function.
pub fn tck_fn(r#fn: &ast::Function) -> Result<Function<'_>, Error> {
    // Setup the type-checker.
    let num_vars = r#fn.variable_ids.len();
    let mut storage = Storage {
        ast: r#fn,
        types: iter::repeat(None).take(num_vars).collect(),
        tvars: VecUnionFindByRank::new(0 .. num_vars).unwrap(),
    };

    // Resolve all function arguments.
    for arg in &r#fn.args {
        let ident: usize = arg.variable.expr.ident.try_into().unwrap();
        storage.types[ident] = Some(arg.r#type.clone().into());
    }

    // Resolve the function body.
    let rett = storage.tck_expr(&r#fn.body, ScalarType::Any)?;
    if !rett.is_subtype_of(&r#fn.rett.clone().into()) {
        return Err(logic::Error::Subtype.into());
    }

    // Resolve every expression in the body.
    let types = (0 .. num_vars)
        .map(|i| storage.tvars.find_shorten(&i).unwrap())
        .map(|i| storage.types[i]
             .map(ext::StreamType::try_from)
             .unwrap_or(Err(Error::Unresolvable)))
        .collect::<Result<Vec<_>, Error>>()?;

    Ok(Function { ast: r#fn, types })
}

/// Storage for type-checking.
pub struct Storage<'ast> {
    /// The AST of the function being type-checked.
    ast: &'ast ast::Function,

    /// The type of every expression in the AST.
    ///
    /// If an expression is unresolved, [`None`] is stored; if the scalar part
    /// of the expression is unresolved, then the union-find should be used to
    /// resolve it.
    ///
    /// The indices here correspond to the IDs of the stored expressions whose
    /// types are being inferred.
    types: Vec<Option<MappedType>>,

    /// A union-find structure for equating scalar types.
    ///
    /// The elements / IDs here correspond to the IDs of the stored expressions
    /// whose types are being inferred.
    tvars: VecUnionFindByRank<usize>,
}

impl<'ast> Storage<'ast> {
    /// Type-check a statement.
    fn tck_stmt(
        &mut self,
        stmt: &Stored<ast::Stmt>,
    ) -> Result<(), Error> {
        match &**stmt {
            ast::Stmt::Let(variable) => {
                self.tck_expr(&variable.expr, ScalarType::Any)?;
            },
        }

        Ok(())
    }

    /// Type-check a specific expression.
    ///
    /// Given a type the expression's scalar type must be a subtype of, the
    /// expression's type is resolved and returned.  Note that the type might
    /// not be fully resolved.
    fn tck_expr(
        &mut self,
        expr: &Stored<ast::Expr>,
        sup: ScalarType,
    ) -> Result<MappedType, Error> {
        let r#type = match &**expr {
            ast::Expr::Una(UnaOp::Neg, x) => {
                // Resolve the sub-expression.
                let x_sup = ScalarType::merge_min(ScalarType::Int(None), sup)?;
                self.tck_expr(&x, x_sup)?
            },

            ast::Expr::Una(UnaOp::Not, x) => {
                // Resolve the sub-expression.
                let x_sup = ScalarType::merge_min(ScalarType::Int(None), sup)?;
                self.tck_expr(&x, x_sup)?
            },

            ast::Expr::Bin(
                BinOp::Add | BinOp::Sub
                    | BinOp::Mul | BinOp::Div | BinOp::Rem
                    | BinOp::And | BinOp::IOr | BinOp::XOr
                    | BinOp::ShL | BinOp::ShR,
                [l, r]) => {

                // Resolve the sub-expressions.
                let x_sup = ScalarType::merge_min(ScalarType::Int(None), sup)?;
                let lhs = self.tck_expr(&l, x_sup)?;
                let rhs = self.tck_expr(&r, x_sup)?;

                // Equate the sub-expressions' types.
                let res = self.merge_exprs(&l, &r)?;

                // Incorporate any mapping components.
                MappedType {
                    stream: Subtyping::merge_max(lhs.stream, rhs.stream)?,
                    vector: Subtyping::merge_max(lhs.vector, rhs.vector)?,
                    option: Subtyping::merge_max(lhs.option, rhs.option)?,
                    scalar: res,
                }
            },

            ast::Expr::Bin(BinOp::Cat, [l, r]) => {
                // Resolve the sub-expressions.
                let lhs = self.tck_expr(&l, sup)?;
                let rhs = self.tck_expr(&r, sup)?;

                // Equate the sub-expressions' types.
                let res = self.merge_exprs(&l, &r)?;

                // Calculate the new vector size.
                let vector_size =
                    lhs.vector.map_or(1, |v| v.size) +
                    rhs.vector.map_or(1, |v| v.size);

                // Incorporate any mapping components.
                MappedType {
                    scalar: res,
                    option: Subtyping::merge_max(lhs.option, rhs.option)?,
                    vector: Some(VectorPart { size: vector_size }),
                    stream: Subtyping::merge_max(lhs.stream, rhs.stream)?,
                }
            },

            ast::Expr::Bin(BinOp::Ind, x) => {
                // Resolve the sub-expressions.
                let lhs = self.tck_expr(&x[0], sup)?;
                let rhs = self.tck_expr(&x[1], ScalarType::Int(None))?;

                // Ensure that the left-hand side is an array.
                if lhs.vector.is_none() {
                    return Err(logic::Error::Subtype.into());
                }

                // Incorporate any mapping components.
                MappedType {
                    scalar: lhs.scalar,
                    option: Subtyping::merge_max(lhs.option, rhs.option)?,
                    vector: rhs.vector,
                    stream: Subtyping::merge_max(lhs.stream, rhs.stream)?,
                }
            },

            ast::Expr::Bin(BinOp::Exp, x) => {
                // Resolve the sub-expressions.
                let lhs = self.tck_expr(&x[0], sup)?;
                let rhs = self.tck_expr(&x[1], ScalarType::Int(None))?;

                // Ensure that the right-hand side is a stream.
                if rhs.stream.is_none() {
                    return Err(logic::Error::Subtype.into());
                }

                // Incorporate any mapping components.
                MappedType {
                    scalar: lhs.scalar,
                    option: Some(OptionPart {}),
                    vector: Subtyping::merge_max(lhs.vector, rhs.vector)?,
                    stream: Some(StreamPart {}),
                }
            },

            ast::Expr::Bin(BinOp::Red, x) => {
                // Resolve the sub-expressions.
                let lhs = self.tck_expr(&x[0], sup)?;
                let rhs = self.tck_expr(&x[1], ScalarType::Int(None))?;

                // Ensure that the left-hand side is a stream.
                if lhs.stream.is_none() {
                    return Err(logic::Error::Subtype.into());
                }

                // Ensure that the right-hand side is a stream.
                if rhs.stream.is_none() {
                    return Err(logic::Error::Subtype.into());
                }

                // Ensure that the right-hand side is not optional.
                if rhs.option.is_some() {
                    return Err(logic::Error::Subtype.into());
                }

                // Incorporate any mapping components.
                MappedType {
                    scalar: lhs.scalar,
                    option: lhs.option,
                    vector: Subtyping::merge_max(lhs.vector, rhs.vector)?,
                    stream: Some(StreamPart {}),
                }
            },

            ast::Expr::Bin(BinOp::Cmp(_), x) => {
                // Resolve the sub-expressions.
                let lhs = self.tck_expr(&x[0], sup)?;
                let rhs = self.tck_expr(&x[1], sup)?;

                // Equate the sub-expressions' types.
                self.merge_exprs(&x[0], &x[1])?;

                // Incorporate any mapping components.
                MappedType {
                    scalar: ScalarType::Int(Some(IntType {
                        sign: IntSign::U,
                        size: NonZeroU32::new(1).unwrap(),
                    })),
                    option: Subtyping::merge_max(lhs.option, rhs.option)?,
                    vector: Subtyping::merge_max(lhs.vector, rhs.vector)?,
                    stream: Subtyping::merge_max(lhs.stream, rhs.stream)?,
                }
            }

            ast::Expr::Bin(BinOp::Cond, x) => {
                // Resolve the sub-expressions.
                let lhs = self.tck_expr(&x[0], ScalarType::Int(None))?;
                let rhs = self.tck_expr(&x[1], sup)?;

                // Incorporate any mapping components.
                MappedType {
                    scalar: rhs.scalar,
                    option: Some(OptionPart {}),
                    vector: Subtyping::merge_max(lhs.vector, rhs.vector)?,
                    stream: Subtyping::merge_max(lhs.stream, rhs.stream)?,
                }
            },

            ast::Expr::Bin(BinOp::Else, x) => {
                // Resolve the sub-expressions.
                let lhs = self.tck_expr(&x[0], sup)?;
                let rhs = self.tck_expr(&x[1], sup)?;

                // Equate the sub-expressions' types.
                let res = self.merge_exprs(&x[0], &x[1])?;

                // Ensure that the left-hand side is optional.
                if lhs.option.is_none() {
                    return Err(logic::Error::Subtype.into());
                }

                // Incorporate any mapping components.
                MappedType {
                    scalar: res,
                    option: rhs.option,
                    vector: Subtyping::merge_max(lhs.vector, rhs.vector)?,
                    stream: Subtyping::merge_max(lhs.stream, rhs.stream)?,
                }
            },

            ast::Expr::Par(x) => self.tck_expr(&x, sup)?,

            ast::Expr::Cast(t, x) => {
                // Resolve the sub-expression.
                let x = self.tck_expr(&x, ScalarType::Any)?;
                let t = MappedType::from(t.clone());

                // Ensure that the output type fits the supertype.
                if !t.scalar.is_subtype_of(&sup) {
                    return Err(logic::Error::Subtype.into());
                }

                // Ensure that the subexpression is not optional.
                if x.option.is_some() {
                    return Err(logic::Error::Subtype.into());
                }

                // Ensure that the target type is not optional.
                if t.option.is_some() {
                    return Err(logic::Error::Subtype.into());
                }

                t
            },

            ast::Expr::Int(_) => {
                // Ensure that an integer is allowed here.
                if !ScalarType::Int(None).is_subtype_of(&sup) {
                    return Err(logic::Error::Subtype.into());
                }

                MappedType {
                    stream: None,
                    vector: None,
                    option: None,
                    scalar: ScalarType::Int(None),
                }
            },

            ast::Expr::Var(v) => self.infer_expr(&v.expr, sup)?,

            // 'Arg' is only possible within function argument definitions,
            // which are never visited by this function.
            ast::Expr::Arg => unreachable!(),

            ast::Expr::Blk { stmts, rexpr } => {
                for stmt in stmts {
                    self.tck_stmt(&stmt)?;
                }

                self.tck_expr(&rexpr, sup)?
            },
        };

        // Save the result for later and return.
        let ident = usize::try_from(expr.ident).unwrap();
        self.types[ident] = Some(r#type);
        Ok(r#type)
    }

    /// Merge the scalar types of the given expressions.
    fn merge_exprs(
        &mut self,
        lhs: &Stored<ast::Expr>,
        rhs: &Stored<ast::Expr>,
    ) -> Result<ScalarType, Error> {
        let [lhs, rhs] = [lhs, rhs]
            .map(|x| x.ident - self.ast.expr_ids.start)
            .map(|x| usize::try_from(x).unwrap());

        // Resolve both nodes all the way in.
        let l = self.tvars.find_shorten(&lhs).unwrap();
        let r = self.tvars.find_shorten(&rhs).unwrap();

        // Merge the underlying type values.
        let [lt, rt] = [l, r].map(|x| self.types[x].unwrap().scalar);
        let xt = ScalarType::merge_max(lt, rt)?;

        // Update the union-find to combine the two.
        self.tvars.union_by_rank(&l, &r).unwrap();

        // Update the types to use the merged result.
        self.types[l].as_mut().unwrap().scalar = xt;
        self.types[r].as_mut().unwrap().scalar = xt;

        Ok(xt)
    }

    /// Infer the type of an expression from a supertype.
    fn infer_expr(
        &mut self,
        expr: &Stored<ast::Expr>,
        sup: ScalarType,
    ) -> Result<MappedType, Error> {
        let expr = usize::try_from(expr.ident).unwrap();

        // Resolve the expression node.
        let x = self.tvars.find_shorten(&expr).unwrap();

        // Infer the underlying type value.
        let mut xt = self.types[x].unwrap();
        xt.scalar = xt.scalar.infer_min(sup)?;

        // Update the type to use the merged result.
        self.types[x] = Some(xt);

        Ok(xt)
    }
}

/// A type-checking error.
#[derive(Debug, Error)]
pub enum Error {
    #[error("A type logic error occurred: {0}")]
    Logic(#[from] logic::Error),

    #[error("An expression's type could not be resolved")]
    Unresolvable,
}
