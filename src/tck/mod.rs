//! Type-checking for Solo.

use thiserror::Error;
use unionfind::VecUnionFind;

use crate::ast;
use crate::storage::{ID, Stored};

use self::logic::*;

pub mod logic;

/// Storage for type-checking.
pub struct Storage<'ast> {
    /// Storage for the underlying AST.
    ast: &'ast ast::Storage<'ast>,

    /// The type of every expression in the AST.
    ///
    /// If an expression is unresolved, [`None`] is stored; if the scalar part
    /// of the expression is unresolved, then the union-find should be used to
    /// resolve it.
    ///
    /// The indices here correspond to the IDs of the stored expressions whose
    /// types are being inferred.
    types: Vec<Option<MapType>>,

    /// A union-find structure for equating scalar types.
    ///
    /// The elements / IDs here correspond to the IDs of the stored expressions
    /// whose types are being inferred.
    tvars: VecUnionFind<usize>,
}

impl<'ast> Storage<'ast> {
    /// Construct a new [`Storage`].
    pub fn new(ast: &'ast ast::Storage<'ast>) -> Self {
        Self {
            ast,
            types: Vec::new(),
            tvars: VecUnionFind::new([]).unwrap(),
        }
    }

    /// Type-check the given function.
    ///
    /// Once type-checked, the type of every expression in the function will be
    /// resolved, and can be retrieved using [`Storage::get_expr_type()`].
    pub fn tck_fn(&mut self, r#fn: &'ast Stored<ast::Fn<'ast>>) -> Result<()> {
        // Extend the type arrays to fit all possible expressions.
        (self.types.len() .. self.ast.exprs.num())
            .for_each(|index| self.tvars.add(index).unwrap());
        self.types.resize(self.ast.exprs.num(), None);

        // Resolve the type of every function argument.
        for arg in r#fn.args {
            self.types[usize::from(arg.expr.id())] = Some(arg.r#type.into());
        }

        // Resolve the function body.
        let rett = self.tck_expr(r#fn.body, Type::Any)?;
        if !rett.is_subtype_of(&r#fn.rett.into()) {
            return Err(logic::Error::Subtype.into());
        }

        Ok(())
    }

    /// Type-check a statement.
    fn tck_stmt(
        &mut self,
        stmt: &'ast Stored<ast::Stmt<'ast>>,
    ) -> Result<()> {
        match **stmt {
            ast::Stmt::Let(_, e) => {
                self.tck_expr(e, Type::Any)?;
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
        expr: &'ast Stored<ast::Expr<'ast>>,
        sup: Type,
    ) -> Result<MapType> {
        let r#type = match **expr {
            ast::Expr::Not(x) => {
                // Resolve the sub-expression.
                let x_sup = Type::merge_min(Type::Int(IntType::Any), sup)?;
                self.tck_expr(x, x_sup)?
            },

            ast::Expr::Add(x) | ast::Expr::Sub(x)
                | ast::Expr::Mul(x) | ast::Expr::Div(x) | ast::Expr::Rem(x)
                | ast::Expr::And(x) | ast::Expr::IOr(x) | ast::Expr::XOr(x)
                | ast::Expr::ShL(x) | ast::Expr::ShR(x) => {

                // Resolve the sub-expressions.
                let x_sup = Type::merge_min(Type::Int(IntType::Any), sup)?;
                let lhs = self.tck_expr(x[0], x_sup)?;
                let rhs = self.tck_expr(x[1], x_sup)?;

                // Equate the sub-expressions' types.
                let res = self.merge_exprs(x[0].id(), x[1].id())?;

                // Incorporate any mapping components.
                MapType {
                    scalar: res,
                    option: lhs.option || rhs.option,
                    vector: lhs.vector || rhs.vector,
                    stream: lhs.stream || rhs.stream,
                }
            },

            ast::Expr::Cat(x) => {
                // Resolve the sub-expressions.
                let lhs = self.tck_expr(x[0], sup)?;
                let rhs = self.tck_expr(x[1], sup)?;

                // Equate the sub-expressions' types.
                let res = self.merge_exprs(x[0].id(), x[1].id())?;

                // Incorporate any mapping components.
                MapType {
                    scalar: res,
                    option: lhs.option || rhs.option,
                    vector: true,
                    stream: lhs.stream || rhs.stream,
                }
            },

            ast::Expr::Ind(x) => {
                // Resolve the sub-expressions.
                let lhs = self.tck_expr(x[0], sup)?;
                let rhs = self.tck_expr(x[1], Type::Int(IntType::Any))?;

                // Ensure that the left-hand side is an array.
                if !lhs.vector { return Err(logic::Error::Subtype.into()); }

                // Incorporate any mapping components.
                MapType {
                    scalar: lhs.scalar,
                    option: lhs.option || rhs.option,
                    vector: false,
                    stream: lhs.stream || rhs.stream,
                }
            },

            ast::Expr::Exp(x) => {
                // Resolve the sub-expressions.
                let lhs = self.tck_expr(x[0], sup)?;
                let rhs = self.tck_expr(x[1], Type::Int(IntType::Any))?;

                // Ensure that the right-hand side is a stream.
                if !rhs.stream { return Err(logic::Error::Subtype.into()); }

                // Incorporate any mapping components.
                MapType {
                    scalar: lhs.scalar,
                    option: true,
                    vector: lhs.vector || rhs.vector,
                    stream: true,
                }
            },

            ast::Expr::Red(x) => {
                // Resolve the sub-expressions.
                let lhs = self.tck_expr(x[0], sup)?;
                let rhs = self.tck_expr(x[1], Type::Int(IntType::Any))?;

                // Ensure that the left-hand side is a stream.
                if !lhs.stream { return Err(logic::Error::Subtype.into()); }

                // Ensure that the right-hand side is a stream.
                if !rhs.stream { return Err(logic::Error::Subtype.into()); }

                // Ensure that the right-hand side is not optional.
                if rhs.option { return Err(logic::Error::Subtype.into()); }

                // Incorporate any mapping components.
                MapType {
                    scalar: lhs.scalar,
                    option: lhs.option,
                    vector: lhs.vector || rhs.vector,
                    stream: true,
                }
            },

            ast::Expr::IsEq(x) | ast::Expr::IsNE(x)
                | ast::Expr::IsLT(x) | ast::Expr::IsLE(x)
                | ast::Expr::IsGT(x) | ast::Expr::IsGE(x) => {

                // Resolve the sub-expressions.
                let lhs = self.tck_expr(x[0], sup)?;
                let rhs = self.tck_expr(x[1], sup)?;

                // Equate the sub-expressions' types.
                let res = self.merge_exprs(x[0].id(), x[1].id())?;

                // Incorporate any mapping components.
                MapType {
                    scalar: res,
                    option: lhs.option || rhs.option,
                    vector: lhs.vector || rhs.vector,
                    stream: lhs.stream || rhs.stream,
                }
            }

            ast::Expr::Cond(x) => {
                // Resolve the sub-expressions.
                let lhs = self.tck_expr(x[0], Type::Int(IntType::Any))?;
                let rhs = self.tck_expr(x[1], sup)?;

                // Incorporate any mapping components.
                MapType {
                    scalar: rhs.scalar,
                    option: true,
                    vector: lhs.vector || rhs.vector,
                    stream: lhs.stream || rhs.stream,
                }
            },

            ast::Expr::Else(x) => {
                // Resolve the sub-expressions.
                let lhs = self.tck_expr(x[0], sup)?;
                let rhs = self.tck_expr(x[1], sup)?;

                // Equate the sub-expressions' types.
                let res = self.merge_exprs(x[0].id(), x[1].id())?;

                // Ensure that the left-hand side is optional.
                if !lhs.option { return Err(logic::Error::Subtype.into()); }

                // Incorporate any mapping components.
                MapType {
                    scalar: res,
                    option: rhs.option,
                    vector: lhs.vector || rhs.vector,
                    stream: lhs.stream || rhs.stream,
                }
            },

            ast::Expr::Int(_) => {
                // Ensure that an integer is allowed here.
                if !Type::Int(IntType::Any).is_subtype_of(&sup) {
                    return Err(logic::Error::Subtype.into());
                }

                MapType {
                    scalar: Type::Int(IntType::Any),
                    option: false,
                    vector: false,
                    stream: false,
                }
            }

            ast::Expr::Var(_, x) => self.infer_expr(x.id(), sup)?,

            // 'Arg' is only possible within function argument definitions,
            // which are never visited by this function.
            ast::Expr::Arg(_) => unreachable!(),

            ast::Expr::Blk { stmts, rexpr } => {
                for stmt in stmts {
                    self.tck_stmt(stmt)?;
                }

                self.tck_expr(rexpr, sup)?
            },
        };

        // Save the result for later and return.
        self.types[usize::from(expr.id())] = Some(r#type);
        Ok(r#type)
    }

    /// Merge the scalar types of the given expressions.
    fn merge_exprs(
        &mut self,
        lhs: ID<ast::Expr<'ast>>,
        rhs: ID<ast::Expr<'ast>>,
    ) -> Result<Type> {
        todo!()
    }

    /// Infer the type of an expression from a supertype.
    fn infer_expr(
        &mut self,
        expr: ID<ast::Expr<'ast>>,
        sup: Type,
    ) -> Result<MapType> {
        todo!()
    }

    /// Get the type of an expression.
    ///
    /// The function containing the expression must have already been resolved
    /// using [`Storage::tck_fn()`].
    pub fn get_expr_type(&self, expr: ID<ast::Expr<'ast>>) -> MapType {
        self.types[usize::from(expr)].unwrap()
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

/// A type-checking result.
pub type Result<T> = std::result::Result<T, Error>;
