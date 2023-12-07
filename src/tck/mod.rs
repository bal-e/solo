//! Type-checking for Solo.

use thiserror::Error;
use unionfind::VecUnionFindByRank;

use crate::soa::ID;

use crate::ast;
use crate::ops::tys::*;
use crate::tys::{*, fix, var::*};

/// Storage for type-checking.
pub struct Storage<'ast> {
    /// Storage for the AST.
    ast: &'ast ast::Storage,

    /// The type of every expression in the AST.
    ///
    /// The indices here correspond to the IDs of the stored expressions whose
    /// types are being inferred.
    types: Vec<MappedType>,

    /// A union-find structure for equating scalar types.
    ///
    /// The elements / IDs here correspond to the IDs of the stored expressions
    /// whose types are being inferred.
    tvars: VecUnionFindByRank<usize>,
}

impl<'ast> Storage<'ast> {
    /// Construct a new [`Storage`].
    pub fn new(ast: &'ast ast::Storage) -> Self {
        let num = ast.exprs.num();

        let mut types = Vec::with_capacity(num);
        types.resize(num, MappedType {
            stream: Partial::Any,
            vector: Partial::Any,
            option: Partial::Any,
            scalar: Partial::Any,
        });

        let tvars = VecUnionFindByRank::new(0 .. num).unwrap();

        Self { ast, types, tvars }
    }

    /// Retrieve the type of the given expression.
    pub fn get_expr_type(&self, id: ID<ast::Expr>) -> fix::MappedType {
        self.types[usize::from(id)].try_into()
            .expect("This type must have already been resolved")
    }

    /// Type-check a function.
    pub fn tck_function(
        &mut self,
        function: &ast::Function,
    ) -> Result<(), Error> {
        // Directly insert type information for arguments.
        for arg in self.ast.arguments.get_seq(function.args) {
            let variable = self.ast.variables.get(arg.variable);
            let ident = usize::from(variable.expr);
            self.types[ident] = arg.r#type.into();
        }

        // Type-check the body (and verify it against the return type).
        let body_id = function.body.last().unwrap();
        let body_sup = Partial::Val(function.rett.scalar.into());
        let rett = self.tck_expr(body_id, body_sup)?;
        if rett != function.rett.into() {
            return Err(Error::Logic(BoundError));
        }

        // Ensure that all types are resolved.
        for expr_id in function.body.iter() {
            let sid = usize::from(expr_id);
            let did = self.tvars.find_shorten(&sid).unwrap();

            // Update the scalar part of the type.
            let det = self.types[did];
            let set = &mut self.types[sid];
            set.scalar = det.scalar;

            // Ensure that the updated type is resolved.
            let _ = fix::MappedType::try_from(*set)?;
        }

        Ok(())
    }

    /// Type-check a statement.
    fn tck_stmt(
        &mut self,
        stmt_id: ID<ast::Stmt>,
    ) -> Result<(), Error> {
        println!("tck_stmt({:?})", stmt_id);
        let stmt = self.ast.stmts.get(stmt_id);
        match stmt {
            ast::Stmt::Let(variable) => {
                let variable = self.ast.variables.get(*variable);
                self.tck_expr(variable.expr, Partial::Max)?;
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
        expr_id: ID<ast::Expr>,
        sup: Partial<ScalarType>,
    ) -> Result<MappedType, Error> {
        println!("beg tck_expr({:?}, {})", expr_id, sup);
        let expr = self.ast.exprs.get(expr_id);
        let (r#type, eqto) = match *expr {
            ast::Expr::Una(uop, [src_id]) => {
                println!("- una {} of {:?}", uop, src_id);
                let sup = uop.src_type(sup).ok()?;
                let src = self.tck_expr(src_id, sup)?;

                let eqto = match uop.merges() {
                    UnaMerges::Nil => None,
                    UnaMerges::SD => Some(src_id),
                };

                (uop.dst_type(src).ok()?, eqto)
            },

            ast::Expr::Bin(bop, [lhs_id, rhs_id]) => {
                println!("- bin {} of {:?}, {:?}", bop, lhs_id, rhs_id);
                let sup = bop.src_type(sup).ok()?;
                let lhs = self.tck_expr(lhs_id, sup[0])?;
                let rhs = self.tck_expr(rhs_id, sup[1])?;

                if matches!(bop.merges(), BinMerges::LR | BinMerges::LRD) {
                    println!("- merging lr");
                    self.unify_exprs_min(lhs_id, rhs_id)?;
                }

                let eqto = match bop.merges() {
                    BinMerges::Nil => None,
                    BinMerges::LD => Some(lhs_id),
                    BinMerges::RD => Some(rhs_id),
                    BinMerges::LR => None,
                    BinMerges::LRD => Some(lhs_id),
                };

                (bop.dst_type([lhs, rhs]).ok()?, eqto)
            },

            ast::Expr::Par(src) => (self.tck_expr(src, sup)?, Some(src)),

            ast::Expr::Cast(dt, src) => {
                let mut st = self.tck_expr(src, Partial::Max)?;
                let mut dt: MappedType = dt.into();

                // Ensure that the output type fits the supertype.
                dt.scalar = Subtyping::infer(dt.scalar, sup).ok()?;

                if st.stream == Partial::Val(StreamPart::None)
                        && dt.stream == Partial::Val(StreamPart::Some {}) {
                    return Err(Error::Logic(BoundError));
                }

                // Disallow the use of options.
                let none = Partial::Val(OptionPart::None);
                st.option = Subtyping::infer(st.option, none).ok()?;
                dt.option = Subtyping::infer(dt.option, none).ok()?;

                (dt, None)
            },

            ast::Expr::Int(_) => {
                let res = Partial::Val(ScalarType::Int(Partial::Any));
                (Subtyping::infer(res, sup).ok()?
                    .with_part(Partial::Val(OptionPart::None))
                    .with_part(Partial::Val(VectorPart::None))
                    .with_part(Partial::Val(StreamPart::None)),
                    None)
            },

            ast::Expr::Var(variable) => {
                let variable = self.ast.variables.get(variable);
                (self.infer_expr(variable.expr, sup)?, Some(variable.expr))
            },

            // 'Arg' is only possible within function argument definitions,
            // which are never visited by this function.
            ast::Expr::Arg => unreachable!(),

            ast::Expr::Blk { stmts, rexpr } => {
                for stmt in stmts.iter() {
                    self.tck_stmt(stmt)?;
                }

                (self.tck_expr(rexpr, sup)?, Some(rexpr))
            },
        };

        println!("end tck_expr({:?}) -> {}", expr_id, r#type);

        let expr_id = usize::from(expr_id);
        self.types[expr_id] = r#type;
        if let Some(eqto) = eqto {
            let eqto_id = usize::from(eqto);
            self.tvars.union_by_rank(&expr_id, &eqto_id).unwrap();
        }

        Ok(r#type)
    }

    /// Merge the scalar types of the given expressions.
    fn unify_exprs_min(
        &mut self,
        lhs_id: ID<ast::Expr>,
        rhs_id: ID<ast::Expr>,
    ) -> Result<Partial<ScalarType>, Error> {
        println!("unify_exprs_min({:?}, {:?})", lhs_id, rhs_id);

        // Resolve both nodes.
        let lhs_id = self.tvars.find_shorten(&lhs_id.into()).unwrap();
        let rhs_id = self.tvars.find_shorten(&rhs_id.into()).unwrap();

        // Load and combine the nodes.
        let lhs = self.types[usize::from(lhs_id)];
        let rhs = self.types[usize::from(rhs_id)];

        println!("- types {}, {}", lhs, rhs);

        // Combine the types of the expressions.
        let res = Subtyping::unify_min(lhs.scalar, rhs.scalar).ok()?;

        // Update the union-find to combine the two.
        self.tvars.union_by_rank(&lhs_id, &rhs_id).unwrap();

        // Update the nodes to use the merged result.
        self.types[lhs_id].scalar = res;
        self.types[rhs_id].scalar = res;

        Ok(res)
    }

    /// Infer the type of an expression from a supertype.
    fn infer_expr(
        &mut self,
        expr: ID<ast::Expr>,
        sup: Partial<ScalarType>,
    ) -> Result<MappedType, Error> {
        let sid = usize::from(expr);
        let did = self.tvars.find_shorten(&sid).unwrap();

        let det = &mut self.types[did];
        let res = Subtyping::infer(det.scalar, sup).ok()?;
        det.scalar = res;

        let set = &mut self.types[sid];
        set.scalar = res;

        Ok(*set)
    }
}

/// A type-checking error.
#[derive(Debug, Error)]
pub enum Error {
    #[error("A type logic error occurred")]
    Logic(#[from] BoundError),

    #[error("An expression's type could not be resolved")]
    Unresolvable(#[from] UnresolvedError),
}
