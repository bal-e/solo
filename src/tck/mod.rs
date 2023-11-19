//! Type-checking for Solo.

use core::iter;
use core::num::NonZeroU32;
use core::ops::Deref;

use thiserror::Error;
use unionfind::VecUnionFindByRank;

use crate::ast::{self, ops::*, Stored};
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
    let num_exprs = r#fn.expr_ids.len();
    let mut storage = Storage {
        ast: r#fn,
        types: iter::repeat(None).take(num_exprs).collect(),
        tvars: VecUnionFindByRank::new(0 .. num_exprs).unwrap(),
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
    let types = (0 .. num_exprs)
        .map(|i| (i, storage.tvars.find_shorten(&i).unwrap()))
        .map(|(i, j)| storage.types[i].zip(storage.types[j]))
        .map(|x| x.map(|(mut i, j)| { i.data.data.data = j.data.data.data; i }))
        .map(|t| t
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
    types: Vec<Option<StreamType>>,

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
        stmt: &'ast Stored<ast::Stmt>,
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
        expr: &'ast Stored<ast::Expr>,
        sup: ScalarType,
    ) -> Result<StreamType, Error> {
        let (r#type, eqto) = match **expr {
            ast::Expr::Una(uop, ref src) => {
                let sup = Self::tck_stream_uop_src(uop, sup)?;
                let src_type = self.tck_expr(&src, sup)?;
                self.tck_stream_uop(uop, (&src, src_type))?
            },

            ast::Expr::Bin(bop, [ref lhs, ref rhs]) => {
                let sup = Self::tck_stream_bop_src(bop, sup)?;
                let lhs_type = self.tck_expr(&lhs, sup[0])?;
                let rhs_type = self.tck_expr(&rhs, sup[1])?;
                self.tck_stream_bop(bop, (&lhs, lhs_type), (&rhs, rhs_type))?
            },

            ast::Expr::Par(ref x) => (self.tck_expr(&x, sup)?, Some(&**x)),

            ast::Expr::Cast(ref t, ref x) => {
                // Resolve the sub-expression.
                let x = self.tck_expr(&x, ScalarType::Any)?;
                let t = StreamType::from(t.clone());

                // Ensure that the output type fits the supertype.
                if !t.data.data.data.is_subtype_of(&sup) {
                    return Err(logic::Error::Subtype.into());
                }

                // A non-stream object cannot be casted into a stream.
                if x.part.is_none() && t.part.is_some() {
                    return Err(logic::Error::Subtype.into());
                }

                // Disallow options when bit-casting.
                if x.part.is_some() && t.part.is_none() {
                    if x.data.data.part.is_some() {
                        return Err(logic::Error::Subtype.into());
                    }

                    if t.data.data.part.is_some() {
                        return Err(logic::Error::Subtype.into());
                    }
                }

                (t, None)
            },

            ast::Expr::Int(_) => {
                // Ensure that an integer is allowed here.
                if !ScalarType::Int(None).is_subtype_of(&sup) {
                    return Err(logic::Error::Subtype.into());
                }

                (StreamType {
                    part: None,
                    data: VectorType {
                        part: None,
                        data: OptionType {
                            part: None,
                            data: ScalarType::Int(None),
                        },
                    },
                }, None)
            },

            ast::Expr::Var(ref v) =>
                (self.infer_expr(&v.expr, sup)?, Some(v.expr.deref())),

            // 'Arg' is only possible within function argument definitions,
            // which are never visited by this function.
            ast::Expr::Arg => unreachable!(),

            ast::Expr::Blk { ref stmts, ref rexpr } => {
                for stmt in stmts {
                    self.tck_stmt(&stmt)?;
                }

                (self.tck_expr(&rexpr, sup)?, Some(rexpr.deref()))
            },
        };

        let id = usize::try_from(expr.ident).unwrap();
        self.types[id] = Some(r#type);
        if let Some(eqto) = eqto {
            let expr_id = id;
            let eqto_id = usize::try_from(eqto.ident).unwrap();
            self.tvars.union_by_rank(&expr_id, &eqto_id).unwrap();
        }

        Ok(r#type)
    }

    /// Type-check a binary operation on streams.
    fn tck_stream_bop(
        &mut self,
        bop: StreamBinOp,
        lhs: (&'ast Stored<ast::Expr>, StreamType),
        rhs: (&'ast Stored<ast::Expr>, StreamType),
    ) -> Result<(StreamType, Option<&'ast Stored<ast::Expr>>), Error> {
        Ok(match bop {
            StreamBinOp::Map(bop) => {
                let part = Subtyping::merge_min(lhs.1.part, rhs.1.part)?;
                let (data, eqto) = self.tck_vector_bop(bop,
                        (lhs.0, lhs.1.data), (rhs.0, rhs.1.data))?;
                (StreamType { part, data }, eqto)
            },

            StreamBinOp::Exp => {
                if rhs.1.part.is_none() {
                    return Err(logic::Error::Subtype.into());
                }

                if rhs.1.data.data.part.is_some() {
                    return Err(logic::Error::Subtype.into());
                }

                (StreamType {
                    part: rhs.1.part,
                    data: VectorType {
                        part: Subtyping::merge_min(lhs.1.data.part, rhs.1.data.part)?,
                        data: lhs.1.data.data,
                    },
                }, Some(lhs.0))
            },

            StreamBinOp::Red => {
                if lhs.1.part.is_none() || rhs.1.part.is_none() {
                    return Err(logic::Error::Subtype.into());
                }

                if rhs.1.data.data.part.is_some() {
                    return Err(logic::Error::Subtype.into());
                }

                (StreamType {
                    part: Some(StreamPart {}),
                    data: VectorType {
                        part: Subtyping::merge_min(lhs.1.data.part, rhs.1.data.part)?,
                        data: lhs.1.data.data,
                    },
                }, Some(lhs.0))
            },
        })
    }

    /// Infer the source types for a binary operation on streams.
    fn tck_stream_bop_src(
        bop: StreamBinOp,
        sup: ScalarType,
    ) -> Result<[ScalarType; 2], Error> {
        Ok(match bop {
            StreamBinOp::Map(bop) =>
                Self::tck_vector_bop_src(bop, sup)?,
            StreamBinOp::Exp =>
                [sup, ScalarType::Int(None)],
            StreamBinOp::Red =>
                [sup, ScalarType::Int(None)],
        })
    }

    /// Type-check a unary operation on streams.
    fn tck_stream_uop(
        &mut self,
        uop: StreamUnaOp,
        src: (&'ast Stored<ast::Expr>, StreamType),
    ) -> Result<(StreamType, Option<&'ast Stored<ast::Expr>>), Error> {
        Ok(match uop {
            StreamUnaOp::Map(uop) => {
                let part = src.1.part;
                let (data, eqto) = self
                    .tck_vector_uop(uop, (src.0, src.1.data))?;
                (StreamType { part, data }, eqto)
            },
        })
    }

    /// Infer the source type for a unary operation on streams.
    fn tck_stream_uop_src(
        uop: StreamUnaOp,
        sup: ScalarType,
    ) -> Result<ScalarType, Error> {
        Ok(match uop {
            StreamUnaOp::Map(uop) => Self::tck_vector_uop_src(uop, sup)?,
        })
    }

    /// Type-check a binary operation on vectors.
    fn tck_vector_bop(
        &mut self,
        bop: VectorBinOp,
        lhs: (&'ast Stored<ast::Expr>, VectorType),
        rhs: (&'ast Stored<ast::Expr>, VectorType),
    ) -> Result<(VectorType, Option<&'ast Stored<ast::Expr>>), Error> {
        Ok(match bop {
            VectorBinOp::Map(bop) => {
                let part = Subtyping::merge_min(lhs.1.part, rhs.1.part)?;
                let (data, eqto) = self.tck_option_bop(bop,
                        (lhs.0, lhs.1.data), (rhs.0, rhs.1.data))?;
                (VectorType { part, data }, eqto)
            },

            VectorBinOp::Cat => {
                let size =
                    lhs.1.part.map_or(1, |p| p.size) +
                    rhs.1.part.map_or(1, |p| p.size);
                let part = Some(VectorPart { size });
                let (data, eqto) = self.merge_expr_options(
                    (lhs.0, lhs.1.data), (rhs.0, rhs.1.data))?;
                (VectorType { part, data }, Some(eqto))
            },

            VectorBinOp::Ind => (VectorType {
                part: rhs.1.part,
                data: OptionType {
                    part: Subtyping::merge_min(lhs.1.data.part, rhs.1.data.part)?,
                    data: lhs.1.data.data,
                },
            }, Some(lhs.0)),
        })
    }

    /// Infer the source types for a binary operation on vectors.
    fn tck_vector_bop_src(
        bop: VectorBinOp,
        sup: ScalarType,
    ) -> Result<[ScalarType; 2], Error> {
        Ok(match bop {
            VectorBinOp::Map(bop) =>
                Self::tck_option_bop_src(bop, sup)?,
            VectorBinOp::Cat =>
                [sup; 2],
            VectorBinOp::Ind =>
                [sup, ScalarType::Int(None)],
        })
    }

    /// Type-check a unary operation on vectors.
    fn tck_vector_uop(
        &mut self,
        uop: VectorUnaOp,
        src: (&'ast Stored<ast::Expr>, VectorType),
    ) -> Result<(VectorType, Option<&'ast Stored<ast::Expr>>), Error> {
        Ok(match uop {
            VectorUnaOp::Map(uop) => {
                let part = src.1.part;
                let (data, eqto) = self
                    .tck_option_uop(uop, (src.0, src.1.data))?;
                (VectorType { part, data }, eqto)
            },
        })
    }

    /// Infer the source type for a unary operation on vectors.
    fn tck_vector_uop_src(
        uop: VectorUnaOp,
        sup: ScalarType,
    ) -> Result<ScalarType, Error> {
        Ok(match uop {
            VectorUnaOp::Map(uop) => Self::tck_option_uop_src(uop, sup)?,
        })
    }

    /// Type-check a binary operation on options.
    fn tck_option_bop(
        &mut self,
        bop: OptionBinOp,
        lhs: (&'ast Stored<ast::Expr>, OptionType),
        rhs: (&'ast Stored<ast::Expr>, OptionType),
    ) -> Result<(OptionType, Option<&'ast Stored<ast::Expr>>), Error> {
        Ok(match bop {
            OptionBinOp::Map(bop) => {
                let part = Subtyping::merge_min(lhs.1.part, rhs.1.part)?;
                let (data, eqto) = self.tck_scalar_bop(bop,
                        (lhs.0, lhs.1.data), (rhs.0, rhs.1.data))?;
                (OptionType { part, data }, eqto)
            }

            OptionBinOp::Cond => {
                if lhs.1.part.is_some() {
                    return Err(logic::Error::Subtype.into());
                }

                (OptionType {
                    part: Some(OptionPart {}),
                    data: rhs.1.data,
                }, Some(rhs.0))
            },

            OptionBinOp::Else => {
                let part = None;
                let (data, eqto) = self.merge_expr_scalars(
                    (lhs.0, lhs.1.data), (rhs.0, rhs.1.data))?;
                (OptionType { part, data }, Some(eqto))
            },
        })
    }

    /// Infer the source types for a binary operation on options.
    fn tck_option_bop_src(
        bop: OptionBinOp,
        sup: ScalarType,
    ) -> Result<[ScalarType; 2], Error> {
        Ok(match bop {
            OptionBinOp::Map(bop) =>
                Self::tck_scalar_bop_src(bop, sup)?,
            OptionBinOp::Cond =>
                [ScalarType::Int(None), sup],
            OptionBinOp::Else =>
                [sup; 2],
        })
    }

    /// Type-check a unary operation on options.
    fn tck_option_uop(
        &mut self,
        uop: OptionUnaOp,
        src: (&'ast Stored<ast::Expr>, OptionType),
    ) -> Result<(OptionType, Option<&'ast Stored<ast::Expr>>), Error> {
        Ok(match uop {
            OptionUnaOp::Map(uop) => {
                let part = src.1.part;
                let (data, eqto) = self
                    .tck_scalar_uop(uop, (src.0, src.1.data))?;
                (OptionType { part, data }, eqto)
            },
        })
    }

    /// Infer the source type for a unary operation on options.
    fn tck_option_uop_src(
        uop: OptionUnaOp,
        sup: ScalarType,
    ) -> Result<ScalarType, Error> {
        Ok(match uop {
            OptionUnaOp::Map(uop) => Self::tck_scalar_uop_src(uop, sup)?,
        })
    }

    /// Type-check a binary operation on scalars.
    fn tck_scalar_bop(
        &mut self,
        bop: ScalarBinOp,
        lhs: (&'ast Stored<ast::Expr>, ScalarType),
        rhs: (&'ast Stored<ast::Expr>, ScalarType),
    ) -> Result<(ScalarType, Option<&'ast Stored<ast::Expr>>), Error> {
        Ok(match bop {
            ScalarBinOp::Int(_) => {
                let (data, eqto) = self.merge_expr_scalars(lhs, rhs)?;
                (data, Some(eqto))
            },

            ScalarBinOp::Cmp(_) => {
                self.merge_expr_scalars(lhs, rhs)?;
                (ScalarType::Int(Some(IntType {
                    sign: IntSign::U,
                    size: NonZeroU32::new(1).unwrap(),
                })), None)
            },
        })
    }

    /// Infer the source types for a binary operation on scalars.
    fn tck_scalar_bop_src(
        bop: ScalarBinOp,
        sup: ScalarType,
    ) -> Result<[ScalarType; 2], Error> {
        Ok(match bop {
            ScalarBinOp::Int(_) =>
                [Subtyping::merge_min(ScalarType::Int(None), sup)?; 2],
            ScalarBinOp::Cmp(_) =>
                [sup; 2],
        })
    }

    /// Type-check a unary operation on scalars.
    fn tck_scalar_uop(
        &mut self,
        uop: ScalarUnaOp,
        src: (&'ast Stored<ast::Expr>, ScalarType),
    ) -> Result<(ScalarType, Option<&'ast Stored<ast::Expr>>), Error> {
        Ok(match uop {
            ScalarUnaOp::Int(_) => (src.1, Some(src.0)),
        })
    }

    /// Infer the source type for a unary operation on scalars.
    fn tck_scalar_uop_src(
        uop: ScalarUnaOp,
        sup: ScalarType,
    ) -> Result<ScalarType, Error> {
        Ok(match uop {
            ScalarUnaOp::Int(_) =>
                Subtyping::merge_min(ScalarType::Int(None), sup)?,
        })
    }

    /// Merge the option types of the given expressions.
    fn merge_expr_options(
        &mut self,
        lhs: (&'ast Stored<ast::Expr>, OptionType),
        rhs: (&'ast Stored<ast::Expr>, OptionType),
    ) -> Result<(OptionType, &'ast Stored<ast::Expr>), Error> {
        let part = Subtyping::merge_min(lhs.1.part, rhs.1.part)?;
        let (data, eqto) = self.merge_expr_scalars(
            (lhs.0, lhs.1.data), (rhs.0, rhs.1.data))?;
        Ok((OptionType { part, data }, eqto))
    }

    /// Merge the scalar types of the given expressions.
    fn merge_expr_scalars(
        &mut self,
        lhs: (&'ast Stored<ast::Expr>, ScalarType),
        rhs: (&'ast Stored<ast::Expr>, ScalarType),
    ) -> Result<(ScalarType, &'ast Stored<ast::Expr>), Error> {
        // Combine the types of the expressions.
        let res = Subtyping::merge_min(lhs.1, rhs.1)?;

        // Resolve both nodes all the way in.
        let [l, r] = [lhs.0, rhs.0]
            .map(|x| x.ident - self.ast.expr_ids.start)
            .map(|x| usize::try_from(x).unwrap());
        let l = self.tvars.find_shorten(&l).unwrap();
        let r = self.tvars.find_shorten(&r).unwrap();

        // Update the union-find to combine the two.
        self.tvars.union_by_rank(&l, &r).unwrap();

        // Update the types to use the merged result.
        self.types[l].as_mut().unwrap().data.data.data = res;
        self.types[r].as_mut().unwrap().data.data.data = res;

        Ok((res, lhs.0))
    }

    /// Infer the type of an expression from a supertype.
    fn infer_expr(
        &mut self,
        expr: &Stored<ast::Expr>,
        sup: ScalarType,
    ) -> Result<StreamType, Error> {
        let sid = usize::try_from(expr.ident).unwrap();
        let did = self.tvars.find_shorten(&sid).unwrap();

        let det = self.types[did].as_mut().unwrap();
        let res = det.data.data.data.infer_min(sup)?;
        det.data.data.data = res;

        let set = self.types[sid].as_mut().unwrap();
        set.data.data.data = res;

        Ok(*set)
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
