//! Parsing a type-checked Solo AST into HIR.

use egg::{self, RecExpr};

use crate::ast::{self, Stored};
use crate::tck;

use super::*;

/// Parse a type-checked function into HIR.
pub fn parse_fn(ast: &ast::Function, tck: &tck::Function) -> RecExpr<Node> {
    Parser::parse(ast, tck)
}

/// A parser for type-checked Solo ASTs.
struct Parser<'tck, 'ast: 'tck> {
    /// Type-checking information.
    tck: &'tck tck::Function<'ast>,

    /// The expressions in the code.
    exprs: RecExpr<Node>,

    /// The ID for every seen variable definition.
    bindings: Vec<egg::Id>,
}

impl<'tck, 'ast: 'tck> Parser<'tck, 'ast> {
    /// Parse a function from a type-checked AST.
    fn parse(
        ast: &'ast ast::Function,
        tck: &'tck tck::Function<'ast>,
    ) -> RecExpr<Node> {
        let mut this = Self {
            tck,
            exprs: RecExpr::default(),
            bindings: Vec::default(),
        };

        // Add the arguments to the set of bindings.
        for (i, arg) in ast.args.iter().enumerate() {
            let dst = tck.get_expr_type(&arg.variable.expr);
            let nop = ast::ops::ScalarNilOp::Arg(i as u32);
            let nop = ast::ops::OptionNilOp::Map(nop);
            let nop = ast::ops::VectorNilOp::Map(nop);
            let nop = ast::ops::StreamNilOp::Map(nop);
            let id = Self::parse_stream_nop(nop, dst, |n| this.parse_ins(n));
            assert_eq!(Ok(this.bindings.len()), arg.variable.ident.try_into());
            this.bindings.push(id);
        }

        // Parse the function body.
        this.parse_expr(&ast.body);

        this.exprs
    }

    fn parse_stmt(&mut self, node: &'ast Stored<ast::Stmt>) {
        match &**node {
            ast::Stmt::Let(v) => {
                // Add this variable to the local bindings.
                let id = self.parse_expr(&v.expr);
                assert_eq!(Ok(self.bindings.len()), v.ident.try_into());
                self.bindings.push(id);
            },
        }
    }

    fn parse_expr(
        &mut self,
        ast: &'ast Stored<ast::Expr>,
    ) -> egg::Id {
        match &**ast {
            ast::Expr::Una(uop, src) => {
                let src_ty = self.tck.get_expr_type(&src);
                let src_id = self.parse_expr(&src);
                let src = (src_id, src_ty.data);

                Self::parse_stream_uop(*uop, src, |n| self.parse_ins(n))
            },

            ast::Expr::Bin(bop, [lhs, rhs]) => {
                let lhs_ty = self.tck.get_expr_type(&lhs);
                let lhs_id = self.parse_expr(&lhs);
                let lhs = (lhs_id, lhs_ty.data);

                let rhs_ty = self.tck.get_expr_type(&rhs);
                let rhs_id = self.parse_expr(&rhs);
                let rhs = (rhs_id, rhs_ty.data);

                Self::parse_stream_bop(*bop, lhs, rhs, |n| self.parse_ins(n))
            },

            ast::Expr::Par(src) => self.parse_expr(&src),

            ast::Expr::Cast(..) => todo!(),

            ast::Expr::Int(num) => {
                let dst = self.tck.get_expr_type(ast);
                let nop = ast::ops::ScalarNilOp::Int(num.clone());
                let nop = ast::ops::OptionNilOp::Map(nop);
                let nop = ast::ops::VectorNilOp::Map(nop);
                let nop = ast::ops::StreamNilOp::Map(nop);

                Self::parse_stream_nop(nop, dst, |n| self.parse_ins(n))
            },

            ast::Expr::Var(v) => self.bindings[v.ident as usize],

            ast::Expr::Arg => unreachable!(),

            ast::Expr::Blk { stmts, rexpr } => {
                stmts.iter().for_each(|s| self.parse_stmt(s));
                return self.parse_expr(&rexpr)
            },
        }
    }

    fn parse_ins(&mut self, node: StreamNode) -> egg::Id {
        self.exprs.add(match node {
            StreamNode::Una(uop, src) => Node::Una(uop, src),
            StreamNode::Bin(bop, src) => Node::Bin(bop, src),
            StreamNode::Nil(nop) => Node::Nil(nop),
        })
    }

    fn parse_stream_bop(
        bop: ast::ops::StreamBinOp,
        lhs: (egg::Id, VectorType),
        rhs: (egg::Id, VectorType),
        mut ins: impl FnMut(StreamNode) -> egg::Id,
    ) -> egg::Id {
        match bop {
            ast::ops::StreamBinOp::Map(bop) => {
                Self::parse_vector_bop(bop, lhs, rhs, |node| {
                    Self::parse_stream_map(None, &mut ins, node)
                })
            },

            ast::ops::StreamBinOp::Exp => {
                assert_eq!(None, rhs.1.part);
                assert_eq!(None, rhs.1.data.part);
                assert!(matches!(rhs.1.data.data, ScalarType::Int(_)));

                let bop = StreamBinOp::Exp { lhs: lhs.1 };
                (ins)(StreamNode::Bin(bop, [lhs.0, rhs.0]))
            },

            ast::ops::StreamBinOp::Red => {
                assert_eq!(None, rhs.1.part);
                assert_eq!(None, rhs.1.data.part);
                assert!(matches!(rhs.1.data.data, ScalarType::Int(_)));

                let bop = StreamBinOp::Red { lhs: lhs.1 };
                (ins)(StreamNode::Bin(bop, [lhs.0, rhs.0]))
            },
        }
    }

    fn parse_stream_uop(
        uop: ast::ops::StreamUnaOp,
        src: (egg::Id, VectorType),
        mut ins: impl FnMut(StreamNode) -> egg::Id,
    ) -> egg::Id {
        match uop {
            ast::ops::StreamUnaOp::Map(uop) => {
                Self::parse_vector_uop(uop, src, |node| {
                    Self::parse_stream_map(None, &mut ins, node)
                })
            },
        }
    }

    fn parse_stream_nop(
        nop: ast::ops::StreamNilOp,
        dst: StreamType,
        mut ins: impl FnMut(StreamNode) -> egg::Id,
    ) -> egg::Id {
        match nop {
            ast::ops::StreamNilOp::Map(nop) => {
                Self::parse_vector_nop(nop, dst.data, |node| {
                    Self::parse_stream_map(dst.part, &mut ins, node)
                })
            },
        }
    }

    fn parse_stream_map(
        map: Option<StreamPart>,
        mut ins: impl FnMut(StreamNode) -> egg::Id,
        node: VectorNode,
    ) -> egg::Id {
        (ins)(match node {
            VectorNode::Nil(nop) => {
                let nop = StreamNilOp::Map { nop, map };
                StreamNode::Nil(nop)
            },

            VectorNode::Una(uop, src) => {
                let uop = StreamUnaOp::Map { uop };
                StreamNode::Una(uop, src)
            },

            VectorNode::Bin(bop, src) => {
                let bop = StreamBinOp::Map { bop };
                StreamNode::Bin(bop, src)
            },
        })
    }

    fn parse_vector_bop(
        bop: ast::ops::VectorBinOp,
        lhs: (egg::Id, VectorType),
        rhs: (egg::Id, VectorType),
        mut ins: impl FnMut(VectorNode) -> egg::Id,
    ) -> egg::Id {
        match bop {
            ast::ops::VectorBinOp::Map(bop) => {
                let (lhs_id, rhs_id, map)
                    = Self::parse_vector_zip(lhs, rhs, &mut ins);
                let lhs = (lhs_id, lhs.1.data);
                let rhs = (rhs_id, rhs.1.data);
                Self::parse_option_bop(bop, lhs, rhs, |node| {
                    Self::parse_vector_map(map, &mut ins, node)
                })
            },

            ast::ops::VectorBinOp::Cat => {
                assert_eq!(lhs.1.data, rhs.1.data);

                // Zip up options.
                let map = [lhs, rhs]
                    .into_iter()
                    .map(|x| x.1.data.part)
                    .fold(None, Option::or);

                let [lhs_id, rhs_id] = [lhs, rhs].map(|x| {
                    Self::parse_option_new(map, (x.0, x.1.data), |node| {
                        Self::parse_vector_map(x.1.part, &mut ins, node)
                    })
                });

                let lhs_ty = OptionType { part: map, .. lhs.1.data };
                let lhs_ty = VectorType { data: lhs_ty, .. lhs.1 };
                let lhs = (lhs_id, lhs_ty);

                let rhs_ty = OptionType { part: map, .. rhs.1.data };
                let rhs_ty = VectorType { data: rhs_ty, .. rhs.1 };
                let rhs = (rhs_id, rhs_ty);

                // Convert to vectors wherever necessary.
                let src = lhs.1.data;
                let [lhs, rhs] = [lhs, rhs].map(|(id, ty)| match ty.part {
                    Some(part) => (id, part),
                    None => {
                        let map = VectorPart { size: 1 };
                        let uop = VectorUnaOp::New { src, map };
                        ((ins)(VectorNode::Una(uop, [id])), map)
                    },
                });

                let bop = VectorBinOp::Cat { src, lhs: lhs.1, rhs: rhs.1 };
                (ins)(VectorNode::Bin(bop, [lhs.0, rhs.0]))
            },

            ast::ops::VectorBinOp::Ind => {
                // Zip up options.
                let map = [lhs, rhs]
                    .into_iter()
                    .map(|x| x.1.data.part)
                    .fold(None, Option::or);

                let [lhs_id, rhs_id] = [lhs, rhs].map(|x| {
                    Self::parse_option_new(map, (x.0, x.1.data), |node| {
                        Self::parse_vector_map(x.1.part, &mut ins, node)
                    })
                });

                let lhs_ty = OptionType { part: map, .. lhs.1.data };
                let lhs_ty = VectorType { data: lhs_ty, .. lhs.1 };
                let lhs = (lhs_id, lhs_ty);

                let rhs_ty = OptionType { part: map, .. rhs.1.data };
                let rhs_ty = VectorType { data: rhs_ty, .. rhs.1 };
                let rhs = (rhs_id, rhs_ty);

                let bop = VectorBinOp::Ind {
                    lhs: (lhs.1.part.unwrap(), lhs.1.data),
                    rhs: rhs.1,
                };
                (ins)(VectorNode::Bin(bop, [lhs.0, rhs.0]))
            },
        }
    }

    fn parse_vector_uop(
        uop: ast::ops::VectorUnaOp,
        src: (egg::Id, VectorType),
        mut ins: impl FnMut(VectorNode) -> egg::Id,
    ) -> egg::Id {
        match uop {
            ast::ops::VectorUnaOp::Map(uop) =>
                Self::parse_option_uop(uop, (src.0, src.1.data),
                    |node| Self::parse_vector_map(src.1.part, &mut ins, node)),
        }
    }

    fn parse_vector_nop(
        nop: ast::ops::VectorNilOp,
        dst: VectorType,
        mut ins: impl FnMut(VectorNode) -> egg::Id,
    ) -> egg::Id {
        match nop {
            ast::ops::VectorNilOp::Map(nop) => {
                Self::parse_option_nop(nop, dst.data, |node| {
                    Self::parse_vector_map(dst.part, &mut ins, node)
                })
            },
        }
    }

    fn parse_vector_zip(
        lhs: (egg::Id, VectorType),
        rhs: (egg::Id, VectorType),
        mut ins: impl FnMut(VectorNode) -> egg::Id,
    ) -> (egg::Id, egg::Id, Option<VectorPart>) {
        let map = [lhs.1.part, rhs.1.part].into_iter().fold(None, Option::or);
        let lhs_id = map.map(|map| Self::parse_vector_new(map, lhs, &mut ins));
        let rhs_id = map.map(|map| Self::parse_vector_new(map, rhs, &mut ins));
        (lhs_id.unwrap_or(lhs.0), rhs_id.unwrap_or(rhs.0), map)
    }

    fn parse_vector_new(
        map: VectorPart,
        src: (egg::Id, VectorType),
        mut ins: impl FnMut(VectorNode) -> egg::Id,
    ) -> egg::Id {
        if let Some(part) = src.1.part {
            assert_eq!(map, part);
            src.0
        } else {
            let uop = VectorUnaOp::New { src: src.1.data, map };
            (ins)(VectorNode::Una(uop, [src.0]))
        }
    }

    fn parse_vector_map(
        map: Option<VectorPart>,
        mut ins: impl FnMut(VectorNode) -> egg::Id,
        node: OptionNode,
    ) -> egg::Id {
        (ins)(match node {
            OptionNode::Nil(nop) => {
                let nop = VectorNilOp::Map { nop, map };
                VectorNode::Nil(nop)
            },

            OptionNode::Una(uop, src) => {
                let uop = VectorUnaOp::Map { uop, map };
                VectorNode::Una(uop, src)
            },

            OptionNode::Bin(bop, src) => {
                let bop = VectorBinOp::Map { bop, map };
                VectorNode::Bin(bop, src)
            },
        })
    }

    fn parse_option_bop(
        bop: ast::ops::OptionBinOp,
        lhs: (egg::Id, OptionType),
        rhs: (egg::Id, OptionType),
        mut ins: impl FnMut(OptionNode) -> egg::Id,
    ) -> egg::Id {
        match bop {
            ast::ops::OptionBinOp::Map(bop) => {
                let (lhs_id, rhs_id, map) =
                    Self::parse_option_zip(lhs, rhs, &mut ins);
                let lhs = (lhs_id, lhs.1.data);
                let rhs = (rhs_id, rhs.1.data);
                Self::parse_scalar_bop(bop, lhs, rhs, |node| {
                    Self::parse_option_map(map, &mut ins, node)
                })
            },

            ast::ops::OptionBinOp::Cond => {
                let map = Some(OptionPart {});
                let lhs_id = lhs.0;
                let rhs_id = Self::parse_option_new(map, rhs, &mut ins);

                let bop = OptionBinOp::Cond { rhs: rhs.1.data };
                (ins)(OptionNode::Bin(bop, [lhs_id, rhs_id]))
            },

            ast::ops::OptionBinOp::Else => {
                let (lhs_id, rhs_id) = (lhs.0, rhs.0);
                let bop = OptionBinOp::Else {
                    src: lhs.1.data,
                    rhs: rhs.1.part,
                };
                (ins)(OptionNode::Bin(bop, [lhs_id, rhs_id]))
            },
        }
    }

    fn parse_option_uop(
        uop: ast::ops::OptionUnaOp,
        src: (egg::Id, OptionType),
        mut ins: impl FnMut(OptionNode) -> egg::Id,
    ) -> egg::Id {
        match uop {
            ast::ops::OptionUnaOp::Map(uop) =>
                Self::parse_scalar_uop(uop, (src.0, src.1.data),
                    |node| Self::parse_option_map(src.1.part, &mut ins, node)),
        }
    }

    fn parse_option_nop(
        nop: ast::ops::OptionNilOp,
        dst: OptionType,
        mut ins: impl FnMut(OptionNode) -> egg::Id,
    ) -> egg::Id {
        match nop {
            ast::ops::OptionNilOp::Map(nop) => {
                Self::parse_scalar_nop(nop, dst.data, |node| {
                    Self::parse_option_map(dst.part, &mut ins, node)
                })
            },
        }
    }

    fn parse_option_zip(
        lhs: (egg::Id, OptionType),
        rhs: (egg::Id, OptionType),
        mut ins: impl FnMut(OptionNode) -> egg::Id,
    ) -> (egg::Id, egg::Id, Option<OptionPart>) {
        let map = [lhs.1.part, rhs.1.part].into_iter().fold(None, Option::or);
        let lhs_id = Self::parse_option_new(map, lhs, &mut ins);
        let rhs_id = Self::parse_option_new(map, rhs, &mut ins);
        (lhs_id, rhs_id, map)
    }

    fn parse_option_new(
        map: Option<OptionPart>,
        src: (egg::Id, OptionType),
        mut ins: impl FnMut(OptionNode) -> egg::Id,
    ) -> egg::Id {
        if let Some(part) = src.1.part {
            assert_eq!(map, Some(part));
            src.0
        } else if let Some(map) = map {
            let uop = OptionUnaOp::New { src: src.1.data, map };
            (ins)(OptionNode::Una(uop, [src.0]))
        } else {
            src.0
        }
    }

    fn parse_option_map(
        map: Option<OptionPart>,
        mut ins: impl FnMut(OptionNode) -> egg::Id,
        node: ScalarNode,
    ) -> egg::Id {
        (ins)(match node {
            ScalarNode::Nil(nop) => {
                let nop = OptionNilOp::Map { nop, map };
                OptionNode::Nil(nop)
            },

            ScalarNode::Una(uop, src) => {
                let uop = OptionUnaOp::Map { uop, map };
                OptionNode::Una(uop, src)
            },

            ScalarNode::Bin(bop, src) => {
                let bop = OptionBinOp::Map { bop, map };
                OptionNode::Bin(bop, src)
            },
        })
    }

    fn parse_scalar_bop(
        bop: ast::ops::ScalarBinOp,
        lhs: (egg::Id, ScalarType),
        rhs: (egg::Id, ScalarType),
        mut ins: impl FnMut(ScalarNode) -> egg::Id,
    ) -> egg::Id {
        match bop {
            ast::ops::ScalarBinOp::Int(bop) => {
                let ScalarType::Int(lhs_type) = lhs.1;
                let ScalarType::Int(rhs_type) = rhs.1;
                assert_eq!(lhs_type, rhs_type);
                let bop = ScalarBinOp::Int { bop, map: lhs_type };
                (ins)(ScalarNode::Bin(bop, [lhs.0, rhs.0]))
            },

            ast::ops::ScalarBinOp::Cmp(bop) => {
                assert_eq!(lhs.1, rhs.1);
                let bop = ScalarBinOp::Cmp { bop, map: lhs.1 };
                (ins)(ScalarNode::Bin(bop, [lhs.0, rhs.0]))
            },
        }
    }

    fn parse_scalar_uop(
        uop: ast::ops::ScalarUnaOp,
        src: (egg::Id, ScalarType),
        mut ins: impl FnMut(ScalarNode) -> egg::Id,
    ) -> egg::Id {
        match uop {
            ast::ops::ScalarUnaOp::Int(uop) => {
                let ScalarType::Int(src_type) = src.1;
                let uop = ScalarUnaOp::Int { uop, map: src_type };
                (ins)(ScalarNode::Una(uop, [src.0]))
            },
        }
    }

    fn parse_scalar_nop(
        nop: ast::ops::ScalarNilOp,
        dst: ScalarType,
        mut ins: impl FnMut(ScalarNode) -> egg::Id,
    ) -> egg::Id {
        match nop {
            ast::ops::ScalarNilOp::Int(val) => {
                let ScalarType::Int(dst) = dst;
                let nop = ScalarNilOp::Int { val, dst };
                (ins)(ScalarNode::Nil(nop))
            },

            ast::ops::ScalarNilOp::Arg(num) => {
                let nop = ScalarNilOp::Arg { num, dst };
                (ins)(ScalarNode::Nil(nop))
            },
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum StreamNode {
    Nil(StreamNilOp),
    Una(StreamUnaOp, [Id; 1]),
    Bin(StreamBinOp, [Id; 2]),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum VectorNode {
    Nil(VectorNilOp),
    Una(VectorUnaOp, [Id; 1]),
    Bin(VectorBinOp, [Id; 2]),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum OptionNode {
    Nil(OptionNilOp),
    Una(OptionUnaOp, [Id; 1]),
    Bin(OptionBinOp, [Id; 2]),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum ScalarNode {
    Nil(ScalarNilOp),
    Una(ScalarUnaOp, [Id; 1]),
    Bin(ScalarBinOp, [Id; 2]),
}
