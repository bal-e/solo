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
            let id = this.exprs.add(Node::Arg(i as u32));
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
            ast::Expr::Una(o, x) => {
                let xt = self.tck.get_expr_type(&x);
                let x = self.parse_expr(&x);

                let ast::ops::StreamUnaOp::Map(o) = o;
                Self::parse_vector_uop(*o, (x, xt.data), |uop, src| {
                    self.exprs.add(Node::SingleUna(SingleUnaOp::Reg(uop), [src]))
                })
            },

            ast::Expr::Var(v) => self.bindings[v.ident as usize],

            ast::Expr::Blk { stmts, rexpr } => {
                stmts.iter().for_each(|s| self.parse_stmt(s));
                return self.parse_expr(&rexpr)
            },

            _ => todo!(),
        }
    }

    fn parse_vector_bop(
        bop: ast::ops::VectorBinOp,
        lhs: (egg::Id, VectorType),
        rhs: (egg::Id, VectorType),
        mut ins: impl FnMut(VectorNode) -> egg::Id,
    ) -> egg::Id {
        match bop {
            ast::ops::VectorBinOp::Map(bop) => {
                let (lhs_id, rhs_id, map) = match (lhs.1.part, rhs.1.part) {
                    (None, None) => (lhs.0, rhs.0, None),
                    (Some(map), None) => {
                        let uop = VectorUnaOp::New { src: rhs.1.data, map };
                        (lhs.0, (ins)(VectorNode::Una(uop, [rhs.0])), Some(map))
                    },
                    (None, Some(map)) => {
                        let uop = VectorUnaOp::New { src: lhs.1.data, map };
                        ((ins)(VectorNode::Una(uop, [lhs.0])), rhs.0, Some(map))
                    },
                    (Some(lhs_part), Some(rhs_part)) => {
                        let VectorPart { size: lhs_size } = lhs_part;
                        let VectorPart { size: rhs_size } = rhs_part;
                        assert_eq!(lhs_size, rhs_size);
                        (lhs.0, rhs.0, Some(VectorPart { size: lhs_size }))
                    },
                };

                let bop = VectorBinOp::Map { bop, map };
                (ins)(VectorNode::Bin(bop, [lhs_id, rhs_id]))
            },

            ast::ops::VectorBinOp::Cat => {
                let (lhs_id, lhs_part) = match lhs.1.part {
                    None => {
                        let map = Some(VectorPart { size: 1 });
                        let uop = VectorUnaOp::New { src: lhs.1.data, map };
                        ((ins)(VectorNode::Una(uop, [lhs.0])), map)
                    },
                    Some(part) => (lhs.0, part)
                };

                let (rhs_id, rhs_part) = match rhs.1.part {
                    None => {
                        let map = Some(VectorPart { size: 1 });
                        let uop = VectorUnaOp::New { src: rhs.1.data, map };
                        ((ins)(VectorNode::Una(uop, [rhs.0])), map)
                    },
                    Some(part) => (rhs.0, part)
                };

                let bop = VectorBinOp::Cat {
                    src: lhs.1.data,
                    lhs: lhs_part,
                    rhs: rhs_part,
                };
                (ins)(VectorNode::Bin(bop, [lhs_id, rhs_id]))
            },

            ast::ops::VectorBinOp::Ind => {
                let (lhs_id, rhs_id) = (lhs.0, rhs.0);
                let bop = VectorBinOp::Ind {
                    lhs: (lhs.1.part.unwrap(), lhs.1.data),
                    rhs: rhs.1,
                };
                (ins)(VectorNode::Bin(bop, [lhs_id, rhs_id]))
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
                    |node| Self::parse_vector_map(src.1.part, ins, node)),
        }
    }

    fn parse_vector_map(
        map: Option<VectorPart>,
        mut ins: impl FnMut(VectorNode) -> egg::Id,
        node: OptionNode,
    ) -> egg::Id {
        (ins)(match node {
            OptionNode::Una(uop, src) => {
                let uop = VectorUnaOp::Map { uop, map };
                VectorNode::Una(uop, src)
            },

            OptionNode::Bin(bop, src) => {
                let uop = VectorBinOp::Map { bop, map };
                VectorNode::Bin(bop, src)
            },

            OptionNode::Int(n) => VectorNode::Int(n),
            OptionNode::Arg(n) => VectorNode::Arg(n),
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
                let (lhs_id, rhs_id, map) = match (lhs.1.part, rhs.1.part) {
                    (None, None) => (lhs.0, rhs.0, None),
                    (Some(map), None) => {
                        let uop = OptionUnaOp::New { src: rhs.1.data, map };
                        (lhs.0, (ins)(OptionNode::Una(uop, [rhs.0])), Some(map))
                    },
                    (None, Some(map)) => {
                        let uop = OptionUnaOp::New { src: lhs.1.data, map };
                        ((ins)(OptionNode::Una(uop, [lhs.0])), rhs.0, Some(map))
                    },
                    (Some(lhs_part), Some(rhs_part)) => {
                        let OptionPart {} = lhs_part;
                        let OptionPart {} = rhs_part;
                        (lhs.0, rhs.0, Some(OptionPart {}))
                    },
                };

                let bop = OptionBinOp::Map { bop, map };
                (ins)(OptionNode::Bin(bop, [lhs_id, rhs_id]))
            },

            ast::ops::OptionBinOp::Cond => {
                let lhs_id = lhs.0;
                let rhs_id = match rhs.1.part {
                    None => {
                        let map = Some(OptionPart {});
                        let uop = OptionUnaOp::New { src: rhs.1.data, map };
                        (ins)(OptionNode::Una(uop, [rhs.0]))
                    },
                    Some(OptionPart {}) => rhs.0,
                };

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
                    |node| Self::parse_option_map(src.1.part, ins, node)),
        }
    }

    fn parse_option_map(
        map: Option<OptionPart>,
        mut ins: impl FnMut(OptionNode) -> egg::Id,
        node: ScalarNode,
    ) -> egg::Id {
        (ins)(match node {
            ScalarNode::Una(uop, src) => {
                let uop = OptionUnaOp::Map { uop, map };
                OptionNode::Una(uop, src)
            },

            ScalarNode::Bin(bop, src) => {
                let uop = OptionBinOp::Map { bop, map };
                OptionNode::Bin(bop, src)
            },

            ScalarNode::Int(n) => OptionNode::Int(n),
            ScalarNode::Arg(n) => OptionNode::Arg(n),
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
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum StreamNode {
    Una(StreamUnaOp, [Id; 1]),
    Bin(StreamBinOp, [Id; 2]),
    Int(BigInt),
    Arg(u32),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum VectorNode {
    Una(VectorUnaOp, [Id; 1]),
    Bin(VectorBinOp, [Id; 2]),
    Int(BigInt),
    Arg(u32),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum OptionNode {
    Una(OptionUnaOp, [Id; 1]),
    Bin(OptionBinOp, [Id; 2]),
    Int(BigInt),
    Arg(u32),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum ScalarNode {
    Una(ScalarUnaOp, [Id; 1]),
    Bin(ScalarBinOp, [Id; 2]),
    Int(BigInt),
    Arg(u32),
}
