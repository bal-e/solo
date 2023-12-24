//! Parsing a type-checked Solo AST into HIR.

use egg::{self, RecExpr};

use crate::ast;
use crate::tck;
use crate::soa::*;

use super::*;

impl Function {
    /// Parse a function from the AST.
    pub fn parse(
        ast: &ast::Storage,
        tck: &tck::Storage<'_>,
        ast_fn: &ast::Function,
    ) -> Self {
        let mut parser = Parser {
            ast,
            tck,
            ast_fn,
            exprs: RecExpr::default(),
            bindings: Vec::default(),
        };

        let mut args = Vec::with_capacity(ast_fn.args.len());

        // Add the arguments to the set of bindings.
        for argument_id in ast_fn.args.iter() {
            let argument = ast.arguments.get(argument_id);

            let node = Node::Arg(argument_id.into());
            let dstt = argument.r#type;
            args.push(dstt);
            let id = parser.exprs.add(TypedNode { node, dstt });

            let var_beg = usize::from(ast_fn.variables_beg);
            let var_cur = usize::from(argument.variable);
            let var_off = var_cur - var_beg;
            assert_eq!(parser.bindings.len(), var_off);
            parser.bindings.push(id);
        }

        // Parse the function body.
        parser.parse_expr(ast_fn.body, Some(ast_fn.rett.into()));

        Self {
            name: ast_fn.name.clone(),
            args,
            body: parser.exprs,
        }
    }
}

/// A parser for type-checked Solo ASTs.
struct Parser<'ast: 'tck, 'tck> {
    /// Storage for the AST.
    ast: &'ast ast::Storage,

    /// Storage for type-checking information.
    tck: &'tck tck::Storage<'ast>,

    /// The function being converted to HIR.
    ast_fn: &'ast ast::Function,

    /// The expressions in the code.
    exprs: RecExpr<TypedNode>,

    /// The ID for every seen variable definition.
    bindings: Vec<egg::Id>,
}

impl<'ast: 'tck, 'tck> Parser<'ast, 'tck> {
    fn parse_stmt(&mut self, node_id: ID<ast::Stmt>) {
        let node = self.ast.stmts.get(node_id);
        match *node {
            ast::Stmt::Let(variable_id) => {
                let variable = self.ast.variables.get(variable_id);

                let id = self.parse_expr(variable.expr, None);

                let var_beg = usize::from(self.ast_fn.variables_beg);
                let var_cur = usize::from(variable_id);
                let var_off = var_cur - var_beg;
                assert_eq!(self.bindings.len(), var_off);
                self.bindings.push(id);
            },
        }
    }

    fn parse_expr(
        &mut self,
        node_id: ID<ast::Expr>,
        mapt: Option<MappedPart>,
    ) -> egg::Id {
        let node = self.ast.exprs.get(node_id);
        let dstt = self.tck.get_expr_type(node_id);
        let node = match *node {
            ast::Expr::Una(uop, [src]) => {
                let src = self.parse_expr(src, None);
                Node::Una(uop, [src])
            },

            ast::Expr::Bin(bop, [lhs, rhs]) => {
                let lhs_type = self.tck.get_expr_type(lhs);
                let rhs_type = self.tck.get_expr_type(rhs);
                let map_parts = [lhs_type, rhs_type].map(From::from);
                let map_parts = bop.src_map_part(map_parts);
                let lhs = self.parse_expr(lhs, Some(map_parts[0]));
                let rhs = self.parse_expr(rhs, Some(map_parts[1]));
                Node::Bin(bop, [lhs, rhs])
            },

            ast::Expr::Par(src) => {
                let src = self.parse_expr(src, None);
                return src;
            },

            ast::Expr::Cast(dstt, src) => {
                let src = self.parse_expr(src, None);
                let cop = {
                    if let StreamPart::Some { .. } = dstt.stream {
                        CastOp::Stream
                    } else if let VectorPart::Some { .. } = dstt.vector {
                        CastOp::Vector
                    } else if let OptionPart::Some { .. } = dstt.option {
                        CastOp::Option
                    } else {
                        CastOp::Scalar
                    }
                };

                Node::BitCast(cop, src)
            },

            ast::Expr::Int(ref val) => {
                Node::Int(val.clone())
            },

            ast::Expr::Vec(src) => {
                todo!()
            },

            ast::Expr::Var(variable_id) => {
                let var_beg = usize::from(self.ast_fn.variables_beg);
                let var_cur = usize::from(variable_id);
                let var_off = var_cur - var_beg;
                return self.bindings[var_off];
            },

            ast::Expr::Arg => unreachable!(),

            ast::Expr::Blk { stmts, rexpr } => {
                stmts.iter().for_each(|s| self.parse_stmt(s));
                return self.parse_expr(rexpr, None);
            },
        };

        let mut dstt = dstt;
        let mut result = self.exprs.add(TypedNode { node, dstt });
        let mapt = mapt.unwrap_or(dstt.into());

        if dstt.option != mapt.option {
            let node = Node::MapCast(CastOp::Option, result);
            dstt.option = mapt.option;
            result = self.exprs.add(TypedNode { node, dstt });
        }

        if dstt.vector != mapt.vector {
            let node = Node::MapCast(CastOp::Vector, result);
            dstt.vector = mapt.vector;
            result = self.exprs.add(TypedNode { node, dstt });
        }

        if dstt.stream != mapt.stream {
            let node = Node::MapCast(CastOp::Stream, result);
            dstt.stream = mapt.stream;
            result = self.exprs.add(TypedNode { node, dstt });
        }

        result
    }
}
