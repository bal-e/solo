//! Parsing a type-checked Solo AST into HIR.

use egg::{self, RecExpr};

use crate::ast;
use crate::tck;
use crate::soa::*;

use super::*;

/// A parser for type-checked Solo ASTs.
pub struct Parser<'ast: 'tck, 'tck> {
    /// Storage for the AST.
    ast: &'ast ast::Storage,

    /// Storage for type-checking information.
    tck: &'tck tck::Storage<'ast>,

    /// The function being converted to HIR.
    function: &'ast ast::Function,

    /// The expressions in the code.
    exprs: RecExpr<TypedNode>,

    /// The ID for every seen variable definition.
    bindings: Vec<egg::Id>,
}

impl<'ast: 'tck, 'tck> Parser<'ast, 'tck> {
    /// Parse a function from a type-checked AST.
    pub fn parse(
        ast: &'ast ast::Storage,
        tck: &'tck tck::Storage<'ast>,
        function: &'ast ast::Function,
    ) -> RecExpr<TypedNode> {
        let mut this = Self {
            ast,
            tck,
            function,
            exprs: RecExpr::default(),
            bindings: Vec::default(),
        };

        // Add the arguments to the set of bindings.
        for argument_id in function.args.iter() {
            let argument = ast.arguments.get(argument_id);

            let node = Node::Arg(argument_id.into());
            let dstt = argument.r#type;
            let id = this.exprs.add(TypedNode { node, dstt });

            let var_beg = usize::from(function.variables_beg);
            let var_cur = usize::from(argument.variable);
            let var_off = var_cur - var_beg;
            assert_eq!(this.bindings.len(), var_off);
            this.bindings.push(id);
        }

        // Parse the function body.
        this.parse_expr(function.body);

        this.exprs
    }

    fn parse_stmt(&mut self, node_id: ID<ast::Stmt>) {
        let node = self.ast.stmts.get(node_id);
        match *node {
            ast::Stmt::Let(variable_id) => {
                let variable = self.ast.variables.get(variable_id);

                let id = self.parse_expr(variable.expr);

                let var_beg = usize::from(self.function.variables_beg);
                let var_cur = usize::from(variable_id);
                let var_off = var_cur - var_beg;
                assert_eq!(self.bindings.len(), var_off);
                self.bindings.push(id);
            },
        }
    }

    fn parse_expr(&mut self, node_id: ID<ast::Expr>) -> egg::Id {
        let node = self.ast.exprs.get(node_id);
        let node = match *node {
            ast::Expr::Una(uop, [src]) => {
                let src = self.parse_expr(src);
                Node::Una(uop, [src])
            },

            ast::Expr::Bin(bop, [lhs, rhs]) => {
                let lhs = self.parse_expr(lhs);
                let rhs = self.parse_expr(rhs);
                Node::Bin(bop, [lhs, rhs])
            },

            ast::Expr::Par(src) => {
                let src = self.parse_expr(src);
                return src;
            },

            ast::Expr::Cast(dstt, src) => {
                let src = self.parse_expr(src);
                Node::Cast(dstt, src)
            },

            ast::Expr::Int(ref val) => {
                Node::Int(val.clone())
            },

            ast::Expr::Var(variable_id) => {
                let var_beg = usize::from(self.function.variables_beg);
                let var_cur = usize::from(variable_id);
                let var_off = var_cur - var_beg;
                return self.bindings[var_off];
            },

            ast::Expr::Arg => unreachable!(),

            ast::Expr::Blk { stmts, rexpr } => {
                stmts.iter().for_each(|s| self.parse_stmt(s));
                return self.parse_expr(rexpr);
            },
        };

        let dstt = self.tck.get_expr_type(node_id);
        self.exprs.add(TypedNode { node, dstt })
    }
}
