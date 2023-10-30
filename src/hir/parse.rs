//! Parsing a type-checked Solo AST into HIR.

use egg::{self, RecExpr};

use rustc_hash::FxHashMap;

use super::*;
use crate::ast;
use crate::storage::Stored;

/// A parser for type-checked Solo ASTs.
pub struct Parser<'ast> {
    /// The expressions in the code.
    exprs: RecExpr<ExprNode>,
    /// A hashmap from variable definitions to their IDs.
    bindings: FxHashMap<*const Stored<ast::Expr<'ast>>, egg::Id>,
    /// The storage for the AST being parsed.
    _storage: &'ast ast::Storage<'ast>,
}

impl<'ast> Parser<'ast> {
    /// Parse a function from type-checked AST.
    pub fn parse(
        storage: &'ast ast::Storage<'ast>,
        function: &'ast ast::Fn<'ast>,
    ) -> RecExpr<ExprNode> {
        let mut this = Self {
            exprs: RecExpr::default(),
            bindings: FxHashMap::default(),
            _storage: storage,
        };

        // Add the arguments to the set of bindings.
        for arg in function.args.iter().map(|a| &*a) {
            let symbol = egg::Symbol::new(arg.name);
            let id = this.exprs.add(ExprNode::Arg(symbol));
            this.bindings.insert(arg.expr as *const _, id);
        }

        // Parse the function body.
        this.parse_expr(function.body);

        this.exprs
    }

    fn parse_stmt(&mut self, node: &'ast ast::Stmt<'ast>) {
        match node {
            ast::Stmt::Let(_, e) => {
                // Add this variable to the local bindings.
                let id = self.parse_expr(e);
                self.bindings.insert(*e as *const _, id);
            },
        }
    }

    fn parse_expr(&mut self, node: &'ast ast::Expr<'ast>) -> egg::Id {
        fn bin_parse<'ast>(
            this: &mut Parser<'ast>,
            args: &[&'ast Stored<ast::Expr<'ast>>; 2],
        ) -> [egg::Id; 2] {
            args.map(|arg| this.parse_expr(arg))
        }

        let expr = match node {
            ast::Expr::Not(x) => ExprNode::Not(self.parse_expr(x)),

            ast::Expr::Add(x) => ExprNode::Add(bin_parse(self, x)),
            ast::Expr::Sub(x) => ExprNode::Sub(bin_parse(self, x)),
            ast::Expr::Mul(x) => ExprNode::Mul(bin_parse(self, x)),
            ast::Expr::Div(x) => ExprNode::Div(bin_parse(self, x)),
            ast::Expr::Rem(x) => ExprNode::Rem(bin_parse(self, x)),

            ast::Expr::And(x) => ExprNode::And(bin_parse(self, x)),
            ast::Expr::IOr(x) => ExprNode::IOr(bin_parse(self, x)),
            ast::Expr::XOr(x) => ExprNode::XOr(bin_parse(self, x)),
            ast::Expr::ShL(x) => ExprNode::ShL(bin_parse(self, x)),
            ast::Expr::ShR(x) => ExprNode::ShR(bin_parse(self, x)),

            ast::Expr::Cat(x) => ExprNode::Cat(bin_parse(self, x)),
            ast::Expr::Ind(x) => ExprNode::Ind(bin_parse(self, x)),
            ast::Expr::Exp(x) => ExprNode::Exp(bin_parse(self, x)),
            ast::Expr::Red(x) => ExprNode::Red(bin_parse(self, x)),

            ast::Expr::IsEq(x) => ExprNode::IsEq(bin_parse(self, x)),
            ast::Expr::IsNE(x) => ExprNode::IsNE(bin_parse(self, x)),
            ast::Expr::IsLT(x) => ExprNode::IsLT(bin_parse(self, x)),
            ast::Expr::IsLE(x) => ExprNode::IsLE(bin_parse(self, x)),
            ast::Expr::IsGT(x) => ExprNode::IsGT(bin_parse(self, x)),
            ast::Expr::IsGE(x) => ExprNode::IsGE(bin_parse(self, x)),

            ast::Expr::Cond(x) => ExprNode::Cond(bin_parse(self, x)),
            ast::Expr::Else(x) => ExprNode::Else(bin_parse(self, x)),

            ast::Expr::Int(v) => ExprNode::Int((**v).clone()),
            ast::Expr::Var(_, e) => return self.bindings[&(*e as *const _)],
            ast::Expr::Arg(n) => ExprNode::Arg(egg::Symbol::new(n)),
            ast::Expr::Blk { stmts, rexpr } => {
                stmts.iter().for_each(|s| self.parse_stmt(s));
                return self.parse_expr(rexpr)
            },
        };
        self.exprs.add(expr)
    }
}
