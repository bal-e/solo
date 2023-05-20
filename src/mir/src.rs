//! Parsing MIR from source code.

use pest::{iterators::Pair, pratt_parser::PrattParser};

use egg::{Id, RecExpr};

use symbol_table::{Symbol, SymbolTable};

use rustc_hash::FxHashMap;

use crate::src::*;

use super::Expr;

/// A parser converting source code to MIR.
pub struct Parser<'a> {
    /// The MIR block being added to.
    expr: RecExpr<Expr>,
    /// The source code being parsed.
    code: &'a str,
    /// A cached Pratt parser for expressions.
    pratt: PrattParser<Rule>,
    /// A list of variable bindings in scope.
    binds: FxHashMap<Symbol, Id>,
    /// Variable bindings from outer scopes.
    old_binds: Vec<OldBind>,
    /// A [`SymbolTable`] for identifiers.
    symbols: SymbolTable,
}

impl<'a> Parser<'a> {
    /// Parse an MIR expression.
    fn parse_expr(&mut self, pair: Pair<'a, Rule>) -> Id {
        self.pratt
            .map_primary(|p| match p.as_rule() {
                Rule::expr_int => {
                    let num = p.as_str().parse().unwrap();
                    self.expr.add(Expr::Int(num))
                },
                Rule::expr_var => {
                    let sym = self.symbols.intern(p.as_str());
                    *self.binds.get(&sym).expect("Variable not found!")
                },
                Rule::expr_arr => {
                    p.into_inner()
                        .fold(self.expr.add(Expr::NilArr), |prev, expr| {
                            let expr = self.parse_expr(expr);
                            self.expr.add(Expr::Cat([prev, expr]))
                        })
                },
                Rule::expr_let => {
                    // Temporarily put all the binds in the secondary list
                    let mut to_add = 0;
                    let mut expr = None;
                    for item in p.into_inner() {
                        if item.as_rule() == Rule::bind {
                            let (sym, expr) = self.parse_bind(item);
                            self.old_binds.push(OldBind::Swap(sym, expr));
                            to_add += 1;
                        } else {
                            // Parse later, once bindings are applied
                            expr = Some(item);
                        }
                    }

                    // Move the binds into the hashmap atomically (wrt parsing)
                    let off = self.old_binds.len() - to_add;
                    for old_bind in self.old_binds[off ..].iter_mut() {
                        let OldBind::Swap(sym, expr) = *old_bind
                            else { unreachable!() };
                        *old_bind = OldBind::Sole(sym);
                        self.binds.entry(sym)
                            .and_modify(|old_expr| {
                                let expr = std::mem::replace(old_expr, expr);
                                *old_bind = OldBind::Swap(sym, expr);
                            })
                            .or_insert(expr);
                    }

                    // Parse the subexpression with the updated context
                    let res = self.parse_expr(expr.unwrap());

                    // Restore the previous binds
                    let len = self.old_binds.len();
                    for bind in self.old_binds.drain(off .. len) {
                        match bind {
                            OldBind::Swap(sym, expr) =>
                                self.binds.insert(sym, expr),
                            OldBind::Sole(sym) =>
                                self.binds.remove(&sym),
                        };
                    }

                    res
                },
                Rule::expr => self.parse_expr(p),
                _ => unreachable!(),
            })
            .map_prefix(|op, sub| match op.as_rule() {
                Rule::op_neg => self.expr.add(Expr::Neg([sub])),
                Rule::op_not => self.expr.add(Expr::Not([sub])),
                Rule::op_opt => self.expr.add(Expr::Opt([sub])),
                _ => unreachable!(),
            })
            .map_postfix(|sub, op| match op.as_rule() {
                Rule::op_arr_map => self.expr.add(Expr::MapArr([sub])),
                Rule::op_opt_map => self.expr.add(Expr::MapOpt([sub])),
                _ => unreachable!(),
            })
            .map_infix(|lhs, op, rhs| match op.as_rule() {
                Rule::op_add => self.expr.add(Expr::Add([lhs, rhs])),
                Rule::op_sub => self.expr.add(Expr::Sub([lhs, rhs])),
                Rule::op_mul => self.expr.add(Expr::Mul([lhs, rhs])),
                Rule::op_div => self.expr.add(Expr::Div([lhs, rhs])),
                Rule::op_rem => self.expr.add(Expr::Rem([lhs, rhs])),

                Rule::op_and => self.expr.add(Expr::And([lhs, rhs])),
                Rule::op_ior => self.expr.add(Expr::IOr([lhs, rhs])),
                Rule::op_xor => self.expr.add(Expr::XOr([lhs, rhs])),
                Rule::op_shl => self.expr.add(Expr::ShL([lhs, rhs])),
                Rule::op_shr => self.expr.add(Expr::ShR([lhs, rhs])),

                Rule::op_iseq => self.expr.add(Expr::IsEq([lhs, rhs])),
                Rule::op_isne => self.expr.add(Expr::IsNE([lhs, rhs])),
                Rule::op_islt => self.expr.add(Expr::IsLT([lhs, rhs])),
                Rule::op_isle => self.expr.add(Expr::IsLE([lhs, rhs])),
                Rule::op_isgt => self.expr.add(Expr::IsGT([lhs, rhs])),
                Rule::op_isge => self.expr.add(Expr::IsGE([lhs, rhs])),

                Rule::op_toeq => self.expr.add(Expr::ToEq([lhs, rhs])),
                Rule::op_tolt => self.expr.add(Expr::ToLT([lhs, rhs])),

                Rule::op_then => self.expr.add(Expr::Then([lhs, rhs])),
                Rule::op_else => self.expr.add(Expr::Else([lhs, rhs])),
                Rule::op_cat => self.expr.add(Expr::Cat([lhs, rhs])),
                Rule::op_exp => self.expr.add(Expr::Exp([lhs, rhs])),
                Rule::op_red => self.expr.add(Expr::Red([lhs, rhs])),
                Rule::op_mvl => self.expr.add(Expr::MvL([lhs, rhs])),
                Rule::op_mvr => self.expr.add(Expr::MvR([lhs, rhs])),

                _ => unreachable!(),
            })
            .parse(pair.into_inner())
    }

    /// Parse a bind expression.
    fn parse_bind(&mut self, pair: Pair<'a, Rule>) -> (Symbol, Id) {
        let mut pairs = pair.into_inner();
        let name = pairs.next().unwrap();
        let expr = pairs.next().unwrap();
        assert!(pairs.next().is_none());

        (self.symbols.intern(name.as_str()), self.parse_expr(expr))
    }
}

#[derive(Copy, Clone, Debug)]
enum OldBind {
    Swap(Symbol, Id),
    Sole(Symbol),
}
