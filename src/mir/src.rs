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
    /// A list of variable bindings in scope.
    binds: FxHashMap<Symbol, Id>,
    /// Variable bindings from outer scopes.
    old_binds: Vec<OldBind>,
    /// A [`SymbolTable`] for identifiers.
    symbols: SymbolTable,
}

impl<'a> Parser<'a> {
    /// Parse an MIR expression.
    fn parse_expr(&mut self, pair: Pair<'a, Rule>, pratt: &PrattParser<Rule>) -> Id {
        pratt
            .map_primary(|p, this: &mut Self| match p.as_rule() {
                Rule::expr_int => {
                    let num = p.as_str().parse().unwrap();
                    this.expr.add(Expr::Int(num))
                },
                Rule::expr_var => {
                    let sym = this.symbols.intern(p.as_str());
                    *this.binds.get(&sym).expect("Variable not found!")
                },
                Rule::expr_arr => {
                    p.into_inner()
                        .fold(this.expr.add(Expr::NilArr), |prev, expr| {
                            let expr = this.parse_expr(expr, pratt);
                            this.expr.add(Expr::Cat([prev, expr]))
                        })
                },
                Rule::expr_let => {
                    // Temporarily put all the binds in the secondary list
                    let mut to_add = 0;
                    let mut expr = None;
                    for item in p.into_inner() {
                        if item.as_rule() == Rule::bind {
                            let (sym, expr) = this.parse_bind(item, pratt);
                            this.old_binds.push(OldBind::Swap(sym, expr));
                            to_add += 1;
                        } else {
                            // Parse later, once bindings are applied
                            expr = Some(item);
                        }
                    }

                    // Move the binds into the hashmap atomically (wrt parsing)
                    let off = this.old_binds.len() - to_add;
                    for old_bind in this.old_binds[off ..].iter_mut() {
                        let OldBind::Swap(sym, expr) = *old_bind
                            else { unreachable!() };
                        *old_bind = OldBind::Sole(sym);
                        this.binds.entry(sym)
                            .and_modify(|old_expr| {
                                let expr = std::mem::replace(old_expr, expr);
                                *old_bind = OldBind::Swap(sym, expr);
                            })
                            .or_insert(expr);
                    }

                    // Parse the subexpression with the updated context
                    let res = this.parse_expr(expr.unwrap(), pratt);

                    // Restore the previous binds
                    let len = this.old_binds.len();
                    for bind in this.old_binds.drain(off .. len) {
                        match bind {
                            OldBind::Swap(sym, expr) =>
                                this.binds.insert(sym, expr),
                            OldBind::Sole(sym) =>
                                this.binds.remove(&sym),
                        };
                    }

                    res
                },
                Rule::expr => this.parse_expr(p, pratt),
                _ => unreachable!(),
            })
            .map_prefix(|op, sub, this: &mut Self| match op.as_rule() {
                Rule::op_neg => this.expr.add(Expr::Neg([sub])),
                Rule::op_not => this.expr.add(Expr::Not([sub])),
                Rule::op_opt => this.expr.add(Expr::Opt([sub])),
                _ => unreachable!(),
            })
            .map_postfix(|sub, op, this: &mut Self| match op.as_rule() {
                Rule::op_arr_map => this.expr.add(Expr::MapArr([sub])),
                Rule::op_opt_map => this.expr.add(Expr::MapOpt([sub])),
                _ => unreachable!(),
            })
            .map_infix(|lhs, op, rhs, this: &mut Self| match op.as_rule() {
                Rule::op_add => this.expr.add(Expr::Add([lhs, rhs])),
                Rule::op_sub => this.expr.add(Expr::Sub([lhs, rhs])),
                Rule::op_mul => this.expr.add(Expr::Mul([lhs, rhs])),
                Rule::op_div => this.expr.add(Expr::Div([lhs, rhs])),
                Rule::op_rem => this.expr.add(Expr::Rem([lhs, rhs])),

                Rule::op_and => this.expr.add(Expr::And([lhs, rhs])),
                Rule::op_ior => this.expr.add(Expr::IOr([lhs, rhs])),
                Rule::op_xor => this.expr.add(Expr::XOr([lhs, rhs])),
                Rule::op_shl => this.expr.add(Expr::ShL([lhs, rhs])),
                Rule::op_shr => this.expr.add(Expr::ShR([lhs, rhs])),

                Rule::op_iseq => this.expr.add(Expr::IsEq([lhs, rhs])),
                Rule::op_isne => this.expr.add(Expr::IsNE([lhs, rhs])),
                Rule::op_islt => this.expr.add(Expr::IsLT([lhs, rhs])),
                Rule::op_isle => this.expr.add(Expr::IsLE([lhs, rhs])),
                Rule::op_isgt => this.expr.add(Expr::IsGT([lhs, rhs])),
                Rule::op_isge => this.expr.add(Expr::IsGE([lhs, rhs])),

                Rule::op_toeq => this.expr.add(Expr::ToEq([lhs, rhs])),
                Rule::op_tolt => this.expr.add(Expr::ToLT([lhs, rhs])),

                Rule::op_then => this.expr.add(Expr::Then([lhs, rhs])),
                Rule::op_else => this.expr.add(Expr::Else([lhs, rhs])),
                Rule::op_cat => this.expr.add(Expr::Cat([lhs, rhs])),
                Rule::op_exp => this.expr.add(Expr::Exp([lhs, rhs])),
                Rule::op_red => this.expr.add(Expr::Red([lhs, rhs])),
                Rule::op_mvl => this.expr.add(Expr::MvL([lhs, rhs])),
                Rule::op_mvr => this.expr.add(Expr::MvR([lhs, rhs])),

                _ => unreachable!(),
            })
            .parse(pair.into_inner(), self)
    }

    /// Parse a bind expression.
    fn parse_bind(&mut self, pair: Pair<'a, Rule>, pratt: &PrattParser<Rule>) -> (Symbol, Id) {
        let mut pairs = pair.into_inner();
        let name = pairs.next().unwrap();
        let expr = pairs.next().unwrap();
        assert!(pairs.next().is_none());

        (self.symbols.intern(name.as_str()), self.parse_expr(expr, pratt))
    }
}

#[derive(Copy, Clone, Debug)]
enum OldBind {
    Swap(Symbol, Id),
    Sole(Symbol),
}
