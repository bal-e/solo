//! Parsing MIR from source code.

use pest::{self, iterators::Pair, pratt_parser::PrattParser};

use egg::{Id, RecExpr};

use symbol_table::{Symbol, SymbolTable};

use rustc_hash::FxHashMap;

use crate::src::*;

use super::ExprNode;

/// A parser converting source code to MIR.
pub struct Parser<'a> {
    /// The MIR block being added to.
    expr: RecExpr<ExprNode>,
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
    /// Parse the given code as an MIR expression.
    pub fn parse_expr(code: &'a str) -> RecExpr<ExprNode> {
        let mut this = Self {
            expr: RecExpr::default(),
            code,
            binds: FxHashMap::default(),
            old_binds: Vec::new(),
            symbols: SymbolTable::new(),
        };
        let pratt = pratt_parser();

        let mut pairs = <SidParser as pest::Parser<Rule>>::parse(Rule::expr, code).unwrap();
        this.work_expr(pairs.next().unwrap(), &pratt);
        this.expr
    }

    /// Parse an MIR expression.
    fn work_expr(
        &mut self,
        pair: Pair<'a, Rule>,
        pratt: &PrattParser<Rule>,
    ) -> Id {
        pratt
            .map_primary(|p, this: &mut Self| match p.as_rule() {
                Rule::expr_int => {
                    let num = p.as_str().parse().unwrap();
                    this.expr.add(ExprNode::Int(num))
                },
                Rule::expr_var => {
                    let sym = this.symbols.intern(p.as_str());
                    *this.binds.get(&sym).expect("Variable not found!")
                },
                Rule::expr_arr => {
                    p.into_inner()
                        .fold(this.expr.add(ExprNode::NilArr), |prev, expr| {
                            let expr = this.work_expr(expr, pratt);
                            this.expr.add(ExprNode::Cat([prev, expr]))
                        })
                },
                Rule::expr_let => {
                    // Temporarily put all the binds in the secondary list
                    let mut to_add = 0;
                    let mut expr = None;
                    for item in p.into_inner() {
                        if item.as_rule() == Rule::bind {
                            let (sym, expr) = this.work_bind(item, pratt);
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
                    let res = this.work_expr(expr.unwrap(), pratt);

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
                Rule::expr => this.work_expr(p, pratt),
                _ => unreachable!(),
            })
            .map_prefix(|op, sub, this: &mut Self| match op.as_rule() {
                Rule::op_neg => this.expr.add(ExprNode::Neg([sub])),
                Rule::op_not => this.expr.add(ExprNode::Not([sub])),
                Rule::op_opt => this.expr.add(ExprNode::Opt([sub])),
                _ => unreachable!(),
            })
            .map_postfix(|sub, op, this: &mut Self| match op.as_rule() {
                Rule::op_arr_map => this.expr.add(ExprNode::MapArr([sub])),
                Rule::op_opt_map => this.expr.add(ExprNode::MapOpt([sub])),
                _ => unreachable!(),
            })
            .map_infix(|lhs, op, rhs, this: &mut Self| match op.as_rule() {
                Rule::op_add => this.expr.add(ExprNode::Add([lhs, rhs])),
                Rule::op_sub => this.expr.add(ExprNode::Sub([lhs, rhs])),
                Rule::op_mul => this.expr.add(ExprNode::Mul([lhs, rhs])),
                Rule::op_div => this.expr.add(ExprNode::Div([lhs, rhs])),
                Rule::op_rem => this.expr.add(ExprNode::Rem([lhs, rhs])),

                Rule::op_and => this.expr.add(ExprNode::And([lhs, rhs])),
                Rule::op_ior => this.expr.add(ExprNode::IOr([lhs, rhs])),
                Rule::op_xor => this.expr.add(ExprNode::XOr([lhs, rhs])),
                Rule::op_shl => this.expr.add(ExprNode::ShL([lhs, rhs])),
                Rule::op_shr => this.expr.add(ExprNode::ShR([lhs, rhs])),

                Rule::op_iseq => this.expr.add(ExprNode::IsEq([lhs, rhs])),
                Rule::op_isne => this.expr.add(ExprNode::IsNE([lhs, rhs])),
                Rule::op_islt => this.expr.add(ExprNode::IsLT([lhs, rhs])),
                Rule::op_isle => this.expr.add(ExprNode::IsLE([lhs, rhs])),
                Rule::op_isgt => this.expr.add(ExprNode::IsGT([lhs, rhs])),
                Rule::op_isge => this.expr.add(ExprNode::IsGE([lhs, rhs])),

                Rule::op_toeq => this.expr.add(ExprNode::ToEq([lhs, rhs])),
                Rule::op_tolt => this.expr.add(ExprNode::ToLT([lhs, rhs])),

                Rule::op_then => this.expr.add(ExprNode::Then([lhs, rhs])),
                Rule::op_else => this.expr.add(ExprNode::Else([lhs, rhs])),
                Rule::op_cat => this.expr.add(ExprNode::Cat([lhs, rhs])),
                Rule::op_exp => this.expr.add(ExprNode::Exp([lhs, rhs])),
                Rule::op_red => this.expr.add(ExprNode::Red([lhs, rhs])),
                Rule::op_mvl => this.expr.add(ExprNode::MvL([lhs, rhs])),
                Rule::op_mvr => this.expr.add(ExprNode::MvR([lhs, rhs])),

                _ => unreachable!(),
            })
            .parse(pair.into_inner(), self)
    }

    /// Parse a bind expression.
    fn work_bind(&mut self, pair: Pair<'a, Rule>, pratt: &PrattParser<Rule>) -> (Symbol, Id) {
        let mut pairs = pair.into_inner();
        let name = pairs.next().unwrap();
        let expr = pairs.next().unwrap();
        assert!(pairs.next().is_none());

        (self.symbols.intern(name.as_str()), self.work_expr(expr, pratt))
    }
}

#[derive(Copy, Clone, Debug)]
enum OldBind {
    Swap(Symbol, Id),
    Sole(Symbol),
}
