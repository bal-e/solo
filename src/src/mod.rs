///! Source code for sid.

use pest_derive::Parser;
use pest::pratt_parser::{Assoc, Op, PrattParser};

#[derive(Parser)]
#[grammar = "src/sid.pest"]
pub struct SidParser;

/// Construct a [`PrattParser`] for parsing expressions.
///
/// The same parser can be used multiple times, and creating it requires heap
/// allocation; cache and reuse it as much as possible.
pub fn pratt_parser() -> PrattParser<Rule> {
    PrattParser::new()
        .op(Op::infix(Rule::op_then, Assoc::Left)
          | Op::infix(Rule::op_else, Assoc::Left))
        .op(Op::infix(Rule::op_iseq, Assoc::Left)
          | Op::infix(Rule::op_isne, Assoc::Left)
          | Op::infix(Rule::op_isle, Assoc::Left)
          | Op::infix(Rule::op_islt, Assoc::Left)
          | Op::infix(Rule::op_isge, Assoc::Left)
          | Op::infix(Rule::op_isgt, Assoc::Left))
        .op(Op::infix(Rule::op_add, Assoc::Left)
          | Op::infix(Rule::op_sub, Assoc::Left))
        .op(Op::infix(Rule::op_mul, Assoc::Left)
          | Op::infix(Rule::op_div, Assoc::Left)
          | Op::infix(Rule::op_rem, Assoc::Left))
        .op(Op::infix(Rule::op_ior, Assoc::Left))
        .op(Op::infix(Rule::op_and, Assoc::Left))
        .op(Op::infix(Rule::op_xor, Assoc::Left))
        .op(Op::infix(Rule::op_shl, Assoc::Left)
          | Op::infix(Rule::op_shr, Assoc::Left)
          | Op::infix(Rule::op_mvl, Assoc::Left)
          | Op::infix(Rule::op_mvr, Assoc::Left))
        .op(Op::infix(Rule::op_cat, Assoc::Left)
          | Op::infix(Rule::op_exp, Assoc::Left)
          | Op::infix(Rule::op_red, Assoc::Left))
        .op(Op::infix(Rule::op_toeq, Assoc::Left)
          | Op::infix(Rule::op_tolt, Assoc::Left))
        .op(Op::prefix(Rule::op_neg)
          | Op::prefix(Rule::op_not)
          | Op::prefix(Rule::op_opt))
        .op(Op::postfix(Rule::op_arr_map)
          | Op::postfix(Rule::op_opt_map))
}
