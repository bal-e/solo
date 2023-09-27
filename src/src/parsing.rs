use std::iter;

use pest;
use pest::iterators::Pair;
use pest::pratt_parser::{Assoc, Op, PrattParser};
use pest_derive::Parser;

use super::*;

#[derive(Parser)]
#[grammar = "src/solo.pest"]
pub struct SoloParser;

/// A Pratt Parser for Solo.
pub type SoloPrattParser = PrattParser<Rule>;

/// Construct a new [`SoloPrattParser`].
pub fn new_pratt() -> SoloPrattParser {
    SoloPrattParser::new()
        .op(Op::infix(Rule::op_iseq, Assoc::Left)
          | Op::infix(Rule::op_isne, Assoc::Left)
          | Op::infix(Rule::op_islt, Assoc::Left)
          | Op::infix(Rule::op_isle, Assoc::Left)
          | Op::infix(Rule::op_isgt, Assoc::Left)
          | Op::infix(Rule::op_isge, Assoc::Left))
        .op(Op::infix(Rule::op_shl, Assoc::Left)
          | Op::infix(Rule::op_shr, Assoc::Left))
        .op(Op::infix(Rule::op_add, Assoc::Left)
          | Op::infix(Rule::op_sub, Assoc::Left))
        .op(Op::infix(Rule::op_ior, Assoc::Left)
          | Op::infix(Rule::op_div, Assoc::Left))
        .op(Op::infix(Rule::op_and, Assoc::Left)
          | Op::infix(Rule::op_xor, Assoc::Left)
          | Op::infix(Rule::op_mul, Assoc::Left)
          | Op::infix(Rule::op_rem, Assoc::Left))
        .op(Op::prefix(Rule::op_not))
}

/// The type of parser errors.
pub type Error = pest::error::Error<Rule>;

/// Parse a module.
pub fn parse_module<'i, 'a>(
    storage: &Storage<'a>,
    input: Pair<'i, Rule>,
    name: Symbol,
    source: ModuleSource<'a>,
) -> Module<'a> {
    assert_eq!(Rule::module, input.as_rule());

    let funcs = input.into_inner()
        .filter(|p| p.as_rule() == Rule::func)
        .map(|p| parse_func(storage, p));
    let funcs = storage.funcs.alloc_extend(funcs);

    Module { name, funcs, source }
}

/// Parse a function.
pub fn parse_func<'i, 'a>(
    storage: &Storage<'a>,
    input: Pair<'i, Rule>,
) -> Function<'a> {
    assert_eq!(Rule::func, input.as_rule());

    let mut pairs = input.into_inner().peekable();
    assert_eq!(Rule::func_kw, pairs.next().unwrap().as_rule());
    let name = parse_name(storage, pairs.next().unwrap());
    let args = iter::from_fn(|| pairs
        .next_if(|p| p.as_rule() == Rule::func_arg)
        .map(|p| parse_func_arg(storage, p)));
    let args = storage.func_args.alloc_extend(args);
    let rett = parse_type(storage, pairs.next().unwrap());
    let body = parse_expr_blk(storage, pairs.next().unwrap());
    let body = storage.exprs.alloc(body);

    Function { name, args, rett, body }
}

/// Parse a function argument.
pub fn parse_func_arg<'i, 'a>(
    storage: &Storage<'a>,
    input: Pair<'i, Rule>,
) -> (Symbol, Type) {
    assert_eq!(Rule::func_arg, input.as_rule());

    let mut pairs = input.into_inner();
    let name = parse_name(storage, pairs.next().unwrap());
    let r#type = parse_type(storage, pairs.next().unwrap());

    (name, r#type)
}

/// Parse a type.
pub fn parse_type<'i, 'a>(
    storage: &Storage<'a>,
    input: Pair<'i, Rule>,
) -> Type {
    assert_eq!(Rule::r#type, input.as_rule());

    let mut pairs = input.into_inner().peekable();
    let stream = pairs
        .next_if(|p| p.as_rule() == Rule::type_stream)
        .is_some();
    let scalar = parse_type_scalar(storage, pairs.next().unwrap());

    Type { scalar, stream }
}

/// Parse a scalar type.
pub fn parse_type_scalar<'i, 'a>(
    _: &Storage<'a>,
    input: Pair<'i, Rule>,
) -> ScalarType {
    assert_eq!(Rule::type_scalar, input.as_rule());
    ScalarType::U64
}

/// Parse an expression.
pub fn parse_expr<'i, 'a>(
    storage: &Storage<'a>,
    input: Pair<'i, Rule>,
) -> Expr<'a> {
    storage.pratt
        .map_primary(|p| match p.as_rule() {
            Rule::expr_int => Expr::Int(p.as_str().parse().unwrap()),
            Rule::expr_var => parse_expr_var(storage, p),
            Rule::expr_blk => parse_expr_blk(storage, p),
            Rule::expr => parse_expr(storage, p),
            _ => unreachable!(),
        })
        .map_prefix(|p, rhs| {
            let rhs = storage.exprs.alloc(rhs);
            match p.as_rule() {
                Rule::op_not => Expr::Not(rhs),
                _ => unreachable!()
            }
        })
        .map_postfix(|lhs, p| {
            let _ = storage.exprs.alloc(lhs);
            match p.as_rule() {
                _ => unreachable!(),
            }
        })
        .map_infix(|lhs, p, rhs| {
            let lhs = storage.exprs.alloc(lhs);
            let rhs = storage.exprs.alloc(rhs);
            match p.as_rule() {
                Rule::op_add => Expr::Add(lhs, rhs),
                Rule::op_sub => Expr::Sub(lhs, rhs),
                Rule::op_mul => Expr::Mul(lhs, rhs),
                Rule::op_div => Expr::Div(lhs, rhs),
                Rule::op_rem => Expr::Rem(lhs, rhs),

                Rule::op_and => Expr::And(lhs, rhs),
                Rule::op_ior => Expr::IOr(lhs, rhs),
                Rule::op_xor => Expr::XOr(lhs, rhs),
                Rule::op_shl => Expr::ShL(lhs, rhs),
                Rule::op_shr => Expr::ShR(lhs, rhs),

                Rule::op_iseq => Expr::IsEq(lhs, rhs),
                Rule::op_isne => Expr::IsNE(lhs, rhs),
                Rule::op_islt => Expr::IsLT(lhs, rhs),
                Rule::op_isle => Expr::IsLE(lhs, rhs),
                Rule::op_isgt => Expr::IsGT(lhs, rhs),
                Rule::op_isge => Expr::IsGE(lhs, rhs),

                _ => unreachable!(),
            }
        })
        .parse(input.into_inner())
}

/// Parse a variable reference expression.
pub fn parse_expr_var<'i, 'a>(
    storage: &Storage<'a>,
    input: Pair<'i, Rule>,
) -> Expr<'a> {
    assert_eq!(Rule::expr_var, input.as_rule());
    let name = parse_name(storage, input.into_inner().next().unwrap());
    Expr::Var(name)
}

/// Parse a block expression.
pub fn parse_expr_blk<'i, 'a>(
    storage: &Storage<'a>,
    input: Pair<'i, Rule>,
) -> Expr<'a> {
    assert_eq!(Rule::expr_blk, input.as_rule());

    let mut pairs = input.into_inner().peekable();
    let stmts = iter::from_fn(|| pairs
        .next_if(|p| p.as_rule() == Rule::stmt)
        .map(|p| parse_stmt(storage, p)));
    let stmts = storage.stmts.alloc_extend(stmts);
    let expr = parse_expr(storage, pairs.next().unwrap());
    let expr = storage.exprs.alloc(expr);

    Expr::Blk(stmts, expr)
}

/// Parse a statement.
pub fn parse_stmt<'i, 'a>(
    storage: &Storage<'a>,
    input: Pair<'i, Rule>,
) -> Stmt<'a> {
    assert_eq!(Rule::stmt, input.as_rule());

    let inner = input.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::stmt_let => parse_stmt_let(storage, inner),
        _ => unreachable!(),
    }
}

/// Parse a let statement.
pub fn parse_stmt_let<'i, 'a>(
    storage: &Storage<'a>,
    input: Pair<'i, Rule>,
) -> Stmt<'a> {
    assert_eq!(Rule::stmt_let, input.as_rule());

    let mut pairs = input.into_inner();
    let name = parse_name(storage, pairs.next().unwrap());
    let expr = parse_expr(storage, pairs.next().unwrap());
    let expr = storage.exprs.alloc(expr);

    Stmt::Let(name, expr)
}

/// Parse a name.
pub fn parse_name<'i, 'a>(
    storage: &Storage<'a>,
    input: Pair<'i, Rule>,
) -> Symbol {
    assert_eq!(Rule::name, input.as_rule());
    storage.syms.intern(input.as_str())
}
