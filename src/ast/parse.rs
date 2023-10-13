//! Parsing Solo code into an AST.

use std::iter;

use pest::{self, iterators::{Pair, Pairs}};

use thiserror::Error;

use super::*;
use crate::src::Rule;

/// A parser for grammatical Solo code.
///
/// Given [`pest`]-parsed Solo code, this type can be used to construct an AST.
pub struct Parser<'ast> {
    /// Storage for the AST.
    storage: &'ast Storage<'ast>,
}

impl<'ast> Parser<'ast> {
    /// Construct a new [`Parser`].
    pub fn new(storage: &'ast Storage<'ast>) -> Self {
        Self { storage }
    }

    /// Parse a module.
    pub fn parse_module(
        &self,
        input: Pair<'_, Rule>,
        name: &str,
        source: ModSource<'ast>,
    ) -> Result<Mod<'ast>> {
        assert_eq!(Rule::module, input.as_rule());

        let name = self.storage.store_ref(name);
        let funcs = input.into_inner()
            .filter(|p| p.as_rule() == Rule::func)
            .map(|p| self.parse_func(p));
        let funcs = self.storage.store_try_many(funcs)?;

        Ok(Mod { name, funcs, source })
    }

    /// Parse a function.
    pub fn parse_func(&self, input: Pair<'_, Rule>) -> Result<Fn<'ast>> {
        assert_eq!(Rule::func, input.as_rule());

        let mut pairs = input.into_inner().peekable();
        assert_eq!(Rule::func_kw, pairs.next().unwrap().as_rule());
        let name = self.parse_name(pairs.next().unwrap());
        let args = iter::from_fn(|| pairs
            .next_if(|p| p.as_rule() == Rule::func_arg)
            .map(|p| self.parse_func_arg(p)));
        let args = self.storage.store_many(args);
        let rett = self.parse_type(pairs.next().unwrap());
        let body = self.parse_expr_blk(pairs.next().unwrap())?;
        let body = self.storage.store(body);

        Ok(Fn { name, args, rett, body })
    }

    /// Parse a function argument.
    pub fn parse_func_arg(&self, input: Pair<'_, Rule>) -> FnArg<'ast> {
        assert_eq!(Rule::func_arg, input.as_rule());

        let mut pairs = input.into_inner();
        let name = self.parse_name(pairs.next().unwrap());
        let r#type = self.parse_type(pairs.next().unwrap());

        FnArg { name, r#type }
    }

    /// Parse a type.
    pub fn parse_type(&self, input: Pair<'_, Rule>) -> Type {
        assert_eq!(Rule::r#type, input.as_rule());

        let mut pairs = input.into_inner().peekable();
        let stream = pairs
            .next_if(|p| p.as_rule() == Rule::type_stream)
            .is_some();
        let scalar = self.parse_type_scalar(pairs.next().unwrap());

        Type { scalar, stream }
    }

    /// Parse a scalar type.
    pub fn parse_type_scalar(&self, input: Pair<'_, Rule>) -> ScalarType {
        assert_eq!(Rule::type_scalar, input.as_rule());
        ScalarType::U64
    }

    /// Parse an expression.
    pub fn parse_expr(&self, input: Pair<'_, Rule>) -> Result<Expr<'ast>> {
        fn parse_inner<'ast>(
            parser: &Parser<'ast>,
            prev: Option<&Pair<'_, Rule>>,
            input: &mut iter::Peekable<Pairs<'_, Rule>>,
        ) -> Result<Expr<'ast>> {
            // The expression parsed thus far.
            let mut expr = parser.parse_expr_unit(input.next().unwrap())?;

            // Try parsing another binary operation.
            while let Some(next) = input.peek() {
                // Test that the operator is part of this expression.
                if binop_cmp(prev, next)? == Assoc::Left {
                    break;
                }

                let next = input.next().unwrap();
                let lhs = parser.storage.store(expr);
                let rhs = parse_inner(parser, Some(&next), input)?;
                let rhs = parser.storage.store(rhs);

                // Construct the new binary expression.
                expr = match next.into_inner().next().unwrap().as_rule() {
                    Rule::op_add => Expr::Add([lhs, rhs]),
                    Rule::op_sub => Expr::Sub([lhs, rhs]),
                    Rule::op_mul => Expr::Mul([lhs, rhs]),
                    Rule::op_div => Expr::Div([lhs, rhs]),
                    Rule::op_rem => Expr::Rem([lhs, rhs]),

                    Rule::op_and => Expr::And([lhs, rhs]),
                    Rule::op_ior => Expr::IOr([lhs, rhs]),
                    Rule::op_xor => Expr::XOr([lhs, rhs]),
                    Rule::op_shl => Expr::ShL([lhs, rhs]),
                    Rule::op_shr => Expr::ShR([lhs, rhs]),

                    Rule::op_iseq => Expr::IsEq([lhs, rhs]),
                    Rule::op_isne => Expr::IsNE([lhs, rhs]),
                    Rule::op_islt => Expr::IsLT([lhs, rhs]),
                    Rule::op_isle => Expr::IsLE([lhs, rhs]),
                    Rule::op_isgt => Expr::IsGT([lhs, rhs]),
                    Rule::op_isge => Expr::IsGE([lhs, rhs]),

                    _ => unreachable!(),
                };
            }

            Ok(expr)
        }

        /// Compare two binary operators for associativity.
        fn binop_cmp(
            lhs: Option<&Pair<'_, Rule>>,
            rhs: &Pair<'_, Rule>,
        ) -> Result<Assoc> {
            let lhs_prec = lhs.map_or(Prec::Min, binop_prec);
            if let Some(assoc) = Prec::cmp(lhs_prec, binop_prec(rhs)) {
                // The operators are compatible.
                return Ok(assoc);
            }

            // 'lhs' had to exist for incompatobility to occur.
            let lhs = lhs.unwrap();

            let message = format!(
                "The operators '{}' and '{}' are incompatible - wrap one or \
                 the other in parentheses.",
                lhs.as_str(), rhs.as_str());
            Err(Error::Grammar(pest::error::Error::new_from_span(
                pest::error::ErrorVariant::CustomError { message },
                lhs.as_span().start_pos().span(&rhs.as_span().end_pos()),
            )))
        }

        /// The precedence of a binary operator.
        fn binop_prec(input: &Pair<'_, Rule>) -> Prec {
            assert_eq!(Rule::op_bin, input.as_rule());
            match input.clone().into_inner().next().unwrap().as_rule() {
                Rule::op_add | Rule::op_sub => Prec::AddSub,
                Rule::op_mul | Rule::op_div | Rule::op_rem => Prec::MulDiv,
                Rule::op_and | Rule::op_ior | Rule::op_xor => Prec::Bitwise,
                Rule::op_shl | Rule::op_shr => Prec::Shift,
                Rule::op_iseq | Rule::op_isne => Prec::Compare,
                Rule::op_islt | Rule::op_isle => Prec::Compare,
                Rule::op_isgt | Rule::op_isge => Prec::Compare,
                _ => unreachable!(),
            }
        }

        assert_eq!(Rule::expr, input.as_rule());
        parse_inner(self, None, &mut input.into_inner().peekable())
    }

    /// Parse an indivisible expression.
    pub fn parse_expr_unit(&self, input: Pair<'_, Rule>) -> Result<Expr<'ast>> {
        fn parse_pres<'ast>(
            parser: &Parser<'ast>,
            mut input: Pairs<'_, Rule>,
        ) -> Result<Expr<'ast>> {
            let pair = input.next().unwrap();
            match pair.as_rule() {
                Rule::op_pre => {
                    let expr = parse_pres(parser, input)?;
                    let expr = parser.storage.store(expr);
                    Ok(match pair.into_inner().next().unwrap().as_rule() {
                        Rule::op_not => Expr::Not(expr),
                        _ => unreachable!(),
                    })
                },
                Rule::atom => {
                    let expr = parser.parse_atom(pair)?;
                    Ok(input.fold(expr, |expr, op| {
                        let _expr = parser.storage.store(expr);
                        match op.as_rule() {
                            _ => unreachable!(),
                        }
                    }))
                },
                _ => unreachable!(),
            }
        }

        assert_eq!(Rule::expr_unit, input.as_rule());
        parse_pres(self, input.into_inner())
    }

    /// Parse an atomic expression.
    pub fn parse_atom(&self, input: Pair<'_, Rule>) -> Result<Expr<'ast>> {
        assert_eq!(Rule::atom, input.as_rule());
        let inner = input.into_inner().next().unwrap();
        Ok(match inner.as_rule() {
            Rule::expr_int => self.parse_expr_int(inner),
            Rule::expr_var => self.parse_expr_var(inner),
            Rule::expr_blk => self.parse_expr_blk(inner)?,
            Rule::expr => self.parse_expr(inner)?,
            _ => unreachable!(),
        })
    }

    /// Parse an integer literal expression.
    pub fn parse_expr_int(&self, input: Pair<'_, Rule>) -> Expr<'ast> {
        assert_eq!(Rule::expr_int, input.as_rule());
        let value: BigInt = input.as_str().parse().unwrap();
        Expr::Int(self.storage.store(value))
    }

    /// Parse a variable reference expression.
    pub fn parse_expr_var(&self, input: Pair<'_, Rule>) -> Expr<'ast> {
        assert_eq!(Rule::expr_var, input.as_rule());
        let name = self.parse_name(input.into_inner().next().unwrap());
        Expr::Var(name)
    }

    /// Parse a block expression.
    pub fn parse_expr_blk(&self, input: Pair<'_, Rule>) -> Result<Expr<'ast>> {
        assert_eq!(Rule::expr_blk, input.as_rule());

        let mut pairs = input.into_inner().peekable();
        let mut error = None;
        let stmts = iter::from_fn(|| pairs
            .next_if(|p| p.as_rule() == Rule::stmt)
            .and_then(|p| self.parse_stmt(p)
                .map_or_else(|err| { error = Some(err); None }, Some)));
        let stmts = self.storage.store_many(stmts);
        if let Some(error) = error { return Err(error); }
        let rexpr = self.parse_expr(pairs.next().unwrap())?;
        let rexpr = self.storage.store(rexpr);

        Ok(Expr::Blk { stmts, rexpr })
    }

    /// Parse a statement.
    pub fn parse_stmt(&self, input: Pair<'_, Rule>) -> Result<Stmt<'ast>> {
        assert_eq!(Rule::stmt, input.as_rule());

        let inner = input.into_inner().next().unwrap();
        match inner.as_rule() {
            Rule::stmt_let => self.parse_stmt_let(inner),
            _ => unreachable!(),
        }
    }

    /// Parse a let statement.
    pub fn parse_stmt_let(&self, input: Pair<'_, Rule>) -> Result<Stmt<'ast>> {
        assert_eq!(Rule::stmt_let, input.as_rule());

        let mut pairs = input.into_inner();
        let name = self.parse_name(pairs.next().unwrap());
        let expr = self.parse_expr(pairs.next().unwrap())?;
        let expr = self.storage.store(expr);

        Ok(Stmt::Let(name, expr))
    }

    /// Parse a name.
    pub fn parse_name(&self, input: Pair<'_, Rule>) -> &'ast str {
        assert_eq!(Rule::name, input.as_rule());
        self.storage.store_ref(input.as_str())
    }
}

/// A parsing error.
#[derive(Debug, Error)]
pub enum Error {
    #[error("There was a grammatical error in the source code: {0}")]
    Grammar(#[from] pest::error::Error<Rule>),
}

/// A parsing result.
pub type Result<T> = std::result::Result<T, Error>;
