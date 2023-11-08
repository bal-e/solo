//! Parsing Solo code into an AST.

use std::iter;

use num_bigint::BigInt;
use pest::{self, iterators::{Pair, Pairs}};
use rustc_hash::FxHashMap;
use thiserror::Error;

use crate::storage::{self, *};
use crate::storage::share::ShareDisposition;
use crate::storage::syms::Symbol;

use crate::src::Rule;

use super::{*, Storage};

/// A parser for grammatical Solo code.
///
/// Given [`pest`]-parsed Solo code, this type can be used to construct an AST.
pub struct Parser<'ast, D: Disposition> {
    /// Storage for the AST.
    storage: &'ast Storage<ShareDisposition<D>>,

    /// A stack of scopes used for name resolution.
    scopes: Vec<FxHashMap<ID<Symbol>, ID<Variable>>>,
}

impl<'ast, D: Disposition> Parser<'ast, D>
where &'ast Storage<ShareDisposition<D>>
              : StoragePut<Variable>
              + StoragePut<Expr>
              + StoragePutTmp<Symbol>
              + SeqStoragePut<Stmt>
              + SeqStoragePut<Argument>
              + SeqStoragePut<Function> {
    /// Construct a new [`Parser`].
    pub fn new(storage: &'ast Storage<ShareDisposition<D>>) -> Self {
        Self {
            storage,
            scopes: Vec::new(),
        }
    }

    /// Parse a module.
    pub fn parse_mod(
        &mut self,
        input: Pair<'_, Rule>,
        name: &str,
        source: ModuleSource,
    ) -> Result<Module> {
        assert_eq!(Rule::r#mod, input.as_rule());
        let storage = self.storage;

        // Begin module scope.
        assert!(self.scopes.is_empty());
        self.scopes.push(FxHashMap::default());

        let name = storage.put_tmp(Symbol::new(name));
        let functions = input.into_inner()
            .filter(|p| p.as_rule() == Rule::r#fn)
            .map(|p| self.parse_fn(p));
        let functions = storage.try_put_seq(functions)?;

        // End module scope.
        self.scopes.pop();

        Ok(Module { name, functions, source })
    }

    /// Parse a function.
    pub fn parse_fn(
        &mut self,
        input: Pair<'_, Rule>,
    ) -> Result<Function> {
        assert_eq!(Rule::r#fn, input.as_rule());
        let storage = self.storage;

        // Begin function scope.
        self.scopes.push(FxHashMap::default());

        let mut pairs = input.into_inner().peekable();
        assert_eq!(Rule::fn_kw, pairs.next().unwrap().as_rule());
        let name = self.parse_name(pairs.next().unwrap());
        let args = iter::from_fn(|| pairs
            .next_if(|p| p.as_rule() == Rule::fn_arg)
            .map(|p| self.parse_fn_arg(p)));
        let args = storage.try_put_seq(args)?;
        let rett = self.parse_type(pairs.next().unwrap())?;
        let body = self.parse_expr_blk(pairs.next().unwrap())?;
        let body = storage.put(body);

        // End function scope.
        self.scopes.pop();

        Ok(Function { name, args, rett, body })
    }

    /// Parse a function argument.
    pub fn parse_fn_arg(
        &mut self,
        input: Pair<'_, Rule>,
    ) -> Result<Argument> {
        assert_eq!(Rule::fn_arg, input.as_rule());
        let storage = self.storage;

        let mut pairs = input.into_inner();
        let name = self.parse_name(pairs.next().unwrap());
        let r#type = self.parse_type(pairs.next().unwrap())?;

        // Register the argument as an expression and as a binding.
        let expr = storage.put(Expr::Arg);
        let variable = storage.put(Variable { name, expr });
        self.scopes.last_mut().unwrap().insert(name, variable);

        Ok(Argument { variable, r#type })
    }

    /// Parse a type.
    pub fn parse_type(
        &mut self,
        input: Pair<'_, Rule>,
    ) -> Result<Type> {
        assert_eq!(Rule::r#type, input.as_rule());

        let input_info = (input.as_str(), input.as_span());
        let mut pairs = input.into_inner().peekable();
        let stream = pairs
            .next_if(|p| p.as_rule() == Rule::type_stream)
            .is_some();
        let vector = pairs
            .next_if(|p| p.as_rule() == Rule::type_vector)
            .map(|p| p.into_inner().next().unwrap().as_str())
            .map(|p| p.parse::<u32>().map_err(|err| {
                let message = format!(
                    "The vector size component of '{}' is invalid: {}",
                    input_info.0, err);
                Error::Grammar(pest::error::Error::new_from_span(
                    pest::error::ErrorVariant::CustomError { message },
                    input_info.1))
            }))
            .transpose()?;
        let option = pairs
            .next_if(|p| p.as_rule() == Rule::type_option)
            .is_some();
        let scalar = self.parse_type_scalar(pairs.next().unwrap())?;

        Ok(Type { scalar, option, vector, stream })
    }

    /// Parse a scalar type.
    pub fn parse_type_scalar(
        &mut self,
        input: Pair<'_, Rule>,
    ) -> Result<ScalarType> {
        assert_eq!(Rule::type_scalar, input.as_rule());

        match input.into_inner().next().unwrap() {
            t if t.as_rule() == Rule::type_int =>
                self.parse_type_int(t).map(ScalarType::Int),
            _ => unreachable!(),
        }
    }

    /// Parse an integer scalar type.
    pub fn parse_type_int(
        &mut self,
        input: Pair<'_, Rule>,
    ) -> Result<IntType> {
        assert_eq!(Rule::type_int, input.as_rule());

        let (maker, size): (fn(_) -> _, _) = match input.as_str().split_at(1) {
            ("u", x) => (IntType::U, x),
            ("s", x) => (IntType::S, x),
            _ => unreachable!(),
        };

        let size = size.parse::<NonZeroU32>().map_err(|err| {
            let message = format!(
                "The size component of '{}' is invalid: {}",
                input.as_str(), err);
            Error::Grammar(pest::error::Error::new_from_span(
                pest::error::ErrorVariant::CustomError { message },
                input.as_span()))
        })?;

        Ok((maker)(size))
    }

    /// Parse an expression.
    pub fn parse_expr(
        &mut self,
        input: Pair<'_, Rule>,
    ) -> Result<Expr> {
        fn parse_inner<'ast, D: Disposition>(
            parser: &mut Parser<'ast, D>,
            prev: Option<&Pair<'_, Rule>>,
            input: &mut iter::Peekable<Pairs<'_, Rule>>,
        ) -> Result<Expr> {
            // The expression parsed thus far.
            let mut expr = parser.parse_expr_unit(input.next().unwrap())?;

            // Try parsing another binary operation.
            while let Some(next) = input.peek() {
                // Test that the operator is part of this expression.
                if binop_cmp(prev, next)? == Assoc::Left {
                    break;
                }

                let next = input.next().unwrap();
                let lhs = parser.storage.put(expr);
                let rhs = parse_inner(parser, Some(&next), input)?;
                let rhs = parser.storage.put(rhs);

                let binop = match next.into_inner().next().unwrap().as_rule() {
                    Rule::op_add => BinOp::Add,
                    Rule::op_sub => BinOp::Sub,
                    Rule::op_mul => BinOp::Mul,
                    Rule::op_div => BinOp::Div,
                    Rule::op_rem => BinOp::Rem,

                    Rule::op_and => BinOp::And,
                    Rule::op_ior => BinOp::IOr,
                    Rule::op_xor => BinOp::XOr,
                    Rule::op_shl => BinOp::ShL,
                    Rule::op_shr => BinOp::ShR,

                    Rule::op_cat => BinOp::Cat,
                    Rule::op_exp => BinOp::Exp,
                    Rule::op_red => BinOp::Red,

                    Rule::op_iseq => BinOp::Cmp(CmpOp::IsEq),
                    Rule::op_isne => BinOp::Cmp(CmpOp::IsNE),
                    Rule::op_islt => BinOp::Cmp(CmpOp::IsLT),
                    Rule::op_isle => BinOp::Cmp(CmpOp::IsLE),
                    Rule::op_isgt => BinOp::Cmp(CmpOp::IsGT),
                    Rule::op_isge => BinOp::Cmp(CmpOp::IsGE),

                    Rule::op_cond => BinOp::Cond,
                    Rule::op_else => BinOp::Else,

                    _ => unreachable!(),
                };

                // Construct the new binary expression.
                expr = Expr::Bin(binop, [lhs, rhs]);
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

            // 'lhs' had to exist for incompatibility to occur.
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
                Rule::op_cat => Prec::Concat,
                Rule::op_exp | Rule::op_red => Prec::ExpRed,
                Rule::op_iseq | Rule::op_isne => Prec::Compare,
                Rule::op_islt | Rule::op_isle => Prec::Compare,
                Rule::op_isgt | Rule::op_isge => Prec::Compare,
                Rule::op_cond => Prec::Cond,
                Rule::op_else => Prec::Else,
                _ => unreachable!(),
            }
        }

        assert_eq!(Rule::expr, input.as_rule());
        parse_inner(self, None, &mut input.into_inner().peekable())
    }

    /// Parse an indivisible expression.
    pub fn parse_expr_unit(
        &mut self,
        input: Pair<'_, Rule>,
    ) -> Result<Expr> {
        fn parse_pres<'ast, D: Disposition>(
            parser: &mut Parser<'ast, D>,
            mut input: Pairs<'_, Rule>,
        ) -> Result<Expr> {
            let pair = input.next().unwrap();
            match pair.as_rule() {
                Rule::op_pre => {
                    let expr = parse_pres(parser, input)?;
                    let expr = parser.storage.put(expr);
                    Ok(match pair.into_inner().next().unwrap().as_rule() {
                        Rule::op_not => Expr::Not(expr),
                        _ => unreachable!(),
                    })
                },
                Rule::atom => {
                    let expr = parser.parse_atom(pair)?;
                    input.try_fold(expr, |expr, op| {
                        let expr = parser.storage.put(expr);
                        match op.as_rule() {
                            Rule::op_ind => {
                                let ind = op.into_inner().next().unwrap();
                                let ind = parser.parse_expr(ind)?;
                                let ind = parser.storage.put(ind);
                                Ok(Expr::Ind([expr, ind]))
                            },
                            _ => unreachable!(),
                        }
                    })
                },
                _ => unreachable!(),
            }
        }

        assert_eq!(Rule::expr_unit, input.as_rule());
        parse_pres(self, input.into_inner())
    }

    /// Parse an atomic expression.
    pub fn parse_atom(
        &mut self,
        input: Pair<'_, Rule>,
    ) -> Result<Expr> {
        assert_eq!(Rule::atom, input.as_rule());
        let inner = input.into_inner().next().unwrap();
        Ok(match inner.as_rule() {
            Rule::expr_int => self.parse_expr_int(inner),
            Rule::expr_cst => self.parse_expr_cst(inner)?,
            Rule::expr_var => self.parse_expr_var(inner)?,
            Rule::expr_blk => self.parse_expr_blk(inner)?,
            Rule::expr => self.parse_expr(inner)?,
            _ => unreachable!(),
        })
    }

    /// Parse an integer literal expression.
    pub fn parse_expr_int(
        &mut self,
        input: Pair<'_, Rule>,
    ) -> Expr {
        assert_eq!(Rule::expr_int, input.as_rule());
        let value: BigInt = input.as_str().parse().unwrap();
        Expr::Int(self.storage.put(value))
    }

    /// Parse a cast expression.
    pub fn parse_expr_cst(
        &mut self,
        input: Pair<'_, Rule>,
    ) -> Result<Expr> {
        assert_eq!(Rule::expr_cst, input.as_rule());

        let mut pairs = input.into_inner();
        let r#type = self.parse_type(pairs.next().unwrap())?;
        let r#type = self.storage.put(r#type);
        let expr = self.parse_atom(pairs.next().unwrap())?;
        let expr = self.storage.put(expr);
        Ok(Expr::Cast(r#type, expr))
    }

    /// Parse a variable reference expression.
    pub fn parse_expr_var(
        &mut self,
        input: Pair<'_, Rule>,
    ) -> Result<Expr> {
        assert_eq!(Rule::expr_var, input.as_rule());

        let name = self.parse_name(input.into_inner().next().unwrap());

        // Resolve the variable identifier within the innermost scope.
        let variable = self.scopes.iter().rev()
            .find_map(|s| s.get(&name).copied())
            .ok_or_else(|| {
                let name = self.storage.get_ref(name);
                Error::Unresolved(name.to_string())
            })?;

        Ok(Expr::Var(variable))
    }

    /// Parse a block expression.
    pub fn parse_expr_blk(
        &mut self,
        input: Pair<'_, Rule>,
    ) -> Result<Expr> {
        assert_eq!(Rule::expr_blk, input.as_rule());
        let storage = self.storage;

        // Begin block scope.
        self.scopes.push(FxHashMap::default());

        let mut pairs = input.into_inner().peekable();
        let stmts = iter::from_fn(|| pairs
            .next_if(|p| p.as_rule() == Rule::stmt)
            .map(|p| self.parse_stmt(p)));
        let stmts = storage.try_put_seq(stmts)?;
        let rexpr = self.parse_expr(pairs.next().unwrap())?;
        let rexpr = storage.put(rexpr);

        // End block scope.
        self.scopes.pop();

        Ok(Expr::Blk { stmts, rexpr })
    }

    /// Parse a statement.
    pub fn parse_stmt(
        &mut self,
        input: Pair<'_, Rule>,
    ) -> Result<Stmt> {
        assert_eq!(Rule::stmt, input.as_rule());

        let inner = input.into_inner().next().unwrap();
        match inner.as_rule() {
            Rule::stmt_let => self.parse_stmt_let(inner),
            _ => unreachable!(),
        }
    }

    /// Parse a let statement.
    pub fn parse_stmt_let(
        &mut self,
        input: Pair<'_, Rule>,
    ) -> Result<Stmt> {
        assert_eq!(Rule::stmt_let, input.as_rule());
        let storage = self.storage;

        let mut pairs = input.into_inner();
        let name = self.parse_name(pairs.next().unwrap());
        let expr = self.parse_expr(pairs.next().unwrap())?;
        let expr = storage.put(expr);

        // Register the new binding.
        let variable = storage.put(Variable { name, expr });
        self.scopes.last_mut().unwrap().insert(name, variable);

        Ok(Stmt::Let(variable))
    }

    /// Parse a name.
    pub fn parse_name(&mut self, input: Pair<'_, Rule>) -> ID<Symbol> {
        assert_eq!(Rule::name, input.as_rule());
        self.storage.put_tmp(Symbol::new(input.as_str()))
    }
}

/// A parsing error.
#[derive(Debug, Error)]
pub enum Error {
    #[error("There was a grammatical error in the source code: {0}")]
    Grammar(#[from] pest::error::Error<Rule>),

    #[error("The variable '{0}' could not be found")]
    Unresolved(String),
}

/// A parsing result.
pub type Result<T> = std::result::Result<T, Error>;
