//! Parsing Solo code into an AST.

use std::iter;
use std::rc::Rc;

use pest::{self, iterators::{Pair, Pairs}};
use rustc_hash::FxHashMap;
use thiserror::Error;

use crate::src::Rule;

use super::*;

/// Parse the given module.
pub fn parse_mod(
    input: Pair<'_, Rule>,
    name: String,
    source: ModuleSource,
) -> Result<Module> {
    let mut parser = Parser::default();
    parser.parse_mod(input, name, source)
}

#[derive(Default)]
struct Parser {
    /// A stack of scopes used for name resolution.
    scopes: Vec<FxHashMap<String, Rc<Variable>>>,
}

impl Parser {
    /// Parse a module.
    fn parse_mod(
        &mut self,
        input: Pair<'_, Rule>,
        name: String,
        source: ModuleSource,
    ) -> Result<Module> {
        assert_eq!(Rule::r#mod, input.as_rule());

        assert!(self.scopes.is_empty());
        self.scopes.push(FxHashMap::default());

        let functions = input.into_inner()
            .filter(|p| p.as_rule() == Rule::r#fn)
            .map(|p| self.parse_fn(p))
            .collect::<Result<Vec<_>>>()?;

        self.scopes.pop();

        Ok(Module { name, functions, source })
    }

    /// Parse a function.
    fn parse_fn(
        &mut self,
        input: Pair<'_, Rule>,
    ) -> Result<Function> {
        assert_eq!(Rule::r#fn, input.as_rule());

        // Begin function scope.
        self.scopes.push(FxHashMap::default());

        let mut pairs = input.into_inner().peekable();
        assert_eq!(Rule::fn_kw, pairs.next().unwrap().as_rule());
        let name = self.parse_name(pairs.next().unwrap());
        let args = iter::from_fn(|| pairs
            .next_if(|p| p.as_rule() == Rule::fn_arg)
            .map(|p| self.parse_fn_arg(p)))
            .collect::<Result<Vec<_>>>()?;
        let rett = self.parse_type(pairs.next().unwrap())?;
        let body = self.parse_expr_blk(pairs.next().unwrap())?;

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

        let mut pairs = input.into_inner();
        let name = self.parse_name(pairs.next().unwrap());
        let r#type = self.parse_type(pairs.next().unwrap())?;

        // Register the argument as an expression and as a binding.
        let expr = Expr::Arg;
        let variable = Rc::new(Variable { name: name.clone(), expr });
        self.scopes.last_mut().unwrap().insert(name, variable.clone());

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
        assert_eq!(Rule::expr, input.as_rule());
        self.parse_expr_inner(None, &mut input.into_inner().peekable())
    }

    fn parse_expr_inner(
        &mut self,
        prev: Option<&Pair<'_, Rule>>,
        input: &mut iter::Peekable<Pairs<'_, Rule>>,
    ) -> Result<Expr> {
        // The expression parsed thus far.
        let mut expr = self.parse_expr_unit(input.next().unwrap())?;

        // Try parsing another binary operation.
        while let Some(next) = input.peek() {
            // Test that the operator is part of this expression.
            if Self::binop_cmp(prev, next)? == Assoc::Left {
                break;
            }

            let next = input.next().unwrap();
            let rhs = self.parse_expr_inner(Some(&next), input)?;

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
            expr = Expr::Bin(binop, [expr, rhs].map(Box::new));
        }

        Ok(expr)
    }

    /// Compare two binary operators for associativity.
    fn binop_cmp(
        lhs: Option<&Pair<'_, Rule>>,
        rhs: &Pair<'_, Rule>,
    ) -> Result<Assoc> {
        let lhs_prec = lhs.map_or(Prec::Min, Self::binop_prec);
        if let Some(assoc) = Prec::cmp(lhs_prec, Self::binop_prec(rhs)) {
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
            Rule::op_cat => Prec::CatInd,
            Rule::op_exp | Rule::op_red => Prec::ExpRed,
            Rule::op_iseq | Rule::op_isne => Prec::Compare,
            Rule::op_islt | Rule::op_isle => Prec::Compare,
            Rule::op_isgt | Rule::op_isge => Prec::Compare,
            Rule::op_cond => Prec::Cond,
            Rule::op_else => Prec::Else,
            _ => unreachable!(),
        }
    }

    /// Parse an indivisible expression.
    pub fn parse_expr_unit(
        &mut self,
        input: Pair<'_, Rule>,
    ) -> Result<Expr> {
        assert_eq!(Rule::expr_unit, input.as_rule());
        let mut pairs = input.into_inner().rev();

        let expr = self.parse_expr_sufs(pairs.next().unwrap())?;
        Ok(pairs.fold(expr, |expr, op| {
            assert_eq!(Rule::op_pre, op.as_rule());
            let op = op.into_inner().next().unwrap();
            let op = match op.as_rule() {
                Rule::op_neg => UnaOp::Neg,
                Rule::op_not => UnaOp::Not,
                _ => unreachable!(),
            };

            Expr::Una(op, Box::new(expr))
        }))
    }

    /// Parse an indivisible expression without prefixes.
    fn parse_expr_sufs(
        &mut self,
        input: Pair<'_, Rule>,
    ) -> Result<Expr> {
        assert_eq!(Rule::expr_sufs, input.as_rule());
        let mut pairs = input.into_inner();

        let expr = self.parse_atom(pairs.next().unwrap())?;
        pairs.try_fold(expr, |expr, op| {
            assert_eq!(Rule::op_suf, op.as_rule());
            let op = op.into_inner().next().unwrap();
            match op.as_rule() {
                Rule::op_ind => {
                    let ind = op.into_inner().next().unwrap();
                    let ind = self.parse_expr(ind)?;
                    Ok(Expr::Bin(BinOp::Ind, [expr, ind].map(Box::new)))
                },
                _ => unreachable!(),
            }
        })
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
        Expr::Int(input.as_str().parse().unwrap())
    }

    /// Parse a cast expression.
    pub fn parse_expr_cst(
        &mut self,
        input: Pair<'_, Rule>,
    ) -> Result<Expr> {
        assert_eq!(Rule::expr_cst, input.as_rule());
        let mut pairs = input.into_inner();

        let r#type = self.parse_type(pairs.next().unwrap())?;
        let expr = self.parse_expr_unit(pairs.next().unwrap())?;
        Ok(Expr::Cast(r#type, Box::new(expr)))
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
            .find_map(|s| s.get(&name).cloned())
            .ok_or_else(|| Error::Unresolved(name))?;

        Ok(Expr::Var(variable))
    }

    /// Parse a block expression.
    pub fn parse_expr_blk(
        &mut self,
        input: Pair<'_, Rule>,
    ) -> Result<Expr> {
        assert_eq!(Rule::expr_blk, input.as_rule());
        let mut pairs = input.into_inner().peekable();

        // Begin block scope.
        self.scopes.push(FxHashMap::default());

        let stmts = iter::from_fn(|| pairs
            .next_if(|p| p.as_rule() == Rule::stmt)
            .map(|p| self.parse_stmt(p)))
            .collect::<Result<Vec<_>>>()?;
        let rexpr = Box::new(self.parse_expr(pairs.next().unwrap())?);

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
        let mut pairs = input.into_inner();

        let name = self.parse_name(pairs.next().unwrap());
        let expr = self.parse_expr(pairs.next().unwrap())?;

        // Register the new binding.
        let variable = Rc::new(Variable { name: name.clone(), expr });
        self.scopes.last_mut().unwrap().insert(name, variable.clone());

        Ok(Stmt::Let(variable))
    }

    /// Parse a name.
    pub fn parse_name(
        &mut self,
        input: Pair<'_, Rule>,
    ) -> String {
        assert_eq!(Rule::name, input.as_rule());
        input.as_str().to_string()
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
