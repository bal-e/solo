//! Parsing Solo code into an AST.

use std::iter;
use std::ops::RangeFrom;
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

struct Parser {
    /// Available variable IDs.
    variable_ids: RangeFrom<u32>,

    /// Available statement IDs.
    stmt_ids: RangeFrom<u32>,

    /// Available expression IDs.
    expr_ids: RangeFrom<u32>,

    /// A stack of scopes used for name resolution.
    scopes: Vec<FxHashMap<String, Rc<Stored<Variable>>>>,
}

impl Default for Parser {
    fn default() -> Self {
        Self {
            variable_ids: 0 ..,
            stmt_ids: 0 ..,
            expr_ids: 0 ..,
            scopes: Vec::new(),
        }
    }
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

        let variable_id_beg = self.variable_ids.start;
        let stmt_id_beg = self.stmt_ids.start;
        let expr_id_beg = self.expr_ids.start;

        let mut pairs = input.into_inner().peekable();
        assert_eq!(Rule::fn_kw, pairs.next().unwrap().as_rule());
        let name = self.parse_name(pairs.next().unwrap());
        let args = iter::from_fn(|| pairs
            .next_if(|p| p.as_rule() == Rule::fn_arg)
            .map(|p| self.parse_fn_arg(p)))
            .collect::<Result<Vec<_>>>()?;
        let rett = self.parse_type(pairs.next().unwrap())?;
        let body = self.parse_expr_blk(pairs.next().unwrap())?;
        let body = Box::new(self.store_expr(body));

        let variable_id_end = self.variable_ids.start;
        let stmt_id_end = self.stmt_ids.start;
        let expr_id_end = self.expr_ids.start;

        // End function scope.
        self.scopes.pop();

        let variable_ids = variable_id_beg .. variable_id_end;
        let stmt_ids = stmt_id_beg .. stmt_id_end;
        let expr_ids = expr_id_beg .. expr_id_end;

        Ok(Function {
            name, args, rett, body,
            variable_ids, stmt_ids, expr_ids,
        })
    }

    /// Parse a function argument.
    fn parse_fn_arg(
        &mut self,
        input: Pair<'_, Rule>,
    ) -> Result<Argument> {
        assert_eq!(Rule::fn_arg, input.as_rule());

        let mut pairs = input.into_inner();
        let name = self.parse_name(pairs.next().unwrap());
        let r#type = self.parse_type(pairs.next().unwrap())?;

        // Register the argument as an expression and as a binding.
        let expr = Box::new(self.store_expr(Expr::Arg));
        let variable = Variable { name: name.clone(), expr };
        let variable = Rc::new(self.store_variable(variable));
        self.scopes.last_mut().unwrap().insert(name, variable.clone());

        Ok(Argument { variable, r#type })
    }

    /// Parse a type.
    fn parse_type(
        &mut self,
        input: Pair<'_, Rule>,
    ) -> Result<MappedType> {
        assert_eq!(Rule::r#type, input.as_rule());

        let input_info = (input.as_str(), input.as_span());
        let mut pairs = input.into_inner().peekable();
        let stream = pairs
            .next_if(|p| p.as_rule() == Rule::type_stream)
            .map(|_| StreamPart {});
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
            .transpose()?
            .map(|size| VectorPart { size });
        let option = pairs
            .next_if(|p| p.as_rule() == Rule::type_option)
            .map(|_| OptionPart {});
        let scalar = self.parse_type_scalar(pairs.next().unwrap())?;

        Ok(MappedType { stream, vector, option, scalar })
    }

    /// Parse a scalar type.
    fn parse_type_scalar(
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
    fn parse_type_int(
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
    fn parse_expr(
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
            let rhs = Box::new(self.store_expr(rhs));

            let bop = match next.into_inner().next().unwrap().as_rule() {
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
            let lhs = Box::new(self.store_expr(expr));
            expr = Expr::Bin(bop, [lhs, rhs]);
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
    fn parse_expr_unit(
        &mut self,
        input: Pair<'_, Rule>,
    ) -> Result<Expr> {
        assert_eq!(Rule::expr_unit, input.as_rule());
        let mut pairs = input.into_inner().rev();

        let expr = self.parse_expr_sufs(pairs.next().unwrap())?;
        Ok(pairs.fold(expr, |expr, uop| {
            assert_eq!(Rule::op_pre, uop.as_rule());
            let uop = uop.into_inner().next().unwrap();
            let uop = match uop.as_rule() {
                Rule::op_neg => UnaOp::Neg,
                Rule::op_not => UnaOp::Not,
                _ => unreachable!(),
            };

            Expr::Una(uop, Box::new(self.store_expr(expr)))
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
                    let lhs = Box::new(self.store_expr(expr));
                    let rhs = Box::new(self.store_expr(ind));
                    Ok(Expr::Bin(BinOp::Ind, [lhs, rhs]))
                },
                _ => unreachable!(),
            }
        })
    }

    /// Parse an atomic expression.
    fn parse_atom(
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
    fn parse_expr_int(
        &mut self,
        input: Pair<'_, Rule>,
    ) -> Expr {
        assert_eq!(Rule::expr_int, input.as_rule());
        Expr::Int(input.as_str().parse().unwrap())
    }

    /// Parse a cast expression.
    fn parse_expr_cst(
        &mut self,
        input: Pair<'_, Rule>,
    ) -> Result<Expr> {
        assert_eq!(Rule::expr_cst, input.as_rule());
        let mut pairs = input.into_inner();

        let r#type = self.parse_type(pairs.next().unwrap())?;
        let expr = self.parse_expr_unit(pairs.next().unwrap())?;
        let expr = Box::new(self.store_expr(expr));
        Ok(Expr::Cast(r#type, expr))
    }

    /// Parse a variable reference expression.
    fn parse_expr_var(
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
    fn parse_expr_blk(
        &mut self,
        input: Pair<'_, Rule>,
    ) -> Result<Expr> {
        assert_eq!(Rule::expr_blk, input.as_rule());
        let mut pairs = input.into_inner().peekable();

        // Begin block scope.
        self.scopes.push(FxHashMap::default());

        let stmts = iter::from_fn(|| pairs
            .next_if(|p| p.as_rule() == Rule::stmt))
            .map(|p| self.parse_stmt(p)
                 .map(|s| self.store_stmt(s)))
            .collect::<Result<Vec<_>>>()?;
        let rexpr = self.parse_expr(pairs.next().unwrap())?;
        let rexpr = Box::new(self.store_expr(rexpr));

        // End block scope.
        self.scopes.pop();

        Ok(Expr::Blk { stmts, rexpr })
    }

    /// Parse a statement.
    fn parse_stmt(
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
    fn parse_stmt_let(
        &mut self,
        input: Pair<'_, Rule>,
    ) -> Result<Stmt> {
        assert_eq!(Rule::stmt_let, input.as_rule());
        let mut pairs = input.into_inner();

        let name = self.parse_name(pairs.next().unwrap());
        let expr = self.parse_expr(pairs.next().unwrap())?;
        let expr = Box::new(self.store_expr(expr));

        // Register the new binding.
        let variable = Variable { name: name.clone(), expr };
        let variable = Rc::new(self.store_variable(variable));
        self.scopes.last_mut().unwrap().insert(name, variable.clone());

        Ok(Stmt::Let(variable))
    }

    /// Parse a name.
    fn parse_name(
        &mut self,
        input: Pair<'_, Rule>,
    ) -> String {
        assert_eq!(Rule::name, input.as_rule());
        input.as_str().to_string()
    }

    /// Store a variable.
    fn store_variable(&mut self, variable: Variable) -> Stored<Variable> {
        let ident = self.variable_ids.next().unwrap();
        Stored { ident, inner: variable }
    }

    /// Store a statement.
    fn store_stmt(&mut self, stmt: Stmt) -> Stored<Stmt> {
        let ident = self.stmt_ids.next().unwrap();
        Stored { ident, inner: stmt }
    }

    /// Store an expression.
    fn store_expr(&mut self, expr: Expr) -> Stored<Expr> {
        let ident = self.expr_ids.next().unwrap();
        Stored { ident, inner: expr }
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
