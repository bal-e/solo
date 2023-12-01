//! Parsing Solo code into an AST.

use core::iter;
use core::num::NonZeroU32;

use pest::{self, iterators::{Pair, Pairs}};
use rustc_hash::FxHashMap;
use thiserror::Error;

use crate::prec::*;
use crate::src::Rule;

use super::{soa::*, *};

impl Module {
    /// Parse a node from source code.
    pub fn parse(
        name: String,
        i: Pair<'_, Rule>,
        p: &mut Parser,
    ) -> Result<Self, Error> {
        assert_eq!(Rule::r#mod, i.as_rule());

        p.scoped(|p| {
            let mut fns = i.into_inner()
                .filter(|i| i.as_rule() == Rule::r#fn);
            let functions = FunctionsMut::try_put_seq_rec(p, |p| {
                fns.next()
                    .map(|i| Function::parse(i, p))
                    .transpose()
            })?;

            Ok(Module { name, functions })
        })
    }
}

impl Function {
    /// Parse a node from source code.
    pub fn parse(
        i: Pair<'_, Rule>,
        p: &mut Parser,
    ) -> Result<Self, Error> {
        assert_eq!(Rule::r#fn, i.as_rule());

        p.scoped(|p| {
            let mut pairs = i.into_inner().peekable();
            assert_eq!(Rule::fn_kw, pairs.next().unwrap().as_rule());
            let name = pairs.next().unwrap().as_str().to_string();
            let args = ArgumentsMut::try_put_seq_rec(p, |p| {
                pairs.next_if(|i| i.as_rule() == Rule::fn_arg)
                    .map(|i| Argument::parse(i, p))
                    .transpose()
            })?;
            let rett = MappedType::parse(pairs.next().unwrap(), p)?;
            let body = Expr::parse(pairs.next().unwrap(), p)?;
            let body = p.storage.exprs.put(body);

            Ok(Function { name, args, rett, body })
        })
    }
}

impl Argument {
    /// Parse a node from source code.
    pub fn parse(
        i: Pair<'_, Rule>,
        p: &mut Parser,
    ) -> Result<Self, Error> {
        assert_eq!(Rule::fn_arg, i.as_rule());

        let mut pairs = i.into_inner();
        let name = pairs.next().unwrap().as_str().to_string();
        let r#type = MappedType::parse(pairs.next().unwrap(), p)?;

        // Register the argument as an expression and as a binding.
        let expr = p.storage.exprs.put(Expr::Arg);
        let variable = p.def_var(Variable { name, expr });

        Ok(Argument { variable, r#type })
    }
}

impl MappedType {
    /// Parse a node from source code.
    pub fn parse(
        i: Pair<'_, Rule>,
        p: &mut Parser,
    ) -> Result<Self, Error> {
        assert_eq!(Rule::r#type, i.as_rule());

        let input_info = (i.as_str(), i.as_span());
        let mut pairs = i.into_inner().peekable();
        let stream = pairs
            .next_if(|i| i.as_rule() == Rule::type_stream)
            .map_or(StreamPart::None, |_| StreamPart::Some {});
        let vector = pairs
            .next_if(|i| i.as_rule() == Rule::type_vector)
            .map(|i| i.into_inner().next().unwrap().as_str())
            .map(|i| i.parse::<u32>().map_err(|err| {
                let message = format!(
                    "The vector size component of '{}' is invalid: {}",
                    input_info.0, err);
                Error::Grammar(pest::error::Error::new_from_span(
                    pest::error::ErrorVariant::CustomError { message },
                    input_info.1))
            }))
            .transpose()?
            .map_or(VectorPart::None, |size| VectorPart::Some { size });
        let option = pairs
            .next_if(|i| i.as_rule() == Rule::type_option)
            .map_or(OptionPart::None, |_| OptionPart::Some {});
        let scalar = ScalarType::parse(pairs.next().unwrap(), p)?;

        Ok(MappedType { stream, vector, option, scalar })
    }
}

impl ScalarType {
    /// Parse a node from source code.
    pub fn parse(
        i: Pair<'_, Rule>,
        p: &mut Parser,
    ) -> Result<Self, Error> {
        assert_eq!(Rule::type_scalar, i.as_rule());

        match i.into_inner().next().unwrap() {
            t if t.as_rule() == Rule::type_int =>
                IntType::parse(t, p).map(ScalarType::Int),
            _ => unreachable!(),
        }
    }
}

impl IntType {
    /// Parse a node from source code.
    pub fn parse(
        i: Pair<'_, Rule>,
        _: &mut Parser,
    ) -> Result<Self, Error> {
        assert_eq!(Rule::type_int, i.as_rule());

        let (sign, bits) = match i.as_str().split_at(1) {
            ("u", x) => (IntSign::U, x),
            ("s", x) => (IntSign::S, x),
            _ => unreachable!(),
        };

        let bits = bits.parse::<NonZeroU32>().map_err(|err| {
            let message = format!(
                "The size component of '{}' is invalid: {}",
                i.as_str(), err);
            Error::Grammar(pest::error::Error::new_from_span(
                pest::error::ErrorVariant::CustomError { message },
                i.as_span()))
        })?;

        Ok(IntType { sign, bits })
    }
}

impl Stmt {
    /// Parse a node from source code.
    pub fn parse(
        i: Pair<'_, Rule>,
        p: &mut Parser,
    ) -> Result<Self, Error> {
        assert_eq!(Rule::stmt, i.as_rule());

        let i = i.into_inner().next().unwrap();
        match i.as_rule() {
            Rule::stmt_let => {
                let mut pairs = i.into_inner();
                let name = pairs.next().unwrap().as_str().to_string();
                let expr = Expr::parse(pairs.next().unwrap(), p)?;
                let expr = p.storage.exprs.put(expr);

                // Register the new binding.
                let variable = p.def_var(Variable { name, expr });

                Ok(Stmt::Let(variable))
            },
            _ => unreachable!(),
        }
    }
}

impl Expr {
    /// Parse a node from source code.
    pub fn parse(
        i: Pair<'_, Rule>,
        p: &mut Parser,
    ) -> Result<Self, Error> {
        assert_eq!(Rule::expr, i.as_rule());
        Self::parse_inner(None, &mut i.into_inner().peekable(), p)
    }

    fn parse_inner(
        prev: Option<&Pair<'_, Rule>>,
        i: &mut iter::Peekable<Pairs<'_, Rule>>,
        p: &mut Parser,
    ) -> Result<Self, Error> {
        // The expression parsed thus far.
        let mut expr = Self::parse_unit(i.next().unwrap(), p)?;

        // Try parsing another binary operation.
        while let Some(next) = i.peek() {
            // Test that the operator is part of this expression.
            if Self::binop_cmp(prev, next)? == Assoc::Left {
                break;
            }

            let next = i.next().unwrap();
            let rhs = Self::parse_inner(Some(&next), i, p)?;
            let rhs = p.storage.exprs.put(rhs);

            let bop = match next.into_inner().next().unwrap().as_rule() {
                Rule::op_add => BinOp::Int(IntBinOp::Add),
                Rule::op_sub => BinOp::Int(IntBinOp::Sub),
                Rule::op_mul => BinOp::Int(IntBinOp::Mul),
                Rule::op_div => BinOp::Int(IntBinOp::Div),
                Rule::op_rem => BinOp::Int(IntBinOp::Rem),

                Rule::op_and => BinOp::Int(IntBinOp::And),
                Rule::op_ior => BinOp::Int(IntBinOp::IOr),
                Rule::op_xor => BinOp::Int(IntBinOp::XOr),
                Rule::op_shl => BinOp::Int(IntBinOp::ShL),
                Rule::op_shr => BinOp::Int(IntBinOp::ShR),

                Rule::op_cat => BinOp::Cat,
                Rule::op_ind => BinOp::Ind,
                Rule::op_exp => BinOp::Exp,
                Rule::op_red => BinOp::Red,

                Rule::op_iseq => BinOp::Cmp(CmpBinOp::IsEq),
                Rule::op_isne => BinOp::Cmp(CmpBinOp::IsNE),
                Rule::op_islt => BinOp::Cmp(CmpBinOp::IsLT),
                Rule::op_isle => BinOp::Cmp(CmpBinOp::IsLE),
                Rule::op_isgt => BinOp::Cmp(CmpBinOp::IsGT),
                Rule::op_isge => BinOp::Cmp(CmpBinOp::IsGE),

                Rule::op_cond => BinOp::Cond,
                Rule::op_else => BinOp::Else,

                _ => unreachable!(),
            };

            // Construct the new binary expression.
            let lhs = p.storage.exprs.put(expr);
            expr = Self::Bin(bop, [lhs, rhs]);
        }

        Ok(expr)
    }

    /// Compare two binary operators for associativity.
    fn binop_cmp(
        lhs: Option<&Pair<'_, Rule>>,
        rhs: &Pair<'_, Rule>,
    ) -> Result<Assoc, Error> {
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
            Rule::op_cat | Rule::op_ind => Prec::CatInd,
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
    fn parse_unit(
        i: Pair<'_, Rule>,
        p: &mut Parser,
    ) -> Result<Self, Error> {
        assert_eq!(Rule::expr_unit, i.as_rule());
        let mut pairs = i.into_inner().rev();

        let expr = Self::parse_sufs(pairs.next().unwrap(), p)?;
        Ok(pairs.fold(expr, |src, uop| {
            assert_eq!(Rule::op_pre, uop.as_rule());
            let uop = uop.into_inner().next().unwrap();

            let uop = match uop.as_rule() {
                Rule::op_neg => UnaOp::Int(IntUnaOp::Neg),
                Rule::op_not => UnaOp::Int(IntUnaOp::Not),
                _ => unreachable!(),
            };

            let src = p.storage.exprs.put(src);
            Self::Una(uop, [src])
        }))
    }

    /// Parse an indivisible expression without prefixes.
    fn parse_sufs(
        i: Pair<'_, Rule>,
        p: &mut Parser,
    ) -> Result<Self, Error> {
        assert_eq!(Rule::expr_sufs, i.as_rule());
        let mut pairs = i.into_inner();

        let expr = Self::parse_atom(pairs.next().unwrap(), p)?;
        pairs.try_fold(expr, |_expr, op| {
            assert_eq!(Rule::op_suf, op.as_rule());
            let op = op.into_inner().next().unwrap();
            match op.as_rule() {
                _ => unreachable!(),
            }
        })
    }

    /// Parse an atomic expression.
    fn parse_atom(
        i: Pair<'_, Rule>,
        p: &mut Parser,
    ) -> Result<Expr, Error> {
        assert_eq!(Rule::atom, i.as_rule());
        let i = i.into_inner().next().unwrap();
        match i.as_rule() {
            Rule::expr_int => Self::parse_int(i, p),
            Rule::expr_cst => Self::parse_cst(i, p),
            Rule::expr_var => Self::parse_var(i, p),
            Rule::expr_blk => Self::parse_blk(i, p),
            Rule::expr => Self::parse(i, p),
            _ => unreachable!(),
        }
    }

    /// Parse an integer literal expression.
    fn parse_int(
        i: Pair<'_, Rule>,
        _: &mut Parser,
    ) -> Result<Self, Error> {
        assert_eq!(Rule::expr_int, i.as_rule());
        Ok(Self::Int(i.as_str().parse().unwrap()))
    }

    /// Parse a cast expression.
    fn parse_cst(
        i: Pair<'_, Rule>,
        p: &mut Parser,
    ) -> Result<Self, Error> {
        assert_eq!(Rule::expr_cst, i.as_rule());
        let mut pairs = i.into_inner();

        let r#type = MappedType::parse(pairs.next().unwrap(), p)?;
        let expr = Expr::parse_unit(pairs.next().unwrap(), p)?;
        let expr = p.storage.exprs.put(expr);

        Ok(Self::Cast(r#type, expr))
    }

    /// Parse a variable reference expression.
    fn parse_var(
        i: Pair<'_, Rule>,
        p: &mut Parser,
    ) -> Result<Self, Error> {
        assert_eq!(Rule::expr_var, i.as_rule());

        let name = i.into_inner().next().unwrap().as_str().to_string();

        // Resolve the variable identifier within the innermost scope.
        let variable = p.get_var(&name)
            .ok_or_else(|| Error::Unresolved(name))?;

        Ok(Self::Var(variable))
    }

    /// Parse a block expression.
    fn parse_blk(
        i: Pair<'_, Rule>,
        p: &mut Parser,
    ) -> Result<Self, Error> {
        assert_eq!(Rule::expr_blk, i.as_rule());
        let mut pairs = i.into_inner().peekable();

        p.scoped(|p| {
            let stmts = StmtsMut::try_put_seq_rec(p, |p| {
                pairs
                    .next_if(|i| i.as_rule() == Rule::stmt)
                    .map(|i| Stmt::parse(i, p))
                    .transpose()
            })?;
            let rexpr = Expr::parse(pairs.next().unwrap(), p)?;
            let rexpr = p.storage.exprs.put(rexpr);
            Ok(Self::Blk { stmts, rexpr })
        })
    }
}

/// A parser of grammatical Solo code.
#[derive(Default)]
pub struct Parser {
    /// Storage for the AST being constructed.
    pub storage: StorageMut,

    /// A stack of scopes used for name resolution.
    scopes: Vec<FxHashMap<String, ID<Variable>>>,
}

impl Parser {
    /// Execute code within a new scope.
    pub fn scoped<R, F>(&mut self, f: F) -> R
    where F: FnOnce(&mut Self) -> R {
        self.scopes.push(FxHashMap::default());
        let result = (f)(self);
        self.scopes.pop();
        result
    }

    /// Define a variable for the current scope.
    pub fn def_var(&mut self, v: Variable) -> ID<Variable> {
        let name = v.name.clone();
        let id = self.storage.variables.put(v);
        self.scopes.last_mut()
            .expect("Variables can only be defined within scopes")
            .insert(name, id);
        id
    }

    /// Find the variable of the given name.
    pub fn get_var(&self, n: &str) -> Option<ID<Variable>> {
        self.scopes.iter().rev()
            .find_map(|s| s.get(n).copied())
    }
}

impl<T> AsMut<T> for Parser
where StorageMut: AsMut<T> {
    fn as_mut(&mut self) -> &mut T {
        self.storage.as_mut()
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
