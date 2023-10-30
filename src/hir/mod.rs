//! A Higher Intermediary Representation (HIR) for Solo.

use core::{fmt, mem, slice};

use egg::{self, Id, Language, Symbol};

use num_bigint::BigInt;

pub mod opt;
pub use opt::Optimizer;

pub mod parse;
pub use parse::Parser;

/// An HIR expression node.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ExprNode {
    Not(Id),

    Add([Id; 2]),
    Sub([Id; 2]),
    Mul([Id; 2]),
    Div([Id; 2]),
    Rem([Id; 2]),

    And([Id; 2]),
    IOr([Id; 2]),
    XOr([Id; 2]),
    ShL([Id; 2]),
    ShR([Id; 2]),

    IsEq([Id; 2]),
    IsNE([Id; 2]),
    IsLT([Id; 2]),
    IsLE([Id; 2]),
    IsGT([Id; 2]),
    IsGE([Id; 2]),

    // TODO: array operations

    Int(BigInt),
    Arg(Symbol),
}

impl Language for ExprNode {
    #[inline(always)]
    fn matches(&self, other: &Self) -> bool {
        if mem::discriminant(self) != mem::discriminant(other) { return false };
        match (self, other) {
            (Self::Int(l), Self::Int(r)) => l == r,
            (Self::Arg(l), Self::Arg(r)) => l == r,

            // We never have variable-length children, and the remaining
            // variants only consist of 'Id's, which we aren't checking.
            _ => true,
        }
    }

    fn children(&self) -> &[Id] {
        match self {
            Self::Not(x) => slice::from_ref(x),
            Self::Add(x) | Self::Sub(x) | Self::Mul(x)
                | Self::Div(x) | Self::Rem(x) => x,
            Self::And(x) | Self::IOr(x) | Self::XOr(x)
                | Self::ShL(x) | Self::ShR(x) => x,
            Self::IsEq(x) | Self::IsNE(x)
                | Self::IsLT(x) | Self::IsLE(x)
                | Self::IsGT(x) | Self::IsGE(x) => x,
            Self::Int(..) | Self::Arg(..) => &[],
        }
    }

    fn children_mut(&mut self) -> &mut [Id] {
        match self {
            Self::Not(x) => slice::from_mut(x),
            Self::Add(x) | Self::Sub(x) | Self::Mul(x)
                | Self::Div(x) | Self::Rem(x) => x,
            Self::And(x) | Self::IOr(x) | Self::XOr(x)
                | Self::ShL(x) | Self::ShR(x) => x,
            Self::IsEq(x) | Self::IsNE(x)
                | Self::IsLT(x) | Self::IsLE(x)
                | Self::IsGT(x) | Self::IsGE(x) => x,
            Self::Int(..) | Self::Arg(..) => &mut [],
        }
    }
}

impl fmt::Display for ExprNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Not(..) => "not",

            Self::Add(..) => "add",
            Self::Sub(..) => "sub",
            Self::Mul(..) => "mul",
            Self::Div(..) => "div",
            Self::Rem(..) => "rem",

            Self::And(..) => "and",
            Self::IOr(..) => "ior",
            Self::XOr(..) => "xor",
            Self::ShL(..) => "shl",
            Self::ShR(..) => "shr",

            Self::IsEq(..) => "iseq",
            Self::IsNE(..) => "isne",
            Self::IsLT(..) => "islt",
            Self::IsLE(..) => "isle",
            Self::IsGT(..) => "isgt",
            Self::IsGE(..) => "isge",

            Self::Int(v) => return fmt::Display::fmt(v, f),
            Self::Arg(n) => return write!(f, "${}", n),
        })
    }
}

impl egg::FromOp for ExprNode {
    type Error = egg::FromOpError;

    fn from_op(op: &str, children: Vec<Id>) -> Result<Self, Self::Error> {
        Ok(match (op, &*children) {
            ("not", &[id]) => Self::Not(id),

            ("add", &[lhs, rhs]) => Self::Add([lhs, rhs]),
            ("sub", &[lhs, rhs]) => Self::Sub([lhs, rhs]),
            ("mul", &[lhs, rhs]) => Self::Mul([lhs, rhs]),
            ("div", &[lhs, rhs]) => Self::Div([lhs, rhs]),
            ("rem", &[lhs, rhs]) => Self::Rem([lhs, rhs]),

            ("and", &[lhs, rhs]) => Self::And([lhs, rhs]),
            ("ior", &[lhs, rhs]) => Self::IOr([lhs, rhs]),
            ("xor", &[lhs, rhs]) => Self::XOr([lhs, rhs]),
            ("shl", &[lhs, rhs]) => Self::ShL([lhs, rhs]),
            ("shr", &[lhs, rhs]) => Self::ShR([lhs, rhs]),

            ("iseq", &[lhs, rhs]) => Self::IsEq([lhs, rhs]),
            ("isne", &[lhs, rhs]) => Self::IsNE([lhs, rhs]),
            ("islt", &[lhs, rhs]) => Self::IsLT([lhs, rhs]),
            ("isle", &[lhs, rhs]) => Self::IsLE([lhs, rhs]),
            ("isgt", &[lhs, rhs]) => Self::IsGT([lhs, rhs]),
            ("isge", &[lhs, rhs]) => Self::IsGE([lhs, rhs]),

            (op, &[]) => {
                if let Some(ident) = op.strip_prefix('$') {
                    Self::Arg(egg::Symbol::new(ident))
                } else if let Ok(value) = op.parse::<BigInt>() {
                    Self::Int(value)
                } else {
                    return Err(egg::FromOpError::new(op, children));
                }
            },

            _ => return Err(egg::FromOpError::new(op, children)),
        })
    }
}
