//! A Higher Intermediary Representation (HIR) for Solo.

use egg::{self, Id, Language, RecExpr};

use crate::types::*;

pub mod ops;
use ops::*;

pub mod parse;
pub use parse::parse_fn;

/// An HIR function.
#[derive(Clone, Debug)]
pub struct Function {
    /// The name of the function.
    pub name: String,

    /// The body of the function.
    pub body: RecExpr<Node>,
}

/// An HIR node.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Node {
    /// A nilary operation.
    Nil(StreamNilOp),

    /// A unary operation.
    Una(StreamUnaOp, [Id; 1]),

    /// A binary operation.
    Bin(StreamBinOp, [Id; 2]),
}

impl Language for Node {
    #[inline(always)]
    fn matches(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Nil(l), Self::Nil(r)) => l == r,
            (Self::Una(l, _), Self::Una(r, _)) => l == r,
            (Self::Bin(l, _), Self::Bin(r, _)) => l == r,
            _ => false,
        }
    }

    fn children(&self) -> &[Id] {
        match self {
            Self::Nil(_) => &[],
            Self::Una(_, x) => x,
            Self::Bin(_, x) => x,
        }
    }

    fn children_mut(&mut self) -> &mut [Id] {
        match self {
            Self::Nil(_) => &mut [],
            Self::Una(_, x) => x,
            Self::Bin(_, x) => x,
        }
    }
}
