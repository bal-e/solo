//! A Higher Intermediary Representation (HIR) for Solo.

use egg::{self, Id, Language, RecExpr};

use num_bigint::BigInt;

use crate::ops::*;
use crate::types::*;

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
    /// A unary operation on a single.
    SingleUna(SingleUnaOp, [Id; 1]),

    /// A binary operation on singles.
    SingleBin(SingleBinOp, [Id; 2]),

    /// A collection operation on a stream.
    SingleCol(SingleColOp, [Id; 1]),

    /// A unary operation on a stream.
    StreamUna(StreamUnaOp, [Id; 1]),

    /// A binary operation on a stream.
    StreamBin(StreamBinOp, [Id; 2]),

    /// An integer literal.
    Int(BigInt),

    /// A specific function argument.
    Arg(u32),
}

impl Language for Node {
    #[inline(always)]
    fn matches(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::SingleUna(l, _), Self::SingleUna(r, _)) => l == r,
            (Self::SingleBin(l, _), Self::SingleBin(r, _)) => l == r,
            (Self::SingleCol(l, _), Self::SingleCol(r, _)) => l == r,
            (Self::StreamUna(l, _), Self::StreamUna(r, _)) => l == r,
            (Self::StreamBin(l, _), Self::StreamBin(r, _)) => l == r,
            (Self::Int(l), Self::Int(r)) => l == r,
            (Self::Arg(l), Self::Arg(r)) => l == r,
            _ => false,
        }
    }

    fn children(&self) -> &[Id] {
        match self {
            Self::SingleUna(_, x) => x,
            Self::SingleBin(_, x) => x,
            Self::SingleCol(_, x) => x,
            Self::StreamUna(_, x) => x,
            Self::StreamBin(_, x) => x,
            Self::Int(..) | Self::Arg(..) => &[],
        }
    }

    fn children_mut(&mut self) -> &mut [Id] {
        match self {
            Self::SingleUna(_, x) => x,
            Self::SingleBin(_, x) => x,
            Self::SingleCol(_, x) => x,
            Self::StreamUna(_, x) => x,
            Self::StreamBin(_, x) => x,
            Self::Int(..) | Self::Arg(..) => &mut [],
        }
    }
}
