//! A Higher Intermediary Representation (HIR) for Solo.

use core::slice;

use num_bigint::BigInt;

use egg::{self, Id, Language, RecExpr};

use crate::ops::*;
use crate::tys::fix::*;

mod fmt;
pub mod parse;

/// An HIR function.
#[derive(Clone, Debug)]
pub struct Function {
    /// The name of the function.
    pub name: String,

    /// The body of the function.
    pub body: RecExpr<TypedNode>,
}

/// A typed HIR node.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypedNode {
    /// The underlying, untyped node.
    pub node: Node,

    /// The output type of the node.
    pub dstt: MappedType,
}

impl Language for TypedNode {
    fn matches(&self, other: &Self) -> bool {
        self.node.matches(&other.node) && self.dstt == other.dstt
    }

    fn children(&self) -> &[Id] {
        self.node.children()
    }

    fn children_mut(&mut self) -> &mut [Id] {
        self.node.children_mut()
    }
}

/// An untyped HIR node.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Node {
    /// A binary operation.
    Bin(MappedBinOp, [Id; 2]),

    /// A unary operation.
    Una(MappedUnaOp, [Id; 1]),

    /// A cast operation.
    Cast(MappedType, Id),

    /// A function argument.
    Arg(u32),

    /// An integer literal.
    Int(BigInt),
}

impl Language for Node {
    fn matches(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bin(l, _), Self::Bin(r, _)) => l == r,
            (Self::Una(l, _), Self::Una(r, _)) => l == r,
            (Self::Cast(l, _), Self::Cast(r, _)) => l == r,
            (Self::Arg(l), Self::Arg(r)) => l == r,
            (Self::Int(l), Self::Int(r)) => l == r,
            _ => false,
        }
    }

    fn children(&self) -> &[Id] {
        match self {
            Self::Bin(_, x) => x,
            Self::Una(_, x) => x,
            Self::Cast(_, x) => slice::from_ref(x),
            Self::Arg(_) | Self::Int(_) => &[],
        }
    }

    fn children_mut(&mut self) -> &mut [Id] {
        match self {
            Self::Bin(_, x) => x,
            Self::Una(_, x) => x,
            Self::Cast(_, x) => slice::from_mut(x),
            Self::Arg(_) | Self::Int(_) => &mut [],
        }
    }
}
