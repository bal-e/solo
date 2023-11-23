use num_bigint::BigInt;

use super::Prec;

pub use crate::ops::{ScalarIntBinOp, ScalarCmpBinOp, ScalarIntUnaOp};

/// A binary operation on streams.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum StreamBinOp {
    /// A mapped binary operation on vectors.
    Map(VectorBinOp),

    /// Stream expansion.
    Exp,

    /// Stream reduction.
    Red,
}

impl StreamBinOp {
    /// The precedence of this operation.
    pub fn prec(&self) -> Prec {
        match self {
            Self::Map(bop) => bop.prec(),
            Self::Exp | Self::Red => Prec::ExpRed,
        }
    }
}

/// A unary operation on streams.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum StreamUnaOp {
    /// A mapped unary operation on vectors.
    Map(VectorUnaOp),
}

/// A nilary operation of streams.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum StreamNilOp {
    /// A mapped nilary operation of vectors.
    Map(VectorNilOp),
}

/// A binary operation on maybe-vectors.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum VectorBinOp {
    /// A mapped binary operation on options.
    Map(OptionBinOp),

    /// Vector concatenation.
    Cat,

    /// Vector indexing.
    Ind,
}

impl VectorBinOp {
    /// The precedence of this operation.
    pub fn prec(&self) -> Prec {
        match self {
            Self::Map(bop) => bop.prec(),
            Self::Cat | Self::Ind => Prec::CatInd,
        }
    }
}

/// A unary operation on maybe-vectors.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum VectorUnaOp {
    /// A mapped unary operation on options.
    Map(OptionUnaOp),
}

/// A nilary operation of maybe-vectors.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum VectorNilOp {
    /// A mapped nilary operation of options.
    Map(OptionNilOp),
}

/// A binary operation on maybe-options.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum OptionBinOp {
    /// A mapped binary operation on scalars.
    Map(ScalarBinOp),

    /// Option conditioning.
    Cond,

    /// Option defaulting.
    Else,
}

impl OptionBinOp {
    /// The precedence of this operation.
    pub fn prec(&self) -> Prec {
        match self {
            Self::Map(bop) => bop.prec(),
            Self::Cond => Prec::Cond,
            Self::Else => Prec::Else,
        }
    }
}

/// A unary operation on maybe-options.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum OptionUnaOp {
    /// A mapped unary operation on scalars.
    Map(ScalarUnaOp),
}

/// A nilary operation of maybe-options.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum OptionNilOp {
    /// A mapped nilary operation of scalars.
    Map(ScalarNilOp),
}

/// A binary operation on scalars.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ScalarBinOp {
    /// An operation on integers.
    Int(ScalarIntBinOp),

    /// A comparison operation.
    Cmp(ScalarCmpBinOp),
}

impl ScalarBinOp {
    /// The precedence of this operation.
    pub fn prec(&self) -> Prec {
        match self {
            Self::Int(bop) => bop.prec(),
            Self::Cmp(_) => Prec::Compare,
        }
    }
}

/// A unary operation on scalars.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ScalarUnaOp {
    /// An operation on integers.
    Int(ScalarIntUnaOp),
}

/// A nilary operation of scalars.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ScalarNilOp {
    /// An integer literal.
    Int(BigInt),

    /// An argument.
    Arg(u32),
}

impl ScalarIntBinOp {
    /// The precedence of this operation.
    pub fn prec(&self) -> Prec {
        match self {
            Self::Add | Self::Sub => Prec::AddSub,
            Self::Mul | Self::Div | Self::Rem => Prec::MulDiv,

            Self::And | Self::IOr | Self::XOr => Prec::Bitwise,
            Self::ShL | Self::ShR => Prec::Shift,
        }
    }
}
