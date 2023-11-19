use num_bigint::BigInt;

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
