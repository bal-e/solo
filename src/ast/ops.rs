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
