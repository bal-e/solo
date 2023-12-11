//! Operations.

mod cvt;
mod fmt;
mod prec;
mod syn;
pub mod tys;

/// A binary operation.
pub type BinOp = StreamBinOp;

/// A streaming binary operation.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum StreamBinOp {
    /// Stream expansion.
    Exp,

    /// Stream reduction.
    Red,

    /// Vector concatenation.
    Cat,

    /// Vector indexing / permutation.
    Ind,

    /// Option conditioning.
    Cond,

    /// Option defaulting.
    Else,

    /// An integer binary operation.
    Int(IntBinOp),

    /// A binary comparison operation.
    Cmp(CmpBinOp),
}

/// A singular binary operation.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SingleBinOp {
    /// Vector concatenation.
    Cat,

    /// Vector indexing / permutation.
    Ind,

    /// Option conditioning.
    Cond,

    /// Option defaulting.
    Else,

    /// An integer binary operation.
    Int(IntBinOp),

    /// A binary comparison operation.
    Cmp(CmpBinOp),
}

/// A unary operation.
pub type UnaOp = StreamUnaOp;

/// A streaming unary operation.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum StreamUnaOp {
    /// An integer unary operation.
    Int(IntUnaOp),
}

/// A singular unary operation.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SingleUnaOp {
    /// An integer unary operation.
    Int(IntUnaOp),
}

/// A singular collection operation.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SingleColOp {
    /// Collect into an array.
    Arr,
}

/// A casting operation.
pub type CastOp = StreamCastOp;

/// A streaming casting operation.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum StreamCastOp {
    /// Only cast the scalar part.
    Scalar,

    /// Cast the option and scalar parts.
    Option,

    /// Cast the vector, option, and scalar parts.
    Vector,

    /// Cast the stream, vector, option, and scalar parts.
    Stream,
}

/// A singular casting operation.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SingleCastOp {
    /// Only cast the scalar part.
    Scalar,

    /// Cast the option and scalar parts.
    Option,

    /// Cast the vector, option, and scalar parts.
    Vector,
}

/// An integer binary operation.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IntBinOp {
    /// Addition.
    Add,

    /// Subtraction.
    Sub,

    /// Multiplication.
    Mul,

    /// Division.
    Div,

    /// Remainder.
    Rem,

    /// Bitwise AND.
    And,

    /// Bitwise inclusive OR.
    IOr,

    /// Bitwise exclusive OR.
    XOr,

    /// Bit-shift left.
    ShL,

    /// Bit-shift right.
    ShR,
}

/// A binary comparison operation.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CmpBinOp {
    /// Test that inputs are equal.
    IsEq,

    /// Test that inputs are not equal.
    IsNE,

    /// Test that one input is strictly less than another.
    IsLT,

    /// Test that one input is less than or equal to another.
    IsLE,

    /// Test that one input is strictly greater than another.
    IsGT,

    /// Test that one input is greater than or equal to another.
    IsGE,
}

/// An integer unary operation.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IntUnaOp {
    /// Negation.
    Neg,

    /// Bitwise NOT.
    Not,
}
