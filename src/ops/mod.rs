//! Operations.

mod fmt;
mod prec;
mod syn;
mod tys;

/// A binary operation.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinOp {
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

/// A unary operation.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnaOp {
    /// An integer unary operation.
    Int(IntUnaOp),
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
