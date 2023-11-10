//! MIR operations.
//!
//! This module enumerates the operations supported at the MIR level.  Unlike
//! the HIR, the MIR performs no implicit broadcasting, so explicit instructions
//! are inserted wherever a broadcast is necessary.  MIR operations are typed so
//! that enough information is available to perform code generation.

use crate::tck::*;

/// A singular binary operation.
pub enum SingleBinOp {
    /// A regular binary operation on vectors.
    Reg(VectorBinOp),
}

/// A singular unary operation.
pub enum SingleUnaOp {
    /// A regular unary operation on vectors.
    Reg(VectorUnaOp),
}

/// A singular collection operation.
pub enum SingleColOp {
    /// Collection into an array.
    Arr(StreamPart, VectorType),
}

/// A binary operation on streams.
pub enum StreamBinOp {
    /// A mapped binary operation on vectors.
    Map {
        /// The underlying operation.
        bop: VectorBinOp,

        /// The stream part being mapped over.
        map: StreamPart,
    },

    /// Stream expansion.
    Exp {
        /// The type of the left side.
        lhs: (StreamPart, VectorType),

        /// The stream part of the expansion mask.
        rhs: StreamPart,
    },

    /// Stream reduction.
    Red {
        /// The underlying type of the left side.
        lhs: VectorType,

        /// The stream part being zipped across.
        zip: StreamPart,
    },
}

/// A unary operation on streams.
pub enum StreamUnaOp {
    /// A mapped unary operation on vectors.
    Map {
        /// The underlying operation.
        uop: VectorUnaOp,

        /// The stream part being mapped over.
        map: StreamPart,
    },
}

/// A binary operation on maybe-vectors.
pub enum VectorBinOp {
    /// A mapped binary operation on options.
    Map {
        /// The underlying operation.
        bop: OptionBinOp,

        /// The vector part being mapped over.
        map: Option<VectorPart>,
    },

    /// Vector concatenation.
    Cat {
        /// The underlying type of both operands.
        src: OptionType,

        /// The vector part of the left side.
        lhs: VectorPart,

        /// The vector part of the right side.
        rhs: VectorPart,
    },

    /// Vector indexing.
    Ind {
        /// The type of the left side.
        lhs: (VectorPart, OptionType),

        /// The type of the right side.
        rhs: VectorType,
    },
}

/// A unary operation on maybe-vectors.
pub enum VectorUnaOp {
    /// A mapped unary operation on options.
    Map {
        /// The underlying operation.
        uop: OptionUnaOp,

        /// The vector part being mapped over.
        map: Option<VectorPart>,
    },

    /// Broadcasting into a vector.
    New {
        /// The input type.
        src: OptionType,

        /// The vector part being broadcasted into.
        map: VectorPart,
    },
}

/// A binary operation on maybe-options.
pub enum OptionBinOp {
    /// A mapped binary operation on scalars.
    Map {
        /// The underlying operation.
        bop: ScalarBinOp,

        /// The option part being mapped over.
        map: Option<OptionPart>,
    },

    /// Option conditioning.
    Cond {
        /// The type of the right side.
        rhs: OptionType,
    },

    /// Option defaulting.
    Else {
        /// The underlying type of both operands.
        src: ScalarType,

        /// The option part of the right side.
        rhs: Option<OptionPart>,
    },
}

/// A unary operation on maybe-options.
pub enum OptionUnaOp {
    /// A mapped unary operation on scalars.
    Map {
        /// The underlying operation.
        uop: ScalarUnaOp,

        /// The option part being mapped over.
        map: Option<OptionPart>,
    },

    /// Broadcasting into an option.
    New {
        /// The input type.
        src: ScalarType,

        /// The option part being broadcasted into.
        map: OptionPart,
    },
}

/// A binary operation on scalars.
pub enum ScalarBinOp {
    /// An operation on items of the same type.
    Map {
        /// The underlying operation.
        bop: ScalarMapBinOp,

        /// The scalar type of both operands.
        map: ScalarType,
    },

    /// An operation with disjoint operand types.
    Sep {
        /// The underlying operation.
        bop: ScalarSepBinOp,

        /// The type of the left side.
        lhs: ScalarType,

        /// The type of the right side.
        rhs: ScalarType,
    },

    /// A comparison operation.
    Cmp {
        /// The underlying operation.
        bop: ScalarCmpBinOp,

        /// The scalar type of both operands.
        map: ScalarType,
    },
}

/// A unary operation on scalars.
pub enum ScalarUnaOp {
    /// An operation on items of the same type.
    Map {
        /// The underlying operation.
        uop: ScalarMapUnaOp,

        /// The scalar type of the operand.
        map: ScalarType,
    },
}

/// A binary operation on scalars of the same type.
pub enum ScalarMapBinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,

    And,
    IOr,
    XOr,
}

/// A binary operation on scalars of different types.
pub enum ScalarSepBinOp {
    ShL,
    ShR,
}

/// A binary comparison operation.
pub enum ScalarCmpBinOp {
    IsEq,
    IsNE,
    IsLT,
    IsLE,
    IsGT,
    IsGE,
}

/// A unary operation on scalars of the same type.
pub enum ScalarMapUnaOp {
    Neg,
    Not,
}
