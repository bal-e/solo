//! MIR operations.
//!
//! This module enumerates the operations supported at the MIR level.  Unlike
//! the HIR, the MIR performs no implicit broadcasting, so explicit instructions
//! are inserted wherever a broadcast is necessary.  MIR operations are typed so
//! that enough information is available to perform code generation.

use crate::tck::*;

/// A singular binary operation.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum SingleBinOp {
    /// A regular binary operation on vectors.
    Reg(VectorBinOp),
}

/// A singular unary operation.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum SingleUnaOp {
    /// A regular unary operation on vectors.
    Reg(VectorUnaOp),
}

/// A singular collection operation.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum SingleColOp {
    /// Collection into an array.
    Arr(StreamPart, VectorType),
}

/// A binary operation on streams.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
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
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
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
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
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

impl VectorBinOp {
    /// The left-hand source type.
    pub fn lhs_type(self) -> VectorType {
        match self {
            Self::Map { bop, map } =>
                VectorType::new(bop.lhs_type(), map),
            Self::Cat { src, lhs, rhs: _ } =>
                VectorType::new(src, Some(lhs)),
            Self::Ind { lhs, rhs: _ } =>
                VectorType::new(lhs.1, Some(lhs.0)),
        }
    }

    /// The right-hand source type.
    pub fn rhs_type(self) -> VectorType {
        match self {
            Self::Map { bop, map } =>
                VectorType::new(bop.rhs_type(), map),
            Self::Cat { src, lhs: _, rhs } =>
                VectorType::new(src, Some(rhs)),
            Self::Ind { lhs: _, rhs } => rhs,
        }
    }

    /// The destination type.
    pub fn dst_type(self) -> VectorType {
        match self {
            Self::Map { bop, map } =>
                VectorType::new(bop.dst_type(), map),
            Self::Cat { src, lhs, rhs } => {
                let part = VectorPart::new(lhs.size + rhs.size);
                VectorType::new(src, Some(part))
            },
            Self::Ind { lhs: (_, lhs_option), rhs } =>
                VectorType::new(
                    OptionType::new(
                        lhs_option.scalar,
                        Subtyping::merge_max(lhs_option.part, rhs.option.part)
                            .unwrap()),
                    rhs.part),

        }
    }
}

/// A unary operation on maybe-vectors.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
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

impl VectorUnaOp {
    /// The source type.
    pub fn src_type(self) -> VectorType {
        match self {
            Self::Map { uop, map } => VectorType::new(uop.src_type(), map),
            Self::New { src, map: _ } => VectorType::new(src, None),
        }
    }

    /// The destination type.
    pub fn dst_type(self) -> VectorType {
        match self {
            Self::Map { uop, map } => ScalarType::new(uop.dst_type(), map),
            Self::New { src, map } => ScalarType::new(src, Some(map)),
        }
    }
}

/// A binary operation on maybe-options.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
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
        rhs: ScalarType,
    },

    /// Option defaulting.
    Else {
        /// The underlying type of both operands.
        src: ScalarType,

        /// The option part of the right side.
        rhs: Option<OptionPart>,
    },
}

impl OptionBinOp {
    /// The left-hand source type.
    pub fn lhs_type(self) -> OptionType {
        match self {
            Self::Map { bop, map } =>
                OptionType::new(bop.lhs_type(), map),
            Self::Cond { rhs: _ } =>
                ScalarType::Int(IntType {
                    sign: IntSign::U,
                    size: IntSize::new(1),
                }),
            Self::Else { src, rhs: _ } =>
                OptionType::new(src, Some(OptionPart {})),
        }
    }

    /// The right-hand source type.
    pub fn rhs_type(self) -> OptionType {
        match self {
            Self::Map { bop, map } =>
                OptionType::new(bop.rhs_type(), map),
            Self::Cond { rhs } =>
                OptionType::new(rhs, Some(OptionPart {})),
            Self::Else { src, rhs } =>
                OptionType::new(src, rhs),
        }
    }

    /// The destination type.
    pub fn dst_type(self) -> OptionType {
        match self {
            Self::Map { bop, map } =>
                OptionType::new(bop.dst_type(), map),
            Self::Cond { rhs } =>
                OptionType::new(rhs, Some(OptionPart {})),
            Self::Else { src, rhs } =>
                OptionType::new(src, rhs),
        }
    }
}

/// A unary operation on maybe-options.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
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

impl OptionUnaOp {
    /// The source type.
    pub fn src_type(self) -> OptionType {
        match self {
            Self::Map { uop, map } => OptionType::new(uop.src_type(), map),
            Self::New { src, map: _ } => OptionType::new(src, None),
        }
    }

    /// The destination type.
    pub fn dst_type(self) -> OptionType {
        match self {
            Self::Map { uop, map } => OptionType::new(uop.dst_type(), map),
            Self::New { src, map } => OptionType::new(src, Some(map)),
        }
    }
}

/// A binary operation on scalars.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
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

impl ScalarBinOp {
    /// The left-hand source type.
    pub fn lhs_type(self) -> ScalarType {
        match self {
            Self::Map { bop: _, map } => map,
            Self::Sep { bop: _, lhs, rhs: _ } => lhs,
            Self::Cmp { bop: _, map } => map,
        }
    }

    /// The right-hand source type.
    pub fn rhs_type(self) -> ScalarType {
        match self {
            Self::Map { bop: _, map } => map,
            Self::Sep { bop: _, lhs: _, rhs } => rhs,
            Self::Cmp { bop: _, map } => map,
        }
    }

    /// The destination type.
    pub fn dst_type(self) -> ScalarType {
        match self {
            Self::Map { bop: _, map } => map,
            Self::Sep { bop: ScalarSepBinOp::ShL, lhs, rhs: _ } => lhs,
            Self::Sep { bop: ScalarSepBinOp::ShR, lhs, rhs: _ } => lhs,
            Self::Cmp { bop: _, map: _ } => ScalarType::Int(IntType {
                sign: IntSign::U,
                size: IntSize::new(1),
            }),
        }
    }
}

/// A unary operation on scalars.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ScalarUnaOp {
    /// An operation on items of the same type.
    Map {
        /// The underlying operation.
        uop: ScalarMapUnaOp,

        /// The scalar type of the operand.
        map: ScalarType,
    },
}

impl ScalarUnaOp {
    /// The source type.
    pub fn src_type(self) -> ScalarType {
        match self {
            Self::Map { uop: _, map } => map,
        }
    }

    /// The destination type.
    pub fn dst_type(self) -> ScalarType {
        match self {
            Self::Map { uop: _, map } => map,
        }
    }
}

/// A binary operation on scalars of the same type.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
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
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ScalarSepBinOp {
    ShL,
    ShR,
}

/// A binary comparison operation.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ScalarCmpBinOp {
    IsEq,
    IsNE,
    IsLT,
    IsLE,
    IsGT,
    IsGE,
}

/// A unary operation on scalars of the same type.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ScalarMapUnaOp {
    Neg,
    Not,
}
