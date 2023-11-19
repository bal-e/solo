use core::num::NonZeroU32;

use num_bigint::BigInt;

use crate::types::*;

/// A binary operation on maybe-vectors.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
                VectorType { part: map, data: bop.lhs_type() },
            Self::Cat { src, lhs, rhs: _ } =>
                VectorType { part: Some(lhs), data: src },
            Self::Ind { lhs, rhs: _ } =>
                VectorType { part: Some(lhs.0), data: lhs.1 },
        }
    }

    /// The right-hand source type.
    pub fn rhs_type(self) -> VectorType {
        match self {
            Self::Map { bop, map } =>
                VectorType { part: map, data: bop.rhs_type() },
            Self::Cat { src, lhs: _, rhs } =>
                VectorType { part: Some(rhs), data: src },
            Self::Ind { lhs: _, rhs } => rhs,
        }
    }

    /// The destination type.
    pub fn dst_type(self) -> VectorType {
        match self {
            Self::Map { bop, map } =>
                VectorType { part: map, data: bop.dst_type() },
            Self::Cat { src, lhs, rhs } => {
                let part = VectorPart { size: lhs.size + rhs.size };
                VectorType { part: Some(part), data: src }
            },
            Self::Ind { lhs: (_, lhs_option), rhs } =>
                VectorType {
                    part: rhs.part,
                    data: OptionType {
                        part: lhs_option.part.zip(rhs.data.part)
                            .map(|(OptionPart {}, OptionPart {})| OptionPart {}),
                        data: lhs_option.data,
                    },
                },
        }
    }
}

/// A unary operation on maybe-vectors.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
            Self::Map { uop, map } => VectorType { part: map, data: uop.src_type() },
            Self::New { src, map: _ } => VectorType { part: None, data: src },
        }
    }

    /// The destination type.
    pub fn dst_type(self) -> VectorType {
        match self {
            Self::Map { uop, map } => VectorType { part: map, data: uop.dst_type() },
            Self::New { src, map } => VectorType { part: Some(map), data: src },
        }
    }
}

/// A nilary operation of vectors.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum VectorNilOp {
    /// A mapped nilary operation of scalars.
    Map {
        /// The underlying operation.
        nop: OptionNilOp,

        /// The vector part being mapped over.
        map: Option<VectorPart>,
    },
}

impl VectorNilOp {
    /// The destination type.
    pub fn dst_type(self) -> VectorType {
        match self {
            Self::Map { nop, map } => VectorType { part: map, data: nop.dst_type() },
        }
    }
}

/// A binary operation on maybe-options.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
                OptionType { part: map, data: bop.lhs_type() },
            Self::Cond { rhs: _ } =>
                OptionType {
                    part: None,
                    data: ScalarType::Int(IntType {
                        sign: IntSign::U,
                        size: NonZeroU32::new(1).unwrap(),
                    }),
                },
            Self::Else { src, rhs: _ } =>
                OptionType { part: Some(OptionPart {}), data: src },
        }
    }

    /// The right-hand source type.
    pub fn rhs_type(self) -> OptionType {
        match self {
            Self::Map { bop, map } =>
                OptionType { part: map, data: bop.rhs_type() },
            Self::Cond { rhs } =>
                OptionType { part: Some(OptionPart {}), data: rhs },
            Self::Else { src, rhs } =>
                OptionType { part: rhs, data: src },
        }
    }

    /// The destination type.
    pub fn dst_type(self) -> OptionType {
        match self {
            Self::Map { bop, map } =>
                OptionType { part: map, data: bop.dst_type() },
            Self::Cond { rhs } =>
                OptionType { part: Some(OptionPart {}), data: rhs },
            Self::Else { src, rhs } =>
                OptionType { part: rhs, data: src },
        }
    }
}

/// A unary operation on maybe-options.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
            Self::Map { uop, map } => OptionType { part: map, data: uop.src_type() },
            Self::New { src, map: _ } => OptionType { part: None, data: src },
        }
    }

    /// The destination type.
    pub fn dst_type(self) -> OptionType {
        match self {
            Self::Map { uop, map } => OptionType { part: map, data: uop.dst_type() },
            Self::New { src, map } => OptionType { part: Some(map), data: src },
        }
    }
}

/// A nilary operation of options.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum OptionNilOp {
    /// A mapped nilary operation of scalars.
    Map {
        /// The underlying operation.
        nop: ScalarNilOp,

        /// The option part being mapped over.
        map: Option<OptionPart>,
    },
}

impl OptionNilOp {
    /// The destination type.
    pub fn dst_type(self) -> OptionType {
        match self {
            Self::Map { nop, map } => OptionType { part: map, data: nop.dst_type() },
        }
    }
}

/// A binary operation on scalars.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ScalarBinOp {
    /// An operation on integers.
    Int {
        /// The underlying operation.
        bop: ScalarIntBinOp,

        /// The integer type used.
        map: IntType,
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
            Self::Int { bop: _, map } => ScalarType::Int(map),
            Self::Cmp { bop: _, map } => map,
        }
    }

    /// The right-hand source type.
    pub fn rhs_type(self) -> ScalarType {
        match self {
            Self::Int { bop: _, map } => ScalarType::Int(map),
            Self::Cmp { bop: _, map } => map,
        }
    }

    /// The destination type.
    pub fn dst_type(self) -> ScalarType {
        match self {
            Self::Int { bop: _, map } => ScalarType::Int(map),
            Self::Cmp { bop: _, map: _ } => ScalarType::Int(IntType {
                sign: IntSign::U,
                size: NonZeroU32::new(1).unwrap(),
            }),
        }
    }
}

/// A unary operation on scalars.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ScalarUnaOp {
    /// An operation on integers.
    Int {
        /// The underlying operation.
        uop: ScalarIntUnaOp,

        /// The integer type used.
        map: IntType,
    },
}

impl ScalarUnaOp {
    /// The source type.
    pub fn src_type(self) -> ScalarType {
        match self {
            Self::Int { uop: _, map } => ScalarType::Int(map),
        }
    }

    /// The destination type.
    pub fn dst_type(self) -> ScalarType {
        match self {
            Self::Int { uop: _, map } => ScalarType::Int(map),
        }
    }
}

/// A nilary operation of scalars.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ScalarNilOp {
    /// An integer literal.
    Int {
        /// The integer value.
        val: BigInt,

        /// The integer type.
        dst: IntType,
    },

    /// An argument.
    Arg {
        /// The argument number.
        num: u32,

        /// The type of the argument.
        dst: ScalarType,
    },
}

impl ScalarNilOp {
    /// The destination type.
    pub fn dst_type(self) -> ScalarType {
        match self {
            Self::Int { dst, .. } => ScalarType::Int(dst),
            Self::Arg { dst, .. } => dst,
        }
    }
}

/// A binary operation on integers.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ScalarIntBinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,

    And,
    IOr,
    XOr,
    ShL,
    ShR,
}

/// A binary comparison operation.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ScalarCmpBinOp {
    IsEq,
    IsNE,
    IsLT,
    IsLE,
    IsGT,
    IsGE,
}

/// A unary operation on integers.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ScalarIntUnaOp {
    Neg,
    Not,
}
