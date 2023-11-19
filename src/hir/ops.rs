pub use crate::ops::*;
use crate::types::*;

/// A binary operation on streams.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum StreamBinOp {
    /// A mapped binary operation on vectors.
    Map {
        /// The underlying operation.
        bop: VectorBinOp,
    },

    /// Stream expansion.
    Exp {
        /// The type of the left side.
        lhs: VectorType,
    },

    /// Stream reduction.
    Red {
        /// The underlying type of the left side.
        lhs: VectorType,
    },
}

impl StreamBinOp {
    /// The destination type.
    pub fn dst_type(self, src: [Option<StreamPart>; 2]) -> StreamType {
        match self {
            Self::Map { bop } => StreamType {
                part: src.into_iter().fold(None, Option::or),
                data: bop.dst_type(),
            },

            Self::Exp { lhs } => StreamType {
                part: Some(StreamPart {}),
                data: lhs,
            },

            Self::Red { lhs } => StreamType {
                part: Some(StreamPart {}),
                data: lhs,
            },
        }
    }
}

/// A unary operation on streams.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum StreamUnaOp {
    /// A mapped unary operation on vectors.
    Map {
        /// The underlying operation.
        uop: VectorUnaOp,
    },
}

impl StreamUnaOp {
    /// The destination type.
    pub fn dst_type(self, src: Option<StreamPart>) -> StreamType {
        match self {
            Self::Map { uop } => StreamType {
                part: src,
                data: uop.dst_type(),
            },
        }
    }
}

/// A nilary operation of streams.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum StreamNilOp {
    /// A mapped nilary operation of vectors.
    Map {
        /// The underlying operation.
        nop: VectorNilOp,

        /// The stream part being mapped over.
        map: Option<StreamPart>,
    },
}

impl StreamNilOp {
    /// The destination type.
    pub fn dst_type(self) -> StreamType {
        match self {
            Self::Map { nop, map } => StreamType { part: map, data: nop.dst_type() },
        }
    }
}
