//! Types in Solo.

use core::num::NonZeroU32;

pub mod fix;
pub mod var;

mod err;
pub use err::*;

mod sub;
pub use sub::*;

mod cvt;
mod fmt;

/// The mapping components of a type.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MappedPart {
    pub stream: StreamPart,
    pub vector: VectorPart,
    pub option: OptionPart,
}

/// A stream component to type.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum StreamPart {
    Some {},
    None,
}

/// A vector component to a type.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum VectorPart {
    Some { size: u32 },
    None,
}

impl VectorPart {
    /// Get the vector size.
    pub fn size(self) -> Option<u32> {
        match self {
            Self::Some { size } => Some(size),
            Self::None => None,
        }
    }
}

/// An option component to a type.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum OptionPart {
    Some {},
    None,
}

/// An integer type.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IntType {
    pub sign: IntSign,
    pub bits: NonZeroU32,
}

/// The sign of an integer type.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IntSign {
    U,
    S,
}
