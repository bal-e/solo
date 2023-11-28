use core::num::NonZeroU32;

mod cvt;
mod fmt;

pub use super::{StreamPart, VectorPart, OptionPart, IntSign};

/// A partially resolved type with all available mapping components.
pub type MappedType = StreamType;

/// A partially resolved type with a stream component.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StreamType {
    pub stream: StreamPart,
    pub vector: VectorPart,
    pub option: OptionPart,
    pub scalar: ScalarType,
}

/// A partially resolved type with a vector component.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VectorType {
    pub vector: VectorPart,
    pub option: OptionPart,
    pub scalar: ScalarType,
}

/// A partially resolved type with an option component.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OptionType {
    pub option: OptionPart,
    pub scalar: ScalarType,
}

/// A partially resolved scalar type.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ScalarType {
    /// An unknown type.
    Any,

    /// An integer type.
    Int(IntType),
}

/// A partially resolved integer type.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IntType {
    pub sign: Option<IntSign>,
    pub bits: Option<NonZeroU32>,
}
