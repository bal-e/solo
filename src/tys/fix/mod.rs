//! Concrete types in Solo.

mod cvt;
mod fmt;

pub use super::{
    MappedPart, StreamPart, VectorPart, OptionPart,
    IntType, IntSign,
};

/// A resolved type with mapping components.
pub type MappedType = StreamType;

/// A resolved type with a stream component.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StreamType {
    pub stream: StreamPart,
    pub vector: VectorPart,
    pub option: OptionPart,
    pub scalar: ScalarType,
}

/// A resolved type with a vector component.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VectorType {
    pub vector: VectorPart,
    pub option: OptionPart,
    pub scalar: ScalarType,
}

/// A resolved type with an option component.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OptionType {
    pub option: OptionPart,
    pub scalar: ScalarType,
}

/// A resolved scalar type.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ScalarType {
    /// An integer type.
    Int(IntType),
}
