//! Partially resolved types in Solo.

mod cvt;
mod fmt;
mod sub;

pub use super::{
    Partial,
    MappedPart, StreamPart, VectorPart, OptionPart,
    IntType, IntSign,
};

/// A partially resolved type with mapping components.
pub type MappedType = StreamType;

/// A partially resolved type with a stream component.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StreamType {
    pub stream: Partial<StreamPart>,
    pub vector: Partial<VectorPart>,
    pub option: Partial<OptionPart>,
    pub scalar: Partial<ScalarType>,
}

/// A partially resolved type with a vector component.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VectorType {
    pub vector: Partial<VectorPart>,
    pub option: Partial<OptionPart>,
    pub scalar: Partial<ScalarType>,
}

/// A partially resolved type with an option component.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OptionType {
    pub option: Partial<OptionPart>,
    pub scalar: Partial<ScalarType>,
}

/// A partially resolved scalar type.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ScalarType {
    /// An integer type.
    Int(Partial<IntType>),
}
