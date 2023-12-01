//! Partially resolved types in Solo.

mod cvt;
mod fmt;

mod sub;
pub use sub::*;

pub use super::{StreamPart, VectorPart, OptionPart, IntType, IntSign};
pub use super::err::UnresolvedError;

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

/// A partially resolved object.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Partial<T> {
    /// The minimum object.
    Min,

    /// The maximum object.
    Max,

    /// An unresolved object.
    Any,

    /// A concrete object.
    Val(T),
}

impl<T> Partial<T> {
    /// Extract a concrete object or fail.
    pub fn val(self) -> Result<T, UnresolvedError> {
        match self {
            Self::Val(val) => Ok(val),
            _ => Err(UnresolvedError),
        }
    }
}

impl<T> From<T> for Partial<T> {
    fn from(value: T) -> Self {
        Self::Val(value)
    }
}
