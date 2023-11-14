use core::num::NonZeroU32;

use crate::ast;
use crate::types;

use super::Error;

/// A mapped type.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct MappedType {
    /// The stream part, if any.
    pub stream: Option<StreamPart>,
    /// The vector part, if any.
    pub vector: Option<VectorPart>,
    /// The option part, if any.
    pub option: Option<OptionPart>,
    /// The underlying scalar type.
    pub scalar: ScalarType,
}

impl From<ast::MappedType> for MappedType {
    fn from(value: ast::MappedType) -> Self {
        Self {
            stream: value.stream.map(From::from),
            vector: value.vector.map(From::from),
            option: value.option.map(From::from),
            scalar: value.scalar.into(),
        }
    }
}

impl From<types::StreamType> for MappedType {
    fn from(value: types::StreamType) -> Self {
        Self {
            stream: value.part.map(From::from),
            vector: value.data.part.map(From::from),
            option: value.data.data.part.map(From::from),
            scalar: value.data.data.data.into(),
        }
    }
}

impl TryFrom<MappedType> for types::StreamType {
    type Error = Error;

    fn try_from(value: MappedType) -> Result<Self, Self::Error> {
        Ok(types::StreamType {
            part: value.stream.map(From::from),
            data: types::VectorType {
                part: value.vector.map(From::from),
                data: types::OptionType {
                    part: value.option.map(From::from),
                    data: value.scalar.try_into()?,
                },
            },
        })
    }
}

/// The stream component of a type.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct StreamPart {}

impl From<ast::StreamPart> for StreamPart {
    fn from(value: ast::StreamPart) -> Self {
        let ast::StreamPart {} = value;
        Self {}
    }
}

impl From<types::StreamPart> for StreamPart {
    fn from(value: types::StreamPart) -> Self {
        let types::StreamPart {} = value;
        Self {}
    }
}

impl From<StreamPart> for types::StreamPart {
    fn from(value: StreamPart) -> Self {
        let StreamPart {} = value;
        Self {}
    }
}

/// The vector component of a type.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct VectorPart {
    /// The number of elements in each vector.
    pub size: u32,
}

impl From<ast::VectorPart> for VectorPart {
    fn from(value: ast::VectorPart) -> Self {
        Self { size: value.size }
    }
}

impl From<types::VectorPart> for VectorPart {
    fn from(value: types::VectorPart) -> Self {
        Self { size: value.size }
    }
}

impl From<VectorPart> for types::VectorPart {
    fn from(value: VectorPart) -> Self {
        Self { size: value.size }
    }
}

/// The option component of a type.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct OptionPart {}

impl From<ast::OptionPart> for OptionPart {
    fn from(value: ast::OptionPart) -> Self {
        let ast::OptionPart {} = value;
        Self {}
    }
}

impl From<types::OptionPart> for OptionPart {
    fn from(value: types::OptionPart) -> Self {
        let types::OptionPart {} = value;
        Self {}
    }
}

impl From<OptionPart> for types::OptionPart {
    fn from(value: OptionPart) -> Self {
        let OptionPart {} = value;
        Self {}
    }
}

/// A scalar type.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ScalarType {
    /// An arbitrary scalar type.
    Any,

    /// An integer type.
    Int(Option<IntType>),
}

impl From<ast::ScalarType> for ScalarType {
    fn from(value: ast::ScalarType) -> Self {
        match value {
            ast::ScalarType::Int(r#type) =>
                Self::Int(Some(r#type.into())),
        }
    }
}

impl From<types::ScalarType> for ScalarType {
    fn from(value: types::ScalarType) -> Self {
        match value {
            types::ScalarType::Int(r#type) =>
                Self::Int(Some(r#type.into())),
        }
    }
}

impl TryFrom<ScalarType> for types::ScalarType {
    type Error = Error;

    fn try_from(value: ScalarType) -> Result<Self, Self::Error> {
        match value {
            ScalarType::Any => Err(Error::Unresolvable),
            ScalarType::Int(None) => Err(Error::Unresolvable),
            ScalarType::Int(Some(r#type)) => Ok(Self::Int(r#type.into())),
        }
    }
}

/// An integer type.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct IntType {
    /// The sign of the type.
    pub sign: IntSign,
    /// The size of each integer, in bits.
    pub size: NonZeroU32,
}

impl From<ast::IntType> for IntType {
    fn from(value: ast::IntType) -> Self {
        match value {
            ast::IntType::U(size) => Self { sign: IntSign::U, size },
            ast::IntType::S(size) => Self { sign: IntSign::S, size },
        }
    }
}

impl From<types::IntType> for IntType {
    fn from(value: types::IntType) -> Self {
        Self {
            sign: value.sign.into(),
            size: value.size,
        }
    }
}

impl From<IntType> for types::IntType {
    fn from(value: IntType) -> Self {
        Self {
            sign: value.sign.into(),
            size: value.size,
        }
    }
}

/// The sign of an integer type.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum IntSign {
    /// An unsigned integer.
    U,
    /// A signed integer.
    S,
}

impl From<types::IntSign> for IntSign {
    fn from(value: types::IntSign) -> Self {
        match value {
            types::IntSign::U => Self::U,
            types::IntSign::S => Self::S,
        }
    }
}

impl From<IntSign> for types::IntSign {
    fn from(value: IntSign) -> Self {
        match value {
            IntSign::U => Self::U,
            IntSign::S => Self::S,
        }
    }
}
