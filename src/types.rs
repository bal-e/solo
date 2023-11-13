//! Types.

use core::fmt;
use core::num::NonZeroU32;

/// A stream type.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct StreamType {
    /// The stream part, if any.
    pub part: Option<StreamPart>,
    /// The underlying type.
    pub data: VectorType,
}

impl fmt::Display for StreamType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(part) = self.part {
            fmt::Display::fmt(&part, f)?;
        }
        fmt::Display::fmt(&self.data, f)
    }
}

/// The stream component of a type.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct StreamPart {}

impl fmt::Display for StreamPart {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("..")
    }
}

/// A vector type.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct VectorType {
    /// The vector part, if any.
    pub part: Option<VectorPart>,
    /// The underlying type.
    pub data: OptionType,
}

impl fmt::Display for VectorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(part) = self.part {
            fmt::Display::fmt(&part, f)?;
        }
        fmt::Display::fmt(&self.data, f)
    }
}

/// The vector component of a type.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct VectorPart {
    /// The number of elements in each vector.
    pub size: u32,
}

impl fmt::Display for VectorPart {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<{}>", self.size)
    }
}

/// An option type.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct OptionType {
    /// The option part, if any.
    pub part: Option<OptionPart>,
    /// The underlying type.
    pub data: ScalarType,
}

impl fmt::Display for OptionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(part) = self.part {
            fmt::Display::fmt(&part, f)?;
        }
        fmt::Display::fmt(&self.data, f)
    }
}

/// The option component of a type.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct OptionPart {}

impl fmt::Display for OptionPart {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("?")
    }
}

/// A scalar type.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ScalarType {
    /// An integer type.
    Int(IntType),
}

impl fmt::Display for ScalarType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(this) => fmt::Display::fmt(&this, f),
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

impl fmt::Display for IntType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.sign, f)?;
        fmt::Display::fmt(&self.size, f)
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

impl fmt::Display for IntSign {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::U => f.write_str("u"),
            Self::S => f.write_str("s"),
        }
    }
}
