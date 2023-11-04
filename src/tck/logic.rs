//! Type logic.

use core::fmt;
use core::num::NonZeroU32;

use thiserror::Error;

/// Subtyping.
pub trait Subtyping: Sized {
    /// Whether this is a subtype of another.
    fn is_subtype_of(&self, sup: &Self) -> bool;

    /// Merge two types, tending to the minimum.
    fn merge_min(lhs: Self, rhs: Self) -> Result<Self, Error>;

    /// Merge two types, tending to the maximum.
    fn merge_max(lhs: Self, rhs: Self) -> Result<Self, Error>;

    /// Infer a subtype based on its supertype.
    fn infer_min(self, sup: Self) -> Result<Self, Error>;
}

/// A mapping type.
#[derive(Copy, Clone)]
pub struct MapType {
    /// The underlying scalar type.
    pub scalar: Type,
    /// Whether elements are optional.
    pub option: bool,
    /// Whether elements are vectors (of options).
    pub vector: bool,
    /// Whether elements are streams (of vectors and/or of options),
    pub stream: bool,
}

impl Subtyping for MapType {
    fn is_subtype_of(&self, sup: &Self) -> bool {
        // If 'sup' has a mapping component but 'self' does not, instances of
        // 'self' would be broadcasted to fit; on the other hand, 'self' cannot
        // have any mapping components that 'sup' is not expecting.

        self.scalar.is_subtype_of(&sup.scalar)
            && (!self.option || sup.option)
            && (!self.vector || sup.vector)
            && (!self.stream || sup.stream)
    }

    fn merge_min(lhs: Self, rhs: Self) -> Result<Self, Error> {
        Ok(Self {
            scalar: Type::merge_min(lhs.scalar, rhs.scalar)?,
            option: lhs.option && rhs.option,
            vector: lhs.vector && rhs.vector,
            stream: lhs.stream && rhs.stream,
        })
    }

    fn merge_max(lhs: Self, rhs: Self) -> Result<Self, Error> {
        Ok(Self {
            scalar: Type::merge_max(lhs.scalar, rhs.scalar)?,
            option: lhs.option || rhs.option,
            vector: lhs.vector || rhs.vector,
            stream: lhs.stream || rhs.stream,
        })
    }

    fn infer_min(self, sup: Self) -> Result<Self, Error> {
        let scalar = self.scalar.infer_min(sup.scalar)?;
        if !self.is_subtype_of(&sup) { return Err(Error::Subtype); }

        // None of the mapping components in 'self' can change.
        Ok(Self { scalar, ..self })
    }
}

impl fmt::Display for MapType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.stream { f.write_str("[]")?; }
        if self.vector { f.write_str("()")?; }
        if self.option { f.write_str("?")?; }
        <Type as fmt::Display>::fmt(&self.scalar, f)
    }
}

/// A scalar type.
#[derive(Copy, Clone)]
pub enum Type {
    /// An arbitrary type.
    Any,

    /// An integer type.
    Int(IntType),
}

impl Subtyping for Type {
    fn is_subtype_of(&self, sup: &Self) -> bool {
        match (self, sup) {
            // Everything is a supertype of 'Any'.
            (_, Self::Any) => true,
            // Delegate to 'IntType' when appropriate.
            (Self::Int(l), Self::Int(r)) => l.is_subtype_of(r),
            // All other combinations are invalid.
            _ => false,
        }
    }

    fn merge_min(lhs: Self, rhs: Self) -> Result<Self, Error> {
        match (lhs, rhs) {
            // Ignore any 'Any'.
            (Self::Any, x) | (x, Self::Any) => Ok(x),
            // Delegate to 'IntType' when appropriate.
            (Self::Int(l), Self::Int(r)) =>
                IntType::merge_min(l, r).map(Self::Int),
        }
    }

    fn merge_max(lhs: Self, rhs: Self) -> Result<Self, Error> {
        match (lhs, rhs) {
            // If an 'Any' is seen, succeed immediately.
            (Self::Any, _) | (_, Self::Any) => Ok(Self::Any),
            // Delegate to 'IntType' when appropriate.
            (Self::Int(l), Self::Int(r)) =>
                IntType::merge_max(l, r).map(Self::Int),
        }
    }

    fn infer_min(self, sup: Self) -> Result<Self, Error> {
        match (self, sup) {
            // Everything is a supertype of 'Any'.
            (_, Self::Any) => Ok(self),
            // If 'self' is 'Any', then infer from the supertype.
            (Self::Any, _) => Ok(sup),
            // Delegate to 'IntType' when appropriate.
            (Self::Int(l), Self::Int(r)) =>
                l.infer_min(r).map(Self::Int),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Any => f.write_str("any"),
            Self::Int(i) => <IntType as fmt::Display>::fmt(&i, f),
        }
    }
}

/// An integer type.
#[derive(Copy, Clone)]
pub enum IntType {
    /// An arbitrary integer type.
    Any,

    /// A signed integer type.
    S(NonZeroU32),

    /// An unsigned integer type.
    U(NonZeroU32),
}

impl Subtyping for IntType {
    fn is_subtype_of(&self, sup: &Self) -> bool {
        match (self, sup) {
            // Everything is a supertype of 'Any'.
            (_, Self::Any) => true,
            // Delegate to 'U' when appropriate.
            (Self::U(l), Self::U(r)) => l == r,
            // Delegate to 'S' when appropriate.
            (Self::S(l), Self::S(r)) => l == r,
            // All other combinations are invalid.
            _ => false,
        }
    }

    fn merge_min(lhs: Self, rhs: Self) -> Result<Self, Error> {
        match (lhs, rhs) {
            // Ignore any 'Any'.
            (Self::Any, x) | (x, Self::Any) => Ok(x),
            // Delegate to 'U' when appropriate.
            (Self::U(l), Self::U(r)) if l == r => Ok(Self::U(l)),
            // Delegate to 'S' when appropriate.
            (Self::S(l), Self::S(r)) if l == r => Ok(Self::S(l)),
            // All other combinations are invalid.
            _ => Err(Error::Merge),
        }
    }

    fn merge_max(lhs: Self, rhs: Self) -> Result<Self, Error> {
        match (lhs, rhs) {
            // If an 'Any' is seen, succeed immediately.
            (Self::Any, _) | (_, Self::Any) => Ok(Self::Any),
            // Delegate to 'U' when appropriate.
            (Self::U(l), Self::U(r)) if l == r => Ok(Self::U(l)),
            // Delegate to 'S' when appropriate.
            (Self::S(l), Self::S(r)) if l == r => Ok(Self::S(l)),
            // All other combinations are invalid.
            _ => Err(Error::Merge),
        }
    }

    fn infer_min(self, sup: Self) -> Result<Self, Error> {
        match (self, sup) {
            // Everything is a supertype of 'Any'.
            (_, Self::Any) => Ok(self),
            // If 'self' is 'Any', then infer from the supertype.
            (Self::Any, _) => Ok(sup),
            // Delegate to 'U' when appropriate.
            (Self::U(l), Self::U(r)) if l == r => Ok(Self::U(l)),
            // Delegate to 'S' when appropriate.
            (Self::S(l), Self::S(r)) if l == r => Ok(Self::S(l)),
            // All other combinations are invalid.
            _ => Err(Error::Subtype),
        }
    }
}

impl fmt::Display for IntType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Any => f.write_str("int"),
            Self::U(s) => write!(f, "u{s}"),
            Self::S(s) => write!(f, "s{s}"),
        }
    }
}

/// Type logic errors.
#[derive(Debug, Error)]
pub enum Error {
    #[error("Incompatible types cannot be merged")]
    Merge,

    #[error("A type was not a subset of another")]
    Subtype,
}
