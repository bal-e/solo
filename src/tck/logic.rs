//! Type logic.

use thiserror::Error;

use super::types::*;

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

impl<T: Subtyping> Subtyping for Option<T> {
    fn is_subtype_of(&self, sup: &Self) -> bool {
        match (self, sup) {
            (None, _) => true,
            (Some(_), None) => false,
            (Some(l), Some(r)) => l.is_subtype_of(r),
        }
    }

    fn merge_min(lhs: Self, rhs: Self) -> Result<Self, Error> {
        match (lhs, rhs) {
            (None, x) | (x, None) => Ok(x),
            (Some(l), Some(r)) => T::merge_min(l, r).map(Some),
        }
    }

    fn merge_max(lhs: Self, rhs: Self) -> Result<Self, Error> {
        match (lhs, rhs) {
            (None, _) | (_, None) => Ok(None),
            (Some(l), Some(r)) => T::merge_max(l, r).map(Some),
        }
    }

    fn infer_min(self, sup: Self) -> Result<Self, Error> {
        match (self, sup) {
            (None, x) | (x, None) => Ok(x),
            (Some(l), Some(r)) => T::infer_min(l, r).map(Some),
        }
    }
}

impl Subtyping for StreamType {
    fn is_subtype_of(&self, sup: &Self) -> bool {
        self.part.is_subtype_of(&sup.part) && self.data.is_subtype_of(&sup.data)
    }

    fn merge_min(lhs: Self, rhs: Self) -> Result<Self, Error> {
        Ok(Self {
            part: Subtyping::merge_min(lhs.part, rhs.part)?,
            data: Subtyping::merge_min(lhs.data, rhs.data)?,
        })
    }

    fn merge_max(lhs: Self, rhs: Self) -> Result<Self, Error> {
        Ok(Self {
            part: Subtyping::merge_max(lhs.part, rhs.part)?,
            data: Subtyping::merge_max(lhs.data, rhs.data)?,
        })
    }

    fn infer_min(self, sup: Self) -> Result<Self, Error> {
        if !self.is_subtype_of(&sup) { return Err(Error::Subtype); }
        Ok(Self { data: self.data.infer_min(sup.data)?, ..self })
    }
}

impl Subtyping for VectorType {
    fn is_subtype_of(&self, sup: &Self) -> bool {
        self.part.is_subtype_of(&sup.part) && self.data.is_subtype_of(&sup.data)
    }

    fn merge_min(lhs: Self, rhs: Self) -> Result<Self, Error> {
        Ok(Self {
            part: Subtyping::merge_min(lhs.part, rhs.part)?,
            data: Subtyping::merge_min(lhs.data, rhs.data)?,
        })
    }

    fn merge_max(lhs: Self, rhs: Self) -> Result<Self, Error> {
        Ok(Self {
            part: Subtyping::merge_max(lhs.part, rhs.part)?,
            data: Subtyping::merge_max(lhs.data, rhs.data)?,
        })
    }

    fn infer_min(self, sup: Self) -> Result<Self, Error> {
        if !self.is_subtype_of(&sup) { return Err(Error::Subtype); }
        Ok(Self { data: self.data.infer_min(sup.data)?, ..self })
    }
}

impl Subtyping for OptionType {
    fn is_subtype_of(&self, sup: &Self) -> bool {
        self.part.is_subtype_of(&sup.part) && self.data.is_subtype_of(&sup.data)
    }

    fn merge_min(lhs: Self, rhs: Self) -> Result<Self, Error> {
        Ok(Self {
            part: Subtyping::merge_min(lhs.part, rhs.part)?,
            data: Subtyping::merge_min(lhs.data, rhs.data)?,
        })
    }

    fn merge_max(lhs: Self, rhs: Self) -> Result<Self, Error> {
        Ok(Self {
            part: Subtyping::merge_max(lhs.part, rhs.part)?,
            data: Subtyping::merge_max(lhs.data, rhs.data)?,
        })
    }

    fn infer_min(self, sup: Self) -> Result<Self, Error> {
        if !self.is_subtype_of(&sup) { return Err(Error::Subtype); }
        Ok(Self { data: self.data.infer_min(sup.data)?, ..self })
    }
}

impl Subtyping for StreamPart {
    fn is_subtype_of(&self, _: &Self) -> bool {
        true
    }

    fn merge_min(_: Self, _: Self) -> Result<Self, Error> {
        Ok(Self {})
    }

    fn merge_max(_: Self, _: Self) -> Result<Self, Error> {
        Ok(Self {})
    }

    fn infer_min(self, _: Self) -> Result<Self, Error> {
        Ok(Self {})
    }
}

impl Subtyping for VectorPart {
    fn is_subtype_of(&self, sup: &Self) -> bool {
        self == sup
    }

    fn merge_min(lhs: Self, rhs: Self) -> Result<Self, Error> {
        if lhs == rhs {
            Ok(lhs)
        } else {
            Err(Error::Merge)
        }
    }

    fn merge_max(lhs: Self, rhs: Self) -> Result<Self, Error> {
        if lhs == rhs {
            Ok(lhs)
        } else {
            Err(Error::Merge)
        }
    }

    fn infer_min(self, sup: Self) -> Result<Self, Error> {
        if self == sup {
            Ok(self)
        } else {
            Err(Error::Subtype)
        }
    }
}

impl Subtyping for OptionPart {
    fn is_subtype_of(&self, _: &Self) -> bool {
        true
    }

    fn merge_min(_: Self, _: Self) -> Result<Self, Error> {
        Ok(Self {})
    }

    fn merge_max(_: Self, _: Self) -> Result<Self, Error> {
        Ok(Self {})
    }

    fn infer_min(self, _: Self) -> Result<Self, Error> {
        Ok(Self {})
    }
}

impl Subtyping for ScalarType {
    fn is_subtype_of(&self, sup: &Self) -> bool {
        match (self, sup) {
            // Everything is a supertype of 'Any'.
            (_, Self::Any) => true,
            // Delegate when appropriate.
            (Self::Int(l), Self::Int(r)) => l.is_subtype_of(r),
            // All other combinations are invalid.
            _ => false,
        }
    }

    fn merge_min(lhs: Self, rhs: Self) -> Result<Self, Error> {
        match (lhs, rhs) {
            // Ignore any 'Any'.
            (Self::Any, x) | (x, Self::Any) => Ok(x),
            // Delegate when appropriate.
            (Self::Int(l), Self::Int(r)) =>
                Subtyping::merge_min(l, r).map(Self::Int),
        }
    }

    fn merge_max(lhs: Self, rhs: Self) -> Result<Self, Error> {
        match (lhs, rhs) {
            // Ignore any 'Any'.
            (Self::Any, x) | (x, Self::Any) => Ok(x),
            // Delegate when appropriate.
            (Self::Int(l), Self::Int(r)) =>
                Subtyping::merge_max(l, r).map(Self::Int),
        }
    }

    fn infer_min(self, sup: Self) -> Result<Self, Error> {
        match (self, sup) {
            // Everything is a supertype of 'Any'.
            (x, Self::Any) => Ok(x),
            // If 'self' is 'Any', then infer from the supertype.
            (Self::Any, x) => Ok(x),
            // Delegate when appropriate.
            (Self::Int(l), Self::Int(r)) =>
                l.infer_min(r).map(Self::Int),
        }
    }
}

impl Subtyping for IntType {
    fn is_subtype_of(&self, sup: &Self) -> bool {
        self == sup
    }

    fn merge_min(lhs: Self, rhs: Self) -> Result<Self, Error> {
        if lhs == rhs {
            Ok(lhs)
        } else {
            Err(Error::Merge)
        }
    }

    fn merge_max(lhs: Self, rhs: Self) -> Result<Self, Error> {
        if lhs == rhs {
            Ok(lhs)
        } else {
            Err(Error::Merge)
        }
    }

    fn infer_min(self, sup: Self) -> Result<Self, Error> {
        if self == sup {
            Ok(self)
        } else {
            Err(Error::Subtype)
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
