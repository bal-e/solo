use super::*;

// --- Subtyping --- //

/// Subtyping which continues after failure.
pub trait Subtyping: Sized {
    /// Unify two types, tending to the minimum.
    fn unify_min(lhs: Self, rhs: Self) -> BoundResult<Self>;

    /// Unify two types, tending to the maximum.
    fn unify_max(lhs: Self, rhs: Self) -> BoundResult<Self>;

    /// Infer this type based on its supertype.
    fn infer(self, sup: Self) -> BoundResult<Self>;
}

/// Subtyping which might stop upon failure.
pub trait SubtypingStop: Sized {
    /// Unify two types, tending to the minimum.
    fn unify_min(lhs: Self, rhs: Self) -> BoundResult<Option<Self>>;

    /// Unify two types, tending to the maximum.
    fn unify_max(lhs: Self, rhs: Self) -> BoundResult<Option<Self>>;

    /// Infer this type based on its supertype.
    fn infer(self, sup: Self) -> BoundResult<Option<Self>>;
}

impl<T: SubtypingStop> SubtypingStop for Option<T> {
    fn unify_min(lhs: Self, rhs: Self) -> BoundResult<Option<Self>> {
        match (lhs, rhs) {
            (Some(l), Some(r)) => SubtypingStop::unify_min(l, r).map(Some),
            (None, None) => Some(None).into(),
            _ => BoundResult { error: Some(BoundError), value: None },
        }
    }

    fn unify_max(lhs: Self, rhs: Self) -> BoundResult<Option<Self>> {
        match (lhs, rhs) {
            (Some(l), Some(r)) => SubtypingStop::unify_max(l, r).map(Some),
            (None, None) => Some(None).into(),
            _ => BoundResult { error: Some(BoundError), value: None },
        }
    }

    fn infer(self, sup: Self) -> BoundResult<Option<Self>> {
        match (self, sup) {
            (Some(l), Some(r)) => SubtypingStop::infer(l, r).map(Some),
            (None, None) => Some(None).into(),
            _ => BoundResult { error: Some(BoundError), value: None },
        }
    }
}

impl SubtypingStop for StreamPart {
    fn unify_min(lhs: Self, rhs: Self) -> BoundResult<Option<Self>> {
        let value = (lhs == rhs).then_some(lhs);
        BoundResult { error: value.is_none().then_some(BoundError), value }
    }

    fn unify_max(lhs: Self, rhs: Self) -> BoundResult<Option<Self>> {
        let value = (lhs == rhs).then_some(lhs);
        BoundResult { error: value.is_none().then_some(BoundError), value }
    }

    fn infer(self, sup: Self) -> BoundResult<Option<Self>> {
        let value = (self == sup).then_some(self);
        BoundResult { error: value.is_none().then_some(BoundError), value }
    }
}

impl SubtypingStop for VectorPart {
    fn unify_min(lhs: Self, rhs: Self) -> BoundResult<Option<Self>> {
        let value = (lhs == rhs).then_some(lhs);
        BoundResult { error: value.is_none().then_some(BoundError), value }
    }

    fn unify_max(lhs: Self, rhs: Self) -> BoundResult<Option<Self>> {
        let value = (lhs == rhs).then_some(lhs);
        BoundResult { error: value.is_none().then_some(BoundError), value }
    }

    fn infer(self, sup: Self) -> BoundResult<Option<Self>> {
        let value = (self == sup).then_some(self);
        BoundResult { error: value.is_none().then_some(BoundError), value }
    }
}

impl SubtypingStop for OptionPart {
    fn unify_min(lhs: Self, rhs: Self) -> BoundResult<Option<Self>> {
        let value = (lhs == rhs).then_some(lhs);
        BoundResult { error: value.is_none().then_some(BoundError), value }
    }

    fn unify_max(lhs: Self, rhs: Self) -> BoundResult<Option<Self>> {
        let value = (lhs == rhs).then_some(lhs);
        BoundResult { error: value.is_none().then_some(BoundError), value }
    }

    fn infer(self, sup: Self) -> BoundResult<Option<Self>> {
        let value = (self == sup).then_some(self);
        BoundResult { error: value.is_none().then_some(BoundError), value }
    }
}

impl SubtypingStop for IntType {
    fn unify_min(lhs: Self, rhs: Self) -> BoundResult<Option<Self>> {
        let value = (lhs == rhs).then_some(lhs);
        BoundResult { error: value.is_none().then_some(BoundError), value }
    }

    fn unify_max(lhs: Self, rhs: Self) -> BoundResult<Option<Self>> {
        let value = (lhs == rhs).then_some(lhs);
        BoundResult { error: value.is_none().then_some(BoundError), value }
    }

    fn infer(self, sup: Self) -> BoundResult<Option<Self>> {
        let value = (self == sup).then_some(self);
        BoundResult { error: value.is_none().then_some(BoundError), value }
    }
}

// --- Partial --- //

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

impl<T: SubtypingStop> Subtyping for Partial<T> {
    fn unify_min(lhs: Self, rhs: Self) -> BoundResult<Self> {
        match (lhs, rhs) {
            (Self::Min, _) | (_, Self::Min) =>
                BoundResult { error: None, value: Self::Min },
            (Self::Max, x) | (x, Self::Max) =>
                BoundResult { error: None, value: x },
            (Self::Any, x) | (x, Self::Any) =>
                BoundResult { error: None, value: x },
            (Self::Val(l), Self::Val(r)) =>
                SubtypingStop::unify_min(l, r)
                    .map(|x| x.map_or(Self::Max, Self::Val)),
        }
    }

    fn unify_max(lhs: Self, rhs: Self) -> BoundResult<Self> {
        match (lhs, rhs) {
            (Self::Min, x) | (x, Self::Min) =>
                BoundResult { error: None, value: x },
            (Self::Max, _) | (_, Self::Max) =>
                BoundResult { error: None, value: Self::Max },
            (Self::Any, x) | (x, Self::Any) =>
                BoundResult { error: None, value: x },
            (Self::Val(l), Self::Val(r)) =>
                SubtypingStop::unify_max(l, r)
                    .map(|x| x.map_or(Self::Max, Self::Val)),
        }
    }

    fn infer(self, sup: Self) -> BoundResult<Self> {
        match (self, sup) {
            (x@Self::Min, _) | (x, Self::Max) =>
                BoundResult { error: None, value: x },
            (Self::Max, x) | (_, x@Self::Min) =>
                BoundResult { error: Some(BoundError), value: x },
            (Self::Any, x) | (x, Self::Any) =>
                BoundResult { error: None, value: x },
            (Self::Val(l), Self::Val(r)) =>
                SubtypingStop::infer(l, r)
                    .map(|x| x.map_or(Self::Max, Self::Val)),
        }
    }
}

impl<T: SubtypingStop> SubtypingStop for Partial<T> {
    fn unify_min(lhs: Self, rhs: Self) -> BoundResult<Option<Self>> {
        Subtyping::unify_min(lhs, rhs).map(Some)
    }

    fn unify_max(lhs: Self, rhs: Self) -> BoundResult<Option<Self>> {
        Subtyping::unify_max(lhs, rhs).map(Some)
    }

    fn infer(self, sup: Self) -> BoundResult<Option<Self>> {
        Subtyping::infer(self, sup).map(Some)
    }
}

// --- BoundResult --- //

/// The result of bounded type unification.
pub struct BoundResult<T> {
    /// The error in bounding, if any.
    pub error: Option<BoundError>,

    /// The underlying value.
    pub value: T,
}

impl<T> BoundResult<T> {
    /// Convert into a [`Result`].
    pub fn ok(self) -> Result<T, BoundError> {
        self.error.map_or(Ok(self.value), Err)
    }

    /// Map over the underlying value.
    pub fn map<R, F>(self, f: F) -> BoundResult<R>
    where F: FnOnce(T) -> R {
        BoundResult {
            error: self.error,
            value: (f)(self.value),
        }
    }

    /// Zip two [`BoundResult`]s.
    pub fn zip<U>(self, other: BoundResult<U>) -> BoundResult<(T, U)> {
        BoundResult {
            error: self.error.or(other.error),
            value: (self.value, other.value),
        }
    }

    /// Fallibly transform the underlying value.
    pub fn and_then<R, F>(self, f: F) -> BoundResult<R>
    where F: FnOnce(T) -> BoundResult<R> {
        let result = (f)(self.value);
        BoundResult {
            error: self.error.or(result.error),
            value: result.value,
        }
    }
}

impl<T> From<T> for BoundResult<T> {
    fn from(value: T) -> Self {
        Self { error: None, value }
    }
}
