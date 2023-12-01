use super::BoundError;

/// Subtyping.
pub trait Subtyping: Sized {
    /// Unify two types, tending to the minimum.
    fn unify_min(lhs: Self, rhs: Self) -> BoundResult<Self>;

    /// Unify two types, tending to the maximum.
    fn unify_max(lhs: Self, rhs: Self) -> BoundResult<Self>;

    /// Infer this type based on its supertype.
    fn infer(self, sup: Self) -> BoundResult<Self>;
}

/// The result of bounded type unification.
pub struct BoundResult<T> {
    /// The error in bounding, if any.
    pub error: Option<BoundError>,

    /// The underlying value.
    pub value: T,
}

impl<T> BoundResult<T> {
    /// Map over the underlying value.
    pub fn map<R, F>(self, f: F) -> BoundResult<R>
    where F: FnOnce(T) -> R {
        BoundResult {
            error: self.error,
            value: (f)(self.value),
        }
    }
}
