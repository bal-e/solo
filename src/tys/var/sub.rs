use super::{super::{Subtyping, BoundResult, BoundError}, *};

impl<T: Subtyping> Subtyping for Partial<T> {
    fn unify_min(lhs: Self, rhs: Self) -> BoundResult<Self> {
        match (lhs, rhs) {
            (Self::Min, _) | (_, Self::Min) =>
                BoundResult { error: None, value: Self::Min },
            (Self::Max, x) | (x, Self::Max) =>
                BoundResult { error: None, value: x },
            (Self::Any, x) | (x, Self::Any) =>
                BoundResult { error: None, value: x },
            (Self::Val(l), Self::Val(r)) =>
                Subtyping::unify_min(l, r).map(Self::Val),
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
                Subtyping::unify_max(l, r).map(Self::Val),
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
                Subtyping::infer(l, r).map(Self::Val),
        }
    }
}
