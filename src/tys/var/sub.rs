use super::{super::{Subtyping, BoundResult, BoundError}, *};

impl Subtyping for IntType {
    fn unify_min(lhs: Self, rhs: Self) -> BoundResult<Self> {
        match (lhs, rhs) {
            (Self::Min, _) | (_, Self::Min) =>
                BoundResult { error: None, inner: Self::Min },
            (Self::Max, x) | (x, Self::Max) =>
                BoundResult { error: None, inner: x },
            (Self::Any, x) | (x, Self::Any) =>
                BoundResult { error: None, inner: x },
            (
                Self::Val { sign: ls, bits: lb },
                Self::Val { sign: rs, bits: rb },
            ) => if ls == rs && lb == rb {
                BoundResult {
                    error: None,
                    inner: Self::Val {
                        sign: ls,
                        bits: lb,
                    },
                }
            } else {
                BoundResult {
                    error: Some(BoundError),
                    inner: Self::Max,
                }
            },
        }
    }

    fn unify_max(lhs: Self, rhs: Self) -> BoundResult<Self> {
        match (lhs, rhs) {
            (Self::Min, x) | (x, Self::Min) =>
                BoundResult { error: None, inner: x },
            (Self::Max, _) | (_, Self::Max) =>
                BoundResult { error: None, inner: Self::Max },
            (Self::Any, x) | (x, Self::Any) =>
                BoundResult { error: None, inner: x },
            (
                Self::Val { sign: ls, bits: lb },
                Self::Val { sign: rs, bits: rb },
            ) => if ls == rs && lb == rb {
                BoundResult {
                    error: None,
                    inner: Self::Val {
                        sign: ls,
                        bits: lb,
                    },
                }
            } else {
                BoundResult {
                    error: Some(BoundError),
                    inner: Self::Min,
                }
            },
        }
    }

    fn infer(self, sup: Self) -> BoundResult<Self> {
        match (self, sup) {
            (x@Self::Min, _) | (x, Self::Max) =>
                BoundResult { error: None, inner: x },
            (Self::Max, x) | (_, x@Self::Min) =>
                BoundResult { error: Some(BoundError), inner: x },
            (Self::Any, x) | (x, Self::Any) =>
                BoundResult { error: None, inner: x },
            (l@Self::Val { .. }, r@Self::Val { .. }) =>
                BoundResult {
                    error: (l != r).then_some(BoundError),
                    inner: r,
                },
        }
    }
}
