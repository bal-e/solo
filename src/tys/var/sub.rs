use super::{super::*, *};

impl Subtyping for OptionType {
    fn unify_min(lhs: Self, rhs: Self) -> BoundResult<Self> {
        Subtyping::unify_min(lhs.scalar, rhs.scalar)
            .zip(Subtyping::unify_max(lhs.option, rhs.option))
            .map(|(scalar, option)| Self { scalar, option })
    }

    fn unify_max(lhs: Self, rhs: Self) -> BoundResult<Self> {
        Subtyping::unify_max(lhs.scalar, rhs.scalar)
            .zip(Subtyping::unify_max(lhs.option, rhs.option))
            .map(|(scalar, option)| Self { scalar, option })
    }

    fn infer(self, sup: Self) -> BoundResult<Self> {
        Subtyping::infer(self.scalar, sup.scalar)
            .zip(Subtyping::infer(self.option, sup.option))
            .map(|(scalar, option)| Self { scalar, option })
    }
}

impl SubtypingStop for OptionType {
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

impl Subtyping for ScalarType {
    fn unify_min(lhs: Self, rhs: Self) -> BoundResult<Self> {
        match (lhs, rhs) {
            (Self::Int(l), Self::Int(r)) =>
                Subtyping::unify_min(l, r).map(Self::Int),
        }
    }

    fn unify_max(lhs: Self, rhs: Self) -> BoundResult<Self> {
        match (lhs, rhs) {
            (Self::Int(l), Self::Int(r)) =>
                Subtyping::unify_max(l, r).map(Self::Int),
        }
    }

    fn infer(self, sup: Self) -> BoundResult<Self> {
        match (self, sup) {
            (Self::Int(l), Self::Int(r)) =>
                Subtyping::infer(l, r).map(Self::Int),
        }
    }
}

impl SubtypingStop for ScalarType {
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
