use crate::tys::{Subtyping, BoundResult, var::*};

use super::*;

impl BinOp {
    /// Determine the merges involved in this type of operation.
    pub fn merges(&self) -> BinMerges {
        match self {
            Self::Exp => BinMerges::LD,
            Self::Red => BinMerges::LD,
            Self::Cat => BinMerges::LRD,
            Self::Ind => BinMerges::LD,
            Self::Cond => BinMerges::RD,
            Self::Else => BinMerges::LRD,
            Self::Int(_) => BinMerges::LRD,
            Self::Cmp(_) => BinMerges::LR,
        }
    }

    /// Determine the destination type of this operation.
    pub fn dst_type(
        &self,
        src: [MappedType; 2],
    ) -> BoundResult<MappedType> {
        let [lhs, rhs] = src;

        let stream = |dst| dst;

        let vector = |dst: BoundResult<VectorType>| {
            (stream)(dst.and_then(|dst| {
                Subtyping::unify_max(lhs.stream, rhs.stream)
                    .map(|stream| dst.with_part(stream))
            }))
        };

        let option = |dst: BoundResult<OptionType>| {
            (vector)(dst.and_then(|dst| {
                Subtyping::unify_max(lhs.vector, rhs.vector)
                    .map(|vector| dst.with_part(vector))
            }))
        };

        let scalar = |dst: BoundResult<Partial<ScalarType>>| {
            (option)(dst.and_then(|dst| {
                Subtyping::unify_max(lhs.option, rhs.option)
                    .map(|option| dst.with_part(option))
            }))
        };

        match self {
            Self::Exp => (stream)({
                rhs.stream.infer(Partial::Val(StreamPart::Some {}))
                    .zip(rhs.option.infer(Partial::Val(OptionPart::None)))
                .and_then(|(rhs_stream, rhs_option)| {
                    let mut rhs = rhs;
                    rhs.stream = rhs_stream;
                    rhs.option = rhs_option;

                    Subtyping::unify_max(lhs.vector, rhs.vector)
                        .map(|vector| OptionType::from(lhs)
                             .with_part(vector)
                             .with_part(rhs.stream))
                })
            }),

            Self::Red => (stream)({
                lhs.stream.infer(Partial::Val(StreamPart::Some {}))
                    .zip(rhs.stream.infer(Partial::Val(StreamPart::Some {})))
                    .zip(rhs.option.infer(Partial::Val(OptionPart::None)))
                .and_then(|((lhs_stream, rhs_stream), rhs_option)| {
                    let (mut lhs, mut rhs) = (lhs, rhs);
                    lhs.stream = lhs_stream;
                    rhs.stream = rhs_stream;
                    rhs.option = rhs_option;

                    Subtyping::unify_max(lhs.vector, rhs.vector)
                        .map(|vector| OptionType::from(lhs)
                             .with_part(vector)
                             .with_part(rhs.stream))
                })
            }),

            Self::Cat => (vector)({
                let vector = match (lhs.vector, rhs.vector) {
                    (Partial::Min, _) | (_, Partial::Min) => Partial::Min,
                    (Partial::Max, _) | (_, Partial::Max) => Partial::Max,
                    (Partial::Any, _) | (_, Partial::Any) => Partial::Any,
                    (Partial::Val(l), Partial::Val(r)) =>
                        Partial::Val(VectorPart::Some {
                            size: l.size().unwrap_or(1) + r.size().unwrap_or(1),
                        }),
                };

                OptionType::unify_max(lhs.into(), rhs.into())
                    .map(|option| option.with_part(vector))
            }),

            Self::Ind => (vector)({
                Subtyping::unify_max(lhs.option, rhs.option)
                    .map(|option| lhs.scalar
                         .with_part(option)
                         .with_part(rhs.vector))
            }),

            Self::Cond => (option)({
                Subtyping::infer(lhs.option, Partial::Val(OptionPart::None))
                    .map(|_| {
                        rhs.scalar.with_part(Partial::Val(OptionPart::Some {}))
                    })
            }),

            Self::Else => (option)({
                Subtyping::infer(lhs.option, Partial::Val(OptionPart::None))
                    .and_then(|lhs_option| {
                        let mut lhs = lhs;
                        lhs.option = lhs_option;

                        Subtyping::unify_min(lhs.scalar, rhs.scalar)
                            .map(|scalar| scalar.with_part(rhs.option))
                    })
            }),

            Self::Int(_) => (scalar)({
                Subtyping::unify_min(lhs.scalar, rhs.scalar)
            }),

            Self::Cmp(_) => (scalar)({
                Subtyping::unify_min(lhs.scalar, rhs.scalar)
                    .map(|_| Partial::Val(ScalarType::Int(Partial::Any)))
            }),
        }
    }

    /// Infer the scalar parts of the source types of this operation.
    pub fn src_type(
        &self,
        dst: Partial<ScalarType>,
    ) -> BoundResult<[Partial<ScalarType>; 2]> {
        match self {
            Self::Exp => [dst, Partial::Val(ScalarType::Int(Partial::Max))],
            Self::Red => [dst, Partial::Val(ScalarType::Int(Partial::Max))],
            Self::Cat => [dst, dst],
            Self::Ind => [dst, Partial::Val(ScalarType::Int(Partial::Max))],
            Self::Cond => [Partial::Val(ScalarType::Int(Partial::Max)), dst],
            Self::Else => [dst, dst],
            Self::Int(_) => {
                let src = Partial::Val(ScalarType::Int(Partial::Max));
                return dst.infer(src).map(|src| [src, src]);
            }
            Self::Cmp(_) => [dst, dst],
        }.into()
    }

    /// Infer the mapping parts of the source types of this operation.
    pub fn src_map_part(&self, mut src: [MappedPart; 2]) -> [MappedPart; 2] {
        // Infer stream parts.
        if let Self::Exp | Self::Red = self {
            let stream = StreamPart::Some {};
            src.iter_mut().for_each(|x| x.stream = stream);
        } else {
            let stream = Subtyping::unify_max(src[0].stream, src[1].stream)
                .ok().expect("Type-checking has already completed");
            src.iter_mut().for_each(|x| x.stream = stream);
        }

        // Infer vector parts.
        if let Self::Cat | Self::Ind = self {
            // Both sides have independent 'vector' parts.
        } else {
            use crate::tys::SubtypingStop;

            let vector = SubtypingStop::unify_max(src[0].vector, src[1].vector)
                .ok().expect("Type-checking has already completed").unwrap();
            src.iter_mut().for_each(|x| x.vector = vector);
        }

        // Infer option parts.
        match self {
            Self::Exp => {
                // The output is optional anyways, so we always broadcast.
                src[0].option = OptionPart::Some {};
            },

            Self::Red => {
                // As a mask, the RHS never has an 'option' part.
            },

            Self::Cond => {
                // The output has an 'option' part anyways, so we broadcast.
                src[1].option = OptionPart::Some {};
            },

            Self::Else => {
                // The LHS is always expected to have an 'option' part.
                src[0].option = OptionPart::Some {};
            },

            _ => {
                let option = Subtyping::unify_max(src[0].option, src[1].option)
                    .ok().expect("Type-checking has already completed");
                src.iter_mut().for_each(|x| x.option = option);
            },
        };

        src
    }
}

impl UnaOp {
    /// Determine the merges involved in this type of operation.
    pub fn merges(&self) -> UnaMerges {
        match self {
            Self::Int(_) => UnaMerges::SD,
        }
    }

    /// Determine the destination type of this operation.
    pub fn dst_type(
        &self,
        src: MappedType,
    ) -> BoundResult<MappedType> {
        let stream = |dst| dst;

        let vector = |dst: BoundResult<VectorType>| {
            (stream)(dst.map(|dst| dst.with_part(src.stream)))
        };

        let option = |dst: BoundResult<OptionType>| {
            (vector)(dst.map(|dst| dst.with_part(src.vector)))
        };

        let scalar = |dst: BoundResult<Partial<ScalarType>>| {
            (option)(dst.map(|dst| dst.with_part(src.option)))
        };

        match self {
            Self::Int(_) => (scalar)(src.scalar.into()),
        }
    }

    /// Infer the scalar parts of the source types of this operation.
    pub fn src_type(
        &self,
        dst: Partial<ScalarType>,
    ) -> BoundResult<Partial<ScalarType>> {
        match self {
            Self::Int(_) => {
                Partial::Val(ScalarType::Int(Partial::Any)).infer(dst)
            },
        }
    }
}

/// Information about the merges in a binary operation.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinMerges {
    /// No merges need to be performed.
    Nil,

    /// Merge the left side and the destination.
    LD,

    /// Merge the right side and the destination.
    RD,

    /// Merge the left and right sides, but not the destination.
    LR,

    /// Merge the left and right sides, and the destination.
    LRD,
}

/// Information about the merges in a unaryy operation.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnaMerges {
    /// No merges need to be performed.
    Nil,

    /// Merge the source and the destination.
    SD,
}
