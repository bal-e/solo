//! The Medium Intermediary Representation (MIR).
//!
//! The MIR breaks down function bodies into loops, which operate on streams,
//! and single operations, which operate outside loops.  Loops cannot be nested
//! as streams cannot be nested.  Single operations are only executed once when
//! a function is executed, unlike the operations within loops.  MIR provides a
//! means to describe these loops and optimize code around them.

use num_bigint::BigInt;

use crate::ops::*;
use crate::soa::{ID, SeqID};
use crate::tys::fix::*;

mod soa;
pub use soa::*;

/// A typed streaming instruction.
#[derive(Clone, Debug)]
pub struct TypedStreamInst {
    pub inst: StreamInst,
    pub dstt: MappedType,
}

/// A streaming instruction.
#[derive(Clone, Debug)]
pub enum StreamInst {
    /// A regular streaming binary operation.
    Bin(StreamBinOp, [ID<Self>; 2]),

    /// A regular streaming unary operation.
    Una(StreamUnaOp, [ID<Self>; 1]),

    /// Cast a value without changing its 'shape'.
    MapCast(ID<Self>),

    /// Cast a value by changing its 'shape'.
    BitCast(ID<Self>),

    /// Stream a singular value.
    Map(ID<SingleInst>),
}

/// A typed singular instruction.
#[derive(Clone, Debug)]
pub struct TypedSingleInst {
    pub inst: SingleInst,
    pub dstt: MappedType,
}

/// A singular instruction.
#[derive(Clone, Debug)]
pub enum SingleInst {
    /// A regular singular binary operation.
    Bin(SingleBinOp, [ID<Self>; 2]),

    /// A regular singular unary operation.
    Una(SingleUnaOp, [ID<Self>; 1]),

    /// Cast a value without changing its 'shape'.
    MapCast(ID<Self>),

    /// Cast a value by changing its 'shape'.
    BitCast(ID<Self>),

    /// Collect a stream into a singular value.
    Col(SingleColOp, ID<StreamInst>),

    /// A function argument.
    Arg(u32),

    /// An integer literal.
    Int(BigInt),
}

/// A loop processing some streams.
#[derive(Clone, Debug)]
pub struct Loop {
    /// The streaming instructions in this loop.
    pub streams: SeqID<StreamInst>,

    /// The singular instructions following this loop.
    pub singles: SeqID<SingleInst>,
}
