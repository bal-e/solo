//! The Medium Intermediary Representation (MIR).
//!
//! The MIR breaks down function bodies into loops, which operate on streams,
//! and single operations, which operate outside loops.  Loops cannot be nested
//! as streams cannot be nested.  Single operations are only executed once when
//! a function is executed, unlike the operations within loops.  MIR provides a
//! means to describe these loops and optimize code around them.

pub mod ops;
use ops::*;
