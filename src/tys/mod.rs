//! Types in Solo.

pub mod fix;
pub mod var;

mod err;
pub use err::*;

mod sub;
pub use sub::*;

mod cvt;
mod fmt;

/// A stream component to type.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum StreamPart {
    Some {},
    None,
}

/// A vector component to a type.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum VectorPart {
    Some { size: u32 },
    None,
}

/// An option component to a type.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum OptionPart {
    Some {},
    None,
}

/// The sign of an integer type.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IntSign {
    U,
    S,
}
