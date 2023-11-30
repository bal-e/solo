use core::fmt;

use thiserror::Error;

/// A type did not match its bound.
#[derive(Debug, Error)]
pub struct BoundError;

impl fmt::Display for BoundError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("A type did not match its bound")
    }
}

/// A type was unresolved.
#[derive(Debug, Error)]
pub struct UnresolvedError;

impl fmt::Display for UnresolvedError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("A type was not fully resolved")
    }
}
