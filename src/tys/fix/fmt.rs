use core::fmt;

use super::*;

impl fmt::Display for StreamType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.stream, f)?;
        fmt::Display::fmt(&self.vector, f)?;
        fmt::Display::fmt(&self.option, f)?;
        fmt::Display::fmt(&self.scalar, f)
    }
}

impl fmt::Display for VectorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.vector, f)?;
        fmt::Display::fmt(&self.option, f)?;
        fmt::Display::fmt(&self.scalar, f)
    }
}

impl fmt::Display for OptionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.option, f)?;
        fmt::Display::fmt(&self.scalar, f)
    }
}

impl fmt::Display for ScalarType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(r#type) => fmt::Display::fmt(&r#type, f),
        }
    }
}

impl fmt::Display for IntType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.sign, self.bits)
    }
}
