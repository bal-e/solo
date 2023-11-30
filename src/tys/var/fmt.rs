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
            Self::Min => f.write_str("min"),
            Self::Max => f.write_str("max"),
            Self::Any => f.write_str("any"),
            Self::Int(r#type) => fmt::Display::fmt(&r#type, f),
        }
    }
}

impl fmt::Display for IntType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Min => f.write_str("imin"),
            Self::Max => f.write_str("imax"),
            Self::Any => f.write_str("iany"),
            Self::Val { sign, bits } => write!(f, "{}{}", sign, bits),
        }
    }
}
