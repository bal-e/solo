use core::fmt;

use super::*;

impl fmt::Display for MappedType {
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

impl<T: fmt::Display> fmt::Display for Partial<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Min => f.write_str("min"),
            Self::Max => f.write_str("max"),
            Self::Any => f.write_str("any"),
            Self::Val(obj) => fmt::Display::fmt(&obj, f),
        }
    }
}
