use core::fmt;

use super::*;

impl fmt::Display for MappedPart {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.stream, f)?;
        fmt::Display::fmt(&self.vector, f)?;
        fmt::Display::fmt(&self.option, f)?;
        Ok(())
    }
}

impl fmt::Display for StreamPart {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Some {} => f.write_str("[]"),
            Self::None => Ok(()),
        }
    }
}

impl fmt::Display for VectorPart {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Some { size } => write!(f, "<{}>", size),
            Self::None => Ok(()),
        }
    }
}

impl fmt::Display for OptionPart {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Some {} => f.write_str("?"),
            Self::None => Ok(()),
        }
    }
}

impl fmt::Display for IntType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.sign, self.bits)
    }
}

impl fmt::Display for IntSign {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::U => f.write_str("u"),
            Self::S => f.write_str("s"),
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
