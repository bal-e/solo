use core::fmt;

use super::*;

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

impl fmt::Display for IntSign {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::U => f.write_str("u"),
            Self::S => f.write_str("s"),
        }
    }
}
