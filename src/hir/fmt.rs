use core::fmt;

use super::*;

impl fmt::Display for TypedNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.node, self.dstt)
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Bin(bop, _) => fmt::Display::fmt(&bop, f),
            Self::Una(uop, _) => fmt::Display::fmt(&uop, f),
            Self::BitCast(cop, _) => write!(f, "bitcast:{}", cop),
            Self::MapCast(cop, _) => write!(f, "mapcast:{}", cop),
            Self::Arg(num) => write!(f, "arg:{}", num),
            Self::Int(val) => write!(f, "int:{}", val),
            Self::Vec => f.write_str("vec"),
        }
    }
}
