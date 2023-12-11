use core::fmt;

use super::*;

impl fmt::Display for TypedStreamInst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.dstt, self.inst)
    }
}

impl fmt::Display for StreamInst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bin(bop, [lhs, rhs]) =>
                write!(f, "{} {:?}, {:?}", bop, lhs, rhs),
            Self::Una(uop, [src]) =>
                write!(f, "{} {:?}", uop, src),
            Self::BitCast(cop, src) =>
                write!(f, "bitcast {}, {:?}", cop, src),
            Self::MapCast(cop, src) =>
                write!(f, "mapcast {}, {:?}", cop, src),
            Self::Map(src) =>
                write!(f, "map {:?}", src),
        }
    }
}

impl fmt::Display for TypedSingleInst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.dstt, self.inst)
    }
}

impl fmt::Display for SingleInst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bin(bop, [lhs, rhs]) =>
                write!(f, "{} {:?}, {:?}", bop, lhs, rhs),
            Self::Una(uop, [src]) =>
                write!(f, "{} {:?}", uop, src),
            Self::BitCast(cop, src) =>
                write!(f, "bitcast {}, {:?}", cop, src),
            Self::MapCast(cop, src) =>
                write!(f, "mapcast {}, {:?}", cop, src),
            Self::Col(cop, src) =>
                write!(f, "col {}, {:?}", cop, src),
            Self::Arg(num) =>
                write!(f, "arg {}", num),
            Self::Int(val) =>
                write!(f, "int {}", val),
        }
    }
}

impl fmt::Display for Loop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(streams {:?}, singles {:?})", self.streams, self.singles)
    }
}
