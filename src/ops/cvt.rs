use super::*;

// --- BinOp --- //

impl From<SingleBinOp> for StreamBinOp {
    fn from(value: SingleBinOp) -> Self {
        match value {
            SingleBinOp::Cat => Self::Cat,
            SingleBinOp::Ind => Self::Ind,
            SingleBinOp::Cond => Self::Cond,
            SingleBinOp::Else => Self::Else,
            SingleBinOp::Int(o) => Self::Int(o),
            SingleBinOp::Cmp(o) => Self::Cmp(o),
        }
    }
}

impl TryFrom<StreamBinOp> for SingleBinOp {
    type Error = ();

    fn try_from(value: StreamBinOp) -> Result<Self, Self::Error> {
        Ok(match value {
            StreamBinOp::Exp | StreamBinOp::Red => return Err(()),
            StreamBinOp::Cat => Self::Cat,
            StreamBinOp::Ind => Self::Ind,
            StreamBinOp::Cond => Self::Cond,
            StreamBinOp::Else => Self::Else,
            StreamBinOp::Int(o) => Self::Int(o),
            StreamBinOp::Cmp(o) => Self::Cmp(o),
        })
    }
}

// --- UnaOp --- //

impl From<SingleUnaOp> for StreamUnaOp {
    fn from(value: SingleUnaOp) -> Self {
        match value {
            SingleUnaOp::Int(o) => Self::Int(o),
        }
    }
}

impl TryFrom<StreamUnaOp> for SingleUnaOp {
    type Error = ();

    fn try_from(value: StreamUnaOp) -> Result<Self, Self::Error> {
        Ok(match value {
            StreamUnaOp::Int(o) => Self::Int(o),
        })
    }
}

// --- CastOp --- //

impl From<SingleCastOp> for StreamCastOp {
    fn from(value: SingleCastOp) -> Self {
        match value {
            SingleCastOp::Scalar => Self::Scalar,
            SingleCastOp::Option => Self::Option,
            SingleCastOp::Vector => Self::Vector,
        }
    }
}

impl TryFrom<StreamCastOp> for SingleCastOp {
    type Error = ();

    fn try_from(value: StreamCastOp) -> Result<Self, Self::Error> {
        Ok(match value {
            StreamCastOp::Scalar => Self::Scalar,
            StreamCastOp::Option => Self::Option,
            StreamCastOp::Vector => Self::Vector,
            StreamCastOp::Stream => return Err(()),
        })
    }
}
