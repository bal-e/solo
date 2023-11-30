use super::*;

// --- Fully to partially resolved types --- //

impl From<fix::StreamType> for var::StreamType {
    fn from(value: fix::StreamType) -> Self {
        Self {
            stream: value.stream,
            vector: value.vector,
            option: value.option,
            scalar: value.scalar.into(),
        }
    }
}

impl From<fix::VectorType> for var::VectorType {
    fn from(value: fix::VectorType) -> Self {
        Self {
            vector: value.vector,
            option: value.option,
            scalar: value.scalar.into(),
        }
    }
}

impl From<fix::OptionType> for var::OptionType {
    fn from(value: fix::OptionType) -> Self {
        Self {
            option: value.option,
            scalar: value.scalar.into(),
        }
    }
}

impl From<fix::ScalarType> for var::ScalarType {
    fn from(value: fix::ScalarType) -> Self {
        match value {
            fix::ScalarType::Int(r#type) => Self::Int(r#type.into()),
        }
    }
}

impl From<fix::IntType> for var::IntType {
    fn from(value: fix::IntType) -> Self {
        Self::Val {
            sign: value.sign,
            bits: value.bits,
        }
    }
}

// --- Fully to partially resolved types --- //

impl TryFrom<var::StreamType> for fix::StreamType {
    type Error = UnresolvedError;

    fn try_from(value: var::StreamType) -> Result<Self, Self::Error> {
        Ok(Self {
            stream: value.stream,
            vector: value.vector,
            option: value.option,
            scalar: value.scalar.try_into()?,
        })
    }
}

impl TryFrom<var::VectorType> for fix::VectorType {
    type Error = UnresolvedError;

    fn try_from(value: var::VectorType) -> Result<Self, Self::Error> {
        Ok(Self {
            vector: value.vector,
            option: value.option,
            scalar: value.scalar.try_into()?,
        })
    }
}

impl TryFrom<var::OptionType> for fix::OptionType {
    type Error = UnresolvedError;

    fn try_from(value: var::OptionType) -> Result<Self, Self::Error> {
        Ok(Self {
            option: value.option,
            scalar: value.scalar.try_into()?,
        })
    }
}

impl TryFrom<var::ScalarType> for fix::ScalarType {
    type Error = UnresolvedError;

    fn try_from(value: var::ScalarType) -> Result<Self, Self::Error> {
        Ok(match value {
            var::ScalarType::Min => return Err(UnresolvedError),
            var::ScalarType::Max => return Err(UnresolvedError),
            var::ScalarType::Any => return Err(UnresolvedError),
            var::ScalarType::Int(r#type) => Self::Int(r#type.try_into()?),
        })
    }
}

impl TryFrom<var::IntType> for fix::IntType {
    type Error = UnresolvedError;

    fn try_from(value: var::IntType) -> Result<Self, Self::Error> {
        Ok(match value {
            var::IntType::Min => return Err(UnresolvedError),
            var::IntType::Max => return Err(UnresolvedError),
            var::IntType::Any => return Err(UnresolvedError),
            var::IntType::Val { sign, bits } => Self { sign, bits },
        })
    }
}
