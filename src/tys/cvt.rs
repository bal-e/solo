use super::*;

// --- Fully to partially resolved types --- //

impl From<fix::StreamType> for var::StreamType {
    fn from(value: fix::StreamType) -> Self {
        Self {
            stream: value.stream.into(),
            vector: value.vector.into(),
            option: value.option.into(),
            scalar: var::ScalarType::from(value.scalar).into(),
        }
    }
}

impl From<fix::VectorType> for var::VectorType {
    fn from(value: fix::VectorType) -> Self {
        Self {
            vector: value.vector.into(),
            option: value.option.into(),
            scalar: var::ScalarType::from(value.scalar).into(),
        }
    }
}

impl From<fix::OptionType> for var::OptionType {
    fn from(value: fix::OptionType) -> Self {
        Self {
            option: value.option.into(),
            scalar: var::ScalarType::from(value.scalar).into(),
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

// --- Fully to partially resolved types --- //

impl TryFrom<var::StreamType> for fix::StreamType {
    type Error = UnresolvedError;

    fn try_from(value: var::StreamType) -> Result<Self, Self::Error> {
        Ok(Self {
            stream: value.stream.val()?,
            vector: value.vector.val()?,
            option: value.option.val()?,
            scalar: value.scalar.val()?.try_into()?,
        })
    }
}

impl TryFrom<var::VectorType> for fix::VectorType {
    type Error = UnresolvedError;

    fn try_from(value: var::VectorType) -> Result<Self, Self::Error> {
        Ok(Self {
            vector: value.vector.val()?,
            option: value.option.val()?,
            scalar: value.scalar.val()?.try_into()?,
        })
    }
}

impl TryFrom<var::OptionType> for fix::OptionType {
    type Error = UnresolvedError;

    fn try_from(value: var::OptionType) -> Result<Self, Self::Error> {
        Ok(Self {
            option: value.option.val()?,
            scalar: value.scalar.val()?.try_into()?,
        })
    }
}

impl TryFrom<var::ScalarType> for fix::ScalarType {
    type Error = UnresolvedError;

    fn try_from(value: var::ScalarType) -> Result<Self, Self::Error> {
        Ok(match value {
            var::ScalarType::Int(r#type) => Self::Int(r#type.val()?),
        })
    }
}
