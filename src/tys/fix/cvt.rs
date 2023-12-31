use core::convert::{AsRef, AsMut};

use super::*;

// --- MappedType --- //

impl From<MappedType> for MappedPart {
    fn from(value: MappedType) -> Self {
        Self {
            stream: value.stream,
            vector: value.vector,
            option: value.option,
        }
    }
}

// --- StreamType --- //

impl From<StreamType> for VectorType {
    fn from(value: StreamType) -> Self {
        Self {
            vector: value.vector,
            option: value.option,
            scalar: value.scalar,
        }
    }
}

impl From<StreamType> for OptionType {
    fn from(value: StreamType) -> Self {
        Self {
            option: value.option,
            scalar: value.scalar,
        }
    }
}

impl From<StreamType> for ScalarType {
    fn from(value: StreamType) -> Self {
        value.scalar
    }
}

impl AsRef<StreamPart> for StreamType {
    fn as_ref(&self) -> &StreamPart {
        &self.stream
    }
}

impl AsRef<VectorPart> for StreamType {
    fn as_ref(&self) -> &VectorPart {
        &self.vector
    }
}

impl AsRef<OptionPart> for StreamType {
    fn as_ref(&self) -> &OptionPart {
        &self.option
    }
}

impl AsRef<ScalarType> for StreamType {
    fn as_ref(&self) -> &ScalarType {
        &self.scalar
    }
}

impl AsMut<StreamPart> for StreamType {
    fn as_mut(&mut self) -> &mut StreamPart {
        &mut self.stream
    }
}

impl AsMut<VectorPart> for StreamType {
    fn as_mut(&mut self) -> &mut VectorPart {
        &mut self.vector
    }
}

impl AsMut<OptionPart> for StreamType {
    fn as_mut(&mut self) -> &mut OptionPart {
        &mut self.option
    }
}

impl AsMut<ScalarType> for StreamType {
    fn as_mut(&mut self) -> &mut ScalarType {
        &mut self.scalar
    }
}

// --- VectorType --- //

impl VectorType {
    /// Construct a [`StreamType`] by adding a mapping part.
    pub fn with_part(self, part: StreamPart) -> StreamType {
        StreamType {
            stream: part,
            vector: self.vector,
            option: self.option,
            scalar: self.scalar,
        }
    }
}

impl From<VectorType> for OptionType {
    fn from(value: VectorType) -> Self {
        Self {
            option: value.option,
            scalar: value.scalar,
        }
    }
}

impl From<VectorType> for ScalarType {
    fn from(value: VectorType) -> Self {
        value.scalar
    }
}

impl AsRef<VectorPart> for VectorType {
    fn as_ref(&self) -> &VectorPart {
        &self.vector
    }
}

impl AsRef<OptionPart> for VectorType {
    fn as_ref(&self) -> &OptionPart {
        &self.option
    }
}

impl AsRef<ScalarType> for VectorType {
    fn as_ref(&self) -> &ScalarType {
        &self.scalar
    }
}

impl AsMut<VectorPart> for VectorType {
    fn as_mut(&mut self) -> &mut VectorPart {
        &mut self.vector
    }
}

impl AsMut<OptionPart> for VectorType {
    fn as_mut(&mut self) -> &mut OptionPart {
        &mut self.option
    }
}

impl AsMut<ScalarType> for VectorType {
    fn as_mut(&mut self) -> &mut ScalarType {
        &mut self.scalar
    }
}

// --- OptionType --- //

impl OptionType {
    /// Construct a [`VectorType`] by adding a mapping part.
    pub fn with_part(self, part: VectorPart) -> VectorType {
        VectorType {
            vector: part,
            option: self.option,
            scalar: self.scalar,
        }
    }
}

impl From<OptionType> for ScalarType {
    fn from(value: OptionType) -> Self {
        value.scalar
    }
}

impl AsRef<OptionPart> for OptionType {
    fn as_ref(&self) -> &OptionPart {
        &self.option
    }
}

impl AsRef<ScalarType> for OptionType {
    fn as_ref(&self) -> &ScalarType {
        &self.scalar
    }
}

impl AsMut<OptionPart> for OptionType {
    fn as_mut(&mut self) -> &mut OptionPart {
        &mut self.option
    }
}

impl AsMut<ScalarType> for OptionType {
    fn as_mut(&mut self) -> &mut ScalarType {
        &mut self.scalar
    }
}

// --- ScalarType --- //

impl ScalarType {
    /// Construct an [`OptionType`] by adding a mapping part.
    pub fn with_part(self, part: OptionPart) -> OptionType {
        OptionType {
            option: part,
            scalar: self,
        }
    }
}
