use core::convert::{AsRef, AsMut};

use super::*;

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

impl From<StreamType> for Partial<ScalarType> {
    fn from(value: StreamType) -> Self {
        value.scalar
    }
}

impl AsRef<Partial<StreamPart>> for StreamType {
    fn as_ref(&self) -> &Partial<StreamPart> {
        &self.stream
    }
}

impl AsRef<Partial<VectorPart>> for StreamType {
    fn as_ref(&self) -> &Partial<VectorPart> {
        &self.vector
    }
}

impl AsRef<Partial<OptionPart>> for StreamType {
    fn as_ref(&self) -> &Partial<OptionPart> {
        &self.option
    }
}

impl AsRef<Partial<ScalarType>> for StreamType {
    fn as_ref(&self) -> &Partial<ScalarType> {
        &self.scalar
    }
}

impl AsMut<Partial<StreamPart>> for StreamType {
    fn as_mut(&mut self) -> &mut Partial<StreamPart> {
        &mut self.stream
    }
}

impl AsMut<Partial<VectorPart>> for StreamType {
    fn as_mut(&mut self) -> &mut Partial<VectorPart> {
        &mut self.vector
    }
}

impl AsMut<Partial<OptionPart>> for StreamType {
    fn as_mut(&mut self) -> &mut Partial<OptionPart> {
        &mut self.option
    }
}

impl AsMut<Partial<ScalarType>> for StreamType {
    fn as_mut(&mut self) -> &mut Partial<ScalarType> {
        &mut self.scalar
    }
}

// --- VectorType --- //

impl From<VectorType> for OptionType {
    fn from(value: VectorType) -> Self {
        Self {
            option: value.option,
            scalar: value.scalar,
        }
    }
}

impl From<VectorType> for Partial<ScalarType> {
    fn from(value: VectorType) -> Self {
        value.scalar
    }
}

impl AsRef<Partial<VectorPart>> for VectorType {
    fn as_ref(&self) -> &Partial<VectorPart> {
        &self.vector
    }
}

impl AsRef<Partial<OptionPart>> for VectorType {
    fn as_ref(&self) -> &Partial<OptionPart> {
        &self.option
    }
}

impl AsRef<Partial<ScalarType>> for VectorType {
    fn as_ref(&self) -> &Partial<ScalarType> {
        &self.scalar
    }
}

impl AsMut<Partial<VectorPart>> for VectorType {
    fn as_mut(&mut self) -> &mut Partial<VectorPart> {
        &mut self.vector
    }
}

impl AsMut<Partial<OptionPart>> for VectorType {
    fn as_mut(&mut self) -> &mut Partial<OptionPart> {
        &mut self.option
    }
}

impl AsMut<Partial<ScalarType>> for VectorType {
    fn as_mut(&mut self) -> &mut Partial<ScalarType> {
        &mut self.scalar
    }
}

// --- OptionType --- //

impl From<OptionType> for Partial<ScalarType> {
    fn from(value: OptionType) -> Self {
        value.scalar
    }
}

impl AsRef<Partial<OptionPart>> for OptionType {
    fn as_ref(&self) -> &Partial<OptionPart> {
        &self.option
    }
}

impl AsRef<Partial<ScalarType>> for OptionType {
    fn as_ref(&self) -> &Partial<ScalarType> {
        &self.scalar
    }
}

impl AsMut<Partial<OptionPart>> for OptionType {
    fn as_mut(&mut self) -> &mut Partial<OptionPart> {
        &mut self.option
    }
}

impl AsMut<Partial<ScalarType>> for OptionType {
    fn as_mut(&mut self) -> &mut Partial<ScalarType> {
        &mut self.scalar
    }
}
