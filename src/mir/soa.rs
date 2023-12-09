use core::convert::{AsRef, AsMut};

use crate::soa;

use super::*;

macro_rules! def_storage {
    ($($field:ident : $type:ident { $storage:ident + $storage_mut:ident },)*) => {
        /// Immutable MIR storage.
        #[derive(Clone, Debug)]
        pub struct Storage {
            $(pub $field : $storage),*
        }

        $(impl AsRef<$storage> for Storage {
            fn as_ref(&self) -> &$storage {
                &self.$field
            }
        })*

        /// Mutable MIR storage.
        #[derive(Clone, Debug, Default)]
        pub struct StorageMut {
            $(pub $field : $storage_mut),*
        }

        $(
            impl AsRef<$storage_mut> for StorageMut {
                fn as_ref(&self) -> &$storage_mut {
                    &self.$field
                }
            }

            impl AsMut<$storage_mut> for StorageMut {
                fn as_mut(&mut self) -> &mut $storage_mut {
                    &mut self.$field
                }
            }
        )*

        impl From<StorageMut> for Storage {
            fn from(value: StorageMut) -> Self {
                Self {
                    $($field: value.$field.into(),)*
                }
            }
        }
    };
}

def_storage!(
    loops: Loop { Loops + LoopsMut },
    streams: StreamInst { Streams + StreamsMut },
    singles: SingleInst { Singles + SinglesMut },
);

/// Immutable storage for [`Loop`]s.
pub type Loops = soa::Storage<Loop>;

/// Mutable storage for [`Loop`]s.
pub type LoopsMut = soa::StorageMut<Loop>;

/// Immutable storage for [`StreamInst`]s.
pub type Streams = soa::Storage<StreamInst>;

/// Mutable storage for [`StreamInst`]s.
pub type StreamsMut = soa::StorageMut<StreamInst>;

/// Immutable storage for [`SingleInst`]s.
pub type Singles = soa::Storage<SingleInst>;

/// Mutable storage for [`SingleInst`]s.
pub type SinglesMut = soa::StorageMut<SingleInst>;
