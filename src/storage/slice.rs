//! Slice-based object storage.

use core::iter;
use core::ops::Range;
use core::slice;

use super::*;

/// A [`Disposition`] to use slices.
pub struct SliceDisposition;

impl<'s> Disposition<'s> for SliceDisposition {
    type Storage<T: 's> = &'s [T];
    type SeqStorage<T: 's> = &'s [T];
}

impl<'s, T> Storage<'s, T> for &'s [T] {
    type ID = ident::IDLen<T>;
    type Disposition = SliceDisposition;
}

impl<'s, T: Clone + 's> StorageGet<'s, T> for &'s [T] {
    unsafe fn get(&self, id: Self::ID) -> T {
        self.get_unchecked(usize::from(id)).clone()
    }
}

impl<'s, T: 's> StorageGetTmp<'s, T> for &'s [T] {
    unsafe fn get_tmp<R, F>(&self, id: Self::ID, func: F) -> R
    where F: FnOnce(&T) -> R {
        (func)(self.get_unchecked(usize::from(id)))
    }
}

impl<'s, T: 's> StorageGetRef<'s, T> for &'s [T] {
    unsafe fn get_ref(&self, id: Self::ID) -> &T {
        self.get_unchecked(usize::from(id))
    }
}

impl<'s, T: 's> SeqStorage<'s, T> for &'s [T] {
    type SeqID = ident::SeqIDLen<T>;
}

impl<'s, T: Clone + 's> SeqStorageGet<'s, T> for &'s [T] {
    type Seq<'t> = iter::Cloned<slice::Iter<'t, T>> where 's: 't;

    unsafe fn get_seq<'t>(&'t self, id: Self::SeqID) -> Self::Seq<'t>
    where 's: 't {
        self.get_unchecked(Range::<usize>::from(id)).iter().cloned()
    }
}

impl<'s, T: 's> SeqStorageGetTmp<'s, T> for &'s [T] {
    unsafe fn get_seq_tmp<R, F>(&self, id: Self::SeqID, func: F) -> R
    where F: FnOnce(&[T]) -> R {
        (func)(self.get_unchecked(Range::<usize>::from(id)))
    }
}

impl<'s, T: 's> SeqStorageGetRef<'s, T> for &'s [T] {
    unsafe fn get_seq_ref(&self, id: Self::SeqID) -> &[T] {
        self.get_unchecked(Range::<usize>::from(id))
    }
}
