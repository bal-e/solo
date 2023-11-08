//! Slice-based object storage.

use core::iter;
use core::ops::Range;
use core::slice;

use super::*;

/// A [`Disposition`] to use slices.
pub struct SliceDisposition;

impl Disposition for SliceDisposition {
    type Storage<'s, T: 's> = &'s [T];
    type SeqStorage<'s, T: 's> = &'s [T];
}

impl<T> Storage<T> for &[T] {
    type ID = ident::IDLen<T>;
    type Disposition = SliceDisposition;
}

impl<T: Clone> StorageGet<T> for &[T] {
    unsafe fn get(&self, id: Self::ID) -> T {
        self.get_unchecked(usize::from(id)).clone()
    }
}

impl<T> StorageGetTmp<T> for &[T] {
    unsafe fn get_tmp<R, F>(&self, id: Self::ID, func: F) -> R
    where F: FnOnce(&T) -> R {
        (func)(self.get_unchecked(usize::from(id)))
    }
}

impl<T> StorageGetRef<T> for &[T] {
    unsafe fn get_ref(&self, id: Self::ID) -> &T {
        self.get_unchecked(usize::from(id))
    }
}

impl<T> SeqStorage<T> for &[T] {
    type SeqID = ident::SeqIDLen<T>;
}

impl<T: Clone> SeqStorageGet<T> for &[T] {
    type Seq<'t> = iter::Cloned<slice::Iter<'t, T>> where Self: 't, T: 't;

    unsafe fn get_seq<'t>(&'t self, id: Self::SeqID) -> Self::Seq<'t>
    where T: 't {
        self.get_unchecked(Range::<usize>::from(id)).iter().cloned()
    }
}

impl<T> SeqStorageGetTmp<T> for &[T] {
    unsafe fn get_seq_tmp<R, F>(&self, id: Self::SeqID, func: F) -> R
    where F: FnOnce(&[T]) -> R {
        (func)(self.get_unchecked(Range::<usize>::from(id)))
    }
}

impl<T> SeqStorageGetRef<T> for &[T] {
    unsafe fn get_seq_ref(&self, id: Self::SeqID) -> &[T] {
        self.get_unchecked(Range::<usize>::from(id))
    }
}
