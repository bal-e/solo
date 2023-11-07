//! [`Vec`]-based object storage.

use core::iter;
use core::ops::{ControlFlow, FromResidual, Range, Residual, Try};
use core::slice;

use super::*;

/// A [`Disposition`] to use [`Vec`]s.
pub struct VecDisposition;

impl<'s> Disposition<'s> for VecDisposition {
    type Storage<T: 's> = Vec<T>;
    type SeqStorage<T: 's> = Vec<T>;
}

impl<'s, T: 's> Storage<'s, T> for Vec<T> {
    type ID = ident::IDLen<T>;
    type Disposition = VecDisposition;
}

impl<'s, T: Clone + 's> StorageGet<'s, T> for Vec<T> {
    unsafe fn get(&self, id: Self::ID) -> T {
        self.get_unchecked(usize::from(id)).clone()
    }
}

impl<'s, T: 's> StorageGetTmp<'s, T> for Vec<T> {
    unsafe fn get_tmp<R, F>(&self, id: Self::ID, func: F) -> R
    where F: FnOnce(&T) -> R {
        (func)(self.get_unchecked(usize::from(id)))
    }
}

impl<'s, T: 's> StorageGetRef<'s, T> for Vec<T> {
    unsafe fn get_ref(&self, id: Self::ID) -> &T {
        self.get_unchecked(usize::from(id))
    }
}

impl<'s, T: 's> StoragePut<'s, T> for Vec<T> {
    fn put(&mut self, object: T) -> Self::ID {
        let id = self.len();
        self.push(object);
        id.into()
    }
}

impl<'s, T: Clone + 's> StoragePutTmp<'s, T> for Vec<T> {
    fn put_tmp(&mut self, object: &T) -> Self::ID {
        let id = self.len();
        self.push(object.clone());
        id.into()
    }
}

impl<'s, T: 's> SeqStorage<'s, T> for Vec<T> {
    type SeqID = ident::SeqIDLen<T>;
}

impl<'s, T: Clone + 's> SeqStorageGet<'s, T> for Vec<T> {
    type Seq<'t>
        = iter::Cloned<slice::Iter<'t, T>>
        where Self: 't, T: 't, 's: 't;

    unsafe fn get_seq<'t>(&'t self, id: Self::SeqID) -> Self::Seq<'t>
    where 's: 't {
        self.get_unchecked(Range::<usize>::from(id)).iter().cloned()
    }
}

impl<'s, T: 's> SeqStorageGetTmp<'s, T> for Vec<T> {
    unsafe fn get_seq_tmp<R, F>(&self, id: Self::SeqID, func: F) -> R
    where F: FnOnce(&[T]) -> R {
        (func)(self.get_unchecked(Range::<usize>::from(id)))
    }
}

impl<'s, T: 's> SeqStorageGetRef<'s, T> for Vec<T> {
    unsafe fn get_seq_ref(&self, id: Self::SeqID) -> &[T] {
        self.get_unchecked(Range::<usize>::from(id))
    }
}

impl<'s, T: 's> SeqStoragePut<'s, T> for Vec<T> {
    fn put_seq<I>(&mut self, series: I) -> Self::SeqID
    where I: IntoIterator<Item = T> {
        let beg = Self::ID::from(self.len());
        <Self as iter::Extend<T>>::extend(self, series);
        let end = Self::ID::from(self.len());
        (beg .. end).into()
    }

    fn try_put_seq<E, F, I>(
        &mut self,
        series: I,
    ) -> <F as Residual<Self::SeqID>>::TryType
    where I: IntoIterator<Item = E>,
          E: Try<Output = T, Residual = F>,
          F: Residual<Self::SeqID> {
        let beg = Self::ID::from(self.len());
        for item in series {
            match item.branch() {
                ControlFlow::Continue(item) => {
                    self.push(item);
                },
                ControlFlow::Break(residual) => {
                    self.truncate(usize::from(beg));
                    return FromResidual::from_residual(residual);
                },
            }
        }
        let end = Self::ID::from(self.len());
        Try::from_output((beg .. end).into())
    }
}

impl<'s, T: Clone + 's> SeqStoragePutTmp<'s, T> for Vec<T> {
    fn put_seq_tmp(&mut self, series: &[T]) -> Self::SeqID {
        let beg = Self::ID::from(self.len());
        self.extend_from_slice(series);
        let end = Self::ID::from(self.len());
        (beg .. end).into()
    }
}
