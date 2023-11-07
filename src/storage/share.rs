//! Safely sharing collections thread-locally.

use core::cell::{self, RefCell};
use core::mem::ManuallyDrop;
use core::ops::{ControlFlow, FromResidual, Residual, Try};

use crate::util::MutCell;

use super::*;

/// A [`Disposition`] for using [`Share`].
#[derive(Default)]
pub struct ShareDisposition<D> {
    inner: D,
}

impl<D> ShareDisposition<D> {
    /// Construct a new [`ShareDisposition`].
    pub fn new(disposition: D) -> Self {
        Self { inner: disposition }
    }

    /// Return the inner [`Disposition`].
    pub fn into_inner(self) -> D {
        self.inner
    }
}

impl<'s, D: Disposition<'s>> Disposition<'s> for ShareDisposition<D> {
    type Storage<T: 's> = Share<D::Storage<T>>;
    type SeqStorage<T: 's> = SeqShare<D::SeqStorage<T>, T>;
}

/// A sharing wrapper around a [`Storage`].
#[derive(Default)]
pub struct Share<S> {
    inner: RefCell<S>,
}

impl<S> Share<S> {
    /// Wrap the given [`Storage`] in a [`Share`].
    pub fn new(inner: S) -> Self {
        Self { inner: RefCell::new(inner) }
    }

    /// Destroy the [`Share`] and return the collection.
    pub fn into_inner(self) -> S {
        self.inner.into_inner()
    }
}

impl<'s, S, T> Storage<'s, T> for Share<S>
where S: Storage<'s, T>, T: ?Sized + 's {
    type ID = S::ID;
    type Disposition = ShareDisposition<S::Disposition>;
}

impl<'s, S, T> StorageGet<'s, T> for Share<S>
where S: StorageGet<'s, T>, T: 's {
    unsafe fn get(&self, id: Self::ID) -> T {
        self.inner.borrow().get(id)
    }
}

impl<'s, S, T> StorageGetTmp<'s, T> for Share<S>
where S: StorageGetTmp<'s, T>, T: ?Sized + 's {
    unsafe fn get_tmp<R, F>(&self, id: Self::ID, func: F) -> R
    where F: FnOnce(&T) -> R {
        self.inner.borrow().get_tmp(id, func)
    }
}

// impl<'s, S, T> !StorageGetRef<'s, T> for Share<S>
// where S: StorageGetRef<'s, T>, T: ?Sized + 's;

impl<'s, S, T> StoragePut<'s, T> for Share<S>
where S: StoragePut<'s, T>, T: 's {
    fn put(&mut self, object: T) -> Self::ID {
        self.inner.get_mut().put(object)
    }
}

impl<'s, S, T> StoragePut<'s, T> for &'s Share<S>
where S: StoragePut<'s, T>, T: 's {
    fn put(&mut self, object: T) -> Self::ID {
        self.inner.borrow_mut().put(object)
    }
}

impl<'s, S, T> StoragePutTmp<'s, T> for Share<S>
where S: StoragePutTmp<'s, T>, T: ?Sized + 's {
    fn put_tmp(&mut self, object: &T) -> Self::ID {
        self.inner.get_mut().put_tmp(object)
    }
}

impl<'s, S, T> StoragePutTmp<'s, T> for &'s Share<S>
where S: StoragePutTmp<'s, T>, T: ?Sized + 's {
    fn put_tmp(&mut self, object: &T) -> Self::ID {
        self.inner.borrow_mut().put_tmp(object)
    }
}

impl<'s, S, T> SeqStorage<'s, T> for Share<S>
where S: SeqStorage<'s, T>, T: 's {
    type SeqID = S::SeqID;
}

impl<'s, S, T> SeqStorageGet<'s, T> for Share<S>
where S: SeqStorageGet<'s, T>, T: 's {
    type Seq<'t> = Seq<'s, 't, S, T> where 's: 't;

    unsafe fn get_seq<'t>(&'t self, id: Self::SeqID) -> Self::Seq<'t>
    where 's: 't {
        let borrow = self.inner.borrow();
        // SAFETY: The cell has already been borrowed immutably.
        let inner = unsafe { &* self.inner.as_ptr() };
        let seq = inner.get_seq(id);
        Seq { borrow, inner: ManuallyDrop::new(seq) }
    }
}

impl<'s, S, T> SeqStorageGetTmp<'s, T> for Share<S>
where S: SeqStorageGetTmp<'s, T>, T: 's {
    unsafe fn get_seq_tmp<R, F>(&self, id: Self::SeqID, func: F) -> R
    where F: FnOnce(&[T]) -> R {
        self.inner.borrow().get_seq_tmp(id, func)
    }
}

impl<'s, S, T> SeqStoragePut<'s, T> for Share<S>
where S: SeqStoragePut<'s, T>, T: 's {
    fn put_seq<I>(&mut self, series: I) -> Self::SeqID
    where I: IntoIterator<Item = T> {
        self.inner.get_mut().put_seq(series)
    }

    fn try_put_seq<E, F, I>(
        &mut self,
        series: I,
    ) -> <F as Residual<Self::SeqID>>::TryType
    where I: IntoIterator<Item = E>,
          E: Try<Output = T, Residual = F>,
          F: Residual<Self::SeqID> {
        self.inner.get_mut().try_put_seq(series)
    }
}

// impl<'s, S, T> !SeqStoragePut<'s, T> for &'s Share<S>
// where S: SeqStoragePut<'s, T>, T: ?Sized + 's;

impl<'s, S, T> SeqStoragePutTmp<'s, T> for Share<S>
where S: SeqStoragePutTmp<'s, T>, T: 's {
    fn put_seq_tmp(&mut self, series: &[T]) -> Self::SeqID {
        self.inner.get_mut().put_seq_tmp(series)
    }
}

impl<'s, S, T> SeqStoragePutTmp<'s, T> for &'s Share<S>
where S: SeqStoragePutTmp<'s, T>, T: 's {
    fn put_seq_tmp(&mut self, series: &[T]) -> Self::SeqID {
        self.inner.borrow_mut().put_seq_tmp(series)
    }
}

/// A [`Share`] that offers sequence access.
pub struct SeqShare<S, T> {
    /// The underlying storage.
    inner: RefCell<S>,

    /// A stack of items in the process of being added.
    stack: MutCell<Vec<T>>,
}

impl<S, T> SeqShare<S, T> {
    /// Wrap the given [`Storage`] in a [`SeqShare`].
    pub fn new(inner: S) -> Self {
        Self {
            inner: RefCell::new(inner),
            stack: Default::default(),
        }
    }

    /// Destroy the [`SeqShare`] and return the storage.
    pub fn into_inner(self) -> S {
        self.inner.into_inner()
    }
}

impl<S: Default, T> Default for SeqShare<S, T> {
    fn default() -> Self {
        Self {
            inner: Default::default(),
            stack: Default::default(),
        }
    }
}

impl<'s, S, T, U> Storage<'s, U> for SeqShare<S, T>
where S: Storage<'s, U>, T: 's, U: ?Sized + 's {
    type ID = S::ID;
    type Disposition = ShareDisposition<S::Disposition>;
}

impl<'s, S, T, U> StorageGet<'s, U> for SeqShare<S, T>
where S: StorageGet<'s, U>, T: 's, U: 's {
    unsafe fn get(&self, id: Self::ID) -> U {
        self.inner.borrow().get(id)
    }
}

impl<'s, S, T, U> StorageGetTmp<'s, U> for SeqShare<S, T>
where S: StorageGetTmp<'s, U>, T: 's, U: ?Sized + 's {
    unsafe fn get_tmp<R, F>(&self, id: Self::ID, func: F) -> R
    where F: FnOnce(&U) -> R {
        self.inner.borrow().get_tmp(id, func)
    }
}

// impl<'s, S, T, U> !StorageGetRef<'s, U> for SeqShare<S, T>
// where S: StorageGetRef<'s, U>, T: 's, U: ?Sized + 's;

impl<'s, S, T, U> StoragePut<'s, U> for SeqShare<S, T>
where S: StoragePut<'s, U>, T: 's, U: 's {
    fn put(&mut self, object: U) -> Self::ID {
        self.inner.get_mut().put(object)
    }
}

impl<'s, S, T, U> StoragePut<'s, U> for &'s SeqShare<S, T>
where S: StoragePut<'s, U>, T: 's, U: 's {
    fn put(&mut self, object: U) -> Self::ID {
        self.inner.borrow_mut().put(object)
    }
}

impl<'s, S, T, U> StoragePutTmp<'s, U> for SeqShare<S, T>
where S: StoragePutTmp<'s, U>, T: 's, U: ?Sized + 's {
    fn put_tmp(&mut self, object: &U) -> Self::ID {
        self.inner.get_mut().put_tmp(object)
    }
}

impl<'s, S, T, U> StoragePutTmp<'s, U> for &'s SeqShare<S, T>
where S: StoragePutTmp<'s, U>, T: 's, U: ?Sized + 's {
    fn put_tmp(&mut self, object: &U) -> Self::ID {
        self.inner.borrow_mut().put_tmp(object)
    }
}

impl<'s, S, T, U> SeqStorage<'s, U> for SeqShare<S, T>
where S: SeqStorage<'s, U>, T: 's, U: 's {
    type SeqID = S::SeqID;
}

impl<'s, S, T, U> SeqStorageGet<'s, U> for SeqShare<S, T>
where S: SeqStorageGet<'s, U>, T: 's, U: 's {
    type Seq<'t> = Seq<'s, 't, S, U> where 's: 't;

    unsafe fn get_seq<'t>(&'t self, id: Self::SeqID) -> Self::Seq<'t>
    where 's: 't {
        let borrow = self.inner.borrow();
        // SAFETY: The cell has already been borrowed immutably.
        let inner = unsafe { &* self.inner.as_ptr() };
        let seq = inner.get_seq(id);
        Seq { borrow, inner: ManuallyDrop::new(seq) }
    }
}

impl<'s, S, T, U> SeqStorageGetTmp<'s, U> for SeqShare<S, T>
where S: SeqStorageGetTmp<'s, U>, T: 's, U: 's {
    unsafe fn get_seq_tmp<R, F>(&self, id: Self::SeqID, func: F) -> R
    where F: FnOnce(&[U]) -> R {
        self.inner.borrow().get_seq_tmp(id, func)
    }
}

impl<'s, S, T, U> SeqStoragePut<'s, U> for SeqShare<S, T>
where S: SeqStoragePut<'s, U>, T: 's, U: 's {
    fn put_seq<I>(&mut self, series: I) -> Self::SeqID
    where I: IntoIterator<Item = U> {
        self.inner.get_mut().put_seq(series)
    }

    fn try_put_seq<E, F, I>(
        &mut self,
        series: I,
    ) -> <F as Residual<Self::SeqID>>::TryType
    where I: IntoIterator<Item = E>,
          E: Try<Output = U, Residual = F>,
          F: Residual<Self::SeqID> {
        self.inner.get_mut().try_put_seq(series)
    }
}

impl<'s, S, T> SeqStoragePut<'s, T> for &'s SeqShare<S, T>
where S: SeqStoragePut<'s, T>, T: 's {
    fn put_seq<I>(&mut self, series: I) -> Self::SeqID
    where I: IntoIterator<Item = T> {
        // We load the series into the local stack first, so that the iterator
        // can use the collection however it wants.  Once that's done, we pass
        // the elements to the collection.

        let beg = self.stack.borrow_tmp(|stack| stack.len());

        for item in series {
            self.stack.borrow().push(item);
        }

        let mut stack = self.stack.borrow();
        self.inner.borrow_mut().put_seq(stack.drain(beg ..))
    }

    fn try_put_seq<E, F, I>(
        &mut self,
        series: I,
    ) -> <F as Residual<Self::SeqID>>::TryType
    where I: IntoIterator<Item = E>,
          E: Try<Output = T, Residual = F>,
          F: Residual<Self::SeqID> {
        // We load the series into the local stack first, so that the iterator
        // can use the collection however it wants.  Once that's done, we pass
        // the elements to the collection.

        let beg = self.stack.borrow_tmp(|stack| stack.len());

        for item in series {
            match item.branch() {
                ControlFlow::Continue(item) => {
                    self.stack.borrow().push(item);
                },
                ControlFlow::Break(residual) => {
                    self.stack.borrow().truncate(beg);
                    return FromResidual::from_residual(residual);
                },
            }
        }

        let mut stack = self.stack.borrow();
        let mut inner = self.inner.borrow_mut();
        Try::from_output(inner.put_seq(stack.drain(beg ..)))
    }
}

impl<'s, S, T, U> SeqStoragePutTmp<'s, U> for SeqShare<S, T>
where S: SeqStoragePutTmp<'s, U>, T: 's, U: 's {
    fn put_seq_tmp(&mut self, series: &[U]) -> Self::SeqID {
        self.inner.get_mut().put_seq_tmp(series)
    }
}

impl<'s, S, T, U> SeqStoragePutTmp<'s, U> for &'s SeqShare<S, T>
where S: SeqStoragePutTmp<'s, U>, T: 's, U: 's {
    fn put_seq_tmp(&mut self, series: &[U]) -> Self::SeqID {
        self.inner.borrow_mut().put_seq_tmp(series)
    }
}

/// A getter sequence from a [`Share`].
#[derive(Debug)]
pub struct Seq<'s: 't, 't, S: Storage<'s, T>, T: 's>
where S: SeqStorageGet<'s, T> {
    /// A borrow of the shared collection.
    borrow: cell::Ref<'t, S>,

    /// The storage's getter sequence.
    inner: ManuallyDrop<S::Seq<'t>>,
}

impl<'s: 't, 't, S: Storage<'s, T>, T: 's> Seq<'s, 't, S, T>
where S: SeqStorageGet<'s, T> {
    /// Access the underlying iterator immutably.
    pub fn get(&self) -> &S::Seq<'t> {
        &self.inner
    }

    /// Access the underlying iterator mutably.
    pub fn get_mut(&mut self) -> &mut S::Seq<'t> {
        &mut self.inner
    }
}

impl<'s: 't, 't, S: Storage<'s, T>, T: 's> Clone for Seq<'s, 't, S, T>
where S: SeqStorageGet<'s, T>, S::Seq<'t>: Clone {
    fn clone(&self) -> Self {
        Self {
            borrow: cell::Ref::clone(&self.borrow),
            inner: self.inner.clone(),
        }
    }
}

impl<'s: 't, 't, S: Storage<'s, T>, T: 's> Drop for Seq<'s, 't, S, T>
where S: SeqStorageGet<'s, T> {
    fn drop(&mut self) {
        // Drop 'inner' strictly before the borrow.
        unsafe { ManuallyDrop::drop(&mut self.inner) };
    }
}

impl<'s: 't, 't, S: Storage<'s, T>, T: 's> Iterator for Seq<'s, 't, S, T>
where S: SeqStorageGet<'s, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

// TODO: Implement more iterator methods for 'Seq'.
