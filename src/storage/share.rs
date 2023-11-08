//! Safely sharing collections thread-locally.

use core::cell::RefCell;
use core::ops::{ControlFlow, FromResidual, Residual, Try};

use crate::util::MutCell;

use super::*;

/// A [`Disposition`] for using [`Share`].
#[derive(Default)]
pub struct ShareDisposition<D: Disposition> {
    inner: D,
}

impl<D: Disposition> ShareDisposition<D> {
    /// Construct a new [`ShareDisposition`].
    pub fn new(disposition: D) -> Self {
        Self { inner: disposition }
    }

    /// Return the inner [`Disposition`].
    pub fn into_inner(self) -> D {
        self.inner
    }
}

impl<D: Disposition> Disposition for ShareDisposition<D> {
    type Storage<T> = Share<D::Storage<T>>;
    type SeqStorage<T> = SeqShare<D::SeqStorage<T>, T>;
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

impl<S, T> Storage<T> for Share<S>
where S: Storage<T>, T: ?Sized {
    type ID = S::ID;
    type Disposition = ShareDisposition<S::Disposition>;
}

impl<S, T> Storage<T> for &Share<S>
where S: Storage<T>, T: ?Sized {
    type ID = S::ID;
    type Disposition = ShareDisposition<S::Disposition>;
}

impl<S, T> Storage<T> for &mut Share<S>
where S: Storage<T>, T: ?Sized {
    type ID = S::ID;
    type Disposition = ShareDisposition<S::Disposition>;
}

impl<S, T> StorageGet<T> for Share<S>
where S: StorageGet<T> {
    unsafe fn get(&self, id: Self::ID) -> T {
        self.inner.borrow().get(id)
    }
}

impl<S, T> StorageGet<T> for &Share<S>
where S: StorageGet<T> {
    unsafe fn get(&self, id: Self::ID) -> T {
        self.inner.borrow().get(id)
    }
}

impl<S, T> StorageGet<T> for &mut Share<S>
where S: StorageGet<T> {
    unsafe fn get(&self, id: Self::ID) -> T {
        self.inner.borrow().get(id)
    }
}

impl<S, T> StorageGetTmp<T> for Share<S>
where S: StorageGetTmp<T>, T: ?Sized {
    unsafe fn get_tmp<R, F>(&self, id: Self::ID, func: F) -> R
    where F: FnOnce(&T) -> R {
        self.inner.borrow().get_tmp(id, func)
    }
}

impl<S, T> StorageGetTmp<T> for &Share<S>
where S: StorageGetTmp<T>, T: ?Sized {
    unsafe fn get_tmp<R, F>(&self, id: Self::ID, func: F) -> R
    where F: FnOnce(&T) -> R {
        self.inner.borrow().get_tmp(id, func)
    }
}

impl<S, T> StorageGetTmp<T> for &mut Share<S>
where S: StorageGetTmp<T>, T: ?Sized {
    unsafe fn get_tmp<R, F>(&self, id: Self::ID, func: F) -> R
    where F: FnOnce(&T) -> R {
        self.inner.borrow().get_tmp(id, func)
    }
}

// impl<S, T> !StorageGetRef<T> for Share<S>
// where S: StorageGetRef<T>, T: ?Sized;

impl<S, T> StoragePut<T> for Share<S>
where S: StoragePut<T> {
    fn put(&mut self, object: T) -> Self::ID {
        self.inner.get_mut().put(object)
    }
}

impl<S, T> StoragePut<T> for &Share<S>
where S: StoragePut<T> {
    fn put(&mut self, object: T) -> Self::ID {
        self.inner.borrow_mut().put(object)
    }
}

impl<S, T> StoragePut<T> for &mut Share<S>
where S: StoragePut<T> {
    fn put(&mut self, object: T) -> Self::ID {
        self.inner.get_mut().put(object)
    }
}

impl<S, T> StoragePutTmp<T> for Share<S>
where S: StoragePutTmp<T>, T: ?Sized {
    fn put_tmp(&mut self, object: &T) -> Self::ID {
        self.inner.get_mut().put_tmp(object)
    }
}

impl<S, T> StoragePutTmp<T> for &Share<S>
where S: StoragePutTmp<T>, T: ?Sized {
    fn put_tmp(&mut self, object: &T) -> Self::ID {
        self.inner.borrow_mut().put_tmp(object)
    }
}

impl<S, T> StoragePutTmp<T> for &mut Share<S>
where S: StoragePutTmp<T>, T: ?Sized {
    fn put_tmp(&mut self, object: &T) -> Self::ID {
        self.inner.get_mut().put_tmp(object)
    }
}

impl<S, T> SeqStorage<T> for Share<S>
where S: SeqStorage<T> {
    type SeqID = S::SeqID;
}

impl<S, T> SeqStorage<T> for &Share<S>
where S: SeqStorage<T> {
    type SeqID = S::SeqID;
}

impl<S, T> SeqStorage<T> for &mut Share<S>
where S: SeqStorage<T> {
    type SeqID = S::SeqID;
}

impl<S, T> SeqStorageGetTmp<T> for Share<S>
where S: SeqStorageGetTmp<T> {
    unsafe fn get_seq_tmp<R, F>(&self, id: Self::SeqID, func: F) -> R
    where F: FnOnce(&[T]) -> R {
        self.inner.borrow().get_seq_tmp(id, func)
    }
}

impl<S, T> SeqStorageGetTmp<T> for &Share<S>
where S: SeqStorageGetTmp<T> {
    unsafe fn get_seq_tmp<R, F>(&self, id: Self::SeqID, func: F) -> R
    where F: FnOnce(&[T]) -> R {
        self.inner.borrow().get_seq_tmp(id, func)
    }
}

impl<S, T> SeqStorageGetTmp<T> for &mut Share<S>
where S: SeqStorageGetTmp<T> {
    unsafe fn get_seq_tmp<R, F>(&self, id: Self::SeqID, func: F) -> R
    where F: FnOnce(&[T]) -> R {
        self.inner.borrow().get_seq_tmp(id, func)
    }
}

impl<S, T> SeqStoragePut<T> for Share<S>
where S: SeqStoragePut<T> {
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

// impl<S, T> !SeqStoragePut<T> for &'s Share<S>
// where S: SeqStoragePut<T>, T: ?Sized;

impl<S, T> SeqStoragePut<T> for &mut Share<S>
where S: SeqStoragePut<T> {
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

impl<S, T> SeqStoragePutTmp<T> for Share<S>
where S: SeqStoragePutTmp<T> {
    fn put_seq_tmp(&mut self, series: &[T]) -> Self::SeqID {
        self.inner.get_mut().put_seq_tmp(series)
    }
}

impl<S, T> SeqStoragePutTmp<T> for &Share<S>
where S: SeqStoragePutTmp<T> {
    fn put_seq_tmp(&mut self, series: &[T]) -> Self::SeqID {
        self.inner.borrow_mut().put_seq_tmp(series)
    }
}

impl<S, T> SeqStoragePutTmp<T> for &mut Share<S>
where S: SeqStoragePutTmp<T> {
    fn put_seq_tmp(&mut self, series: &[T]) -> Self::SeqID {
        self.inner.get_mut().put_seq_tmp(series)
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

impl<S, T, U> Storage<U> for SeqShare<S, T>
where S: Storage<U>, U: ?Sized {
    type ID = S::ID;
    type Disposition = ShareDisposition<S::Disposition>;
}

impl<S, T, U> Storage<U> for &SeqShare<S, T>
where S: Storage<U>, U: ?Sized {
    type ID = S::ID;
    type Disposition = ShareDisposition<S::Disposition>;
}

impl<S, T, U> Storage<U> for &mut SeqShare<S, T>
where S: Storage<U>, U: ?Sized {
    type ID = S::ID;
    type Disposition = ShareDisposition<S::Disposition>;
}

impl<S, T, U> StorageGet<U> for SeqShare<S, T>
where S: StorageGet<U> {
    unsafe fn get(&self, id: Self::ID) -> U {
        self.inner.borrow().get(id)
    }
}

impl<S, T, U> StorageGet<U> for &SeqShare<S, T>
where S: StorageGet<U> {
    unsafe fn get(&self, id: Self::ID) -> U {
        self.inner.borrow().get(id)
    }
}

impl<S, T, U> StorageGet<U> for &mut SeqShare<S, T>
where S: StorageGet<U> {
    unsafe fn get(&self, id: Self::ID) -> U {
        self.inner.borrow().get(id)
    }
}

impl<S, T, U> StorageGetTmp<U> for SeqShare<S, T>
where S: StorageGetTmp<U>, U: ?Sized {
    unsafe fn get_tmp<R, F>(&self, id: Self::ID, func: F) -> R
    where F: FnOnce(&U) -> R {
        self.inner.borrow().get_tmp(id, func)
    }
}

impl<S, T, U> StorageGetTmp<U> for &SeqShare<S, T>
where S: StorageGetTmp<U>, U: ?Sized {
    unsafe fn get_tmp<R, F>(&self, id: Self::ID, func: F) -> R
    where F: FnOnce(&U) -> R {
        self.inner.borrow().get_tmp(id, func)
    }
}

impl<S, T, U> StorageGetTmp<U> for &mut SeqShare<S, T>
where S: StorageGetTmp<U>, U: ?Sized {
    unsafe fn get_tmp<R, F>(&self, id: Self::ID, func: F) -> R
    where F: FnOnce(&U) -> R {
        self.inner.borrow().get_tmp(id, func)
    }
}

// impl<S, T, U> !StorageGetRef<U> for SeqShare<S, T>
// where S: StorageGetRef<U>, U: ?Sized;

impl<S, T, U> StoragePut<U> for SeqShare<S, T>
where S: StoragePut<U> {
    fn put(&mut self, object: U) -> Self::ID {
        self.inner.get_mut().put(object)
    }
}

impl<S, T, U> StoragePut<U> for &SeqShare<S, T>
where S: StoragePut<U> {
    fn put(&mut self, object: U) -> Self::ID {
        self.inner.borrow_mut().put(object)
    }
}

impl<S, T, U> StoragePut<U> for &mut SeqShare<S, T>
where S: StoragePut<U> {
    fn put(&mut self, object: U) -> Self::ID {
        self.inner.get_mut().put(object)
    }
}

impl<S, T, U> StoragePutTmp<U> for SeqShare<S, T>
where S: StoragePutTmp<U>, U: ?Sized {
    fn put_tmp(&mut self, object: &U) -> Self::ID {
        self.inner.get_mut().put_tmp(object)
    }
}

impl<S, T, U> StoragePutTmp<U> for &SeqShare<S, T>
where S: StoragePutTmp<U>, U: ?Sized {
    fn put_tmp(&mut self, object: &U) -> Self::ID {
        self.inner.borrow_mut().put_tmp(object)
    }
}

impl<S, T, U> StoragePutTmp<U> for &mut SeqShare<S, T>
where S: StoragePutTmp<U>, U: ?Sized {
    fn put_tmp(&mut self, object: &U) -> Self::ID {
        self.inner.get_mut().put_tmp(object)
    }
}

impl<S, T, U> SeqStorage<U> for SeqShare<S, T>
where S: SeqStorage<U> {
    type SeqID = S::SeqID;
}

impl<S, T, U> SeqStorage<U> for &SeqShare<S, T>
where S: SeqStorage<U> {
    type SeqID = S::SeqID;
}

impl<S, T, U> SeqStorage<U> for &mut SeqShare<S, T>
where S: SeqStorage<U> {
    type SeqID = S::SeqID;
}

impl<S, T, U> SeqStorageGetTmp<U> for SeqShare<S, T>
where S: SeqStorageGetTmp<U> {
    unsafe fn get_seq_tmp<R, F>(&self, id: Self::SeqID, func: F) -> R
    where F: FnOnce(&[U]) -> R {
        self.inner.borrow().get_seq_tmp(id, func)
    }
}

impl<S, T, U> SeqStorageGetTmp<U> for &SeqShare<S, T>
where S: SeqStorageGetTmp<U> {
    unsafe fn get_seq_tmp<R, F>(&self, id: Self::SeqID, func: F) -> R
    where F: FnOnce(&[U]) -> R {
        self.inner.borrow().get_seq_tmp(id, func)
    }
}

impl<S, T, U> SeqStorageGetTmp<U> for &mut SeqShare<S, T>
where S: SeqStorageGetTmp<U> {
    unsafe fn get_seq_tmp<R, F>(&self, id: Self::SeqID, func: F) -> R
    where F: FnOnce(&[U]) -> R {
        self.inner.borrow().get_seq_tmp(id, func)
    }
}

impl<S, T, U> SeqStoragePut<U> for SeqShare<S, T>
where S: SeqStoragePut<U> {
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

impl<S, T> SeqStoragePut<T> for &SeqShare<S, T>
where S: SeqStoragePut<T> {
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

impl<S, T, U> SeqStoragePut<U> for &mut SeqShare<S, T>
where S: SeqStoragePut<U> {
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

impl<S, T, U> SeqStoragePutTmp<U> for SeqShare<S, T>
where S: SeqStoragePutTmp<U> {
    fn put_seq_tmp(&mut self, series: &[U]) -> Self::SeqID {
        self.inner.get_mut().put_seq_tmp(series)
    }
}

impl<S, T, U> SeqStoragePutTmp<U> for &SeqShare<S, T>
where S: SeqStoragePutTmp<U>, {
    fn put_seq_tmp(&mut self, series: &[U]) -> Self::SeqID {
        self.inner.borrow_mut().put_seq_tmp(series)
    }
}

impl<S, T, U> SeqStoragePutTmp<U> for &mut SeqShare<S, T>
where S: SeqStoragePutTmp<U> {
    fn put_seq_tmp(&mut self, series: &[U]) -> Self::SeqID {
        self.inner.get_mut().put_seq_tmp(series)
    }
}
