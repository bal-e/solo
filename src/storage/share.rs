//! Safely sharing collections thread-locally.

use core::cell::{self, RefCell};
use core::mem::ManuallyDrop;
use core::ops::{ControlFlow, FromResidual, Residual, Try};

use crate::util::MutCell;

use super::*;

/// A sharing wrapper around a [`Storage`].
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

    /// Get a new [`ShareStack`] for the given element type.
    pub fn stack<T>(&self) -> ShareStack<'_, S, T>
    where S: Storage<T> {
        ShareStack {
            share: self,
            stack: MutCell::default(),
        }
    }
}

impl<S: Storage<T>, T: ?Sized> Storage<T> for Share<S> {
    type ID = S::ID;
    type Disposition = S::Disposition;
}

impl<S: StorageGet<T>, T> StorageGet<T> for Share<S> {
    unsafe fn get(&self, id: Self::ID) -> T {
        self.inner.borrow().get(id)
    }
}

impl<S: StorageGetTmp<T>, T> StorageGetTmp<T> for Share<S> {
    unsafe fn get_tmp<R, F>(&self, id: Self::ID, func: F) -> R
    where F: FnOnce(&T) -> R {
        self.inner.borrow().get_tmp(id, func)
    }
}

// impl<S: Storage<T>, T: ?Sized> !StorageGetRef<T> for Share<S>;

impl<S: StoragePut<T>, T> StoragePut<T> for Share<S> {
    fn put(&mut self, object: T) -> Self::ID {
        self.inner.get_mut().put(object)
    }
}

impl<S: StoragePut<T>, T> StoragePut<T> for &Share<S> {
    fn put(&mut self, object: T) -> Self::ID {
        self.inner.borrow_mut().put(object)
    }
}

impl<S: StoragePutTmp<T>, T: ?Sized> StoragePutTmp<T> for Share<S> {
    fn put_tmp(&mut self, object: &T) -> Self::ID {
        self.inner.get_mut().put_tmp(object)
    }
}

impl<S: StoragePutTmp<T>, T: ?Sized> StoragePutTmp<T> for &Share<S> {
    fn put_tmp(&mut self, object: &T) -> Self::ID {
        self.inner.borrow_mut().put_tmp(object)
    }
}

impl<S: SeqStorage<T>, T> SeqStorage<T> for Share<S> {
    type SeqID = S::SeqID;
}

impl<S: SeqStorageGet<T>, T> SeqStorageGet<T> for Share<S> {
    type Seq<'a> = ShareSeq<'a, S, T> where Self: 'a, T: 'a;

    unsafe fn get_seq(&self, id: Self::SeqID) -> Self::Seq<'_> {
        let borrow = self.inner.borrow();
        // SAFETY: The cell has already been borrowed immutably.
        let inner = unsafe { &* self.inner.as_ptr() };
        let seq = inner.get_seq(id);
        ShareSeq { borrow, inner: ManuallyDrop::new(seq) }
    }
}

impl<S: SeqStorageGetTmp<T>, T> SeqStorageGetTmp<T> for Share<S> {
    unsafe fn get_seq_tmp<R, F>(&self, id: Self::SeqID, func: F) -> R
    where F: FnOnce(&[T]) -> R {
        self.inner.borrow().get_seq_tmp(id, func)
    }
}

impl<S: SeqStoragePut<T>, T> SeqStoragePut<T> for Share<S> {
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

// impl<S: SeqStorage<T>, T> !SeqStoragePut<T> for &Share<S>;

impl<S: SeqStoragePutTmp<T>, T> SeqStoragePutTmp<T> for Share<S> {
    fn put_seq_tmp(&mut self, series: &[T]) -> Self::SeqID {
        self.inner.get_mut().put_seq_tmp(series)
    }
}

impl<S: SeqStoragePutTmp<T>, T> SeqStoragePutTmp<T> for &Share<S> {
    fn put_seq_tmp(&mut self, series: &[T]) -> Self::SeqID {
        self.inner.borrow_mut().put_seq_tmp(series)
    }
}

/// A stack providing concurrent access to a [`Share`].
pub struct ShareStack<'a, S: Storage<T>, T> {
    /// The underlying share.
    share: &'a Share<S>,

    /// A stack of items in the process of being added.
    stack: MutCell<Vec<T>>,
}

impl<'a, S: Storage<T>, T> ShareStack<'a, S, T> {
    /// Get the underlying [`Share`].
    pub fn share(&self) -> &'a Share<S> {
        self.share
    }
}

impl<S: Storage<T>, T, U: ?Sized> Storage<U> for ShareStack<'_, S, T>
where S: Storage<U> {
    type ID = <S as Storage<U>>::ID;
    type Disposition = <S as Storage<U>>::Disposition;
}

impl<S: Storage<T>, T, U> StorageGet<U> for ShareStack<'_, S, T>
where S: StorageGet<U> {
    unsafe fn get(&self, id: Self::ID) -> U {
        self.share.inner.borrow().get(id)
    }
}

impl<S: Storage<T>, T, U: ?Sized> StorageGetTmp<U> for ShareStack<'_, S, T>
where S: StorageGetTmp<U> {
    unsafe fn get_tmp<R, F>(&self, id: Self::ID, func: F) -> R
    where F: FnOnce(&U) -> R {
        self.share.inner.borrow().get_tmp(id, func)
    }
}

impl<S: Storage<T>, T, U> StoragePut<U> for ShareStack<'_, S, T>
where S: StoragePut<U> {
    fn put(&mut self, object: U) -> Self::ID {
        self.share.inner.borrow_mut().put(object)
    }
}

impl<S: Storage<T>, T, U: ?Sized> StoragePutTmp<U> for ShareStack<'_, S, T>
where S: StoragePutTmp<U> {
    fn put_tmp(&mut self, object: &U) -> Self::ID {
        self.share.inner.borrow_mut().put_tmp(object)
    }
}

impl<S: Storage<T>, T, U> SeqStorage<U> for ShareStack<'_, S, T>
where S: SeqStorage<U> {
    type SeqID = <S as SeqStorage<U>>::SeqID;
}

impl<S: Storage<T>, T, U> SeqStorageGet<U> for ShareStack<'_, S, T>
where S: SeqStorageGet<U> {
    type Seq<'a> = ShareSeq<'a, S, U> where Self: 'a, U: 'a;

    unsafe fn get_seq(&self, id: Self::SeqID) -> Self::Seq<'_> {
        self.share.get_seq(id)
    }
}

impl<S: Storage<T>, T, U> SeqStorageGetTmp<U> for ShareStack<'_, S, T>
where S: SeqStorageGetTmp<U> {
    unsafe fn get_seq_tmp<R, F>(&self, id: Self::SeqID, func: F) -> R
    where F: FnOnce(&[U]) -> R {
        self.share.get_seq_tmp(id, func)
    }
}

impl<S: SeqStoragePut<T>, T> SeqStoragePut<T> for ShareStack<'_, S, T> {
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
        self.share.inner.borrow_mut().put_seq(stack.drain(beg ..))
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
        let mut share = self.share.inner.borrow_mut();
        Try::from_output(share.put_seq(stack.drain(beg ..)))
    }
}

impl<S: Storage<T>, T, U> SeqStoragePutTmp<U> for ShareStack<'_, S, T>
where S: SeqStoragePutTmp<U> {
    fn put_seq_tmp(&mut self, series: &[U]) -> Self::SeqID {
        self.share.inner.borrow_mut().put_seq_tmp(series)
    }
}

/// A getter sequence from a [`Share`].
#[derive(Debug)]
pub struct ShareSeq<'a, S: Storage<T>, T: 'a>
where S: SeqStorageGet<T> {
    /// A borrow of the shared collection.
    borrow: cell::Ref<'a, S>,

    /// The storage's getter sequence.
    inner: ManuallyDrop<S::Seq<'a>>,
}

impl<'a, S: Storage<T>, T: 'a> ShareSeq<'a, S, T>
where S: SeqStorageGet<T> {
    /// Access the underlying iterator immutably.
    pub fn get(&self) -> &S::Seq<'a> {
        &self.inner
    }

    /// Access the underlying iterator mutably.
    pub fn get_mut(&mut self) -> &mut S::Seq<'a> {
        &mut self.inner
    }
}

impl<'a, S: Storage<T>, T: 'a> Clone for ShareSeq<'a, S, T>
where S: SeqStorageGet<T>, S::Seq<'a>: Clone {
    fn clone(&self) -> Self {
        Self {
            borrow: cell::Ref::clone(&self.borrow),
            inner: self.inner.clone(),
        }
    }
}

impl<'a, S: Storage<T>, T: 'a> Drop for ShareSeq<'a, S, T>
where S: SeqStorageGet<T> {
    fn drop(&mut self) {
        // Drop 'inner' strictly before the borrow.
        unsafe { ManuallyDrop::drop(&mut self.inner) };
    }
}

impl<'a, S: Storage<T>, T: 'a> Iterator for ShareSeq<'a, S, T>
where S: SeqStorageGet<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

// TODO: Implement more iterator methods for 'ShareSeq'.
