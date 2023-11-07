use core::cell::{Cell, UnsafeCell};
use core::ops::{Deref, DerefMut};

/// A thread-local mutex.
pub struct MutCell<T: ?Sized> {
    inuse: Cell<bool>,
    inner: UnsafeCell<T>,
}

impl<T> MutCell<T> {
    /// Construct a new [`MutCell`].
    pub fn new(inner: T) -> Self {
        Self {
            inuse: Cell::new(false),
            inner: UnsafeCell::new(inner),
        }
    }

    /// Retrieve the value inside a [`MutCell`].
    pub fn into_inner(self) -> T {
        self.inner.into_inner()
    }
}

impl<T: ?Sized> MutCell<T> {
    /// Borrow a [`MutCell`].
    pub fn borrow(&self) -> MutBorrow<'_, T> {
        if self.inuse.replace(true) {
            panic!("already borrowed");
        }

        MutBorrow { inner: self }
    }

    /// Borrow a [`MutCell`] temporarily.
    pub fn borrow_tmp<R, F>(&self, f: F) -> R
    where F: FnOnce(&mut T) -> R {
        if self.inuse.replace(true) {
            panic!("already borrowed");
        }

        // SAFETY: 'inuse' was just set, so nobody else has access.
        let inner = unsafe { &mut * self.inner.get() };
        let result = (f)(inner);

        self.inuse.set(false);
        result
    }

    /// Directly access the data underlying the [`MutCell`].
    pub fn get_mut(&mut self) -> &mut T {
        self.inner.get_mut()
    }
}

impl<T: Default> Default for MutCell<T> {
    fn default() -> Self {
        Self {
            inuse: Cell::new(false),
            inner: UnsafeCell::new(T::default()),
        }
    }
}

/// A borrow of a [`MutCell`].
pub struct MutBorrow<'a, T: ?Sized> {
    /// The underlying cell.
    inner: &'a MutCell<T>,
}

impl<'a, T: ?Sized> Drop for MutBorrow<'a, T> {
    fn drop(&mut self) {
        self.inner.inuse.set(false);
    }
}

impl<'a, T: ?Sized> Deref for MutBorrow<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        // SAFETY: At most one 'MutBorrow' exists.
        unsafe { &* self.inner.inner.get() }
    }
}

impl<'a, T: ?Sized> DerefMut for MutBorrow<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // SAFETY: At most one 'MutBorrow' exists.
        unsafe { &mut * self.inner.inner.get() }
    }
}
