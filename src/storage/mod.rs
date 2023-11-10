//! Data storage.

use std::marker::PhantomData;
use std::ops::{Deref, Residual, Try};

mod object;
pub use object::ObjectStorage;

/// The ID of an object in storage.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ID<T> {
    /// The underlying ID number.
    inner: u32,

    _elem: PhantomData<*const T>,
}

impl<T> From<ID<T>> for u32 {
    fn from(value: ID<T>) -> Self {
        value.inner
    }
}

impl<T> From<ID<T>> for usize {
    fn from(value: ID<T>) -> Self {
        value.inner as usize
    }
}

/// An object in storage.
pub struct Stored<T> {
    /// The ID of the object.
    idnum: u32,

    /// The underlying object.
    inner: T,
}

impl<T> Stored<T> {
    /// Get the ID of this object.
    pub fn id(&self) -> ID<T> {
        ID { inner: self.idnum, _elem: PhantomData }
    }
}

impl<T> Deref for Stored<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

/// The ability to store objects.
pub trait Store<T: ?Sized> {
    type Stored<'a> where Self: 'a;

    /// Store an object.
    fn store(&self, object: T) -> Self::Stored<'_>;
}

/// The ability to store objects from references.
pub trait StoreRef<T: ?Sized> {
    type Stored<'a> where Self: 'a;

    /// Store an object from a reference.
    fn store_ref(&self, object: &T) -> Self::Stored<'_>;
}

/// The ability to store series of objects.
pub trait StoreMany<T: ?Sized> {
    type Stored<'a> where Self: 'a;

    /// Store a series of objects.
    fn store_many<I>(&self, objects: I) -> Self::Stored<'_>
    where I: IntoIterator<Item = T>;
}

/// The ability to store fallible series of objects.
pub trait StoreTryMany<T: ?Sized> {
    type Stored<'a> where Self: 'a;

    /// Store a fallible series of objects.
    fn store_try_many<'a, E, F, I>(&'a self, objects: I)
        -> <F as Residual<Self::Stored<'_>>>::TryType
    where I: IntoIterator<Item = E>,
          E: Try<Output = T, Residual = F>,
          F: Residual<Self::Stored<'a>>;
}
