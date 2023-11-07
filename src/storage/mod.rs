//! Data storage.

use core::fmt::Debug;
use core::hash::Hash;
use core::iter::Step;
use core::ops::{Range, Residual, Try};

// Utilities
pub mod ident;
//pub mod share;
//pub mod slice;
//pub mod vec;

// Specialized objects
//pub mod ints;
//pub mod syms;

use self::ident::{IDLen, SeqIDLen};

/// The ID of an object.
pub type ID<T> = <T as Object>::ID;

/// The ID of a sequence of objects.
pub type SeqID<T> = <T as SeqObject>::SeqID;

/// An object that can be stored.
///
/// This trait is implemented by types which have a canonical [`Storage`] type
/// for storing collections of objects efficiently.
pub trait Object {
    /// The type of identifiers to objects.
    type ID: Ident<Object = Self>;
}

/// An object of which sequences can be stored.
///
/// This trait is implemented by types which have a canonical [`Storage`] type
/// for storing collections of sequences of objects efficiently.
pub trait SeqObject: Object + Sized {
    /// The type of identifiers to sequences of objects.
    type SeqID: SeqIdent<Object = Self, Single = Self::ID>;
}

/// An identifier for an object in a collection.
pub trait Ident: Copy + Eq + Hash + Debug {
    /// The type of objects being identified.
    type Object: ?Sized;
}

/// An identifier for a sequence of objects in a collection.
pub trait SeqIdent: Copy + Eq + Hash + Debug
where Self: From<Range<Self::Single>> + Into<Range<Self::Single>> {
    /// The type of objects being identified.
    type Object: ?Sized;

    /// The type of identifiers for single elements in the series.
    type Single: Ident<Object = Self::Object> + Step;
}

/// Storage for objects.
///
/// This trait is implemented by types which store objects of the specified
/// type (among others, possibly).  It can be implemented on aggregates which
/// have multiple fields implementing [`Storage`], or it can be implemented on
/// "leaf" types which provide specialized storage for a certain set of objects.
pub trait Storage<'a, T: ?Sized + 'a>: 'a {
    /// The type of identifiers to objects in the collection.
    type ID: Ident<Object = T>;

    /// The [`Disposition`] backing this storage.
    type Disposition: Disposition<'a>;
}

impl<'a, S, T> Storage<'a, T> for &'a S
where S: Storage<'a, T>, T: ?Sized + 'a {
    type ID = S::ID;
    type Disposition = S::Disposition;
}

impl<'a, S, T> Storage<'a, T> for &'a mut S
where S: Storage<'a, T>, T: ?Sized + 'a {
    type ID = S::ID;
    type Disposition = S::Disposition;
}

/// [`Storage`] where objects can be accessed by cloning them.
pub trait StorageGet<'a, T: 'a>: Storage<'a, T> {
    /// Retrieve an object given its ID.
    unsafe fn get(&self, id: Self::ID) -> T;
}

impl<'a, S, T> StorageGet<'a, T> for &'a S
where S: StorageGet<'a, T>, T: 'a {
    unsafe fn get(&self, id: Self::ID) -> T {
        (**self).get(id)
    }
}

impl<'a, S, T> StorageGet<'a, T> for &'a mut S
where S: StorageGet<'a, T>, T: 'a {
    unsafe fn get(&self, id: Self::ID) -> T {
        (**self).get(id)
    }
}

/// [`Storage`] where objects can be accessed by temporary reference.
pub trait StorageGetTmp<'a, T: ?Sized + 'a>: Storage<'a, T> {
    /// Use an object by reference given its ID.
    unsafe fn get_tmp<R, F>(&self, id: Self::ID, func: F) -> R
    where F: FnOnce(&T) -> R;
}

impl<'a, S, T> StorageGetTmp<'a, T> for &'a S
where S: StorageGetTmp<'a, T>, T: ?Sized + 'a {
    unsafe fn get_tmp<R, F>(&self, id: Self::ID, func: F) -> R
    where F: FnOnce(&T) -> R {
        (**self).get_tmp(id, func)
    }
}

impl<'a, S, T> StorageGetTmp<'a, T> for &'a mut S
where S: StorageGetTmp<'a, T>, T: ?Sized + 'a {
    unsafe fn get_tmp<R, F>(&self, id: Self::ID, func: F) -> R
    where F: FnOnce(&T) -> R {
        (**self).get_tmp(id, func)
    }
}

/// [`Storage`] where objects can be accessed by reference.
pub trait StorageGetRef<'a, T: ?Sized + 'a>: Storage<'a, T> {
    /// Retrieve an object by reference given its ID.
    unsafe fn get_ref(&self, id: Self::ID) -> &T;
}

impl<'a, S, T> StorageGetRef<'a, T> for &'a S
where S: StorageGetRef<'a, T>, T: ?Sized + 'a {
    unsafe fn get_ref(&self, id: Self::ID) -> &T {
        (**self).get_ref(id)
    }
}

impl<'a, S, T> StorageGetRef<'a, T> for &'a mut S
where S: StorageGetRef<'a, T>, T: ?Sized + 'a {
    unsafe fn get_ref(&self, id: Self::ID) -> &T {
        (**self).get_ref(id)
    }
}

/// [`Storage`] where objects can be inserted.
pub trait StoragePut<'a, T: 'a>: Storage<'a, T> {
    /// Insert an object and get its new ID.
    fn put(&mut self, object: T) -> Self::ID;
}

impl<'a, S, T> StoragePut<'a, T> for &'a mut S
where S: StoragePut<'a, T>, T: 'a {
    fn put(&mut self, object: T) -> Self::ID {
        (**self).put(object)
    }
}

/// [`Storage`] where objects can be inserted by temporary reference.
pub trait StoragePutTmp<'a, T: ?Sized + 'a>: Storage<'a, T> {
    /// Insert an object and get its new ID.
    fn put_tmp(&mut self, object: &T) -> Self::ID;
}

impl<'a, S, T> StoragePutTmp<'a, T> for &'a mut S
where S: StoragePutTmp<'a, T>, T: 'a {
    fn put_tmp(&mut self, object: &T) -> Self::ID {
        (**self).put_tmp(object)
    }
}

/// [`Storage`] which can hold sequences of objects.
pub trait SeqStorage<'a, T: 'a>: Storage<'a, T> {
    /// The type of identifiers to sequences of objects.
    type SeqID: SeqIdent<Object = T, Single = Self::ID>;
}

impl<'a, S, T> SeqStorage<'a, T> for &'a S
where S: SeqStorage<'a, T>, T: 'a {
    type SeqID = S::SeqID;
}

impl<'a, S, T> SeqStorage<'a, T> for &'a mut S
where S: SeqStorage<'a, T>, T: 'a {
    type SeqID = S::SeqID;
}

/// [`Storage`] where sequences of objects can be cloned.
pub trait SeqStorageGet<'a, T: 'a>: SeqStorage<'a, T>
where Self: StorageGet<'a, T> {
    /// The item sequence.
    type Seq<'t>: Iterator<Item = T> + 't where Self: 't, T: 't;

    /// Retrieve a series of objects given its ID.
    unsafe fn get_seq(&self, id: Self::SeqID) -> Self::Seq<'_>;
}

impl<'a, S, T> SeqStorageGet<'a, T> for &'a S
where S: SeqStorageGet<'a, T>, T: 'a {
    type Seq<'t> = S::Seq<'t> where Self: 't, T: 't;

    unsafe fn get_seq(&self, id: Self::SeqID) -> Self::Seq<'_> {
        (**self).get_seq(id)
    }
}

impl<'a, S, T> SeqStorageGet<'a, T> for &'a mut S
where S: SeqStorageGet<'a, T>, T: 'a {
    type Seq<'t> = S::Seq<'t> where Self: 't, T: 't;

    unsafe fn get_seq(&self, id: Self::SeqID) -> Self::Seq<'_> {
        (**self).get_seq(id)
    }
}

/// [`Storage`] where sequences of objects can be temporarily referenced.
pub trait SeqStorageGetTmp<'a, T: 'a>: SeqStorage<'a, T>
where Self: StorageGetTmp<'a, T> {
    /// Use an object by reference given its ID.
    unsafe fn get_seq_tmp<R, F>(&self, id: Self::SeqID, func: F) -> R
    where F: FnOnce(&[T]) -> R;
}

impl<'a, S, T> SeqStorageGetTmp<'a, T> for &'a S
where S: SeqStorageGetTmp<'a, T>, T: 'a {
    unsafe fn get_seq_tmp<R, F>(&self, id: Self::SeqID, func: F) -> R
    where F: FnOnce(&[T]) -> R {
        (**self).get_seq_tmp(id, func)
    }
}

impl<'a, S, T> SeqStorageGetTmp<'a, T> for &'a mut S
where S: SeqStorageGetTmp<'a, T>, T: 'a {
    unsafe fn get_seq_tmp<R, F>(&self, id: Self::SeqID, func: F) -> R
    where F: FnOnce(&[T]) -> R {
        (**self).get_seq_tmp(id, func)
    }
}

/// [`Storage`] where sequences of objects can be referenced.
pub trait SeqStorageGetRef<'a, T: 'a>: SeqStorage<'a, T>
where Self: StorageGetRef<'a, T> {
    /// Retrieve a series of objects by reference given its ID.
    unsafe fn get_seq_ref(&self, id: Self::SeqID) -> &[T];
}

impl<'a, S, T> SeqStorageGetRef<'a, T> for &'a S
where S: SeqStorageGetRef<'a, T>, T: 'a {
    unsafe fn get_seq_ref(&self, id: Self::SeqID) -> &[T] {
        (**self).get_seq_ref(id)
    }
}

impl<'a, S, T> SeqStorageGetRef<'a, T> for &'a mut S
where S: SeqStorageGetRef<'a, T>, T: 'a {
    unsafe fn get_seq_ref(&self, id: Self::SeqID) -> &[T] {
        (**self).get_seq_ref(id)
    }
}

/// [`Storage`] where sequences of objects can be added.
pub trait SeqStoragePut<'a, T: 'a>: SeqStorage<'a, T>
where Self: StoragePut<'a, T> {
    /// Insert a sequence of objects and get their IDs.
    fn put_seq<I>(&mut self, series: I) -> Self::SeqID
    where I: IntoIterator<Item = T>;

    /// Insert a fallible sequences of objects and get their IDs.
    fn try_put_seq<E, F, I>(
        &mut self,
        series: I,
    ) -> <F as Residual<Self::SeqID>>::TryType
    where I: IntoIterator<Item = E>,
          E: Try<Output = T, Residual = F>,
          F: Residual<Self::SeqID>;
}

impl<'a, S, T> SeqStoragePut<'a, T> for &'a mut S
where S: SeqStoragePut<'a, T>, T: 'a {
    fn put_seq<I>(&mut self, series: I) -> Self::SeqID
    where I: IntoIterator<Item = T> {
        (**self).put_seq(series)
    }

    fn try_put_seq<E, F, I>(
        &mut self,
        series: I,
    ) -> <F as Residual<Self::SeqID>>::TryType
    where I: IntoIterator<Item = E>,
          E: Try<Output = T, Residual = F>,
          F: Residual<Self::SeqID> {
        (**self).try_put_seq(series)
    }
}

/// [`Storage`] where sequences of objects can be added by temporary reference.
pub trait SeqStoragePutTmp<'a, T: 'a>: SeqStorage<'a, T>
where Self: StoragePutTmp<'a, T> {
    /// Insert a sequence of objects and get their IDs.
    fn put_seq_tmp(&mut self, series: &[T]) -> Self::SeqID;
}

impl<'a, S, T> SeqStoragePutTmp<'a, T> for &'a mut S
where S: SeqStoragePutTmp<'a, T>, T: 'a {
    fn put_seq_tmp(&mut self, series: &[T]) -> Self::SeqID {
        (**self).put_seq_tmp(series)
    }
}

/// The disposition with which objects should be stored.
///
/// A [`Disposition`] is a backing source for [`Storage`]s.
pub trait Disposition<'a> {
    /// Backing storage for individual objects.
    type Storage<T: 'a>
        : Storage<'a, T, ID = IDLen<T>, Disposition = Self>;

    /// Backing storage for sequences of objects.
    type SeqStorage<T: 'a>
        : Storage<'a, T, ID = IDLen<T>, Disposition = Self>
        + SeqStorage<'a, T, SeqID = SeqIDLen<T>>;
}
