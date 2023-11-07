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
pub mod ints;
pub mod syms;

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
pub trait Storage<'s, T: ?Sized + 's>: 's {
    /// The type of identifiers to objects in the collection.
    type ID: Ident<Object = T>;

    /// The [`Disposition`] backing this storage.
    type Disposition: Disposition<'s>;
}

impl<'s, S, T> Storage<'s, T> for &'s S
where S: Storage<'s, T>, T: ?Sized + 's {
    type ID = S::ID;
    type Disposition = S::Disposition;
}

impl<'s, S, T> Storage<'s, T> for &'s mut S
where S: Storage<'s, T>, T: ?Sized + 's {
    type ID = S::ID;
    type Disposition = S::Disposition;
}

/// [`Storage`] where objects can be accessed by cloning them.
pub trait StorageGet<'s, T: 's>: Storage<'s, T> {
    /// Retrieve an object given its ID.
    unsafe fn get(&self, id: Self::ID) -> T;
}

impl<'s, S, T> StorageGet<'s, T> for &'s S
where S: StorageGet<'s, T>, T: 's {
    unsafe fn get(&self, id: Self::ID) -> T {
        (**self).get(id)
    }
}

impl<'s, S, T> StorageGet<'s, T> for &'s mut S
where S: StorageGet<'s, T>, T: 's {
    unsafe fn get(&self, id: Self::ID) -> T {
        (**self).get(id)
    }
}

/// [`Storage`] where objects can be accessed by temporary reference.
pub trait StorageGetTmp<'s, T: ?Sized + 's>: Storage<'s, T> {
    /// Use an object by reference given its ID.
    unsafe fn get_tmp<R, F>(&self, id: Self::ID, func: F) -> R
    where F: FnOnce(&T) -> R;
}

impl<'s, S, T> StorageGetTmp<'s, T> for &'s S
where S: StorageGetTmp<'s, T>, T: ?Sized + 's {
    unsafe fn get_tmp<R, F>(&self, id: Self::ID, func: F) -> R
    where F: FnOnce(&T) -> R {
        (**self).get_tmp(id, func)
    }
}

impl<'s, S, T> StorageGetTmp<'s, T> for &'s mut S
where S: StorageGetTmp<'s, T>, T: ?Sized + 's {
    unsafe fn get_tmp<R, F>(&self, id: Self::ID, func: F) -> R
    where F: FnOnce(&T) -> R {
        (**self).get_tmp(id, func)
    }
}

/// [`Storage`] where objects can be accessed by reference.
pub trait StorageGetRef<'s, T: ?Sized + 's>: Storage<'s, T> {
    /// Retrieve an object by reference given its ID.
    unsafe fn get_ref(&self, id: Self::ID) -> &T;
}

impl<'s, S, T> StorageGetRef<'s, T> for &'s S
where S: StorageGetRef<'s, T>, T: ?Sized + 's {
    unsafe fn get_ref(&self, id: Self::ID) -> &T {
        (**self).get_ref(id)
    }
}

impl<'s, S, T> StorageGetRef<'s, T> for &'s mut S
where S: StorageGetRef<'s, T>, T: ?Sized + 's {
    unsafe fn get_ref(&self, id: Self::ID) -> &T {
        (**self).get_ref(id)
    }
}

/// [`Storage`] where objects can be inserted.
pub trait StoragePut<'s, T: 's>: Storage<'s, T> {
    /// Insert an object and get its new ID.
    fn put(&mut self, object: T) -> Self::ID;
}

impl<'s, S, T> StoragePut<'s, T> for &'s mut S
where S: StoragePut<'s, T>, T: 's {
    fn put(&mut self, object: T) -> Self::ID {
        (**self).put(object)
    }
}

/// [`Storage`] where objects can be inserted by temporary reference.
pub trait StoragePutTmp<'s, T: ?Sized + 's>: Storage<'s, T> {
    /// Insert an object and get its new ID.
    fn put_tmp(&mut self, object: &T) -> Self::ID;
}

impl<'s, S, T> StoragePutTmp<'s, T> for &'s mut S
where S: StoragePutTmp<'s, T>, T: 's {
    fn put_tmp(&mut self, object: &T) -> Self::ID {
        (**self).put_tmp(object)
    }
}

/// [`Storage`] which can hold sequences of objects.
pub trait SeqStorage<'s, T: 's>: Storage<'s, T> {
    /// The type of identifiers to sequences of objects.
    type SeqID: SeqIdent<Object = T, Single = Self::ID>;
}

impl<'s, S, T> SeqStorage<'s, T> for &'s S
where S: SeqStorage<'s, T>, T: 's {
    type SeqID = S::SeqID;
}

impl<'s, S, T> SeqStorage<'s, T> for &'s mut S
where S: SeqStorage<'s, T>, T: 's {
    type SeqID = S::SeqID;
}

/// [`Storage`] where sequences of objects can be cloned.
pub trait SeqStorageGet<'s, T: 's>: SeqStorage<'s, T>
where Self: StorageGet<'s, T> {
    /// The item sequence.
    type Seq<'t>: Iterator<Item = T> + 't where Self: 't, T: 't;

    /// Retrieve a series of objects given its ID.
    unsafe fn get_seq(&self, id: Self::SeqID) -> Self::Seq<'_>;
}

impl<'s, S, T> SeqStorageGet<'s, T> for &'s S
where S: SeqStorageGet<'s, T>, T: 's {
    type Seq<'t> = S::Seq<'t> where Self: 't, T: 't;

    unsafe fn get_seq(&self, id: Self::SeqID) -> Self::Seq<'_> {
        (**self).get_seq(id)
    }
}

impl<'s, S, T> SeqStorageGet<'s, T> for &'s mut S
where S: SeqStorageGet<'s, T>, T: 's {
    type Seq<'t> = S::Seq<'t> where Self: 't, T: 't;

    unsafe fn get_seq(&self, id: Self::SeqID) -> Self::Seq<'_> {
        (**self).get_seq(id)
    }
}

/// [`Storage`] where sequences of objects can be temporarily referenced.
pub trait SeqStorageGetTmp<'s, T: 's>: SeqStorage<'s, T>
where Self: StorageGetTmp<'s, T> {
    /// Use an object by reference given its ID.
    unsafe fn get_seq_tmp<R, F>(&self, id: Self::SeqID, func: F) -> R
    where F: FnOnce(&[T]) -> R;
}

impl<'s, S, T> SeqStorageGetTmp<'s, T> for &'s S
where S: SeqStorageGetTmp<'s, T>, T: 's {
    unsafe fn get_seq_tmp<R, F>(&self, id: Self::SeqID, func: F) -> R
    where F: FnOnce(&[T]) -> R {
        (**self).get_seq_tmp(id, func)
    }
}

impl<'s, S, T> SeqStorageGetTmp<'s, T> for &'s mut S
where S: SeqStorageGetTmp<'s, T>, T: 's {
    unsafe fn get_seq_tmp<R, F>(&self, id: Self::SeqID, func: F) -> R
    where F: FnOnce(&[T]) -> R {
        (**self).get_seq_tmp(id, func)
    }
}

/// [`Storage`] where sequences of objects can be referenced.
pub trait SeqStorageGetRef<'s, T: 's>: SeqStorage<'s, T>
where Self: StorageGetRef<'s, T> {
    /// Retrieve a series of objects by reference given its ID.
    unsafe fn get_seq_ref(&self, id: Self::SeqID) -> &[T];
}

impl<'s, S, T> SeqStorageGetRef<'s, T> for &'s S
where S: SeqStorageGetRef<'s, T>, T: 's {
    unsafe fn get_seq_ref(&self, id: Self::SeqID) -> &[T] {
        (**self).get_seq_ref(id)
    }
}

impl<'s, S, T> SeqStorageGetRef<'s, T> for &'s mut S
where S: SeqStorageGetRef<'s, T>, T: 's {
    unsafe fn get_seq_ref(&self, id: Self::SeqID) -> &[T] {
        (**self).get_seq_ref(id)
    }
}

/// [`Storage`] where sequences of objects can be added.
pub trait SeqStoragePut<'s, T: 's>: SeqStorage<'s, T>
where Self: StoragePut<'s, T> {
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

impl<'s, S, T> SeqStoragePut<'s, T> for &'s mut S
where S: SeqStoragePut<'s, T>, T: 's {
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
pub trait SeqStoragePutTmp<'s, T: 's>: SeqStorage<'s, T>
where Self: StoragePutTmp<'s, T> {
    /// Insert a sequence of objects and get their IDs.
    fn put_seq_tmp(&mut self, series: &[T]) -> Self::SeqID;
}

impl<'s, S, T> SeqStoragePutTmp<'s, T> for &'s mut S
where S: SeqStoragePutTmp<'s, T>, T: 's {
    fn put_seq_tmp(&mut self, series: &[T]) -> Self::SeqID {
        (**self).put_seq_tmp(series)
    }
}

/// The disposition with which objects should be stored.
///
/// A [`Disposition`] is a backing source for [`Storage`]s.
pub trait Disposition<'s>: 's {
    /// Backing storage for individual objects.
    type Storage<T: 's>
        : Storage<'s, T, ID = IDLen<T>, Disposition = Self>;

    /// Backing storage for sequences of objects.
    type SeqStorage<T: 's>
        : Storage<'s, T, ID = IDLen<T>, Disposition = Self>
        + SeqStorage<'s, T, SeqID = SeqIDLen<T>>;
}
