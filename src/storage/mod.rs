//! Data storage.

use core::fmt::Debug;
use core::hash::Hash;
use core::iter::Step;
use core::ops::{Range, Residual, Try};

// Utilities
pub mod ident;
pub mod share;
pub mod slice;
pub mod vec;

// Specialized objects
pub mod ints;
pub mod syms;

use self::ident::{IDLen, SeqIDLen};

/// An object that can be stored.
///
/// This trait is implemented by types which have a canonical [`Storage`] type
/// for storing collections of objects efficiently.
pub trait Object {
    /// The type of identifiers to objects.
    type ID: Ident<Object = Self>;

    /// The canonical [`Storage`] for the object.
    ///
    /// This is parametric over the [`Disposition`] used, which affects the
    /// operations available on the storage.
    type Storage<D: Disposition>
        : ?Sized
        + Storage<Self, ID = Self::ID, Disposition = D>;
}

/// An object of which sequences can be stored.
///
/// This trait is implemented by types which have a canonical [`Storage`] type
/// for storing collections of sequences of objects efficiently.
pub trait SeqObject: Object + Sized {
    /// The type of identifiers to sequences of objects.
    type SeqID: SeqIdent<Object = Self, Single = Self::ID>;

    /// The canonical [`SeqStorage`] for the object.
    ///
    /// This is parametric over the [`Disposition`] used, which affects the
    /// operations available on the storage.
    type SeqStorage<D: Disposition>
        : SeqStorage<Self, SeqID = Self::SeqID, Disposition = D>;
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
pub trait Storage<T: ?Sized> {
    /// The type of identifiers to objects in the collection.
    type ID: Ident<Object = T>;

    /// The [`Disposition`] backing this storage.
    type Disposition: Disposition;
}

impl<S: Storage<T>, T: ?Sized> Storage<T> for &S {
    type ID = S::ID;
    type Disposition = S::Disposition;
}

impl<S: Storage<T>, T: ?Sized> Storage<T> for &mut S {
    type ID = S::ID;
    type Disposition = S::Disposition;
}

/// [`Storage`] where objects can be accessed by cloning them.
pub trait StorageGet<T>: Storage<T> {
    /// Retrieve an object given its ID.
    unsafe fn get(&self, id: Self::ID) -> T;
}

impl<S: StorageGet<T>, T> StorageGet<T> for &S {
    unsafe fn get(&self, id: Self::ID) -> T {
        (**self).get(id)
    }
}

impl<S: StorageGet<T>, T> StorageGet<T> for &mut S {
    unsafe fn get(&self, id: Self::ID) -> T {
        (**self).get(id)
    }
}

/// [`Storage`] where objects can be accessed by temporary reference.
pub trait StorageGetTmp<T: ?Sized>: Storage<T> {
    /// Use an object by reference given its ID.
    unsafe fn get_tmp<R, F>(&self, id: Self::ID, func: F) -> R
    where F: FnOnce(&T) -> R;
}

impl<S: StorageGetTmp<T>, T: ?Sized> StorageGetTmp<T> for &S {
    unsafe fn get_tmp<R, F>(&self, id: Self::ID, func: F) -> R
    where F: FnOnce(&T) -> R {
        (**self).get_tmp(id, func)
    }
}

impl<S: StorageGetTmp<T>, T: ?Sized> StorageGetTmp<T> for &mut S {
    unsafe fn get_tmp<R, F>(&self, id: Self::ID, func: F) -> R
    where F: FnOnce(&T) -> R {
        (**self).get_tmp(id, func)
    }
}

/// [`Storage`] where objects can be accessed by reference.
pub trait StorageGetRef<T: ?Sized>: Storage<T> {
    /// Retrieve an object by reference given its ID.
    unsafe fn get_ref(&self, id: Self::ID) -> &T;
}

impl<S: StorageGetRef<T>, T: ?Sized> StorageGetRef<T> for &S {
    unsafe fn get_ref(&self, id: Self::ID) -> &T {
        (**self).get_ref(id)
    }
}

impl<S: StorageGetRef<T>, T: ?Sized> StorageGetRef<T> for &mut S {
    unsafe fn get_ref(&self, id: Self::ID) -> &T {
        (**self).get_ref(id)
    }
}

/// [`Storage`] where objects can be inserted.
pub trait StoragePut<T>: Storage<T> {
    /// Insert an object and get its new ID.
    fn put(&mut self, object: T) -> Self::ID;
}

impl<S: StoragePut<T>, T> StoragePut<T> for &mut S {
    fn put(&mut self, object: T) -> Self::ID {
        (**self).put(object)
    }
}

/// [`Storage`] where objects can be inserted by temporary reference.
pub trait StoragePutTmp<T: ?Sized>: Storage<T> {
    /// Insert an object and get its new ID.
    fn put_tmp(&mut self, object: &T) -> Self::ID;
}

impl<S: StoragePutTmp<T>, T: ?Sized> StoragePutTmp<T> for &mut S {
    fn put_tmp(&mut self, object: &T) -> Self::ID {
        (**self).put_tmp(object)
    }
}

/// [`Storage`] which can hold sequences of objects.
pub trait SeqStorage<T>: Storage<T> {
    /// The type of identifiers to sequences of objects.
    type SeqID: SeqIdent<Object = T, Single = Self::ID>;
}

impl<S: SeqStorage<T>, T> SeqStorage<T> for &S {
    type SeqID = S::SeqID;
}

impl<S: SeqStorage<T>, T> SeqStorage<T> for &mut S {
    type SeqID = S::SeqID;
}

/// [`Storage`] where sequences of objects can be cloned.
pub trait SeqStorageGet<T>: SeqStorage<T>
where Self: StorageGet<T> {
    /// The item sequence.
    type Seq<'a>: Iterator<Item = T> + 'a where Self: 'a, T: 'a;

    /// Retrieve a series of objects given its ID.
    unsafe fn get_seq(&self, id: Self::SeqID) -> Self::Seq<'_>;
}

impl<S: SeqStorageGet<T>, T> SeqStorageGet<T> for &S {
    type Seq<'a> = S::Seq<'a> where Self: 'a, T: 'a;

    unsafe fn get_seq(&self, id: Self::SeqID) -> Self::Seq<'_> {
        (**self).get_seq(id)
    }
}

impl<S: SeqStorageGet<T>, T> SeqStorageGet<T> for &mut S {
    type Seq<'a> = S::Seq<'a> where Self: 'a, T: 'a;

    unsafe fn get_seq(&self, id: Self::SeqID) -> Self::Seq<'_> {
        (**self).get_seq(id)
    }
}

/// [`Storage`] where sequences of objects can be temporarily referenced.
pub trait SeqStorageGetTmp<T>: SeqStorage<T>
where Self: StorageGetTmp<T> {
    /// Use an object by reference given its ID.
    unsafe fn get_seq_tmp<R, F>(&self, id: Self::SeqID, func: F) -> R
    where F: FnOnce(&[T]) -> R;
}

impl<S: SeqStorageGetTmp<T>, T> SeqStorageGetTmp<T> for &S {
    unsafe fn get_seq_tmp<R, F>(&self, id: Self::SeqID, func: F) -> R
    where F: FnOnce(&[T]) -> R {
        (**self).get_seq_tmp(id, func)
    }
}

impl<S: SeqStorageGetTmp<T>, T> SeqStorageGetTmp<T> for &mut S {
    unsafe fn get_seq_tmp<R, F>(&self, id: Self::SeqID, func: F) -> R
    where F: FnOnce(&[T]) -> R {
        (**self).get_seq_tmp(id, func)
    }
}

/// [`Storage`] where sequences of objects can be referenced.
pub trait SeqStorageGetRef<T>: SeqStorage<T>
where Self: StorageGetRef<T> {
    /// Retrieve a series of objects by reference given its ID.
    unsafe fn get_seq_ref(&self, id: Self::SeqID) -> &[T];
}

impl<S: SeqStorageGetRef<T>, T> SeqStorageGetRef<T> for &S {
    unsafe fn get_seq_ref(&self, id: Self::SeqID) -> &[T] {
        (**self).get_seq_ref(id)
    }
}

impl<S: SeqStorageGetRef<T>, T> SeqStorageGetRef<T> for &mut S {
    unsafe fn get_seq_ref(&self, id: Self::SeqID) -> &[T] {
        (**self).get_seq_ref(id)
    }
}

/// [`Storage`] where sequences of objects can be added.
pub trait SeqStoragePut<T>: SeqStorage<T>
where Self: StoragePut<T> {
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

impl<S: SeqStoragePut<T>, T> SeqStoragePut<T> for &mut S {
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
pub trait SeqStoragePutTmp<T>: SeqStorage<T>
where Self: StoragePutTmp<T> {
    /// Insert a sequence of objects and get their IDs.
    fn put_seq_tmp(&mut self, series: &[T]) -> Self::SeqID;
}

impl<S: SeqStoragePutTmp<T>, T> SeqStoragePutTmp<T> for &mut S {
    fn put_seq_tmp(&mut self, series: &[T]) -> Self::SeqID {
        (**self).put_seq_tmp(series)
    }
}

/// The disposition with which objects should be stored.
///
/// A [`Disposition`] is a backing source for [`Storage`]s.
pub trait Disposition {
    /// Backing storage for individual objects.
    type Storage<T>
        : ?Sized
        + Storage<T, ID = IDLen<T>, Disposition = Self>;

    /// Backing storage for sequences of objects.
    type SeqStorage<T>
        : ?Sized
        + Storage<T, ID = IDLen<T>, Disposition = Self>
        + SeqStorage<T, SeqID = SeqIDLen<T>>;
}
