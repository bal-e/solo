//! Data storage.

use core::fmt::Debug;
use core::hash::Hash;
use core::iter::Step;
use core::ops::{Range, Residual, Try};

// Utilities
pub mod ident;
pub mod share;
//pub mod slice;
pub mod vec;

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
pub trait Storage<T: ?Sized> {
    /// The type of identifiers to objects in the collection.
    type ID: Ident<Object = T>;

    /// The [`Disposition`] backing this storage.
    type Disposition: Disposition;
}

/// [`Storage`] where objects can be accessed by cloning them.
pub trait StorageGet<T>: Storage<T> {
    /// Retrieve an object given its ID.
    unsafe fn get(&self, id: Self::ID) -> T;
}

/// [`Storage`] where objects can be accessed by temporary reference.
pub trait StorageGetTmp<T: ?Sized>: Storage<T> {
    /// Use an object by reference given its ID.
    unsafe fn get_tmp<R, F>(&self, id: Self::ID, func: F) -> R
    where F: FnOnce(&T) -> R;
}

/// [`Storage`] where objects can be accessed by reference.
pub trait StorageGetRef<T: ?Sized>: Storage<T> {
    /// Retrieve an object by reference given its ID.
    unsafe fn get_ref(&self, id: Self::ID) -> &T;
}

/// [`Storage`] where objects can be inserted.
pub trait StoragePut<T>: Storage<T> {
    /// Insert an object and get its new ID.
    fn put(&mut self, object: T) -> Self::ID;
}

/// [`Storage`] where objects can be inserted by temporary reference.
pub trait StoragePutTmp<T: ?Sized>: Storage<T> {
    /// Insert an object and get its new ID.
    fn put_tmp(&mut self, object: &T) -> Self::ID;
}

/// [`Storage`] which can hold sequences of objects.
pub trait SeqStorage<T>: Storage<T> {
    /// The type of identifiers to sequences of objects.
    type SeqID: SeqIdent<Object = T, Single = Self::ID>;
}

/// [`Storage`] where sequences of objects can be cloned.
pub trait SeqStorageGet<T>: SeqStorage<T>
where Self: StorageGet<T> {
    /// The item sequence.
    type Seq<'t>: Iterator<Item = T> + 't where Self: 't, T: 't;

    /// Retrieve a series of objects given its ID.
    unsafe fn get_seq<'t>(&'t self, id: Self::SeqID) -> Self::Seq<'t>
    where T: 't;
}

/// [`Storage`] where sequences of objects can be temporarily referenced.
pub trait SeqStorageGetTmp<T>: SeqStorage<T>
where Self: StorageGetTmp<T> {
    /// Use an object by reference given its ID.
    unsafe fn get_seq_tmp<R, F>(&self, id: Self::SeqID, func: F) -> R
    where F: FnOnce(&[T]) -> R;
}

/// [`Storage`] where sequences of objects can be referenced.
pub trait SeqStorageGetRef<T>: SeqStorage<T>
where Self: StorageGetRef<T> {
    /// Retrieve a series of objects by reference given its ID.
    unsafe fn get_seq_ref(&self, id: Self::SeqID) -> &[T];
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

/// [`Storage`] where sequences of objects can be added by temporary reference.
pub trait SeqStoragePutTmp<T>: SeqStorage<T>
where Self: StoragePutTmp<T> {
    /// Insert a sequence of objects and get their IDs.
    fn put_seq_tmp(&mut self, series: &[T]) -> Self::SeqID;
}

/// The disposition with which objects should be stored.
///
/// A [`Disposition`] is a backing source for [`Storage`]s.
pub trait Disposition {
    /// Backing storage for individual objects.
    type Storage<T>
        : Storage<T, ID = IDLen<T>, Disposition = Self>;

    /// Backing storage for sequences of objects.
    type SeqStorage<T>
        : Storage<T, ID = IDLen<T>, Disposition = Self>
        + SeqStorage<T, SeqID = SeqIDLen<T>>;
}
