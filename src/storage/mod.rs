//! Data storage.

use core::fmt::Debug;
use core::hash::Hash;
use core::ops::{Range, Residual, Try};

pub mod ident;
pub mod share;

pub mod ints;
pub mod syms;

mod vec;
pub use vec::VecCollector;

mod slice;

/// An identifier for an object in a collection.
pub trait Identifier: Copy + Eq + Ord + Hash + Debug {
    /// The type of objects being identified.
    type Object;
}

/// An identifier for a series of objects in a collection.
pub trait SeriesIdentifier: Copy + Eq + Ord + Hash + Debug {
    /// The type of objects being identified.
    type Object;

    /// The type of identifiers for single elements in the series.
    type Single: Identifier<Object = Self::Object>;

    /// Destructure this series into a [`Range`].
    fn into_range(self) -> Range<Self::Single>;
}

/// Objects that can be collected.
///
/// This trait indicates that values of this type can be stored in collections.
/// It is provided automatically for
///
/// Types implementing [`Object`] can be broken down into a fixed number of
/// atomic object types; [`Object::Collection`] maps a [`Collector`] onto each
/// of these atomic objects.
pub trait Object<C: Collector> {
    /// The collection type of this object.
    ///
    /// This is a type providing access to a sequence of objects of this type,
    /// using the given [`Collector`] as backing storage.
    type Collection: Collection<Self>;
}

/// An [`Object`] that cannot be subdivided.
///
/// Objects implementing this trait use [`Collector`]-provided storage.
pub trait AtomicObject: Sized {}

/// A collector providing a data source.
pub trait Collector {
    /// A collection for the given atomic object type.
    type Collection<T: AtomicObject>: Collection<T>;
}

/// A collection of objects.
pub trait Collection<T: ?Sized> {
    /// The type of identifiers to objects in the collection.
    type ID: Identifier;

    /// Retrieve an object given its ID.
    unsafe fn get(&self, id: Self::ID) -> T
    where T: Sized + Clone;
}

impl<C: Collection<T>, T: ?Sized> Collection<T> for &C {
    type ID = <C as Collection<T>>::ID;

    unsafe fn get(&self, id: Self::ID) -> T
    where T: Sized + Clone {
        <C as Collection<T>>::get(**self, id)
    }
}

impl<C: Collection<T>, T: ?Sized> Collection<T> for &mut C {
    type ID = <C as Collection<T>>::ID;

    unsafe fn get(&self, id: Self::ID) -> T
    where T: Sized + Clone {
        <C as Collection<T>>::get(**self, id)
    }
}

/// A [`Collection`] where objects can be referenced.
pub trait CollectionRef<T: ?Sized>: Collection<T> {
    /// Retrieve an object by reference given its ID.
    unsafe fn get_ref(&self, id: Self::ID) -> &T;
}

impl<C: CollectionRef<T>, T: ?Sized> CollectionRef<T> for &C {
    unsafe fn get_ref(&self, id: Self::ID) -> &T {
        <C as CollectionRef<T>>::get_ref(**self, id)
    }
}

impl<C: CollectionRef<T>, T: ?Sized> CollectionRef<T> for &mut C {
    unsafe fn get_ref(&self, id: Self::ID) -> &T {
        <C as CollectionRef<T>>::get_ref(**self, id)
    }
}

/// A [`Collection`] where individual objects can be inserted.
pub trait CollectionInsert<T: Sized>: Collection<T> {
    /// Insert an object and get its new ID.
    fn insert(&mut self, object: T) -> Self::ID;
}

impl<C: CollectionInsert<T>, T> CollectionInsert<T> for &mut C {
    fn insert(&mut self, object: T) -> Self::ID {
        <C as CollectionInsert<T>>::insert(**self, object)
    }
}

/// A [`Collection`] where individual objects can be inserted by reference.
pub trait CollectionInsertRef<T: ?Sized>: Collection<T> {
    /// Insert an object and get its new ID.
    fn insert_ref(&mut self, object: &T) -> Self::ID;
}

impl<C: CollectionInsertRef<T>, T> CollectionInsertRef<T> for &mut C {
    fn insert_ref(&mut self, object: &T) -> Self::ID {
        <C as CollectionInsertRef<T>>::insert_ref(**self, object)
    }
}

/// A [`Collection`] which can hold series of objects.
pub trait SeriesCollection<T: Sized>: Collection<T> {
    /// The type of identifiers to series of objects.
    type SeriesID: SeriesIdentifier<Single = Self::ID>;
}

impl<C: SeriesCollection<T>, T: ?Sized> SeriesCollection<T> for &C {
    type SeriesID = <C as SeriesCollection<T>>::SeriesID;
}

impl<C: SeriesCollection<T>, T: ?Sized> SeriesCollection<T> for &mut C {
    type SeriesID = <C as SeriesCollection<T>>::SeriesID;
}

/// A [`Collection`] where series of objects can be referenced.
pub trait SeriesCollectionRef<T: Sized>: SeriesCollection<T>
where Self: CollectionRef<T> {
    /// Retrieve a series of objects by reference given its ID.
    unsafe fn get_series_ref(&self, id: Self::SeriesID) -> &[T];
}

impl<C: SeriesCollectionRef<T>, T> SeriesCollectionRef<T> for &C {
    unsafe fn get_series_ref(&self, id: Self::SeriesID) -> &[T] {
        <C as SeriesCollectionRef<T>>::get_series_ref(**self, id)
    }
}

impl<C: SeriesCollectionRef<T>, T> SeriesCollectionRef<T> for &mut C {
    unsafe fn get_series_ref(&self, id: Self::SeriesID) -> &[T] {
        <C as SeriesCollectionRef<T>>::get_series_ref(**self, id)
    }
}

/// A [`Collection`] where series of objects can be added.
pub trait CollectionExtend<T: Sized>: SeriesCollection<T>
where Self: CollectionInsert<T> {
    /// Insert a series of objects and get their IDs.
    fn extend<I>(&mut self, series: I) -> Self::SeriesID
    where I: IntoIterator<Item = T>;

    /// Insert a fallible series of objects and get their IDs.
    fn try_extend<E, F, I>(
        &mut self,
        series: I,
    ) -> <F as Residual<Self::SeriesID>>::TryType
    where I: IntoIterator<Item = E>,
          E: Try<Output = T, Residual = F>,
          F: Residual<Self::SeriesID>;
}

impl<C: CollectionExtend<T>, T> CollectionExtend<T> for &mut C {
    fn extend<I>(&mut self, series: I) -> Self::SeriesID
    where I: IntoIterator<Item = T> {
        <C as CollectionExtend<T>>::extend(**self, series)
    }

    fn try_extend<E, F, I>(
        &mut self,
        series: I,
    ) -> <F as Residual<Self::SeriesID>>::TryType
    where I: IntoIterator<Item = E>,
          E: Try<Output = T, Residual = F>,
          F: Residual<Self::SeriesID> {
        <C as CollectionExtend<T>>::try_extend(**self, series)
    }
}
