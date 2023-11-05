use core::ops::{Residual, Try};

use super::*;

/// Forwarding [`Collection`] operations.
pub trait ForwardCollection<T: ?Sized>: AsRef<Self::Target> {
    /// The type to forward to.
    type Target: Collection<T>;
}

impl<C: ForwardCollection<T>, T: ?Sized> Collection<T> for C {
    type ID = <C::Target as Collection<T>>::ID;

    unsafe fn get(&self, id: Self::ID) -> T
    where T: Sized + Clone {
        <C as AsRef<C::Target>>::as_ref(self).get()
    }
}

impl<C: ForwardCollection<T>, T: ?Sized> CollectionRef<T> for C
where C::Target: CollectionRef<T> {
    unsafe fn get_ref(&self, id: Self::ID) -> &T {
        <C as AsRef<C::Target>>::as_ref(self).get_ref()
    }
}

impl<C: ForwardCollection<T>, T: ?Sized> CollectionInsert<T> for C
where C: AsMut<C::Target>, C::Target: CollectionInsert<T> {
    fn insert(&mut self, object: T) -> Self::ID {
        <C as AsMut<C::Target>>::as_mut(self).insert(object)
    }
}

impl<C: ForwardCollection<T>, T: ?Sized> CollectionInsertRef<T> for C
where C: AsMut<C::Target>, C::Target: CollectionInsertRef<T> {
    fn insert_ref(&mut self, object: &T) -> Self::ID {
        <C as AsMut<C::Target>>::as_mut(self).insert_ref(object)
    }
}

impl<C: ForwardCollection<T>, T: ?Sized> SeriesCollection<T> for C
where C::Target: SeriesCollection<T> {
    type SeriesID = <C::Target as SeriesCollection<T>>::SeriesID;
}

impl<C: ForwardCollection<T>, T: ?Sized> SeriesCollectionRef<T> for C
where C::Target: SeriesCollectionRef<T> {
    unsafe fn get_series_ref(&self, id: Self::SeriesID) -> &[T] {
        <C as AsRef<C::Target>>::as_ref(self).get_series_ref()
    }
}

impl<C: ForwardCollection<T>, T: ?Sized> CollectionExtend<T> for C
where C: AsMut<C::Target>, C::Target: CollectionExtend<T> {
    fn extend<I>(&mut self, series: I) -> Self::SeriesID
    where I: IntoIterator<Item = T> {
        <C as AsMut<C::Target>>::as_mut(self).extend(series)
    }

    fn try_extend<E, F, I>(
        &mut self,
        series: I,
    ) -> <F as Residual<Self::SeriesID>>::TryType
    where I: IntoIterator<Item = E>,
          E: Try<Output = T, Residual = F>,
          F: Residual<Self::SeriesID> {
        <C as AsMut<C::Target>>::as_mut(self).try_extend(series)
    }
}
