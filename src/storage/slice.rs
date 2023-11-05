//! Slice-based object storage.

use core::ops::Range;

use super::*;

impl<T> Collection<T> for [T] {
    type ID = ident::ID32<T>;

    fn get(&self, id: Self::ID) -> T
    where T: Sized + Clone {
        self.get(usize::from(id)).unwrap().clone()
    }
}

impl<T> CollectionRef<T> for [T] {
    fn get_ref(&self, id: Self::ID) -> &T {
        self.get(usize::from(id)).unwrap()
    }
}

impl<T> SeriesCollection<T> for [T] {
    type SeriesID = ident::SeriesID32<T>;
}

impl<T> SeriesCollectionRef<T> for [T] {
    fn get_series_ref(&self, id: Self::SeriesID) -> &[T] {
        let Range { start: beg, end } = id.into();
        let [beg, end] = [beg, end].map(usize::from);
        &self[beg .. end]
    }
}
