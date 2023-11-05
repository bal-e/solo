//! [`Vec`]-based object storage.

use core::iter;
use core::ops::{ControlFlow, FromResidual, Range, Residual, Try};

use super::*;

impl<T> Collection<T> for Vec<T> {
    type ID = ident::ID32<T>;

    fn get(&self, id: Self::ID) -> T
    where T: Sized + Copy {
        self.get(usize::from(id)).unwrap().clone()
    }
}

impl<T> CollectionRef<T> for Vec<T> {
    fn get_ref(&self, id: Self::ID) -> &T {
        self.get(usize::from(id)).unwrap()
    }
}

impl<T> CollectionInsert<T> for Vec<T> {
    fn insert(&mut self, object: T) -> Self::ID {
        let id = Self::ID::new(self.len());
        self.push(object);
        id
    }
}

impl<T> SeriesCollection<T> for Vec<T> {
    type SeriesID = ident::SeriesID32<T>;
}

impl<T> SeriesCollectionRef<T> for Vec<T> {
    fn get_series_ref(&self, id: Self::SeriesID) -> &[T] {
        let Range { start: beg, end } = id.into();
        let [beg, end] = [beg, end].map(usize::from);
        &self[beg .. end]
    }
}

impl<T> CollectionExtend<T> for Vec<T> {
    fn extend<I>(&mut self, series: I) -> Self::SeriesID
    where I: IntoIterator<Item = T> {
        let beg = Self::ID::new(self.len());
        <Self as iter::Extend<T>>::extend(self, series);
        let end = Self::ID::new(self.len());
        (beg .. end).into()
    }

    fn try_extend<E, F, I>(
        &mut self,
        series: I,
    ) -> <F as Residual<Self::SeriesID>>::TryType
    where I: IntoIterator<Item = E>,
          E: Try<Output = T, Residual = F>,
          F: Residual<Self::SeriesID> {
        let beg = Self::ID::new(self.len());
        for item in series {
            match item.branch() {
                ControlFlow::Continue(item) => {
                    self.push(item);
                },
                ControlFlow::Break(residual) => {
                    self.truncate(usize::from(beg));
                    return FromResidual::from_residual(residual);
                },
            }
        }
        let end = Self::ID::new(self.len());
        Try::from_output((beg .. end).into())
    }
}

/// A [`Collector`] that wraps items in [`Vec`].
pub struct VecCollector;

impl Collector for VecCollector {
    type Collection<T: AtomicObject> = Vec<T>;
}
