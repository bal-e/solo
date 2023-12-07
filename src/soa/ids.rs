use core::cmp::Ordering;
use core::fmt;
use core::hash::{Hash, Hasher};
use core::marker::PhantomData;
use core::ops::Range;

/// The ID for an object in storage.
pub struct ID<T> {
    value: u32,
    _data: PhantomData<fn(&[T]) -> &T>,
}

impl<T> Copy for ID<T> {}

impl<T> Clone for ID<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> fmt::Debug for ID<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "@{}", self.value)
    }
}

impl<T> PartialEq for ID<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<T> Eq for ID<T> {}

impl<T> PartialOrd for ID<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.value.partial_cmp(&other.value)
    }
}

impl<T> Ord for ID<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.value.cmp(&other.value)
    }
}

impl<T> Hash for ID<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.hash(state)
    }
}

impl<T> From<u32> for ID<T> {
    fn from(value: u32) -> Self {
        Self { value, _data: PhantomData }
    }
}

impl<T> From<usize> for ID<T> {
    fn from(value: usize) -> Self {
        u32::try_from(value)
            .expect("IDs always fit within 'u32'!")
            .into()
    }
}

impl<T> From<ID<T>> for u32 {
    fn from(value: ID<T>) -> Self {
        value.value
    }
}

impl<T> From<ID<T>> for usize {
    fn from(value: ID<T>) -> Self {
        u32::from(value)
            .try_into()
            .expect("IDs always fit within 'usize'!")
    }
}

/// The IDs for a sequence of objects in storage.
pub struct SeqID<T> {
    range: (u32, u32),
    _data: PhantomData<fn(&[T]) -> &[T]>,
}

impl<T> SeqID<T> {
    /// Whether the sequence is empty or not.
    pub fn is_empty(&self) -> bool {
        self.range.0 >= self.range.1
    }

    /// Get the first ID in this sequence, if any.
    pub fn first(&self) -> Option<ID<T>> {
        if self.range.0 < self.range.1 {
            Some(ID::from(self.range.0))
        } else {
            None
        }
    }

    /// Get the last ID in this sequence, if any.
    pub fn last(&self) -> Option<ID<T>> {
        if self.range.0 < self.range.1 {
            Some(ID::from(self.range.1 - 1))
        } else {
            None
        }
    }

    /// Iterate over the IDs in this sequence.
    pub fn iter(self) -> impl Iterator<Item = ID<T>> {
        (self.range.0 .. self.range.1).map(ID::from)
    }
}

impl<T> Copy for SeqID<T> {}

impl<T> Clone for SeqID<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> fmt::Debug for SeqID<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "@{}..{}", self.range.0, self.range.1)
    }
}

impl<T> PartialEq for SeqID<T> {
    fn eq(&self, other: &Self) -> bool {
        self.range == other.range
    }
}

impl<T> Eq for SeqID<T> {}

impl<T> PartialOrd for SeqID<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.range.partial_cmp(&other.range)
    }
}

impl<T> Ord for SeqID<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.range.cmp(&other.range)
    }
}

impl<T> Hash for SeqID<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.range.hash(state)
    }
}

impl<T> From<Range<u32>> for SeqID<T> {
    fn from(range: Range<u32>) -> Self {
        Self {
            range: (range.start, range.end),
            _data: PhantomData,
        }
    }
}

impl<T> From<Range<usize>> for SeqID<T> {
    fn from(value: Range<usize>) -> Self {
        let beg = u32::try_from(value.start)
            .expect("IDs always fit within 'u32'!");
        let end = u32::try_from(value.end)
            .expect("IDs always fit within 'u32'!");
        (beg .. end).into()
    }
}

impl<T> From<SeqID<T>> for Range<u32> {
    fn from(value: SeqID<T>) -> Self {
        value.range.0 .. value.range.1
    }
}

impl<T> From<SeqID<T>> for Range<usize> {
    fn from(value: SeqID<T>) -> Self {
        let beg = value.range.0.try_into()
            .expect("IDs always fit within 'usize'!");
        let end = value.range.1.try_into()
            .expect("IDs always fit within 'usize'!");
        beg .. end
    }
}
