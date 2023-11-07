//! Identifiers for objects in [`Storage`].

use core::cmp::Ordering;
use core::fmt;
use core::hash::{Hash, Hasher};
use core::iter::Step;
use core::marker::PhantomData;
use core::num::TryFromIntError;
use core::ops::Range;

use super::{Ident, SeqIdent};

/// An integer overflow error.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct OverflowError;

impl fmt::Display for OverflowError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("an illegal integer overflow occurred")
    }
}

impl From<TryFromIntError> for OverflowError {
    fn from(_: TryFromIntError) -> Self {
        Self
    }
}

macro_rules! decl_id_any {
    {
        $intt:ty ;
        $( #[ $oneas:meta ] )* pub struct $onet:ident ;
        $( #[ $seqas:meta ] )* pub struct $seqt:ident ;
    } => {
        $( #[ $oneas ] )*
        pub struct $onet<T: ?Sized> {
            index: $intt,
            _data: PhantomData<T>,
        }

        impl<T: ?Sized> Copy for $onet<T> {}

        impl<T: ?Sized> Clone for $onet<T> {
            fn clone(&self) -> Self {
                *self
            }
        }

        impl<T: ?Sized> PartialEq for $onet<T> {
            fn eq(&self, other: &Self) -> bool {
                self.index == other.index
            }
        }

        impl<T: ?Sized> Eq for $onet<T> {}

        impl<T: ?Sized> PartialOrd for $onet<T> {
            fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                self.index.partial_cmp(&other.index)
            }
        }

        impl<T: ?Sized> Ord for $onet<T> {
            fn cmp(&self, other: &Self) -> Ordering {
                self.index.cmp(&other.index)
            }
        }

        impl<T: ?Sized> Hash for $onet<T> {
            fn hash<H: Hasher>(&self, state: &mut H) {
                self.index.hash(state)
            }
        }

        impl<T: ?Sized> From<$intt> for $onet<T> {
            fn from(index: $intt) -> Self {
                Self { index, _data: PhantomData }
            }
        }

        impl<T: ?Sized> From<$onet<T>> for $intt {
            fn from(ident: $onet<T>) -> Self {
                ident.index
            }
        }

        impl<T: ?Sized> Ident for $onet<T> {
            type Object = T;
        }

        impl<T: ?Sized> fmt::Debug for $onet<T> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                <$intt as fmt::Debug>::fmt(&self.index, f)
            }
        }

        impl<T: ?Sized> fmt::Display for $onet<T> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                <$intt as fmt::Display>::fmt(&self.index, f)
            }
        }

        impl<T: ?Sized> Step for $onet<T> {
            fn steps_between(beg: &Self, end: &Self) -> Option<usize> {
                let beg = usize::try_from(*beg).ok()?;
                let end = usize::try_from(*end).ok()?;
                end.checked_sub(beg)
            }

            fn forward_checked(beg: Self, num: usize) -> Option<Self> {
                let num = <$intt>::try_from(num).ok()?;
                let index = beg.index.checked_add(num)?;
                Some(Self { index, _data: PhantomData })
            }

            fn backward_checked(beg: Self, num: usize) -> Option<Self> {
                let num = <$intt>::try_from(num).ok()?;
                let index = beg.index.checked_sub(num)?;
                Some(Self { index, _data: PhantomData })
            }
        }

        $( #[ $seqas ] )*
        pub struct $seqt<T: ?Sized> {
            range: [$intt; 2],
            _data: PhantomData<T>,
        }

        impl<T: ?Sized> Copy for $seqt<T> {}

        impl<T: ?Sized> Clone for $seqt<T> {
            fn clone(&self) -> Self {
                *self
            }
        }

        impl<T: ?Sized> PartialEq for $seqt<T> {
            fn eq(&self, other: &Self) -> bool {
                self.range == other.range
            }
        }

        impl<T: ?Sized> Eq for $seqt<T> {}

        impl<T: ?Sized> Hash for $seqt<T> {
            fn hash<H: Hasher>(&self, state: &mut H) {
                self.range.hash(state)
            }
        }

        impl<T: ?Sized> From<Range<$onet<T>>> for $seqt<T> {
            fn from(range: Range<$onet<T>>) -> Self {
                Self {
                    range: [range.start, range.end].map(<$intt>::from),
                    _data: PhantomData,
                }
            }
        }

        impl<T: ?Sized> TryFrom<($onet<T>, $intt)> for $seqt<T> {
            type Error = OverflowError;

            fn try_from(range: ($onet<T>, $intt)) -> Result<Self, Self::Error> {
                let end = range.0.index
                    .checked_add(range.1)
                    .ok_or(OverflowError)?;

                Ok(Self { range: [range.0.index, end], _data: PhantomData })
            }
        }

        impl<T: ?Sized> From<Range<$intt>> for $seqt<T> {
            fn from(range: Range<$intt>) -> Self {
                Self { range: [range.start, range.end], _data: PhantomData }
            }
        }

        impl<T: ?Sized> From<$seqt<T>> for Range<$onet<T>> {
            fn from(ident: $seqt<T>) -> Self {
                let [beg, end] = ident.range.map(<$onet<T>>::from);
                beg .. end
            }
        }

        impl<T: ?Sized> From<$seqt<T>> for ($onet<T>, $intt) {
            fn from(ident: $seqt<T>) -> Self {
                let beg = <$onet<T>>::from(ident.range[0]);
                let len = ident.range[1]
                    .checked_sub(ident.range[0])
                    .unwrap_or(0);

                (beg, len)
            }
        }

        impl<T: ?Sized> From<$seqt<T>> for Range<$intt> {
            fn from(ident: $seqt<T>) -> Self {
                ident.range[0] .. ident.range[1]
            }
        }

        impl<T: ?Sized> fmt::Debug for $seqt<T> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                let range = self.range[0] .. self.range[1];
                f.debug_struct(stringify!($seqt))
                    .field("range", &range)
                    .finish()
            }
        }

        impl<T: ?Sized> fmt::Display for $seqt<T> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{} .. {}", self.range[0], self.range[1])
            }
        }

        impl<T: ?Sized> SeqIdent for $seqt<T> {
            type Single = $onet<T>;
            type Object = T;
        }
    };
}

macro_rules! decl_id_fix {
    {
        $intt:ty ;
        $( #[ $oneas:meta ] )* pub struct $onet:ident ;
        $( #[ $seqas:meta ] )* pub struct $seqt:ident ;
    } => {
        decl_id_any! { $intt ;
            $( #[ $oneas ] )* pub struct $onet ;
            $( #[ $seqas ] )* pub struct $seqt ;
        }

        impl<T: ?Sized> TryFrom<usize> for $onet<T> {
            type Error = <$intt as TryFrom<usize>>::Error;

            fn try_from(index: usize) -> Result<Self, Self::Error> {
                Ok(Self { index: index.try_into()?, _data: PhantomData })
            }
        }

        impl<T: ?Sized> TryFrom<$onet<T>> for usize {
            type Error = <usize as TryFrom<$intt>>::Error;

            fn try_from(ident: $onet<T>) -> Result<Self, Self::Error> {
                ident.index.try_into()
            }
        }

        impl<T: ?Sized> TryFrom<($onet<T>, usize)> for $seqt<T> {
            type Error = OverflowError;

            fn try_from(range: ($onet<T>, usize)) -> Result<Self, Self::Error> {
                let len = <$intt>::try_from(range.1)?;
                let end = range.0.index.checked_add(len).ok_or(OverflowError)?;

                Ok(Self { range: [range.0.index, end], _data: PhantomData })
            }
        }

        impl<T: ?Sized> TryFrom<Range<usize>> for $seqt<T> {
            type Error = <$intt as TryFrom<usize>>::Error;

            fn try_from(range: Range<usize>) -> Result<Self, Self::Error> {
                let [beg, end] = [range.start, range.end].map(<$intt>::try_from);
                Ok(Self { range: [beg?, end?], _data: PhantomData })
            }
        }

        impl<T: ?Sized> TryFrom<$seqt<T>> for ($onet<T>, usize) {
            type Error = OverflowError;

            fn try_from(ident: $seqt<T>) -> Result<Self, Self::Error> {
                let beg = <$onet<T>>::from(ident.range[0]);
                let len = ident.range[1]
                    .checked_sub(ident.range[0])
                    .unwrap_or(0);

                Ok((beg, len.try_into()?))
            }
        }

        impl<T: ?Sized> TryFrom<$seqt<T>> for Range<usize> {
            type Error = <usize as TryFrom<$intt>>::Error;

            fn try_from(ident: $seqt<T>) -> Result<Self, Self::Error> {
                let [beg, end] = ident.range.map(usize::try_from);
                Ok(beg? .. end?)
            }
        }
    }
}

decl_id_fix! { u32;
    /// A general 32-bit [`Ident`].
    pub struct ID32;

    /// A general 32-bit [`SeqIdent`].
    pub struct SeqID32;
}

decl_id_any! { usize;
    /// A general natural-width [`Ident`].
    pub struct IDLen;

    /// A general natural-width [`SeqIdent`].
    pub struct SeqIDLen;
}
