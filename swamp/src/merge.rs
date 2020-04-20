#[derive(Debug)]
pub enum MergeError {
    Immutable,
    Incompatible,
}

pub trait Merge: Sized {
    /// soft_merge never overwrites data (used for read-only field)
    fn soft_merge(self, other: Self) -> Result<Self, MergeError>;
    /// hard_merge only fails if an inner soft_merge fails
    fn hard_merge(self, other: Self) -> Result<Self, MergeError>;
}

macro_rules! impl_merge_for_primitive {
    ($p:ident) => {
        impl Merge for $p {
            fn soft_merge(self, other: Self) -> Result<Self, MergeError> {
                if self == Self::default() {
                    Ok(other)
                } else {
                    Err(MergeError::Immutable)
                }
            }
            fn hard_merge(self, other: Self) -> Result<Self, MergeError> {
                Ok(other)
            }
        }
    };
}

impl_merge_for_primitive!(i32);
impl_merge_for_primitive!(u32);
impl_merge_for_primitive!(String);

impl<T: Merge> Merge for Option<T> {
    fn soft_merge(self, other: Self) -> Result<Self, MergeError> {
        if self.is_none() {
            Ok(other)
        } else {
            Err(MergeError::Immutable)
        }
    }
    fn hard_merge(self, other: Self) -> Result<Self, MergeError> {
        if let Some(other) = other {
            if let Some(thing) = self {
                let merged = thing.hard_merge(other)?;
                Ok(Some(merged))
            } else {
                Ok(Some(other))
            }
        } else {
            Ok(self)
        }
    }
}

impl<T> Merge for Vec<T> {
    fn soft_merge(self, other: Self) -> Result<Self, MergeError> {
        if self.len() == 0 {
            Ok(other)
        } else {
            Err(MergeError::Immutable)
        }
    }
    fn hard_merge(mut self, other: Self) -> Result<Self, MergeError> {
        self.extend(other.into_iter());
        Ok(self)
    }
}
