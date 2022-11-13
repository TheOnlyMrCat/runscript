use std::ops::Deref;

pub enum Ref<'a, T> {
    Shared(&'a T),
    Unique(&'a mut T),
}

impl<'a, T> Ref<'a, T>
where
    T: Clone,
{
    pub fn into_unique(this: Ref<'a, T>) -> Unique<'a, T> {
        match this {
            Ref::Shared(t) => Unique::Owned(t.clone()),
            Ref::Unique(t) => Unique::Borrowed(t),
        }
    }
}

impl<'a, T> AsRef<T> for Ref<'a, T> {
    fn as_ref(&self) -> &T {
        self
    }
}

impl<'a, T> Deref for Ref<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            Ref::Shared(t) => t,
            Ref::Unique(t) => t,
        }
    }
}

impl<'a, T> From<&'a T> for Ref<'a, T> {
    fn from(r: &'a T) -> Self {
        Self::Shared(r)
    }
}

impl<'a, T> From<&'a mut T> for Ref<'a, T> {
    fn from(r: &'a mut T) -> Self {
        Self::Unique(r)
    }
}

pub enum Unique<'a, T> {
    Borrowed(&'a mut T),
    Owned(T),
}

impl<'a, T> AsRef<T> for Unique<'a, T> {
    fn as_ref(&self) -> &T {
        self
    }
}

impl<'a, T> Deref for Unique<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            Unique::Borrowed(t) => t,
            Unique::Owned(t) => t,
        }
    }
}

impl<'a, T> std::ops::DerefMut for Unique<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            Unique::Borrowed(t) => t,
            Unique::Owned(t) => t,
        }
    }
}

impl<'a, T> From<&'a mut T> for Unique<'a, T> {
    fn from(r: &'a mut T) -> Self {
        Self::Borrowed(r)
    }
}

impl<'a, T> From<T> for Unique<'a, T> {
    fn from(r: T) -> Self {
        Self::Owned(r)
    }
}
