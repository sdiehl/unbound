//! Name type for representing variables with globally unique identifiers

use std::fmt;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::sync::atomic::{AtomicUsize, Ordering};

/// A name with a phantom type parameter
#[derive(Clone, Debug)]
pub struct Name<T> {
    string: String,
    index: usize,
    _phantom: PhantomData<T>,
}

impl<T> Name<T> {
    /// Create a new name from a string
    pub fn new(s: impl Into<String>) -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        Name {
            string: s.into(),
            index: COUNTER.fetch_add(1, Ordering::SeqCst),
            _phantom: PhantomData,
        }
    }

    /// Create a name from string with explicit index (for testing)
    pub fn with_index(s: impl Into<String>, index: usize) -> Self {
        Name {
            string: s.into(),
            index,
            _phantom: PhantomData,
        }
    }

    /// Get the string part of the name
    pub fn string(&self) -> &str {
        &self.string
    }

    /// Get the index part of the name
    pub fn index(&self) -> usize {
        self.index
    }
}

impl<T> fmt::Display for Name<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}@{}", self.string, self.index)
    }
}

impl<T> PartialEq for Name<T> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl<T> Eq for Name<T> {}

impl<T> Hash for Name<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.index.hash(state);
    }
}
