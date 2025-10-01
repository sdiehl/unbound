//! Locally nameless representation for capture-avoiding substitution
//! and alpha equivalence in Rust.

use std::fmt;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::sync::atomic::{AtomicUsize, Ordering};

pub use unbound_derive::{Alpha, Subst};

mod fresh;
pub use fresh::{run_fresh, Fresh, FreshM, FreshState};

mod subst;
pub use subst::{Subst, SubstName};

pub mod alpha;
pub use alpha::Alpha;

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

/// A binding construct that binds a name within a term
#[derive(Clone, Debug, PartialEq)]
pub struct Bind<P, T> {
    pattern: P,
    body: T,
}

impl<P, T> Bind<P, T> {
    /// Create a new binding
    pub fn new(pattern: P, body: T) -> Self {
        Bind { pattern, body }
    }

    /// Unbind a binding, returning the pattern and body
    /// This should be used within FreshM to get fresh names
    pub fn unbind(self) -> (P, T) {
        (self.pattern, self.body)
    }

    /// Get a reference to the pattern
    pub fn pattern(&self) -> &P {
        &self.pattern
    }

    /// Get a reference to the body
    pub fn body(&self) -> &T {
        &self.body
    }
}

impl<P: fmt::Display, T: fmt::Display> fmt::Display for Bind<P, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<{}> {}", self.pattern, self.body)
    }
}

/// Helper function to create a name from a string (like s2n in Haskell)
pub fn s2n<T>(s: impl Into<String>) -> Name<T> {
    Name::new(s)
}

/// Helper function to bind a pattern in a body
pub fn bind<P, T>(pattern: P, body: T) -> Bind<P, T> {
    Bind::new(pattern, body)
}
