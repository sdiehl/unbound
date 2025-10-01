//! Binding construct for representing name binding in terms

use std::fmt;

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
