//! Helper functions for working with names and bindings

use crate::{Bind, Name};

/// Helper function to create a name from a string (like s2n in Haskell)
pub fn s2n<T>(s: impl Into<String>) -> Name<T> {
    Name::new(s)
}

/// Helper function to bind a pattern in a body
pub fn bind<P, T>(pattern: P, body: T) -> Bind<P, T> {
    Bind::new(pattern, body)
}
