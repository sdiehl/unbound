//! Shorthands for building names and bindings.

use crate::alpha::{Alpha, Pattern};
use crate::{Bind, Name};

/// Create a fresh name with the given spelling.
pub fn s2n<T>(s: impl Into<String>) -> Name<T> {
    Name::new(s)
}

/// Bind a pattern's names in a body, closing the body over them.
pub fn bind<P: Pattern, T: Alpha>(pattern: P, body: T) -> Bind<P, T> {
    Bind::new(pattern, body)
}
