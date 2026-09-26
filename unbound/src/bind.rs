//! The binding construct.

use std::fmt;

use crate::alpha::{Alpha, Pattern};
use crate::subst::Subst;
use crate::Name;

/// A pattern binding its names in a body.
///
/// The body is stored *closed*: on construction every free occurrence of a
/// name the pattern binds is replaced by a de Bruijn coordinate. That is
/// what makes alpha equivalence structural and substitution incapable of
/// capture, and it is why the body should be reached through [`unbind`]
/// rather than [`body`].
///
/// [`unbind`]: Bind::unbind
/// [`body`]: Bind::body
#[derive(Clone, Debug)]
pub struct Bind<P, T> {
    pattern: P,
    body: T,
}

impl<P: Pattern, T: Alpha> Bind<P, T> {
    /// Bind the pattern's names in `body`, closing the body over them.
    pub fn new(pattern: P, mut body: T) -> Self {
        body.close(0, &pattern.binders());
        Bind { pattern, body }
    }

    /// Open the binding, giving the pattern and body fresh binder names.
    ///
    /// Each call produces names distinct from every other name in the
    /// program, so repeatedly unbinding the same term never aliases.
    pub fn unbind(self) -> (P, T) {
        let (pattern, names) = self.pattern.freshen();
        let mut body = self.body;
        body.open(0, &names);
        (pattern, body)
    }

    /// Open the binding without consuming it.
    pub fn unbind_ref(&self) -> (P, T)
    where
        P: Clone,
        T: Clone, {
        self.clone().unbind()
    }

    /// The body with `value` in place of the pattern's single binder.
    ///
    /// # Panics
    ///
    /// If the pattern does not bind exactly one name.
    pub fn instantiate<V>(&self, value: &V) -> T
    where
        P: Clone,
        T: Clone + Subst<V>, {
        self.instantiate_all(std::slice::from_ref(value))
    }

    /// The body with `values` in place of the pattern's binders, in binding
    /// order.
    ///
    /// # Panics
    ///
    /// If the number of values differs from the number of binders.
    pub fn instantiate_all<V>(&self, values: &[V]) -> T
    where
        P: Clone,
        T: Clone + Subst<V>, {
        let (pattern, body) = self.unbind_ref();
        let names = pattern.binders();
        assert_eq!(names.len(), values.len(), "instantiate: arity mismatch");
        // The opened names are fresh, so no value can mention one and the
        // substitutions cannot interfere.
        names
            .iter()
            .zip(values)
            .fold(body, |body, (n, v)| body.subst(&Name::from_any(n), v))
    }
}

impl<P, T> Bind<P, T> {
    /// Assemble a binding from parts that are already closed.
    pub(crate) fn from_parts(pattern: P, body: T) -> Self {
        Bind { pattern, body }
    }

    /// The pattern, whose binder names are arbitrary until unbound.
    pub fn pattern(&self) -> &P {
        &self.pattern
    }

    /// The closed body. Bound variables appear as de Bruijn coordinates, so
    /// prefer [`unbind`](Bind::unbind) unless you mean to inspect that form.
    pub fn body(&self) -> &T {
        &self.body
    }

    pub(crate) fn pattern_mut(&mut self) -> &mut P {
        &mut self.pattern
    }

    pub(crate) fn body_mut(&mut self) -> &mut T {
        &mut self.body
    }
}

impl<P: fmt::Display, T: fmt::Display> fmt::Display for Bind<P, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<{}> {}", self.pattern, self.body)
    }
}
