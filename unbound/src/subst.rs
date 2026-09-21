//! Capture-avoiding substitution.
//!
//! Because bodies are stored closed, a variable bound by an enclosing
//! binder is a de Bruijn coordinate rather than a name, and there is no
//! name under a binder for an incoming term to be captured by. Substitution
//! is therefore a plain traversal with no freshening and no shadowing check.

use std::collections::HashMap;

use crate::{Bind, Name};

/// A name discovered by [`Subst::is_var`].
pub enum SubstName<T> {
    /// The term was a variable standing for this name.
    Name(Name<T>),
}

/// Terms that admit substitution of `V` for a `Name<V>`.
///
/// Derive this rather than writing it by hand.
pub trait Subst<V>: Sized {
    /// The name this term stands for, if it is a variable.
    fn is_var(&self) -> Option<SubstName<V>>;

    /// Replace free occurrences of `var` with `value`.
    fn subst(&self, var: &Name<V>, value: &V) -> Self;

    /// Apply a whole substitution at once.
    fn subst_all(&self, subst_map: &HashMap<Name<V>, V>) -> Self
    where
        V: Clone,
        Self: Clone, {
        let mut result = self.clone();
        for (var, val) in subst_map {
            result = result.subst(var, val);
        }
        result
    }
}

/// A name is never itself a term, whatever it stands for, so substitution
/// leaves it alone. This covers binder positions inside patterns.
impl<T, V> Subst<V> for Name<T> {
    fn is_var(&self) -> Option<SubstName<V>> {
        None
    }

    fn subst(&self, _var: &Name<V>, _value: &V) -> Self {
        self.clone()
    }
}

macro_rules! subst_atom {
    ($($ty:ty),* $(,)?) => {$(
        impl<V> Subst<V> for $ty {
            fn is_var(&self) -> Option<SubstName<V>> { None }
            fn subst(&self, _var: &Name<V>, _value: &V) -> Self { self.clone() }
        }
    )*};
}

subst_atom!(bool, char, String, u8, u16, u32, u64, usize, i8, i16, i32, i64, isize);

impl<T: Subst<V>, V> Subst<V> for Option<T> {
    fn is_var(&self) -> Option<SubstName<V>> {
        None
    }

    fn subst(&self, var: &Name<V>, value: &V) -> Self {
        self.as_ref().map(|x| x.subst(var, value))
    }
}

impl<T: Subst<V>, V> Subst<V> for Vec<T> {
    fn is_var(&self) -> Option<SubstName<V>> {
        None
    }

    fn subst(&self, var: &Name<V>, value: &V) -> Self {
        self.iter().map(|x| x.subst(var, value)).collect()
    }
}

impl<T: Subst<V>, V> Subst<V> for Box<T> {
    fn is_var(&self) -> Option<SubstName<V>> {
        (**self).is_var()
    }

    fn subst(&self, var: &Name<V>, value: &V) -> Self {
        Box::new((**self).subst(var, value))
    }
}

impl<A: Subst<V>, B: Subst<V>, V> Subst<V> for (A, B) {
    fn is_var(&self) -> Option<SubstName<V>> {
        None
    }

    fn subst(&self, var: &Name<V>, value: &V) -> Self {
        (self.0.subst(var, value), self.1.subst(var, value))
    }
}

/// Substituting under a binder needs no special care: the binder's own
/// variables are coordinates, and `value` is locally closed, so moving it
/// underneath any number of binders cannot disturb it.
impl<P: Subst<V>, T: Subst<V>, V> Subst<V> for Bind<P, T> {
    fn is_var(&self) -> Option<SubstName<V>> {
        None
    }

    fn subst(&self, var: &Name<V>, value: &V) -> Self {
        Bind::from_parts(
            self.pattern().subst(var, value),
            self.body().subst(var, value),
        )
    }
}
