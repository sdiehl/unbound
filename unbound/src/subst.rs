//! Substitution for terms with binding

use std::collections::HashMap;

use crate::{Bind, Name};

/// A wrapper type for names used in substitution
pub enum SubstName<T> {
    Name(Name<T>),
}

/// Trait for types that support substitution
pub trait Subst<V>: Sized {
    /// Check if this term is a variable and return its name
    fn is_var(&self) -> Option<SubstName<V>>;

    /// Perform substitution of `value` for `var` in `self`
    fn subst(&self, var: &Name<V>, value: &V) -> Self;

    /// Perform substitution with a mapping
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

// Default implementation for Name (variables don't substitute into themselves)
impl<T: Clone> Subst<T> for Name<T> {
    fn is_var(&self) -> Option<SubstName<T>> {
        None
    }

    fn subst(&self, _var: &Name<T>, _value: &T) -> Self {
        self.clone()
    }
}

// Implementation for Bind
impl<P: Subst<V> + Clone, T: Subst<V> + Clone, V> Subst<V> for Bind<P, T> {
    fn is_var(&self) -> Option<SubstName<V>> {
        None
    }

    fn subst(&self, var: &Name<V>, value: &V) -> Self {
        // Substitute in both pattern and body
        // Note: a more sophisticated implementation would check for capture
        Bind::new(
            self.pattern().subst(var, value),
            self.body().subst(var, value),
        )
    }
}

// Implementation for basic types
impl<V> Subst<V> for String {
    fn is_var(&self) -> Option<SubstName<V>> {
        None
    }

    fn subst(&self, _var: &Name<V>, _value: &V) -> Self {
        self.clone()
    }
}

impl<V> Subst<V> for usize {
    fn is_var(&self) -> Option<SubstName<V>> {
        None
    }

    fn subst(&self, _var: &Name<V>, _value: &V) -> Self {
        *self
    }
}

impl<V> Subst<V> for i32 {
    fn is_var(&self) -> Option<SubstName<V>> {
        None
    }

    fn subst(&self, _var: &Name<V>, _value: &V) -> Self {
        *self
    }
}

impl<T: Subst<V> + Clone, V> Subst<V> for Option<T> {
    fn is_var(&self) -> Option<SubstName<V>> {
        None
    }

    fn subst(&self, var: &Name<V>, value: &V) -> Self {
        self.as_ref().map(|x| x.subst(var, value))
    }
}

impl<T: Subst<V> + Clone, V> Subst<V> for Vec<T> {
    fn is_var(&self) -> Option<SubstName<V>> {
        None
    }

    fn subst(&self, var: &Name<V>, value: &V) -> Self {
        self.iter().map(|x| x.subst(var, value)).collect()
    }
}

impl<T: Subst<V> + Clone, V> Subst<V> for Box<T> {
    fn is_var(&self) -> Option<SubstName<V>> {
        (**self).is_var()
    }

    fn subst(&self, var: &Name<V>, value: &V) -> Self {
        Box::new((**self).subst(var, value))
    }
}
