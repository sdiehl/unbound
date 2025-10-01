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

// Implementation for tuples
impl<A: Subst<V> + Clone, B: Subst<V> + Clone, V> Subst<V> for (A, B) {
    fn is_var(&self) -> Option<SubstName<V>> {
        None
    }

    fn subst(&self, var: &Name<V>, value: &V) -> Self {
        (self.0.subst(var, value), self.1.subst(var, value))
    }
}

// Specialized implementation for Bind with Name pattern
impl<T: Subst<V> + Clone, V: Clone> Subst<V> for Bind<Name<V>, Box<T>> {
    fn is_var(&self) -> Option<SubstName<V>> {
        None
    }

    fn subst(&self, var: &Name<V>, value: &V) -> Self {
        if self.pattern() == var {
            // Variable is bound, no substitution
            self.clone()
        } else {
            Bind::new(self.pattern().clone(), self.body().subst(var, value))
        }
    }
}

// Implementation for Bind with Vec<Name> pattern where we need to check if
// names match
impl<T: Subst<V> + Clone, V: Clone> Subst<V> for Bind<Vec<Name<V>>, Box<T>> {
    fn is_var(&self) -> Option<SubstName<V>> {
        None
    }

    fn subst(&self, var: &Name<V>, value: &V) -> Self {
        if self.pattern().iter().any(|n| n == var) {
            // Variable is bound, no substitution
            self.clone()
        } else {
            Bind::new(self.pattern().clone(), self.body().subst(var, value))
        }
    }
}

// Implementation for Bind with tuple pattern (Name, Extra)
impl<T: Subst<V> + Clone, U: Subst<V> + Clone, V: Clone> Subst<V> for Bind<(Name<V>, U), Box<T>> {
    fn is_var(&self) -> Option<SubstName<V>> {
        None
    }

    fn subst(&self, var: &Name<V>, value: &V) -> Self {
        let (name, extra) = self.pattern();
        if name == var {
            // Variable is bound, but still substitute in annotation
            Bind::new((name.clone(), extra.subst(var, value)), self.body().clone())
        } else {
            Bind::new(
                (name.clone(), extra.subst(var, value)),
                self.body().subst(var, value),
            )
        }
    }
}
