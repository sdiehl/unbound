//! Capture-avoiding substitution.
//!
//! Because bodies are stored closed, a variable bound by an enclosing
//! binder is a de Bruijn coordinate rather than a name, and there is no
//! name under a binder for an incoming term to be captured by. Substitution
//! is therefore a plain traversal with no freshening and no shadowing check.

use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

use crate::shared::Memo;
use crate::{Bind, Name};

pub struct SubstCtx<'a, V> {
    var: &'a Name<V>,
    value: &'a V,
    pub(crate) memo: Memo,
}
impl<'a, V> SubstCtx<'a, V> {
    pub fn new(var: &'a Name<V>, value: &'a V) -> Self {
        Self {
            var,
            value,
            memo: Memo::default(),
        }
    }
    pub fn var(&self) -> &Name<V> {
        self.var
    }
    pub fn value(&self) -> &V {
        self.value
    }
}

pub struct InstantiateCtx<'a, V> {
    values: &'a [V],
    pub(crate) memo: Memo,
}
impl<'a, V> InstantiateCtx<'a, V> {
    pub fn new(values: &'a [V]) -> Self {
        Self {
            values,
            memo: Memo::default(),
        }
    }
    pub fn values(&self) -> &[V] {
        self.values
    }
}

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

    fn subst_with(&self, ctx: &mut SubstCtx<'_, V>) -> Self {
        self.subst(ctx.var(), ctx.value())
    }

    /// None requests the compatible open-then-substitute fallback.
    fn instantiate_with(&self, _level: usize, _ctx: &mut InstantiateCtx<'_, V>) -> Option<Self> {
        None
    }

    /// Apply a whole substitution at once.
    fn subst_all(&self, subst_map: &HashMap<Name<V>, V>) -> Self
    where
        V: Clone,
        Self: Clone,
    {
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
    fn instantiate_with(&self, _level: usize, _ctx: &mut InstantiateCtx<'_, V>) -> Option<Self> {
        Some(self.clone())
    }

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
            fn instantiate_with(&self, _level: usize, _ctx: &mut InstantiateCtx<'_, V>) -> Option<Self> { Some(self.clone()) }
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
        self.subst_with(&mut SubstCtx::new(var, value))
    }
    fn subst_with(&self, ctx: &mut SubstCtx<'_, V>) -> Self {
        self.as_ref().map(|x| x.subst_with(ctx))
    }
    fn instantiate_with(&self, level: usize, ctx: &mut InstantiateCtx<'_, V>) -> Option<Self> {
        Some(match self {
            Some(x) => Some(x.instantiate_with(level, ctx)?),
            None => None,
        })
    }
}

impl<T: Subst<V>, V> Subst<V> for Vec<T> {
    fn is_var(&self) -> Option<SubstName<V>> {
        None
    }
    fn subst(&self, var: &Name<V>, value: &V) -> Self {
        self.subst_with(&mut SubstCtx::new(var, value))
    }
    fn subst_with(&self, ctx: &mut SubstCtx<'_, V>) -> Self {
        self.iter().map(|x| x.subst_with(ctx)).collect()
    }
    fn instantiate_with(&self, level: usize, ctx: &mut InstantiateCtx<'_, V>) -> Option<Self> {
        self.iter()
            .map(|x| x.instantiate_with(level, ctx))
            .collect()
    }
}

impl<T: Subst<V>, V> Subst<V> for Box<T> {
    fn is_var(&self) -> Option<SubstName<V>> {
        (**self).is_var()
    }
    fn subst(&self, var: &Name<V>, value: &V) -> Self {
        self.subst_with(&mut SubstCtx::new(var, value))
    }
    fn subst_with(&self, ctx: &mut SubstCtx<'_, V>) -> Self {
        Box::new((**self).subst_with(ctx))
    }
    fn instantiate_with(&self, level: usize, ctx: &mut InstantiateCtx<'_, V>) -> Option<Self> {
        Some(Box::new((**self).instantiate_with(level, ctx)?))
    }
}

impl<T: Subst<V>, V> Subst<V> for Rc<T> {
    fn is_var(&self) -> Option<SubstName<V>> {
        (**self).is_var()
    }
    fn subst(&self, var: &Name<V>, value: &V) -> Self {
        self.subst_with(&mut SubstCtx::new(var, value))
    }
    fn subst_with(&self, ctx: &mut SubstCtx<'_, V>) -> Self {
        Rc::new((**self).subst_with(ctx))
    }
    fn instantiate_with(&self, level: usize, ctx: &mut InstantiateCtx<'_, V>) -> Option<Self> {
        Some(Rc::new((**self).instantiate_with(level, ctx)?))
    }
}

impl<T: Subst<V>, V> Subst<V> for Arc<T> {
    fn is_var(&self) -> Option<SubstName<V>> {
        (**self).is_var()
    }
    fn subst(&self, var: &Name<V>, value: &V) -> Self {
        self.subst_with(&mut SubstCtx::new(var, value))
    }
    fn subst_with(&self, ctx: &mut SubstCtx<'_, V>) -> Self {
        Arc::new((**self).subst_with(ctx))
    }
    fn instantiate_with(&self, level: usize, ctx: &mut InstantiateCtx<'_, V>) -> Option<Self> {
        Some(Arc::new((**self).instantiate_with(level, ctx)?))
    }
}

impl<A: Subst<V>, B: Subst<V>, V> Subst<V> for (A, B) {
    fn is_var(&self) -> Option<SubstName<V>> {
        None
    }
    fn subst(&self, var: &Name<V>, value: &V) -> Self {
        self.subst_with(&mut SubstCtx::new(var, value))
    }
    fn subst_with(&self, ctx: &mut SubstCtx<'_, V>) -> Self {
        (self.0.subst_with(ctx), self.1.subst_with(ctx))
    }
    fn instantiate_with(&self, level: usize, ctx: &mut InstantiateCtx<'_, V>) -> Option<Self> {
        Some((
            self.0.instantiate_with(level, ctx)?,
            self.1.instantiate_with(level, ctx)?,
        ))
    }
}

impl<P: Subst<V>, T: Subst<V>, V> Subst<V> for Bind<P, T> {
    fn is_var(&self) -> Option<SubstName<V>> {
        None
    }
    fn subst(&self, var: &Name<V>, value: &V) -> Self {
        self.subst_with(&mut SubstCtx::new(var, value))
    }
    fn subst_with(&self, ctx: &mut SubstCtx<'_, V>) -> Self {
        Bind::from_parts(self.pattern().subst_with(ctx), self.body().subst_with(ctx))
    }
    fn instantiate_with(&self, level: usize, ctx: &mut InstantiateCtx<'_, V>) -> Option<Self> {
        Some(Bind::from_parts(
            self.pattern().instantiate_with(level, ctx)?,
            self.body().instantiate_with(level + 1, ctx)?,
        ))
    }
}
