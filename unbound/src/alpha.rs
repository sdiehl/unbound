//! Alpha equivalence, and the closing and opening operations it rests on.
//!
//! A term is kept in *locally nameless* form: free variables are named,
//! variables bound by an enclosing [`Bind`] are de Bruijn coordinates. Two
//! terms are then alpha-equivalent exactly when they are structurally equal,
//! so [`Alpha::aeq`] needs no renaming context.
//!
//! [`Bind`]: crate::Bind

use std::rc::Rc;
use std::sync::Arc;

use crate::name::AnyName;
use crate::{AlphaCtx, Bind, Name, Support};

/// Terms that contain names and can be compared up to alpha equivalence.
///
/// Derive this rather than writing it by hand.
pub trait Alpha {
    /// Whether two terms are alpha-equivalent.
    fn aeq(&self, other: &Self) -> bool;

    fn aeq_with(&self, other: &Self, _ctx: &mut AlphaCtx) -> bool {
        self.aeq(other)
    }

    fn support(&self) -> Support {
        Support::unknown()
    }
    fn support_in(&self, acc: &mut Support) {
        acc.merge(self.support());
    }

    fn close_with(&mut self, level: usize, names: &[AnyName], _ctx: &mut AlphaCtx) {
        self.close(level, names);
    }

    fn open_with(&mut self, level: usize, names: &[AnyName], _ctx: &mut AlphaCtx) {
        self.open(level, names);
    }

    /// Abstract `names` into bound variables at de Bruijn `level`.
    ///
    /// `level` counts binders outwards from the one doing the abstracting,
    /// and a name's position in `names` becomes its position in the binder.
    fn close(&mut self, level: usize, names: &[AnyName]);

    /// Replace variables bound at de Bruijn `level` with `names`.
    ///
    /// The inverse of [`Alpha::close`].
    fn open(&mut self, level: usize, names: &[AnyName]);

    /// Collect the free names of this term into `acc`, without duplicates.
    fn fv_in(&self, acc: &mut Vec<AnyName>);

    /// The free names of this term, in order of first occurrence.
    fn fv(&self) -> Vec<AnyName> {
        let mut acc = Vec::new();
        self.fv_in(&mut acc);
        acc
    }
}

/// Patterns that a [`Bind`] can abstract over.
///
/// A pattern supplies the names it binds, and knows how to treat its own
/// binders differently from any annotations it carries: annotations sit
/// outside the scope of the binder, so they are closed and opened at the
/// enclosing level and contribute to the free names of the whole binding.
///
/// [`Bind`]: crate::Bind
pub trait Pattern: Sized {
    /// The names this pattern binds, in binding order.
    fn binders(&self) -> Vec<AnyName>;

    /// A copy of this pattern with freshly indexed binders, and those names.
    fn freshen(&self) -> (Self, Vec<AnyName>);

    /// Compare two patterns ignoring binder identity, which alpha
    /// equivalence is free to rename.
    fn pattern_aeq(&self, other: &Self) -> bool;

    /// Collect the free names of the pattern's annotations, not its binders.
    fn pattern_fv(&self, acc: &mut Vec<AnyName>);

    /// Close the pattern's annotations, leaving its binders alone.
    fn pattern_close(&mut self, level: usize, names: &[AnyName]);

    /// Open the pattern's annotations, leaving its binders alone.
    fn pattern_open(&mut self, level: usize, names: &[AnyName]);

    fn pattern_support(&self) -> Support {
        Support::unknown()
    }
    fn pattern_close_with(&mut self, level: usize, names: &[AnyName], _ctx: &mut AlphaCtx) {
        self.pattern_close(level, names);
    }
    fn pattern_open_with(&mut self, level: usize, names: &[AnyName], _ctx: &mut AlphaCtx) {
        self.pattern_open(level, names);
    }
    fn pattern_aeq_with(&self, other: &Self, _ctx: &mut AlphaCtx) -> bool {
        self.pattern_aeq(other)
    }
}

/// Push `name` onto `acc` unless an equal name is already there.
fn push_unique(acc: &mut Vec<AnyName>, name: AnyName) {
    if !acc.contains(&name) {
        acc.push(name);
    }
}

impl<T> Alpha for Name<T> {
    fn support(&self) -> Support {
        Support::name(self)
    }
    fn aeq(&self, other: &Self) -> bool {
        self == other
    }

    fn close(&mut self, level: usize, names: &[AnyName]) {
        self.close_name(level, names);
    }

    fn open(&mut self, level: usize, names: &[AnyName]) {
        self.open_name(level, names);
    }

    fn fv_in(&self, acc: &mut Vec<AnyName>) {
        if let Some(any) = self.to_any() {
            push_unique(acc, any);
        }
    }
}

macro_rules! alpha_atom {
    ($($ty:ty),* $(,)?) => {$(
        impl Alpha for $ty {
            fn aeq(&self, other: &Self) -> bool { self == other }
            fn support(&self) -> Support { Support::default() }
            fn close(&mut self, _level: usize, _names: &[AnyName]) {}
            fn open(&mut self, _level: usize, _names: &[AnyName]) {}
            fn fv_in(&self, _acc: &mut Vec<AnyName>) {}
        }
    )*};
}

alpha_atom!(bool, char, String, u8, u16, u32, u64, usize, i8, i16, i32, i64, isize);

impl<T: Alpha> Alpha for Option<T> {
    fn aeq(&self, other: &Self) -> bool {
        self.aeq_with(other, &mut AlphaCtx::default())
    }
    fn aeq_with(&self, other: &Self, ctx: &mut AlphaCtx) -> bool {
        match (self, other) {
            (None, None) => true,
            (Some(a), Some(b)) => a.aeq_with(b, ctx),
            _ => false,
        }
    }
    fn close(&mut self, level: usize, names: &[AnyName]) {
        self.close_with(level, names, &mut AlphaCtx::default());
    }
    fn open(&mut self, level: usize, names: &[AnyName]) {
        self.open_with(level, names, &mut AlphaCtx::default());
    }
    fn close_with(&mut self, level: usize, names: &[AnyName], ctx: &mut AlphaCtx) {
        if let Some(x) = self {
            x.close_with(level, names, ctx);
        }
    }
    fn open_with(&mut self, level: usize, names: &[AnyName], ctx: &mut AlphaCtx) {
        if let Some(x) = self {
            x.open_with(level, names, ctx);
        }
    }
    fn support(&self) -> Support {
        self.as_ref().map_or_else(Support::default, Alpha::support)
    }
    fn fv_in(&self, acc: &mut Vec<AnyName>) {
        if let Some(x) = self {
            x.fv_in(acc);
        }
    }
}

impl<T: Alpha> Alpha for Vec<T> {
    fn aeq(&self, other: &Self) -> bool {
        self.aeq_with(other, &mut AlphaCtx::default())
    }
    fn aeq_with(&self, other: &Self, ctx: &mut AlphaCtx) -> bool {
        self.len() == other.len() && self.iter().zip(other).all(|(a, b)| a.aeq_with(b, ctx))
    }
    fn close(&mut self, level: usize, names: &[AnyName]) {
        self.close_with(level, names, &mut AlphaCtx::default());
    }
    fn open(&mut self, level: usize, names: &[AnyName]) {
        self.open_with(level, names, &mut AlphaCtx::default());
    }
    fn close_with(&mut self, level: usize, names: &[AnyName], ctx: &mut AlphaCtx) {
        for x in self {
            x.close_with(level, names, ctx);
        }
    }
    fn open_with(&mut self, level: usize, names: &[AnyName], ctx: &mut AlphaCtx) {
        for x in self {
            x.open_with(level, names, ctx);
        }
    }
    fn support(&self) -> Support {
        {
            let mut out = Support::default();
            for x in self {
                out.merge(x.support());
            }
            out
        }
    }
    fn fv_in(&self, acc: &mut Vec<AnyName>) {
        for x in self {
            x.fv_in(acc);
        }
    }
}

impl<T: Alpha> Alpha for Box<T> {
    fn aeq(&self, other: &Self) -> bool {
        self.aeq_with(other, &mut AlphaCtx::default())
    }
    fn aeq_with(&self, other: &Self, ctx: &mut AlphaCtx) -> bool {
        (**self).aeq_with(other, ctx)
    }
    fn close(&mut self, level: usize, names: &[AnyName]) {
        self.close_with(level, names, &mut AlphaCtx::default());
    }
    fn open(&mut self, level: usize, names: &[AnyName]) {
        self.open_with(level, names, &mut AlphaCtx::default());
    }
    fn close_with(&mut self, level: usize, names: &[AnyName], ctx: &mut AlphaCtx) {
        (**self).close_with(level, names, ctx);
    }
    fn open_with(&mut self, level: usize, names: &[AnyName], ctx: &mut AlphaCtx) {
        (**self).open_with(level, names, ctx);
    }
    fn support(&self) -> Support {
        (**self).support()
    }
    fn fv_in(&self, acc: &mut Vec<AnyName>) {
        (**self).fv_in(acc);
    }
}

impl<T: Alpha + Clone> Alpha for Rc<T> {
    fn aeq(&self, other: &Self) -> bool {
        self.aeq_with(other, &mut AlphaCtx::default())
    }
    fn aeq_with(&self, other: &Self, ctx: &mut AlphaCtx) -> bool {
        Rc::ptr_eq(self, other) || (**self).aeq_with(other, ctx)
    }
    fn close(&mut self, level: usize, names: &[AnyName]) {
        self.close_with(level, names, &mut AlphaCtx::default());
    }
    fn open(&mut self, level: usize, names: &[AnyName]) {
        self.open_with(level, names, &mut AlphaCtx::default());
    }
    fn close_with(&mut self, level: usize, names: &[AnyName], ctx: &mut AlphaCtx) {
        Rc::make_mut(self).close_with(level, names, ctx);
    }
    fn open_with(&mut self, level: usize, names: &[AnyName], ctx: &mut AlphaCtx) {
        Rc::make_mut(self).open_with(level, names, ctx);
    }
    fn support(&self) -> Support {
        (**self).support()
    }
    fn fv_in(&self, acc: &mut Vec<AnyName>) {
        (**self).fv_in(acc);
    }
}

impl<T: Alpha + Clone> Alpha for Arc<T> {
    fn aeq(&self, other: &Self) -> bool {
        self.aeq_with(other, &mut AlphaCtx::default())
    }
    fn aeq_with(&self, other: &Self, ctx: &mut AlphaCtx) -> bool {
        Arc::ptr_eq(self, other) || (**self).aeq_with(other, ctx)
    }
    fn close(&mut self, level: usize, names: &[AnyName]) {
        self.close_with(level, names, &mut AlphaCtx::default());
    }
    fn open(&mut self, level: usize, names: &[AnyName]) {
        self.open_with(level, names, &mut AlphaCtx::default());
    }
    fn close_with(&mut self, level: usize, names: &[AnyName], ctx: &mut AlphaCtx) {
        Arc::make_mut(self).close_with(level, names, ctx);
    }
    fn open_with(&mut self, level: usize, names: &[AnyName], ctx: &mut AlphaCtx) {
        Arc::make_mut(self).open_with(level, names, ctx);
    }
    fn support(&self) -> Support {
        (**self).support()
    }
    fn fv_in(&self, acc: &mut Vec<AnyName>) {
        (**self).fv_in(acc);
    }
}

impl<A: Alpha, B: Alpha> Alpha for (A, B) {
    fn aeq(&self, other: &Self) -> bool {
        self.aeq_with(other, &mut AlphaCtx::default())
    }
    fn aeq_with(&self, other: &Self, ctx: &mut AlphaCtx) -> bool {
        self.0.aeq_with(&other.0, ctx) && self.1.aeq_with(&other.1, ctx)
    }
    fn close(&mut self, level: usize, names: &[AnyName]) {
        self.close_with(level, names, &mut AlphaCtx::default());
    }
    fn open(&mut self, level: usize, names: &[AnyName]) {
        self.open_with(level, names, &mut AlphaCtx::default());
    }
    fn close_with(&mut self, level: usize, names: &[AnyName], ctx: &mut AlphaCtx) {
        self.0.close_with(level, names, ctx);
        self.1.close_with(level, names, ctx);
    }
    fn open_with(&mut self, level: usize, names: &[AnyName], ctx: &mut AlphaCtx) {
        self.0.open_with(level, names, ctx);
        self.1.open_with(level, names, ctx);
    }
    fn support(&self) -> Support {
        {
            let mut out = self.0.support();
            out.merge(self.1.support());
            out
        }
    }
    fn fv_in(&self, acc: &mut Vec<AnyName>) {
        self.0.fv_in(acc);
        self.1.fv_in(acc);
    }
}

impl<P: Pattern, T: Alpha> Alpha for Bind<P, T> {
    fn aeq(&self, other: &Self) -> bool {
        self.aeq_with(other, &mut AlphaCtx::default())
    }
    fn aeq_with(&self, other: &Self, ctx: &mut AlphaCtx) -> bool {
        self.pattern().pattern_aeq_with(other.pattern(), ctx)
            && self.body().aeq_with(other.body(), ctx)
    }
    fn close(&mut self, level: usize, names: &[AnyName]) {
        self.close_with(level, names, &mut AlphaCtx::default());
    }
    fn open(&mut self, level: usize, names: &[AnyName]) {
        self.open_with(level, names, &mut AlphaCtx::default());
    }
    fn close_with(&mut self, level: usize, names: &[AnyName], ctx: &mut AlphaCtx) {
        self.pattern_mut().pattern_close_with(level, names, ctx);
        self.body_mut().close_with(level + 1, names, ctx);
    }
    fn open_with(&mut self, level: usize, names: &[AnyName], ctx: &mut AlphaCtx) {
        self.pattern_mut().pattern_open_with(level, names, ctx);
        self.body_mut().open_with(level + 1, names, ctx);
    }
    fn support(&self) -> Support {
        {
            let mut out = self.pattern().pattern_support();
            out.merge(self.body().support().under_binder());
            out
        }
    }
    fn fv_in(&self, acc: &mut Vec<AnyName>) {
        self.pattern().pattern_fv(acc);
        self.body().fv_in(acc);
    }
}

/// A single binder.
impl<T> Pattern for Name<T> {
    fn pattern_support(&self) -> Support {
        Support::default()
    }
    fn binders(&self) -> Vec<AnyName> {
        self.to_any().into_iter().collect()
    }

    fn freshen(&self) -> (Self, Vec<AnyName>) {
        let fresh = Name::freshen(self);
        let names = fresh.to_any().into_iter().collect();
        (fresh, names)
    }

    fn pattern_aeq(&self, _other: &Self) -> bool {
        true
    }

    fn pattern_fv(&self, _acc: &mut Vec<AnyName>) {}

    fn pattern_close(&mut self, _level: usize, _names: &[AnyName]) {}

    fn pattern_open(&mut self, _level: usize, _names: &[AnyName]) {}
}

/// A telescope of binders, bound simultaneously.
impl<T> Pattern for Vec<Name<T>> {
    fn pattern_support(&self) -> Support {
        Support::default()
    }
    fn binders(&self) -> Vec<AnyName> {
        self.iter().filter_map(|n| n.to_any()).collect()
    }

    fn freshen(&self) -> (Self, Vec<AnyName>) {
        let fresh: Vec<Name<T>> = self.iter().map(Name::freshen).collect();
        let names = fresh.iter().filter_map(|n| n.to_any()).collect();
        (fresh, names)
    }

    fn pattern_aeq(&self, other: &Self) -> bool {
        self.len() == other.len()
    }

    fn pattern_fv(&self, _acc: &mut Vec<AnyName>) {}

    fn pattern_close(&mut self, _level: usize, _names: &[AnyName]) {}

    fn pattern_open(&mut self, _level: usize, _names: &[AnyName]) {}
}

/// A binder carrying an annotation, such as a type ascription. The
/// annotation is outside the binder's own scope.
impl<T, U: Alpha + Clone> Pattern for (Name<T>, U) {
    fn pattern_support(&self) -> Support {
        self.1.support()
    }
    fn pattern_close_with(&mut self, level: usize, names: &[AnyName], ctx: &mut AlphaCtx) {
        self.1.close_with(level, names, ctx);
    }
    fn pattern_open_with(&mut self, level: usize, names: &[AnyName], ctx: &mut AlphaCtx) {
        self.1.open_with(level, names, ctx);
    }
    fn pattern_aeq_with(&self, other: &Self, ctx: &mut AlphaCtx) -> bool {
        self.0.pattern_aeq(&other.0) && self.1.aeq_with(&other.1, ctx)
    }

    fn binders(&self) -> Vec<AnyName> {
        self.0.to_any().into_iter().collect()
    }

    fn freshen(&self) -> (Self, Vec<AnyName>) {
        let fresh = Name::freshen(&self.0);
        let names = fresh.to_any().into_iter().collect();
        ((fresh, self.1.clone()), names)
    }

    fn pattern_aeq(&self, other: &Self) -> bool {
        self.1.aeq(&other.1)
    }

    fn pattern_fv(&self, acc: &mut Vec<AnyName>) {
        self.1.fv_in(acc);
    }

    fn pattern_close(&mut self, level: usize, names: &[AnyName]) {
        self.1.close(level, names);
    }

    fn pattern_open(&mut self, level: usize, names: &[AnyName]) {
        self.1.open(level, names);
    }
}

/// A telescope of binders carrying an annotation.
impl<T, U: Alpha + Clone> Pattern for (Vec<Name<T>>, U) {
    fn pattern_support(&self) -> Support {
        self.1.support()
    }
    fn pattern_close_with(&mut self, level: usize, names: &[AnyName], ctx: &mut AlphaCtx) {
        self.1.close_with(level, names, ctx);
    }
    fn pattern_open_with(&mut self, level: usize, names: &[AnyName], ctx: &mut AlphaCtx) {
        self.1.open_with(level, names, ctx);
    }
    fn pattern_aeq_with(&self, other: &Self, ctx: &mut AlphaCtx) -> bool {
        self.0.pattern_aeq(&other.0) && self.1.aeq_with(&other.1, ctx)
    }

    fn binders(&self) -> Vec<AnyName> {
        self.0.iter().filter_map(|n| n.to_any()).collect()
    }

    fn freshen(&self) -> (Self, Vec<AnyName>) {
        let fresh: Vec<Name<T>> = self.0.iter().map(Name::freshen).collect();
        let names = fresh.iter().filter_map(|n| n.to_any()).collect();
        ((fresh, self.1.clone()), names)
    }

    fn pattern_aeq(&self, other: &Self) -> bool {
        self.0.len() == other.0.len() && self.1.aeq(&other.1)
    }

    fn pattern_fv(&self, acc: &mut Vec<AnyName>) {
        self.1.fv_in(acc);
    }

    fn pattern_close(&mut self, level: usize, names: &[AnyName]) {
        self.1.close(level, names);
    }

    fn pattern_open(&mut self, level: usize, names: &[AnyName]) {
        self.1.open(level, names);
    }
}
