//! The binding construct.

use std::fmt;

use crate::alpha::{Alpha, Pattern};
use crate::subst::{InstantiateCtx, Subst};
use crate::Name;

/// A pattern binding its names in a body.
///
/// [`Bind<P, T>`] keeps its body in locally nameless form:
///
/// - [`Bind::new`] **closes** the body, rewriting every free occurrence of a
///   name the pattern binds into a de Bruijn coordinate
/// - [`Bind::unbind`] **opens** it again, drawing genuinely fresh names from
///   the global counter
///
/// So `\x. \y. x y` is stored as `\. \. #1.0 #0.0`, and the binder's own
/// name is inert decoration. Three properties fall out:
///
/// - **Alpha equivalence is structural equality.** [`Alpha::aeq`] walks both
///   terms in lockstep without renaming bound variables, since alpha-variants
///   have identical de Bruijn bodies
/// - **Substitution cannot capture.** A bound variable has no name for an
///   incoming term to collide with, so [`Subst::subst`] needs neither
///   freshening nor a shadowing check
/// - **Free variables are exact.** [`Alpha::fv`] collects only genuinely free
///   names, compared by index, so two distinct variables that happen to share
///   a spelling are never conflated
///
/// Because the body is stored closed, reach for it through [`Bind::unbind`]
/// (or [`Bind::unbind_ref`]) rather than [`Bind::body`], which hands back the
/// raw de Bruijn form. To substitute straight into the body instead, as beta
/// reduction or type instantiation does, use [`Bind::instantiate`], or
/// [`Bind::instantiate_all`] for a pattern binding several names.
///
/// ```
/// use unbound::prelude::*;
///
/// #[derive(Clone, Debug, Alpha, Subst)]
/// enum Expr {
///     Var(Name<Expr>),
///     Lam(Bind<Name<Expr>, Box<Expr>>),
///     App(Box<Expr>, Box<Expr>),
/// }
///
/// let (x, y, z): (Name<Expr>, Name<Expr>, Name<Expr>) = (s2n("x"), s2n("y"), s2n("z"));
/// let var = |n: &Name<Expr>| Expr::Var(n.clone());
///
/// // \x. x z
/// let b = bind(x.clone(), Box::new(Expr::App(Box::new(var(&x)), Box::new(var(&z)))));
/// assert_eq!(b.fv().len(), 1);
///
/// // Beta reduction: (\x. x z) y = y z
/// let reduced = b.instantiate(&var(&y));
/// assert!(reduced.aeq(&Box::new(Expr::App(Box::new(var(&y)), Box::new(var(&z))))));
///
/// // Opening yields a name distinct from every other, whatever its spelling.
/// let (x1, _) = b.unbind_ref();
/// assert_ne!(x1, x);
/// ```
///
/// [`Alpha::aeq`]: crate::Alpha::aeq
/// [`Alpha::fv`]: crate::Alpha::fv
/// [`Subst::subst`]: crate::Subst::subst
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
        T: Clone,
    {
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
        T: Clone + Subst<V>,
    {
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
        T: Clone + Subst<V>,
    {
        assert_eq!(
            self.pattern.binders().len(),
            values.len(),
            "instantiate: arity mismatch"
        );
        if let Some(body) = self
            .body
            .instantiate_with(0, &mut InstantiateCtx::new(values))
        {
            return body;
        }
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
