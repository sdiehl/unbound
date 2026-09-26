//! Regression tests for the locally nameless core: the de Bruijn form a
//! binder closes into, and the capture and scoping properties that follow.

use unbound::prelude::*;

type Var = Name<Expr>;

#[derive(Clone, Debug, Alpha, Subst)]
enum Expr {
    V(Var),
    Lam(Bind<Var, Box<Expr>>),
    /// A binder carrying an annotation, which sits outside its own scope.
    Ann(Bind<(Var, Box<Expr>), Box<Expr>>),
    /// Several names bound at once.
    Many(Bind<Vec<Var>, Box<Expr>>),
    App(Box<Expr>, Box<Expr>),
}

fn lam(v: Var, body: Expr) -> Expr {
    Expr::Lam(bind(v, Box::new(body)))
}

fn app(a: Expr, b: Expr) -> Expr {
    Expr::App(Box::new(a), Box::new(b))
}

/// The de Bruijn coordinates of every variable, in left-to-right order.
/// A free variable reads as `None`.
fn coords(e: &Expr) -> Vec<Option<(usize, usize)>> {
    let mut out = Vec::new();
    fn go(e: &Expr, out: &mut Vec<Option<(usize, usize)>>) {
        match e {
            Expr::V(x) => out.push(x.coordinates()),
            Expr::Lam(b) => go(b.body(), out),
            Expr::Ann(b) => {
                go(&b.pattern().1, out);
                go(b.body(), out);
            }
            Expr::Many(b) => go(b.body(), out),
            Expr::App(a, b) => {
                go(a, out);
                go(b, out);
            }
        }
    }
    go(e, &mut out);
    out
}

#[test]
fn binders_close_to_de_bruijn_coordinates() {
    let x: Var = s2n("x");
    let y: Var = s2n("y");

    // \x. \y. x y  closes to  \. \. #1.0 #0.0
    let term = lam(x.clone(), lam(y.clone(), app(Expr::V(x), Expr::V(y))));
    assert_eq!(coords(&term), vec![Some((1, 0)), Some((0, 0))]);
}

#[test]
fn simultaneous_binders_take_distinct_positions() {
    let a: Var = s2n("a");
    let b: Var = s2n("b");

    let term = Expr::Many(bind(
        vec![a.clone(), b.clone()],
        Box::new(app(Expr::V(b), Expr::V(a))),
    ));
    // b is position 1, a is position 0, both at level 0.
    assert_eq!(coords(&term), vec![Some((0, 1)), Some((0, 0))]);
}

#[test]
fn free_variables_stay_free_under_binders() {
    let x: Var = s2n("x");
    let free: Var = s2n("free");

    let term = lam(x.clone(), app(Expr::V(x), Expr::V(free.clone())));
    assert_eq!(coords(&term), vec![Some((0, 0)), None]);
    assert_eq!(term.fv(), vec![free.to_any().unwrap()]);
}

#[test]
fn substitution_does_not_capture() {
    let x: Var = s2n("x");
    let y: Var = s2n("y");

    // (\y. x)[x := y] must not become the identity.
    let term = lam(y.clone(), Expr::V(x.clone()));
    let result = term.subst(&x, &Expr::V(y.clone()));

    let identity = lam(y.clone(), Expr::V(y.clone()));
    assert!(!result.aeq(&identity), "y was captured by the binder");

    // The binder is vacuous and y remains free in the result.
    assert_eq!(result.fv(), vec![y.to_any().unwrap()]);
    assert_eq!(coords(&result), vec![None]);
}

#[test]
fn free_variables_are_identities_not_spellings() {
    // Two distinct variables that happen to share a spelling.
    let outer: Var = s2n("v");
    let inner: Var = s2n("v");
    assert_ne!(outer, inner);

    // \v_outer. v_inner leaves v_inner free despite the shared spelling.
    let term = lam(outer, Expr::V(inner.clone()));
    assert_eq!(term.fv(), vec![inner.to_any().unwrap()]);
}

#[test]
fn annotations_sit_outside_the_binder_scope() {
    let x: Var = s2n("x");

    // (x : x). x  binds only the occurrence in the body.
    let term = Expr::Ann(bind(
        (x.clone(), Box::new(Expr::V(x.clone()))),
        Box::new(Expr::V(x.clone())),
    ));

    // Annotation first, then body: free, then bound.
    assert_eq!(coords(&term), vec![None, Some((0, 0))]);
    assert_eq!(term.fv(), vec![x.to_any().unwrap()]);
}

#[test]
fn unbinding_twice_yields_distinct_names() {
    let x: Var = s2n("x");
    let term = bind(x.clone(), Box::new(Expr::V(x)));

    let (a, _) = term.unbind_ref();
    let (b, _) = term.unbind_ref();
    assert_ne!(a, b, "each unbind must draw a genuinely fresh name");
}

#[test]
fn fresh_names_never_collide_with_existing_ones() {
    // Freshening draws from the same global counter as ordinary names, so a
    // fresh name cannot alias one that already exists.
    let existing: Var = s2n("q");
    let fresh = run_fresh(Fresh::fresh(&existing));
    assert_ne!(existing, fresh);
    assert_ne!(existing.index(), fresh.index());
}

#[test]
fn alpha_equivalence_ignores_binder_names() {
    let x: Var = s2n("x");
    let y: Var = s2n("y");

    assert!(lam(x.clone(), Expr::V(x.clone())).aeq(&lam(y.clone(), Expr::V(y))));
    // But a vacuous binder is not the identity.
    assert!(!lam(x.clone(), Expr::V(x.clone())).aeq(&lam(s2n("z"), Expr::V(x))));
}

#[test]
fn closing_and_opening_round_trip() {
    let x: Var = s2n("x");
    let y: Var = s2n("y");
    let term = lam(x.clone(), lam(y.clone(), app(Expr::V(x), Expr::V(y))));

    let Expr::Lam(outer) = term.clone() else {
        unreachable!()
    };
    let (x2, body) = outer.unbind();
    let rebound = lam(x2, *body);

    assert!(
        term.aeq(&rebound),
        "unbind then rebind must be the identity"
    );
}

/// The same calculus over shared pointers.
#[test]
fn instantiate_substitutes_for_the_binder() {
    let (x, y, z): (Var, Var, Var) = (s2n("x"), s2n("y"), s2n("z"));
    let Expr::Lam(b) = lam(x.clone(), app(Expr::V(x), Expr::V(y.clone()))) else {
        unreachable!()
    };
    let out = b.instantiate(&Expr::V(z.clone()));
    assert!(out.aeq(&Box::new(app(Expr::V(z), Expr::V(y)))));
}

#[test]
fn instantiate_all_fills_binders_in_order() {
    let (x, y): (Var, Var) = (s2n("x"), s2n("y"));
    let (a, b): (Var, Var) = (s2n("a"), s2n("b"));
    let many = bind(
        vec![x.clone(), y.clone()],
        Box::new(app(Expr::V(y), Expr::V(x))),
    );
    let out = many.instantiate_all(&[Expr::V(a.clone()), Expr::V(b.clone())]);
    assert!(out.aeq(&Box::new(app(Expr::V(b), Expr::V(a)))));
}

#[test]
fn global_names_scope_lexically() {
    // \x. \y. (\x. x) x, built bottom-up as a parser would.
    let x = || Name::<Expr>::global("x");
    let y = Name::<Expr>::global("y");
    let inner = lam(x(), Expr::V(x()));
    let term = lam(x(), lam(y, app(inner, Expr::V(x()))));
    assert_eq!(
        coords(&term),
        vec![Some((0, 0)), Some((1, 0))],
        "each occurrence belongs to its nearest binder"
    );
    assert_ne!(x(), s2n::<Expr>("x"));
}

#[test]
fn name_scope_renames_only_to_avoid_capture() {
    fn show(e: &Expr, s: &mut NameScope) -> String {
        match e {
            Expr::V(v) => s.get(v).to_string(),
            Expr::Lam(b) => {
                let (x, body) = b.unbind_ref();
                let d = s.bind(&x, &body.fv());
                let out = format!("\\{d}. {}", show(&body, s));
                s.pop();
                out
            }
            Expr::App(f, a) => format!("({} {})", show(f, s), show(a, s)),
            _ => unreachable!(),
        }
    }
    let print = |e: &Expr| show(e, &mut NameScope::new(&e.fv()));
    let (x, x2): (Var, Var) = (s2n("x"), s2n("x"));

    // The inner x must not capture the outer one.
    let k = lam(x.clone(), lam(x2.clone(), Expr::V(x.clone())));
    assert_eq!(print(&k), "\\x. \\x1. x");

    // Shadowing an unused outer x is fine.
    let k2 = lam(x.clone(), lam(x2.clone(), Expr::V(x2.clone())));
    assert_eq!(print(&k2), "\\x. \\x. x");

    // Distinct free names that share a spelling are told apart.
    assert_eq!(print(&app(Expr::V(x), Expr::V(x2))), "(x x1)");
}

mod typed {
    use std::rc::Rc;

    use unbound::prelude::*;

    #[derive(Clone, Debug, PartialEq, Alpha, Subst)]
    #[subst(_)]
    enum Kind {
        Star,
        Arr(Rc<Kind>, Rc<Kind>),
    }

    #[derive(Clone, Debug, Alpha, Subst)]
    #[subst(Self, Tm)]
    enum Ty {
        Var(Name<Ty>),
        Forall(Bind<(Name<Ty>, Kind), Rc<Ty>>),
        Int,
    }

    #[derive(Clone, Debug, Alpha, Subst)]
    #[subst(Self, Ty)]
    enum Tm {
        Var(Name<Tm>),
        Lam(Bind<(Name<Tm>, Ty), Rc<Tm>>),
        App(Rc<Tm>, Rc<Tm>),
    }

    #[test]
    fn terms_take_type_substitutions() {
        let a: Name<Ty> = s2n("a");
        let x: Name<Tm> = s2n("x");
        let id = Tm::Lam(bind((x.clone(), Ty::Var(a.clone())), Rc::new(Tm::Var(x))));
        let Tm::Lam(b) = id.subst(&a, &Ty::Int) else {
            unreachable!()
        };
        assert!(b.pattern().1.aeq(&Ty::Int));
    }

    #[test]
    fn terms_substitute_into_themselves_past_annotations() {
        let (x, y, z): (Name<Tm>, Name<Tm>, Name<Tm>) = (s2n("x"), s2n("y"), s2n("z"));
        let k = Tm::Lam(bind((y.clone(), Ty::Int), Rc::new(Tm::Var(x.clone()))));
        let Tm::Lam(b) = k.subst(&x, &Tm::Var(z.clone())) else {
            unreachable!()
        };
        assert!(b.instantiate(&Tm::Var(y)).aeq(&Rc::new(Tm::Var(z))));
    }

    #[test]
    fn types_without_variables_pass_through() {
        let a: Name<Ty> = s2n("a");
        let k = Kind::Arr(Rc::new(Kind::Star), Rc::new(Kind::Star));
        assert_eq!(k.subst(&a, &Ty::Int), k);
        let all = Ty::Forall(bind((a.clone(), k), Rc::new(Ty::Var(a))));
        assert!(all.subst(&s2n("b"), &Ty::Int).aeq(&all));
    }
}

mod shared {
    use std::rc::Rc;

    use unbound::prelude::*;

    #[derive(Clone, Debug, Alpha, Subst)]
    enum Expr {
        V(Name<Expr>),
        Lam(Bind<Name<Expr>, Rc<Expr>>),
        App(Rc<Expr>, Rc<Expr>),
    }

    fn lam(v: &Name<Expr>, body: Expr) -> Expr {
        Expr::Lam(bind(v.clone(), Rc::new(body)))
    }

    #[test]
    fn rc_terms_are_alpha_equivalent_and_substitute_without_capture() {
        let x: Name<Expr> = s2n("x");
        let y: Name<Expr> = s2n("y");
        assert!(lam(&x, Expr::V(x.clone())).aeq(&lam(&y, Expr::V(y.clone()))));

        // (\y. x)[x := y] must not become the identity.
        let k = lam(&y, Expr::V(x.clone()));
        let out = k.subst(&x, &Expr::V(y.clone()));
        assert!(!out.aeq(&lam(&y, Expr::V(y.clone()))));
    }

    #[test]
    fn opening_a_shared_body_leaves_other_owners_closed() {
        let x: Name<Expr> = s2n("x");
        let shared = Rc::new(lam(
            &x,
            Expr::App(Rc::new(Expr::V(x.clone())), Rc::new(Expr::V(x.clone()))),
        ));
        let alias = Rc::clone(&shared);
        let Expr::Lam(b) = &*shared else {
            unreachable!()
        };
        let (x2, body) = b.unbind_ref();
        assert!(body.fv().iter().any(|n| *n == x2));
        let Expr::Lam(b) = &*alias else {
            unreachable!()
        };
        assert!(b.body().fv().is_empty());
    }
}
