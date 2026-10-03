use std::collections::HashSet;
use unbound::prelude::*;
use unbound::{InstantiateCtx, SubstCtx};

#[derive(Clone, Debug, Alpha, Subst)]
enum Expr {
    Var(Name<Expr>),
    Atom(u32),
    App(Shared<Expr>, Shared<Expr>),
    Lam(Bind<Name<Expr>, Shared<Expr>>),
    Ann(Bind<(Name<Expr>, Shared<Expr>), Shared<Expr>>),
    Many(Bind<Vec<Name<Expr>>, Shared<Expr>>),
}

fn node(e: Expr) -> Shared<Expr> {
    Shared::new(e)
}

#[test]
fn cached_support_exposes_loose_coordinates_and_unknown_state() {
    let term = node(Expr::App(
        node(Expr::Var(Name::bound(2, 1))),
        node(Expr::Var(Name::bound(0, 0))),
    ));
    let support = term.support_ref();
    assert!(support.is_known());
    assert!(support.has_loose_bound_vars());
    assert_eq!(support.max_loose_level(), Some(2));
    assert_eq!(
        support.bound_coordinates().collect::<Vec<_>>(),
        vec![(0, 0), (2, 1)]
    );
    assert!(std::ptr::eq(support, term.support_ref()));
    let under = term.support().under_binder();
    assert_eq!(under.bound_coordinates().collect::<Vec<_>>(), vec![(1, 1)]);
    let empty = Support::default();
    assert!(empty.is_known());
    assert!(!empty.has_loose_bound_vars());
    assert_eq!(empty.max_loose_level(), None);
    let unknown = Support::unknown();
    assert!(!unknown.is_known());
    assert!(unknown.has_loose_bound_vars());
}
fn dag(depth: usize, leaf: Shared<Expr>) -> Shared<Expr> {
    (0..depth).fold(leaf, |e, _| node(Expr::App(e.clone(), e)))
}
fn count(e: &Shared<Expr>) -> usize {
    let mut seen = HashSet::new();
    let mut stack = vec![e];
    while let Some(e) = stack.pop() {
        if !seen.insert(e.as_ptr()) {
            continue;
        }
        match &**e {
            Expr::App(a, b) => stack.extend([a, b]),
            Expr::Lam(b) => stack.push(b.body()),
            Expr::Ann(b) => stack.extend([&b.pattern().1, b.body()]),
            Expr::Many(b) => stack.push(b.body()),
            _ => {}
        }
    }
    seen.len()
}

#[test]
fn unused_binders_and_substitutions_preserve_identity() {
    let x = Name::new("x");
    let input = dag(60, node(Expr::Atom(0)));
    let binder = bind(x.clone(), input.clone());
    assert!(binder.body().ptr_eq(&input));
    assert!(binder.unbind_ref().1.ptr_eq(&input));
    assert!(binder.instantiate(&Expr::Atom(1)).ptr_eq(&input));
    assert!(input.subst(&x, &Expr::Atom(2)).ptr_eq(&input));
    assert_eq!(count(&input), 61);
}

#[test]
fn affected_nodes_remain_shared_and_aliases_are_immutable() {
    let x = Name::new("x");
    let input = dag(60, node(Expr::Var(x.clone())));
    let binder = bind(x.clone(), input.clone());
    let (fresh, opened) = binder.unbind_ref();
    for e in [
        binder.body(),
        &opened,
        &binder.instantiate(&Expr::Atom(9)),
        &input.subst(&x, &Expr::Atom(9)),
    ] {
        assert_eq!(count(e), 61);
    }
    assert_eq!(input.fv(), vec![x.to_any().unwrap()]);
    assert_eq!(opened.fv(), vec![fresh.to_any().unwrap()]);
    assert!(binder.body().fv().is_empty());
    assert!(binder
        .instantiate(&Expr::Atom(9))
        .aeq(&dag(60, node(Expr::Atom(9)))));
}

#[test]
fn equality_handles_independently_allocated_dags() {
    assert!(dag(60, node(Expr::Atom(0))).aeq(&dag(60, node(Expr::Atom(0)))));
    assert!(!dag(60, node(Expr::Atom(0))).aeq(&dag(60, node(Expr::Atom(1)))));
}

#[test]
fn close_cache_distinguishes_binding_depth() {
    let x = Name::new("x");
    let leaf = node(Expr::Var(x.clone()));
    let nested = node(Expr::Lam(bind(Name::new("y"), leaf.clone())));
    let b = bind(x.clone(), node(Expr::App(leaf.clone(), nested)));
    let Expr::App(direct, nested) = &**b.body() else {
        panic!()
    };
    let Expr::Var(direct) = &**direct else {
        panic!()
    };
    let Expr::Lam(nested) = &**nested else {
        panic!()
    };
    let Expr::Var(nested) = &**nested.body() else {
        panic!()
    };
    assert_eq!(direct.coordinates(), Some((0, 0)));
    assert_eq!(nested.coordinates(), Some((1, 0)));
    assert_eq!(leaf.fv(), vec![x.to_any().unwrap()]);
}

#[test]
fn shared_open_cache_distinguishes_depth_and_name_mapping() {
    let raw = node(Expr::Var(Name::bound(1, 0)));
    let x: Name<Expr> = Name::new("x");
    let y: Name<Expr> = Name::new("y");
    let mut ctx = AlphaCtx::default();
    let mut at_zero = raw.clone();
    let mut at_one = raw.clone();
    let mut different_name = raw.clone();
    at_zero.open_with(0, &[x.to_any().unwrap()], &mut ctx);
    at_one.open_with(1, &[x.to_any().unwrap()], &mut ctx);
    different_name.open_with(1, &[y.to_any().unwrap()], &mut ctx);
    assert!(at_zero.ptr_eq(&raw));
    assert!(at_one.aeq(&node(Expr::Var(x))));
    assert!(different_name.aeq(&node(Expr::Var(y))));
}

#[test]
fn close_cache_distinguishes_name_mapping_and_operation() {
    let x: Name<Expr> = Name::new("x");
    let y: Name<Expr> = Name::new("y");
    let raw = node(Expr::Var(x.clone()));
    let mut a = raw.clone();
    let mut b = raw.clone();
    let mut ctx = AlphaCtx::default();
    a.close_with(0, &[x.to_any().unwrap(), y.to_any().unwrap()], &mut ctx);
    b.close_with(0, &[y.to_any().unwrap(), x.to_any().unwrap()], &mut ctx);
    assert!(a.aeq(&node(Expr::Var(Name::bound(0, 0)))));
    assert!(b.aeq(&node(Expr::Var(Name::bound(0, 1)))));
    a.open_with(0, &[x.to_any().unwrap()], &mut ctx);
    assert!(a.aeq(&raw));
}

#[test]
fn substitution_values_do_not_leak_across_operations() {
    let x = Name::new("x");
    let term = dag(40, node(Expr::Var(x.clone())));
    assert!(term
        .subst(&x, &Expr::Atom(1))
        .aeq(&dag(40, node(Expr::Atom(1)))));
    assert!(term
        .subst(&x, &Expr::Atom(2))
        .aeq(&dag(40, node(Expr::Atom(2)))));
    let a = Expr::Atom(3);
    let mut ctx = SubstCtx::new(&x, &a);
    assert!(term.subst_with(&mut ctx).ptr_eq(&term.subst_with(&mut ctx)));
}

#[test]
fn direct_instantiation_respects_binder_depth_and_capture() {
    let x = Name::new("x");
    let y = Name::new("y");
    let body = node(Expr::Lam(bind(
        y.clone(),
        node(Expr::App(
            node(Expr::Var(x.clone())),
            node(Expr::Var(y.clone())),
        )),
    )));
    let b = bind(x, body);
    let result = b.instantiate(&Expr::Var(y.clone()));
    let Expr::Lam(result) = &*result else {
        panic!()
    };
    let (fresh, body) = result.unbind_ref();
    assert!(body.aeq(&node(Expr::App(node(Expr::Var(y)), node(Expr::Var(fresh))))));
}

#[test]
fn instantiate_context_keeps_depth_in_its_key() {
    let raw = node(Expr::Var(Name::bound(1, 0)));
    let values = [Expr::Atom(7)];
    let mut ctx = InstantiateCtx::new(&values);
    assert!(raw.instantiate_with(0, &mut ctx).unwrap().ptr_eq(&raw));
    assert!(raw
        .instantiate_with(1, &mut ctx)
        .unwrap()
        .aeq(&node(Expr::Atom(7))));
}

#[test]
fn simultaneous_instantiation_is_not_sequential_substitution() {
    let x = Name::new("x");
    let y = Name::new("y");
    let b = bind(
        vec![x.clone(), y.clone()],
        node(Expr::App(
            node(Expr::Var(x.clone())),
            node(Expr::Var(y.clone())),
        )),
    );
    assert!(b
        .instantiate_all(&[Expr::Var(y.clone()), Expr::Var(x.clone())])
        .aeq(&node(Expr::App(node(Expr::Var(y)), node(Expr::Var(x))))));
}

#[test]
fn annotated_binders_keep_annotations_outside_scope() {
    let x = Name::new("x");
    let v = node(Expr::Var(x.clone()));
    let ann = node(Expr::Ann(bind((x.clone(), v.clone()), v)));
    assert_eq!(ann.fv(), vec![x.to_any().unwrap()]);
    let outer = bind(x, ann);
    let out = outer.instantiate(&Expr::Atom(42));
    let Expr::Ann(b) = &*out else { panic!() };
    assert!(b.pattern().1.aeq(&node(Expr::Atom(42))));
    assert!(b.instantiate(&Expr::Atom(7)).aeq(&node(Expr::Atom(7))));
}

#[test]
fn vector_patterns_and_free_name_order() {
    let x = Name::new("x");
    let y = Name::new("y");
    let e = node(Expr::App(
        node(Expr::Var(y.clone())),
        node(Expr::Var(x.clone())),
    ));
    assert_eq!(e.fv(), vec![y.to_any().unwrap(), x.to_any().unwrap()]);
    let many = node(Expr::Many(bind(vec![x, y], e)));
    assert!(many.fv().is_empty());
}

#[test]
fn empty_and_partial_opening_do_not_lose_unmatched_coordinates() {
    let b = node(Expr::Var(Name::bound(0, 1)));
    let x: Name<Expr> = Name::new("x");
    let mut a = b.clone();
    a.open(0, &[]);
    a.open(0, &[x.to_any().unwrap()]);
    assert!(a.ptr_eq(&b));
}

#[derive(Clone)]
struct Manual(Expr);
impl Alpha for Manual {
    fn aeq(&self, other: &Self) -> bool {
        self.0.aeq(&other.0)
    }
    fn close(&mut self, depth: usize, names: &[AnyName]) {
        self.0.close(depth, names);
    }
    fn open(&mut self, depth: usize, names: &[AnyName]) {
        self.0.open(depth, names);
    }
    fn fv_in(&self, acc: &mut Vec<AnyName>) {
        self.0.fv_in(acc);
    }
}
impl Subst<Expr> for Manual {
    fn is_var(&self) -> Option<SubstName<Expr>> {
        None
    }
    fn subst(&self, var: &Name<Expr>, value: &Expr) -> Self {
        Self(self.0.subst(var, value))
    }
}

#[test]
fn manual_implementations_use_conservative_metadata_and_fallback() {
    let x = Name::new("x");
    let body = Shared::new(Manual(Expr::Var(x.clone())));
    let b = bind(x, body);
    assert!(b.body().fv().is_empty());
    let out = b.instantiate(&Expr::Atom(9));
    assert!(out.0.aeq(&Expr::Atom(9)));
}

#[test]
fn direct_instantiation_matches_open_then_substitute() {
    fn generate(seed: &mut u64, depth: usize, vars: &[Name<Expr>]) -> Shared<Expr> {
        *seed = seed.wrapping_mul(6364136223846793005).wrapping_add(1);
        let choice = (*seed >> 32) as usize;
        if depth == 0 {
            return node(Expr::Var(vars[choice % vars.len()].clone()));
        }
        match choice % 5 {
            0 => node(Expr::Atom(choice as u32)),
            1 => {
                let t = generate(seed, depth - 1, vars);
                node(Expr::App(t.clone(), t))
            }
            2 => node(Expr::App(
                generate(seed, depth - 1, vars),
                generate(seed, depth - 1, vars),
            )),
            _ => {
                let x = Name::new("x");
                let mut scope = vars.to_vec();
                scope.push(x.clone());
                let body = generate(seed, depth - 1, &scope);
                if choice % 5 == 3 {
                    node(Expr::Lam(bind(x, body)))
                } else {
                    node(Expr::Ann(bind((x, generate(seed, depth - 1, vars)), body)))
                }
            }
        }
    }
    let mut seed = 17;
    for _ in 0..300 {
        let vars: Vec<Name<Expr>> = (0..4).map(|_| Name::new("x")).collect();
        let body = generate(&mut seed, 5, &vars);
        let b = bind(vars[..2].to_vec(), body);
        let values = [
            (*generate(&mut seed, 3, &vars)).clone(),
            (*generate(&mut seed, 3, &vars)).clone(),
        ];
        let direct = b.instantiate_all(&values);
        let (fresh, mut old) = b.unbind_ref();
        for (n, v) in fresh.iter().zip(&values) {
            old = old.subst(n, v);
        }
        assert!(direct.aeq(&old));
    }
}

fn cons(e: Expr) -> Shared<Expr> {
    Shared::intern(e)
}
fn cons_dag(depth: usize, leaf: Shared<Expr>) -> Shared<Expr> {
    (0..depth).fold(leaf, |e, _| cons(Expr::App(e.clone(), e)))
}
fn ident(x: &str) -> Shared<Expr> {
    let x = Name::new(x);
    cons(Expr::Lam(bind(x.clone(), cons(Expr::Var(x)))))
}

#[test]
fn interned_alpha_variants_are_one_node() {
    assert!(ident("x").ptr_eq(&ident("y")));
    assert!(cons_dag(60, cons(Expr::Atom(0))).ptr_eq(&cons_dag(60, cons(Expr::Atom(0)))));
    let x = Name::new("x");
    let konst = cons(Expr::Lam(bind(Name::new("y"), cons(Expr::Var(x)))));
    assert!(!konst.ptr_eq(&ident("y")));
    assert!(!konst.aeq(&ident("y")));
}

#[test]
fn operations_on_interned_nodes_stay_interned() {
    let x = Name::new("x");
    let input = cons_dag(40, cons(Expr::Var(x.clone())));
    let target = cons_dag(40, cons(Expr::Atom(9)));
    let replaced = input.subst(&x, &Expr::Atom(9));
    assert!(replaced.is_interned());
    assert!(replaced.ptr_eq(&target));
    let b = bind(x, input.clone());
    assert!(b.body().is_interned());
    assert!(b.instantiate(&Expr::Atom(9)).ptr_eq(&target));
    let (_, opened) = b.unbind_ref();
    assert!(opened.is_interned());
    assert_eq!(count(&opened), 41);
}

#[test]
fn mixed_interned_and_plain_nodes_compare_structurally() {
    let plain = dag(30, node(Expr::Atom(1)));
    let interned = cons_dag(30, cons(Expr::Atom(1)));
    assert!(plain.aeq(&interned));
    assert!(!plain.aeq(&cons_dag(30, cons(Expr::Atom(2)))));
}

#[test]
fn alpha_hash_agrees_with_aeq() {
    let x = Name::new("x");
    let y = Name::new("y");
    let pairs = [
        (
            node(Expr::Lam(bind(x.clone(), node(Expr::Var(x.clone()))))),
            node(Expr::Lam(bind(y.clone(), node(Expr::Var(y.clone()))))),
        ),
        (
            node(Expr::Many(bind(
                vec![x.clone(), y.clone()],
                node(Expr::Var(y.clone())),
            ))),
            node(Expr::Many(bind(
                vec![y.clone(), x.clone()],
                node(Expr::Var(x.clone())),
            ))),
        ),
        (
            node(Expr::Ann(bind(
                (x.clone(), node(Expr::Atom(3))),
                node(Expr::Var(x.clone())),
            ))),
            node(Expr::Ann(bind(
                (y.clone(), node(Expr::Atom(3))),
                node(Expr::Var(y.clone())),
            ))),
        ),
    ];
    for (a, b) in &pairs {
        assert!(a.aeq(b));
        assert_eq!(a.alpha_hash(), b.alpha_hash());
    }
    assert_ne!(
        node(Expr::Var(x.clone())).alpha_hash(),
        node(Expr::Var(y)).alpha_hash()
    );
}

#[test]
fn dropped_interned_nodes_are_reclaimed() {
    let keep = cons(Expr::Atom(u32::MAX));
    for i in 0..10_000 {
        drop(cons(Expr::Atom(i)));
    }
    let again = cons(Expr::Atom(u32::MAX));
    assert!(again.ptr_eq(&keep));
    assert!(cons(Expr::Atom(5)).aeq(&node(Expr::Atom(5))));
}
