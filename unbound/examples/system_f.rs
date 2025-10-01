//! System F with type and term variables
//!
//! This example demonstrates the unbound library with a more complex language
//! that includes both type-level and term-level binding.

use std::fmt;

use unbound::{bind, s2n, Alpha, Bind, FreshM, Name, Subst};

/// Type variable names
type TyName = Name<Ty>;

/// Term variable names
type TmName = Name<Tm>;

/// System F types
#[derive(Clone, Debug, Alpha)]
enum Ty {
    /// Type variables
    TyVar(TyName),
    /// Function types (Arrow)
    Arr(Box<Ty>, Box<Ty>),
    /// Universal quantification
    All(Bind<Vec<TyName>, Box<Ty>>),
}

// Only need to specify when it's a variable
impl Subst<Ty> for Ty {
    fn is_var(&self) -> Option<unbound::SubstName<Ty>> {
        match self {
            Ty::TyVar(v) => Some(unbound::SubstName::Name(v.clone())),
            _ => None,
        }
    }

    fn subst(&self, var: &Name<Ty>, value: &Ty) -> Self {
        match self {
            Ty::TyVar(v) if v == var => value.clone(),
            Ty::TyVar(v) => Ty::TyVar(v.clone()),
            Ty::Arr(t1, t2) => Ty::Arr(
                Box::new((**t1).subst(var, value)),
                Box::new((**t2).subst(var, value)),
            ),
            Ty::All(bnd) => {
                let vars = bnd.pattern();
                if vars.iter().any(|v| v == var) {
                    // Variable is bound, no substitution
                    self.clone()
                } else {
                    Ty::All(Bind::new(
                        vars.clone(),
                        Box::new((**bnd.body()).subst(var, value)),
                    ))
                }
            }
        }
    }
}

// Subst Tm Ty - terms don't substitute into types
impl Subst<Tm> for Ty {
    fn is_var(&self) -> Option<unbound::SubstName<Tm>> {
        None
    }

    fn subst(&self, _var: &Name<Tm>, _value: &Tm) -> Self {
        self.clone()
    }
}

impl Ty {
    fn tyvar(name: TyName) -> Ty {
        Ty::TyVar(name)
    }

    fn arr(t1: Ty, t2: Ty) -> Ty {
        Ty::Arr(Box::new(t1), Box::new(t2))
    }

    fn forall(vars: Vec<TyName>, ty: Ty) -> Ty {
        Ty::All(bind(vars, Box::new(ty)))
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::TyVar(a) => write!(f, "{}", a),
            Ty::Arr(t1, t2) => write!(f, "({} → {})", t1, t2),
            Ty::All(bnd) => {
                let vars = bnd.pattern();
                write!(f, "∀")?;
                for (i, v) in vars.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, ". {}", bnd.body())
            }
        }
    }
}

/// Embedded type annotation
#[derive(Clone, Debug, PartialEq)]
struct Embed<T>(T);

impl<T: Alpha> Alpha for Embed<T> {
    fn aeq(&self, other: &Self) -> bool {
        self.0.aeq(&other.0)
    }

    fn aeq_in(&self, ctx: &mut unbound::alpha::AlphaCtx, other: &Self) -> bool {
        self.0.aeq_in(ctx, &other.0)
    }

    fn fv_in(&self, vars: &mut Vec<String>) {
        self.0.fv_in(vars)
    }
}

impl<T: Subst<V>, V> Subst<V> for Embed<T> {
    fn is_var(&self) -> Option<unbound::SubstName<V>> {
        None
    }

    fn subst(&self, var: &Name<V>, value: &V) -> Self {
        Embed(self.0.subst(var, value))
    }
}

/// System F terms
#[derive(Clone, Debug, Alpha)]
enum Tm {
    /// Term variables
    TmVar(TmName),
    /// Term abstraction (lambda)
    Lam(Bind<(TmName, Embed<Ty>), Box<Tm>>),
    /// Type abstraction (big lambda)
    TLam(Bind<Vec<TyName>, Box<Tm>>),
    /// Term application
    App(Box<Tm>, Box<Tm>),
    /// Type application
    TApp(Box<Tm>, Vec<Ty>),
}

// Only need to specify when it's a variable for Subst<Tm>
impl Subst<Tm> for Tm {
    fn is_var(&self) -> Option<unbound::SubstName<Tm>> {
        match self {
            Tm::TmVar(v) => Some(unbound::SubstName::Name(v.clone())),
            _ => None,
        }
    }

    fn subst(&self, var: &Name<Tm>, value: &Tm) -> Self {
        match self {
            Tm::TmVar(v) if v == var => value.clone(),
            Tm::TmVar(v) => Tm::TmVar(v.clone()),
            Tm::Lam(b) => {
                let (x, ann) = b.pattern();
                if x == var {
                    // Variable is bound, no substitution
                    self.clone()
                } else {
                    // Substitute in body
                    Tm::Lam(Bind::new(
                        (x.clone(), ann.clone()),
                        Box::new((**b.body()).subst(var, value)),
                    ))
                }
            }
            Tm::TLam(b) => {
                // Type variables can't capture term variables, so substitute in body
                Tm::TLam(Bind::new(
                    b.pattern().clone(),
                    Box::new((**b.body()).subst(var, value)),
                ))
            }
            Tm::App(e1, e2) => Tm::App(
                Box::new((**e1).subst(var, value)),
                Box::new((**e2).subst(var, value)),
            ),
            Tm::TApp(t, tys) => Tm::TApp(Box::new((**t).subst(var, value)), tys.clone()),
        }
    }
}

// Subst Ty Tm - substituting types in terms
impl Subst<Ty> for Tm {
    fn is_var(&self) -> Option<unbound::SubstName<Ty>> {
        None // Terms never contain type variables at the term level
    }

    fn subst(&self, var: &Name<Ty>, value: &Ty) -> Self {
        match self {
            Tm::TmVar(x) => Tm::TmVar(x.clone()),
            Tm::Lam(b) => {
                let (x, Embed(ty)) = b.pattern();
                Tm::Lam(Bind::new(
                    (x.clone(), Embed(ty.subst(var, value))),
                    Box::new((**b.body()).subst(var, value)),
                ))
            }
            Tm::TLam(b) => {
                let vars = b.pattern();
                if vars.iter().any(|v| v == var) {
                    // Type variable is bound, no substitution
                    self.clone()
                } else {
                    Tm::TLam(Bind::new(
                        vars.clone(),
                        Box::new((**b.body()).subst(var, value)),
                    ))
                }
            }
            Tm::App(e1, e2) => Tm::App(
                Box::new((**e1).subst(var, value)),
                Box::new((**e2).subst(var, value)),
            ),
            Tm::TApp(t, tys) => Tm::TApp(
                Box::new((**t).subst(var, value)),
                tys.iter().map(|ty| ty.subst(var, value)).collect(),
            ),
        }
    }
}

impl Tm {
    fn var(name: TmName) -> Tm {
        Tm::TmVar(name)
    }

    fn lam(var: TmName, ty: Ty, body: Tm) -> Tm {
        Tm::Lam(bind((var, Embed(ty)), Box::new(body)))
    }

    fn tlam(tyvars: Vec<TyName>, body: Tm) -> Tm {
        Tm::TLam(bind(tyvars, Box::new(body)))
    }

    fn app(t1: Tm, t2: Tm) -> Tm {
        Tm::App(Box::new(t1), Box::new(t2))
    }

    fn tapp(t: Tm, tys: Vec<Ty>) -> Tm {
        Tm::TApp(Box::new(t), tys)
    }
}

impl fmt::Display for Tm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Tm::TmVar(x) => write!(f, "{}", x),
            Tm::Lam(bnd) => {
                let (x, Embed(ty)) = bnd.pattern();
                write!(f, "λ{}:{}. ...", x, ty)
            }
            Tm::TLam(bnd) => {
                let vars = bnd.pattern();
                write!(f, "Λ")?;
                for (i, v) in vars.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, ". ...")
            }
            Tm::App(t1, t2) => write!(f, "({} {})", t1, t2),
            Tm::TApp(t, tys) => {
                write!(f, "({}", t)?;
                for ty in tys {
                    write!(f, " [{}]", ty)?;
                }
                write!(f, ")")
            }
        }
    }
}

/// Type checking context
#[derive(Clone, Debug)]
struct Context {
    /// Type variables in scope
    type_vars: Vec<TyName>,
    /// Term variables with their types
    term_vars: Vec<(TmName, Ty)>,
}

impl Context {
    fn new() -> Self {
        Context {
            type_vars: Vec::new(),
            term_vars: Vec::new(),
        }
    }

    fn extend_type(&self, vars: Vec<TyName>) -> Self {
        let mut ctx = self.clone();
        ctx.type_vars.extend(vars);
        ctx
    }

    fn extend_term(&self, var: TmName, ty: Ty) -> Self {
        let mut ctx = self.clone();
        ctx.term_vars.push((var, ty));
        ctx
    }

    fn lookup_type(&self, var: &TyName) -> bool {
        self.type_vars.iter().any(|v| v == var)
    }

    fn lookup_term(&self, var: &TmName) -> Option<Ty> {
        self.term_vars
            .iter()
            .find(|(v, _)| v == var)
            .map(|(_, ty)| ty.clone())
    }
}

/// Type check a type
fn check_type(ctx: &Context, ty: &Ty) -> Result<(), String> {
    match ty {
        Ty::TyVar(a) => {
            if ctx.lookup_type(a) {
                Ok(())
            } else {
                Err(format!("Type variable {} not in scope", a))
            }
        }
        Ty::Arr(t1, t2) => {
            check_type(ctx, t1)?;
            check_type(ctx, t2)
        }
        Ty::All(bnd) => {
            let (vars, body) = bnd.clone().unbind();
            let new_ctx = ctx.extend_type(vars);
            check_type(&new_ctx, &body)
        }
    }
}

/// Type inference for terms
fn type_infer(ctx: &Context, tm: &Tm) -> FreshM<Result<Ty, String>> {
    match tm {
        Tm::TmVar(x) => FreshM::pure(
            ctx.lookup_term(x)
                .ok_or_else(|| format!("Term variable {} not in scope", x)),
        ),
        Tm::Lam(bnd) => {
            let ((x, Embed(ty)), body) = bnd.clone().unbind();
            if let Err(e) = check_type(ctx, &ty) {
                return FreshM::pure(Err(e));
            }
            let new_ctx = ctx.extend_term(x, ty.clone());
            type_infer(&new_ctx, &body).map(|result| result.map(|body_ty| Ty::arr(ty, body_ty)))
        }
        Tm::TLam(bnd) => {
            let (vars, body) = bnd.clone().unbind();
            let new_ctx = ctx.extend_type(vars.clone());
            type_infer(&new_ctx, &body).map(|result| result.map(|ty| Ty::forall(vars, ty)))
        }
        Tm::App(t1, t2) => {
            let ctx_clone = ctx.clone();
            let t2_clone = t2.clone();
            type_infer(ctx, t1).and_then(move |ty1_result| match ty1_result {
                Ok(Ty::Arr(arg_ty, ret_ty)) => {
                    type_infer(&ctx_clone, &t2_clone).map(move |ty2_result| match ty2_result {
                        Ok(ty2) if arg_ty.aeq(&Box::new(ty2.clone())) => Ok(*ret_ty),
                        Ok(ty2) => Err(format!("Type mismatch: expected {}, got {}", arg_ty, ty2)),
                        Err(e) => Err(e),
                    })
                }
                Ok(ty) => FreshM::pure(Err(format!("Expected function type, got {}", ty))),
                Err(e) => FreshM::pure(Err(e)),
            })
        }
        Tm::TApp(t, tys) => {
            // Check all type arguments are well-formed
            for ty in tys {
                if let Err(e) = check_type(ctx, ty) {
                    return FreshM::pure(Err(e));
                }
            }

            let tys_clone = tys.clone();
            type_infer(ctx, t).map(move |ty_result| match ty_result {
                Ok(Ty::All(bnd)) => {
                    let (vars, body) = bnd.clone().unbind();
                    if vars.len() != tys_clone.len() {
                        Err(format!(
                            "Type application arity mismatch: expected {}, got {}",
                            vars.len(),
                            tys_clone.len()
                        ))
                    } else {
                        // Substitute type arguments
                        let mut result = *body;
                        for (var, ty) in vars.iter().zip(tys_clone.iter()) {
                            result = result.subst(var, ty);
                        }
                        Ok(result)
                    }
                }
                Ok(ty) => Err(format!("Expected polymorphic type, got {}", ty)),
                Err(e) => Err(e),
            })
        }
    }
}

fn main() {
    println!("=== System F Example ===\n");

    // Type and term variables
    let a = s2n::<Ty>("a");
    let b = s2n::<Ty>("b");
    let x = s2n::<Tm>("x");
    let y = s2n::<Tm>("y");

    // 1. Polymorphic identity
    println!("1. Polymorphic Identity");
    println!("-----------------------");

    // Λa. λx:a. x
    let poly_id = Tm::tlam(
        vec![a.clone()],
        Tm::lam(x.clone(), Ty::tyvar(a.clone()), Tm::var(x.clone())),
    );
    println!("poly_id = Λa. λx:a. x");

    // ∀a. a → a
    let poly_id_ty = Ty::forall(
        vec![a.clone()],
        Ty::arr(Ty::tyvar(a.clone()), Ty::tyvar(a.clone())),
    );
    println!("poly_id_ty = {}", poly_id_ty);

    // Type check poly_id
    let ctx = Context::new();
    match unbound::run_fresh(type_infer(&ctx, &poly_id)) {
        Ok(ty) => {
            println!("Type of poly_id: {}", ty);
            assert!(ty.aeq(&poly_id_ty), "Types should be alpha-equivalent");
            println!("✓ Type matches expected!\n");
        }
        Err(e) => println!("Type error: {}\n", e),
    }

    // 2. Polymorphic const function
    println!("2. Polymorphic Const");
    println!("--------------------");

    // Λa. Λb. λx:a. λy:b. x
    let const_tm = Tm::tlam(
        vec![a.clone(), b.clone()],
        Tm::lam(
            x.clone(),
            Ty::tyvar(a.clone()),
            Tm::lam(y.clone(), Ty::tyvar(b.clone()), Tm::var(x.clone())),
        ),
    );
    println!("const = Λa. Λb. λx:a. λy:b. x");

    // ∀a b. a → b → a
    let const_ty = Ty::forall(
        vec![a.clone(), b.clone()],
        Ty::arr(
            Ty::tyvar(a.clone()),
            Ty::arr(Ty::tyvar(b.clone()), Ty::tyvar(a.clone())),
        ),
    );
    println!("const_ty = {}", const_ty);

    match unbound::run_fresh(type_infer(&ctx, &const_tm)) {
        Ok(ty) => {
            println!("Type of const: {}", ty);
            assert!(ty.aeq(&const_ty), "Types should be alpha-equivalent");
            println!("✓ Type matches expected!\n");
        }
        Err(e) => println!("Type error: {}\n", e),
    }

    // 3. Type application
    println!("3. Type Application");
    println!("-------------------");

    // Apply poly_id to type (∀b. b)
    let c = s2n::<Ty>("c");
    let some_poly_ty = Ty::forall(vec![c.clone()], Ty::tyvar(c));
    let applied = Tm::tapp(poly_id.clone(), vec![some_poly_ty.clone()]);

    println!("poly_id [∀c. c] : (∀c. c) → (∀c. c)");
    match unbound::run_fresh(type_infer(&ctx, &applied)) {
        Ok(ty) => {
            println!("Type after application: {}", ty);
            let expected = Ty::arr(some_poly_ty.clone(), some_poly_ty);
            assert!(
                ty.aeq(&expected),
                "Applied type should be (∀c. c) → (∀c. c)"
            );
            println!("✓ Type application works!\n");
        }
        Err(e) => println!("Type error: {}\n", e),
    }

    // 4. Alpha equivalence of polymorphic functions
    println!("4. Alpha Equivalence");
    println!("--------------------");

    let d = s2n::<Ty>("d");
    let z = s2n::<Tm>("z");

    // Λd. λz:d. z (same as poly_id but with different names)
    let poly_id2 = Tm::tlam(
        vec![d.clone()],
        Tm::lam(z.clone(), Ty::tyvar(d.clone()), Tm::var(z.clone())),
    );

    println!("poly_id  = Λa. λx:a. x");
    println!("poly_id2 = Λd. λz:d. z");
    println!("Are they alpha-equivalent? {}", poly_id.aeq(&poly_id2));
    assert!(poly_id.aeq(&poly_id2), "Should be alpha-equivalent");
    println!("✓ Alpha equivalence works for System F!\n");

    // 5. Ill-typed term
    println!("5. Type Checking Errors");
    println!("------------------------");

    // Λa. λy:b. y (where b is not bound)
    let bad_term = Tm::tlam(
        vec![a.clone()],
        Tm::lam(y.clone(), Ty::tyvar(b.clone()), Tm::var(y.clone())),
    );

    println!("bad_term = Λa. λy:b. y (b is unbound)");
    match unbound::run_fresh(type_infer(&ctx, &bad_term)) {
        Ok(_) => println!("Unexpected success!"),
        Err(e) => println!("✓ Correctly caught error: {}", e),
    }
}
