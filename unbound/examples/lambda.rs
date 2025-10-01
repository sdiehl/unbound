//! Example: Untyped lambda calculus with the unbound library

use unbound::{bind, run_fresh, s2n, Alpha, Bind, Fresh, FreshM, Name, Subst};

// Variables are names that stand for expressions
type Var = Name<Expr>;

// Lambda calculus expressions
#[derive(Clone, Debug, Alpha, Subst)]
enum Expr {
    V(Var),                    // Variables
    Lam(Bind<Var, Box<Expr>>), // Lambda abstractions
    App(Box<Expr>, Box<Expr>), // Applications
}

impl Expr {
    fn var(name: Var) -> Expr {
        Expr::V(name)
    }

    fn lam(var: Var, body: Expr) -> Expr {
        Expr::Lam(bind(var, Box::new(body)))
    }

    fn app(e1: Expr, e2: Expr) -> Expr {
        Expr::App(Box::new(e1), Box::new(e2))
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::V(x) => write!(f, "{}", x),
            Expr::Lam(bnd) => write!(f, "λ{}", bnd),
            Expr::App(e1, e2) => write!(f, "({} {})", e1, e2),
        }
    }
}

// Evaluate to normal form
fn eval(expr: Expr) -> FreshM<Expr> {
    match expr {
        Expr::V(x) => FreshM::pure(Expr::V(x)),
        Expr::Lam(bnd) => {
            let (x, body) = bnd.unbind();
            x.fresh().and_then(move |fresh_x| {
                let body_subst = body.subst(&x, &Expr::V(fresh_x.clone()));
                eval(*body_subst)
                    .map(move |evaluated_body| Expr::Lam(bind(fresh_x, Box::new(evaluated_body))))
            })
        }
        Expr::App(e1, e2) => eval(*e1.clone()).and_then(move |v1| {
            eval(*e2.clone()).and_then(move |v2| match v1 {
                Expr::Lam(bnd) => {
                    let (x, body) = bnd.unbind();
                    let substituted = body.subst(&x, &v2);
                    eval(*substituted)
                }
                other => FreshM::pure(Expr::app(other, v2)),
            })
        }),
    }
}

fn main() {
    println!("=== Unbound Lambda Calculus Demo ===\n");

    // 1. Alpha equivalence
    println!("1. Alpha Equivalence");
    println!("--------------------");
    let x = s2n("x");
    let y = s2n("y");
    let id_x = Expr::lam(x.clone(), Expr::var(x));
    let id_y = Expr::lam(y.clone(), Expr::var(y));

    println!("λx.x = {}", id_x);
    println!("λy.y = {}", id_y);
    println!("Are they alpha-equivalent? {}\n", id_x.aeq(&id_y));

    // 2. Substitution
    println!("2. Capture-Avoiding Substitution");
    println!("---------------------------------");
    let z = s2n("z");
    let expr = Expr::app(id_x.clone(), Expr::var(z.clone()));

    println!("Expression: (λx.x) z = {}", expr);
    let result = run_fresh(eval(expr));
    println!("After evaluation: {}\n", result);

    // 3. Church Booleans
    println!("3. Church Booleans");
    println!("------------------");
    let t = s2n::<Expr>("t");
    let f = s2n::<Expr>("f");

    let true_church = Expr::lam(t.clone(), Expr::lam(f.clone(), Expr::var(t.clone())));
    let false_church = Expr::lam(t.clone(), Expr::lam(f.clone(), Expr::var(f.clone())));

    println!("TRUE  = λt.λf.t = {}", true_church);
    println!("FALSE = λt.λf.f = {}", false_church);
    println!(
        "Are they alpha-equivalent? {}\n",
        true_church.aeq(&false_church)
    );

    // 4. Church Numerals
    println!("4. Church Numerals");
    println!("------------------");
    let f = s2n::<Expr>("f");
    let x = s2n::<Expr>("x");

    let zero = Expr::lam(f.clone(), Expr::lam(x.clone(), Expr::var(x.clone())));
    let one = Expr::lam(
        f.clone(),
        Expr::lam(
            x.clone(),
            Expr::app(Expr::var(f.clone()), Expr::var(x.clone())),
        ),
    );
    let two = Expr::lam(
        f.clone(),
        Expr::lam(
            x.clone(),
            Expr::app(
                Expr::var(f.clone()),
                Expr::app(Expr::var(f.clone()), Expr::var(x)),
            ),
        ),
    );

    println!("ZERO = λf.λx.x     = {}", zero);
    println!("ONE  = λf.λx.f x   = {}", one);
    println!("TWO  = λf.λx.f(f x) = {}", two);
    println!("Is ZERO ≡ ONE? {}", zero.aeq(&one));
    println!("Is ZERO ≡ TWO? {}", zero.aeq(&two));
    println!("Is ONE ≡ TWO? {}\n", one.aeq(&two));

    // 5. Combinator Calculus
    println!("5. Combinators");
    println!("--------------");

    // S combinator: λf.λg.λx.f x (g x)
    let f = s2n::<Expr>("f");
    let g = s2n::<Expr>("g");
    let x = s2n::<Expr>("x");

    let s_combinator = Expr::lam(
        f.clone(),
        Expr::lam(
            g.clone(),
            Expr::lam(
                x.clone(),
                Expr::app(
                    Expr::app(Expr::var(f.clone()), Expr::var(x.clone())),
                    Expr::app(Expr::var(g), Expr::var(x.clone())),
                ),
            ),
        ),
    );

    // K combinator: λx.λy.x
    let k_x = s2n::<Expr>("x");
    let k_y = s2n::<Expr>("y");
    let k_combinator = Expr::lam(k_x.clone(), Expr::lam(k_y, Expr::var(k_x)));

    // I combinator can be derived from S and K: S K K
    let i_derived = Expr::app(
        Expr::app(s_combinator.clone(), k_combinator.clone()),
        k_combinator.clone(),
    );

    println!("S = λf.λg.λx.f x (g x) = {}", s_combinator);
    println!("K = λx.λy.x = {}", k_combinator);
    println!("I = S K K = {}", i_derived);

    // Test that S K K behaves like identity
    let test = s2n::<Expr>("test");
    let test_app = Expr::app(i_derived, Expr::var(test.clone()));
    let result = run_fresh(eval(test_app));
    println!("(S K K) test = {}", result);

    match result {
        Expr::V(v) if v == test => println!("✓ S K K behaves like identity!"),
        _ => println!("✗ S K K does not behave like identity"),
    }
}
