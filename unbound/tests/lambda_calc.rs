//! Example untyped lambda calculus implementation using unbound

use unbound::prelude::*;

/// Variables stand for expressions
type Var = Name<Expr>;

/// Lambda calculus expressions
#[derive(Clone, Debug, Alpha, Subst)]
enum Expr {
    /// Variables
    V(Var),
    /// Lambda abstractions
    Lam(Bind<Var, Box<Expr>>),
    /// Application
    App(Box<Expr>, Box<Expr>),
}

impl Expr {
    /// Create a variable expression
    fn var(name: Var) -> Expr {
        Expr::V(name)
    }

    /// Create a lambda abstraction
    fn lam(var: Var, body: Expr) -> Expr {
        Expr::Lam(bind(var, Box::new(body)))
    }

    /// Create an application
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

/// Evaluate an expression to normal form
fn eval(expr: Expr) -> FreshM<Expr> {
    match expr {
        Expr::V(x) => FreshM::pure(Expr::V(x)), // Free variables remain as-is
        Expr::Lam(bnd) => {
            // Evaluate under lambdas for normal form
            let (x, body) = bnd.unbind();
            eval(*body).map(move |evaluated_body| Expr::Lam(bind(x, Box::new(evaluated_body))))
        }
        Expr::App(e1, e2) => {
            // Evaluate the function position
            eval(*e1).and_then(move |v1| match v1 {
                Expr::Lam(bnd) => {
                    // Unbind the lambda
                    let (x, body) = bnd.unbind();
                    // First evaluate the argument to normal form
                    eval(*e2).and_then(move |v2| {
                        // Substitute and continue evaluating
                        let substituted = body.subst(&x, &v2);
                        eval(*substituted)
                    })
                }
                // If not a lambda, evaluate the argument and rebuild
                other => eval(*e2).map(move |v2| Expr::app(other, v2)),
            })
        }
    }
}

/// Run evaluation on an expression
fn run_eval(expr: Expr) -> Expr {
    run_fresh(eval(expr))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_identity() {
        // λx. x
        let x = s2n("x");
        let id = Expr::lam(x.clone(), Expr::var(x.clone()));

        // (λx. x) y
        let y = s2n("y");
        let app = Expr::app(id, Expr::var(y.clone()));

        let result = run_eval(app);
        match result {
            Expr::V(v) => assert_eq!(v, y),
            _ => panic!("Expected variable y"),
        }
    }

    #[test]
    fn test_const() {
        // λx. λy. x
        let x = s2n::<Expr>("x");
        let y = s2n::<Expr>("y");
        let const_fn = Expr::lam(x.clone(), Expr::lam(y.clone(), Expr::var(x.clone())));

        // ((λx. λy. x) a) b
        let a = s2n::<Expr>("a");
        let b = s2n::<Expr>("b");
        let app = Expr::app(Expr::app(const_fn, Expr::var(a.clone())), Expr::var(b));

        let result = run_eval(app);
        match result {
            Expr::V(v) => assert_eq!(v, a),
            _ => panic!("Expected variable a"),
        }
    }

    #[test]
    fn test_alpha_equivalence() {
        // λx. x and λy. y should be alpha-equivalent
        let x = s2n::<Expr>("x");
        let y = s2n::<Expr>("y");

        let id1 = Expr::lam(x.clone(), Expr::var(x));
        let id2 = Expr::lam(y.clone(), Expr::var(y));

        assert!(
            id1.aeq(&id2),
            "Identity functions should be alpha-equivalent"
        );
    }

    #[test]
    fn test_church_numerals() {
        // Church encoding: 0 = λf. λx. x
        let f = s2n::<Expr>("f");
        let x = s2n::<Expr>("x");
        let zero = Expr::lam(f.clone(), Expr::lam(x.clone(), Expr::var(x.clone())));

        // Church encoding: 1 = λf. λx. f x
        let one = Expr::lam(
            f.clone(),
            Expr::lam(
                x.clone(),
                Expr::app(Expr::var(f.clone()), Expr::var(x.clone())),
            ),
        );

        // Test that they're not alpha-equivalent
        assert!(!zero.aeq(&one), "0 and 1 should not be alpha-equivalent");

        // Church successor: succ = λn. λf. λx. f (n f x)
        let n = s2n::<Expr>("n");
        let succ = Expr::lam(
            n.clone(),
            Expr::lam(
                f.clone(),
                Expr::lam(
                    x.clone(),
                    Expr::app(
                        Expr::var(f.clone()),
                        Expr::app(
                            Expr::app(Expr::var(n.clone()), Expr::var(f.clone())),
                            Expr::var(x.clone()),
                        ),
                    ),
                ),
            ),
        );

        // Apply successor to zero
        let succ_zero = Expr::app(succ.clone(), zero.clone());
        println!("succ zero (before eval): {}", succ_zero);
        let one_computed = run_eval(succ_zero);
        println!("one_computed (after eval): {}", one_computed);
        println!("expected one: {}", one);

        // They should be alpha-equivalent
        assert!(
            one.aeq(&one_computed),
            "succ 0 should be alpha-equivalent to 1\none: {}\none_computed: {}",
            one,
            one_computed
        );
    }

    #[test]
    fn test_self_application() {
        // The famous Mockingbird combinator: M = λf. f f
        let f = s2n::<Expr>("f");
        let _mockingbird = Expr::lam(f.clone(), Expr::app(Expr::var(f.clone()), Expr::var(f)));

        // Apply identity to itself: (λx. x) (λx. x)
        let x = s2n::<Expr>("x");
        let id = Expr::lam(x.clone(), Expr::var(x));
        let self_app = Expr::app(id.clone(), id.clone());

        let result = run_eval(self_app);
        assert!(
            result.aeq(&id),
            "Identity applied to itself should be identity"
        );
    }

    #[test]
    fn test_substitution() {
        // Test basic substitution
        let x = s2n::<Expr>("x");
        let y = s2n::<Expr>("y");
        let z = s2n::<Expr>("z");

        // Expression: x
        let expr = Expr::var(x.clone());

        // Substitute y for x
        let subst_expr = Expr::var(y.clone());
        let result = expr.subst(&x, &subst_expr);

        match result {
            Expr::V(v) => assert_eq!(v, y),
            _ => panic!("Expected variable y"),
        }

        // Test that substitution doesn't affect unrelated variables
        let expr2 = Expr::var(z.clone());
        let result2 = expr2.subst(&x, &subst_expr);
        match result2 {
            Expr::V(v) => assert_eq!(v, z),
            _ => panic!("Expected variable z"),
        }
    }

    #[test]
    fn test_complex_example() {
        // The example from the Haskell code
        let x = s2n::<Expr>("x");
        let y = s2n::<Expr>("y");

        // e = λx. λy. y x
        let e = Expr::lam(
            x.clone(),
            Expr::lam(y.clone(), Expr::app(Expr::var(y), Expr::var(x))),
        );

        // Evaluate ((e e) e)
        let result = run_eval(Expr::app(Expr::app(e.clone(), e.clone()), e.clone()));

        // The result should be: λy. y (λx. λy. y x)
        // Let's check the structure
        match result {
            Expr::Lam(_) => {
                // It's a lambda, which is expected
                println!("Result: {}", result);
            }
            _ => panic!("Expected a lambda"),
        }
    }
}
