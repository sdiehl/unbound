# Unbound

A Rust library for working with locally nameless representations, providing
automatic capture-avoiding substitution and alpha equivalence for abstract
syntax trees with binding when building functional language typecheckers and
compilers.

Provides two derivable macros for automatic implementation of:

- **`Alpha`**: Automatically derived alpha equivalence checking that correctly
  handles binding
- **`Subst`**: Automatically derived capture-avoiding substitution


```rust
use unbound::prelude::*;

#[derive(Clone, Debug, Alpha, Subst)]
enum Expr {
    Var(Name<Expr>),
    Lam(Bind<Name<Expr>, Box<Expr>>),
    App(Box<Expr>, Box<Expr>),
}
```

## How It Works

### Phantom Types

The `Name<T>` type uses phantom type parameters to ensure type safety at the binding level. The phantom parameter `T` represents the type of AST that can contain this name as a variable:

```rust
pub struct Name<T> {
    string: String,      // Human-readable identifier
    index: usize,        // Globally unique index via AtomicUsize
    _phantom: PhantomData<T>,
}
```

This prevents mixing names from different AST types - a `Name<Expr>` cannot be confused with a `Name<Type>` at compile time, despite having identical runtime representations. The phantom type has zero runtime cost while providing complete type safety.

### Locally Nameless Representation

Names are compared by their globally unique index, not their string representation. This makes alpha-equivalence trivial for free variables (same index = same variable) while the `Bind<P, T>` type handles the complexity of bound variables:

- During `Alpha` checking, bound names are pushed onto a context stack with their corresponding names from the compared term
- The `AlphaCtx` maintains a bijection between names in the two terms being compared
- When encountering a variable, we check if it's bound (exists in context) or free (compare indices directly)

### Capture-Avoiding Substitution

The `Subst` trait's key insight is the `is_var` method:

```rust
trait Subst<V> {
    fn is_var(&self) -> Option<SubstName<V>>;
    fn subst(&self, var: &Name<V>, value: &V) -> Self;
}
```

The derive macro generates `subst` implementations that:

1. Check if the current term `is_var` matching the substitution target
2. Traverse the AST, tracking which names are bound by `Bind` constructs
3. Skip substitution under binders that capture the variable being substituted
4. Recursively apply substitution to subterms

### Fresh Name Generation

The `FreshM<T>` context threads a `FreshState` containing a counter and a map of "hints" for generating human-readable names. It's implemented as a closure that takes the state:

```rust
pub struct FreshM<T> {
    computation: Box<dyn FnOnce(Rc<RefCell<FreshState>>) -> T>,
}
```

This allows `unbind` operations to generate fresh names when opening binders, ensuring no accidental capture during substitution operations.

## License

MIT Licensed. Copyright 2025 Stephen Diehl.
