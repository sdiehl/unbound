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

## License

MIT Licensed. Copyright 2025-2026 Stephen Diehl.

