# Unbound

A Rust library for working with locally nameless representations, providing
automatic capture-avoiding substitution and alpha equivalence for abstract
syntax trees with binding when building functional language typecheckers and
compilers.

The library provides two derivable macros for automatic implementation of:

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

The library implements the locally nameless representation where:

- Free variables are represented by names with globally unique indices
- Bound variables use the same representation but are tracked through `Bind`
  constructs
- Alpha equivalence compares terms modulo renaming of bound variables
- Substitution automatically avoids variable capture

## License

MIT Licensed. Copyright 2025 Stephen Diehl.
