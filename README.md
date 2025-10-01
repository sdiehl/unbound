# Unbound - Locally Nameless Representation in Rust

A Rust for working with locally nameless representations, providing automatic capture-avoiding substitution and alpha equivalence for abstract syntax trees with binding.

This provides a macro API for provides:

- **Name types** for representing variables with globally unique identifiers
- **Bind types** for representing binding constructs (like lambda abstractions)
- **Automatic alpha equivalence** via the `Alpha` trait (derivable)
- **Capture-avoiding substitution** via the `Subst` trait (derivable)
- **Fresh name generation** via the `FreshM` monad

The library provides derive macros for automatic implementation of:

- **`Alpha`**: Alpha equivalence checking that correctly handles binding
- **`Subst`**: Capture-avoiding substitution

## How It Works

The library implements the locally nameless representation where:
- Free variables are represented by names with globally unique indices
- Bound variables use the same representation but are tracked through `Bind` constructs
- Alpha equivalence compares terms modulo renaming of bound variables
- Substitution automatically avoids variable capture
