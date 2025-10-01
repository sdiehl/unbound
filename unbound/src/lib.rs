//! Locally nameless representation for capture-avoiding substitution
//! and alpha equivalence in Rust.
//!
//! This library provides:
//! - **Name types** for representing variables with globally unique identifiers
//! - **Bind types** for representing binding constructs (like lambda
//!   abstractions)
//! - **Automatic alpha equivalence** via the `Alpha` trait (derivable)
//! - **Capture-avoiding substitution** via the `Subst` trait (derivable)
//! - **Fresh name generation** via the `FreshM` monad
//!
//! # Quick Start
//!
//! ```ignore
//! use unbound::prelude::*;
//!
//! #[derive(Clone, Debug, Alpha, Subst)]
//! enum Expr {
//!     Var(Name<Expr>),
//!     Lam(Bind<Name<Expr>, Box<Expr>>),
//!     App(Box<Expr>, Box<Expr>),
//! }
//! ```

// Module declarations
pub mod alpha;
mod bind;
mod fresh;
mod helpers;
mod name;
mod subst;

// Re-export derive macros at the crate root
// Re-export traits
pub use alpha::{Alpha, AlphaCtx};
// Re-export core types
pub use bind::Bind;
// Re-export fresh monad functionality
pub use fresh::{run_fresh, Fresh, FreshM, FreshState};
// Re-export helper functions
pub use helpers::{bind, s2n};
pub use name::Name;
pub use subst::{Subst, SubstName};
pub use unbound_derive::{Alpha, Subst};

/// A prelude module that re-exports commonly used items
pub mod prelude {
    // Re-export derive macros with the same names
    // They can coexist since one is a trait and one is a derive macro
    pub use unbound_derive::{Alpha, Subst};

    pub use crate::{
        // Traits
        alpha::{Alpha, AlphaCtx},
        // Core types
        bind::Bind,
        // Fresh monad
        fresh::{run_fresh, FreshM},
        // Helper functions
        helpers::{bind, s2n},

        name::Name,

        subst::{Subst, SubstName},
    };
}
