//! Locally nameless representation for capture-avoiding substitution
//! and alpha equivalence in Rust.
//!
//! - **Name types** for representing variables with globally unique identifiers
//! - **Bind types** for representing binding constructs (like lambda
//!   abstractions)
//! - **Automatic alpha equivalence** via the `Alpha` trait (derivable)
//! - **Capture-avoiding substitution** via the `Subst` trait (derivable)
//! - **Fresh name generation** via the `FreshM` monad
//!
//! # Quick Start
//!
//! ```
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

pub use alpha::{Alpha, AlphaCtx};
pub use bind::Bind;
pub use fresh::{run_fresh, Fresh, FreshM, FreshState};
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
