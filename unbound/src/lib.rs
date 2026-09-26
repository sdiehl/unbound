//! Locally nameless representation for capture-avoiding substitution
//! and alpha equivalence in Rust.
//!
//! Free variables are names carrying a globally unique index; variables
//! bound by an enclosing binder are de Bruijn coordinates, installed when
//! [`Bind::new`] closes over a pattern and removed again by
//! [`Bind::unbind`]. Because a bound variable has no name, alpha
//! equivalence is structural equality and substitution cannot capture.
//!
//! - **Name types** for representing variables ([`Name`], [`AnyName`])
//! - **Bind types** for representing binding constructs ([`Bind`])
//! - **Automatic alpha equivalence** via the [`Alpha`] trait (derivable)
//! - **Capture-avoiding substitution** via the [`Subst`] trait (derivable)
//! - **Readable fresh names** via the [`FreshM`] context
//! - **Printing without capture** via [`NameScope`]
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
//!
//! let x: Name<Expr> = s2n("x");
//! let y: Name<Expr> = s2n("y");
//!
//! // \x. x and \y. y are the same function.
//! let id_x = Expr::Lam(bind(x.clone(), Box::new(Expr::Var(x.clone()))));
//! let id_y = Expr::Lam(bind(y.clone(), Box::new(Expr::Var(y.clone()))));
//! assert!(id_x.aeq(&id_y));
//!
//! // Substituting y into \y. x does not capture.
//! let lam = Expr::Lam(bind(y.clone(), Box::new(Expr::Var(x.clone()))));
//! assert!(!lam.subst(&x, &Expr::Var(y.clone())).aeq(&id_y));
//! ```

pub mod alpha;
mod bind;
mod fresh;
mod helpers;
mod name;
mod scope;
mod subst;

pub use alpha::{Alpha, Pattern};
pub use bind::Bind;
pub use fresh::{run_fresh, Fresh, FreshM, FreshState};
pub use helpers::{bind, s2n};
pub use name::{AnyName, Name};
pub use scope::NameScope;
pub use subst::{Subst, SubstName};
pub use unbound_derive::{Alpha, Subst};

/// Commonly used items.
pub mod prelude {
    pub use unbound_derive::{Alpha, Subst};

    pub use crate::alpha::{Alpha, Pattern};
    pub use crate::bind::Bind;
    pub use crate::fresh::{run_fresh, Fresh, FreshM};
    pub use crate::helpers::{bind, s2n};
    pub use crate::name::{AnyName, Name};
    pub use crate::scope::NameScope;
    pub use crate::subst::{Subst, SubstName};
}
