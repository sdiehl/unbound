//! Fresh name generation.
//!
//! Freshness itself is guaranteed by the global name counter, so
//! [`Bind::unbind`] needs no context to be safe. What [`FreshM`] adds is
//! *readable* freshness: it remembers which spellings are already in play
//! within a computation and suffixes new ones, turning a second `x` into
//! `x1` rather than another `x` distinguishable only by index.
//!
//! [`Bind::unbind`]: crate::Bind::unbind

use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

use crate::Name;

/// The spellings handed out so far in a [`FreshM`] computation.
#[derive(Clone, Debug, Default)]
pub struct FreshState {
    used_names: HashSet<String>,
}

impl FreshState {
    /// An empty state.
    pub fn new() -> Self {
        FreshState::default()
    }

    /// Claim a spelling, suffixing it until it is unused.
    fn claim(&mut self, base: &str) -> String {
        let mut candidate = base.to_string();
        let mut suffix = 0;
        while self.used_names.contains(&candidate) {
            suffix += 1;
            candidate = format!("{}{}", base, suffix);
        }
        self.used_names.insert(candidate.clone());
        candidate
    }
}

/// A computation that draws readable fresh names.
pub struct FreshM<T> {
    computation: Box<dyn FnOnce(Rc<RefCell<FreshState>>) -> T>,
}

impl<T> FreshM<T> {
    /// Build a computation from a closure over the naming state.
    pub fn new<F>(f: F) -> Self
    where
        F: FnOnce(Rc<RefCell<FreshState>>) -> T + 'static, {
        FreshM {
            computation: Box::new(f),
        }
    }

    /// A computation that draws no names.
    pub fn pure(value: T) -> Self
    where
        T: 'static, {
        FreshM::new(move |_| value)
    }

    /// Run with a fresh naming state.
    pub fn run_fresh(self) -> T {
        self.run_with_state(Rc::new(RefCell::new(FreshState::new())))
    }

    /// Run with an existing naming state.
    pub fn run_with_state(self, state: Rc<RefCell<FreshState>>) -> T {
        (self.computation)(state)
    }

    /// Map a function over the result.
    pub fn map<U, F>(self, f: F) -> FreshM<U>
    where
        F: FnOnce(T) -> U + 'static,
        T: 'static, {
        FreshM::new(move |state| f(self.run_with_state(state)))
    }

    /// Sequence another computation after this one.
    pub fn flat_map<U, F>(self, f: F) -> FreshM<U>
    where
        F: FnOnce(T) -> FreshM<U> + 'static,
        T: 'static,
        U: 'static, {
        FreshM::new(move |state| {
            let result = self.run_with_state(state.clone());
            f(result).run_with_state(state)
        })
    }

    /// Alias for [`FreshM::flat_map`].
    pub fn and_then<U, F>(self, f: F) -> FreshM<U>
    where
        F: FnOnce(T) -> FreshM<U> + 'static,
        T: 'static,
        U: 'static, {
        self.flat_map(f)
    }
}

/// Values that can be given a fresh, readable variant.
pub trait Fresh {
    /// Produce a fresh variant of this value.
    fn fresh(&self) -> FreshM<Self>
    where
        Self: Sized;
}

impl<T: 'static> Fresh for Name<T> {
    fn fresh(&self) -> FreshM<Self> {
        let base = self.string().unwrap_or("_").to_string();
        FreshM::new(move |state| Name::new(state.borrow_mut().claim(&base)))
    }
}

/// Run a [`FreshM`] computation.
pub fn run_fresh<T>(computation: FreshM<T>) -> T {
    computation.run_fresh()
}
