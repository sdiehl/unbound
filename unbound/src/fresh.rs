//! Fresh name generation monad

use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

use crate::Name;

/// State for fresh name generation
#[derive(Clone, Debug)]
pub struct FreshState {
    counter: usize,
    used_names: HashSet<String>,
}

impl FreshState {
    pub fn new() -> Self {
        FreshState {
            counter: 0,
            used_names: HashSet::new(),
        }
    }
}

impl Default for FreshState {
    fn default() -> Self {
        Self::new()
    }
}

/// The Fresh monad for generating fresh names
pub struct FreshM<T> {
    computation: Box<dyn FnOnce(Rc<RefCell<FreshState>>) -> T>,
}

impl<T> FreshM<T> {
    /// Create a new FreshM computation
    pub fn new<F>(f: F) -> Self
    where
        F: FnOnce(Rc<RefCell<FreshState>>) -> T + 'static, {
        FreshM {
            computation: Box::new(f),
        }
    }

    /// Return a pure value
    pub fn pure(value: T) -> Self
    where
        T: 'static, {
        FreshM::new(move |_| value)
    }

    /// Run the fresh computation with a new state
    pub fn run_fresh(self) -> T {
        let state = Rc::new(RefCell::new(FreshState::new()));
        (self.computation)(state)
    }

    /// Run with an existing state
    pub fn run_with_state(self, state: Rc<RefCell<FreshState>>) -> T {
        (self.computation)(state)
    }

    /// Map a function over the result
    pub fn map<U, F>(self, f: F) -> FreshM<U>
    where
        F: FnOnce(T) -> U + 'static,
        T: 'static, {
        FreshM::new(move |state| {
            let result = self.run_with_state(state.clone());
            f(result)
        })
    }

    /// Monadic bind operation
    pub fn flat_map<U, F>(self, f: F) -> FreshM<U>
    where
        F: FnOnce(T) -> FreshM<U> + 'static,
        T: 'static,
        U: 'static, {
        FreshM::new(move |state| {
            let result = self.run_with_state(state.clone());
            let next = f(result);
            next.run_with_state(state)
        })
    }

    /// Alias for flat_map
    pub fn and_then<U, F>(self, f: F) -> FreshM<U>
    where
        F: FnOnce(T) -> FreshM<U> + 'static,
        T: 'static,
        U: 'static, {
        self.flat_map(f)
    }
}

/// Trait for types that can provide fresh names
pub trait Fresh {
    /// Generate a fresh variant of this value
    fn fresh(&self) -> FreshM<Self>
    where
        Self: Sized;
}

impl<T> Fresh for Name<T> {
    fn fresh(&self) -> FreshM<Self> {
        let base = self.string().to_string();
        FreshM::new(move |state| {
            let mut st = state.borrow_mut();
            let mut candidate = base.clone();
            let mut suffix = 0;

            // Find a unique name
            while st.used_names.contains(&candidate) {
                suffix += 1;
                candidate = format!("{}{}", base, suffix);
            }

            st.used_names.insert(candidate.clone());
            st.counter += 1;

            Name::with_index(candidate, st.counter - 1)
        })
    }
}

/// Helper to run a FreshM computation
pub fn run_fresh<T>(computation: FreshM<T>) -> T {
    computation.run_fresh()
}
