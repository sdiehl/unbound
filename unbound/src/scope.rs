//! Display names for printing.
//!
//! Opening a binder yields a name with a fresh index but a possibly reused
//! spelling, so printing spellings naively can show `\x. \x. x` for a term
//! whose body refers to the outer `x`. A [`NameScope`] tracks the spelling
//! chosen for every name in scope and renames a binder only when its
//! spelling would capture a name the body actually uses, so the printed text
//! reads back as the same term.

use crate::{AnyName, Name};

/// The spellings of the names in scope while printing a term.
#[derive(Clone, Debug, Default)]
pub struct NameScope {
    scope: Vec<(Option<usize>, String)>,
}

impl NameScope {
    /// A scope holding the free names of the term about to be printed, with
    /// suffixes where two distinct names share a spelling.
    pub fn new(free: &[AnyName]) -> Self {
        let mut scope = NameScope::default();
        for n in free {
            let display = scope.spelling(n.string(), free);
            scope.scope.push((Some(n.index()), display));
        }
        scope
    }

    /// A spelling for binder `n` that captures nothing in `body_fv`, the
    /// free names of the binder's body.
    pub fn pick<T>(&self, n: &Name<T>, body_fv: &[AnyName]) -> String {
        self.spelling(n.string().unwrap_or("_"), body_fv)
    }

    /// Bring `n` into scope under `display`.
    pub fn push<T>(&mut self, n: &Name<T>, display: String) {
        self.scope.push((n.index(), display));
    }

    /// [`pick`](NameScope::pick) a spelling for `n` and bring it into scope.
    pub fn bind<T>(&mut self, n: &Name<T>, body_fv: &[AnyName]) -> String {
        let display = self.pick(n, body_fv);
        self.push(n, display.clone());
        display
    }

    /// Leave the innermost scope.
    pub fn pop(&mut self) {
        self.scope.pop();
    }

    /// The spelling to print for an occurrence of `n`.
    pub fn get<'a, T>(&'a self, n: &'a Name<T>) -> &'a str {
        self.scope
            .iter()
            .rev()
            .find(|(i, _)| i.is_some() && *i == n.index())
            .map_or_else(|| n.string().unwrap_or("_"), |(_, d)| d.as_str())
    }

    fn spelling(&self, base: &str, body_fv: &[AnyName]) -> String {
        let taken = |s: &str| {
            self.scope
                .iter()
                .any(|(i, d)| d == s && body_fv.iter().any(|n| Some(n.index()) == *i))
        };
        let mut candidate = base.to_string();
        let mut suffix = 0;
        while taken(&candidate) {
            suffix += 1;
            candidate = format!("{}{}", base, suffix);
        }
        candidate
    }
}
