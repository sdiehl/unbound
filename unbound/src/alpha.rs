//! Alpha equivalence for terms with binding

use std::collections::HashMap;

use crate::{Bind, Name};

/// Trait for types that support alpha equivalence
pub trait Alpha {
    /// Check if two terms are alpha-equivalent
    fn aeq(&self, other: &Self) -> bool;

    /// Check alpha equivalence with a renaming context
    fn aeq_in(&self, ctx: &mut AlphaCtx, other: &Self) -> bool {
        // Default implementation just calls aeq
        let _ = ctx;
        self.aeq(other)
    }

    /// Collect free variables
    fn fv(&self) -> Vec<String> {
        let mut vars = Vec::new();
        self.fv_in(&mut vars);
        vars
    }

    /// Collect free variables into a vector
    fn fv_in(&self, vars: &mut Vec<String>) {
        let _ = vars;
        // Default implementation does nothing
    }
}

/// Context for alpha equivalence checking
#[derive(Debug, Clone)]
pub struct AlphaCtx {
    left_mapping: HashMap<usize, usize>,
    right_mapping: HashMap<usize, usize>,
    next_fresh: usize,
}

impl AlphaCtx {
    /// Create a new empty context
    pub fn new() -> Self {
        AlphaCtx {
            left_mapping: HashMap::new(),
            right_mapping: HashMap::new(),
            next_fresh: 0,
        }
    }

    /// Bind two names to a fresh identifier
    pub fn bind<T>(&mut self, left: &Name<T>, right: &Name<T>) -> (usize, usize) {
        let fresh = self.next_fresh;
        self.next_fresh += 1;

        let old_left = self.left_mapping.insert(left.index(), fresh);
        let old_right = self.right_mapping.insert(right.index(), fresh);

        (
            old_left.unwrap_or(left.index()),
            old_right.unwrap_or(right.index()),
        )
    }

    /// Unbind names (restore old mappings)
    pub fn unbind<T>(&mut self, left: &Name<T>, right: &Name<T>, old: (usize, usize)) {
        if old.0 == left.index() {
            self.left_mapping.remove(&left.index());
        } else {
            self.left_mapping.insert(left.index(), old.0);
        }

        if old.1 == right.index() {
            self.right_mapping.remove(&right.index());
        } else {
            self.right_mapping.insert(right.index(), old.1);
        }
    }

    /// Look up a name in the left mapping
    pub fn lookup_left<T>(&self, name: &Name<T>) -> usize {
        self.left_mapping
            .get(&name.index())
            .copied()
            .unwrap_or(name.index())
    }

    /// Look up a name in the right mapping
    pub fn lookup_right<T>(&self, name: &Name<T>) -> usize {
        self.right_mapping
            .get(&name.index())
            .copied()
            .unwrap_or(name.index())
    }
}

impl Default for AlphaCtx {
    fn default() -> Self {
        Self::new()
    }
}

// Implement Alpha for Name
impl<T> Alpha for Name<T> {
    fn aeq(&self, other: &Self) -> bool {
        self.index() == other.index()
    }

    fn aeq_in(&self, ctx: &mut AlphaCtx, other: &Self) -> bool {
        ctx.lookup_left(self) == ctx.lookup_right(other)
    }

    fn fv_in(&self, vars: &mut Vec<String>) {
        if !vars.contains(&self.string().to_string()) {
            vars.push(self.string().to_string());
        }
    }
}

// Implement Alpha for Bind where the pattern is a Name
impl<T: Alpha> Alpha for Bind<Name<T>, Box<T>> {
    fn aeq(&self, other: &Self) -> bool {
        let mut ctx = AlphaCtx::new();
        self.aeq_in(&mut ctx, other)
    }

    fn aeq_in(&self, ctx: &mut AlphaCtx, other: &Self) -> bool {
        // Bind the two pattern names to a fresh identifier
        let old = ctx.bind(self.pattern(), other.pattern());
        // Check alpha equivalence of bodies with the binding in place
        let result = self.body().aeq_in(ctx, other.body());
        // Restore the old mappings
        ctx.unbind(self.pattern(), other.pattern(), old);
        result
    }

    fn fv_in(&self, vars: &mut Vec<String>) {
        // Collect free vars from body
        self.body().fv_in(vars);
        // Remove the bound variable
        vars.retain(|v| v != self.pattern().string());
    }
}

// Implement Alpha for basic types
impl Alpha for String {
    fn aeq(&self, other: &Self) -> bool {
        self == other
    }
}

impl Alpha for usize {
    fn aeq(&self, other: &Self) -> bool {
        self == other
    }
}

impl Alpha for i32 {
    fn aeq(&self, other: &Self) -> bool {
        self == other
    }
}

impl<T: Alpha> Alpha for Option<T> {
    fn aeq(&self, other: &Self) -> bool {
        match (self, other) {
            (None, None) => true,
            (Some(a), Some(b)) => a.aeq(b),
            _ => false,
        }
    }

    fn aeq_in(&self, ctx: &mut AlphaCtx, other: &Self) -> bool {
        match (self, other) {
            (None, None) => true,
            (Some(a), Some(b)) => a.aeq_in(ctx, b),
            _ => false,
        }
    }
}

impl<T: Alpha> Alpha for Vec<T> {
    fn aeq(&self, other: &Self) -> bool {
        self.len() == other.len() && self.iter().zip(other.iter()).all(|(a, b)| a.aeq(b))
    }

    fn aeq_in(&self, ctx: &mut AlphaCtx, other: &Self) -> bool {
        self.len() == other.len() && self.iter().zip(other.iter()).all(|(a, b)| a.aeq_in(ctx, b))
    }
}

impl<T: Alpha> Alpha for Box<T> {
    fn aeq(&self, other: &Self) -> bool {
        (**self).aeq(&**other)
    }

    fn aeq_in(&self, ctx: &mut AlphaCtx, other: &Self) -> bool {
        (**self).aeq_in(ctx, &**other)
    }

    fn fv_in(&self, vars: &mut Vec<String>) {
        (**self).fv_in(vars)
    }
}
