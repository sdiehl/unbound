//! Names.
//!
//! A name is either *free*, carrying a globally unique index, or *bound*,
//! carrying the de Bruijn coordinates assigned when an enclosing [`Bind`]
//! closed over it. Free names are compared by index, so two distinct
//! variables that happen to share a spelling never alias.
//!
//! [`Bind`]: crate::Bind

use std::fmt;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::sync::atomic::{AtomicUsize, Ordering};

static COUNTER: AtomicUsize = AtomicUsize::new(0);

/// Draw the next globally unique name index.
///
/// Every free name in the process, however it was created, comes from this
/// counter, so a freshly drawn index can never collide with a live one.
pub(crate) fn next_index() -> usize {
    COUNTER.fetch_add(1, Ordering::Relaxed)
}

#[derive(Clone, Debug)]
enum Repr {
    Free { string: String, index: usize },
    Bound { level: usize, position: usize },
}

/// A variable name, parameterised by the AST type it can stand for.
///
/// The phantom parameter keeps a `Name<Expr>` from being mistaken for a
/// `Name<Ty>` at compile time, at no runtime cost.
pub struct Name<T> {
    repr: Repr,
    _phantom: PhantomData<T>,
}

impl<T> Name<T> {
    /// Create a fresh free name with the given spelling.
    pub fn new(s: impl Into<String>) -> Self {
        Name {
            repr: Repr::Free {
                string: s.into(),
                index: next_index(),
            },
            _phantom: PhantomData,
        }
    }

    /// Create a bound name at the given de Bruijn coordinates.
    ///
    /// Terms are closed by [`Bind::new`], so this is rarely needed directly.
    ///
    /// [`Bind::new`]: crate::Bind::new
    pub fn bound(level: usize, position: usize) -> Self {
        Name {
            repr: Repr::Bound { level, position },
            _phantom: PhantomData,
        }
    }

    /// Whether this name is free.
    pub fn is_free(&self) -> bool {
        matches!(self.repr, Repr::Free { .. })
    }

    /// Whether this name is bound by an enclosing binder.
    pub fn is_bound(&self) -> bool {
        matches!(self.repr, Repr::Bound { .. })
    }

    /// The spelling of a free name, or `None` if it is bound.
    pub fn string(&self) -> Option<&str> {
        match &self.repr {
            Repr::Free { string, .. } => Some(string),
            Repr::Bound { .. } => None,
        }
    }

    /// The unique index of a free name, or `None` if it is bound.
    pub fn index(&self) -> Option<usize> {
        match self.repr {
            Repr::Free { index, .. } => Some(index),
            Repr::Bound { .. } => None,
        }
    }

    /// The de Bruijn coordinates of a bound name, or `None` if it is free.
    pub fn coordinates(&self) -> Option<(usize, usize)> {
        match self.repr {
            Repr::Bound { level, position } => Some((level, position)),
            Repr::Free { .. } => None,
        }
    }

    /// Erase the phantom type, yielding `None` for a bound name.
    pub fn to_any(&self) -> Option<AnyName> {
        match &self.repr {
            Repr::Free { string, index } => Some(AnyName {
                string: string.clone(),
                index: *index,
            }),
            Repr::Bound { .. } => None,
        }
    }

    /// Rebuild a typed name from an erased one.
    pub fn from_any(any: &AnyName) -> Self {
        Name {
            repr: Repr::Free {
                string: any.string.clone(),
                index: any.index,
            },
            _phantom: PhantomData,
        }
    }

    /// A new free name with the same spelling and a fresh index.
    ///
    /// A bound name has no spelling of its own, so it freshens to `_`.
    pub fn freshen(&self) -> Self {
        Name::new(self.string().unwrap_or("_"))
    }

    /// Replace this name with a bound one if it appears in `names`.
    pub(crate) fn close_name(&mut self, level: usize, names: &[AnyName]) {
        if let Repr::Free { index, .. } = self.repr {
            if let Some(position) = names.iter().position(|n| n.index == index) {
                self.repr = Repr::Bound { level, position };
            }
        }
    }

    /// Replace this name with a free one if it is bound at `level`.
    pub(crate) fn open_name(&mut self, level: usize, names: &[AnyName]) {
        if let Repr::Bound {
            level: l,
            position: p,
        } = self.repr
        {
            if l == level {
                if let Some(name) = names.get(p) {
                    self.repr = Repr::Free {
                        string: name.string.clone(),
                        index: name.index,
                    };
                }
            }
        }
    }
}

impl<T> Clone for Name<T> {
    fn clone(&self) -> Self {
        Name {
            repr: self.repr.clone(),
            _phantom: PhantomData,
        }
    }
}

impl<T> fmt::Debug for Name<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Name({})", self)
    }
}

impl<T> fmt::Display for Name<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.repr {
            Repr::Free { string, index } => write!(f, "{}@{}", string, index),
            Repr::Bound { level, position } => write!(f, "#{}.{}", level, position),
        }
    }
}

impl<T> PartialEq for Name<T> {
    fn eq(&self, other: &Self) -> bool {
        match (&self.repr, &other.repr) {
            (Repr::Free { index: a, .. }, Repr::Free { index: b, .. }) => a == b,
            (
                Repr::Bound {
                    level: l1,
                    position: p1,
                },
                Repr::Bound {
                    level: l2,
                    position: p2,
                },
            ) => l1 == l2 && p1 == p2,
            _ => false,
        }
    }
}

impl<T> Eq for Name<T> {}

impl<T> Hash for Name<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match &self.repr {
            Repr::Free { index, .. } => (0u8, index).hash(state),
            Repr::Bound { level, position } => (1u8, level, position).hash(state),
        }
    }
}

/// A free name with its phantom type erased.
///
/// Closing and opening work over erased names because a binder abstracts
/// names uniformly, whatever AST type they stand for. Indices are globally
/// unique across all phantom types, so erasure cannot conflate two names.
#[derive(Clone, Debug)]
pub struct AnyName {
    string: String,
    index: usize,
}

impl AnyName {
    /// The spelling of this name.
    pub fn string(&self) -> &str {
        &self.string
    }

    /// The unique index of this name.
    pub fn index(&self) -> usize {
        self.index
    }
}

impl fmt::Display for AnyName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}@{}", self.string, self.index)
    }
}

impl PartialEq for AnyName {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl Eq for AnyName {}

impl Hash for AnyName {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.index.hash(state);
    }
}
