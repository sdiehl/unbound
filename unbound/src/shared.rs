use std::any::{Any, TypeId};
use std::cell::{OnceCell, RefCell};
use std::collections::{BTreeSet, HashMap, HashSet};
use std::fmt;
use std::hash::{BuildHasher, Hasher};
use std::ops::Deref;
use std::rc::{Rc, Weak};

use foldhash::fast::FixedState;
use hashbrown::HashTable;

use crate::{Alpha, AnyName, InstantiateCtx, Name, Subst, SubstCtx, SubstName};

/// Names and loose bound coordinates relative to a subtree's root. Metadata
/// must overapproximate occurrences; use unknown for an unsupported traversal.
#[derive(Clone, Debug, Default)]
pub struct Support {
    unknown: bool,
    /// Free names in order of first occurrence, deduplicated by `indices`.
    free: Vec<AnyName>,
    indices: HashSet<usize, FixedState>,
    bound: BTreeSet<(usize, usize)>,
}

impl Support {
    pub fn unknown() -> Self {
        Self {
            unknown: true,
            ..Self::default()
        }
    }
    pub fn name<T>(name: &Name<T>) -> Self {
        let mut out = Self::default();
        if let Some(n) = name.to_any() {
            out.indices.insert(n.index());
            out.free.push(n);
        }
        if let Some(p) = name.coordinates() {
            out.bound.insert(p);
        }
        out
    }
    pub fn merge(&mut self, other: Self) {
        self.unknown |= other.unknown;
        for n in other.free {
            if self.indices.insert(n.index()) {
                self.free.push(n);
            }
        }
        self.bound.extend(other.bound);
    }
    fn merge_ref(&mut self, other: &Self) {
        self.unknown |= other.unknown;
        for n in &other.free {
            if self.indices.insert(n.index()) {
                self.free.push(n.clone());
            }
        }
        self.bound.extend(&other.bound);
    }
    pub fn under_binder(mut self) -> Self {
        self.bound = self
            .bound
            .into_iter()
            .filter_map(|(d, p)| d.checked_sub(1).map(|d| (d, p)))
            .collect();
        self
    }
    fn closes(&self, names: &[AnyName]) -> bool {
        self.unknown || names.iter().any(|n| self.indices.contains(&n.index()))
    }
    fn opens(&self, level: usize, count: usize) -> bool {
        self.unknown
            || self
                .bound
                .range((level, 0)..(level, count))
                .next()
                .is_some()
    }
    fn substitutes<V>(&self, name: &Name<V>) -> bool {
        self.unknown || name.index().is_none_or(|i| self.indices.contains(&i))
    }
}

#[derive(Default)]
pub struct AlphaCtx {
    transforms: HashMap<TransformKey, Box<dyn Any>>,
    equalities: HashMap<(TypeId, usize, usize), Box<dyn Any>>,
}

#[derive(Hash, PartialEq, Eq)]
struct TransformKey {
    ty: TypeId,
    node: usize,
    level: usize,
    names: Vec<usize>,
    opening: bool,
}

pub(crate) type Memo = HashMap<(TypeId, usize, usize), Box<dyn Any>>;

/// The hasher behind [`Alpha::alpha_hash`]: fast, and fixed so that hashes
/// agree across tables.
pub(crate) fn hasher() -> impl Hasher {
    FixedState::default().build_hasher()
}

/// Live interned nodes of one syntax type, keyed by their alpha hash.
type Table<T> = HashTable<(u64, Weak<Node<T>>)>;

thread_local! {
    static TABLES: RefCell<HashMap<TypeId, Box<dyn Any>, FixedState>> = RefCell::default();
}

struct Node<T> {
    value: T,
    support: OnceCell<Support>,
    hash: OnceCell<u64>,
    interned: bool,
}

/// Immutable shared syntax. Derived traversals preserve its DAG structure;
/// hand-written Alpha implementations without support metadata use conservative traversal.
/// The contained syntax must not change through interior mutability: cached
/// support and hashes, and operation-local identity caches, rely on immutable
/// nodes.
pub struct Shared<T>(Rc<Node<T>>);

impl<T> Shared<T> {
    pub fn new(value: T) -> Self {
        Self::alloc(value, false)
    }
    fn alloc(value: T, interned: bool) -> Self {
        Self(Rc::new(Node {
            value,
            support: OnceCell::new(),
            hash: OnceCell::new(),
            interned,
        }))
    }
    /// Whether this node came from [`Shared::intern`], directly or as the
    /// result of an operation on an interned node.
    pub fn is_interned(&self) -> bool {
        self.0.interned
    }
    pub fn ptr_eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
    pub fn as_ptr(&self) -> *const T {
        &self.0.value
    }
    fn key(&self, depth: usize) -> (TypeId, usize, usize)
    where
        T: 'static,
    {
        (TypeId::of::<T>(), Rc::as_ptr(&self.0) as usize, depth)
    }
}
impl<T> Clone for Shared<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
impl<T> Deref for Shared<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.0.value
    }
}
impl<T: fmt::Debug> fmt::Debug for Shared<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.deref().fmt(f)
    }
}
impl<T: fmt::Display> fmt::Display for Shared<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.deref().fmt(f)
    }
}
impl<T> From<T> for Shared<T> {
    fn from(value: T) -> Self {
        Self::new(value)
    }
}
impl<T: Alpha> Shared<T> {
    fn cached_support(&self) -> &Support {
        self.0.support.get_or_init(|| self.deref().support())
    }
    fn cached_hash(&self) -> u64 {
        *self.0.hash.get_or_init(|| self.deref().alpha_hash())
    }
}

impl<T: Alpha + 'static> Shared<T> {
    /// The canonical node for `value` up to alpha equivalence.
    ///
    /// Each thread keeps one live interned node per alpha-equivalence class,
    /// so interned nodes are alpha-equivalent exactly when they are the same
    /// pointer. Operations on an interned node intern their results, keeping
    /// a graph hash consed through binding and substitution. Binder names are
    /// decoration, so interning `\y. y` may return an earlier `\x. x`.
    pub fn intern(value: T) -> Self {
        let node = Self::alloc(value, true);
        let hash = node.cached_hash();
        let found = TABLES.with(|tables| {
            let mut tables = tables.borrow_mut();
            let table = tables
                .entry(TypeId::of::<T>())
                .or_insert_with(|| Box::new(Table::<T>::new()))
                .downcast_mut::<Table<T>>()
                .expect("matching table type");
            let live = table
                .find(hash, |(h, weak)| {
                    *h == hash && weak.upgrade().is_some_and(|n| n.value.aeq(&node.0.value))
                })
                .and_then(|(_, weak)| weak.upgrade());
            if live.is_none() {
                // Sweep dead entries before the table would grow.
                if table.len() == table.capacity() {
                    table.retain(|(_, weak)| weak.strong_count() > 0);
                }
                table.insert_unique(hash, (hash, Rc::downgrade(&node.0)), |(h, _)| *h);
            }
            live
        });
        found.map_or(node, Self)
    }
    /// A node holding `value`, interned when `self` is.
    fn rebuild(&self, value: T) -> Self {
        if self.0.interned {
            Self::intern(value)
        } else {
            Self::new(value)
        }
    }
}

impl<T: Alpha + Clone + 'static> Shared<T> {
    fn transform(&mut self, level: usize, names: &[AnyName], ctx: &mut AlphaCtx, opening: bool) {
        let relevant = if opening {
            self.cached_support().opens(level, names.len())
        } else {
            self.cached_support().closes(names)
        };
        if !relevant {
            return;
        }
        let key = TransformKey {
            ty: TypeId::of::<T>(),
            node: Rc::as_ptr(&self.0) as usize,
            level,
            names: names.iter().map(AnyName::index).collect(),
            opening,
        };
        if let Some(entry) = ctx.transforms.get(&key) {
            let (_, result) = entry
                .downcast_ref::<(Self, Self)>()
                .expect("matching cache type");
            *self = result.clone();
            return;
        }
        let original = self.clone();
        let mut value = self.0.value.clone();
        if opening {
            value.open_with(level, names, ctx);
        } else {
            value.close_with(level, names, ctx);
        }
        *self = self.rebuild(value);
        // Keep the input alive until the context is dropped: pointer addresses
        // must not be recycled while they are keys in the memo table.
        ctx.transforms
            .insert(key, Box::new((original, self.clone())));
    }
}

impl<T: Alpha + Clone + 'static> Alpha for Shared<T> {
    fn aeq(&self, other: &Self) -> bool {
        self.aeq_with(other, &mut AlphaCtx::default())
    }
    fn aeq_with(&self, other: &Self, ctx: &mut AlphaCtx) -> bool {
        if self.ptr_eq(other) {
            return true;
        }
        if self.0.interned && other.0.interned {
            return false;
        }
        if let (Some(a), Some(b)) = (self.0.hash.get(), other.0.hash.get()) {
            if a != b {
                return false;
            }
        }
        let key = (
            TypeId::of::<T>(),
            Rc::as_ptr(&self.0) as usize,
            Rc::as_ptr(&other.0) as usize,
        );
        if let Some(entry) = ctx.equalities.get(&key) {
            return entry
                .downcast_ref::<(Self, Self, bool)>()
                .expect("matching cache type")
                .2;
        }
        let result = self.deref().aeq_with(other.deref(), ctx);
        ctx.equalities
            .insert(key, Box::new((self.clone(), other.clone(), result)));
        result
    }
    fn close(&mut self, level: usize, names: &[AnyName]) {
        self.close_with(level, names, &mut AlphaCtx::default());
    }
    fn open(&mut self, level: usize, names: &[AnyName]) {
        self.open_with(level, names, &mut AlphaCtx::default());
    }
    fn close_with(&mut self, level: usize, names: &[AnyName], ctx: &mut AlphaCtx) {
        self.transform(level, names, ctx, false);
    }
    fn open_with(&mut self, level: usize, names: &[AnyName], ctx: &mut AlphaCtx) {
        self.transform(level, names, ctx, true);
    }
    fn support(&self) -> Support {
        self.cached_support().clone()
    }
    fn support_in(&self, acc: &mut Support) {
        acc.merge_ref(self.cached_support());
    }
    fn hash_in(&self, state: &mut dyn Hasher) {
        state.write_u64(self.cached_hash());
    }
    fn fv_in(&self, acc: &mut Vec<AnyName>) {
        let support = self.cached_support();
        if support.unknown {
            self.deref().fv_in(acc);
        } else {
            for n in &support.free {
                if !acc.contains(n) {
                    acc.push(n.clone());
                }
            }
        }
    }
}

impl<T: Alpha + Subst<V> + 'static, V> Subst<V> for Shared<T> {
    fn is_var(&self) -> Option<SubstName<V>> {
        self.deref().is_var()
    }
    fn subst(&self, var: &Name<V>, value: &V) -> Self {
        self.subst_with(&mut SubstCtx::new(var, value))
    }
    fn subst_with(&self, ctx: &mut SubstCtx<'_, V>) -> Self {
        if !self.cached_support().substitutes(ctx.var()) {
            return self.clone();
        }
        let key = self.key(0);
        if let Some(entry) = ctx.memo.get(&key) {
            return entry
                .downcast_ref::<(Self, Self)>()
                .expect("matching cache type")
                .1
                .clone();
        }
        let result = self.rebuild(self.deref().subst_with(ctx));
        ctx.memo
            .insert(key, Box::new((self.clone(), result.clone())));
        result
    }
    fn instantiate_with(&self, level: usize, ctx: &mut InstantiateCtx<'_, V>) -> Option<Self> {
        if !self.cached_support().opens(level, ctx.values().len()) {
            return Some(self.clone());
        }
        let key = self.key(level);
        if let Some(entry) = ctx.memo.get(&key) {
            return Some(
                entry
                    .downcast_ref::<(Self, Self)>()
                    .expect("matching cache type")
                    .1
                    .clone(),
            );
        }
        let result = self.rebuild(self.deref().instantiate_with(level, ctx)?);
        ctx.memo
            .insert(key, Box::new((self.clone(), result.clone())));
        Some(result)
    }
}
