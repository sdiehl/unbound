use std::any::{Any, TypeId};
use std::cell::{OnceCell, RefCell};
use std::collections::HashMap;
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
    // Most closed nodes have empty support; reserve only a pointer on each node.
    data: Option<Box<SupportData>>,
}

#[derive(Clone, Debug, Default)]
struct SupportData {
    // Exact free names in first-occurrence order, dropped once wide. The mask
    // overapproximates free indices either way.
    free: Vec<AnyName>,
    mask: u64,
    wide: bool,
    bound: Vec<(usize, usize)>,
}
const NARROW: usize = 8;
fn bit(index: usize) -> u64 {
    1 << (index & 63)
}
impl SupportData {
    fn contains(&self, index: usize) -> bool {
        self.mask & bit(index) != 0 && (self.wide || self.free.iter().any(|n| n.index() == index))
    }
    fn insert_free(&mut self, name: AnyName) {
        if self.wide || self.contains(name.index()) {
            self.mask |= bit(name.index());
            return;
        }
        self.mask |= bit(name.index());
        self.free.push(name);
        if self.free.len() > NARROW {
            self.widen();
        }
    }
    fn widen(&mut self) {
        self.wide = true;
        self.free = Vec::new();
    }
    fn insert_bound(&mut self, coordinate: (usize, usize)) {
        if let Err(i) = self.bound.binary_search(&coordinate) {
            self.bound.insert(i, coordinate);
        }
    }
    fn merge_bound(&mut self, other: &[(usize, usize)]) {
        if other.is_empty() || self.bound == other {
            return;
        }
        if self.bound.is_empty() {
            self.bound.extend_from_slice(other);
            return;
        }
        let (mut i, mut j) = (0, 0);
        let (a, b) = (&self.bound, other);
        let mut out = Vec::with_capacity(a.len() + b.len());
        while i < a.len() && j < b.len() {
            let next = a[i].min(b[j]);
            i += usize::from(a[i] == next);
            j += usize::from(b[j] == next);
            out.push(next);
        }
        out.extend_from_slice(&a[i..]);
        out.extend_from_slice(&b[j..]);
        self.bound = out;
    }
    fn merge_free(&mut self, other: &SupportData) {
        if other.wide {
            self.widen();
        }
        if self.wide {
            self.mask |= other.mask;
        } else {
            for n in &other.free {
                self.insert_free(n.clone());
            }
            self.mask |= other.mask;
        }
    }
}
impl Support {
    /// Whether the recorded support is complete.
    pub fn is_known(&self) -> bool {
        !self.unknown
    }
    /// Whether loose bound coordinates may occur. Unknown support is conservative.
    pub fn has_loose_bound_vars(&self) -> bool {
        self.unknown || !self.bounds().is_empty()
    }
    /// Largest recorded loose level. Only a complete upper bound when `is_known`.
    pub fn max_loose_level(&self) -> Option<usize> {
        self.bounds().last().map(|&(level, _)| level)
    }
    /// Recorded coordinates in ascending order; unknown support may omit entries.
    pub fn bound_coordinates(&self) -> impl DoubleEndedIterator<Item = (usize, usize)> + '_ {
        self.bounds().iter().copied()
    }
    fn bounds(&self) -> &[(usize, usize)] {
        self.data.as_ref().map_or(&[], |data| data.bound.as_slice())
    }
    /// Exact free names, or none once the set is too wide to keep.
    fn free(&self) -> Option<&[AnyName]> {
        match &self.data {
            None => Some(&[]),
            Some(data) if data.wide => None,
            Some(data) => Some(&data.free),
        }
    }
    fn contains(&self, index: usize) -> bool {
        self.data.as_ref().is_some_and(|data| data.contains(index))
    }
    pub fn unknown() -> Self {
        Self {
            unknown: true,
            data: None,
        }
    }
    pub fn name<T>(name: &Name<T>) -> Self {
        let mut out = Self::default();
        if let Some(n) = name.to_any() {
            out.data.get_or_insert_with(Default::default).insert_free(n);
        }
        if let Some(p) = name.coordinates() {
            out.data
                .get_or_insert_with(Default::default)
                .insert_bound(p);
        }
        out
    }
    pub fn merge(&mut self, other: Self) {
        self.unknown |= other.unknown;
        if let Some(data) = &mut self.data {
            if let Some(other) = other.data {
                data.merge_free(&other);
                data.merge_bound(&other.bound);
            }
        } else {
            self.data = other.data;
        }
    }

    fn merge_ref(&mut self, other: &Self) {
        self.unknown |= other.unknown;
        if let Some(other) = &other.data {
            let data = self.data.get_or_insert_with(Default::default);
            data.merge_free(other);
            data.merge_bound(&other.bound);
        }
    }
    pub fn under_binder(mut self) -> Self {
        if let Some(data) = &mut self.data {
            data.bound.retain_mut(|(d, _)| {
                if let Some(lower) = d.checked_sub(1) {
                    *d = lower;
                    true
                } else {
                    false
                }
            });
            if data.mask == 0 && data.bound.is_empty() {
                self.data = None;
            }
        }
        self
    }
    fn closes(&self, names: &[AnyName]) -> bool {
        self.unknown || names.iter().any(|n| self.contains(n.index()))
    }
    fn opens(&self, level: usize, count: usize) -> bool {
        let bounds = self.bounds();
        let i = bounds.partition_point(|&p| p < (level, 0));
        self.unknown || bounds.get(i).is_some_and(|&(d, p)| d == level && p < count)
    }
    fn substitutes<V>(&self, name: &Name<V>) -> bool {
        self.unknown || name.index().is_none_or(|i| self.contains(i))
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
struct Table<T> {
    entries: HashTable<(u64, Weak<Node<T>>)>,
    misses: usize,
}
impl<T> Table<T> {
    fn new() -> Self {
        Self {
            entries: HashTable::new(),
            misses: 0,
        }
    }
    fn collect(&mut self, shrink: bool) -> usize {
        let before = self.entries.len();
        self.entries.retain(|(_, weak)| weak.strong_count() > 0);
        if shrink && self.entries.capacity() > self.entries.len().saturating_mul(4).max(64) {
            self.entries.shrink_to_fit(|(h, _)| *h);
        }
        self.misses = 0;
        before - self.entries.len()
    }
}

thread_local! {
    #[cfg(test)]
    static ALLOCATIONS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
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
        #[cfg(test)]
        ALLOCATIONS.with(|n| n.set(n.get() + 1));
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
    /// Borrow the cached support without cloning its sets.
    pub fn support_ref(&self) -> &Support {
        self.cached_support()
    }
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
        // Look up the borrowed value before allocating a candidate Rc.
        let hash = value.alpha_hash();
        let found = TABLES.with(|tables| {
            let mut tables = tables.borrow_mut();
            let table = tables
                .entry(TypeId::of::<T>())
                .or_insert_with(|| Box::new(Table::<T>::new()))
                .downcast_mut::<Table<T>>()
                .expect("matching table type");
            if let Some(node) = table
                .entries
                .find(hash, |(h, weak)| {
                    *h == hash && weak.upgrade().is_some_and(|n| n.value.aeq(&value))
                })
                .and_then(|(_, weak)| weak.upgrade())
            {
                return Some(node);
            }
            None
        });
        if let Some(node) = found {
            return Self(node);
        }
        // Allocate/drop user values outside the TABLES borrow: destructors may intern.
        let node = Self::alloc(value, true);
        node.0.hash.set(hash).expect("new node hash");
        TABLES.with(|tables| {
            let mut tables = tables.borrow_mut();
            let table = tables
                .get_mut(&TypeId::of::<T>())
                .unwrap()
                .downcast_mut::<Table<T>>()
                .expect("matching table type");
            table.misses += 1;
            // Amortize a full sweep over a proportional number of misses,
            // including workloads that stop growing before capacity is exhausted.
            if table.misses >= table.entries.len().max(1024) {
                table.collect(false);
            }
            if let Some((_, weak)) = table
                .entries
                .find_mut(hash, |(h, weak)| *h == hash && weak.strong_count() == 0)
            {
                *weak = Rc::downgrade(&node.0);
            } else {
                if table.entries.len() == table.entries.capacity() {
                    table.collect(false);
                    table
                        .entries
                        .reserve(table.entries.len().max(1), |(h, _)| *h);
                }
                table
                    .entries
                    .insert_unique(hash, (hash, Rc::downgrade(&node.0)), |(h, _)| *h);
            }
        });
        node
    }
    /// Reclaim dead interned allocations of this type on the current thread and
    /// shrink excess table capacity. Live nodes retain their canonical identity.
    /// Returns the number of removed entries. Call at coarse phase boundaries,
    /// not after each allocation: collection scans the table.
    pub fn collect_dead() -> usize {
        TABLES.with(|tables| {
            tables
                .borrow_mut()
                .get_mut(&TypeId::of::<T>())
                .map_or(0, |table| {
                    table
                        .downcast_mut::<Table<T>>()
                        .expect("matching table type")
                        .collect(true)
                })
        })
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
        match support.free() {
            Some(free) if !support.unknown => {
                for n in free {
                    if !acc.contains(n) {
                        acc.push(n.clone());
                    }
                }
            }
            _ => self.deref().fv_in(acc),
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn intern_hits_do_not_allocate_candidate_nodes() {
        let live = Shared::intern(42u64);
        let before = ALLOCATIONS.with(|n| n.get());
        for _ in 0..1000 {
            assert!(Shared::intern(42u64).ptr_eq(&live));
        }
        assert_eq!(ALLOCATIONS.with(|n| n.get()), before);
    }

    #[test]
    fn compact_support_preserves_order_and_bound_coordinates() {
        assert!(std::mem::size_of::<Support>() <= 2 * std::mem::size_of::<usize>());
        let mut support = Support::default();
        let names: Vec<Name<u64>> = (0..20).map(|_| crate::s2n("x")).collect();
        let narrow = &names[..NARROW];
        for name in narrow.iter().chain(narrow.iter().rev()) {
            support.merge(Support::name(name));
        }
        assert_eq!(
            support
                .free()
                .unwrap()
                .iter()
                .map(AnyName::index)
                .collect::<Vec<_>>(),
            narrow
                .iter()
                .map(|n| n.index().unwrap())
                .collect::<Vec<_>>()
        );
        for name in &names {
            support.merge(Support::name(name));
        }
        assert!(support.free().is_none());
        assert!(names.iter().all(|n| support.substitutes(n)));
        for p in [(2, 1), (0, 0), (1, 2), (2, 1)] {
            support.merge(Support::name(&Name::<u64>::bound(p.0, p.1)));
        }
        let lowered = support.under_binder();
        assert_eq!(
            lowered.bound_coordinates().collect::<Vec<_>>(),
            vec![(0, 2), (1, 1)]
        );
        assert!(lowered.closes(&[names[19].to_any().unwrap()]));
        assert!(Support::name(&Name::<u64>::bound(0, 0))
            .under_binder()
            .data
            .is_none());
    }

    #[test]
    fn collection_releases_allocations_and_preserves_live_identity() {
        let live = Shared::intern(999_999u64);
        let nodes: Vec<_> = (0..4096u64).map(Shared::intern).collect();
        let weak = Rc::downgrade(&nodes[0].0);
        drop(nodes);
        assert_eq!(weak.strong_count(), 0);
        assert!(Shared::<u64>::collect_dead() >= 4096);
        assert!(Shared::intern(999_999u64).ptr_eq(&live));
        TABLES.with(|tables| {
            let tables = tables.borrow();
            let table = tables[&TypeId::of::<u64>()]
                .downcast_ref::<Table<u64>>()
                .unwrap();
            assert_eq!(table.entries.len(), 1);
            assert!(table.entries.capacity() <= 64);
        });
        // A caller's weak handle alone cannot resurrect a collected node.
        assert!(weak.upgrade().is_none());
    }

    #[test]
    fn churn_is_collected_without_reaching_table_capacity() {
        let nodes: Vec<_> = (0..4096u64).map(Shared::intern).collect();
        drop(nodes);
        for i in 0..20_000 {
            drop(Shared::intern(100_000u64 + i));
        }
        TABLES.with(|tables| {
            let tables = tables.borrow();
            let table = tables[&TypeId::of::<u64>()]
                .downcast_ref::<Table<u64>>()
                .unwrap();
            assert!(table.entries.len() <= 1024);
        });
    }

    #[test]
    fn repeated_dead_terms_reuse_their_intern_slots() {
        let live = Shared::intern(1u64);
        for _ in 0..100 {
            drop(Shared::intern(2u64));
            TABLES.with(|tables| {
                let tables = tables.borrow();
                let table = tables[&TypeId::of::<u64>()]
                    .downcast_ref::<Table<u64>>()
                    .unwrap();
                assert_eq!(table.entries.len(), 2);
            });
            assert!(Shared::intern(1u64).ptr_eq(&live));
        }
    }
}
