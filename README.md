# Unbound

A Rust library for working with locally nameless representations, providing
automatic capture-avoiding substitution and alpha equivalence for abstract
syntax trees with binding when building functional language typecheckers and
compilers.

Provides two derivable macros for automatic implementation of:

- **`Alpha`**: Automatically derived alpha equivalence checking that correctly
  handles binding
- **`Subst`**: Automatically derived capture-avoiding substitution


```rust
use unbound::prelude::*;

#[derive(Clone, Debug, Alpha, Subst)]
enum Expr {
    Var(Name<Expr>),
    Lam(Bind<Name<Expr>, Box<Expr>>),
    App(Box<Expr>, Box<Expr>),
}
```

## How It Works

### Phantom Types

The `Name<T>` type uses a phantom parameter to record which AST a name can stand for:

```rust
pub struct Name<T> {
    repr: Repr,             // free (string + unique index), or bound (de Bruijn)
    _phantom: PhantomData<T>,
}
```

This prevents mixing names from different AST types - a `Name<Expr>` cannot be confused with a `Name<Ty>` at compile time, despite having identical runtime representations. The phantom type has zero runtime cost while providing complete type safety.

### Locally Nameless Representation

A name is either **free**, carrying a globally unique index drawn from a process-wide `AtomicUsize`, or **bound**, carrying the de Bruijn coordinates `(level, position)` assigned when an enclosing binder closed over it. `level` counts binders outwards from the innermost one; `position` distinguishes names bound simultaneously by the same pattern.

`Bind<P, T>` maintains that form:

- `Bind::new` **closes** the body, rewriting every free occurrence of a name the pattern binds into a coordinate
- `Bind::unbind` **opens** it again, drawing genuinely fresh names from the same global counter

So `\x. \y. x y` is stored as `\. \. #1.0 #0.0`, and the binder's own name is inert decoration. Three properties fall out:

- **Alpha equivalence is structural equality.** `aeq` walks both terms in lockstep with no renaming context and no allocation, since alpha-variants have identical de Bruijn bodies
- **Substitution cannot capture.** A bound variable has no name for an incoming term to collide with, so `subst` needs neither freshening nor a shadowing check
- **Free variables are exact.** `fv` collects only genuinely free names, compared by index, so two distinct variables that happen to share a spelling are never conflated

Because the body is stored closed, reach for it through `unbind` (or `unbind_ref`) rather than `body`, which hands back the raw de Bruijn form. To substitute straight into the body instead, as beta reduction or type instantiation does, use `instantiate(&value)`, or `instantiate_all(&values)` for a pattern binding several names.

### Parsing

A parser can resolve scope with no separate pass. `Name::global("x")` returns the same name for every call with the same spelling, and `Bind::new` captures only the occurrences still free in its body, so building the term bottom-up with global names gives every occurrence its nearest enclosing binder. Names made with `s2n` are always distinct from global ones.

### Patterns

The `Pattern` trait says what a binder abstracts over. Implementations are provided for a single `Name<T>`, a `Vec<Name<T>>` bound simultaneously, and either of those paired with an annotation, as in `Bind<(Name<Tm>, Ty), Box<Tm>>`. An annotation sits *outside* the scope of the binder it decorates, so it is closed at the enclosing level and contributes to the free variables of the whole binding.

Children may be held in a `Box`, `Rc` or `Arc`. Shared pointers are copy on write, so closing or opening a term never disturbs another owner of the same subtree.

### Capture-Avoiding Substitution

The `Subst` trait's key insight is the `is_var` method:

```rust
trait Subst<V> {
    fn is_var(&self) -> Option<SubstName<V>>;
    fn subst(&self, var: &Name<V>, value: &V) -> Self;
}
```

The derive macro treats a variant named `V`, `Var` or `Variable`, or one marked `#[subst_var]`, as the variable case and generates a plain structural traversal for everything else. Binders need no special handling: a locally closed `value` can be moved under any number of binders without disturbing it.

By default a type substitutes into itself. `#[subst(Ty)]` derives `Subst<Ty>` instead, so a term can take substitutions for the types it mentions, and `#[subst(Self, Ty)]` derives both. A type with no variables of its own, such as the kinds annotating a type binder, takes `#[subst(_)]`, which derives `Subst<V>` for every `V`.

### Printing

`Display` on a name shows its spelling, which need not be unique; `Debug` shows the index too. To print whole terms unambiguously, thread a `NameScope` through the printer. It starts from the term's free names, suffixing any that share a spelling. At each binder, `bind(&name, &body.fv())` picks a spelling and renames only when the plain spelling would capture a name the body actually uses. `get(&name)` gives the spelling for an occurrence, and `pop` leaves the scope. The output reads back as an alpha-equivalent term.

### Fresh Name Generation

Freshness is guaranteed by the global counter, so `unbind` is safe anywhere and needs no context. What `FreshM<T>` adds is *readable* freshness: it tracks which spellings are already in play and suffixes new ones, turning a second `x` into `x1` rather than another `x` distinguishable only by index.

```rust
pub struct FreshM<T> {
    computation: Box<dyn FnOnce(Rc<RefCell<FreshState>>) -> T>,
}
```

## License

MIT Licensed. Copyright 2025 Stephen Diehl.
