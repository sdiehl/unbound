# Changelog

## [0.2.0] - 2026-09-21

The representation is now genuinely locally nameless. Previously names were
fully nominal and alpha equivalence was recovered after the fact by walking a
renaming context, which left several soundness holes. Bound variables are now
de Bruijn coordinates, installed when a binder closes over its pattern and
removed again when it is opened.

### Fixed

- **Substitution could capture.** `subst` skipped a shadowing binder but never
  renamed one whose name occurred free in the incoming value, so
  `(\y. x)[x := y]` silently became the identity. A closed body has no name
  for an incoming term to collide with, so capture is now impossible by
  construction.
- **`fv` dropped genuinely free variables.** Free variables were collected and
  filtered by spelling, so a free variable sharing a binder's name vanished
  from the result. Free variables are now compared by unique index.
- **Fresh names were not fresh.** `FreshState` counted independently of the
  global name counter, so the first freshened name took index 0 and aliased
  the first name ever created. All names, freshened or not, now draw from the
  one global counter.
- **`Bind::unbind` did not freshen.** It destructured the binding and handed
  back the original binder name, despite documenting otherwise. It now opens
  the body with genuinely fresh names, so unbinding the same term twice never
  aliases.

### Added

- `Name::bound`, `Name::coordinates`, `Name::is_free` and `Name::is_bound` for
  working with the de Bruijn form directly.
- `AnyName`, a free name with its phantom type erased, used by closing,
  opening and `fv`.
- `Pattern`, the trait describing what a binder abstracts over, with
  implementations for `Name<T>`, `Vec<Name<T>>`, `(Name<T>, U)` and
  `(Vec<Name<T>>, U)`. Annotations sit outside the scope of the binder they
  decorate.
- `Bind::unbind_ref`, for opening a binding without consuming it.
- `#[subst_var]`, which marks the variable variant for `derive(Subst)`. The
  attribute was previously declared but ignored, leaving the macro dependent
  on a variant being named `V`, `Var` or `Variable`; both now work.
- `Alpha` and `Subst` implementations for the remaining primitive types.
- `derive(Alpha)` now bounds each generic parameter by `Alpha`, so generic
  types such as `struct Embed<T>(T)` can derive it.

### Changed

- `Bind::new` closes the body over the pattern's names, and `Bind::unbind`
  opens it. `Bind::body` therefore returns the raw de Bruijn form; prefer
  `unbind` unless you mean to inspect it.
- `Alpha::aeq` is structural equality and no longer allocates a renaming map.
- `Alpha` gained `close` and `open` and lost `aeq_in`; `AlphaCtx` is gone with
  it, having no remaining purpose.
- `Alpha::fv` returns `Vec<AnyName>` rather than `Vec<String>`, and `fv_in`
  takes `&mut Vec<AnyName>`.
- `Name::string` and `Name::index` return `Option`, since a bound name has
  neither.
- The three `Subst` implementations for specific `Bind` shapes are replaced by
  one covering every pattern, and `Subst for Name<T>` is no longer restricted
  to names of the type being substituted.
- The toolchain pin moves to 1.92.0, the oldest stable cargo that can publish
  a workspace in dependency order, and the publish workflow uses
  `cargo publish --workspace`. A bare `cargo publish` cannot select a package
  in a virtual workspace, so tagging previously built and tested and then
  failed at the upload step.
- `derive(Subst)` no longer special-cases a variant named `Lam`. That case
  existed to skip substitution under a shadowing binder and is now handled
  generically and correctly by `Bind` itself.

### Removed

- `AlphaCtx`, and `Alpha::aeq_in` along with it.
- `Name::with_index`, which allowed forging an index that collided with a live
  name. Use `Name::new`.
- `Bind`'s derived `PartialEq`, which compared binder names and so was never
  alpha equivalence. Use `aeq`.

## [0.1.2] - 2025

Initial published releases.
