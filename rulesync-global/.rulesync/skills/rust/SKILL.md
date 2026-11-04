---
name: rust
description: >-
  Design, implement, refactor, review, and test Rust code. Use for .rs files, Cargo
  dependencies, type and ownership design, async state coordination, synchronization
  (Mutex, RwLock, atomics, channels), Serde, Facet, thiserror, rustdoc, FFI, dynamic
  libraries, and hot reload.
---

# Rust

Apply every required convention to each Rust change. Read a reference only when the change matches its trigger in the reference table.

## Crate Research

- Research new crates and their APIs through the `docs` MCP server (`docs-mcp`) before falling back to GitHub source.

## Required conventions

### Compile-time guarantees

An invariant checked only at runtime fails on a path that tests may never reach. Rust's type system turns the same violation into a `cargo check` error. Encode each invariant in a type so that misuse does not compile.

- Make success values valid by construction. Keep fields private and expose a fallible constructor or `TryFrom` that validates once at entry. Downstream consumers stay infallible unless they add a new failure source.
- Remember that a derived `Deserialize` or `Facet` impl bypasses the constructor. Deserialize a validated type through a raw type and `TryFrom` (Serde's `#[serde(try_from = "Raw")]`, or Facet's proxy) so loaded data passes the same validation.
- Before using `String` or `&str`, check whether an enum can represent the domain values. Use enums for known cases; reserve plain strings for genuinely free-form text or raw input at external boundaries.
- Use validated newtypes for structured values such as identifiers and quantities, so values sharing a primitive cannot be confused. A `type` alias does not provide this distinction.
- Replace `bool` parameters, string discriminants, and flag fields with co-dependent `Option`s with an enum whose variants hold exactly the valid combinations.
- Represent absence with `Option`, never a sentinel such as `-1`, `0`, or `""`.
- Convert numbers with `From`, or with `TryFrom` when the value may not fit. Never use `as` where it can truncate, wrap, or change sign.
- Match enums the crate owns exhaustively, without a `_` arm, so a new variant fails compilation at every consumer.
- Destructure source structs completely in conversions, so a new field fails compilation. Never fill fields that a conversion must consider with `..Default::default()`.
- Consume `self` to encode a required operation order when calling out of order is a bug. Use a state enum instead when transitions come from runtime data.
- Tie paired operations, such as register and unregister, to a guard whose `Drop` performs the release.
- Keep concrete types through abstractions with generics or associated types. Use `dyn Trait` only for heterogeneous collections or plugin boundaries, never recovered by downcasting.
- Reserve `expect` for invariants the type system cannot express, and state the invariant in its message.

### Typed boundaries

Stringly typed contracts compile while hiding unhandled cases, misspelled kinds, and missing fields until runtime. Every trait, adapter, event, and message boundary carries typed values.

- Model a closed set of cases as an exhaustive enum with typed payloads. Never use a `kind: String` discriminant, a `serde_json::Value` payload, or a struct of `Option` fields that consumers interpret by kind.
- Return typed values from trait methods and contract types, never strings that callers parse or compare.
- Parse external protocol data inside the adapter that receives it. Provider vocabulary, JSON paths, and wire spellings never cross the adapter boundary.
- Keep identity that every case requires non-optional.
- Give a field with an open external vocabulary explicit `Missing` and `Unknown(raw)` states. Never add a catch-all variant that defers parsing to consumers.
- Never implement `Deref<Target = str>` or comparable string escape hatches on a typed value.

A contract that consumers decode by string:

```rust
pub struct BackendEvent {
  pub kind: String,
  pub text: Option<String>,
  pub data: serde_json::Value,
}
```

must instead carry each case with exactly its typed payload:

```rust
pub enum BackendEvent {
  Message { address: MessageAddress, update: MessageUpdate },
  Tool { address: ToolAddress, update: ToolUpdate },
}
```

### Encapsulation

Each type owns and protects its invariants. Expose read-only access through `&T` and mutation methods that preserve validity without exposing implementation details.

- Never expose `&mut` access or `pub` fields for state that carries an invariant. Provide mutation methods that preserve it.
- Return `&[T]`, `&str`, or iterators instead of `&Vec<T>`, `&String`, or internal collections.
- Combine check-then-act sequences into one operation, such as get-or-create.
- Complete every fallible step before assigning in a `&mut self` method, so an error leaves the value unchanged.
- Keep interior mutability private. Never return `RefMut`, lock guards, or other borrows of internal state from a public method.

### Modules

- Keep `mod.rs` limited to `mod` declarations and re-exports.
- Start one owned concept in one file. Promote it to a directory only when distinct child modules emerge. Name the directory after the shared feature area so no child repeats its name. Never split a file only to reduce its length.
- Order items after imports: constants, type aliases, enums, struct clusters, free functions, then the `#[cfg(test)]` module. Place exported items before private items in each group.
- Follow each struct with its inherent `impl`, then its trait `impl`s, in the module that defines it. Order inherent methods `pub`, restricted visibility, then private.
- Keep visibility as narrow as real consumers permit. Never widen visibility for a test.

### Naming

Name methods `noun_verb`, placing the target before the operation. Omit the noun when the receiver makes the target unambiguous (`get`, `get_mut`); otherwise, qualify only as much as needed (`source_get`, `asset_get`, `source_asset_get`). Keep target names consistent across related operations (`source_get`, `source_get_mut`, `source_remove`).

- Name fields by their contents (`sources`, `assets`), never `data`. Name monotonic counts `<domain>_count`.
- Put context parameters first after the receiver.
- Do not add a `with_*` constructor for a single caller or a `Typed` prefix to a type name.

### Serialization

- Inspect the library's existing serialized types before adding one. Use Serde when any existing serialized type uses it. Otherwise use Facet. Never add a parallel serialization stack.
- Never change a persisted or wire representation as a side effect of a Rust refactor.
- Check the installed serialization crate's documentation before using a derive or attribute.

### Async mutations

Centralize related state changes so their ordering and interactions are easy to reason about. In complex async flows, have handlers record intent and let one coordinated update path apply changes. Consider commands with completion promises when the state owner should control execution order while callers await results. Use command queues, update passes, or schemas where they clarify that coordination.

### Control flow

- Return early from terminal paths. Drop the `else` after a returning branch.
- Never add a helper that only forwards a call or wraps one error conversion.

### Errors

- Inspect the library's existing error types before adding one. Use thiserror when any existing error type uses it. Otherwise use Facet. Never add a parallel error stack.
- Define Facet errors with `#[derive(Debug, Facet)]` and `#[facet(derive(Error))]`. Variant doc comments become the runtime `Display` text, so editing them changes behavior.
- Never return `Result<T, String>` or flatten a typed cause in a domain API. Render error text only at a UI, log, command, or transport boundary.
- Use `anyhow` only where no caller distinguishes failures, such as binary entry points and top-level orchestration. Library and domain APIs return typed errors.
- Check the installed error crate's documentation before using a derive or attribute.


### Documentation

- Give every function a concise `///` comment that states its responsibility, contract, or non-obvious reason to exist. Document every non-function item visible outside its module.
- Start doc comments with `Get` for accessors, `Create` for constructors, and a precise imperative verb otherwise. Never start with `Return` or `Returns`.
- Place every doc comment above all attributes on its item, field, or variant, including `#[derive]`, `#[serde]`, `#[cfg]`, and attribute macros. Never place it between an attribute and the item.
- Keep each `// SAFETY:` comment directly above its unsafe operation.
- Use the technical-writing skill's API Documentation profile for rustdoc.

### Comments

Prefer types, names, and control flow over comments. Add an inline comment only when correctness depends on context the code cannot express.

- Comment domain rules, safety preconditions, concurrency protocols such as lock order or atomic ordering, external API or platform quirks, and performance choices that look wrong without context.
- Place the comment directly above the statement or branch that enforces the constraint. State the invariant and the failure it prevents.
- Never narrate what the syntax already says, such as `// Increment the retry count`.
- In a function of 40 or more nonblank lines that combines at least two of validation, state mutation or publication, and reconciliation or cleanup, label each phase with an imperative comment that names its state boundary. Do not append formulaic `so ...` clauses.
- Use `//` line comments. Reserve multi-line comments for safety rules or synchronization sequences with several preconditions.
- Delete a comment when a refactor moves its constraint into a type or API. Recheck nearby comments after changing the code they describe.

```rust
// Flush before releasing the lock, or workers read the stale buffer.
writer.flush()?;
drop(guard);
```

## References

| Reference | Read before |
| --- | --- |
| [invariant-apis.md](references/invariant-apis.md) | Designing public methods that read or mutate state, accessors, guards, transactions, or interior mutability |
| [typed-boundaries.md](references/typed-boundaries.md) | Defining or changing traits, adapters, events, protocol messages, or code that parses external data |
| [module-organization.md](references/module-organization.md) | Splitting, moving, or promoting modules, ordering trait implementations, or changing re-exports |
| [type-modeling.md](references/type-modeling.md) | Adding domain types, newtypes, enums, state transitions, behavioral owners, private state aggregates, `Result` success or failure types, retained fields, constructors, operation names, or type-erased stores |
| [control-flow.md](references/control-flow.md) | Restructuring method chains, naming intermediate stages, or extracting fallible helpers |
| [error-handling.md](references/error-handling.md) | Adding or reshaping error types, error derives, or error conversions |
| [serialization.md](references/serialization.md) | Adding serialization derives or changing persisted or wire representations |
| [documentation.md](references/documentation.md) | Writing doc comments, safety comments, or Facet error messages |
| [synchronization.md](references/synchronization.md) | Adding or restructuring locks, atomics, channels, completion queues, cancellation, or shared mutable state |
| [dynamic-libraries.md](references/dynamic-libraries.md) | Changing Rust dylibs, FFI, native loading, host/plugin contracts, or hot-reload unload order |
| [testing.md](references/testing.md) | Adding, moving, or removing tests |


## Testing

Sparingly use add unit tests for algorithms, parsers, and codecs only. Mock external boundaries when isolation proves the owned logic.

Most tests should be integration, include tests on coverage of lifecycle, persistence, concurrent handoff, plugins, rendering, and host/library boundaries.

### Place tests at their boundary

Keep unit tests in the inline `#[cfg(test)]` module of the file they cover.

Keep crate-private integration workflows in the owning module's test module, so they exercise production composition without exposing implementation details. Use the crate's `tests/` directory for public contracts and multi-crate boundaries. Follow the existing fixture layout.

## Verification

- Validate in the order `cargo check`, `cargo test`, `cargo fmt --check`, `cargo clippy`, `cargo build`. Run `cargo check` on the affected package and target first, and fix compiler errors before running tests. After a compilation failure, repeat `cargo check` before retrying a test.
- Never run broad automatic-fix commands such as `cargo clippy --fix` or `cargo fix`.
- Finish source-changing preparation, such as code generation, before validation starts. Repeat the affected checks after any later source change. If a formatting fix changes source, restart from `cargo check`.
- Pass `cargo check`, `cargo clippy`, `cargo test`, and `cargo build` the same arguments as the project's development build. A mismatched argument invalidates the build cache and recompiles the dependency graph.
- Do not add `--all-targets`, `--tests`, `--all-features`, `--release`, or a separate `--target-dir` by default. Select a named integration-test executable (`--test <name>`) only when the requested coverage requires it.
- Select the smallest test coverage that exercises the change. Run individual tests with `cargo test -p <package> <filter>`, batching related tests in one invocation.
