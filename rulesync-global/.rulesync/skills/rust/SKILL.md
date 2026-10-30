---
name: rust
description: >-
  Design, implement, refactor, review, and test Rust code. Use when changing .rs
  files, Cargo dependencies, modules, types, ownership, control flow, Facet, Serde,
  or thiserror derives, error handling, rustdoc, or tests. Also use for Mutex, RwLock,
  atomics, channels, shared-state publication, Rust dynamic libraries, FFI, and hot reload.
---

# Rust engineering

Apply every required convention to each Rust change. Read a reference only when the change matches its trigger in the reference table.

## Required conventions

### Compile-time guarantees

An invariant checked only at runtime fails on a path that tests may never reach. Rust's type system turns the same violation into a `cargo check` error. Encode each invariant in a type so that misuse does not compile.

- Make success values valid by construction. Keep fields private and expose a fallible constructor or `TryFrom` that validates once at entry. Downstream consumers stay infallible unless they add a new failure source.
- Remember that a derived `Deserialize` or `Facet` impl bypasses the constructor. Deserialize a validated type through a raw type and `TryFrom` (Serde's `#[serde(try_from = "Raw")]`, or Facet's proxy) so loaded data passes the same validation.
- Wrap identifiers and quantities in newtypes when values of one primitive type can be confused, so swapped arguments fail to compile. A `type` alias does not count, because it stays interchangeable with its target.
- Replace `bool` parameters, string discriminants, and flag fields with co-dependent `Option`s with an enum whose variants hold exactly the valid combinations.
- Represent absence with `Option`, never a sentinel such as `-1`, `0`, or `""`.
- Convert numbers with `From`, or with `TryFrom` when the value may not fit. Never use `as` where it can truncate, wrap, or change sign.
- Match enums the crate owns exhaustively, without a `_` arm, so a new variant fails compilation at every consumer.
- Destructure source structs completely in conversions, so a new field fails compilation. Never fill fields that a conversion must consider with `..Default::default()`.
- Consume `self` to encode a required operation order when calling out of order is a bug. Use a state enum instead when transitions come from runtime data.
- Tie paired operations, such as register and unregister, to a guard whose `Drop` performs the release.
- Keep concrete types through abstractions with generics or associated types. Use `dyn Trait` only for heterogeneous collections or plugin boundaries, never recovered by downcasting.
- Reserve `expect` for invariants the type system cannot express, and state the invariant in its message.

### Encapsulated invariants

A type that exposes its internals makes every caller responsible for its invariant, and one careless caller corrupts it for all. Each type owns its invariant. Callers read through `&T` and change state only through methods that keep it valid, without knowing how it is kept.

- Never expose `&mut` access or `pub` fields for state that carries an invariant. Provide mutation methods that preserve it.
- Return `&[T]`, `&str`, or iterators instead of `&Vec<T>`, `&String`, or internal collections.
- Combine check-then-act sequences into one operation, such as get-or-create.
- Complete every fallible step before assigning in a `&mut self` method, so an error leaves the value unchanged.
- Keep interior mutability private. Never return `RefMut`, lock guards, or other borrows of internal state from a public method.

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

### Modules and declarations

- Keep `mod.rs` limited to `mod` declarations and re-exports.
- Start one owned concept in one file. Promote it to a directory only when distinct child modules emerge. Name the directory after the shared feature area so no child repeats its name. Never split a file only to reduce its length.
- Order items after imports: constants, type aliases, enums, struct clusters, free functions, then the `#[cfg(test)]` module. Place exported items before private items in each group.
- Follow each struct with its inherent `impl`, then its trait `impl`s, in the module that defines it. Order inherent methods `pub`, restricted visibility, then private.
- Keep visibility as narrow as real consumers permit. Never widen visibility for a test.

### Naming

- Name fields by their contents (`sources`, `assets`), never `data`. Name monotonic counts `<domain>_count`.
- Put context parameters first after the receiver.
- Do not add a `with_*` constructor for a single caller or a `Typed` prefix to a type name.

### Control flow

- Return early from terminal paths. Drop the `else` after a returning branch.
- Never add a helper that only forwards a call or wraps one error conversion.

### Errors

- Inspect the library's existing error types before adding one. Use thiserror when any existing error type uses it. Otherwise use Facet. Never add a parallel error stack.
- Define Facet errors with `#[derive(Debug, Facet)]` and `#[facet(derive(Error))]`. Variant doc comments become the runtime `Display` text, so editing them changes behavior.
- Never return `Result<T, String>` or flatten a typed cause in a domain API. Render error text only at a UI, log, command, or transport boundary.
- Use `anyhow` only where no caller distinguishes failures, such as binary entry points and top-level orchestration. Library and domain APIs return typed errors.
- Check the installed error crate's documentation before using a derive or attribute.

### Serialization

- Inspect the library's existing serialized types before adding one. Use Serde when any existing serialized type uses it. Otherwise use Facet. Never add a parallel serialization stack.
- Never change a persisted or wire representation as a side effect of a Rust refactor.
- Check the installed serialization crate's documentation before using a derive or attribute.

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


## Crate research

- Research new crates and their APIs through the `docs` MCP server (`docs-mcp`) before falling back to GitHub source.

## Verification

- Validate in the order `cargo check`, `cargo test`, `cargo fmt --check`, `cargo clippy`, `cargo build`. Run `cargo check` on the affected package and target first, and fix compiler errors before running tests. After a compilation failure, repeat `cargo check` before retrying a test.
- Finish source-changing preparation, such as code generation, before validation starts. Repeat the affected checks after any later source change. If a formatting fix changes source, restart from `cargo check`.
- Pass `cargo check`, `cargo clippy`, `cargo test`, and `cargo build` the same arguments as the project's development build: the same toolchain, `--target`, profile, `--features` selection, `RUSTFLAGS`, working directory, and target directory. Select the same package and binary or library for clippy and tests. A mismatched argument invalidates the build cache and recompiles the dependency graph.
- Do not add `--all-targets`, `--tests`, `--all-features`, `--release`, or a separate `--target-dir` by default. Select a named integration-test executable (`--test <name>`) only when the requested coverage requires it.
- Do not claim complete build-cache reuse between clippy and tests. Even with aligned arguments, the test harness and clippy analysis produce distinct artifacts.
- Select the smallest test coverage that exercises the change. Run individual tests with `cargo test -p <package> <filter>`, batching related tests in one invocation. Escalate to the whole package only when individual tests cannot cover the affected behavior, and to the workspace only for demonstrated cross-package impact or an explicit user request. State the reason before broadening. A passing targeted test, a compilation failure, or an unknown test name does not justify a broader run.
- For a slow test, prefer a name filter or a cached fixture. Run an unavoidable slow suite with an explicit timeout.
- Report the executed command, timeout, exit status, execution duration for slow runs, and the specific error output or generated artifact. Report unrelated warnings or failures separately and leave unrelated code untouched.
- Never run broad automatic-fix commands such as `cargo clippy --fix` or `cargo fix`.
- Never start a long-lived development process for verification.
