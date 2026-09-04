# API Documentation Profile

*Read when authoring or revising API reference docs, docstrings, rustdoc, JSDoc, OpenAPI schemas, or source-level interface documentation.*

## Controlling Claim

Establish that a caller can predict an interface's runtime behavior, error states, and invariant guarantees without inspecting its underlying source code.

## Structural Obligations

### 1. Type and Struct Documentation
- **Domain Role:** State the single conceptual role and responsibility of the type.
- **Ownership Boundary:** Define whether instances own, borrow, or share their state.
- **State Invariants:** Document conditions that remain true throughout the instance lifecycle.
- **Concurrency Contract:** State thread-safety, synchronization requirements, or `Send`/`Sync` guarantees.
- **Lifecycle:** Document construction, active state transitions, and disposal/cleanup obligations.

### 2. Property and Field Documentation
- Describe a stored property or field with a concise noun phrase that states its value and lifecycle
  boundary.
- Omit articles unless they disambiguate the value.
- Avoid generic action verbs such as `Identifies`, `Reports`, `Contains`, and `Represents` when
  access performs no action.

Good:

```rust
/// Root asset whose graph was removed.
pub(crate) root: RootAssetKey,
```

Bad:

```rust
/// Identifies the root asset whose graph was removed.
pub(crate) root: RootAssetKey,
```

### 3. Callable Documentation (Functions, Methods, RPCs)
- **Action Line:** Begin with a concise, present-tense description of the caller-visible operation (such as `Executes a batch sync against the remote catalog.`).
- **Arguments and Inputs:**
  - Name parameter roles directly when pronouns could be ambiguous.
  - Specify valid value bounds, required formats, encoding expectations, and default values.
  - Document ownership transfer, borrowing, or lifetime constraints.
- **Return Values:** Describe the observable payload, structure, and representation guarantees on success.
- **Error and Failure Modes:**
  - Enumerate every error type, exception, panic condition, or HTTP status code.
  - State the exact condition or invalid input that triggers each failure.
  - State whether the operation is atomic or leaves partial state on failure.
- **Side Effects and Idempotency:** Document external mutation, disk I/O, network traffic, background task spawning, and whether repeat calls are idempotent.

### 4. Trait, Interface, and Protocol Documentation
- Document the capability contract that implementers must fulfill.
- State required invariants and ordering rules between interface methods.
- Document default method implementations and whether implementers should override them.

### 5. Executable Documentation and Schema Generation
- Treat documentation consumed by schema generators, OpenAPI compilers, or discovery endpoints as executable contracts.
- Preserve required annotations, tags, format markers, and parameter descriptions required by tooling.

## Exclude by Default

- Do not restate function signatures, parameter names, or type annotations in prose (avoid `s: string - a string`).
- Do not list member methods inside type documentation.
- Do not expose private collaborators, internal memory layouts, private locking strategies, or internal branch decisions unless callers directly interact with them.

## Completion Check

A caller should understand the item's role, valid usage parameters, return guarantees, failure conditions, and concurrency safety without opening the implementation source.
