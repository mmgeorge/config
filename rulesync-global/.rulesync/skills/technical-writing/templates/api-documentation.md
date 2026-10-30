# API Documentation Profile

*Read when writing or revising API reference pages, docstrings, rustdoc, JSDoc, OpenAPI descriptions, or other documentation that sits next to an interface.*

## Controlling Claim

A caller can predict what the interface does, how it fails, and what stays true, without reading its source code.

## Sections

Document each kind of item as follows.

### 1. Types

- State the one job the type does.
- Say whether it owns its data, borrows it, or shares it.
- State the rules that always hold for an instance.
- Say whether it is safe to use from several threads at once.
- Describe how to create it, how its state changes, and what cleanup it needs.

### 2. Fields and properties

- Describe a field with a short noun phrase that names its value and when that value is valid.
- Leave out articles unless they remove ambiguity.
- Do not start with verbs such as "Identifies," "Reports," "Contains," or "Represents," because reading a field does nothing.

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

### 3. Functions and methods

- Start with one sentence that says what the call does, beginning with an imperative verb such as "Load" or "Validate."
- For each parameter, give its valid range, format, and default, and say whether ownership moves to the callee.
- Describe what a successful call returns.
- List every error, panic, or status code, the input that causes it, and whether a failure leaves partial changes behind.
- Mention side effects such as file writes, network calls, or background tasks, and say whether calling twice is safe.

### 4. Traits and interfaces

- State what every implementation must do, and any required order between methods.
- Describe each default method and when an implementation should override it.

### 5. Generated documentation

- When tools read the documentation to generate schemas or API listings, keep the annotations, tags, and formats those tools require.

## Leave out

- Do not repeat the signature in prose, such as "`s: string`: a string."
- Do not list a type's methods in the type's own documentation.
- Do not describe private helpers, memory layout, locking, or internal branches unless callers deal with them directly.

## Example

A documented function:

```rust
/// Load the document at `path`, or share the load already in progress.
///
/// The first call reads the file on a background task. Later calls for the same path receive the same handle until the load finishes.
/// Fails with `LoadError::NotFound` when no file exists at `path`, and leaves the cache unchanged.
pub fn load(&self, path: &DocumentPath) -> Result<DocumentHandle, LoadError> {
```
