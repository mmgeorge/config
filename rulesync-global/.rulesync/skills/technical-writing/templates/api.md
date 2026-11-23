# API Profile

*Read when writing or revising API reference pages, docstrings, rustdoc, JSDoc, OpenAPI descriptions, CLI references, config schemas, or other documentation that sits next to an interface.*

## Goal

Let a caller predict what an interface does, how it fails, and what invariants hold without reading its implementation.

## Sections

Document each kind of item as follows.

### Types

- State the type's role in one sentence.
- Document invariants, state sharing across clones, cleanup requirements, and concurrency or thread-affinity rules when the type signature does not already express them.

### Fields and properties

- Describe a field with a short noun phrase that names its value and when that value is valid.
- Leave out articles unless they remove ambiguity.
- Do not start with verbs such as "Identifies," "Reports," "Contains," or "Represents," because reading a field does nothing.

Good:

```rust
/// Delay before retrying a rate-limited request, or `None` when the server omitted `Retry-After`.
pub retry_after: Option<Duration>,
```

Bad:

```rust
/// Represents the delay before retrying a rate-limited request.
pub retry_after: Option<Duration>,
```

### Functions and methods

- Open with one sentence starting with an imperative verb (`Load`, `Validate`, `Consume`) stating what the call does.
- Document non-obvious parameter bounds, formats, or defaults, and describe what a successful call returns without repeating the signature.
- List failure modes (errors, panics, status codes), what triggers each, and whether a failure leaves partial state behind.
- Note side effects (disk writes, network calls, background tasks) and idempotency when applicable.

### Traits and interfaces

- State what every implementation must do, and any required order between methods.
- Describe each default method and when an implementation should override it.

### Generated documentation

- When tools read the documentation to generate schemas or API listings, keep the annotations, tags, and formats those tools require.

## Leave out

- Do not repeat the signature in prose, such as "`s: string`: a string."
- Do not list a type's methods in the type's own documentation.
- Do not describe private helpers, memory layout, locking, or internal branches unless callers deal with them directly.

## Example

```rust
/// Per-key request limit enforced by a [`RateLimiter`].
///
/// Immutable once constructed and cheap to copy.
#[derive(Clone, Copy)]
pub struct Quota {
  /// Maximum burst of tokens available at once. Must be at least `1`.
  pub burst: u32,
  /// Steady-state refill rate in tokens per second. Must be greater than `0`.
  pub refill_per_second: u32,
}

/// Token bucket limiter keyed by caller identity.
///
/// Bucket state stays local to the current process and resets on restart.
/// Clones share the same buckets across threads.
#[derive(Clone)]
pub struct RateLimiter {
  /* ... */
}

impl RateLimiter {
  /// Consume `cost` tokens from `key`'s bucket, creating the bucket on first use.
  ///
  /// Returns the remaining token count after deduction. Fails with
  /// [`LimitError::Exceeded`] carrying the earliest retry delay when the bucket
  /// holds fewer than `cost` tokens, leaving the balance unchanged. Fails with
  /// [`LimitError::InvalidCost`] when `cost` is `0` or exceeds the configured
  /// burst size.
  pub fn acquire(&self, key: &str, cost: u32) -> Result<u32, LimitError> {
    /* ... */
  }
}
```
