# Control flow

Read before restructuring method chains, naming intermediate stages, or extracting fallible helpers.

## Name domain stages

Use separate bindings when they name an operation stage, ownership transfer, state snapshot, or success/failure enrichment. Keep a compact expression when an intermediate name would describe only syntax. Line wrapping alone does not require extraction.

Capture a path before starting the request when it makes the sequence explicit:

```rust
let path = root.path().to_owned();
let response = self.service.get(path).await.map_err(AssetError::source)?;
```

Separate decoding from failure enrichment when the source snapshot must occur afterward:

```rust
let decoded = loader.decode(root.clone(), sources.clone()).await;
let result = decoded.map_err(|error| AssetLoadFailure {
  root,
  sources: sources.lock().clone(),
  error,
});
```

These fragments illustrate operation order. Follow the owning project's lock and error APIs. Name each binding after its domain value rather than a generic or numbered temporary.

## Extract an owned operation

Extract a helper when it owns a coherent fallible responsibility. A chain that constructs a key, acquires a guard, looks up a value, clones an owned handle, and produces a domain error forms one such boundary.

The caller can then describe its operation directly:

```rust
let decoder = self.decoder(&root, &response.content_type)?;
```

The helper returns an owned handle so its guard ends before decoding or asynchronous work. Return `Result` or `Option` when that lets the caller continue through `?` or `let else`.

Keep a lone error conversion visible at its call site. If the same conversion repeats across callers, fix the lower-level error contract instead of adding a wrapper.

## Compose before branching

When nested failure branches perform the same action, compose the stages before branching once. Use `?` inside an existing error boundary, or `and_then` when the caller owns the final decision.
