# Authoring `docs/walkthrough.json`

`docs/walkthrough.json` is an ordered JSON array of inline review comments rendered above declarations in `PlanReview`. Reviewers step through entries `1..N` in order. `PlanReview` sorts files by their earliest comment index.

## Schema

- `path` (required): changed repository-relative path (destination for renames, original for deletions).
- `symbol` (required): qualified declaration name such as `Worker::start` (an unambiguous short name also works, and deleted files use baseline declarations).
- `title` (required): 2 to 5 plain-text words naming the action.
- `comment` (required): nonblank Markdown rendered above the symbol.

## Selection and Order

Order entries along the runtime path from entry point through setup, execution, and failure boundaries. In a multi-concern file, place one comment per distinct decision on its owning symbol (revisiting that file later is fine when following a caller-to-callee handoff).

- **Open at the top-level entry point,** such as registration, route, command, or public constructor.
- **Cover key runtime steps:** Include each major step through setup, initialization, and execution.
- **Call out limits and exceptions:** Include non-obvious constraints, capability limits, protocol quirks, fail-closed checks, etc.
- **Skip noise:** Omit mid-stack pass-through wrappers whose target helper already has a comment, mechanical parameter or error-attribution plumbing, module declarations, and tests.

## Style

Write 1 to 2 succinct imperative sentences per comment. Anchor strictly to what the diff at that symbol changes, never unchanged surrounding code or another function's work. Use `<action>` when self-explanatory and `<action>  <reason>` when the reason is non-obvious. When a `Change` or `Algorithm` block hides concrete rules behind an abstract call, spell out the limits.

## Examples

### Registration and capability limits (Scoped API tokens)

```json
[
  {
    "path": "src/http/routes.rs",
    "symbol": "register_account_routes",
    "title": "Register token API routes",
    "comment": "Register `POST`, `GET`, and `DELETE /api/tokens` on the account router."
  },
  {
    "path": "src/auth/token_store.rs",
    "symbol": "TokenStore::create",
    "title": "Store prefix and SHA-256 hash",
    "comment": "Generate an `app_live_` prefix plus 32 random bytes, store only the prefix and SHA-256 hash, and return the plaintext once. The visible prefix makes log correlation possible without ever keeping raw secrets on disk."
  },
  {
    "path": "src/auth/token_service.rs",
    "symbol": "TokenService::issue",
    "title": "Require explicit scope allowlist",
    "comment": "Require a nonempty scope allowlist on creation and cap access at the user's current role. Automation jobs usually need one narrow action, limiting the blast radius of a leaked CI secret."
  },
  {
    "path": "src/auth/middleware.rs",
    "symbol": "AuthMiddleware::authenticate",
    "title": "Intersect token scopes with role",
    "comment": "Reject revoked or expired hashes with `401` and intersect the token's scopes with the owning user's role. Route guards then return `403` on missing scopes without changing downstream handlers."
  },
  {
    "path": "src/auth/token_store.rs",
    "symbol": "TokenStore::touch_last_used",
    "title": "Throttle last-used writes",
    "comment": "Throttle `last_used_at` writes to once per hour to avoid write contention on the tokens table during bursty CI runs."
  }
]
```

### Multi-concern module and private hooks (Scene and snapshot plugins)

```json
[
  {
    "path": "crates/scene/src/plugin.rs",
    "symbol": "ScenePlugin::build",
    "title": "Register opt-in ScenePlugin",
    "comment": "Register `ScenePlugin` as an opt-in plugin owning scene storage, loading, and scene commands. Apps that do not use authored scenes no longer pull in scene dependencies."
  },
  {
    "path": "crates/runtime/src/engine.rs",
    "symbol": "Engine::defer",
    "title": "Expose frame-boundary deferral",
    "comment": "Make frame-boundary deferral public via `Engine::defer`. Now that scene loading lives outside core, external plugins need a way to schedule ordered world mutations from read-only systems and gateway commands."
  },
  {
    "path": "crates/runtime/src/persistence.rs",
    "symbol": "RestoreTransaction::commit",
    "title": "Validate all hooks before swap",
    "comment": "Require an exact match on registered persistence keys and run every prepare hook against a read-only candidate world before swapping. Any mismatch or decode error discards the candidate and leaves live state intact."
  },
  {
    "path": "crates/scene/src/loader.rs",
    "symbol": "SceneLoader::commit_load",
    "title": "Guard against stale async loads",
    "comment": "Re-check that the root still wants this asset and revision before committing an async load. Late loads can arrive after a root switches scenes or unloads, and committing them would corrupt live entities."
  },
  {
    "path": "crates/snapshot/src/storage.rs",
    "symbol": "SnapshotStorage::load_reload_state",
    "title": "Consume reload file once",
    "comment": "Clear `reload-state.json` on startup even when restore fails, while keeping named `snapshot-{name}.json` files on disk after load. Consuming reload state once prevents a broken reload file from crash-looping startup."
  },
  {
    "path": "crates/editor/src/snapshot_proxy.rs",
    "symbol": "EditorUiStateProxy",
    "title": "Preserve typed editor IDs",
    "comment": "Decode `EditorUiState` through a dedicated snapshot proxy. Reflected decoding cannot round-trip its typed IDs directly without altering `EditorSceneDocument`."
  }
]
```

### Small focused optimization (Coalesce concurrent cache misses)

```json
[
  {
    "path": "src/cache/metadata.rs",
    "symbol": "MetadataCache::get",
    "title": "Keep cache hits lock-light",
    "comment": "Check the cache under a read lock first and enter the sharded flight table only on a miss. Concurrent hits stay on the read-only fast path without contending on in-flight RPCs."
  },
  {
    "path": "src/cache/flight.rs",
    "symbol": "FlightTable::join_or_spawn",
    "title": "Share one upstream fetch per key",
    "comment": "Spawn one detached broadcast fetch per missing key and remove the flight entry before broadcasting the result. Clearing the entry first ensures callers arriving after a transient failure start a fresh fetch rather than seeing the stale error."
  },
  {
    "path": "src/cache/flight.rs",
    "symbol": "FlightGuard::drop",
    "title": "Cancel only after last waiter drops",
    "comment": "Detach the fetch task from the first caller's future and cancel it only when the final waiter drops. An early timeout on the first caller lets remaining followers finish without restarting the RPC."
  }
]
```
