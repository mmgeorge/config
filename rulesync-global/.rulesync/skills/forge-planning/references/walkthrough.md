# Authoring `docs/walkthrough.json`

`docs/walkthrough.json` holds an ordered JSON array of inline review comments rendered directly inside the `Changes` diff in `PlanReview`. Reviewers step through these comments in order (`1..N`) to explore the plan's design decisions without scanning every file top to bottom.

## Schema and PlanReview Behavior

Each element in the array is a JSON object with four fields:

- `path` (required): repository-relative path to a changed file. Use the destination path for a rename and the original path for a deletion.
- `symbol` (required): qualified declaration name in that file (such as `Worker::start`). An unambiguous short name works when it identifies one declaration. For a deleted file, name a declaration from its baseline.
- `title` (recommended): 2 to 5 plain-text words naming the action (for example `Acquire worker lease`). `PlanReview` renders it beside the step count in the frame border and falls back to `Walkthrough` when omitted.
- `comment` (required): nonblank Markdown rendered above the anchored declaration.

Give each `(path, symbol)` anchor at most one comment. An empty array `[]` is valid for trivial single-change plans that need no guided tour.

`PlanReview` uses `docs/walkthrough.json` in three ways:

- **File sorting:** Files in `Unviewed Changes` sort by their earliest comment index. Uncommented files trail afterward in dependency order.
- **Step navigation:** Next and previous comment actions step strictly in array order (`1..N`), auto-unfolding the destination file on jump.
- **`public_only` visibility:** Private files and private declarations anchored by a walkthrough comment surface automatically in `public_only` mode, while unannotated private code stays hidden.

## What to Annotate

Annotate the symbols that implement the distinct design decisions in `docs/overview.md`. Do not annotate every changed function.

- **Include:** Entry points, capability or security boundaries, non-obvious protocol or error handling, state isolation rules, cross-file invariants, and private helpers or files carrying real behavioral changes that `public_only` would otherwise hide.
- **Skip:** Thin routing or dispatch wrappers, mechanical parameter forwarding across unchanged call sites, module declarations, and routine test cases already summarized in `docs/tests.md`.
- **Match the actual diff:** Anchor each entry to the exact symbol whose diff implements that choice. Never describe unchanged surrounding behavior or fold a decision from another function into the same comment.

## Ordering

Follow the runtime causal path rather than the file tree:

1. Start at registration, entry points, and capability or policy boundaries.
2. Follow execution through setup, isolation, and initialization.
3. Cover live behavioral divergences, state transitions, and failure boundaries.
4. End with cross-cutting error attribution or a non-obvious test harness setup when relevant.

In a multi-concern file (such as a new adapter module or coordinator), place one symbol-level comment on each distinct decision at the declaration that owns it. Revisiting that file later in the array is fine when it clarifies a caller-to-callee handoff.

## Writing Style

Write 1 to 2 succinct sentences per comment in imperative mood:

- Use `<action>` when the change is self-explanatory.
- Use `<action> because <reason>` (or `<action>. <constraint>.`) only when a non-obvious constraint, unsupported capability, failure boundary, or cross-file handoff matters.
- Avoid filler `"so ..."` clauses and defensive `"instead of ..."` phrasing.
- When a `Change` or `Algorithm` block hides concrete rules behind an abstract pseudocode call, spell out the concrete limits or behavior in the comment rather than repeating the abstract name.

## Calibration

### Drop filler `"so ..."` on obvious actions

- **Avoid:** `Register the worker at startup so jobs queued before the first tick can resolve their handler.`
- **Prefer (obvious):** `Register the worker at startup.`
- **Prefer (non-obvious ordering constraint):** `Register the worker before starting the poller because queued jobs arrive immediately on boot.`

### Anchor on the implementation, not a pass-through wrapper

- **Avoid:** Annotating `router::dispatch` when its diff only adds `"parquet"` to an existing match arm.
- **Prefer:** Leave `router::dispatch` unannotated and anchor on `ParquetWriter::write_batch`, where streaming row groups and footer-abort behavior live.

### Spell out concrete limits hidden by abstract pseudocode

- **Avoid:** `Apply conservative token limits for automation credentials.`
- **Prefer:** `Require an explicit scope allowlist and cap effective access at the owning user's role because automation tokens must not inherit full account permissions.`

### Describe only what the diff at that symbol changes

- **Avoid (on a caller that only threads a new parameter):** `Acquire the database transaction and roll back on error.`
- **Prefer (on the symbol that adds the outbox write):** `Enqueue the webhook row inside the caller's database transaction, preventing orphaned webhooks on rollback.`

### Keep each comment scoped to its own symbol

- **Avoid:** Mentioning credential-path resolution from `apply_launch_env` inside a comment anchored on `create_profile`.
- **Prefer:** Keep `create_profile` focused on directory isolation and place a separate entry on `apply_launch_env` for credential-path resolution.

## Examples

These examples pair with the synthetic domains in `## Plan overview examples` of `SKILL.md`.

### Example 1: Multi-component backend change (Transactional webhook outbox)

```json
[
  {
    "path": "src/orders.rs",
    "symbol": "mark_paid",
    "title": "Enqueue inside order transaction",
    "comment": "Enqueue the `OrderPaid` webhook row inside the existing database transaction. Rolled-back order updates never emit webhooks."
  },
  {
    "path": "src/outbox/worker.rs",
    "symbol": "DeliveryWorker::claim_batch",
    "title": "Claim due rows with lease",
    "comment": "Claim due pending rows with `FOR UPDATE SKIP LOCKED` and stamp a lease expiry, allowing another worker to reclaim a row if the claiming process crashes mid-request."
  },
  {
    "path": "src/outbox/worker.rs",
    "symbol": "DeliveryWorker::send",
    "title": "Attach stable delivery ID",
    "comment": "Send `X-Delivery-Id` set to the row's event ID with the HMAC signature because retries provide at-least-once delivery and receivers deduplicate on that header."
  },
  {
    "path": "src/outbox/worker.rs",
    "symbol": "DeliveryWorker::settle",
    "title": "Classify retryable vs dead errors",
    "comment": "Mark HTTP `4xx` responses (other than `429`) dead immediately while applying exponential backoff up to eight attempts for network errors, timeouts, `429`, and `5xx`."
  },
  {
    "path": "src/outbox/store.rs",
    "symbol": "OutboxStore::purge_delivered",
    "title": "Purge delivered rows after 7 days",
    "comment": "Delete delivered rows older than 7 days to bound table growth while keeping a one-week debugging window."
  }
]
```

### Example 2: Auth and capability limits (Scoped API tokens)

```json
[
  {
    "path": "src/auth/token_store.rs",
    "symbol": "TokenStore::create",
    "title": "Store prefix and SHA-256 digest",
    "comment": "Generate a visible prefix (`app_live_` plus 8 characters) and 32 random bytes, persisting only the prefix and SHA-256 hash while returning the plaintext secret once."
  },
  {
    "path": "src/auth/token_service.rs",
    "symbol": "TokenService::issue",
    "title": "Require explicit scope allowlist",
    "comment": "Require a nonempty scope allowlist at creation because automation tokens must not inherit full account permissions."
  },
  {
    "path": "src/auth/middleware.rs",
    "symbol": "AuthMiddleware::authenticate",
    "title": "Intersect token scopes with role",
    "comment": "Reject revoked or expired hashes with `401 Unauthorized` and intersect granted token scopes with the user's current role, returning `403 Forbidden` on missing scopes."
  },
  {
    "path": "src/auth/token_store.rs",
    "symbol": "TokenStore::touch_last_used",
    "title": "Throttle last-used writes",
    "comment": "Update `last_used_at` at most once per hour because writing on every API call adds write contention under bursty CI traffic."
  }
]
```

### Example 3: Multi-concern extraction with private hooks (Scene and snapshot plugins)

```json
[
  {
    "path": "crates/runtime/src/engine.rs",
    "symbol": "Engine::defer",
    "title": "Expose frame-boundary deferral",
    "comment": "Expose `Engine::defer` for read-only systems and gateway commands to queue ordered mutable updates before the next system pass."
  },
  {
    "path": "crates/runtime/src/persistence.rs",
    "symbol": "RestoreTransaction::commit",
    "title": "Validate all hooks before swap",
    "comment": "Require an exact match on registered persistence keys and run every prepare hook against the read-only candidate world before committing, leaving live state intact on any failure."
  },
  {
    "path": "crates/scene/src/store.rs",
    "symbol": "SceneStore",
    "title": "Key asset scenes by root entity",
    "comment": "Key asset-backed scenes by root `EntityId` while keeping direct editor scenes keyed by `SceneId`, enabling multiple independent instances of the same scene file."
  },
  {
    "path": "crates/scene/src/loader.rs",
    "symbol": "SceneLoader::commit_load",
    "title": "Guard against stale async loads",
    "comment": "Re-check that the root still requests the same asset and revision before committing an async load because late completions must not overwrite a newer selection or revive an unloaded root."
  },
  {
    "path": "crates/snapshot/src/storage.rs",
    "symbol": "SnapshotStorage::load_reload_state",
    "title": "Consume reload file once",
    "comment": "Clear `reload-state.json` on startup even when restore fails while keeping named `snapshots/snapshot-{name}.json` files on disk after load."
  },
  {
    "path": "crates/editor/src/snapshot_proxy.rs",
    "symbol": "EditorUiStateProxy",
    "title": "Preserve typed editor IDs",
    "comment": "Decode `EditorUiState` through a dedicated snapshot proxy because reflected decoding cannot round-trip its typed IDs without changing `EditorSceneDocument`."
  }
]
```

### Example 4: Focused internal optimization (Coalesce concurrent cache misses)

```json
[
  {
    "path": "src/cache/metadata.rs",
    "symbol": "MetadataCache::get",
    "title": "Keep cache hits lock-light",
    "comment": "Check the cache under a read lock first and enter the sharded flight table only on a miss, keeping concurrent hits off the single-flight path."
  },
  {
    "path": "src/cache/flight.rs",
    "symbol": "FlightTable::join_or_spawn",
    "title": "Share one upstream fetch per key",
    "comment": "Spawn a detached fetch task with a broadcast channel on the first miss and subscribe subsequent waiters, removing the key from the flight table before broadcasting results because callers arriving after a transient failure need a fresh fetch."
  },
  {
    "path": "src/cache/flight.rs",
    "symbol": "FlightGuard::drop",
    "title": "Cancel only after last waiter drops",
    "comment": "Detach the fetch task from the first caller's future and cancel it only when the final waiter drops because an early caller timeout must not abort followers."
  }
]
```

## Pre-Submit Check

Before calling `plan_submit`, verify:

1. Every distinct design decision in `docs/overview.md` (including private helpers or files) has an entry anchored on the symbol that implements it.
2. Every comment matches the actual diff at its anchored symbol and spells out concrete limits hidden by abstract pseudocode.
3. Thin dispatchers, mechanical parameter plumbing, and routine tests listed in `docs/tests.md` are omitted.
4. No comment uses filler `"so ..."` clauses or invents reasons for self-explanatory actions.
