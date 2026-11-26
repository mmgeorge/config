---
name: forge-planning
description: >-
  Author, revise, and submit implementation plans. Use only inside Forge Harness
  when an active Plan task supplies a planning directory. Covers declaration
  proposals, behavior blocks, design artifacts, test coverage, and submission diagnostics.
targets:
  - '*'
---

# Planning

Author the plan inside the supplied planning directory using normal read tools and your file-editing tool (such as `apply_patch`) with absolute paths. Never write planning files with shell commands such as `Set-Content`, `Out-File`, or output redirection.

## Workflow

1. **Investigate and decide.** Inspect the repository first and keep any existing draft on continuation. Resolve consequential design choices that the code cannot settle with `question_ask`. After a baseline refresh, compare the reported changes against existing proposals and revise before resubmitting.
2. **Author proposals in `files/`.** Capture every existing repository file with `plan_capture_file` before editing its proposal, create new proposals directly, and keep declarations consistent with the overview.
3. **Write artifacts in `docs/`.** Draft `docs/overview.md` to match the [plan overview examples](#plan-overview-examples) below, then write `docs/flows.md`, `docs/tests.md`, and `docs/checks.md`.
4. **Verify and submit.** Call `plan_status` to check the proposal inventory and dirty state, then call `plan_submit` with the active `plan_id` and no `expected_version`. Repair reported file and line diagnostics in place and submit again. Failed submissions preserve the draft.
5. **Wait for execution.** Further edits require resubmission before acceptance. Submission does not authorize editing repository files. Wait for Harness execution instructions.

## Directory layout

```text
<planning directory>/
  docs/overview.md   design narrative (starts empty)
  docs/flows.md      runtime flows (starts empty)
  docs/tests.md      test coverage (starts empty)
  docs/checks.md     saved check IDs (starts empty)
  files/<path>       one proposal per added, modified, or deleted repository file
```

`docs/` holds only these four files and no subdirectories. `files/` mirrors repository-relative paths (`src/main.rs` maps to `files/src/main.rs`). Any extra entry fails submission. Harness stores captured originals and review snapshots outside the planning directory.

## File proposals

Source proposals contain declarations, complete signatures, and documentation, never executable bodies. Configuration proposals contain complete file contents. Keep the design proportional to the request, verify cohesive ownership, and expose only what callers need.

- **Existing file:** Call `plan_capture_file` with its repository-relative path (no destination argument) before proposing changes. Harness extracts declarations into `files/<path>` and returns the absolute proposal path and a read-only original. Read the proposal and edit it in place: add `Change` under callables with changed behavior, add new declarations, and delete removed declarations. Capture refuses to overwrite an uncaptured proposal at that path, so never hand-write a proposal for an existing file.
- **New file:** Create `files/<path>` directly with its declarations, documentation, and behavior blocks.
- **Deleted file:** Capture the file first, then delete its proposal. Deleting a proposal you created withdraws the addition.
- **Renamed file:** Capture the file first, then move its proposal to the new path.

Repeated capture preserves edits and planned deletions. Restoring a proposal to its captured original withdraws the modification.

### Behavior blocks

Add one `Change` (one to two sentences stating what changes) under each modified callable, and one nonempty `Algorithm` under each callable with new behavior. Never attach both to the same callable, and leave unchanged callables without a block. Behavior classification is independent of file renames. Harness validates supplied blocks rather than deriving newness from path history. Name used symbols such as `Draft::revision` inside the block itself, and never add `Calls`, `Constructs`, or `Accesses` sections.

Place the standalone `Change` or `Algorithm` heading directly below the signature, indent its body, and leave a blank line after the block.

Write each `Algorithm` as compact academic pseudocode, one complete statement per line:

- Assign with `←` or `<-` and compare with `=`, `≠`, `<`, `≤`, `is`, or `is not` (for example, `if target is none`). Combine conditions with `and`, `or`, and `not`, and compute remainders with `%` or `mod`.
- Express each operation as a call such as `spawn_square(position)`. Lowercase abstract operations need no repository declaration. Name real functions, types, and fields directly when known.
- Indent the bodies of `if`, `otherwise`, `for each item in items`, and `while`. Use `return`, and use `break` and `continue` only inside loops.
- Construct values explicitly as `Type(field = value)`, `Velocity(direction)` for a tuple type, or `Marker()`, separate from calls that spawn or insert them.
- Put explanations only in `#` comments. Prose statements and prose conditions fail grammar validation.

Harness checks syntax, not expression types or algorithm correctness.

```text
/// Persist a draft and publish it to readers.
pub fn save(&mut self, draft: Draft) -> Result<()>;
Change
  Reject drafts whose Draft::revision is older than the stored revision.

/// Revision recorded when a draft is stored.
pub struct StoredRevision {
  /// Monotonic revision number of the stored draft.
  pub number: u64,
}

/// Report whether the stored draft supersedes the candidate.
fn is_stale(&self, draft: &Draft) -> bool;
Algorithm
  stored ← self.database.revision(draft.id)
  if stored is none
    return false
  return stored.number > draft.revision
```

### Documentation comments

Document every declaration the plan introduces—including struct fields and enum variants—directly above the declaration and above its attributes. Use the language's doc-comment syntax (`///` in Rust, `/** */` in TypeScript) and never start a doc comment with `"Returns"`.

## Design files

### `docs/overview.md`

Match the section depth, subsectioning, and plain declarative tone of the [plan overview examples](#plan-overview-examples) below. Write the overview as a standalone specification without referring to the user, prompt, or conversation, and keep it consistent with `files/`.

Submission requires only non-whitespace text, and PlanReview renders the submitted Markdown in authored order. Use `# <Short plan title>`, `## Objective`, `## Background`, and `## Design` as the default structure, adapting headings only when the change warrants it:

- **`## Objective`:** State what the change accomplishes and what remains unchanged. Leave verification and test strategy to `docs/tests.md`.
- **`## Background`:** Describe current behavior and the concrete problems or coupling it causes. When several distinct failures exist, list them as bold-lead bullets (see Examples 1, 2, and 4).
- **`## Usage`:** Include whenever user- or caller-facing behavior changes. Describe UI or workflow interactions and their observable results in prose (see Example 1). For APIs, commands, config entries, or shortcuts, include a short fenced snippet with expected results (see Examples 2–4). Omit this section for purely internal changes (see Example 5).
- **`## Decisions`:** Include whenever the plan chooses between real alternatives. Frame each bullet as `- **<Choice> instead of <alternative>.** <Why the alternative fails or costs more.>`, and place external protocol or documentation links here where they justify a constraint rather than appending citations to every paragraph.
- **`## Design`:** Open with a one-sentence architectural lead-in explaining how the central objects cooperate. Break multi-part designs into `###` subheadings by component, ownership boundary, or capability boundary, keeping each paragraph focused on one concern. Write declaratively in active voice rather than as an imperative checklist, describe important failure cases and compatibility boundaries, and leave file-by-file module layouts, per-callable mechanics, and incidental wire keys to `files/`.
- **`## Manual checks`:** Include only for observations automated checks cannot establish. Every dash-list item under this heading becomes a Verify action, so omit the section rather than writing `"None"`. Indent list continuation lines by two spaces.

```markdown
# <Short plan title>

## Objective
<What the change must accomplish and what stays unchanged.>

## Background
<Current behavior and the specific problems it causes.>

## Usage
<How a user or caller invokes the change and what they observe: plain prose for UI workflows, or a short fenced snippet for APIs, commands, or config.>

## Decisions
- **<Choice> instead of <alternative>.** <Why the alternative fails or costs more.>

## Design
<One-sentence architectural lead-in, followed by `###` subheadings by component or boundary for multi-part changes.>

## Manual checks
- <Action to perform and expected observation.>
```

### `docs/flows.md`

Write one `## Title` per independently triggered flow, a one-to-two-line description naming its trigger, and one closed ```` ```mermaid ```` block. Keep independent lifecycles separate. Use `sequenceDiagram` for cross-owner flows, `flowchart` for branching within one owner, and `stateDiagram-v2` for lifecycle transitions.

### `docs/tests.md`

Write one `## path/to/file` heading per file with test entries in the format below, using `new`, `modified`, `removed`, or `reused`. Name each test by the title its source declares, optionally preceded by its enclosing group titles. Describe the scenario and expected result at the owning boundary, choosing isolated or integration tests to match the behavior verified. Omit `[target: ...]` unless the same test name matches cases across multiple test checks. A target is a test check's ID or label from `check_definition_list`, never a runner or tool name.

```text
- new `exact_test_name`: scenario and expected result.
- new `exact_test_name` [target: unit]: scenario and expected result.
```

### `docs/checks.md`

List one saved check ID per line to select checks for plan execution. Discover IDs with `check_definition_list`. Harness adds prerequisites automatically. This file does not configure or authorize checks.

## Prose

Write for an engineer who is new to this codebase. Build intuition for how the system works: ground every claim in proposed implementation, explain what the system does and why its boundaries exist, and connect paragraphs by cause and consequence.

- **Write in a calm, developer-to-developer voice.** Explain constraints matter-of-factly as operational realities.
- **Keep paragraphs focused and prose-first.** Split heavy clauses into short sentences, and explain related components in subsequent paragraphs—using bullets or tables for parallel items, ordered steps, or comparisons.
- **Prefer active voice.** Name the acting component (*"The draft store invalidates the cache on save"*) rather than hiding ownership in passive voice (*"The cache is invalidated on save"*).
- **Prefer present indicative for the design, and conditional mood for counterfactuals.** Describe implemented and target architecture with definite subjects in the present indicative (`The scheduler runs...`). Use conditional mood (`would`, `could`) for rejected alternatives or unmitigated failure scenarios.

**Choppy (disconnected facts):**

> `BuildRunner` compiles into `StagingDir`. Then it calls `Path::rename`. `DevHost` loads the binary from `ActiveDir`.

**Readable (causal flow in plain terms):**

> The build runner compiles each update into a staging directory and moves only finished binaries into the active directory. That keeps the running host from loading an incomplete or failed build.


## Plan overview examples

Reference examples of concise `docs/overview.md` files. Match this plain, causal tone and keep section depth proportional to the change.

### Example 1: UI feature

````markdown
# Parquet price history export

## Objective

Add Parquet as a selectable format in Tickline's historical price export menu, letting analysts download typed, compressed OHLCV bar histories for backtesting notebooks. Existing CSV exports, chart intervals, and price-adjustment calculations remain unchanged.

## Background

Today, Tickline exports historical stock bars only as CSV.

Downloading multi-year histories or watchlists in CSV causes two problems:

- **Slow downloads and notebook loads.** Text encoding inflates multi-year minute-bar series into large files that transfer slowly over HTTP and spend seconds in text parsing when loaded into DuckDB, Polars, or Pandas.
- **Schema and precision drift.** CSV flattens UTC timestamps, fixed-scale prices, and integer share volumes into untyped strings, so analysts must specify column types manually or risk binary floating-point rounding on prices.

## Usage

In Tickline, select a ticker or watchlist, set the date range and bar interval, open the **Export** menu, and choose **Parquet**. The browser downloads a `.parquet` file containing typed UTC timestamps, exact fixed-scale prices, and share volumes ready to query directly without schema hints. If the market-data cursor fails mid-export, the browser reports an interrupted download and the toolbar displays an inline export error rather than saving a truncated file.

## Decisions

- **Stream row groups directly to the HTTP response instead of spooling a full `.parquet` file on server disk.** Spooling multi-year minute-bar exports would add disk pressure under concurrent downloads and delay first-byte delivery. A single-pass writer tracks row-group byte offsets in memory and appends the Parquet footer once the bar cursor drains.
- **Store prices as fixed-precision decimals (`DECIMAL(18, 4)`) instead of 64-bit floats.** Binary floats would introduce rounding drift during split adjustments and cumulative return calculations, whereas fixed-scale decimals preserve exact exchange tick values across Parquet readers.
- **Compress row groups with Zstd instead of Snappy or uncompressed pages.** Monotonic timestamps, repeated ticker symbols, and clustered price values compress tightly under Zstd at low CPU cost, reducing download size while remaining compatible with DuckDB, Polars, and PyArrow.

## Design

The export service plugs a Parquet encoder into the existing format dispatcher, reading the same paginated OHLCV bar cursor used by CSV exports without buffering the full date range in memory.

### Bar Schema

Before writing rows, the export handler initializes a fixed Parquet schema covering `symbol` (dictionary-encoded UTF-8), `timestamp` (UTC millisecond timestamp), `open`, `high`, `low`, `close`, and nullable `adjusted_close` (`DECIMAL(18, 4)`), and `volume` (`INT64`). Dictionary encoding `symbol` keeps multi-ticker watchlist exports compact without changing how downstream tools query the column.

### Streaming Writer

The streaming writer pulls fixed-size batches from the market-data cursor, encodes each batch into a Zstd-compressed row group, and flushes the bytes directly to the chunked HTTP response. Once the cursor drains, the writer emits the Parquet footer and closes the response cleanly.

Because HTTP response headers go out before the first row group, a mid-stream cursor failure cannot rewrite the status code to `500`. On error, the writer aborts the response stream without emitting the Parquet footer so the browser flags the transfer as incomplete rather than writing a silently truncated file.
````

### Example 2: Backend refactor

````markdown
# Transactional webhook outbox

## Objective

Persist outgoing webhook events in the same database transaction as domain writes and deliver them from a background worker. External webhook payloads, signing headers, and endpoint configuration remain unchanged.

## Background

Today, request handlers fire HTTP webhook calls inline after committing a database transaction.

That layout causes three problems:

- **Lost events on crashes.** If the API process restarts between the database commit and the HTTP request, the webhook never fires.
- **Slow API tail latency.** A slow or unresponsive subscriber stalls the request handler until the outbound timeout expires.
- **Duplicate side effects.** Retrying a failed client request re-emits webhooks without a stable delivery ID.

## Usage

Domain handlers enqueue webhooks inside their existing database transaction instead of calling the HTTP client directly:

```rust
let mut tx = pool.begin().await?;
orders::mark_paid(&mut tx, order_id).await?;
outbox::enqueue(&mut tx, WebhookEvent::OrderPaid { order_id }).await?;
tx.commit().await?;
```

## Decisions

- **Use a Postgres outbox table instead of a separate message broker.** Writing to the same database guarantees atomic commit with domain rows without introducing dual-write coordination or new infrastructure.
- **Guarantee at-least-once delivery with a stable delivery ID header.** Exactly-once HTTP delivery across untrusted subscribers is impossible. Sending `X-Delivery-Id` on every attempt lets receivers ignore duplicate deliveries on retries.
- **Purge delivered rows after 7 days.** Retaining payloads forever bloats the table, while a 7-day window leaves enough history for support debugging.

## Design

Domain writes and outbound HTTP delivery are separated by a durable `webhook_outbox` table owned by the notification module.

### Outbox Table

Each row stores the event ID, subscriber endpoint, serialized JSON payload, attempt count, next-attempt timestamp, and terminal status (`pending`, `delivered`, or `dead`). Enqueuing inserts a `pending` row inside the caller's transaction, so rolled-back domain changes never emit webhooks.

### Delivery Worker

Worker loops claim batches of due `pending` rows using `FOR UPDATE SKIP LOCKED` and stamp a lease expiry so another worker can reclaim a row if the claiming process dies mid-request. Each request sends the payload with an HMAC signature and `X-Delivery-Id` set to the row's event ID.

HTTP `2xx` responses mark the row `delivered`. Network errors, timeouts, and `5xx` responses increment the attempt count and schedule exponential backoff up to eight attempts before marking the row `dead`. HTTP `4xx` responses (other than `429`) mark the row `dead` immediately because retrying a rejected payload will not succeed.
````

### Example 3: HTTP auth

````markdown
# Scoped API tokens

## Objective

Allow users to issue, inspect, and revoke scoped bearer tokens for CI jobs and scripts. Existing browser session cookies and role permissions remain unchanged.

## Background

Today, the API authenticates every request through HTTP-only session cookies issued by the browser login flow. CI pipelines and CLI scripts cannot call API endpoints without scraping cookies or sharing long-lived user passwords, and any leaked credential grants full access to the user's account.

## Usage

Users manage tokens from the account API and authenticate automation calls with a standard bearer header:

```text
POST   /api/tokens            // returns plaintext token once plus token metadata
GET    /api/tokens            // lists token prefixes, scopes, expiry, and last-used time
DELETE /api/tokens/{id}       // revokes the token immediately
```

```text
curl -H "Authorization: Bearer app_live_9f8a..." https://api.example.com/v1/releases
```

## Decisions

- **Store SHA-256 token hashes in the database instead of issuing signed JWTs.** Stateless JWTs cannot be revoked before expiry without maintaining a denylist anyway. Hashed opaque tokens revoke immediately on delete and never expose raw secrets in database backups.
- **Require an explicit scope allowlist on creation rather than inheriting all user permissions.** Automation scripts usually need one narrow action (such as `releases:write`). Explicit scopes bound the blast radius of a leaked CI secret, while still capping effective access at the owning user's current role.
- **Throttle `last_used_at` updates to once per hour.** Writing a timestamp on every API request adds unnecessary write contention to the tokens table during bursty CI runs.

## Design

Token authentication extends the existing request auth middleware without altering downstream route handlers, which continue reading a single authenticated `Principal` from request context.

### Token Storage

Each token consists of a visible prefix (`app_live_` plus 8 random characters for log correlation and UI display) and 32 random bytes encoded as base64url. The database stores only the prefix, the SHA-256 digest of the full secret, owning user ID, granted scope set, optional expiry, revocation timestamp, and `last_used_at`. Plaintext secrets are returned once in the creation response and discarded.

### Request Authentication

When an incoming request carries a `Bearer` header, the auth middleware hashes the token, looks up the matching active row, and verifies that the token is neither revoked nor expired. It builds a `Principal` carrying the owning user ID intersected with the token's scope set. Route guards check required scopes against that `Principal`, rejecting missing scopes with `403 Forbidden` and unknown or expired tokens with `401 Unauthorized`.
````

### Example 4: Plugin extraction

````markdown
# Scene and snapshot plugins

## Objective

Move scene loading and application snapshots out of the core runtime into two independent opt-in plugins, `ScenePlugin` and `SnapshotPlugin`. The core runtime keeps ECS storage, frame-boundary deferral (`Engine::defer`), and keyed save/load hooks. `ScenePlugin` owns scene decoding, loading, and scene commands, while `SnapshotPlugin` owns snapshot config, file storage, and snapshot commands. Either plugin builds and runs without the other, cooked scenes and `EditorSceneDocument` stay compatible, and chunk streaming remains with simulation cells.

## Background

Today, the core runtime owns the scene store, origin tracking, and an archive scene slot, the asset plugin registers scene decoding, and the native host drives reload snapshots using one filename passed over the host bridge. Browser and Game poll scene assets in app-specific systems, while Editor mutates definitions directly.

That layout causes three problems:

- **Forced scene dependencies.** Every app pulls in scene storage, origin tracking, and decoding whether it uses authored scenes or not.
- **No plugin snapshot hooks.** Plugins cannot own snapshot paths, persist their own state, or offer named saves during a live session.
- **Global scene keying and private deferral.** Two instances of the same scene cannot coexist, apps duplicate asset-polling loops, and only internal scene queues can defer mutable work to the frame boundary.

## Usage

Apps opt into scenes and snapshots independently during composition, then drive scenes through `SceneRoot` entities or direct in-memory updates:

```rust
let app = App::new()
  .plugin(AssetPlugin)
  .plugin(ScenePlugin)
  .plugin(SnapshotPlugin::default());

// Asset-backed scene (after registering authored component types)
let root_id = SceneRoot::new("scenes/sphere.json").spawn(world);
let status = scene::root_status(engine, root_id);

// Direct in-memory authoring (Editor)
scene::apply(engine, Scene::builder("editor/scene/1").build());
let loaded = scene::definition(engine, "editor/scene/1"); // after frame commit
```

Run a hot reload with `--keep-state` to preserve the current world, or drive snapshots and standalone roots over the command gateway:

```text
snapshot.save { name: "before-edit" }      // write or overwrite named snapshot
snapshot.load { name: "before-edit" }      // restore without consuming the file
scene.load { asset: "scenes/sphere.json" } // spawn root and return its root_id
scene.list {}                              // list direct scenes and live roots
scene.unload { root: root_id }             // unload scene and remove its entities, keep root
```

## Decisions

- **Move scenes into a separate crate.** Feature-gating scenes inside the runtime or asset crate still couples core code to scene types. A standalone crate lets non-scene apps omit scene storage and decoding completely.
- **Key the scene store by owner instead of a flat `SceneId` map.** Today the store maps `SceneId -> Scene` and `SceneOrigin` stores only `(scene_id, entity_id)`, limiting a world to one instance per scene. Keying asset-backed scenes by root `EntityId` (while direct editor scenes keep `SceneId`) and adding that optional root ID to `SceneOrigin` lets multiple roots load the same scene independently or switch to another scene file.
- **Expose a session directory from the host instead of a filename.** Passing one filename over the host bridge ties snapshot paths to the host and leaves no place for named saves. Publishing one directory lets `SnapshotPlugin` own reload and named file layout across app generations.
- **Discover keyed persistence hooks after app composition.** Hardcoding plugin slots in the core archive couples the runtime to every stateful plugin. Keyed, versioned hooks remove install-order constraints and let restore reject mismatched system sets upfront.
- **Bump the snapshot format to v3 and entrypoint ABI to v6.**

## Design

The core runtime drops its private scene queues and hardcoded snapshot archive for two generic hooks that `ScenePlugin` and `SnapshotPlugin` build on independently.

### Runtime Hooks

`Engine::defer` lets read-only systems and gateway commands queue mutable work that runs in order before the next system pass.

Keyed save/load hooks let systems persist versioned state alongside the ECS world. On restore, the runtime requires an exact match on registered keys and runs every prepare hook against the read-only candidate world. Any failure discards the candidate and keeps the live world, system state, and queued commands intact. On success, it drops old queued work, commits the new world and prepared state, and notifies systems to reset transient and GPU state.

### ScenePlugin

`ScenePlugin` keeps the scene store private, exposes loaded scenes as read-only data, and excludes `SceneRoot` and `SceneOrigin` from CUE export. All updates run through `Engine::defer`, logging commit errors without disturbing the current scene or live entities.

One loader system watches every `SceneRoot` for asset loads and reloads, keeping the previous scene active while an asset loads or fails. Before committing an async load, it re-checks that the root still wants that asset and revision so late loads cannot overwrite a newer selection or revive an unloaded root. Changing a root's scene ID replaces all of its entities, while reloading the same scene preserves unchanged live entities and gameplay deletions.

The loader also registers with the runtime's save/load hooks. On save, it stores loaded scenes by owner (including empty scenes), while `SceneRoot` and `SceneOrigin` ride in the ECS world. On restore, it validates saved scenes against the candidate world, then after commit reopens asset handles and cleans up unloaded roots. Library reloads reconcile `scene::apply_startup` against restored state, while named restores keep saved scenes without replaying startup code.

### SnapshotPlugin

`SnapshotPlugin` manages two files inside the host's session directory: `reload-state.json` (cleared before each save and consumed once on startup even on failure) and `snapshots/snapshot-{name}.json` (replaced atomically and kept on disk after load).

On save, it archives the ECS world with the runtime's keyed payloads. On restore, it decodes the world against the app's registered components and hands both to the runtime's restore transaction. An in-place `snapshot.load` preserves the app generation and stops remaining commands in the current batch. In Editor, `EditorUiState` uses a separate snapshot proxy so reflected decoding round-trips its typed IDs without changing `EditorSceneDocument`.

## Manual checks

- In a managed native Browser session with keep-state enabled, switch to Particle Drop through the picker, inspect its scene tags and live entity IDs, then reload the app library in place. Confirm the host PID stays fixed, the library generation changes, and the selected demo and unchanged live IDs survive. Switch back to the landing scene and confirm the outgoing root-owned rows are removed.
````

### Example 5: Internal optimization

````markdown
# Coalesce concurrent cache misses

## Objective

Deduplicate concurrent upstream fetches for the same cache key inside the metadata cache so only one request runs per missing key. Public cache methods, return types, TTL configuration, and eviction policy remain unchanged.

## Background

Today, `MetadataCache::get` checks the in-memory map under a read lock and immediately issues an upstream RPC on a miss.

When a hot key expires under load, that behavior causes two problems:

- **Upstream request storms.** Every concurrent caller misses simultaneously and fires an identical RPC, spiking upstream latency and rate-limit errors.
- **Redundant cache writes.** Each returning RPC overwrites the same entry with the same payload after doing duplicate deserialization work.

## Decisions

- **Track in-flight keys in a sharded wait map instead of holding the cache write lock across the RPC.** Holding the cache lock during network I/O blocks reads and writes for unrelated keys, whereas a separate wait map serializes only callers requesting the same key.
- **Propagate leader cancellation independently of followers instead of tying the fetch to the first caller.** If the first caller drops its future while followers are still waiting, detaching the fetch into a shared task lets the remaining callers finish without restarting the RPC. The task cancels only once the last waiter drops.

## Design

`MetadataCache` inserts a per-key single-flight coordinator between a cache miss and the upstream client, leaving cache hits on the existing read-only fast path.

### Flight Table

On a miss, the cache looks up the key in an in-flight map keyed by cache key. If no fetch is active, it spawns a shared fetch task, inserts a broadcast channel handle into the map, and awaits the result. Subsequent callers for the same key subscribe to the existing handle instead of calling the upstream client.

### Settlement and Cleanup

When the fetch completes, the task writes successful values into the cache, removes the key from the in-flight map, and broadcasts the cloned result or error to all waiters. Removing the entry before broadcasting ensures any new caller arriving after a transient failure starts a fresh fetch rather than observing the stale error. If every waiter drops before completion, the drop guard cancels the task and removes its map entry.
````
