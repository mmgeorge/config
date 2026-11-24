# Plan Overview Examples

Reference examples of concise `docs/overview.md` files. Match this plain, causal tone and keep section depth proportional to the change.

## Example 1: UI interaction feature (TypeScript, with manual check)

````markdown
# Add pinned actions and keyboard reordering to the command palette

## Objective

Let users pin frequently used actions to the top of the command palette and navigate or reorder pinned items from the keyboard. Unpinned fuzzy search ranking and existing shortcut bindings remain unchanged.

## Background

Today, the command palette ranks every action solely by recency and fuzzy match score. Opening the palette with an empty query shows a shifting list of recent commands, which breaks muscle memory for repetitive workflows such as toggling themes or running the active workspace task.

## Usage

Users toggle pins from the palette row action or shortcut, and applications can supply initial defaults when mounting the palette:

```ts
<CommandPalette
  actions={workspaceActions}
  defaultPinnedIds={["task.run-active", "view.toggle-terminal"]}
/>
```

```text
Mod+Shift+P          // toggle pin on highlighted action
Alt+ArrowUp / Down   // move highlighted pinned action up or down
```

## Decisions

- **Store pinned IDs in local UI preferences instead of project config.** Pins reflect personal workflow habits rather than shared repository settings, and unknown IDs from another workspace can be ignored silently at render time.
- **Keep pinned items grouped above search results only on empty queries.** Pinning results above strong fuzzy matches during typing forces extra arrow presses when searching for an unpinned command.

## Design

The palette splits its list model into two sections when the query is empty: pinned actions in stored order, followed by recent unpinned actions. When the user types a query, both groups merge into the normal fuzzy-ranked list while keeping a pin icon on pinned rows so users can still unpin them.

Keyboard reordering applies only while focus sits in the pinned section on an empty query. Moving an item swaps its index in the preference store and keeps focus attached to the moved row so repeated keypresses walk it up or down the list.

## Manual checks

- Open the command palette in a desktop browser, pin three actions, and reorder the bottom item to the top with `Alt+ArrowUp`. Confirm the focus ring follows the moved row and the scroll container keeps it visible without jumping.
````

## Example 2: Backend reliability refactor (bottom-up design)

````markdown
# Move webhook delivery to a transactional outbox

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

## Example 3: New feature addition (HTTP auth capability)

````markdown
# Add scoped API tokens for automation

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

## Example 4: Multi-layer plugin extraction (bottom-up design, with manual check)

````markdown
# Separate scene loading and application snapshots

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
