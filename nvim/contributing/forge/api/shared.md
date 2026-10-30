# Shared methods

These methods own configuration shared across conversations. Optional envelope context validates an existing session and selects its workspace/settings layer where stated. Omission never selects the active conversation.

All calls inherit the [request lifecycle and failure contract](lua-client.md#requestspec-callback-progress). Method failures do not select another session or close an otherwise valid connection.

## Shared result and document schemas

All methods in this chapter use runtime readiness and emit no request-progress stream. None starts a model turn or performs a remote network mutation. Settings writes with explicit session context can reconfigure that session's provider and emit `task_operation` and session-configuration events. Without context, they persist shared settings without creating a conversation. Keep the captured session/view owner when adopting any asynchronous result.

**Profile snapshots** contain `path` (the local JSON file), `revision` (an opaque string used for conditional save), `document`, and optional `error`. A missing file supplies the built-in defaults and revision `missing`. A malformed or unreadable file returns an error-bearing snapshot with the last validated document or defaults. `unreadable` is not a safe revision for overwriting an unobserved file. Profile saves validate the document, take the file lock, verify the exact revision, and atomically replace the file. Lock contention, invalid content, stale revision, and I/O failures reject the save. Reread after an unknown outcome.

A profile document has required `default_profile: string` and `profiles: object`, keyed by profile name. The default must name an enabled entry. Names contain 1–64 ASCII letters, digits, `_`, or `-`, and cannot be `root`, `Main`, or `primary`. Each profile accepts:

| Field | Type and default | Domain |
| --- | --- | --- |
| `enabled` | Boolean, `true` | Enables selection of this profile. |
| `description` | String, empty | UI description. |
| `instructions` | String, empty | Persistent child instructions. |
| `provider` | String, `inherit` | `inherit`, `codex`, `copilot`, `claude`, or `antigravity`. |
| `model` | String, `inherit` | Nonblank model choice or inheritance value. |
| `reasoning_effort` | String, `inherit` | Nonblank effort choice or inheritance value. |
| `service_tier` | String, `inherit` | `inherit`, `default`, `fast`, or `ultrafast`. |
| `approval_mode` | String, `inherit` | `inherit`, `ask`, `deny`, or `allow`. |

Unknown profile fields fail decoding. These are reusable child definitions, not a list of running agents. The [profile store](../../../rust/forge/crates/forge-harness/src/agent/profile.rs) owns validation, file locking, and fallback reads.

**Permission document results** contain `path` and `source`, both strings. `source` is canonical pretty JSON with a trailing LF. Its top-level `permission` object is required, and `$schema` is an optional string. `permission` maps categories to pattern/decision objects. Categories are `bash`, `edit`, `elevate`, `mcp`, `read`, `tool`, and `webfetch`. Patterns must be nonblank. Decisions are `allow`, `ask`, or `deny`. Unknown top-level fields and unsupported categories fail validation. Saving compiles and atomically writes the new policy before replacing the usable in-memory document. Invalid JSON, invalid rules, and I/O failures leave the prior usable policy. Saves have no expected-revision field, so callers must reread rather than assume concurrent edits are merged. The [permission store](../../../rust/forge/crates/forge-harness/src/permissions/store.rs) owns this lifecycle.

**Model-pin results** are arrays of model-identifier strings sorted by identifier. Backend and model parameters must be nonblank. A pin can reference a temporarily unavailable model and does not require querying a provider catalog. Membership writes are idempotent SQLite transactions that preserve other models and backends. A repeated unpin returns the unchanged list. Storage failures affect only the originating request. The [model-pin store](../../../rust/forge/crates/forge-harness/src/storage/model_pin.rs) owns concurrency and persistence.

**Settings snapshots** contain `revision` (unsigned integer), `desired` ([PortableSettings](native-schemas.md#portablesettings)), `catalog` with `mcp` and `skill` arrays, `layers` with `global`, `workspace`, and `session` PortableSettings objects, and `application` status. Optional settings fields are absent when not selected. Each PortableSettings object retains `skill` and `mcp` maps even when empty. Omitted workspace uses the initialized workspace; explicit session context always selects the session's workspace. Workspace resolution and storage errors reject the read rather than retargeting it.

Catalog MCP entries contain string `id`, `name`, and `source`, optional `scope`, an array of provider `targets`, and `transport` (`stdio`, `http`, or `sse`). Catalog skill entries contain string `id`, `name`, `description`, and `source`, provider `targets`, and boolean `user_invocable`. Catalogs omit executable definitions, secrets, and skill prompt bodies.

`application` contains unsigned `desired_revision` and `generation`, string `state`, and optional `operation_id`, `applied_revision`, and `effective` PortableSettings. It starts as `unverified`, records application work separately from persistence, and retains the last effective settings after a failed application. A successful provider application records `applied` and advances the generation. Failure states follow the corresponding command receipt.

**Import previews** contain `source` (canonical path), `fingerprint` (opaque string), `revision` (settings revision), and `candidate` (array). Each candidate contains `id`, `name`, `kind` (`skill` or `mcp`), `source`, optional/null `transport` and `blocked`, and a `conflict` array of strings. A blocked candidate cannot be applied. Preview supports `kind: "mcp"` for JSON/TOML server configuration and `kind: "skills"` for native skill directories. Import scope is `global` or `workspace`, defaulting to `workspace`; `session` scope is rejected. A relative source path resolves against the selected workspace. It reads local files without importing credentials or launching resources. The [import implementation](../../../rust/forge/crates/forge-harness/src/settings/import.rs) owns fingerprint, provenance, and conflict validation. Apply rereads the source, rejects changed fingerprints/revisions and invalid selections, and saves selected resources disabled.

Examples below come from [the isolated shared-API fixture](../../../../nvim/tests/forge/shared_api_examples.lua), which exercises every method without creating an interactive session. `/fixture` replaces its temporary root, and correlation IDs are renumbered for readability. Opaque IDs and fingerprints are retained from the corresponding responses and reused by later requests.

## `agent.profile.get`

Read the reusable agent-profile document.

- **Context:** Shared service with explicit evaluation context.
- **Input:** Empty object. Optional envelope session context validates that the session exists, but does not scope profile storage.
- **Result and effects:** Returns profiles and their revision.
- **Boundary:** Profiles are shared configuration rather than one session's running agents.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service/shared.rs).

**Transport:** Optional explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

## `agent.profile.save`

Save a reusable agent-profile document.

- **Context:** Shared service with explicit evaluation context.
- **Input:** `expected_revision`: required string from the previous snapshot. `document`: required profile document. The store validates the document and revision before saving.
- **Result and effects:** Returns the saved profile snapshot.
- **Boundary:** A stale revision must not overwrite concurrent profile changes.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service/shared.rs).

**Transport:** Optional explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

## `permissions.document.open`

Open the editable permission document.

- **Context:** Shared service with explicit evaluation context.
- **Input:** Empty object. The runtime owns the permission-store path. The caller does not supply another path.
- **Result and effects:** Returns the permission document representation.
- **Boundary:** Opening the document does not save edits.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service/shared.rs).

**Transport:** Optional explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

## `permissions.document.save`

Validate and save the edited permission document.

- **Context:** Shared service with explicit evaluation context.
- **Input:** `source`: required UTF-8 string containing the complete edited permission document. Returns `{ path, source }` from the saved store.
- **Result and effects:** Returns the save outcome/document state.
- **Boundary:** Invalid permission content must not silently replace the prior usable rules.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service/shared.rs).

**Transport:** Optional explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

## `provider.model.pin.list`

List pinned model identifiers for a provider.

- **Context:** Shared service with explicit evaluation context.
- **Input:** `backend`: required provider identifier string. Returns an array of pinned model identifier strings.
- **Result and effects:** Returns the provider's pinned model IDs.
- **Boundary:** This reads shared model-pin storage.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service/shared.rs).

**Transport:** Optional explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

## `provider.model.pin.set`

Set whether a provider model is pinned.

- **Context:** Shared service with explicit evaluation context.
- **Input:** `backend`: required provider identifier string. `model`: required model identifier string. `pinned`: required boolean. Returns the resulting array of pinned model identifiers.
- **Result and effects:** Returns the resulting pinned model IDs.
- **Boundary:** The change is persisted independently of a conversation.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service/shared.rs).

**Transport:** Optional explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

## `settings.get`

Read resolved settings and the available catalogs/layers.

- **Context:** Shared service with explicit evaluation context.
- **Input:** `workspace`: optional path string, used only without explicit session context and defaulting to the initialized workspace. With a session, its workspace and session layer are used. Returns the settings snapshot plus `catalog`, `layers` (`global`, `workspace`, `session`), and `application` status.
- **Result and effects:** Returns effective settings, catalogs, layer settings, and application status.
- **Boundary:** Global, workspace, and session layers are distinct.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service/shared.rs).

**Transport:** Optional explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

## `settings.import.apply`

Apply a reviewed settings import.

- **Context:** Shared service with explicit evaluation context.
- **Input:** All preview fields, plus `expected_revision`: required unsigned integer, `fingerprint`: required reviewed source fingerprint string, `selected`: required array of entry identifier strings, and optional `operation_id`. Re-reads the source and validates the fingerprint before persistence.
- **Result and effects:** Returns the import and application outcome.
- **Boundary:** Validate the captured source/revision before persistence.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service/shared.rs).

**Transport:** Optional explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

## `settings.import.preview`

Preview importing native provider settings.

- **Context:** Shared service with explicit evaluation context.
- **Input:** `kind`: required native import kind. `source`: required local source-path string. `scope`: setting scope, default `workspace`. `workspace`: optional evaluation path without a session. Returns a reviewed source fingerprint and selectable import entries without persisting them.
- **Result and effects:** Returns the import preview without applying it.
- **Boundary:** A preview is not approval or proof that a later import has committed.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service/shared.rs).

**Transport:** Optional explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

## `settings.mcp.set_enabled`

Change an MCP server's enabled setting.

- **Context:** Shared service with explicit evaluation context.
- **Input:** `name`: required resource-name string. `enabled`: required boolean. `resource_id`: optional canonical identifier string, required to disambiguate duplicate names. `expected_revision`: optional unsigned integer, otherwise reads the current revision. `scope`: setting scope, default `global`. `workspace`: optional evaluation path without a session. `operation_id`: optional durable mutation identifier string. Exactly one canonical resource must match.
- **Result and effects:** Returns the settings mutation/application result.
- **Boundary:** This is settings persistence and is not equivalent to a tool invocation.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service/shared.rs).

**Transport:** Optional explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

## `settings.skill.set_enabled`

Change a skill's enabled setting.

- **Context:** Shared service with explicit evaluation context.
- **Input:** `name`: required resource-name string. `enabled`: required boolean. `resource_id`: optional canonical identifier string, required to disambiguate duplicate names. `expected_revision`: optional unsigned integer, otherwise reads the current revision. `scope`: setting scope, default `global`. `workspace`: optional evaluation path without a session. `operation_id`: optional durable mutation identifier string. Exactly one canonical resource must match.
- **Result and effects:** Returns the settings mutation/application result.
- **Boundary:** This is settings persistence and can affect more than one session.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service/shared.rs).

**Transport:** Optional explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

## `settings.update`

Apply an explicit settings mutation.

- **Context:** Shared service with explicit evaluation context.
- **Input:** `expected_revision`: required unsigned integer. `desired`: required [PortableSettings](native-schemas.md#portablesettings) object. `scope`: `global`, `workspace`, or `session`, default `global`. `workspace`: optional evaluation path without a session. `operation_id`: optional string, generated when omitted. Callers that need mutation recovery must retain their own ID. Session scope requires an explicit envelope session ID.
- **Result and effects:** Returns mutation/application information.
- **Boundary:** Persisted intent and runtime application can have separate outcomes.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service/shared.rs).

**Transport:** Optional explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

## Shared mutation settlement

Without a session, a settings mutation returns `{ id, state = "saved", revision, restart_required = false }`. This records durable settings without creating a session or invoking its provider. With explicit session context, the mutation is saved first, then `session.command.apply` applies the captured settings revision. Its receipt can initially be accepted and later settle through `task_operation`. A later application failure does not imply that settings persistence failed. Reconcile both the receipt and the current settings snapshot.

These methods use the runtime readiness policy and normal admission. They emit no request-progress stream. Settings application with a session can emit session configuration and task-operation events. Profile, permission-document, and pin writes use their own store semantics rather than the session command receipt protocol. A transport failure after such a write requires rereading its current document or list.

The [shared-service regression](../../../rust/forge/crates/forge-harness/src/service/shared.rs) exercises these services before any session exists. [request_independence_host.lua](../../../tests/forge/request_independence_host.lua) submits two settings updates at the same revision and asserts exactly one commit. [permissions.lua](../../../tests/forge/permissions.lua) and [subagent_profiles.lua](../../../tests/forge/subagent_profiles.lua) cover document consumers.
