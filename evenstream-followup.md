# Event-stream follow-up work

The event-stream overhaul leaves four separate improvements: avoid allocating unused native conversations, expose provider checklist state that Harness already retains, remove approval-presentation types from the provider API, and move host decision deadlines to their owning Harness services. These follow-ups preserve the overhaul's ownership rules without expanding the meaning of provider turn completion.

This document follows [the event-stream overhaul](eventstream-overhaul.md) and does not describe implemented changes. The numbered work items are independent unless their implementation exposes a concrete dependency.

## 1. Defer native conversation creation

Creating an empty native conversation when Harness opens introduces a provider ID and a configuration-replacement lifecycle before any input needs them. Keep the Harness conversation local until provider work requires a native conversation, then create or resume that conversation once under the existing ownership controls.

### Current behavior and motivation

Harness and a native provider use separate conversation identities. The Harness ID identifies local conversation state and must exist before the first message; a native provider ID becomes necessary only when an operation targets an actual provider conversation.

| Identity | Why it exists | When it is needed |
| --- | --- | --- |
| Harness conversation/thread ID | Identifies the chat, settings, draft, queued input, and pending host actions. | Immediately when the local conversation is created. |
| Native provider conversation ID | Targets provider prompts, history, configuration, and other native operations. | Once native conversation creation or resume actually occurs. |

The current implementation opens the native conversation eagerly. In the [Codex configuration path](nvim/rust/forge/crates/forge-providers/src/codex/provider.rs), changing configuration releases the existing native writer and uses `thread/start` for unused history or `thread/resume` for established history. An empty native thread may therefore receive a replacement ID before the first input.

Established history has a stronger contract: a resume that returns a different native thread ID fails rather than silently replacing the conversation. This follow-up must preserve that rule.

The eager path creates work that the local conversation does not yet need:

1. Open an empty native conversation.
2. Retain its provider ID and resources.
3. Replace or reconfigure it when pre-prompt settings change.
4. Reconcile the replacement ID back into the same Harness conversation.

Deferring creation removes that sequence for a new, unused chat. It does not remove the need for IDs once native work begins.

### Target behavior

Opening a new Harness chat creates only host-owned state. Before the first provider operation, settings changes update that state without creating, cancelling, closing, or replacing a native conversation.

| Action | Target behavior |
| --- | --- |
| Open a new chat. | Allocate the Harness ID and local state; do not create a native conversation. |
| Change model, effort, or permissions before sending input. | Validate and retain the latest local configuration without native conversation mutation. |
| Submit the first input. | Persist the input, acquire creation ownership, capture the applicable configuration, and open the native conversation before dispatch. |
| Submit more input while creation is pending. | Retain it through the existing input owner; do not race a second native creation. |
| Close a never-used chat. | Close local state without a native close request. |
| Resume established history. | Retain its native identity and apply explicit resume semantics; never silently replace missing or incompatible history with a fresh conversation. |
| Perform an explicit operation requiring native history, such as a fork. | Open or resume when that operation requires it; first-prompt laziness must not fabricate success for other operations. |

Provider process startup and model/catalog discovery are separate from native conversation creation. They may still occur when needed for settings or capabilities; do not assume that a model picker requires allocating a native conversation.

### Creation, configuration, and cancellation

The first-use boundary must remain owned and cancellable before a provider ID exists. Use the Harness thread/operation identities to target pending work instead of inventing a native ID or treating the absence of one as proof that nothing is running.

Capture configuration at one serialized admission boundary. A configuration change that arrives before capture belongs to the first open; a change after capture follows the ordered configuration/cancellation path. Do not repeatedly discard partially created native conversations merely to chase newer settings.

Retain the creation worker and any allocated resources across dropped waiters. If cancellation or close arrives while native creation is in flight, prevent input dispatch and collect whatever the native operation created before reporting completion.

Keep input acceptance independent of creation. Successfully opening a native conversation is not proof that the prompt was accepted; a failed open must leave the saved input visible, and uncertain dispatch must retain the existing reconciliation/no-replay behavior.

### Simplifying native identity publication

The native ID can normally return as part of the owned open/resume result. The owner should commit that association directly instead of sending a redundant identity notification back through the provider stream.

Audit `Thread.IdentityResolved` and its current `SessionIdentity` predecessor after introducing lazy creation. Remove the event if all supported late-ID changes are now covered by explicit operation results and no asynchronous consumer requires it; do not remove identity retention itself.

Check the following before deleting any identity path:

- Codex fresh creation, established resume, and fork return the correct native thread association.
- Copilot session creation/resume and any recovery path retain the intended native history.
- Claude ACP and Antigravity ACP `session/new`/`session/load` results populate the native association without assuming a native turn ID exists.
- Child conversations retain their own IDs rather than borrowing the parent's association.
- Pending operations and late observations still validate their original binding generation.

The event-stream overhaul continues to define normalized host IDs and provider-owned mappings. This follow-up changes when native conversations are allocated, not what those IDs mean.

### Implementation and verification

Implement lazy creation at the shared conversation owner rather than separately in each UI command. Reuse its operation serialization, input journal, retained worker, and cleanup boundaries; do not introduce another coordinator.

Start with native-peer fixtures that count actual create/resume/configure/close calls. Update the shared open path and all three native adapter families, then remove empty-history replacement paths that no longer have a valid caller.

| Scenario | Required assertion |
| --- | --- |
| Open a chat and change settings several times without sending input. | No native conversation is created or replaced. |
| Send the first prompt after those changes. | Exactly one owned native creation uses the configuration selected at admission. |
| Submit concurrent input during creation. | Inputs remain ordered and do not cause duplicate native conversations or sends. |
| Cancel or close before creation starts. | No native creation or prompt dispatch occurs. |
| Cancel while creation is pending. | Any late-created resources are collected; the prompt is not dispatched afterward. |
| Creation fails. | The failure is visible, input remains retained, and no fake acceptance or replacement history appears. |
| Change settings after the first-use configuration boundary. | The existing ordered control path applies the change without races or lost input. |
| Resume or fork real history. | The original history contract and intended target identity remain intact. |
| Lose acknowledgement after prompt dispatch. | Delivery remains uncertain; lazy creation does not authorize replay. |
| Reopen or replace a host while old callbacks remain. | Binding-generation checks reject stale ownership and preserve live-owner cleanup rules. |

Follow the repository's [build and test procedure](nvim/contributing/forge/building-and-testing.md), including a rebuilt executable and fresh-host checks after implementation. If disposable state or wire contracts change, version them explicitly; preserve user-authored settings and do not migrate incompatible sessions.

## 2. Visualize provider checklists in Harness status

Harness currently retains the provider's checklist and associates tools with its entries, but neither the Lua views nor the Rust transcript renderer displays that checklist. Show this existing progress information in the status display for the owning Harness conversation, rather than collecting it without a visible use.

The [broker checklist handler](nvim/rust/forge/crates/forge-harness/src/broker/mod.rs) saves the checklist on the exchange and publishes a state refresh. The [checklist tracker](nvim/rust/forge/crates/forge-harness/src/exchange/checklist.rs) preserves stable entry identities, marks previously referenced entries as superseded, and attributes tools only when exactly one entry is in progress.

### Status presentation

Display the provider's current checklist with its entry text and pending, in-progress, or completed state. Label it as provider-reported progress so it cannot be mistaken for Harness's Plan/Execute/Goal lifecycle or verified check results.

Use the canonical Rust-owned checklist snapshot and the existing status publication path. Lua presents that state; it must not reconstruct checklist state from message text, infer completion from tool success, or maintain a second checklist tracker.

Keep the current entries distinct from superseded history. Historical entries and existing tool associations must remain retained even if the main status display shows only current entries; specify any history navigation using the shared view controls during implementation.

An absent checklist should leave the status display without a provider-checklist section. It must not imply that the provider completed all work, and a cancelled turn must not automatically mark remaining checklist entries complete.

### Implementation and verification

Add this presentation at the Harness status boundary, not the unrelated Git working-tree Status view. Preserve the owning conversation/request identity during updates so late checklist events cannot change another session's status.

| Scenario | Required assertion |
| --- | --- |
| A provider publishes a checklist. | Harness status shows its current entries and reported states. |
| An entry moves from pending to in progress to completed. | The display updates from canonical state without duplicating entries or moving the user's cursor unnecessarily. |
| The provider replaces, renames, or reorders entries. | Stable identities and superseded history follow the existing tracker rather than Lua heuristics. |
| Several entries are in progress. | The view shows them without inventing a unique tool-attribution target. |
| The turn is interrupted or the session changes. | Remaining entries are not falsely completed, and late updates retain their original owner. |
| A saved conversation is reopened. | Its retained checklist is available without querying the provider again solely to reconstruct the display. |
| No checklist was reported. | No empty or success-shaped checklist is fabricated. |
| All provider entries are checked off. | No Harness task, plan, or verification gate is completed merely because the checklist says so. |

Exercise the rendered status in an editor fixture as well as the native snapshot/publication path. Follow the existing build, Lua validation, and fresh-host procedures when implementation changes Rust or Lua.

## 3. Remove approval presentation from the provider API

The current provider API includes host UI data that native adapters should neither construct nor consume. Move approval display records into Harness so the provider contract describes permission requests and responses, not how the editor renders them.

This coupling already exists in runtime source; it is not merely a proposed event-stream type. [Provider API approval records](nvim/rust/forge/crates/forge-provider-api/src/approval.rs) define `ApprovalCommand` with source text, focus ranges, and `highlight_list: Vec<CommandHighlight>`. [The shell crate](nvim/rust/forge/crates/forge-shell/src/shell.rs) defines `CommandHighlight` as a text range, highlight-group name, and priority.

### Target ownership

Keep semantic authorization data at the provider boundary and construct presentation only after Harness receives the request. The provider adapter must still translate native permission requests and responses, including any native option IDs, without importing editor highlight groups or host UI-choice records.

| Data | Target owner |
| --- | --- |
| Requested command, read/write path, network target, or other permission target. | The provider-facing semantic permission contract. |
| Request identity, scope, cancellation, and normalized allow/deny/cancel response. | The provider-facing authorization contract and its host implementation. |
| Native approval-option IDs and their response translation. | The owning native adapter. |
| Display titles, explanatory UI details, offered host policy choices, focus ranges, and syntax highlights. | Harness approval presentation. |
| Saved rules, read/write directory grants, decision source, and approval persistence. | Harness permission policy and state. |

Audit `ApprovalRequestView`, `ApprovalItem`, `ApprovalChoice`, and `ApprovalCommand` together rather than moving only `CommandHighlight`. Separate host presentation and policy-choice types from any semantic enum currently shared with adapters; preserve the meaning of native permission responses during that split.

Remove these presentation records completely from `forge-provider-api`, including its public re-exports, event payloads, trait signatures, and transitive authorization types. Do not retain deprecated aliases, compatibility wrappers, or optional presentation fields there. Any still-needed host notification belongs to a Harness-owned contract; the provider boundary retains only the semantic authorization request/response.

The struct definitions copied into `eventstream-overhaul.md` document the existing and proposed host payloads; they are not a requirement to keep presentation in `forge-provider-api`. Update that plan's module ownership and imported-type references when implementing this follow-up.

### Implementation and verification

Trace actual adapter imports before relocating types, then update their definitions and every affected caller in one coherent change. Reuse shared shell parsing/highlighting services from the Harness presentation owner where appropriate; do not make adapters generate highlights or add a reverse dependency from the provider API to Harness.

| Scenario | Required assertion |
| --- | --- |
| A native adapter requests command or file permission. | Its normalized request contains semantic targets and ownership, not highlight groups, display ranges, or host UI choices. |
| Harness builds an approval prompt. | Command highlighting, source focus, titles, and policy choices remain available through Harness-owned records. |
| The user allows, denies, cancels, or lets a request expire. | The adapter receives the correct normalized response and translates it to the original native request without depending on UI records. |
| The user chooses an exact/broad rule or directory grant. | Harness persists only the selected policy change; adapters do not own saved rules or read/write scope. |
| Approval rendering or requester lifetime changes. | Existing cancellation, persistence-failure, and stale-request protections remain intact. |
| The provider API crate is compiled independently. | Its definitions, exports, events, callbacks, and authorization payloads contain no approval-presentation types or shell-token highlighting records, including compatibility aliases. |

Follow the existing provider-boundary, permission-policy, editor-fixture, and build procedures. Removing this dependency must not weaken command analysis, authorization, read/write separation, or explicit error reporting.

## 4. Move user-decision deadlines out of the provider API

Harness owns how long an approval or structured user question may remain pending. Keep the corresponding timeout settings, persisted deadlines, and running timers in Harness rather than making them part of the native provider contract.

The existing [deadline module](nvim/rust/forge/crates/forge-provider-api/src/deadline.rs) defines `InputDeadline` with `expires_at_ms` and a process-local `Option<tokio::time::Instant>`. The wall-clock timestamp supports persistence and display; the monotonic timer supports an active wait without changing its duration when the system clock changes. These are two representations of one host decision deadline, not two independent timeout policies.

### Target ownership

Relocate `InputDeadline` and host-specific decision-timeout configuration to their owning Harness deadline/approval/question services. Remove their provider API definitions and re-exports completely, updating all callers rather than retaining compatibility aliases.

Keep the provider-facing permission callback focused on its semantic request, response, and cancellation. Harness applies its deadline, records an explicit timeout decision, and returns the appropriate denied/cancelled native response according to the existing contract; the adapter does not manage the approval countdown.

Do not conflate this work with provider operation deadlines. `OperationContext` deadlines bound provider open, send, interrupt, and other operations and still belong to the provider-operation contract. Audit the uses of `InputTimeout`, `TimeoutUnit`, and `ResolutionSource` individually: move host-owned uses, but preserve any independently required provider execution policy through an appropriately scoped semantic type.

### Implementation and verification

Coordinate the relocation with the approval-presentation follow-up so moving `ApprovalRequestView` does not leave a transitive deadline dependency in `forge-provider-api`. Preserve question deadlines and decision-source behavior even where they use the same current module.

| Scenario | Required assertion |
| --- | --- |
| An approval or question is waiting. | Harness owns its captured deadline and the adapter receives no UI countdown or persisted timer record. |
| A deadline expires. | Harness records timeout as the decision source rather than inventing a user denial or silently abandoning the waiter. |
| The system clock changes during an active wait. | The existing monotonic waiting behavior remains intact. |
| Pending state is saved or reloaded. | Only persistable deadline data is serialized; a process-local `Instant` is never restored as if it survived restart. |
| The turn is cancelled or the requester disappears. | Cancellation resolves the pending wait without a second timeout decision. |
| A provider operation exceeds its own deadline. | The independent `OperationContext` error, cancellation, and retained cleanup contracts remain unchanged. |
| The provider API surface is reviewed. | No host user-decision deadline/settings type remains in definitions, exports, callback payloads, or compatibility aliases. |

Use the existing permission/question timeout fixtures and provider operation-deadline fixtures to verify the separation. Preserve user-authored timeout settings when moving their implementation; report unsupported settings or migration failures explicitly.
