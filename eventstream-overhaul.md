# Provider event-stream overhaul

An interrupted request must not become a successfully completed request because two parts of an adapter interpreted the same provider events differently. Normalize each response cycle into one turn, retain its native outcome once, collect its provider-owned resources, and publish one authoritative turn-completion event.

This is an implementation plan, not a description of functionality already shipped. It targets the provider API, all native adapters, the shared execution owner, and their directly dependent Harness consumers. The source baseline is `bc36b830`; references below identify the pre-overhaul implementation. Read [the architecture overview](nvim/contributing/forge/architecture/README.md), [provider architecture](nvim/contributing/forge/architecture/providers.md), and [submission guarantees](nvim/contributing/forge/provider-submissions.md) before implementation.

[The lazy native-conversation follow-up](evenstream-followup.md) separately plans removal of eager empty-conversation creation and its native-ID replacement flow. It also identifies when `Thread.IdentityResolved` can be replaced by direct application of an open/resume result.

## 1. Motivation and architectural delta

The current implementation can publish individually well-formed but mutually inconsistent lifecycle events. The overhaul removes duplicate authority over completion; it does not replace the existing ownership, cancellation, delivery-journal, or checkpoint guarantees.

### 1.1 Observed failure

A settings change interrupted the question "What is this repo?" approximately four seconds after submission. The retained exchange contained no response, its provider turn was cancelled, and its exchange was nevertheless complete.

The diagnostic sequence established the following facts:

| Observation | Recorded result | Consequence |
| --- | --- | --- |
| The active turn received a reasoning-effort change. | The configuration command began cancellation and collection. | The unfinished question should remain eligible for continuation. |
| The transcript turn ended. | `TurnOutcome::Cancelled`. | No answer had completed. |
| The adapter published execution completion. | `ExecutionResult::Completed`. | The shared owner received contradictory information. |
| The adapter published settlement. | `ExecutionResult::Completed`. | The execution/settlement comparison accepted the same incorrect result twice. |
| Harness finalized the exchange. | Complete, with zero messages and a roughly four-second duration. | Configuration found no unfinished exchange to resume. |

The immediate classification error is in [Copilot's `terminal_executions`](nvim/rust/forge/crates/forge-providers/src/copilot/turn.rs): `(RootTurnPhase::Finished { .. }, _)` becomes `ExecutionResult::Completed`, even if the idle outcome is aborted. Separately, [the prompt path](nvim/rust/forge/crates/forge-providers/src/copilot/prompt.rs) recognizes cancellation and asks the transcript decoder to finish the turn as cancelled. The [collector](nvim/rust/forge/crates/forge-providers/src/copilot/provider.rs) subsequently reconstructs settlement from the execution classification.

The shared owner checks execution against settlement in [its event reducer](nvim/rust/forge/crates/forge-harness/src/provider/session_execution/runtime.rs). It does not make those two wrong values disagree with each other, and the transcript outcome is not their authority. The [foreground finalizer](nvim/rust/forge/crates/forge-harness/src/broker/foreground_finalization.rs) already has a cancelled-for-reconfiguration branch that preserves the request; the incorrect success bypasses that branch.

### 1.2 Related problems confirmed by source inspection

The problem is broader than the one Copilot match arm. Several distinctions are currently represented by unrelated events, permissive matches, or names that obscure ownership.

| Current boundary | Verified problem or constraint |
| --- | --- |
| Codex transcript versus execution decoding. | [Transcript normalization](nvim/rust/forge/crates/forge-providers/src/codex/normalization.rs) defaults unknown, absent, and `cancelled` statuses to `Completed`; [execution decoding](nvim/rust/forge/crates/forge-providers/src/codex/provider.rs) recognizes `interrupted` and `cancelled` and rejects missing status. |
| Generic event publication. | [`ProviderEventSink::send` and `send_wait`](nvim/rust/forge/crates/forge-provider-api/src/events.rs) accept every event, including independently constructed lifecycle results. |
| Timing and lifecycle. | `Turn.CallFinished` freezes a timestamp without finishing the turn; `Turn.Summary` carries duration and usage, not a prose summary. Their names imply more lifecycle authority than they have. |
| Runtime observations. | Copilot emits normalized `Runtime.Progress` and `Runtime.ForegroundIdle`; the inspected production consumers have no dedicated handling for these variants. Native idle remains essential inside the adapter. |
| Mixed event producers. | Approval presentation, directory grants, accepted-input presentation, retries, and skill attribution originate in Harness but use `ProviderEvent`. |
| Diagnostics. | One `Diagnostic` plus a `fatal` flag covers warnings, optional-feature failures, native errors, and observation failure. The flag does not define a complete lifecycle transition. |
| Existing tests. | Harness fixtures often provide already-consistent cancelled events. Such fixtures cannot catch an adapter that misclassifies native abort evidence before emitting those events. |

Fixing the individual matches is necessary but insufficient. The target API must make it unnecessary for adapter authors to decide which of several completion events to emit or which result should be copied into cleanup.

### 1.3 Accepted target and exclusions

The target vocabulary is `thread`, `turn`, and `item`, following the [Codex app-server primitives](https://developers.openai.com/codex/app-server/). The namespace identifies the affected object; explicit, distinctly typed ID fields identify the object and its ownership relationships.

The design requires:

1. One authoritative normalized turn outcome, emitted after provider-owned foreground cleanup succeeds.
2. Native completion returned to a retained shared runner, not manually published by several adapter components.
3. Separate operation acceptance, native outcome, provider cleanup, and Harness commit facts without separate competing turn outcomes.
4. Closed Rust enums, variant-specific payloads, exhaustive reducers, and explicit errors for unsupported lifecycle evidence.
5. Provider notifications separated from host authorization requests and Harness-owned presentation events.
6. Exact native-to-normalized identity and input-coverage mappings, including steering, child conversations, and late background output.

Do not add another session coordinator, a second completion queue, arbitrary content limits, generic replay, or a new cross-provider compatibility layer. Do not flatten provider differences by claiming unsupported operations succeeded. Do not treat a cosmetic namespace rename as completion of this plan.

## 2. Vocabulary, identities, and ownership

The public model must not imply that Codex exposes separate conversation objects called "invocation" and "execution." Forge still needs internal request identities and retained workers before native IDs exist, but those are ownership mechanisms rather than another public conversation hierarchy.

### 2.1 Canonical objects

One normalized turn covers an agent response cycle, including model calls and tool activity. It is not one token stream, one tool call, or necessarily one native event named `assistant.turn_start`.

| Object | Meaning | Owner and lifetime |
| --- | --- | --- |
| Provider connection | A live adapter binding and its ordered stream. | The provider runtime owns it until successful close; replacement gets a different binding identity. |
| Thread | A provider conversation. | The host assigns a thread ID; the adapter associates its native conversation ID when established. |
| Turn | One admitted response cycle in a thread. | The host assigns the turn identity before dispatch; the runner owns it through completion or explicit unavailability. |
| Item | One message, reasoning item, command, file change, or tool call within a turn. | The adapter maps native item IDs to the original normalized turn; identity never follows the currently visible turn. |
| Model call | A provider's internal call within the normalized turn. | Accounting retains its report identity and timing; it does not independently complete the normalized turn. |
| Harness exchange | The user's request through resolution, potentially spanning turns. | Harness retains its existing request/task/checkpoint responsibilities. |
| Submission and attempt | A logical input and one attempt to deliver it. | The existing journal owns these identities independently of turn completion. |
| Agent | An independently identified child with a parent relationship and, when known, its own thread. | Parent delegation and child content retain their respective thread, turn, and item IDs. |

Codex's root native turn maps directly to a normalized turn. Copilot may emit several native model-call turn boundaries during one response cycle; the adapter aggregates them rather than exposing several independently completed normalized turns. ACP v1 uses `session/prompt` through its terminal response as the response cycle.

### 2.2 Identity contracts

IDs must retain their creator, scope, and original association. Renaming must happen at definitions and all dependent call sites, not through aliases that leave two vocabularies describing the same value.

| Target type | Source or allocation | Required invariant |
| --- | --- | --- |
| `BindingId` | Retain the current runtime-issued generation ID. | A retired connection cannot publish into its replacement. |
| `ThreadOwner` | Rename the host-owned role currently represented by `ExecutionOwner`. | Retain the Harness session and agent ownership pair without exposing mutable fields. |
| `ThreadId` | Host-assigned conversation ID, resolved through the binding's thread registry. | Exists before a late native conversation identity is available; the registry retains its owner. |
| `TurnId` | Host-assigned response-cycle ID, carried alongside `thread_id`. | Exists before `turn/start` acknowledgement; the registry validates its thread and retains it for cancellation during preparation. |
| Native thread/turn IDs | Adapter-owned mappings, not replacements for host IDs. | Codex may map one-to-one; Copilot may map several native calls into one turn; ACP may not provide a native turn ID. |
| `ItemId` | Validated item ID, carried alongside `thread_id` and `turn_id`. | Type and original owner cannot change after admission. |
| `UsageReportId` | Native call/report identity or a documented adapter-local identity. | A repeated report cannot increment totals twice. |
| `SubmissionId`, `DeliveryAttemptId` | Retain the existing journal's identities. | Accepted or uncertain attempts cannot be replayed as new inputs. |
| `ApprovalRequestId` | Host request registry, correlated with native callback/RPC ID privately. | A response resolves only that pending request. |
| `AgentId` | Native correlation plus host registry; parent and child-thread associations remain explicit registry relationships. | Child output targets the child thread/turn, not the parent's current turn. |
| `CompactionId` | Native operation/item ID, or the existing correlated host operation. | Manual and automatic observations reconcile to the same operation without duplicating history. |

Do not substitute empty strings, zero IDs, or an unrelated "current" turn when a provider omits identity. Buffer correlated metadata until its owner is known, or return a typed ownership/observation failure. Any buffered content remains uncapped by arbitrary count or size limits and belongs to a retained worker.

`ThreadId`, `TurnId`, and `ItemId` are distinct validated newtypes, not interchangeable strings or aliases. Their types prevent mixing ID categories; the registry must additionally verify that the item belongs to the turn and the turn belongs to the thread. A well-typed but mismatched combination is still an ownership error.

Internal code may retain a composite helper such as `TurnAddress` when it repeatedly passes or validates an identity group. Public event payloads use explicit ID fields instead of address wrappers. Provider binding identity stays in the event envelope, and the registry resolves ownership without repeating binding and owner fields inside each event.

### 2.3 Mapping normalized IDs to Codex, Copilot, and ACP

Separate public IDs do not require every native protocol to supply a separate ID for every object. The shared runtime retains normalized thread/turn records, and the adapter retains native associations and item records that those IDs reference.

| Provider | Native identities and boundaries | Normalized ID mapping |
| --- | --- | --- |
| Codex | Native `threadId`, `turnId`, and item IDs are available through thread/turn/item messages. | Associate them with the registered `ThreadId`, `TurnId`, and `ItemId`; retain the host turn ID before native start acknowledgement so interruption during preparation stays addressable. |
| Copilot | SDK session ID, native messages/tool calls, and `assistant.turn_start` event identity are available. Native `turnId` numbers can repeat, and multiple native call boundaries can belong to one response cycle. | Retain one normalized `TurnId` for the response cycle; associate each native call occurrence using its event identity and correlated input evidence, not the reused native number alone. Allocate/map message and tool item IDs within the owning turn. |
| Claude ACP v1 | `sessionId` identifies the conversation, `session/prompt` identifies active request ownership locally, and `toolCallId` identifies tools. Text chunks need not contain native turn or message IDs. | Retain a normalized turn record for the owned prompt request and allocate message item IDs when an identified message stream begins. Associate subsequent chunks with that same item; map tools through `toolCallId` plus their original prompt/session ownership. |
| Antigravity ACP v1 | Uses the same session/prompt and tool-call identity model, but does not necessarily advertise Claude's extensions. | Use the same normalized ID rules without assuming native steering, model-call IDs, or per-message IDs exist. |

The current implementation already demonstrates both required techniques. [ACP child/prompt tracking](nvim/rust/forge/crates/forge-providers/src/acp/children.rs) creates synthetic `acp:` and `acp-child:` turn IDs, and [ACP message handling](nvim/rust/forge/crates/forge-providers/src/acp/session.rs) allocates synthetic message IDs and reuses them for subsequent chunks. [Copilot's decoder](nvim/rust/forge/crates/forge-providers/src/copilot/event_decoder.rs) uses native start-event identity because background wakeups can reuse `interactionId` and `turnId`; [its turn tests](nvim/rust/forge/crates/forge-providers/src/copilot/turn.rs) cover repeated native turn numbers.

Use the existing ownership stores as the basis for these records; do not add a second conversation store or let each decoder invent unrelated host thread/turn IDs. The host registers the thread and turn before dispatch and supplies their typed IDs to the adapter. The adapter allocates or resolves item identities once and retains the association for duplicate detection, final snapshots, and permitted late observations.

For session-only ACP updates, "active prompt" means the adapter's exclusively owned prompt and reader generation, not whichever turn the editor currently displays. Before a successor takes ownership, the old prompt's reader/callback work must be collected or explicitly retained with its old association. If a late native message cannot be attributed, report the ambiguity rather than attaching it to the successor. Synthetic IDs name known objects; they are never evidence of native acceptance, native start, or successful completion.

Retain these mappings through the applicable turn/background-observer lifetime and persist the normalized IDs referenced by durable items. Reattachment and replay must reuse recorded identities when replaying the same objects; a fresh incompatible state version may allocate fresh identities. The provider binding in the envelope still rejects events from retired connections even when native conversation IDs are reused.

### 2.4 Event envelopes and producer authority

The stream retains binding identity, monotonically increasing sequence, and typed payload. These fields establish source and order; they do not by themselves establish that an operation succeeded.

Use `event` for a nested event payload and `*Event` for its enum type: for example, `Thread { thread_id, event: ThreadEvent }`. Reserve `update`/`*Update` for actual data mutations such as `TextUpdate::Append` or `ToolInputUpdate::Arguments`. Event variants such as `CompactionUpdated` still describe something that happened and do not need renaming.

`ProviderEvent` is the readable public notification union. Adapter code cannot pass an arbitrary `ProviderEvent` to a public generic send method. A scoped adapter writer accepts only the `AdapterActivity` enum specified below; the shared runner alone constructs `Turn.Started`, `Turn.Completed`, and `Provider.Unavailable`.

The host reader validates all received envelopes even though producers use restricted Rust APIs. Serde must validate private invariant-carrying records through `TryFrom` or equivalent checked deserialization. A derivation that writes private fields directly must not bypass constructors.

## 3. Provider operations and unambiguous call order

The accepted simplification concerns completion authority, not input reliability. Existing operation contexts, deadlines, retained request identities, cancellation sources, and submission receipts remain part of each operation's contract.

### 3.1 Public operations

Rename session/turn-facing operations to the target vocabulary while preserving their existing guarantees. The table describes semantics rather than a drop-in trait listing; parameter records retain the current authorization and operation-context information.

| Before | After | Successful return means |
| --- | --- | --- |
| `session_open` | `thread_open` | An owned thread binding, negotiated capabilities, and its sole event reader are available. |
| `session_configure` | `thread_configure` | Settled defaults were applied to the exact binding; it does not secretly interrupt work. |
| `session_fork` | `thread_fork` | Separate target history was created at the specified source boundary. |
| `session_compact` | `thread_compact` | The correlated compaction operation reached its documented result and stream boundary. |
| `session_close` | `thread_close` | Admission is closed and owned resources are collected; preserve failed cleanup for retry. |
| `input_submit` | `turn_start` | The submitted input was accepted according to that provider's actual protocol. It does not mean the turn completed. |
| `input_steer` | `turn_steer` | The exact active turn accepted the identified additional input, or a typed rejection proves nonconsumption. |
| `cancel` | `turn_interrupt` | The exact addressed work was stopped or already ended, and the shared completion/cleanup path was observed. No independent turn outcome is returned. |
| `input_reconcile` | `input_reconcile` | Native history provided the reported delivery evidence; unknown remains unknown. |
| `input_retract` | `input_retract` | Only the authorized input/history boundary was retracted; no filesystem effects were undone. |
| `inspect`, `authenticate`, `shutdown` | Same operation roles. | Existing discovery, explicit authentication, and full retained-runtime shutdown contracts remain. |
| `model_list`, `skill_list`, `mcp_list` | Same operation roles. | The requested catalog or explicit unsupported/error result is returned. |
| `terminal_list`, `terminal_terminate` | Same operation roles. | Inventory was read or the exact terminal was stopped, not the entire turn. |

`turn_interrupt` returns an observation receipt identifying the operation, turn, and consumed stream boundary, not a second `TurnOutcome`. Repeating it observes the retained original result. Cancelling after genuine completion preserves completion and cannot affect a successor.

### 3.2 Adapter-author contract

One retained runner drives each admitted turn. The adapter author implements native work and cleanup, but does not choose between public `finish`, `execution_finish`, and `settle` notifications.

| Stage | Adapter action | Shared runner action |
| --- | --- | --- |
| Preparation | Validate native settings/resources and maintain the durable dispatch receipt. | Retain ownership before dispatch; allow exact cancellation before a native turn ID exists. |
| Native start | Report positively observed start through a scoped start capability once. | Publish `Turn.Started`; provide the scoped activity writer. |
| Running | Publish permitted item and metadata activity; service authorization callbacks. | Serialize activity, retain ownership, and validate ID relationships and lifetimes. |
| Native end | Return `Result<TurnResult, ProviderError>`. | Retain the result once; stop generation accounting; close new input admission. |
| Collection | Join foreground workers, revoke callbacks, and drain admitted callback work. | Retain collection work across dropped waiters; wait without holding locks needed by callbacks. |
| Publication | Do not construct a completion event. | Publish one `Turn.Completed` after successful provider collection, using the retained result. |
| Harness commit | Do not wait for Harness checkpoint/final-response publication. | Harness consumes completion, collects host-owned effects, applies task/checkpoint state, and commits its own result. |

A native start and an acceptance acknowledgement can arrive in either order. The runner may stream positively attributed work before acceptance returns, but final public completion must wait until each covered submission's receipt has reached a consistent disposition. ACP v1 acceptance can arrive at native end, so no early-ack requirement may be imposed.

Implement this separation with one internal `NativeTurnDriver` interface owned by `forge-providers`, distinct from the public operation trait. Its `run` method returns `Result<TurnResult, ProviderError>` to the retained runner; its `interrupt` method requests native interruption without publishing a result; its `collect` method joins provider-owned foreground resources without changing the retained outcome. Input acceptance is delivered through the existing receipt mechanism independently of `run` returning. The private driver's name describes its implementation boundary; public payloads do not expose native-result or native-failure wrapper types.

Public `turn_start` schedules that owned work and returns the acceptance result when available. Public `turn_interrupt` signals the same driver and awaits the same retained collection/publication boundary; it cannot create a second finalizer. On cancellation, request native stop and revoke callback admission before waiting for callbacks that could themselves be blocked on permission or host work. Do not hold the turn-state lock across those waits.

### 3.3 Native outcome versus cleanup and host commit

Combining the public completion notifications does not combine the underlying facts. Native work can end before provider cleanup, and provider cleanup can end before Harness commits checkpoints and user-visible state.

The existing collectors already provide the later native boundary: [Codex](nvim/rust/forge/crates/forge-providers/src/codex/provider.rs) joins its worker, [Copilot](nvim/rust/forge/crates/forge-providers/src/copilot/provider.rs) additionally revokes and collects callbacks, and [ACP](nvim/rust/forge/crates/forge-providers/src/acp/provider.rs) joins the worker and revokes callback authority. Move authoritative turn publication to that boundary instead of maintaining an earlier public turn result plus a later copy.

Harness's [host-effect collection](nvim/rust/forge/crates/forge-harness/src/provider/host_invocation.rs) must remain downstream. Its `settle` function explicitly avoids depending on terminal event delivery. Do not introduce a cycle in which provider completion waits for the host commit that is waiting for provider completion.

`Turn.Completed(Failed)` means a known unsuccessful native outcome with successful provider collection. Lost observation or failed collection instead produces `Provider.Unavailable`, retains known native evidence and unfinished ownership, fails pending waits explicitly, and forbids unsafe reuse. Do not publish a normal completion merely to make a waiting caller return.

An already-broken event stream cannot be the only route for reporting its own failure. The retained runner must also fail the affected operation/result waiters and mark the binding unavailable through the owning runtime; emitting an availability notification must not be a prerequisite for releasing those waiters with an error. Preserve the first failure even if publication of its diagnostic also fails.

### 3.4 Cancellation and continuation

Interruption describes what happened to provider work, while continuation describes what Harness should do next. Keep those decisions in their respective owners.

| Situation | Provider result | Harness behavior |
| --- | --- | --- |
| User cancels active work and native interruption is confirmed. | `Interrupted`, published after provider collection. | Preserve partial work; do not automatically restart. |
| Model/effort change interrupts active work. | `Interrupted`, published after provider collection. | Preserve the same unfinished exchange, apply configuration, and admit one successor turn. |
| Normal completion wins the cancellation race. | `Completed` with its actual native reason. | Do not replay completed work. |
| A required question or host control tool deliberately stops native generation. | Preserve the actual native outcome; retain structured host effects separately. | Apply the question/task handoff and wait or continue according to the host workflow. |
| Cancellation prevents native dispatch. | Operation-level proven nonconsumption/cancellation; no fabricated native start or completion. | Retain or retract the original input according to explicit user intent and journal evidence. |
| Delivery is uncertain. | Typed uncertainty, not `NotConsumed`. | Block automatic replay, retain the input, and expose reconciliation. |
| Cleanup fails after native interruption. | `Provider.Unavailable` with collection failure and retained native evidence. | Keep the connection fenced and unfinished cleanup owned; do not start a replacement there. |

Replace the current use of native success to encode `HostHandoff` with explicit host workflow handling after native outcome observation. This requires adapting the current completion/effect merge, not simply making every interrupted result discard its successfully recorded control-tool effects.

### 3.5 Error ownership and normalization

The adapter must preserve the scope established by native evidence. Receiving a session error through a prompt-owned subscription proves which reader received it, not which turn caused it; neither subscription ownership nor temporal coincidence authorizes adding a turn ID.

| Condition | Required representation | Required consumer action |
| --- | --- | --- |
| A native error identifies its session but does not establish a turn association. | `Thread.Error { error }` for the mapped native session. | Display and retain a thread-scoped error; do not invent a turn result, retry, or connection failure. |
| A native error explicitly identifies a turn, as supported Codex error notifications do. | `Turn.Error { error }` for the validated native-to-normalized turn mapping. | Display the error on that turn; the notification alone does not establish terminal outcome or cleanup success. |
| Native terminal evidence establishes failed work and collection succeeds. | `Turn.Completed` with `TurnOutcome::Failed { error, retry }`. | Finalize the failed turn once, preserving partial output and completed effects. |
| A recoverable native notice does not end work. | `Provider.Warning` at the known scope. | Show the warning without declaring turn failure or inferring retry eligibility. |
| The connection is unusable, required lifecycle observation is lost/invalid, or collection fails. | `Provider.Unavailable` for the binding in the event envelope. | Fence that binding and fail its owned pending waits; retain any known native result and unfinished cleanup internally. A valid session-scoped error lacking a turn ID is not itself observation loss. |
| A directly requested operation fails. | Return `Result::Err` from that operation. | The caller handles the operation failure; invalidate the binding separately only if its failure disposition requires it. |
| An automatic feature refresh fails without making the binding unusable. | The feature's typed result event, such as `Thread.TerminalsUpdated(Err(failure))`. | Handle that feature's failure without automatically failing a healthy turn or connection. |

Copilot `session.error` maps to `Thread.Error`, even when a turn is active and its reader receives the notification. Codex error notifications with explicit native thread/turn correlation can map to `Turn.Error`; a session-only Codex error stays thread-scoped. Validate supplied IDs and retained generation mappings rather than filling missing fields from the current turn. ACP operation errors remain operation results unless its native protocol supplies a separately attributable notification.

Change the current Copilot prompt driver's handling accordingly: a bare `session.error` must not call turn-failure finalization, return a failed native turn result, or construct retry eligibility solely because that prompt received it. Continue observing independently correlated terminal evidence; if observation actually fails or becomes invalid, use the explicit unavailable path instead of manufacturing a turn result.

`Thread.Error` and `Turn.Error` are error observations, not completion events. Only independently established native terminal evidence followed by successful collection produces `Turn.Completed`; do not complete a turn merely because an error was displayed. Associate a later failed completion with its earlier error using retained correlation so the UI does not render duplicate failures.

Do not add a generic top-level `Error` event or a second `Turn.Failed` completion event. `Provider.Warning` and `Provider.Unavailable` retain their distinct consequences, and ordinary operation errors and feature results retain their existing owners. Failure reason and retry eligibility are separate: a transient error category alone never proves replay safety.

### 3.6 Caller-facing results and naming

Callers consume adapter-level results, not objects describing how a particular provider proved those results. Use `Result<InputAcceptance, ProviderError>` for initial input acceptance and `Result<TurnResult, ProviderError>` for owned turn work; preserve the existing steering acceptance distinction for journaled versus contextual input.

Use `error` for failures, `outcome` for completed/interrupted/failed turn status, `stop_reason` for the provider's reported stopping condition, and `retry` for an explicit retry decision. The adapter and shared runtime perform native correlation and replay checks internally; the caller must not interpret native message ancestry or inspect a chain of proof records to use the result.

Reuse existing typed operation-error distinctions such as prevention before dispatch, documented nonconsumption, and uncertain delivery. The existing acceptance receipt and journal remain authoritative if a later operation fails. An `Err` does not erase acceptance, undo effects, prove no native work started, or authorize another send.

This replaces the earlier nested pre-start result/evidence design, rather than adding another compatibility layer beside it. Keep provider-specific names only inside native adapter implementation types such as the private `NativeTurnDriver`, not in the public result payload vocabulary.

## 4. Complete enum inventory

The following declarations define the target sum types for this work. Payload records named in the declarations are specified in the following section; these are contract declarations, not a standalone compilable Rust module.

All additions or further splits discovered during implementation must update this inventory and the mapping tables before they are accepted. Reused enums are identified separately so "every enum" does not mean duplicating already suitable domain types.

### 4.1 Public provider notifications

Each event family requires exactly its relevant typed ID fields, rather than an address wrapper or an optional bag of unrelated IDs. `Turn.Completed` is readable by consumers but is not constructible through the adapter's public writer.

```rust
/// Carry normalized observations from one ordered provider binding.
enum ProviderEvent {
    /// Report capability, warning, or availability information for the enclosing connection.
    Provider { event: ProviderConnectionEvent },
    /// Report a change to the identified conversation without requiring an active turn.
    Thread { thread_id: ThreadId, event: ThreadEvent },
    /// Report lifecycle or accounting for the identified response cycle.
    Turn {
        thread_id: ThreadId,
        turn_id: TurnId,
        event: TurnEvent,
    },
    /// Report content or lifecycle for one item in its original turn.
    Item {
        thread_id: ThreadId,
        turn_id: TurnId,
        item_id: ItemId,
        event: ItemEvent,
    },
    /// Report identity or lifecycle for an independently tracked agent.
    Agent { agent_id: AgentId, event: AgentEvent },
}

/// Report connection-level capability and availability changes.
enum ProviderConnectionEvent {
    /// Replace the effective capability snapshot for this connection.
    CapabilitiesChanged(CapabilityManifest),
    /// Report a scoped limitation without changing the turn's lifecycle.
    Warning(ProviderWarning),
    /// Fence an unusable connection and fail its affected waits explicitly.
    Unavailable(ProviderUnavailable),
}

/// Report conversation metadata independently of any active turn.
enum ThreadEvent {
    /// Report a native session-scoped error without attributing it to an active turn.
    Error { error: ProviderFailure },
    /// Associate the normalized thread with its confirmed native conversation ID.
    IdentityResolved(ProviderSessionId),
    /// Report current context occupancy rather than cumulative token expenditure.
    ContextUsage(ContextUsage),
    /// Report the result of refreshing this thread's terminal inventory.
    TerminalsUpdated(Result<TerminalSnapshot, ProviderFailure>),
    /// Report the state, summary, or measurements of an identified compaction.
    CompactionUpdated(Compaction),
}

/// Report one response cycle and its identified accounting.
enum TurnEvent {
    /// Report an explicitly turn-correlated native error without asserting completion.
    Error { error: ProviderFailure },
    /// Announce positively observed native work without claiming input acceptance.
    Started(TurnStarted),
    /// Report the confirmed model used for this turn.
    ModelResolved(ResolvedModel),
    /// Report identified token or timing measurements without ending the turn.
    Usage(TurnUsageReport),
    /// Apply a native plan/todo observation to this turn's checklist.
    ChecklistUpdated(ProviderChecklistUpdate),
    /// Publish the retained native outcome after provider-owned cleanup succeeds.
    Completed(CompletedTurn),
}

/// Preserve a native outcome without equating a request to stop with confirmed interruption.
enum TurnOutcome {
    /// Record a confirmed native end and its reason, not proof that the user's goal succeeded.
    Completed { stop_reason: TurnCompletionReason },
    /// Record positively confirmed interruption rather than a request to interrupt.
    Interrupted,
    /// Record a confirmed unsuccessful native result with typed failure evidence.
    Failed { error: ProviderFailure, retry: RetryDecision },
}

/// State whether repeating the operation is permitted after delivery and effect checks.
enum RetryDecision {
    /// Do not automatically repeat this operation.
    NotAllowed,
    /// Retrying is safe; honor the provider's minimum delay when one was supplied.
    Allowed { retry_after: Option<Duration> },
}

/// Preserve meaningful native end conditions without inventing task success.
enum TurnCompletionReason {
    /// The provider ended its response cycle without reporting a limiting stop reason.
    Normal,
    /// The provider ended the response because its token allowance was reached.
    TokenLimit,
    /// The provider ended the response because its model/tool request allowance was reached.
    RequestLimit,
    /// The provider ended the response by refusing the requested work.
    Refusal,
}

/// Identify the object affected by a non-lifecycle warning.
enum WarningScope {
    /// The warning concerns the provider connection identified by the enclosing envelope.
    Provider,
    /// The warning concerns one normalized conversation.
    Thread { thread_id: ThreadId },
    /// The warning concerns one response cycle in its owning conversation.
    Turn { thread_id: ThreadId, turn_id: TurnId },
    /// The warning concerns one item and does not transfer ownership to the current turn.
    Item { thread_id: ThreadId, turn_id: TurnId, item_id: ItemId },
    /// The warning concerns the identified root or child agent.
    Agent { agent_id: AgentId },
    /// The warning concerns the identified immutable provider operation.
    Operation { operation_id: OperationId },
}

/// Classify actionable warnings without a fatal flag or native payload dispatch.
enum ProviderWarningKind {
    /// Preserve a message whose native phase cannot be mapped to a known display role.
    UnsupportedMessageClassification,
    /// Some optional tool description is missing, but required execution evidence remains valid.
    IncompleteToolMetadata,
    /// Some reported diff metadata is unavailable without implying that no file changed.
    IncompleteFileChangeMetadata,
    /// Optional compaction summary or metrics are missing while its known state remains visible.
    IncompleteCompactionMetadata,
    /// Report a classified native notice that does not itself establish turn failure.
    NativeNotice { category: ProviderFailureKind },
}

```

`TurnOutcome::Completed` is the native response cycle's status, not a claim that the user's goal was fulfilled. ACP's known token-limit, request-limit, and refusal stops must drive the explicit host stop behavior and visible notices specified in section 8.4; merely retaining `TurnCompletionReason` in a record does not implement this contract.

### 4.2 Items and their events

Item type is encoded by a payload enum, not by a separate type string plus unrelated optional fields. Completion has a matching typed payload, and the item reducer rejects identity or type changes.

```rust
/// Define a newly observed item and the data valid for its type.
enum ItemStart {
    /// Start an observed user-message item, optionally correlated with a submitted input.
    UserMessage(UserMessageStart),
    /// Start assistant prose whose commentary/response classification may arrive later.
    AgentMessage(AgentMessageStart),
    /// Start a full-reasoning or reasoning-summary item.
    Reasoning(ReasoningStart),
    /// Start a requested shell/process operation before its execution necessarily begins.
    CommandExecution(ToolDescriptor),
    /// Start an operation whose native semantics report file changes.
    FileChange(ToolDescriptor),
    /// Start another tool operation without pretending it is a command or file edit.
    ToolCall(ToolDescriptor),
}

/// Describe one item's activity without exposing a separate turn outcome.
enum ItemEvent {
    /// Admit one immutable item identity and its initial typed contents.
    Started(ItemStart),
    /// Apply a message classification or text delta to the existing message item.
    Message { event: MessageItemEvent },
    /// Apply tool metadata, execution, output, or change observations to the existing item.
    Tool { event: ToolItemEvent },
    /// Freeze the item's final content and item-specific outcome without completing the turn.
    Completed(ItemCompletion),
}

/// Preserve message interpretation and ordered content.
enum MessageItemEvent {
    /// Record a known display role or preserve the explicit unclassified state.
    Classified(MessageClassification),
    /// Append this ordered text fragment to the same message.
    Delta(String),
}

/// Preserve the distinction between unclassified output and known assistant roles.
enum MessageClassification {
    /// The provider supplied no supported commentary/response classification.
    Unclassified,
    /// The message has a positively identified assistant-prose role.
    Known(AgentMessageRole),
}

/// Classify assistant prose without conflating reasoning with final output.
enum AgentMessageRole {
    /// The message describes progress or intermediate work rather than the final answer.
    Commentary,
    /// The message is identified as answer content for the turn.
    Response,
}

/// Preserve separately reported reasoning text and reasoning summaries.
enum ReasoningKind {
    /// The provider reports a reasoning-content stream.
    Full,
    /// The provider reports a summary of reasoning rather than the full reasoning stream.
    Summary,
}

/// Preserve tool metadata, execution, output, and file observations.
enum ToolItemEvent {
    /// Replace validated descriptor metadata without creating another item.
    DescriptorUpdated(ToolDescriptor),
    /// Change only the displayed title, not the admitted operation's identity.
    TitleChanged(String),
    /// Apply streamed arguments or a structured argument snapshot.
    Input(ToolInputUpdate),
    /// Record positive evidence that the requested operation actually began executing.
    ExecutionStarted,
    /// Record positive evidence that this operation survives foreground turn completion.
    Backgrounded,
    /// Append output or replace it with an authoritative snapshot.
    Output(TextUpdate),
    /// Retain native-reported file changes independently of Harness checkpoint diffs.
    Changes(ProviderChangeSet),
}

/// Preserve both streamed argument text and structured native argument snapshots.
enum ToolInputUpdate {
    /// Append or replace the native argument text stream.
    Text(TextUpdate),
    /// Replace the structured argument snapshot used for inspection.
    Arguments(InspectableArguments),
}

/// Finish the same item type that was admitted.
enum ItemCompletion {
    /// Freeze the observed user-message text without resolving the surrounding turn.
    UserMessage(MessageCompletion),
    /// Freeze completed answer/commentary text or interrupted partial prose.
    AgentMessage(MessageCompletion),
    /// Freeze completed or interrupted reasoning content.
    Reasoning(MessageCompletion),
    /// Finish the command item with its actual tool outcome and final snapshots.
    CommandExecution(ToolCompletion),
    /// Finish the file-change item without claiming a Harness checkpoint has committed.
    FileChange(ToolCompletion),
    /// Finish another tool item with its actual result, denial, or interruption.
    ToolCall(ToolCompletion),
}

/// Distinguish authoritative complete text from interrupted partial text.
enum MessageOutcome {
    /// The provider established an authoritative complete message.
    Completed,
    /// The message ended with partial content because its work was interrupted.
    Interrupted,
}
```

Retain the existing `TextUpdate::{Append, Replace}` and `ToolOutcome::{Succeeded, Failed, Denied, Cancelled, Interrupted}` enums. Their variants carry the current typed diagnostics or denial reasons. `ToolDescriptor` preserves kind-specific arguments, semantic operation metadata, name, and title; raw arguments may remain inspectable JSON, but consumers cannot decode JSON to discover the event kind or lifecycle.

`DescriptorUpdated` is a source-driven refinement to the earlier event sketch. ACP currently re-emits `Tool.Requested` when `title`, `kind`, or `rawInput` changes. The migration must preserve valid descriptor/argument updates explicitly rather than discard repeated requests or pretend they start another item. An actual immutable item-type conflict remains a protocol failure; incomplete descriptors must not force a premature incorrect type.

`Item.Tool.SkillRead` is not a native observation. Its producer is [Harness skill attribution](nvim/rust/forge/crates/forge-harness/src/skill.rs), so it moves to the host event union below rather than becoming an obligation for every native adapter.

### 4.3 Agent lifecycles

A child belongs to its own conversation and turn identities. Parent relationships establish attribution and cancellation policy; they do not give the parent's newest turn ownership of late child output.

```rust
/// Associate a child with its conversation and report its native lifecycle.
enum AgentEvent {
    /// Associate this agent with its own normalized conversation.
    ConversationResolved { thread_id: ThreadId },
    /// Report a typed child start, active-state change, or observed finish.
    Lifecycle { event: AgentLifecycleEvent },
}

/// Prevent a finished lifecycle event from carrying a running state.
enum AgentLifecycleEvent {
    /// Register a newly observed child and its initial active state.
    Started { agent: AgentDescription, state: AgentActiveState },
    /// Change metadata or active state for the same registered child.
    Updated { agent: AgentDescription, state: AgentActiveState },
    /// Record a terminal child outcome without inventing a separate cleanup guarantee.
    Finished { agent: AgentDescription, outcome: AgentOutcome },
}

/// Report a child that still has live work or a resumable wait.
enum AgentActiveState {
    /// The child is being initialized and has not yet reached running work.
    Starting,
    /// The child is performing work.
    Running,
    /// The child remains active but is waiting for input, a tool, or another dependency.
    Waiting,
}

/// Report the observed end of a child run without implying host cleanup.
enum AgentOutcome {
    /// The provider reports that the child finished normally.
    Completed,
    /// The child ended unsuccessfully with a classified failure.
    Failed(ProviderFailure),
    /// The child stopped without normal completion.
    Interrupted,
    /// The child conversation or native agent was explicitly closed.
    Closed,
}
```

Replace the current independent `AgentLifecycleKind` and `AgentExecutionState` combination with this valid-by-construction shape. Managed children use the same retained runner as roots; provider-native observed children require a separate observed-child ownership path. If the adapter can prove only a child lifecycle summary, publish that summary, not invented child items or a fabricated cleanup certificate.

### 4.4 Host events and authorization

Harness decides approval policy, updates its input queue, attributes skill reads, and schedules retries. These actions must not masquerade as independent native provider observations.

```rust
/// Publish host-owned state changes separately from native observations.
enum HarnessEvent {
    /// Publish host-owned approval UI state independently of native provider observations.
    PermissionApproval { event: PermissionApprovalEvent },
    /// Announce an already-persisted directory read/write grant.
    DirectoryAccessGranted(DirectoryAccessGrant),
    /// Publish the host retry policy's current scheduled or cleared state.
    RetryUpdated {
        thread_id: ThreadId,
        turn_id: TurnId,
        event: RetryEvent,
    },
    /// Publish the parent turn's wait for child results.
    ChildWait {
        thread_id: ThreadId,
        turn_id: TurnId,
        event: ChildWaitEvent,
    },
    /// Attribute a host-verified skill read to the original tool item.
    SkillRead {
        thread_id: ThreadId,
        turn_id: TurnId,
        item_id: ItemId,
        read: Vec<SkillRead>,
    },
}

/// Describe a pending permission decision and its exact disposition.
enum PermissionApprovalEvent {
    /// Open the identified pending decision with its targets, choices, and deadline.
    Requested(PermissionApprovalRequest),
    /// Report committed per-target decisions, including denials and timeouts.
    Resolved(PermissionApprovalResolution),
    /// Remove a pending request that ended without a committed permission decision.
    Cancelled(PermissionApprovalCancellation),
}

/// Identify the authority boundary addressed by an approval.
enum ApprovalScope {
    /// The requested permission concerns one identified item operation.
    Item { thread_id: ThreadId, turn_id: TurnId, item_id: ItemId },
    /// The request concerns a turn without a more specific native item identity.
    Turn { thread_id: ThreadId, turn_id: TurnId },
    /// The request concerns conversation-level authority rather than one turn.
    Thread { thread_id: ThreadId },
    /// The request concerns an identified thread operation such as native resource preparation.
    Operation { thread_id: ThreadId, operation_id: OperationId },
}

/// Record permission decisions without conflating cancellation with denial.
enum PermissionDecision {
    /// Authorize the target once or persist the explicitly selected rule scope.
    Allow { scope: PermissionRuleScope },
    /// Reject the target once or persist the explicitly selected denial rule scope.
    Deny { scope: PermissionRuleScope, reason: SanitizedMessage },
    /// Reject because the approval deadline expired, not because the user chose denial.
    TimedOut,
}

/// Preserve existing one-time, exact-rule, and broad-rule choices.
enum PermissionRuleScope {
    /// Apply the decision only to this request without saving a matching rule.
    Once,
    /// Save a rule for the displayed concrete command or target pattern.
    ExactRule,
    /// Save the displayed wider pattern, such as `cargo *`, not blanket permission.
    BroadRule,
}

/// Classify the saved-rule namespace without making consumers parse a category string.
enum ApprovalRuleCategory {
    /// Match parsed command invocations using the existing command-rule semantics.
    Command,
    /// Match read targets without granting write access.
    Read,
    /// Match write targets without overriding a disabled write policy.
    Write,
    /// Match the requested network target or its explicitly selected wider pattern.
    Network,
    /// Match the requested MCP server/tool target.
    Mcp,
    /// Match an explicitly requested privilege-elevation target.
    Elevation,
    /// Match an ordinary or host-owned tool by its configured rule pattern.
    Tool,
}

/// Preserve the actual source of a permission resolution.
enum ApprovalDecisionSource {
    /// The user explicitly selected the recorded decision.
    User,
    /// An already-saved permission rule supplied the decision.
    SavedPolicy,
    /// The current Ask/Allow/Deny mode supplied the decision without a new user choice.
    ApprovalMode,
    /// The request deadline supplied the automatic denied resolution.
    Timeout,
}

/// Explain why a pending request ended without a committed decision.
enum ApprovalCancellationReason {
    /// The user explicitly dismissed or cancelled the pending approval.
    UserCancelled,
    /// The owning turn was interrupted before the approval could resolve.
    TurnInterrupted,
    /// The owning conversation closed while the approval remained pending.
    ThreadClosed,
    /// The original native requester stopped waiting before a decision committed.
    RequesterGone,
    /// Required rule/grant persistence failed, so no successful resolution can be reported.
    PersistenceFailed(ProviderFailure),
}

/// Replace an optional retry payload with an explicit host state change.
enum RetryEvent {
    /// Show the identified host retry attempt and its delay information.
    Scheduled(ProviderRetry),
    /// Remove the current retry indicator without implying a successful turn.
    Cleared,
}

/// Attribute child waits to the parent turn that owns them.
enum ChildWaitEvent {
    /// Begin the parent turn's wait for the reported number of children.
    Started { agent_count: usize },
    /// Change the number of children still relevant to the parent's wait.
    Updated { agent_count: usize },
    /// End the parent wait without assigning child output to a different turn.
    Finished,
}
```

Keep `PermissionTarget` with its existing command, read, write, network, MCP, elevation, tool, and host-tool distinctions; retain explicit handling for its unclassifiable-target variants. Keep `DirectoryOperation::{Read, Write}` and `DirectoryGrant`, adding the originating request/owner in the enclosing `DirectoryAccessGrant` record. A directory read grant never implies write access, and no approval overrides write scope `Off`.

Provider authorization remains a request/response callback, not a `ProviderEvent`. Its response retains `PermissionResponse::{Allow, Deny, Cancelled}`; the richer host decision is applied and persisted by Harness before the native callback receives its normalized response. A timeout is an explicit denied resolution with source `Timeout`, not an invented user decision.

### 4.5 Restricted adapter activity and runner state

Restricting construction, not merely documenting method order, removes the generic completion bypass. The adapter gets a writer whose input type cannot contain `Turn.Completed`, `Provider.Unavailable`, or any host approval/input/retry notification.

```rust
/// Permit observations that an adapter may publish while its owned work runs.
enum AdapterActivity {
    /// Publish metadata for a thread owned by this adapter binding.
    Thread { thread_id: ThreadId, event: ThreadEvent },
    /// Publish permitted turn metadata, never a final turn outcome.
    Turn {
        thread_id: ThreadId,
        turn_id: TurnId,
        event: TurnMetadataEvent,
    },
    /// Publish an identified item's content or lifecycle under the owned turn.
    Item {
        thread_id: ThreadId,
        turn_id: TurnId,
        item_id: ItemId,
        event: ItemEvent,
    },
    /// Publish an explicitly correlated native child-agent observation.
    Agent { agent_id: AgentId, event: AgentEvent },
    /// Report an actionable nonfatal limitation without changing availability.
    Warning(ProviderWarning),
}

/// Allow turn metadata without allowing an adapter to finish a turn.
enum TurnMetadataEvent {
    /// Report a native error only when native evidence identifies this turn.
    Error { error: ProviderFailure },
    /// Report the actual model once the provider supplies authoritative evidence.
    ModelResolved(ResolvedModel),
    /// Report identified accounting without completing the response cycle.
    Usage(TurnUsageReport),
    /// Report the native checklist associated with the turn.
    ChecklistUpdated(ProviderChecklistUpdate),
}

/// Keep one worker and its result owned through collection and publication.
enum ProviderTurnState {
    /// Own preparation and cancellation while no positive native start has been observed.
    Preparing(PreparingTurn),
    /// Own the active native worker after start has been established.
    Running(RunningTurn),
    /// Retain the native result while provider-owned foreground resources are collected.
    Collecting(CollectingTurn),
    /// Retain the sole completed-turn value until ordered publication finishes.
    Publishing(PublishingTurn),
    /// Retain the published result after successful provider cleanup.
    Completed(CompletedTurn),
    /// Retain the operation error when no native turn started; delivery stays in the journal.
    NotStarted(ProviderError),
    /// Fence the turn's connection while preserving failure evidence and unfinished owners.
    Unavailable(UnavailableTurn),
}

/// Reuse the existing retained cleanup task's private join state.
enum TaskState {
    /// Keep the original worker handle owned while any waiter may be cancelled.
    Running(JoinHandle<Result<(), ProviderError>>),
    /// Reuse the already-joined success or error instead of launching cleanup twice.
    Collected(Result<(), ProviderError>),
}

/// Give reducers a checked admission result rather than silently skipping replays.
enum ObservationAdmission {
    /// Apply a newly validated observation to its original owner.
    Apply,
    /// Acknowledge an exactly matching replay without repeating its effects.
    IdenticalReplay,
}

/// Decode Codex terminal status before any transcript or completion consumer sees it.
enum CodexTurnStatus {
    /// Codex explicitly reported a normally completed turn.
    Completed,
    /// Codex explicitly reported interruption of the turn.
    Interrupted,
    /// The supported native cancellation spelling maps to normalized interruption.
    Cancelled,
    /// Codex explicitly reported a failed turn.
    Failed,
}

/// Classify Copilot's final native evidence without using model-call end as success.
enum CopilotTurnEndCause {
    /// Correlated root idle establishes the response cycle's normal end.
    NormalIdle,
    /// Correlated abort and idle evidence establishes interruption of the response cycle.
    AbortedIdle,
    /// A host control handoff deliberately stopped native generation; host effects remain separate.
    HostHandoffIdle,
    /// Preserve an explicitly turn-correlated transient failure for the separate retry check.
    RetryableFailure { error: ProviderFailure, retry_after: Option<Duration> },
    /// Independent turn-correlated terminal evidence establishes failure, not bare session.error.
    TerminalFailure(ProviderFailure),
}
```

The driver returns `Result<TurnResult, ProviderError>`. `Ok` contains the adapter's confirmed turn outcome, including a failed outcome; `Err` reports an operation or observation failure for which an ordinary turn result could not be established. `CollectingTurn` retains that result and the cleanup job. `PublishingTurn` retains the sole completed-turn value and publication progress; it cannot accept a replacement outcome.

The runner's existing preparing/running state establishes whether native work started; an error alone does not. For a pre-start cancellation or rejection, return the typed operation error without inventing a turn event. Use the existing acceptance receipt and journal to distinguish input that was not sent, explicitly not consumed, accepted, or uncertain. An accepted input cancelled before work starts remains accepted; do not introduce separate delivery-evidence wrappers or erase its receipt.

`AdapterActivity::Thread` is still validated against the writer's binding and thread authority. An adapter cannot publish new capability manifests through arbitrary activity; open/configuration results and explicit runtime capability observations go through the shared connection owner. The start capability similarly has one checked operation that establishes native start, rather than allowing arbitrary `Turn.Started` sends.

ACP continues to use its negotiated v1 schema's `StopReason`; do not introduce a second unchecked string parser. Exhaustively translate supported reasons and return an explicit protocol/unsupported error for a future reason that has no mapping.

### 4.6 Retained enums and required modifications

Many existing enums already encode a useful domain and should be reused rather than replaced for naming alone. Their consumers still fall under the exhaustive-handling requirements.

| Existing enum | Treatment |
| --- | --- |
| `ProviderOperation` | Rename `SessionOpen/Configure/Fork/Compact/Close` to `ThreadOpen/Configure/Fork/Compact/Close`, `InputSubmit` to `TurnStart`, `InputSteer` to `TurnSteer`, and `Cancel` to `TurnInterrupt`; retain the other operation variants. Update every error attribution and operation ledger. |
| `ProviderFailureKind`, `ProviderPhase`, `FailureDisposition` | Retain typed failure categories, phase, and effect disposition. Replace dependent names only where their represented object changes. |
| `ProviderError`, `EventDeliveryError`, `ObservationFailure` | Retain typed causal distinctions and owned cleanup errors; update variants that embed removed execution types. Never flatten them into a warning string. |
| `TurnOutcome`, `TurnUpdate`, `ProviderEvent`, `AgentUpdate` | Replace with the target `TurnOutcome`, `TurnEvent`, `ProviderEvent`, and `AgentEvent` definitions above. Do not leave legacy aliases. |
| `ExecutionResult`, `ExecutionUpdate`, `ExecutionFailure` | Remove the independent public execution lifecycle. Use `TurnOutcome::Failed { error, retry }`, preserve coverage in `TurnResult` and the delivery journal, and expose retry permission rather than native replay-proof records. |
| `MessageRole`, `MessageClassification`, `MessageUpdate` | Replace the mixed thought/prose lifecycle with typed item content, `ReasoningKind`, `AgentMessageRole`, and the message/item declarations above. |
| `ToolUpdate`, `ToolActivityKind` | Replace event dispatch with `ToolItemEvent` and typed item variants; derive the tool-like type from `ItemStart`, not a second potentially conflicting tag. |
| `TextUpdate`, `ToolOutcome`, `ToolSemantics` | Retain the existing value semantics, including append versus replace and explicit unclassified operation handling. |
| `PermissionTarget`, `PermissionResponse`, `DirectoryOperation`, `WriteScope`, `DirectoryScope` | Retain all access distinctions and rejection rules. Do not conflate them with native turn cancellation. |
| `ApprovalResolution` | Translate the old combined policy/UI values into the new permission decision, rule scope, decision source, or cancellation reason. Preserve each existing choice. |
| `ResolutionSource` | Retain for unrelated question/deadline contracts; permission events use the richer `ApprovalDecisionSource` rather than changing unrelated APIs. |
| `CompactionState`, `ChecklistStatus`, `RetryCategory` | Retain their existing variants. Each maps explicitly in event reducers. |
| `ApprovalRuleCategory`, `RetryDecision` | Add typed saved-rule categories and an explicit retry decision. Reuse existing operation errors and delivery receipts rather than introducing separate nonconsumption/evidence enums. |
| `TaskState` | Reuse the private enum from `forge-providers/src/retained_task.rs`; do not introduce a second cleanup-task state machine. |
| `SubmissionState`, `SubmissionEvidence`, `SteeringAcceptance`, `FailureDisposition` | Preserve all distinctions among accepted, unconsumed, uncertain, and contextual delivery. There is no replacement `InputAccepted` event. |
| Host exchange/task/plan state enums | Preserve their ownership and invariants. Update their inputs to canonical turn outcomes rather than rewriting workflows as provider state. |

### 4.7 Existing provider failure categories

`ProviderFailureKind` is the enum used by the `category` field in `ProviderWarningKind::NativeNotice` and the `kind` field in `ProviderFailure`. It already exists in the provider API; the full definition is included here rather than requiring a source lookup to understand those payloads.

These categories identify the kind of problem, not its severity, event scope, or retry safety. This is the existing vocabulary; changing its legacy execution/session names requires updating their definitions and consumers together, not inventing aliases.

```rust
/// Classify a problem without exposing native codes, payloads, or secrets.
enum ProviderFailureKind {
    /// The host explicitly cancelled the addressed operation.
    Cancelled,
    /// An operation ID was reused with different request contents or context.
    OperationConflict,
    /// Another consumer already took the operation's single owned result.
    OperationResultTaken,
    /// Required credentials are missing, invalid, or no longer authenticated.
    Authentication,
    /// The requested action is outside the caller's permitted authority.
    Authorization,
    /// The provider's context capacity prevents the requested operation.
    ContextLimit,
    /// The native runtime or session could not be prepared.
    Initialization,
    /// The adapter cannot enforce the requested policy.
    Policy,
    /// Native evidence violates the required protocol or normalized contract.
    Protocol,
    /// The provider account has exhausted a reported usage allowance.
    Quota,
    /// The provider temporarily limited the request rate.
    RateLimited,
    /// The provider explicitly rejected the operation.
    Rejected,
    /// The native runtime failed without a more specific classification.
    Runtime,
    /// The requested native history or live binding cannot be found.
    SessionMissing,
    /// Communication with the native provider failed.
    Transport,
    /// The operation's deadline elapsed without proving whether input was consumed.
    Timeout,
    /// Available evidence does not support a more precise failure classification.
    Unknown,
    /// Required journal or receipt data could not be persisted.
    Persistence,
    /// The addressed provider binding has permanently closed.
    BindingClosed,
    /// The binding lost authority after an unsafe partial mutation or observation failure.
    BindingInvalidated,
    /// An operation requires idle state but native work remains active.
    ExecutionActive,
    /// Required native terminal evidence is not yet available.
    ExecutionNotTerminal,
    /// The requested owner does not own the addressed native work.
    WrongInvocation,
    /// The native history position no longer matches the requested mutation.
    HistoryConflict,
    /// Surviving resources prevent safe configuration replacement.
    ResourceConflict,
}
```

## 5. Payload records and publication invariants

The enum inventory must not hide required contracts in undefined payload bags. The declarations below define the referenced records, their fields, and their construction boundaries; existing records are identified separately from new ones.

These are schema and ownership declarations, not copy-and-paste implementations. Fields are private unless the existing reused type already exposes them; implementation must add checked constructors, read-only accessors, and checked serialization rather than expose mutation of invariant-bearing records. Standard Rust containers and synchronization primitives are imported, not redefined.

| Record or family | Required contents and invariants |
| --- | --- |
| `TurnStarted` | Original start time only; starting does not prove input acceptance or report the actual model. Model information arrives exclusively through `Turn.ModelResolved`. |
| `ResolvedModel` | Actual reported/confirmed model identity and provenance; never substitute a configured default and label it observed. |
| `TurnUsageReport` | Report identity, optional native call identity, token categories, optional reported duration, and typed incremental/cumulative accounting using the existing usage rules. Missing counts remain unavailable. |
| `TurnResult` | Completion time, one adapter-level `TurnOutcome`, exact covered submissions/attempts, opaque checkpoint association, and final accounting. Validate against the original `TurnContext`; callers do not interpret provider-native failure or delivery records. |
| `CompletedTurn` | Validated native end plus provider-collection time. Only the retained runner constructs it; the event envelope supplies publication identity. It contains no second outcome. |
| Operation errors and acceptance receipts | Reuse `ProviderError` and the existing journal/receipt types. A failed call does not imply nonconsumption, and acceptance survives later turn or presentation failure. |
| `ProviderUnavailable` | Typed failure with operation/phase/disposition and sanitized explanatory text. The envelope supplies the binding; its owner resolves affected turns through the registry. Known native results and unfinished cleanup stay in internal turn records, not a nested public affected-turn payload. |
| `ProviderWarning` | Typed `ProviderWarningKind`, `WarningScope`, sanitized explanatory text; no `fatal` flag and no implicit lifecycle effect. |
| Item-start records | Initial typed content and source identity. A normalized user item requires positive native input evidence; queue insertion alone is not that evidence. |
| `ToolDescriptor` | Name, title, semantic operation, and inspectable arguments. Descriptor updates preserve item identity and cannot silently change its established type. |
| `MessageCompletion` | Authoritative final/partial text and `MessageOutcome`. Final text replaces streamed accumulation; repeated final snapshots must be identical. |
| `ToolCompletion` | `ToolOutcome` plus available final descriptor/output/change observations. Completed tool actions are never replayed by a turn restart. |
| `AgentDescription` | Explicit parent relationship, originating delegation where known, child conversation association, and observed definition/label/task metadata. The enclosing event supplies the child agent ID. |
| `PermissionApprovalRequest` | Approval ID, `ApprovalScope`, typed target list, choices, deadline, and original request ownership. It is host-owned pending state. |
| `PermissionApprovalResolution` | Approval ID, per-target `PermissionDecision`, decision source, and persisted grants/rules. Resolved does not mean allowed. |
| `PermissionApprovalCancellation` | Approval ID and `ApprovalCancellationReason`; no fake user decision. |
| `DirectoryAccessGrant` | Originating approval/owner plus existing directory path and `DirectoryOperation`. Grant publication follows successful persistence. |

Usage accounting has one deduplication owner. Final usage on `CompletedTurn` references or reconciles the same report identities as streamed `Turn.Usage`; it is not an additional token charge. Preserve native-call duration separately from elapsed response-cycle time and provider-collection duration.

Warnings cannot silently conceal missing execution evidence. An unsupported display classification may become a warning and an explicit unclassified message. A missing lifecycle ID, unknown terminal status, conflicting final outcome, lost required output sequence, or unknown delivery result must use the typed error/availability path.

### 5.1 IDs and the event envelope

Each ID is a distinct newtype, even where two native protocols happen to use identical string representations. Constructors reject invalid representations, while the owning registry checks relationships between otherwise valid IDs.

```rust
/// Identify one normalized conversation independently of its native session ID.
struct ThreadId(String);
/// Identify one normalized response cycle, including before native acknowledgement.
struct TurnId(String);
/// Identify one item within its registered normalized turn.
struct ItemId(String);
/// Identify a root or child agent in the host registry.
struct AgentId(String);
/// Identify one usage report so replay cannot charge the same tokens twice.
struct UsageReportId(String);
/// Identify one model-call occurrence in normalized accounting.
struct ModelCallId(String);
/// Identify a model within the owning provider's catalog.
struct ModelId(String);
/// Identify one pending host permission approval.
struct ApprovalRequestId(String);
/// Identify one target within a permission approval request.
struct ApprovalTargetId(String);
/// Identify one offered choice for one permission target.
struct ApprovalChoiceId(String);
/// Identify one correlated manual or automatic compaction.
struct CompactionId(String);

/// Identify the Harness session retained by an internal thread owner.
struct SessionId(String);
/// Identify the provider by its validated registered name.
struct ProviderId(String);
/// Identify the original logical input in the delivery journal.
struct SubmissionId(String);
/// Identify one delivery attempt for a logical input.
struct DeliveryAttemptId(String);
/// Identify one immutable public provider operation.
struct OperationId(String);
/// Identify one live provider binding; replacement allocates a different ID.
struct BindingId(String);
/// Retain the opaque native conversation ID without treating it as a host thread ID.
struct ProviderSessionId(String);
/// Retain an opaque native history boundary for checked fork or retraction.
struct CheckpointId(String);
/// Identify an adapter-owned terminal within one binding.
struct TerminalId(String);
/// Preserve the monotonically increasing position of a published event.
struct EventSequence(u64);
/// Carry native recovery evidence without exposing its representation to Harness.
struct RecoveryToken(Vec<u8>);
/// Carry validated display text that excludes native secrets and payloads.
struct SanitizedMessage(String);

/// Associate an internal thread with its host session and agent.
struct ThreadOwner {
    session_id: SessionId,
    agent_id: AgentId,
}

/// Bind a public event to the connection generation and its publication order.
struct BoundProviderEvent {
    binding_id: BindingId,
    sequence: EventSequence,
    event: ProviderEvent,
}
```

The existing ID types retain their representations unless the new identity contract requires a change. `SanitizedMessage` validates nonempty already-sanitized text; its wrapper must not be described as automatically discovering and removing secrets. The adapter remains responsible for producing safe display text.

`ThreadOwner` is internal ownership information, not another address included in every event. `EventSequence` is allocated only by the ordered publisher; native sequence numbers and timestamps cannot substitute for it.

### 5.2 Turn metadata, native results, and failure evidence

The public completion payload contains one retained native outcome and the successful collection timestamp. A cleanup failure retains native evidence in the unavailable path instead of creating another outcome or claiming ordinary completion.

```rust
/// Record positive native start without claiming input acceptance.
struct TurnStarted {
    started_at_ms: i64,
}

/// Record a model positively confirmed for this turn.
struct ResolvedModel {
    model_id: ModelId,
    observed_at_ms: i64,
}

/// Preserve one identified token and timing observation.
struct TurnUsageReport {
    report_id: UsageReportId,
    model_call_id: Option<ModelCallId>,
    observed_at_ms: i64,
    usage: Option<TokenUsage>,
    cumulative: Option<TokenUsage>,
    cumulative_total: Option<u64>,
    reported_duration_ms: Option<u64>,
    started_at_ms: Option<i64>,
    completed_at_ms: Option<i64>,
}

/// Identify the exact logical input and attempt covered by native completion.
struct CoveredInput {
    submission_id: SubmissionId,
    attempt_id: DeliveryAttemptId,
}

/// Reuse the existing operation receipt identifying accepted input.
struct InputAcceptance {
    submission_id: SubmissionId,
}

/// Preserve the adapter's turn outcome and accounting for final publication.
struct TurnResult {
    outcome: TurnOutcome,
    completed_at_ms: i64,
    reported_duration_ms: Option<u64>,
    covered_inputs: Vec<CoveredInput>,
    final_usage: Vec<TurnUsageReport>,
    checkpoint_id: Option<CheckpointId>,
}

/// Certify that the retained native result also passed provider collection.
struct CompletedTurn {
    result: TurnResult,
    collected_at_ms: i64,
}

/// Report a connection that cannot safely continue without faking turn completion.
struct ProviderUnavailable {
    failure: ProviderFailure,
    message: SanitizedMessage,
}

/// Explain a nonfatal limitation at its exact scope.
struct ProviderWarning {
    scope: WarningScope,
    kind: ProviderWarningKind,
    message: SanitizedMessage,
}
```

Provider-specific correlation and replay checks stay inside the adapter/runtime and existing delivery journal. The caller receives a typed error, an acceptance receipt, or a turn result; it does not inspect native proof objects to decide what those values mean.

`TurnOutcome::Failed.retry` is an explicit `RetryDecision`, separate from its `error`. Only the shared policy/runtime boundary may authorize retry after native correlation, delivery, and completed-effect checks. Unknown delivery, unsafe completed effects, or a session-only error cannot become `Allowed` just because the category is transient. `Allowed` permits retry but does not require the host scheduler to perform it; cancellation, limits, and user policy still apply.

`Turn.ModelResolved` is the only event that supplies the actual model for a turn. `Turn.Started` has no model field, and there is no `ObservedModel` wrapper. The host initially stores no confirmed model and updates that same turn's model only when it consumes `Turn.ModelResolved`; configured thread defaults and requested overrides remain separate.

If the model is already confirmed when work starts, publish `Turn.Started` followed immediately by `Turn.ModelResolved` in the same ordered stream. If confirmation arrives later, publish `Turn.ModelResolved` then. Native model evidence received before native start is retained until start is established; it must not produce a model event for an unstarted turn. If the provider never confirms the actual model, emit no model event and leave the confirmed value absent.

This intentionally replaces the earlier initial-model-plus-late-model design with one consumer path. Validate the original thread/turn IDs, treat an identical repeated model report idempotently, and handle a genuinely changed native model report through that same path without rewriting another turn or the configured thread default.

A timing-only `TurnUsageReport` may have no token counts. A report with no token or timing observation is invalid, and a later report with the same ID must merge consistently or reject conflicting evidence. These optional values describe independently available measurements, not alternative lifecycle states.

### 5.3 Item payloads

An item's type comes from `ItemStart` and remains fixed for that item ID. Completion records preserve final snapshots and outcomes without duplicating the thread, turn, or item IDs supplied by the event.

```rust
/// Preserve a positively observed native user message and any matched submission.
struct UserMessageStart {
    text: String,
    submission_id: Option<SubmissionId>,
}

/// Start assistant prose without guessing an absent commentary/response phase.
struct AgentMessageStart {
    text: String,
    classification: MessageClassification,
}

/// Start a reasoning stream with its explicitly identified representation.
struct ReasoningStart {
    text: String,
    kind: ReasoningKind,
}

/// Retain native tool arguments for inspection, not for event-kind dispatch.
struct InspectableArguments(serde_json::Value);

/// Describe the operation carried by a command, file-change, or tool-call item.
struct ToolDescriptor {
    name: String,
    title: String,
    semantics: ToolSemantics,
    arguments: InspectableArguments,
}

/// Freeze authoritative complete text or the partial text left by interruption.
struct MessageCompletion {
    text: String,
    outcome: MessageOutcome,
}

/// Freeze a tool's outcome and any authoritative final snapshots.
struct ToolCompletion {
    outcome: ToolOutcome,
    descriptor: Option<ToolDescriptor>,
    output: Option<String>,
    changes: ProviderChangeSet,
}

/// Identify the existing parent turn that initiated a child delegation.
struct AgentDelegation {
    parent_thread_id: ThreadId,
    parent_turn_id: TurnId,
    parent_item_id: Option<ItemId>,
}

/// Describe a child without repeating the child ID in its enclosing event.
struct AgentDescription {
    parent_agent_id: Option<AgentId>,
    delegation: Option<AgentDelegation>,
    thread_id: Option<ThreadId>,
    definition: Option<String>,
    nickname: Option<String>,
    task: Option<String>,
}
```

`UserMessageStart.submission_id` links submitted attachments through the existing submission/attachment registry; events must not copy attachment bytes into every message notification. A native history message without a matched live submission keeps that association absent rather than receiving a fabricated journal entry.

An absent final tool output means no replacement snapshot was reported, not "erase previously streamed output." Apply the same distinction to omitted final descriptors. Agent parent/delegation/thread metadata may arrive independently; validate all known relationships and never use absence as permission to attach the child to the current UI turn.

### 5.4 Permission request, decision, and grant records

One approval can contain several independently identified targets and several offered decisions. The payloads preserve the actual selected patterns so labels such as "broad rule" never hide the authority being granted.

```rust
/// Request decisions for identified targets under one captured authority scope.
struct PermissionApprovalRequest {
    request_id: ApprovalRequestId,
    scope: ApprovalScope,
    reason: Option<SanitizedMessage>,
    targets: Vec<PermissionApprovalTarget>,
    deadline: Option<InputDeadline>,
}

/// Preserve one requested permission and its offered choices.
struct PermissionApprovalTarget {
    target_id: ApprovalTargetId,
    target: PermissionTarget,
    title: String,
    detail: String,
    commands: Vec<ApprovalCommand>,
    choices: Vec<PermissionApprovalChoice>,
}

/// Offer one explicit decision, including the exact rules or grants it would save.
struct PermissionApprovalChoice {
    choice_id: ApprovalChoiceId,
    label: String,
    decision: PermissionDecision,
    rules: Vec<ApprovalRulePattern>,
    directory_grants: Vec<DirectoryGrant>,
}

/// Bind a validated saved-rule pattern to its permission category.
struct ApprovalRulePattern {
    category: ApprovalRuleCategory,
    pattern: String,
}

/// Preserve the committed decision and effects for one approval target.
struct ResolvedPermissionTarget {
    target_id: ApprovalTargetId,
    decision: PermissionDecision,
    source: ApprovalDecisionSource,
    rules: Vec<ApprovalRulePattern>,
    directory_grants: Vec<DirectoryGrant>,
}

/// Resolve the original approval once without implying that every target was allowed.
struct PermissionApprovalResolution {
    request_id: ApprovalRequestId,
    scope: ApprovalScope,
    targets: Vec<ResolvedPermissionTarget>,
    resolved_at_ms: i64,
}

/// End a pending approval without fabricating a permission decision.
struct PermissionApprovalCancellation {
    request_id: ApprovalRequestId,
    scope: ApprovalScope,
    reason: ApprovalCancellationReason,
    cancelled_at_ms: i64,
}

/// Publish a successfully persisted read or write grant with its origin.
struct DirectoryAccessGrant {
    request_id: ApprovalRequestId,
    scope: ApprovalScope,
    grant: DirectoryGrant,
    granted_at_ms: i64,
}
```

`PermissionDecision` here is the approval-event decision enum, not the existing saved permission document's `Allow/Ask/Deny` enum. Keep them in distinct owning modules and use explicit conversion names; do not shadow a persisted policy enum with a different meaning.

`Once` affects only this request. `ExactRule` saves the matched concrete command/target pattern; `BroadRule` saves a wider, explicitly displayed pattern chosen by the existing policy builder. For example, the current [command-pattern helper](nvim/rust/forge/crates/forge-harness/src/permissions/command.rs) generalizes an executable invocation to `executable *`; the [target-rule builder](nvim/rust/forge/crates/forge-harness/src/provider/approval.rs) generalizes an MCP target to `server/*`, and read/write targets use their existing category-specific pattern semantics. "Broad" does not mean all permissions, and its pattern is not necessarily a directory grant.

Convert `ApprovalRuleCategory` explicitly to the existing saved category constants: `Command` to `CATEGORY_BASH`, `Read` to `CATEGORY_READ`, `Write` to `CATEGORY_EDIT`, `Network` to `CATEGORY_WEBFETCH`, `Mcp` to `CATEGORY_MCP`, `Elevation` to `CATEGORY_ELEVATE`, and `Tool` to `CATEGORY_TOOL`. Do not derive a new lowercase storage vocabulary from the enum names or silently rewrite existing permission settings.

Only offer an exact or broad choice after its concrete patterns have been validated and displayed. Failed persistence cannot produce `PermissionApproval.Resolved`. Once a decision and its effects have committed, a later requester-disconnect error must not rewrite that decision as a second cancelled terminal state.

An offered `PermissionApprovalChoice` cannot contain `TimedOut`; that variant is created only by deadline resolution. Constructors must also validate that the chosen rule scope matches the displayed rule/grant records, so a once-only choice cannot persist an undisclosed broad rule.

### 5.5 Runner ownership records

These records remain inside the provider runtime and are never serialized as public events. Their retained handles ensure that dropping a result waiter cannot abandon native work, cleanup, or final publication.

```rust
/// Retain the admitted turn's internal owner and operation independently of native IDs.
struct TurnContext {
    binding_id: BindingId,
    owner: ThreadOwner,
    operation_id: OperationId,
    thread_id: ThreadId,
    turn_id: TurnId,
    initial_input: CoveredInput,
}

/// Retain native preparation while interruption is already addressable.
struct PreparingTurn {
    context: TurnContext,
    driver: Arc<dyn NativeTurnDriver>,
    worker: JoinHandle<Result<TurnResult, ProviderError>>,
}

/// Retain the same worker after positively observed native start.
struct RunningTurn {
    context: TurnContext,
    driver: Arc<dyn NativeTurnDriver>,
    worker: JoinHandle<Result<TurnResult, ProviderError>>,
}

/// Retain native evidence while the original foreground cleanup is joined.
struct CollectingTurn {
    context: TurnContext,
    driver: Arc<dyn NativeTurnDriver>,
    result: Result<TurnResult, ProviderError>,
    cleanup: Arc<RetainedTask>,
}

/// Retain the sole completed-turn value until ordered publication finishes.
struct PublishingTurn {
    context: TurnContext,
    completed: CompletedTurn,
    publication: JoinHandle<Result<EventSequence, EventDeliveryError>>,
}

/// Retain failed cleanup and native evidence while the binding remains fenced.
struct UnavailableTurn {
    context: TurnContext,
    driver: Arc<dyn NativeTurnDriver>,
    failure: ProviderFailure,
    /// Preserve a turn result received before subsequent observation or cleanup failed.
    result: Option<TurnResult>,
    cleanup: Arc<RetainedTask>,
}

/// Reuse the existing cancellation-safe owner of one cleanup worker.
struct RetainedTask {
    state: tokio::sync::Mutex<TaskState>,
    operation: ProviderOperation,
}
```

Reuse [the existing `RetainedTask`](nvim/rust/forge/crates/forge-providers/src/retained_task.rs), including its private `TaskState` enum documented in the enum inventory. A run/publication handle stays inside its owner while awaited; transition code must not take and drop it if a waiter is cancelled. The driver retains protocol-specific resource owners needed for a later cleanup attempt, while the first failure remains recorded.

`NativeTurnDriver` is a trait, not an undefined struct. Its `run`, `interrupt`, and `collect` responsibilities are specified in section 3.2; concrete Codex, Copilot, and ACP driver implementations retain their existing native session/resource records. This plan does not duplicate those provider-library internals into the public event schema.

`UnavailableTurn.result` is internal retained state: `None` means no turn result was established, and `Some` preserves a result known before the later failure. The public unavailable event needs only the connection failure; the connection owner already knows its turns and must not reconstruct them from a second list supplied in that event.

### 5.6 Referenced existing value records

The following declarations make reused payloads explicit so the event definitions can be read without guessing their fields. Except for the compaction diagnostic replacement described below, preserve their existing semantics and serialization unless a separate compatibility change is required.

```rust
/// Retain inclusive provider token counts without inventing unavailable categories.
struct TokenUsage {
    input: Option<u64>,
    cached_input: Option<u64>,
    reasoning: Option<u64>,
    output: Option<u64>,
}

/// Describe the current occupied context window, not cumulative token expenditure.
struct ContextUsage {
    used: u64,
    size: u64,
    remaining_percent: u8,
}

/// Preserve the existing capability snapshot and its binding-local revision.
struct CapabilityManifest {
    provider: ProviderId,
    label: String,
    revision: u64,
    effective_revision: u64,
    task_steering: Vec<TaskKind>,
    provider_version: Option<String>,
    notice: Vec<String>,
    capability: Vec<Capability>,
}

/// Describe one supported, limited, or unsupported operation.
struct Capability {
    id: CapabilityId,
    group: String,
    label: String,
    required: bool,
    implementation: CapabilityOwner,
    support: Support,
    application: CapabilityApplication,
    reason: String,
}

/// Preserve terminal-inventory state independently of foreground turn completion.
struct TerminalSnapshot {
    supported: bool,
    starting: bool,
    terminal: Vec<BackgroundTerminal>,
}

/// Identify one live shell in its owning provider binding.
struct BackgroundTerminal {
    id: TerminalId,
    command: String,
}

/// Preserve the failed operation, cause, phase, and effect disposition.
struct ProviderFailure {
    operation: ProviderOperation,
    kind: ProviderFailureKind,
    phase: ProviderPhase,
    disposition: FailureDisposition,
}

/// Preserve a complete or incremental native checklist observation.
struct ProviderChecklistUpdate {
    scope_id: String,
    name: Option<String>,
    complete: bool,
    replace_entries: bool,
    entry_list: Vec<ProviderChecklistEntry>,
}

/// Preserve one checklist entry and its native order.
struct ProviderChecklistEntry {
    provider_id: Option<String>,
    content: String,
    priority: Option<String>,
    status: ChecklistStatus,
    provider_ordinal: usize,
}

/// Preserve all native file edits associated with an item.
struct ProviderChangeSet {
    file: Vec<ProviderFileChange>,
}

/// Describe a native-reported patch without treating it as a workspace checkpoint.
struct ProviderFileChange {
    path: String,
    move_path: Option<String>,
    kind: ProviderChangeKind,
    diff: String,
}

/// Preserve a failed tool's normalized display information.
struct ToolDiagnostic {
    kind: ProviderFailureKind,
    message: SanitizedMessage,
}

/// Attribute a confirmed selected-skill read to its original host item.
struct SkillRead {
    resource_id: String,
    name: String,
    path: String,
    tool_id: String,
}

/// Preserve the existing host retry counter and delay state.
struct ProviderRetry {
    attempt: usize,
    max_attempts: usize,
    retry_at_ms: Option<i64>,
}

/// Preserve a successfully persisted read or write directory grant.
struct DirectoryGrant {
    operation: DirectoryOperation,
    path: String,
}

/// Preserve the original source command and typed presentation annotations.
struct ApprovalCommand {
    source: String,
    shell: CommandShell,
    focus_range_list: Vec<Range<usize>>,
    highlight_list: Vec<CommandHighlight>,
}

/// Preserve one existing shell-token highlight without parsing its display group as authority.
struct CommandHighlight {
    range: Range<usize>,
    group: String,
    priority: usize,
}

/// Preserve a durable deadline and its process-local monotonic counterpart.
struct InputDeadline {
    expires_at_ms: i64,
    instant: Option<tokio::time::Instant>,
}
```

`SkillRead.tool_id` is retained source attribution, not an alternative routing key; the enclosing host event's typed item IDs decide ownership. `InputDeadline.instant` is process-local and skipped during serialization. Existing permission patterns and catalog labels are data, not strings that consumers parse to discover an event variant.

The capability snapshot's existing flags describe independent advertised facts; they do not replace the new lifecycle enums. An unsupported inventory call still returns a typed unsupported result, and a failed inventory read cannot construct an empty successful `TerminalSnapshot`.

`Thread.TerminalsUpdated` carries the result of an automatic inventory refresh, not just a native notification that something changed. Copilot observes `session.background_tasks_changed` and queries SDK `tasks().list()`; Codex's terminal-change observer queries `thread/backgroundTerminals/list`. The adapter normalizes the returned inventory and publishes `Ok(snapshot)`, or publishes the typed refresh error as `Err(failure)`.

On `Ok(snapshot)`, replace the inventory and clear any previous refresh error. On `Err(failure)`, retain the last successful inventory and record the failed refresh without declaring that the provider or turn failed. A later success must publish again after an error even if the snapshot equals the last successful inventory, so the consumer can clear its error state. An explicit `terminal_list` call still returns its own result; it does not require a second event for that same call.

There is no separate `TerminalsUnavailable` event. Unsupported adapters do not start an inventory observer, and actual connection-wide failure still follows `Provider.Unavailable` rather than being hidden as a feature-only refresh error.

### 5.7 Compaction records

Compaction retains all currently reported measurements, but its failure information must not preserve the obsolete generic `ProviderDiagnostic { fatal }` contract. The thread ID comes from the enclosing event; the compaction ID identifies the observation, and an optional operation ID associates it with a known host request.

```rust
/// Track one identified context-compaction operation within its enclosing thread.
struct Compaction {
    id: CompactionId,
    state: CompactionState,
    /// Identify the host request when this compaction is positively correlated with one.
    operation_id: Option<OperationId>,
    started_at_ms: Option<i64>,
    completed_at_ms: Option<i64>,
    result: CompactionResult,
}

/// Preserve reported compaction results without inventing unavailable measurements.
struct CompactionResult {
    context_tokens_before: Option<u64>,
    context_tokens_after: Option<u64>,
    model: Option<String>,
    before_context: Option<CompactionContext>,
    custom_instructions: Option<String>,
    summary_content: Option<String>,
    failure: Option<ProviderFailure>,
    warnings: Vec<ProviderWarning>,
    pre_compaction_tokens: Option<u64>,
    post_compaction_tokens: Option<u64>,
    tokens_removed: Option<u64>,
    pre_compaction_messages_length: Option<u64>,
    messages_removed: Option<u64>,
    compaction_tokens_used: Option<CompactionUsage>,
    context_window: Option<CompactionContext>,
    conversation_tokens: Option<u64>,
    system_tokens: Option<u64>,
    tool_definitions_tokens: Option<u64>,
}

/// Preserve provider-reported context composition.
struct CompactionContext {
    current_tokens: Option<u64>,
    token_limit: Option<u64>,
    messages_length: Option<u64>,
    conversation_tokens: Option<u64>,
    system_tokens: Option<u64>,
    tool_definitions_tokens: Option<u64>,
}

/// Preserve accounting for the model call that performs compaction.
struct CompactionUsage {
    billing: Option<CompactionCost>,
    input: Option<u64>,
    output: Option<u64>,
    cached_input: Option<u64>,
    cache_write_tokens: Option<u64>,
    duration_ms: Option<u64>,
    model: Option<String>,
}

/// Preserve labeled native billing observations without currency conversion.
struct CompactionCost {
    measurements: Vec<ReportedMeasurement>,
}

/// Preserve the native denomination and precision of one reported measurement.
struct ReportedMeasurement {
    label: String,
    value: String,
    unit: String,
}
```

`operation_id` replaces the old `manual` boolean with the association Harness actually needs. `Some(id)` requires positive correlation with that host request; `None` means no host-operation association is known, not that the provider definitely initiated compaction automatically. The adapter correlates native notifications/results with retained requests rather than assuming the provider returns a Harness operation ID.

The checked compaction constructor and reducer validate state/failure consistency, identity, timestamps, and repeat observations. Once a host-operation association is established, delayed native observations must preserve it; an observation cannot acquire a different operation ID merely because another request is now pending. Successful metadata warnings do not become a failed compaction unless required evidence is missing.

### 5.8 Imported enums, primitives, and reference completeness

Every project-specific struct used by the enum and record declarations above is defined in this section. Standard-library, Tokio, and Serde primitives remain imports, and unchanged leaf enums remain owned by their existing modules rather than being redeclared as competing types.

| Referenced non-struct type | Definition or treatment |
| --- | --- |
| `String`, `Vec`, `Option`, `Result`, `Arc`, `Duration`, `Range` | Standard Rust types; `Duration` values have the units stated by the field name or contract. |
| `JoinHandle`, `tokio::sync::Mutex`, `tokio::time::Instant` | Tokio ownership/synchronization types; these occur only in nonserialized runtime records or explicitly skipped deadline state. |
| `serde_json::Value` | Used only inside `InspectableArguments`; never used as the event discriminator or a hidden lifecycle payload. |
| `NativeTurnDriver` | Internal trait with the run/interrupt/collect contract in section 3.2. |
| `ProviderId` | Existing validated provider-ID newtype, declared in section 5.1 and defined in [model.rs](nvim/rust/forge/crates/forge-provider-api/src/model.rs). |
| `CapabilityId`, `CapabilityOwner`, `CapabilityApplication`, `Support`, `TaskKind` | Existing capability/task enums in [capability.rs](nvim/rust/forge/crates/forge-provider-api/src/capability.rs) and [input.rs](nvim/rust/forge/crates/forge-provider-api/src/input.rs). |
| `ProviderOperation`, `ProviderFailureKind`, `ProviderPhase`, `FailureDisposition`, `ProviderError` | Existing operation/error enums in [error.rs](nvim/rust/forge/crates/forge-provider-api/src/error.rs), modified only as required by section 4.6. |
| `EventDeliveryError` | Existing stream-error enum in [events.rs](nvim/rust/forge/crates/forge-provider-api/src/events.rs). |
| `TextUpdate`, `ToolOutcome`, `ToolSemantics`, `ProviderChangeKind` | Existing tool/change value enums in [tool.rs](nvim/rust/forge/crates/forge-provider-api/src/tool.rs) and [change.rs](nvim/rust/forge/crates/forge-provider-api/src/change.rs). |
| `PermissionTarget`, `DirectoryOperation`, `CommandShell` | Existing permission/access and shell value enums; preserve their native-independent semantics. `CommandHighlight` is a record declared in section 5.6 and defined in [shell.rs](nvim/rust/forge/crates/forge-shell/src/shell.rs). |
| `ChecklistStatus`, `CompactionState`, `RetryCategory` | Existing checklist/compaction/retry value enums; the before/after tables still require each variant's behavior. |

When implementation adds a payload record or a new field whose type is not already defined or explicitly imported, update this section in the same change. Neither an unexplained placeholder type nor a broad enum arm that discards its payload satisfies the plan.

## 6. Before/after mapping for every existing event

The following tables are the removal checklist for the current [event union](nvim/rust/forge/crates/forge-provider-api/src/event.rs) and its nested updates. Removing a variant requires its listed replacement behavior and tests, not deletion of its match arm.

### 6.1 Lifecycle, runtime, and accounting

Lifecycle consolidation preserves input coverage, worker ownership, and timing while removing redundant outcome publication. A pre-start failure is not converted into a fictional native turn.

| Before | After | Behavior that must survive |
| --- | --- | --- |
| `Turn.Started` | Runner-owned `Turn.Started`. | Positive native start and original identity; Copilot call boundaries aggregate into one normalized turn. |
| `Turn.CallFinished` | Native-call timing retained in `Turn.Usage`/final accounting. | Freeze actual model-call time without finishing the response cycle or counting cleanup as generation. |
| `Turn.Summary` | `Turn.Usage` and final identified accounting. | Provider-reported duration and tokens, not a prose-summary event. |
| `Turn.Finished` | Runner-owned `Turn.Completed`. | One outcome after provider collection; item closure and host task effects remain ordered. |
| `Execution.Started` | Registered native start within the normalized turn. | Original input/turn association and watchdog activation. No second public lifecycle. |
| `Execution.Finished` | `TurnResult` returned to runner, then `Turn.Completed`. | Exact covered submissions and outcome; provider-specific interpretation stays behind the adapter boundary. |
| `Execution.ResolvedWithoutExecution` | Operation/delivery resolution, or a canonical turn result only when work actually started. | Confirmed consumed-without-start cancellation is not recast as nonconsumption. Preserve the journal evidence even without a turn event. |
| `Execution.PublicationFailed` | Typed host publication failure with retained native evidence; availability/operation failure as appropriate. | Successful native work must not become permission to replay because host publication failed. |
| `Execution.ResolutionPublicationFailed` | Failed operation resolution with retained input/native evidence. | No success-shaped fallback and no invented execution. |
| `Execution.ObservationLost` | `Provider.Unavailable` plus failed affected waits. | Retain uncertainty and cleanup work; do not certify completion. |
| `Settled` | Provider collection prerequisite inside the runner. | Exact resource ownership and final stream boundary; remove the repeated result field. |
| `SettlementFailed` | `Provider.Unavailable` with collection-phase failure. | Keep native evidence, retained handles, and connection fencing. |
| `Runtime.Resolved` | `Turn.ModelResolved` only, after `Turn.Started`. | Emit the separate model event even when confirmation is available at startup; preserve provider display label through connection metadata and never substitute the configured default for actual model evidence. |
| `Runtime.ContextUsage` | `Thread.ContextUsage`. | Context occupancy and post-compaction measurements, including outside active turns. |
| `Runtime.Progress` | No normalized event; native adapter telemetry if needed. | No lifecycle consumer currently depends on the byte counter. Record an explicit source-classification disposition, not a wildcard discard. |
| `Runtime.ForegroundIdle` | Private native end evidence. | Copilot idle is still parsed and correlated; removing the forwarded hint must not remove its completion logic. |
| `Usage` | `Turn.Usage`. | Report identity, unavailable values, cumulative reconciliation, and late reports targeting the original turn. |

### 6.2 Messages and tools

Generic item lifecycle replaces separate message/tool lifecycle names without collapsing their different outcomes. A tool request can precede execution, and an interrupted message is not a completed answer.

| Before | After | Behavior that must survive |
| --- | --- | --- |
| `Message.Started` | `Item.Started` with `UserMessage`, `AgentMessage`, or `Reasoning` content. | Classification, initial content, and native or synthetic identity. |
| `Message.Classified` | `Item.Message.Classified`. | Late commentary/response classification; unknown remains explicitly unclassified. |
| `Message.Delta` | `Item.Message.Delta`. | Ordered content without truncation. |
| `Message.Completed` | Typed message `Item.Completed`. | Authoritative final snapshot replaces accumulated deltas. |
| `Message.Interrupted` | Typed message `Item.Completed` with `MessageOutcome::Interrupted`. | Preserve partial text without presenting it as an authoritative final response. |
| `Tool.Requested` | Tool-like `Item.Started`; later legitimate updates use `DescriptorUpdated`. | Name, arguments, semantics, type, and title; do not duplicate an item. |
| `Tool.TitleChanged` | `Item.Tool.TitleChanged`. | Display change does not replace admitted operation identity. |
| `Tool.Input` | `Item.Tool.Input`. | Append/replace argument text; preserve structured snapshots through `ToolInputUpdate`. |
| `Tool.Started` | `Item.Tool.ExecutionStarted`. | Actual execution timing remains distinct from request/argument admission. |
| `Tool.Backgrounded` | `Item.Tool.Backgrounded`. | Only positively observed continuing work retains background lifetime. |
| `Tool.Output` | `Item.Tool.Output`. | Append versus replace, output sequencing, and original item ownership. |
| `Tool.Changes` | `Item.Tool.Changes`. | Reported diffs remain distinct from authoritative Harness checkpoint diffs. |
| `Tool.SkillRead` | `HarnessEvent::SkillRead`. | Host-derived skill attribution still updates the original item after successful observation. |
| `Tool.Finished` | Typed tool-like `Item.Completed`. | Preserve succeeded, failed, denied, cancelled, and interrupted separately. |

### 6.3 Thread, agent, host, and diagnostic events

Moving host events out of the provider union removes an ownership ambiguity, not their effects. Every presentation mutation still goes through the session owner and its durable state boundary.

| Before | After | Behavior that must survive |
| --- | --- | --- |
| `CapabilitiesChanged` | `Provider.CapabilitiesChanged`. | Binding-specific effective support, revision, and ordered observation. |
| `SessionIdentity` | `Thread.IdentityResolved`. | Late native conversation resolution; never silently create new history after a failed resume. |
| `Terminals` | `Thread.TerminalsUpdated(Ok(snapshot))`. | Binding/thread inventory independent of foreground turn lifetime. |
| Inventory-unavailable diagnostic | `Thread.TerminalsUpdated(Err(failure))`. | Retain the last successful snapshot and expose the refresh failure without automatically disabling an otherwise healthy conversation. |
| `Compaction` | `Thread.CompactionUpdated`. | Compaction identity, optional positively correlated host operation ID, state, summary, metrics, and failure details; absence of correlation does not establish automatic origin. |
| `Agent.ConversationResolved` | `Agent.ConversationResolved`. | Child association, not parent conversation reassignment. |
| `Agent.Lifecycle` | New typed `Agent.Lifecycle`. | Parent relationship and full active/terminal state without impossible kind/state combinations. |
| `Checklist` | `Turn.ChecklistUpdated`. | Native plan/todo identity, replacement/merge semantics, and superseded entries. |
| `Approval.Requested` | Host `PermissionApproval.Requested`. | Pending registry, exact request, per-target choices, and deadline. |
| `Approval.Resolved` | Host `PermissionApproval.Resolved`. | Persisted decision and source; allow and deny remain distinguishable. |
| `Approval.Cancelled` | Host `PermissionApproval.Cancelled`. | Reason why no committed decision remains; no implicit native turn success. |
| `DirectoryAccess` | Host `DirectoryAccess.Granted`. | Persisted read/write grant, originating owner, and policy limits. |
| `InputAccepted.Steering` | Acceptance operation receipt applied directly by the session input owner. | Transcript insertion, deduplication, wait clearing, retraction eligibility, and persistence. |
| `InputAccepted.TaskStarted` | Same receipt path plus existing Harness workflow-entry metadata. | Plan/Execute/Goal timeline entry; it is not native child-agent creation. |
| `Retry(Some)` / `Retry(None)` | Host `RetryUpdated` carrying `RetryEvent::Scheduled` / `RetryEvent::Cleared`. | Retained retry thread/turn IDs, deadline, cancellation, and UI state. |
| `ParentBoundary.WaitStarted/Updated/Finished` | Host `ChildWait.Started/Updated/Finished`. | Parent-turn wait ownership; child turns remain independent. |
| Nonfatal `Diagnostic` | Typed `Provider.Warning` or a feature-specific failure update. | Explicit reason and correct consumer action, not a generic fatal bit. |
| Fatal `Diagnostic` | `Thread.Error`, `Turn.Error`, confirmed native turn/item failure, operation error, or `Provider.Unavailable`, according to evidence. | Do not blindly map every old `fatal: true` into the same lifecycle transition. |
| Native session error without a native turn ID | `Thread.Error { error }`, using its native session association. | Do not assign it to the active turn based on reader ownership or timing, and do not manufacture a terminal outcome. |

## 7. Native provider event and API mappings

Every target event below has an identified producer or an explicit unsupported/host-derived disposition. A blank mapping is not permission to omit an implementation; each unsupported case must be checked against the negotiated capability and covered by a fixture.

### 7.1 Source ledger and protocol versions

Use the installed adapter source and generated protocol schemas as the implementation authority, with public documentation as a semantic reference. The ACP mapping is for negotiated v1 and must not borrow acknowledgement semantics from the v2 draft.

| Label | Source boundary |
| --- | --- |
| `CX-N` | [Codex normalization](nvim/rust/forge/crates/forge-providers/src/codex/normalization.rs), [decode](nvim/rust/forge/crates/forge-providers/src/codex/decode.rs), and [protocol routing](nvim/rust/forge/crates/forge-providers/src/codex/protocol.rs). |
| `CX-P` | [Codex provider operations and collector](nvim/rust/forge/crates/forge-providers/src/codex/provider.rs), [approvals](nvim/rust/forge/crates/forge-providers/src/codex/approval.rs), and [descendants](nvim/rust/forge/crates/forge-providers/src/codex/descendants.rs). |
| `CP-N` | [Copilot event decoder](nvim/rust/forge/crates/forge-providers/src/copilot/event_decoder.rs), [turn evidence](nvim/rust/forge/crates/forge-providers/src/copilot/turn.rs), and [prompt driver](nvim/rust/forge/crates/forge-providers/src/copilot/prompt.rs). |
| `CP-P` | [Copilot provider operations](nvim/rust/forge/crates/forge-providers/src/copilot/provider.rs), [SDK handle](nvim/rust/forge/crates/forge-providers/src/copilot/handle.rs), [terminal inventory](nvim/rust/forge/crates/forge-providers/src/copilot/terminal.rs), and [compaction observer](nvim/rust/forge/crates/forge-providers/src/copilot/compaction.rs). |
| `ACP-N` | [ACP session updates](nvim/rust/forge/crates/forge-providers/src/acp/session.rs), [children](nvim/rust/forge/crates/forge-providers/src/acp/children.rs), and [stop reasons](nvim/rust/forge/crates/forge-providers/src/acp/response.rs). |
| `ACP-P` | [ACP provider](nvim/rust/forge/crates/forge-providers/src/acp/provider.rs), [conversation configuration](nvim/rust/forge/crates/forge-providers/src/acp/conversation.rs), and [submission extension](nvim/rust/forge/crates/forge-providers/src/acp/submission.rs). |
| Public Codex | [App-server API](https://developers.openai.com/codex/app-server/). Names such as `turn/completed` and `item/commandExecution/requestApproval` are native protocol names. |
| Public ACP | [ACP v1 prompt turns](https://agentclientprotocol.com/protocol/v1/prompt-turn), [tool calls](https://agentclientprotocol.com/protocol/v1/tool-calls), and [session setup](https://agentclientprotocol.com/protocol/v1/session-setup). |
| Public Copilot SDK | [SDK session implementation](https://github.com/github/copilot-sdk/blob/main/rust/src/session.rs). Pin validation to the version selected by this repository's Cargo lockfile, not an unreviewed upstream main branch. |

`Synthesized` below means deterministic construction from identified native evidence, not guessed completion. `Conditional` means the negotiated provider/version must support the native observation. Claude ACP and Antigravity ACP share parsing, but their capabilities and extensions must be tested separately.

### 7.2 Provider and thread events

Thread metadata can arrive without a running turn. A feature failure must retain its own scope rather than poisoning the whole connection unless the underlying evidence establishes connection-wide failure.

| Target event | Codex native mapping | Copilot native mapping | Claude/Antigravity ACP mapping |
| --- | --- | --- | --- |
| `Provider.CapabilitiesChanged` | Open/configure results plus adapter-declared support; use `modelProvider/capabilities/read` only when supported and selected. No blanket forwarding of arbitrary native metadata (`CX-P`). | SDK/runtime discovery and negotiated session configuration; no assumed native event with this exact name (`CP-P`). | `initialize` capability exchange and validated configuration/catalog updates; `_session/steering` belongs only to the advertising adapter (`ACP-P`). |
| `Provider.Warning` | Native warnings/notices and unsupported message-phase metadata; native error notifications retain their error scope instead (`CX-N`). | Optional metadata limitations and SDK notices; `session.error` maps to `Thread.Error`, not a warning or inferred turn failure (`CP-N`). | Known optional-metadata/notice observations and explicitly classified normalization limitations; v1 `notice` is conditional on negotiated support (`ACP-N`). |
| `Thread.Error` | Native error notifications that identify a thread without an explicitly correlated turn. | `session.error`, associated with its SDK session; do not add a turn ID from the receiving prompt context. | A native session-scoped error notification only if actually supplied; failed RPC operations remain returned errors, not fabricated notifications. |
| `Provider.Unavailable` | Socket/process failure, required protocol violation, or unsuccessful retained collection; the runtime emits it from typed failure (`CX-P`). | SDK intake loss, required ancestry/identity failure, or unsuccessful callback/worker collection; runtime-owned (`CP-P`). | RPC/process failure, unsupported required terminal response, or failed process/callback collection; runtime-owned (`ACP-P`). |
| `Thread.IdentityResolved` | `thread/start`, `thread/resume`, or `thread/fork` result; validated `thread/started` association (`CX-P`). | Created/resumed SDK session ID and late session identity already retained by the binding (`CP-P`). | `session/new` or `session/load` response `sessionId`; no invented native turn ID (`ACP-P`). |
| `Thread.ContextUsage` | `thread/tokenUsage/updated`-style reports parsed from `tokenUsage.last.totalTokens` and `modelContextWindow`; preserve current accounting/baseline semantics unless separately changed (`CX-N`). | `session.usage_info` with `currentTokens` and `tokenLimit` (`CP-N`). | `session/update` with `usage_update` fields `used` and `size`; outside-turn updates remain thread-scoped (`ACP-N`). |
| `Thread.TerminalsUpdated` | Terminal-change notifications trigger `thread/backgroundTerminals/list`; publish `Ok` with the normalized snapshot or `Err` with the observer's typed query failure (`CX-P`). | `session.background_tasks_changed` triggers SDK `rpc().tasks().list()`, with the existing one-time `tasks().refresh()` at attachment; publish its normalized `Ok` or typed `Err` (`CP-P`). | Explicitly unsupported by the current ACP provider; an explicit inventory call returns unsupported, and no observer or successful empty inventory is fabricated (`ACP-P`). |
| `Thread.CompactionUpdated` | `item/started` and `item/completed` for `contextCompaction`, plus correlated `thread/compact/start` operation results (`CX-N`, `CX-P`). | `session.compaction_start` / `session.compaction_complete` and correlated manual compaction observations (`CP-P`). | Conditional `session/update` variants `compaction_update` and `compaction_summary_chunk`; unsupported summary/metrics remain unavailable (`ACP-N`). |

### 7.3 Turn events

Native status parsing must be shared by transcript/item finalization and returned turn outcome. In particular, a native event named "end" or "completed" is not itself proof of success without its status or terminal evidence.

| Target event | Codex native mapping | Copilot native mapping | Claude/Antigravity ACP mapping |
| --- | --- | --- | --- |
| `Turn.Error` | Native error notification with explicit thread/turn correlation, validated against the retained native ID mapping. It is not equivalent to `turn/completed`. | No mapping from bare `session.error`; only an independently supported native notification with explicit turn correlation could produce this event. | No inferred mapping from session-only updates or prompt ownership; require a supported explicitly turn-correlated native notification. |
| `Turn.Started` | Positive `turn/started` or the validated started turn in `turn/start` response, deduplicated by identity (`CX-P`). | First positively correlated root work for the response to `session.send`; subsequent `assistant.turn_start` model-call boundaries stay inside that turn (`CP-N`). | Accepted processing evidence associated with the retained `session/prompt`; the current adapter creates a synthetic turn address. Do not claim native acceptance merely because the request was written (`ACP-N`, `ACP-P`). |
| `Turn.ModelResolved` | Actual model confirmed in native response/turn metadata when supplied; configured override alone remains requested configuration (`CX-P`). | Root native event model metadata currently handled in `event_decoder`; `set_model` confirmation establishes configuration but must not be mislabeled as per-turn observation (`CP-N`, `CP-P`). | Confirmed native model metadata if the adapter reports it. `config_option_update` or `session/set_model` may establish configuration only; absent actual execution metadata produces no model event (`ACP-P`). |
| `Turn.Usage` | Identified token-usage updates and their cumulative thread counts; model-call timing only if reported (`CX-N`). | `assistant.usage` with API-call/event identity; `assistant.turn_start/end` timestamps contribute native-call timing without ending the canonical turn (`CP-N`). | Claude `_claude/sdkMessage` with `message.type=result`: result UUID identifies accounting and `duration_ms` supplies timing. This is a Claude extension, not a guaranteed Antigravity event. ACP `usage_update` context occupancy alone is not a per-turn token charge (`ACP-N`). |
| `Turn.ChecklistUpdated` | `turn/plan/updated` and supported typed native plan observations currently parsed by the checklist path (`CX-N`). | `session.todos_changed`, followed by the existing SDK todo read and identified checklist update (`CP-N`). | Standard v1 `session/update` with `plan`. The current root update match does not implement it; add the typed mapping and native fixture rather than ignore it (`ACP-N`, public ACP). |
| `Turn.Completed` | Parse final `turn/completed.turn.status` once, retain coverage and outcome, join the worker, then publish through the runner (`CX-P`). | Parse correlated root idle/abort/failure plus all accepted inputs, retain one response-cycle result, collect worker/callbacks, then publish. Never use a single `assistant.turn_end` as success (`CP-N`, `CP-P`). | Parse the schema-valid `session/prompt` terminal `stopReason`, reconcile acceptance, collect the native worker/callbacks, then publish. Cancellation uses `cancelled`; no early ACP v2 acceptance semantics (`ACP-N`, `ACP-P`). |

Codex `completed` maps to `Completed(Normal)`, `interrupted` and supported `cancelled` to `Interrupted`, and `failed` to `Failed` with its native retry/error evidence. Missing or unknown status is an explicit protocol failure, not `Completed`.

ACP `end_turn`, `max_tokens`, `max_turn_requests`, and `refusal` map to the matching `TurnCompletionReason`; `cancelled` maps to `Interrupted`. A future schema variant must not reach a catch-all success branch. Copilot's `HostHandoffIdle` remains an interruption plus retained host handoff state, not fabricated native success.

### 7.4 Item events

Item notifications retain individual completion before turn cleanup so tool deadlines, partial output, and background lifetime remain accurate. The table includes synthesized message completion and metadata updates because not every native protocol emits a corresponding standalone message event.

| Target event | Codex native mapping | Copilot native mapping | Claude/Antigravity ACP mapping |
| --- | --- | --- | --- |
| `Item.Started` / `UserMessage` | Native `userMessage` item/history evidence associated with an accepted submission. | Correlated root `user.message` and its acknowledged message ID. | Conditional native `user_message_chunk` or replay evidence; otherwise retain accepted input in Harness history without inventing a native user item. |
| `Item.Started` / `AgentMessage` | `item/started` with `agentMessage`; a validated first delta can establish missing initial content according to the adapter contract. | `assistant.message_start`, or first identified message delta/snapshot as currently supported. | First `agent_message_chunk` creates a deterministic adapter-local message item under the active prompt. |
| `Item.Started` / `Reasoning` | Reasoning item start and its identified summary/content parts. | `assistant.reasoning_delta` / `assistant.reasoning`, preserving full versus summary semantics. | First `agent_thought_chunk`; provider omission does not create an empty reasoning item. |
| `Item.Started` / tool-like types | `item/started` with `commandExecution`, `fileChange`, `mcpToolCall`, `dynamicToolCall`, or other explicitly mapped tool-like item; preserve original subtype metadata. | Identified `assistant.message.toolRequests` or `tool.execution_start`; normalize command/file-change/tool semantics. | `tool_call` with `kind=execute` maps to command, edit/delete/move to file change, and other supported tool kinds to tool call. |
| `Item.Message.Classified` | Known `agentMessage.phase`: `commentary` or `final_answer`. Unsupported phases warn and remain unclassified. | Message-start/final metadata `commentary` or `final_answer`; preserve current explicit-empty-final behavior. | Current adapter-derived classification: prose preceding tool activity is commentary and terminal assistant prose is response; record this as synthesized classification, not a native phase field. |
| `Item.Message.Delta` | `item/agentMessage/delta` and identified reasoning text/summary deltas. | `assistant.message_delta`, `assistant.reasoning_delta`. | Text from `agent_message_chunk` and `agent_thought_chunk`. |
| `Item.Tool.DescriptorUpdated` | Validated later item metadata for the same native ID; native type conflicts fail. | Later identified tool descriptor metadata, if present; do not invent updates. | Changed `title`, `kind`, or `rawInput` currently re-emitted as `Tool.Requested`; preserve valid updates and reject actual type conflicts. |
| `Item.Tool.TitleChanged` | Identified tool snapshot title change. | Normalized title changes in the existing tool decoder. | `tool_call_update.title`, preserving stable tool identity. |
| `Item.Tool.Input` | Structured arguments from tool item snapshots; text deltas only if the supported native protocol supplies them. | `assistant.tool_call_delta` and final argument snapshots. | `rawInput` snapshots in `tool_call` / `tool_call_update`; do not append whole replacement objects as text. |
| `Item.Tool.ExecutionStarted` | Native tool item execution status/start; normalize once. | `tool.execution_start`. | `tool_call` / `tool_call_update.status=in_progress`; `pending` is not proof of execution. |
| `Item.Tool.Backgrounded` | Positively identified surviving command and native terminal inventory; existing normalization marks pending commands at successful foreground end. | Explicit SDK terminal/tool background evidence. | Unsupported by the current ACP adapter; do not infer background execution from an unfinished tool. |
| `Item.Tool.Output` | `item/commandExecution/outputDelta`, supported tool output deltas, and final output snapshots. | `tool.execution_partial_result`, `tool.execution_progress`, `tool.shell_output`, and final output. | Tool content/raw output in `tool_call_update`; usually replacement snapshots. |
| `Item.Tool.Changes` | Native `fileChange` item contents and validated tool-reported changes. | Structured change data or the existing validated file-change extraction from tool output. | Tool diff content and locations interpreted by `file_changes`; not independent filesystem observation. |
| `Item.Completed` / message types | Native `item/completed` authoritative content; unfinished messages close as interrupted when the native outcome establishes interruption. | `assistant.message` / `assistant.reasoning` snapshots; finalize remaining partial messages from the single retained outcome. | Synthesized from prompt/child termination after flushing identified chunks; no fabricated normal completion after cancellation. |
| `Item.Completed` / tool-like types | Native `item/completed` status and result, preserving failure/denial/interruption. | `tool.execution_complete`, preserving streamed-output sequencing and final snapshots. | Terminal `tool_call_update.status` values: completed, failed, denied, cancelled, interrupted. Unknown status fails explicitly. |

Tool-related events may describe failed work while the turn later completes normally after recovery. A tool's failure is not automatically the turn's failure. Conversely, message completion is not proof that all turn inputs and callbacks are finished.

### 7.5 Agent events

Native child activity is correlated through explicit parent relationships. The adapter must not adopt arbitrary foreign-thread traffic merely because it arrived on the same connection.

| Target event | Codex native mapping | Copilot native mapping | Claude/Antigravity ACP mapping |
| --- | --- | --- | --- |
| `Agent.ConversationResolved` | Child thread IDs reported by supported collaboration/subagent activity or child-open results; current decoder recognizes `collabAgentToolCall` and `subAgentActivity` records. | Child agent/session association from `subagent.started` and subsequent identified child events. | `subagentSessionId` from conditional `subagent_spawned`, or managed child `session/new/load` result. |
| `Agent.Lifecycle.Started` | Explicit spawn/start relationship and child state; exact supported native shapes must replace current substring heuristics. | `subagent.started`. | Conditional `session/update.subagent_spawned`. |
| `Agent.Lifecycle.Updated` | Supported collaboration state updates and child thread/turn observations. | Identified ongoing native child observations where supplied; do not manufacture periodic updates. | Conditional `subagent_state_update` active states; unsupported states require explicit classification. |
| `Agent.Lifecycle.Finished` | Explicit child completion/failure/interruption/closure from supported collaboration or child lifecycle evidence. | `subagent.completed` / `subagent.failed`, preserving the actual outcome. | Terminal `subagent_state_update` values such as completed, failed, cancelled, or disconnected; retain the distinction between interrupted and closed. |

Child turn/item activity uses the same normalized turn/item contract with the child's thread, turn, and item IDs. Agent lifecycle summaries must not publish a second contradictory outcome for an already-known child turn; the adapter's relationship reducer validates that association.

### 7.6 Authorization, host events, and input operations

These mappings establish which side initiates each interaction and which side owns its state change. Host-origin events have no direct native-notification equivalent and must not be added to an adapter's required event match merely to preserve the old union.

| Target request/event | Codex native mapping | Copilot native mapping | Claude/Antigravity ACP mapping |
| --- | --- | --- | --- |
| Authorization request/response | `item/commandExecution/requestApproval`, `item/fileChange/requestApproval`, `item/permissions/requestApproval`, and `mcpServer/elicitation/request`; respond to the exact RPC ID. | SDK permission callback and tool-call correlation; return the SDK permission response through the adapter. | `session/request_permission`; return the selected/cancelled outcome for its request and supported option set. |
| `PermissionApproval.Requested` | Host registry creates the UI request after policy evaluation needs a decision. | Same host behavior. | Same host behavior. |
| `PermissionApproval.Resolved` | Host persists per-target choices/rules, then resolves the native callback. | Same host behavior. | Same host behavior, translating allowed choices into native option IDs. |
| `PermissionApproval.Cancelled` | Host cancels the pending registry entry with a typed reason. | Same host behavior. | Same host behavior; do not report a fake accepted permission choice. |
| `DirectoryAccess.Granted` | Host-persisted directory read/write choice, not an independent Codex notification. | Same host behavior. | Same host behavior. |
| `RetryUpdated.Scheduled/Cleared` | Host retry policy consumes native retry evidence from the turn failure. | Same host policy consumes SDK error classifications. | Same policy only when the adapter supplies sufficient retry/delivery evidence. |
| `ChildWait.Started/Updated/Finished` | Host wait state for managed children; native collaboration wait observations must retain their native origin if they are also represented. | Host wait state; do not infer completion from `assistant.idle`. | Host wait state; no fabricated provider wait event. |
| Host `SkillRead` | Existing semantic read-target observer on successful native tool observations. | Same host observer. | Same host observer. |
| `turn_start` acceptance result | Successful `turn/start` response and its submitted-client-ID correlation. | `session.send` acknowledgement with message ID. | Valid `session/prompt` response in v1, which may arrive only at native end. |
| `turn_steer` acceptance result | Successful `turn/steer` response. | Accepted subsequent `session.send` mapped to the exact live response cycle. | Claude's advertised `_session/steering`; Antigravity remains unsupported unless it explicitly advertises equivalent support. |
| Positive steering nonconsumption | Existing typed pre-admission rejection evidence. | Only positive SDK/native evidence, not timeout. | Claude's exact `promptRequired` / `noRunningTurn` response with `idleBehavior: promptRequired`; no automatic fallback for uncertain results. |

Do not collapse arbitrary MCP elicitation into file or command approval if its schema asks for structured user input rather than authorization. Inspect the installed Codex/ACP request schema in the adapter phase and either route through the existing host question service or reject unsupported semantics explicitly. A matching method name is not proof that the request is a permission prompt.

The inspected `ParentBoundary` event has production consumers and fixture publishers but no production publisher in the native adapters. Connect the new `ChildWait` notifications to the actual Harness child-wait state transitions; do not claim that forwarding a nonexistent native wait event implements this row.

### 7.7 Mapping gates, not silent gaps

Some mappings need new native fixtures because current code is absent or permissive. These are required implementation gates, not optional follow-up work.

| Gate | Required evidence before declaring the mapping complete |
| --- | --- |
| ACP root `plan` updates. | A negotiated-v1 fixture produces a checklist update with correct replacement/merge behavior. |
| ACP user-message replay. | A load/replay fixture proves it cannot duplicate a newly accepted user item or create active work. |
| Actual model reporting. | Each adapter distinguishes configured default, requested override, and confirmed execution model. Confirmation known at startup still emits a separate `Turn.ModelResolved` after `Turn.Started`; unavailable confirmation emits no model event. |
| Codex collaboration shapes. | Exact schemas for the pinned protocol replace substring matching; foreign threads are rejected without side effects. |
| Copilot model-call versus response-cycle boundaries. | Multiple calls, steering into the current cycle, abort after a call end, and background wakeups retain one correct normalized-turn result. |
| Late accounting. | Native usage arriving after content end is attributed and deduplicated without reopening the turn. |
| Descriptor updates. | ACP repeated metadata and partial tool arguments survive without creating duplicate items or changing a completed operation. |
| Warning versus required evidence loss. | Optional display limitations warn; missing output/lifecycle/receipt evidence fails at its owning boundary. |

## 8. Exhaustive handling and prohibition on discarded variants

An enum only helps if consumers actually handle its alternatives. This implementation must not obtain a green build by replacing missing behavior with catch-all branches, empty arms, ignored return values, or successful defaults.

### 8.1 Required implementation rules

Every domain reducer must explicitly match every variant of the enum it receives. A new event or outcome must cause compilation errors in the consumers whose contracts need updating.

Every added or changed enum variant must have a one-line `///` explanation of what evidence or action it represents, including policy scope terms such as `BroadRule`. Every referenced payload struct must have a concrete field definition or an explicit unchanged source definition; a descriptive name alone is not a sufficient contract.

The following are prohibited in lifecycle, native-status translation, event routing, persistence, and outcome conversion:

```rust
// Prohibited patterns, not implementation examples.
_ => Ok(()),
_ => TurnOutcome::Completed { stop_reason: TurnCompletionReason::Normal },
SomeEvent::RequiredVariant { .. } => {},
let _ = apply_required_observation(event);
```

Do not add `#[allow(unreachable_patterns)]`, broad catch-all `Other` events, stringly typed `kind` fields, unchecked JSON dispatch, `as any` equivalents, or generic "ignored event" variants to evade compiler work. Do not use `..` in conversions that must consider every source field; destructure fields explicitly so additions require review.

These rules do not imply every view must render every event. The central router handles all provider variants and sends each consumer a smaller purpose-specific enum. A transcript-only renderer should not receive `Provider.Unavailable` and silently discard it; the connection owner handles availability and the UI receives the resulting typed host state.

### 8.2 Duplicates, stale identities, and unsupported wire events

An identical replay and an unsupported observation are different cases. Only validated identical replay can be treated as an idempotent acknowledgement without reapplying effects.

`ObservationAdmission::IdenticalReplay` requires equality of the original identity, immutable content, outcome, and relevant revision/coverage. Count or trace that disposition under normalized diagnostic metadata. A stale or foreign event must be rejected or routed to its still-owned historical object; it cannot update the newest turn or disappear without an explicit disposition.

Native protocols are open to new wire messages, but the normalized API is closed. Parse native data at the adapter boundary: known irrelevant transport metadata may have a narrowly documented, tested admission policy; unknown content or lifecycle messages must produce a warning or typed failure according to whether required behavior can still be established. Do not invent successful results for unfamiliar values.

The consumer audit must produce an event-to-handler coverage matrix. Every row names the owning handler, durable/presentation effect, duplicate behavior, late-event behavior, and a test. Source-only grep is a review aid, not proof of semantic completeness.

### 8.3 Compile-time and runtime protection

Private construction and restricted writer types prevent adapters from independently publishing a completion or connection-unavailable result. They do not prove that external provider data was interpreted correctly.

Add compile-fail documentation or the repository's existing equivalent showing that an adapter activity writer cannot accept `Turn.Completed`, `Provider.Unavailable`, or host approval events. Use integration/native-peer fixtures to prove classification, ordering, coverage, and cancellation behavior. Do not add a new testing dependency solely for syntax examples if compile-fail doctests suffice.

### 8.4 Required behavior in Harness consumers

The overhaul changes what Harness can learn from its providers, not just the shape of the adapter output. Every newly preserved distinction requires an explicit consumer behavior or a documented, tested metadata purpose; storing a value without using it is not implementation completion.

This section specifies behavior required above the provider boundary: request/task decisions, retry scheduling, persistence, and user presentation. Adapter parsing and successful serialization are prerequisites, not substitutes for these behaviors.

#### Native stop reasons

The current ACP parser erases the difference between normal completion, refusal, token limit, and request limit. The replacement must preserve that difference through finalization and use it even if the provider supplies no explanatory message.

| Native reason | Required visible notice on the affected turn | Required host behavior |
| --- | --- | --- |
| `Normal` | Use normal completed-turn presentation; do not invent a warning. | Apply existing completion/task verification rules; a normal provider end alone still does not prove an objective was achieved. |
| `Refusal` | "The provider refused to continue." | Stop automatic retry and agent continuation for this request; retain partial output and effects, and leave unresolved work explicitly stopped rather than silently successful. |
| `TokenLimit` | "The provider stopped because it reached its token limit." | Stop automatic retry and agent continuation based on this result; retain unresolved work and permit an explicit user decision to continue or change the request. |
| `RequestLimit` | "The provider stopped because this turn reached its model-request limit." | Stop automatic retry and agent continuation based on this result; retain the specific reason and unfinished request state. |

Generate these notices from the typed reason, not from text matching, a generic diagnostic, or the presence of an assistant response. Display them even for an empty response, retain them across history reload, and derive them idempotently so replay cannot append duplicate notices. A token-limit notice must not claim that the thread context is full unless separate context evidence establishes that fact.

For a non-normal stop, finish required resource cleanup, host-effect accounting, and checkpoint finalization. Do not launch another provider turn through goal scheduling, child-result continuation, check-repair continuation, or configuration reconciliation merely because the outer native outcome is named `Completed`. Those paths must inspect the reason and retain the stopped request until an explicit user action authorizes further work; already completed actions must not be replayed.

Do not relabel the native reason as a transport failure or make the provider connection unavailable solely because of refusal or a limit. Conversely, do not interpret `Normal` as permission to bypass task validation or required checks. The request/task owner derives host disposition from the native reason, retained workflow state, and explicit user intent.

This deliberately adds behavior that the current implementation lacks. Any existing goal/plan completion or retry policy that only matches `Completed` must be updated and tested against every `TurnCompletionReason` variant.

#### Behavior matrix for the rest of the contract

Each row names the effect that must accompany producer and schema changes. The implementation checklist must trace the row to concrete consumers and tests; a data-only migration is insufficient.

| Newly explicit distinction | Required consumer behavior | Verification beyond parsing |
| --- | --- | --- |
| One post-cleanup `Turn.Completed`. | Finish native lifecycle once, then run mandatory host finalization; do not wait for removed execution/settlement events. | Root, child, and isolated-generation callers all finish without hanging or duplicate finalization. |
| Confirmed interruption versus requested interruption. | Preserve natural completion if it won the race; otherwise distinguish explicit cancellation from an authorized configuration restart. | Both event orders preserve the true result and never cancel a successor. |
| Known native outcome followed by collection failure. | Fail affected waits, fence the connection, retain the native result and unfinished cleanup, and show the collection failure. | A known successful native result cannot become ordinary completion while cleanup remains unsafe. |
| `Provider.Warning`. | Display the scoped warning without changing unrelated request lifecycle or retry eligibility. | A warning remains visible without ending a healthy turn or being promoted to a generic fatal error. |
| `Provider.Unavailable`. | Block new work on that binding, resolve affected waiters with an error, and expose the recovery boundary. | A broken event channel cannot prevent error delivery through the retained operation/owner path. |
| Session error versus turn error versus connection failure. | Preserve `Thread.Error` for session-only errors and `Turn.Error` only for explicitly turn-correlated errors; neither observation finalizes work. Use availability failure only when connection/lifecycle/collection evidence requires it. | Copilot `session.error` remains thread-scoped even during an active turn; Codex's explicitly identified turn errors reach the matching turn; neither fabricates a failed completion or an automatic retry. |
| Terminal inventory `Ok` versus `Err`. | Replace inventory and clear error on success; retain last successful inventory and report refresh failure on error. | A success with unchanged inventory after an error still clears the error. |
| Optional compaction operation correlation. | Reconcile notifications with the same identified compaction; preserve known operation IDs and do not label absence as proven automatic origin. | Delayed observations cannot attach themselves to the next requested compaction. |
| Compaction failure versus optional metadata warning. | Show the actual operation state and available summary; warnings cannot silently alter success/failure, and required observation loss cannot be hidden as missing metrics. | Replay, failure, and partial summary updates retain the correct operation and visible state. |
| Actual model versus configured default. | Initialize confirmed model state as absent and populate it exclusively through `Turn.ModelResolved`, regardless of when confirmation becomes available. Keep thread defaults separate. | Known-at-start and delayed confirmation use the same handler after `Turn.Started`; absent confirmation remains unknown, duplicate reports are idempotent, and a late report cannot rewrite another turn or thread configuration. |
| Per-call usage versus final/cumulative accounting. | Deduplicate reports, reconcile totals, preserve unavailable categories, and exclude collection time from native duration. | Streamed and final copies of the same usage do not count twice. |
| Thread context occupancy versus token expenditure. | Update the thread meter and compaction measurements without charging the same value as new turn usage. | Context reduction after compaction does not reduce historical token expenditure. |
| Normalized versus native IDs. | Resolve typed IDs through the owning registry and preserve original attribution during delayed observations and replay. | ACP synthetic message IDs and repeated Copilot native turn numbers never merge distinct items or reassign old content. |
| Message completion versus interruption. | Preserve partial text and visibly distinguish an interrupted message from an authoritative final answer. | Empty or partial cancelled turns cannot appear as successful answered requests. |
| Tool request versus execution start. | Show preparation separately from running execution and preserve the appropriate timing/deadline boundary. | Pending argument/approval work does not fabricate a native execution start. |
| Descriptor replacement versus a new item. | Update the existing item without duplicating it or silently changing its established identity/type. | Repeated ACP metadata updates retain a single correctly attributed tool item. |
| Foreground versus background tool lifetime. | Close foreground deadlines at actual item end; preserve background output, exit status, and deadlines under the original turn. | Slow provider collection does not create a false tool timeout or terminate unrelated background commands. |
| Child lifecycle versus parent lifecycle. | Update the child's identity/state and the parent's specific wait separately; child content stays in its own thread/turn. | Parent completion or a newer parent turn cannot steal late child events. |
| Approval resolution source and target decisions. | Display actual per-target decisions and their source; cancellation is not denial, and timeout is not a user choice. | Saved policy, approval mode, user action, and deadline paths remain distinguishable after persistence and reload. |
| Exact/broad rules versus directory grants. | Persist only the selected, displayed patterns/grants; enforce read/write separation and writes-off policy. | A once-only approval cannot create a broad rule, and a read grant cannot authorize writes. |
| Direct input acceptance receipt. | Apply transcript/workflow entries, wait clearing, retraction state, and persistence directly in the session owner. | Removing `InputAccepted` events does not remove their existing host effects or cause accepted input to be resent. |
| Ordinary results instead of delivery-proof wrappers. | Consume typed operation errors and the existing journal directly; determine whether work started from the retained runner state, not from a blanket interpretation of `Err`. | Cancellation before send, explicit rejection, uncertain send, and accepted input followed by failure remain distinct without a second acceptance/evidence model. |
| Explicit retry decision. | Schedule another attempt only when `RetryDecision::Allowed` and current host policy permit it; honor its delay and revalidate cancellation before dispatch. | Temporary errors with unknown delivery or unsafe prior effects remain `NotAllowed`; no provider-specific replay-proof object is required by the caller. |
| Accepted input with no native turn start. | Preserve accepted delivery evidence and complete the host operation without inventing a native turn or nonconsumption. | Cancellation before native start cannot make accepted input eligible for blind replay. |
| Provider checklist versus Harness workflow. | Preserve reconciliation, tool attribution, and retraction behavior; do not interpret checklist completion as task verification. | Current bookkeeping remains functional; visualization is the separate follow-up in `evenstream-followup.md`. |
| Host-derived skill attribution. | Continue associating successful semantic reads with the original tool item after removing the event from the native union. | The host observer still produces the annotation without requiring a provider-specific skill event. |
| Host retry and child-wait events. | Preserve cancellation, scheduling, and visible wait changes while keeping them out of the native producer contract. | Removing the old union variants cannot silently disable retry indicators or strand parent waits. |

For each producer mapping in section 7, record which of these consumer behaviors it enables, which existing behavior it preserves, and which behavior is newly required. If a capability is unavailable, test its explicit absence/error contract rather than claiming the corresponding consumer behavior was exercised.

## 9. Affected modules and preservation obligations

This is a coordinated contract migration across three Rust crates and the host's presentation boundary. Updating only the enum definitions or only Copilot leaves incompatible assumptions in the remaining consumers.

| Boundary | Primary paths | Required change |
| --- | --- | --- |
| Provider contract | `forge-provider-api/src/{provider,event,events,execution,lifecycle,session,operations}.rs` | New operations/identities, restricted activity publication, typed final turn records, and removal of public execution/settlement outcomes. |
| Item and accounting types | `forge-provider-api/src/{message,tool,turn,usage,context,agent}.rs` | Typed item lifecycle and roles, canonical turn accounting, child ownership, and precise scopes. |
| Approval/error types | `forge-provider-api/src/{permissions,approval,access,error}.rs` | Separate host approval notifications, typed permission dispositions, warnings, and availability errors. |
| Shared native runtime | `forge-providers/src/{registry,retained_task}.rs` and a cohesive turn-runner module | Extract the common retained completion/collection path without introducing another coordinator or replacing protocol-specific resource owners. |
| Codex | `forge-providers/src/codex/{provider,normalization,protocol,decode,approval,descendants}.rs` | Single status parser, canonical turns/items, exact native schemas, collector-owned completion, and request mapping. |
| Copilot | `forge-providers/src/copilot/{provider,prompt,turn,event_decoder,event_stream,intake,terminal,compaction}.rs` | Aggregate native call evidence, remove end-marker success inference, preserve input ancestry/coverage, and publish completion only after collection. |
| ACP | `forge-providers/src/acp/{provider,session,response,children,conversation,submission}.rs` | v1 stop reasons, late acceptance, stream-to-item mapping, plan handling, conditional child/compaction extensions, and both adapter configurations. |
| Mock provider | `forge-providers/src/mock.rs` and `forge-harness/src/provider/test_support/` | Implement the same restricted contract; fixtures may not bypass completion ownership. |
| Host provider bridge | `forge-harness/src/provider/{bridge,host_invocation,steering,submission,retry}.rs` | Acceptance returned directly to the owner; completion consumed once; host jobs remain collected downstream. |
| Session execution owner | `forge-harness/src/provider/session_execution.rs` and its submodules | Remove redundant native execution-result state while retaining delivery accounting, admission, collection errors, exact ownership, and host commit tickets. |
| Timing and background output | `forge-harness/src/provider/tool_timeout.rs`, `turn/`, `exchange/metrics.rs` | Preserve native timing and explicit item endings; background deadlines and late output keep original ownership. |
| Broker and workflows | `forge-harness/src/broker/{foreground_*,owner_control,configuration,reopening,publication}.rs` | One turn outcome; explicit task/question handoffs; request continuation and host finalization remain separate. |
| Agents and isolated generation | `forge-harness/src/agent/`, `provider/text_generation/isolated.rs` | Consume the new contract for roots, children, and background generation, including failure without normal completion. |
| Host presentation | `forge-harness/src/exchange/`, `timeline/`, `buffer/`, and relevant `nvim/lua/forge/` views | Preserve messages, folds, input entries, approvals, diagnostics, model labels, and timing from canonical host state. |
| Persistence/protocol | `forge-harness/src/storage/`, submission journal, `forge-protocol`, `nvim/lua/forge/protocol_contract.json` | New disposable state version and any required wire version; checked decoding rejects incompatible state. |

Paths in the table are relative to `nvim/rust/forge/crates/` except explicit `nvim/...` paths. Resolve actual module filenames before editing; do not create an empty replacement module merely because a grouped path appears in this table.

### 9.1 Tool watchdog and late output

The current watchdog stops ordinary tool deadlines on early `Execution.Finished`. Delaying that event without moving the associated state update can charge cleanup time to a tool and turn a valid cancellation into a timeout.

Close each foreground item's native execution at its actual native boundary through typed item updates before provider collection. When native end implicitly terminates an unfinished foreground item, the shared item owner emits its explicit interrupted/failed item completion before collection waits; the watchdog consumes that item event. Do not add a second host-facing "native turn ended" callback merely to preserve the old watchdog implementation. Background items remain governed by their own positively established lifetime and deadlines.

Keep native completion timestamps and per-call duration separate from final publication time. After `Turn.Completed`, permit only documented late accounting and identified background-item observations; reject new foreground work under the retired turn. Never kill legitimate independent background terminals merely to make foreground completion easier to implement.

### 9.2 Acceptance and host presentation

The current `SteerCommand::complete` publishes `InputAccepted` back through the provider stream, and the broker then edits its own transcript/wait state. Replace that round trip with a typed acceptance result delivered to the session input owner.

The input owner must preserve all current effects: receipt reconciliation, transcript/workflow-entry insertion, deduplication, clearing the appropriate wait, retraction eligibility, and durable storage. Publish UI state only after its commit. If presentation/storage fails after native acceptance, keep the accepted receipt and expose the host failure; do not resend the already accepted prompt.

### 9.3 Provider unavailability and recoverable warnings

Connection failure must terminate relevant waits without pretending native work was cleanly collected. Warnings must not secretly suspend a turn or authorize replay.

Route `Provider.Unavailable` through the owner that closes admission, reports affected pending operations, and retains collection work. Preserve the original failure plus later cleanup failures without replacing them with a generic error string. Optional inventory or metadata failures use their feature event or warning when required lifecycle observation remains valid.

## 10. Dependency-ordered implementation phases

Each phase has concrete outputs and a verification gate. Temporary internal adapters may help a branch compile, but they cannot remain as a final dual-lifecycle runtime or be used to skip a consumer.

### Phase 0: Capture failing behavior and freeze the contracts

Reproduce the incident at the native boundary before changing its classifier. Freeze the enum inventory, event mappings, ownership records, and failure dispositions as the implementation's review checklist.

Add a Copilot native-peer fixture with accepted input, native call start, abort, `assistant.turn_end`, and aborted idle. Assert that the existing implementation produces the wrong completion, then preserve that failing assertion as the regression. Add the Codex `cancelled`/missing/unknown-status cases and explicit completion-wins-cancellation cases.

Record every public event variant, every publication entry point, and every consumer before deletion. Add an event mapping inventory test or checked data table so a later enum addition cannot escape the migration checklist.

### Phase 1: Domain entities and data structures

Introduce the target enums, checked payload records, identity mappings, and restricted writer types. Update error and operation identities before changing behavior that depends on them.

Implement checked constructors/deserialization, the single Codex status parser, and the private Copilot end-evidence classifier. Retain existing value enums where the inventory says to reuse them. Establish that adapter code cannot directly send final turn outcomes or host events.

### Phase 2: Shared runner and host storage

Move result retention, provider collection, and final publication into one retained shared runner. Adapt the existing session execution owner rather than adding a competing owner.

Keep control admission responsive while native work or cleanup waits. Consume one canonical completed-turn record for input coverage and native outcome; keep host-effect collection, checkpoint work, and host commit tickets downstream. Make provider unavailability resolve affected waits while preserving unfinished cleanup.

Update the internal timing boundary and watchdog before switching public completion timing. This phase also defines fresh-state persistence and the direct acceptance-to-session-owner path.

### Phase 3: Native adapters and every consumer

Convert Codex, Copilot, Claude ACP, Antigravity ACP, and mock adapters through the same contract. For each event row, pair the producer change with its consumer and fixture before declaring the row complete.

Convert transcript/item reducers, agents, isolated generation, task/question completion, settings restarts, approvals, retry presentation, skill attribution, and late background output. Implement the ACP plan and descriptor-update mappings rather than leaving placeholder arms. A compile failure caused by a new variant is a request to implement its behavior, not to add `_ => {}`.

### Phase 4: Compatibility boundary and deployment preparation

Disposable histories and their associated objects are incompatible when their turn/item identities or serialized event contracts change. Select a fresh `storage::state::VERSION`; do not migrate old sessions, transcripts, submission journals, or caches.

The inspected baseline uses state version 10, so this branch should select version 11 if it is still unused at implementation time. If another incompatible change has already claimed that version, choose the next unused version once for the whole contract and route every dependent disposable artifact through it. Update record-format versions that validate the changed records inside that namespace as well.

Preserve `settings.db`, preferences, permission rules, configured model defaults, and read/write directory settings outside that namespace. Migrate a user-authored setting only if its format actually changes, with explicit errors for unsupported values. Bump the host wire version and update the shared Lua/Rust protocol contract if host-visible schemas change.

Old processes must keep their old-version lease and directories. New processes must start fresh state alongside them. Cleanup skips and reports live versions as complete units, deletes stale versions only after ownership checks, and never deletes persistent settings.

### Phase 5: Remove the old API and finalize documentation

Remove public execution/settlement events, old runtime hints, generic completion publication bypasses, host-origin events in the provider union, and compatibility aliases. Delete temporary conversion code after the real producers and consumers pass.

Update the provider, Harness, timeline, child-agent, permissions, host/protocol, and submission chapters to match the delivered contracts. Correct outdated test filters in `verification-by-area.md` rather than perpetuating pre-split `backend::*` names.

## 11. Verification matrix

Verification must exercise native decoding, retained ownership, persistence, and presentation together. Passing a mocked already-normalized cancellation event is not evidence that a native abort was normalized correctly.

| Area | Required cases | Required assertion |
| --- | --- | --- |
| Copilot cancellation | Abort before/after native call end; multiple model calls; aborted idle with no final message; pending send acknowledgement. | One interrupted normalized turn, correct covered inputs, no fabricated success, and no lost replacement. |
| Completion race | Natural completion immediately before/after the interrupt request. | Positive completion remains completed; no replay and no successor is cancelled by an old request. |
| Reconfiguration | Model and reasoning changes during preparation, running work, approvals, and collection; two rapid changes followed by cancel. | Preserve the same exchange; apply latest allowed configuration; continue once or remain cancelled as ordered intent requires. |
| Codex status parsing | Completed, interrupted, cancelled, failed, missing, and unknown statuses. | Transcript/items and returned native outcome share the same classification; unknown never becomes success. |
| ACP v1 | End-turn, cancellation, token/request limits, refusal, invalid stop reason, late acceptance, Claude steering rejection, Antigravity unsupported steering. | No v2 assumptions, no uncertain replay, preserved end reason, and explicit unsupported behavior. |
| Non-normal native stops | Refusal, token limit, and request limit with no assistant text, with partial text/effects, during task execution, and after history reload. | Show the exact reason once, preserve unfinished work, finish required cleanup/checkpoint work, and prevent retry or agent continuation without explicit user authorization. |
| Shared collection | Slow callbacks, delayed reader shutdown, collector error, dropped result waiter, interrupted collector wait. | Completion waits for provider cleanup; ownership survives; host commit is not awaited by provider collection. |
| Failure availability | Lost stream before native result; failure after a known native result; failed publication; failed cleanup. | No normal completion certificate, no hanging wait, retained original evidence and cleanup, fenced connection. |
| Error routing | Copilot session error during an active turn without native turn correlation; Codex error with explicit turn IDs; session error while idle; warning; foreign/stale error; direct operation failure; feature-refresh failure. | Preserve native scope, reject invalid supplied identities, display errors without inventing completion, keep retry evidence separate from reason, and avoid duplicate presentation when a later terminal failure refers to the same error. |
| Host effects | Structured question, plan submission, goal handoff, and completed tool results followed by interruption. | Effects remain committed/available; native interruption is not rewritten as success merely to retain them. |
| Input journal | Write-before-ack crash, accepted-before-UI-failure, accepted input cancelled before native turn start, unknown reconciliation, positive nonconsumption, attachments, duplicate acknowledgement. | Delivery stays truthful; no duplicate prompt or dropped input; owner applies UI updates once; accepted-without-start is not recast as nonconsumption. |
| Item lifecycle | Request before execution; descriptor update; partial arguments; final snapshot after deltas; identical replay; conflicting replay. | Stable identity/type, correct final content, meaningful descriptor handling, explicit conflicts. |
| Accounting | Multiple calls in one normalized turn; delayed usage; duplicate cumulative reports; duration before cleanup; compaction context change. | No double counting; no invented counts; native time excludes cleanup; context occupancy is not token expenditure. |
| Model event ordering | Confirmation known before start, confirmed at startup, delayed, never reported, replayed identically, and genuinely changed for the same turn. | `Turn.Started` never carries a model; all confirmed model updates use `Turn.ModelResolved` after start, target the original turn, and never modify the configured thread default. |
| Background work | Completed foreground with a certified background command; late output and terminal exit; new foreground turn begins meanwhile. | Original item ownership and deadlines remain intact; no reopening or cross-turn attribution. |
| Child agents | Nested child, foreign thread, child identity learned late, parent completes first, child close/failure. | Trusted parent relation, child-local items, no reassignment to parent/current turn, no fabricated child cleanup. |
| Approvals | Command/read/write/network targets; allow/deny; saved rules; user cancel; timeout; writes off; failed grant persistence. | Correct source and per-target decision; read never grants write; policy denials cannot be bypassed. |
| Optional features | Unsupported terminal inventory; refresh failure followed by an unchanged successful snapshot; malformed optional compaction metadata; ACP plan. | Exhaustively handle inventory `Ok`/`Err`, preserve the last snapshot on error, clear the error on the next success, and never substitute an empty successful inventory or ignore a normalized variant. |
| Type boundaries | Attempt to send completion/host events through an adapter writer; pass a `TurnId` where a `ThreadId` is required; add a test-only enum variant. | Compile failure at illegal publication, mismatched ID types, and affected exhaustive consumers. |
| Identity relationships | Supply validly typed IDs for an item in the wrong turn, a turn in the wrong thread, or a thread from a retired binding. | Explicit ownership failure before state mutation; envelope generation and registry relationships remain authoritative. |
| Native ID adaptation | ACP text chunks without native turn/message IDs; repeated chunks; repeated Copilot turn numbers across calls and response cycles; reused tool IDs in different native owners; reattachment with delayed old events. | Allocate normalized IDs once per known object, preserve their original associations, distinguish native call occurrences, reuse recorded IDs for replay, and reject uncorrelatable or retired-generation events. |
| Version isolation | Active old version, fresh new version, stale-version cleanup, persistent settings. | New state works concurrently; live owner is skipped/reported; stale data is removable; settings survive. |
| Editor integration | Empty interrupted turn, partial answer, model switch, pending approval, delayed accounting, unavailable provider. | Correct transcript/status, no permanent "Thought 4s" success-shaped empty answer, and no hidden pending operation. |

### 11.1 Commands and fixtures

Use the configured checks inside Harness. Outside Harness, follow [building and testing](nvim/contributing/forge/building-and-testing.md), run Cargo from `nvim/rust/forge`, and keep the development profile and lockfile arguments consistent.

The initial compile and focused test commands are:

```text
cargo check --profile dev --locked -p forge-provider-api -p forge-providers -p forge-harness --lib
cargo test --profile dev --locked -p forge-provider-api --lib
cargo test --profile dev --locked -p forge-providers --lib copilot::
cargo test --profile dev --locked -p forge-providers --lib codex::
cargo test --profile dev --locked -p forge-providers --lib acp::
cargo test --profile dev --locked -p forge-harness --lib provider::
cargo test --profile dev --locked -p forge-harness --lib service::tests::owner_
cargo test --profile dev --locked -p forge-harness --lib broker::
```

Run the narrowest exact regression first; the broader selectors above are the phase gates for the affected contracts, not instructions to repeat full suites after every edit. Add the needed storage/version and approval filters when those phases change. Compile-fail doctests require the corresponding package's doctest invocation as well as library tests.

Finish source-changing verification with the configured formatter/linter, matching targeted Cargo clippy arguments, and:

```text
cargo build --profile dev --locked --bin forge
```

Run fresh-host editor fixtures such as `harness_prompt_lifecycle`, `harness_configuration_host`, and the submission host fixture documented in `provider-submissions.md`. Follow `editor-tests.md` for their actual runner arguments. Do not substitute a passing test binary for the executable Neovim launches. Check Lua with the repository's required LuaLS configuration if Lua schemas or consumers change.

Live-provider requests require explicit authorization. Native socket/SDK/ACP mock peers should cover the event orders above without billable requests. No new benchmark is required unless the refactor introduces a new buffering or scheduling mechanism; still record that streaming remains incremental and cleanup delay does not inflate reported native duration.

## 12. Rollout, rollback, and completion criteria

This personal infrastructure does not need a dual-runtime compatibility mode or a feature flag that keeps both completion contracts alive. Build the complete change, select a fresh disposable-state version, and start a new host while existing hosts keep their pinned executable and state leases.

Roll back the executable/source revision if the new host misattributes items, loses accepted inputs, cannot finish cancellation, or reports success without the required collection evidence. A previous executable must use its own compatible state version; do not migrate new disposable sessions backward. Preserve user settings and explicitly review any settings-format rollback separately.

The work is complete only when:

1. Every old event variant has an implemented, tested disposition in the before/after tables.
2. Every new enum variant has an explicit owning consumer, native or host producer mapping, and conformance coverage.
   Every newly preserved distinction also satisfies section 8.4's required behavior; serialization and storage alone are not sufficient.
3. Native turn completion and cleanup cannot publish independently chosen outcomes.
4. Adapter writers cannot publish final turn or host-policy events through a generic bypass.
5. Codex, Copilot, Claude ACP, Antigravity ACP, and mock implementations satisfy the same public contracts without pretending their native protocols are identical.
6. The original settings-change regression and the additional status/ordering cases fail before the fix and pass afterward.
7. Accepted inputs, partial messages, completed tool actions, child ownership, permissions, timing, and background lifetimes survive the transition.
8. Fresh-state coexistence, stale cleanup, persistent settings, current documentation, and a rebuilt executable are verified.

Remaining uncertainty belongs to specific native mappings and must be resolved by schema inspection and fixtures before that phase is signed off. It must never be converted into an ignored enum variant, a synthetic success, or permission to replay an uncertain request.
