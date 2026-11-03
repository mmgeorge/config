# Provider boundary consolidation plan

The current provider split leaves Harness responsible for native execution completion even though it already receives normalized text and tool events. This plan completes that boundary and repairs the Background generation failure documented below, using [provider-leak.md](provider-leak.md) as the source inventory.

Implementation is deferred. This document describes the intended work, not a completed fix; the reported commit-generation failure remains unresolved.

**Controlling requirement: normalization is complete before anything leaves `forge-providers`.** Every outward event, operation result, error, callback request, capability, and descriptor must already have provider-neutral meaning. Consumers must not finish decoding, correlate native events, interpret native status or configuration fields, or reconstruct completion. Hiding provider names behind generic types or raw JSON does not satisfy this requirement.

## Target contract and scope

Persistent sessions expose one normalized event stream and operations for initialization, prompt submission, steering, cancellation, and shutdown. An isolated `generate_text` operation is separate: it submits once, waits, and returns a string or a specific error.

Background must retain its own session, history, timeline, and event stream. Commit and PR generation currently use that Background conversation and must not be rerouted through isolated `generate_text` merely to avoid the failing lifecycle code.

| Consumer | Required interface | Native responsibility |
| --- | --- | --- |
| Interactive Harness and child sessions | They submit input and consume normalized session events. | The provider correlates requests and emits output, execution completion, cancellation, or failure. |
| Background conversation | It owns a separate persistent session and consumes the same normalized lifecycle contract. | The provider handles native session resumption, stream lifetime, and completion without requiring host certificates. |
| Isolated text-generation caller | It awaits `generate_text(request) -> Result<String, ProviderError>`. | The provider owns temporary resources, native waiting, output validation, and cleanup. |

Prompt-submission success means acceptance, not completion. An asynchronous failure after acceptance belongs on the stream; an immediate submission failure belongs in the operation's return value. The provider must distinguish rejected input from uncertain delivery, so an unavailable acknowledgment never authorizes automatic prompt replay.

All provider-specific implementation nouns and behavior belong inside `forge-providers`. Other source code must use normalized identities, registry-supplied descriptors, capabilities, requests, errors, and callbacks rather than concrete provider variants or native configuration formats.

## Specific failure: successful output rejected by certificate bookkeeping

At approximately 14:34:54 local time on 2026-11-02, the affected Background trace recorded `assistant.message`, `assistant.turn_end`, and `session.idle`. Forge then reported `adapter did not certify its terminal coverage` instead of returning the generated commit message.

The observed data and source establish the failure mechanism below. They do not prove which internal certification expression failed in that live process, because the retained trace removes native event identities and the caller discards certification errors.

1. The native persisted history links `assistant.turn_start` to a durable `session.usage_record`, then to `assistant.message` and `assistant.turn_end`. The live events recorded by Forge contain no `session.usage_record`.
2. [`CopilotTurnEvent::certified_submissions` and `certified_executions`](nvim/rust/forge/crates/forge-providers/src/copilot/turn.rs) traverse `event_parents` through `chain_contains`. They require ancestry links that an otherwise valid live notification stream may omit.
3. [`CopilotProvider::run_native`](nvim/rust/forge/crates/forge-harness/src/provider/copilot/mod.rs) calls the native prompt operation, then separately calls `foreground_anchor` and `certified_executions`. Its `if let` accepts only successful certificate inputs; errors or absent anchors silently skip publication.
4. [`SessionEvents::start`](nvim/rust/forge/crates/forge-harness/src/provider/session_events.rs) sees an adapter success without a certificate and converts it into an observation failure.
5. Foreground handling, session draining, and Background reporting wrap the same failure repeatedly. The repeated text does not establish multiple independent native failures.

This is not a requirement imposed by the Copilot SDK. The certificate is Forge's additional completion machinery, and the complete-history assumption is stronger than the function-and-stream contract requires.

### Concrete repair for this incident

The repair must make a correctly correlated native completion sufficient to emit the normalized execution outcome. It must not require unrelated durable history records to appear in the notification stream, and it must not treat the mere presence of text as success.

The following changes belong together; removing the check while leaving request correlation undefined would trade this failure for premature completion.

| Location | Required change | Preserved constraint |
| --- | --- | --- |
| `forge-providers/src/copilot/turn.rs` | Replace full-parent-chain completion certification with native request/event correlation owned by the session execution state. Correlate accepted message IDs, root-envelope input observations, execution boundaries, and ordered terminal signals. | A child event, an old idle, or a reused native turn number cannot finish unrelated accepted input. |
| `forge-providers/src/copilot/prompt.rs` | Translate the native execution into normalized `ExecutionUpdate::Started`, `Finished`, or `ObservationLost` events. Emit exactly the appropriate terminal outcome, with normalized submission correlation, through the session stream. | The native operation cannot return success while leaving its required terminal outcome to be reconstructed by Harness. |
| `forge-providers/src/copilot/intake.rs` | Keep one ordered stream owner through foreground completion, idle periods, cancellation, and resumption. Retain the existing independently drained, unbounded observation queue. | Dropping a prompt waiter cannot silently discard native observations or turn orderly shutdown into transport failure. |
| `forge-harness/src/provider/copilot/mod.rs` | Remove the post-return calls to native anchor/certification helpers and the assembly of `NativeExecutionCertificate`/`ExecutionCertificate`. Retain only host callback adaptation until that adaptation is generalized. | Native protocol state does not become a second completion channel beside the event stream. |
| `forge-harness/src/provider/session_events.rs` | Consume normalized lifecycle events rather than fabricate native success from `ProviderCompletion`. Remove provider-specific certificate expectations when each producer has migrated. | Host effects must be available before a terminal result lets the broker settle; a relay may preserve ordering but must not reinterpret native completion. |
| `forge-harness/src/background/manager.rs` and broker integration | Continue using the retained Background session and normalized stream. Preserve history rotation, cancellation, failure notification, draft preservation, and cleanup ownership. | Do not bypass the incident by switching Background to isolated generation or by disabling observation failures. |

Define the exact request-to-execution mapping against the installed SDK/CLI protocol before implementing the replacement. Include events arriving before acknowledgment, queued input, concurrent steering, multiple native turns for one invocation, repeated native turn IDs across prompts, and root events carrying `parentAgentTaskId`.

The root/child distinction must use actual envelope ownership, not `parentAgentTaskId` alone. Turn IDs cannot be globally unique keys: scope them to the session and execution occurrence, retaining enough accepted-input ordering to distinguish a later prompt that reuses `"0"`.

Do not add unconditional history reads to satisfy the old certificate algorithm. Native history reads remain appropriate for explicit history operations, recovery, or a documented protocol case where request correlation genuinely cannot be established from supported signals; such a case needs its own native-owned behavior and regression, not a generic ancestry repair on every completion.

### Regression that must fail before the repair

Use the real SDK transport against a local JSON-RPC mock, the native implementation, and the shared Harness execution consumer. The fixture must reproduce the incomplete live ancestry without making external model requests.

The mock should accept a send with a message ID, publish a matching root `user.message` containing non-null `parentAgentTaskId`, publish a root `assistant.turn_start`, omit the durable `session.usage_record`, then publish an answer whose `parentId` names that omitted event. It then publishes `assistant.turn_end` and ephemeral `session.idle`.

The expected result is one completed execution and the full answer, without a completion-certificate error, a terminal-history reconstruction request, or a cancellation request caused solely by the omitted bookkeeping event. Run the fixture in both Chat and Background, submit another prompt reusing native turn ID `"0"`, and verify that the retained session still works.

An exploratory fixture already reproduced the current failure: after receiving the answer and idle, Forge requested `session.abort` rather than completing normally. An attempted history-reconciliation implementation did not reach a verified passing state and was withdrawn when this work was deferred. Recreate the regression with the contract above rather than retaining a test that requires the abandoned history-reading approach.

## Boundaries and ownership

The inventory includes more than the incident's completion code. The eventual consolidation must remove concrete native objects, raw protocol helper APIs, native resource encoders, and provider-name registries from their current external locations.

| Boundary | Files and objects | Intended owner |
| --- | --- | --- |
| Shared contract | `forge-provider-api/src/{provider.rs,lifecycle.rs,model.rs,generation.rs,events.rs}` | The API crate defines provider-neutral request, event, result, identity, and host callback contracts. |
| Native implementations | `forge-providers/src/{copilot,codex,acp}/` and `lib.rs` | Private native owners implement operations, protocol decoding, lifecycle publication, catalogs, resources, and cleanup. |
| Provider construction | Harness `provider/contract.rs::{build,registered_capability}` and API `ProviderKind` | The native crate's registry owns concrete names, aliases, defaults, and implementation selection; callers receive opaque normalized identity and descriptors. |
| Host integration | Harness `provider/`, `permissions/`, `tools/`, `settings/`, `broker/`, and `background/` | Harness owns policy, canonical tools, task authority, persistence, resource admission, and consumers of normalized events. |
| Native resource and input encoding | Harness provider `attachments.rs`, `profile.rs`, Codex `resources.rs`, settings `SessionResources::acp`, and question-reply rendering | Native encoders belong in the provider crate; canonical input, answers, credential resolution, and resource leases remain host responsibilities. |
| Editor selection and defaults | `nvim/lua/forge/harness/providers/`, `infra/config.lua`, and `views/harness/subagent_selection.lua` | Lua consumes descriptor data and opaque IDs instead of defining concrete provider sets and launch defaults. |
| Validation fixtures | Provider-specific Harness fixtures, native live tests, and Lua host fixtures listed in the inventory | Native protocol tests live with implementations; host tests exercise normalized contracts without importing native objects. |

Do not move Harness storage, permission policy, plan/task decisions, or canonical tool implementations into `forge-providers` to make imports disappear. Use the existing normalized host interfaces where possible and split mixed owners where necessary.

## Normalized contract specification

The following specifies the structs, enums, operation contracts, and transition logic needed for the cutover. Names marked **proposed** describe required replacements, not types that already exist; reuse and tighten existing normalized types rather than create parallel event hierarchies.

The current definitions are in [`event.rs`](nvim/rust/forge/crates/forge-provider-api/src/event.rs), [`lifecycle.rs`](nvim/rust/forge/crates/forge-provider-api/src/lifecycle.rs), [`execution.rs`](nvim/rust/forge/crates/forge-provider-api/src/execution.rs), and [`provider.rs`](nvim/rust/forge/crates/forge-provider-api/src/provider.rs). Their existence does not establish a normalized boundary: the native crate must produce their complete meanings without subsequent interpretation in Harness.

### Identity, requests, and native ownership

Identity must describe ownership without teaching consumers how a particular CLI names a session or turn. Constructors and deserialization must enforce the same validation, and identifiers must remain opaque to host code.

| Struct or enum | Required fields or cases | Required logic and owner |
| --- | --- | --- |
| **Proposed `ProviderId`**, replacing concrete `ProviderKind` variants | It stores a private, nonempty string and supports equality, serialization, and explicit display. | The provider registry resolves IDs to implementations and owns names/aliases. Consumers persist IDs or compare equality, never parse or branch on vendor spellings. Unknown IDs fail registry resolution explicitly. |
| Existing `ProviderDescriptor` | Its `kind` becomes the normalized provider identity; `label` and `capability` remain provider-produced data. | The native registry supplies descriptors and launch defaults. UI and settings consume the descriptor rather than maintain another provider list. |
| Existing `ExecutionOwner`, `InvocationScope`, and `EventScope` | Retain session/agent ownership, exchange/invocation identities, and the distinction between invocation events and session-operation events. | Harness creates workflow scopes; providers attach them unchanged to corresponding observations. A recovered runtime gets fresh invocation identity; persisted conversation identity may remain unchanged. |
| Existing `SubmissionId` | It identifies one host input journal record and rejects an empty value. | Harness creates it before dispatch; providers echo it in normalized acceptance/completion. It is not replaced by an SDK request or message ID. |
| Existing `TurnAddress`, `MessageAddress`, and `ToolAddress` | Retain scoped turn, message, and tool identity; native strings are opaque values only. | Providers resolve native identities and allocate a distinct normalized occurrence when a native turn number repeats. Host code must not inspect native ancestry or construct these addresses from raw responses. |
| **Proposed `SessionRequest`** | It carries `ExecutionOwner`, workspace, optional `ProviderSessionId`, typed model/access settings, normalized resources, and advertised host-tool definitions. | It replaces per-provider creation options at the public boundary. The native session owner handles create/resume/reconfiguration and retains resources through cleanup. |
| **Proposed `PromptRequest`** | It carries `InvocationScope`, `SubmissionId`, `PromptInput`, admitted attachments, prepared instructions, and an `InputReceipt` capability. | It replaces the native dependency on Harness's `ProviderRequest`/`ControlTurnContext`. Plan stores, task controllers, and `CompletionSlot` never cross into the native crate. |
| **Proposed `SteeringRequest`** | It carries the original owner/scope, a new `SubmissionId`, and normalized input. | Providers decide injection versus queued execution and report the resulting correlation. Consumers do not choose a native turn ID or delivery-mode spelling. |
| Existing `TextGenerationRequest` and `ReasoningSelection` | Retain `model`, `reasoning`, `instructions`, and `input`; reasoning remains `Default`, `Lowest`, or `Level`. | The isolated operation resolves model-native choices, waits for output, validates it, and collects temporary resources. It has no persistent-session completion slot. |

Use the existing provider-neutral callback traits for tool execution, permission decisions, receipts, and tracing. Replace `ProviderHost` associated types that expose Harness workflow requests/results with normalized boundary types; host-specific values stay captured by host callback implementations.

The public provider/session interface must offer session initialization, stream subscription, prompt submission, steering, cancellation, recovery, catalogs/configuration, and shutdown using these normalized contracts. The returned implementation is opaque: no `client()`, `state()`, native `request(method, Value)`, decoder access, or mutable native maps.

### Normalized event enum

Retain an exhaustive typed event enum rather than a native method string with an optional JSON payload. The table covers every current `ProviderEvent` variant and states what its consumer may infer.

| Current variant | Typed payload and required normalization | Consumer rule |
| --- | --- | --- |
| `Execution` | `ExecutionObservation` contains the validated invocation scope and an `ExecutionUpdate` described below. | This is the authoritative asynchronous execution lifecycle, not a hint that triggers native certification. |
| `Message` | `MessageAddress` and `MessageUpdate::{Started, Delta, Completed, Classified, Interrupted}` preserve one message identity. `MessageRole::{Thought, ThoughtSummary, Commentary, Response}` has provider-neutral meaning. | Append deltas; replace text with a completed snapshot. Missing/unknown native phases remain `Unclassified`; consumers never infer phases from native names. |
| `Tool` | `ToolAddress`, `ToolInvocation`, and `ToolUpdate` describe request, title/input updates, start, background continuation, output, changes, skill reads, and terminal outcome. | Use normalized `ToolSemantics`, `ToolActivityKind`, and `ToolOutcome`; raw arguments are inspection content, not authority to reclassify native operations. |
| `Turn` | `TurnAddress` and `TurnUpdate::{Started, CallFinished, Summary, Finished}` report a normalized model-turn boundary and timing. | A turn ending does not independently complete the whole accepted execution or its background tools. |
| `Usage` | `UsageUpdate` normalizes token categories, call identity, context-only adjustments, and billing measurements. | Deduplicate normalized call identity; do not decode native counters or pricing fields. |
| `Runtime` | `RuntimeUpdate::{Resolved, ContextUsage, Progress, ForegroundIdle}` supplies normalized model resolution, context, byte progress, and foreground-idle status. | `ForegroundIdle` is status, not permission to synthesize execution completion. |
| `Compaction` | `EventScope` and `Compaction` contain operation identity, lifecycle state, summary, errors, and normalized accounting. | Manual and automatic compaction retain their own operation scope; they do not borrow a completed prompt's scope. |
| `Agent` | `AgentUpdate::{Lifecycle, ConversationResolved}` carries normalized child ownership/status and opaque conversation identity. | Do not parse native subagent messages or infer ancestry from a provider-specific field. |
| `Checklist` | `ProviderChecklistUpdate` contains typed statuses and scoped entries. | Render normalized items; do not inspect native todo-table or RPC formats. |
| `Approval` | `ApprovalUpdate::{Requested, Resolved, Cancelled}` is generated by the host permission workflow from a normalized permission callback. | Preserve host provenance; never forward native approval request/response envelopes as this payload. |
| `DirectoryAccess` | Typed directory grants come from host policy. | Native adapters cannot grant host access by decoding an event; only the permission owner publishes these grants. |
| `InputAccepted` | Replace presentation-only native receipt dependence with an **accepted submission identity**. Add a normalized `Accepted { submission_id: SubmissionId }` case or an equivalent dedicated typed acceptance observation; retain task/steering display data as host presentation. | Distinguish acceptance from completion. If the operation return and stream both report acceptance, they refer to the same journal identity and are idempotent. |
| `Retry` | A typed retry update reports retry state and normalized classification. | Consumers must not parse native codes to decide retry safety; use the adapter's normalized failure category and host retry policy. |
| `Diagnostic` | `ProviderDiagnostic` supplies scope, diagnostic code, text, and severity/fatality. | Diagnostic text is display-only, not an alternate lifecycle or machine-readable native error channel. |
| `ParentBoundary` | `ParentBoundaryUpdate::{WaitStarted, WaitUpdated, WaitFinished}` reports normalized coordination status and counts. | Do not reconstruct parent/child completion by recognizing native `wait` calls. |

Host-originated approval, directory-grant, and task-presentation events may continue to share the host-facing event transport. They must be composed by the host from normalized callbacks, not become reasons for the provider crate to depend on Harness authority or storage.

`ToolUpdate::Finished` must use `ToolOutcome::{Succeeded, Failed, Denied, Cancelled, Interrupted}`. `TextUpdate::{Append, Replace}` must distinguish cumulative output from deltas, and late tool output must retain its original address without reopening a settled tool. Move native status spelling recognition out of shared `ToolStatus::parse` consumers into native adapters; a generic display serializer is not a native parser.

### Execution state and event transitions

Reuse `ExecutionUpdate` and `ExecutionResult` with explicit ownership semantics. No successful operation return, text message, `TurnUpdate::Finished`, or idle-status event substitutes for a required normalized execution terminal event.

| Enum case | Required contents | Producer obligation |
| --- | --- | --- |
| `ExecutionUpdate::Started` | It includes the normalized `TurnAddress` for the execution occurrence. | Emit once for the correlated execution, before its terminal update; buffer early native observations until their input correlation is known. |
| `ExecutionUpdate::Finished` | It includes the address, covered `SubmissionId` list, completion time, and `ExecutionResult`. | Account only for inputs assigned to this execution. Native send IDs, turn-number reuse, and stream ordering are resolved before publication. |
| `ExecutionUpdate::ResolvedWithoutExecution` | It includes covered submission IDs, completion time, and result, with no fabricated turn address. | Use only for a native operation that actually resolves without starting model work; never to disguise missing start observations. |
| `ExecutionUpdate::ObservationLost` | It includes `ObservationFailure` and any valid partial completion snapshot. | Report disconnect, protocol/ownership failure, or uncertain delivery without claiming normal completion or user cancellation. Fence further native work until recovery. |
| `ExecutionResult::Completed` | It includes the normalized `CompletionSnapshot`. | Native execution finished normally; required host callback work has settled before the host may commit its effects. |
| `ExecutionResult::Cancelled` | It includes the partial completion snapshot. | Native cancellation has settled; sending an abort request or dropping a waiter alone is not this outcome. |
| `ExecutionResult::Failed` | It includes the snapshot and `ExecutionFailure::{Rejected, Terminal, Retryable}`. | Preserve the actual failure and normalized retry eligibility; do not replace it with a missing-certificate error. |

The native owner needs private accepted-input and execution records, not public certificates. Each accepted-input record binds a `SubmissionId` to the native receipt and its execution occurrence; each execution record retains its normalized address, assigned inputs, lifecycle, and output accounting. The native types and wire fields stay private even if these records have provider-neutral internal names.

| Private state model, proposed role | Required enum cases | Transition rule |
| --- | --- | --- |
| Input delivery state | `Prepared`, `Sending`, `Accepted { receipt }`, `Rejected { reason }`, and `Uncertain { failure }` each hold only their applicable fields. | Commit `Sending` before dispatch. Only native acknowledgment establishes `Accepted`; only a definite rejection establishes `Rejected`. A lost acknowledgment establishes uncertainty, not rejection. |
| Execution lifecycle state | `Running`, `Finishing`, `Terminal { result }`, and `ObservationLost { failure }` distinguish live work, callback collection, completed native outcome, and failed observation. | Enter `Finishing` on a correlated native terminal signal, drain required host work, then publish the terminal result once. Failed observation cannot transition to success without an explicit new recovered execution. |
| Session lifetime state | `Open`, `Closing`, `Closed`, and `CleanupFailed { failure }` fence admission and retain cleanup ownership. | Reject new submissions after closing begins. A failed cleanup retains the native resources required for an explicit retry; it cannot report closed or admit replacement work prematurely. |

These are private implementation states, not extra host-visible events or a prescribed global lock. A provider may preserve an existing equivalent enum instead of introducing another one, but it must not encode mutually exclusive outcomes as unrelated booleans or make callers sequence these transitions.

The lifecycle is admission, dispatch, acceptance or explicit rejection/uncertainty, execution start, ordered updates, and one terminal outcome. Some protocols report acceptance only with terminal return, and some events arrive before the send acknowledgment; adapters must buffer/correlate these cases rather than require every native protocol to deliver the same order.

When steering joins a running execution, its accepted submission can be included only in the terminal update that actually covers it. An old idle cannot cover later input. Repeated turn numbers and root/child activity require native-owned correlation; the host checks only normalized scope, known submission identity, and valid state transitions.

After terminal publication, retained background tools may still emit normalized activity on their original addresses. Keep their lifetime distinct from the completed foreground execution, and close the session stream only when the native session owner has collected its readers and processes.

### Errors, callbacks, and host-effect ordering

Keep error categories typed across both function returns and stream failures. Human-readable diagnostic context is useful, but a consumer must never parse that text to decide whether input was accepted, execution finished, or retry is safe.

| Boundary type | Required contract |
| --- | --- |
| `ProviderError` | Preserve typed unsupported-operation, initialization/version, missing-session, rejected/not-consumed, uncertain-delivery, transport/protocol, cancellation, and cleanup outcomes. Convert SDK errors inside native implementations; do not return SDK error enums or native JSON. |
| `ObservationFailure` | Preserve `LifecycleUnavailable`, `Overflow`, `Disconnected`, `DeliveryUncertain`, `Protocol`, and `OwnershipConflict` meanings. A missing unrelated persisted ancestry event is not itself lost observation. |
| `InputReceipt` | Keep journal transitions host-owned and provider-invoked before dispatch and on acceptance/rejection. Replace publicly inspectable `SubmissionTarget::Native { format, checkpoint: Value, ... }` with an opaque recovery token whose encoding and exact-version validation belong to `forge-providers`; host storage persists it without parsing. |
| `ProviderTools`, `ToolCall`, and `ControlToolDefinition` | Host callbacks receive a normalized tool invocation with canonical tool name and application arguments. Native wrappers, SDK tool objects, invocation envelopes, and response encoding remain private. |
| `ProviderPermissions` / `ScopedPermissions` | Callbacks receive typed `PermissionRequest`/`PermissionTarget` and normalized ownership. Responses remain `PermissionResponse::{Allow, Deny, Cancelled}`; native approval mapping and stale native proposal checks stay inside the adapter. |
| `TurnControl` / `HostTurnStatus` | Keep host effect draining, cancellation authority, and host retry/handoff facts separate from native session state. `before_send` cannot require a host callback to mutate a concrete native registry. |
| `ProviderTrace` | Native producers may record redacted diagnostic metadata through the host sink; neither trace content nor native diagnostic codes are a public completion protocol. |

The native execution must await the host callback/effect barrier before terminal delivery can settle the broker. Keep `CompletionSlot` only for host effects if still needed; remove its `certificate` field and the `ExecutionCertificate`/`NativeExecutionCertificate` types. If a generic event relay is needed to order host publication, it forwards the already-normalized native outcome unchanged and reports relay failures without manufacturing native success.

### Resource, setup, and lifecycle operation results

Use typed values for every optional provider operation as well as the prompt stream. Returning a generic JSON blob and documenting its provider-specific fields merely moves decoding into the next crate.

| Contract | Required representation and logic |
| --- | --- |
| Model, skill, MCP, terminal, and capability results | Retain `ProviderModel`, `SkillDefinition`, `McpDefinition`, `TerminalSnapshot`, and `CapabilityManifest`; adapters normalize ordering, status, limitations, and settings before returning them. Model/setting IDs are opaque selections, not names consumers interpret. |
| **Proposed `ProviderSetup`** | It contains typed installation availability, version information, and authentication choices. Replace `Provider::setup -> Value`; registry/native code resolves native handshake fields and terminal authentication commands. |
| **Proposed `AuthenticationMethod`** | It contains opaque method identity, label/description, and a typed interaction enum such as provider-managed versus terminal launch. Resolve the advertised method inside the provider, not by inspecting ACP response JSON in Harness. |
| **Proposed `ResourceSelection` / `McpLaunch`** | Selection contains canonical skills and MCP definitions; launch uses `Stdio { command, arguments, environment }`, `Http { url, headers }`, or `Sse { url, headers }` with typed optional requirements for tool selection, timeout, and working directory. Credentials remain ephemeral; unsupported requirements fail at the native capability boundary, not a shared ACP-specific import check. |
| Attachments and question answers | Retain admitted `FileAttachment`/`MessageInput` where sufficient and add typed MIME/content metadata only where required. Transfer structured question answers through normalized input; native adapters create image items, base64 fields, file references, and question-reply envelopes. |
| Cancellation, recovery, and shutdown | Each operation returns success only after its owned native transition/collection completes, or a typed failure. Native code owns admission fencing, abort repetition for raced sends, observer collection, client stop, and binding retirement. Host code never chooses `ClientShutdown` or calls a sequence of native cleanup helpers. |

These are contracts, not a requirement to add a new public struct for every internal record. Prefer tightening an existing shared type to introducing a duplicate wrapper, and keep native implementation modules private regardless of whether their internal names contain a provider noun.

## Dependency-ordered implementation

Each phase has a concrete completion condition and depends on the preceding contract changes. This ordering is for implementation and verification, not permission to ship partially migrated completion behavior.

### Phase 1: normalized operation and lifecycle contracts

Start with `forge-provider-api`, because both native producers and host consumers must agree on one lifecycle. Reuse its existing `ExecutionObservation`, `ExecutionUpdate`, `ExecutionResult`, `SubmissionId`, and scoped addresses where their semantics fit; do not introduce a second certificate-shaped result channel.

1. Specify acceptance, execution start, output, completion, native failure, cancellation, and lost observation for persistent sessions.
2. Specify how accepted steering maps to terminal coverage without exposing native event IDs or parent chains for callers to interpret.
3. Specify ordering between normalized terminal delivery and host callback/effect completion. Retain generic ownership and duplicate-terminal checks, not provider-specific certification.
4. Keep isolated `generate_text` as an awaited value/error operation with provider-owned cleanup; do not make it depend on persistent session bookkeeping.
5. Replace shared concrete provider enum knowledge with normalized identity plus a registry owned by `forge-providers`. Preserve opaque serialized values where possible rather than introducing compatibility aliases throughout callers.

Exit condition: contract tests can express all persistent and one-shot outcomes without native types, native method strings, or provider-specific terminal modes.

### Phase 2: native stream owners and incident repair

Implement Copilot first because the current incident supplies a concrete failing fixture. Apply the same normalized contract to Codex and ACP rather than leaving one provider on a separate host-certification path.

1. Implement the Copilot incident repair described above and keep acceptance, event normalization, interruption, recovery, and terminal publication in its native owner.
2. Move Codex's raw request/response routing, `CodexTurnCoordinator`, native goal/descendant settlement, background readers, approval-response encoding, and native thread configuration into the provider crate. Keep canonical host tool execution behind callbacks.
3. Move ACP prompt and compaction result interpretation, `stopReason` handling, setup/authentication handshake parsing, session begin/finish transitions, and steering limitations into its native owner.
4. Consolidate each provider's runtime/session shutdown and recovery sequence. Host code must not order `close_observers`, native stop, and session clearing or choose `ClientShutdown`.
5. Make the native session owner retain stream tasks until explicit retirement; join owned work, surface cleanup failures, and never replay uncertain input automatically.

Exit condition: all native implementations publish authoritative normalized execution outcomes, including asynchronous errors, and no implementation requires Harness to construct its terminal event.

### Phase 3: host consumers and separate one-shot generation

Cut over Harness only after native producers implement the agreed contract. Preserve Background's persistent conversation and the editor's existing draft behavior.

1. Remove Harness's concrete `CopilotProvider`, `CodexProvider`, and `AcpProvider` orchestration as their native behavior moves; replace construction with the normalized registry boundary and host callbacks.
2. Remove `ExecutionCertificate`, `NativeExecutionCertificate`, and provider-specific `TerminalBoundary` rules after their consumers migrate. Keep any generic host-effect synchronization required to avoid settling before tools and persistence finish.
3. Update root, child, and Background execution consumers to consume the same stream semantics. A host timeout, native failure, and user cancellation remain distinct outcomes.
4. Verify isolated generation through the existing `HarnessService::generate_text` and native implementations, without rerouting `GenerationService`'s Background requests.
5. For Copilot isolated generation, test SDK `session.sendAndWait` success/error handling, returned assistant content, optional completion-event waiting, and abort/detach/delete/client-stop ordering. No parent-chain certificate belongs in this operation.
6. Preserve both the primary failure and cleanup failure when both occur. Avoid repeated formatting of the same error at each foreground/drain/Background boundary.

Exit condition: Background completes the incident fixture through its retained stream, while isolated generation returns text/error without creating a Background conversation or consulting execution certificates.

### Phase 4: resources, catalogs, identities, and editor callers

Complete the remaining inventory rather than declaring the boundary finished after moving lifecycle code. Native serialization and native configuration recipes must not survive under provider-neutral wrapper names in Harness.

1. Move native attachment formats, MCP payload conversion, provider profile layouts, native tool exclusions, and native question-reply encoding into private implementations.
2. Keep canonical attachments, structured question answers, resource selection, credential resolution, and lease lifetimes host-owned through typed contracts.
3. Supply model ordering, capability limitations, launch defaults, names, aliases, Rulesync targets, and display metadata from the native registry.
4. Replace Lua's hardcoded provider lists/descriptors with registry data. Preserve user-selected provider settings through opaque identities.
5. Inspect persistence and RPC schemas before changing identity types. Preserve current wire/storage representations where valid; if a private format must change, follow the repository's exact-version policy instead of adding migrations or silent fallbacks.

Exit condition: consumer code neither names a concrete provider implementation nor produces or parses its wire/configuration format.

### Phase 5: close exports and relocate fixtures

Make the native module tree private after the last external consumer has migrated. This is the compile-time enforcement of the boundary, not a cosmetic cleanup.

1. Remove public `acp`, `codex`, and `copilot` implementation namespaces and public SDK-bearing helper signatures listed in the inventory.
2. Expose only normalized registry/factory operations and shared contracts. Do not preserve old access through public re-exports, inferred SDK parameters, raw JSON request methods, or `Deref` wrappers.
3. Move provider-wire unit/integration fixtures into the provider crate. Keep host workflow and editor integration coverage through normalized mocks or provider-owned test drivers.
4. Replace incidental concrete provider labels in generic tests with neutral fixture identities. Keep real provider prerequisites and wire recordings with their owning implementation.
5. Update the provider architecture and contribution documents to describe the actual owner, not the intended owner.

Exit condition: the agreed provider-noun search is clean outside `forge-providers` source, and external code cannot compile against native implementation objects.

## Verification matrix

The tests must exercise the boundaries being changed rather than merely assert renamed types. Run exact cases first, combine related selectors, and broaden only for demonstrated cross-module risk.

| Boundary | Required regression coverage | Acceptance condition |
| --- | --- | --- |
| Copilot incident | A durable usage event exists in history but not notifications; answer/turn-end/idle arrive normally. | Background and Chat return the full answer, with no ancestry-related failure or repair read. |
| Input correlation | Events precede send acknowledgment; root input has task metadata; native turn IDs repeat; child idle and stale idle occur; steering races with completion. | Only the intended accepted input completes, once, and the next prompt remains usable. |
| Native failures | Immediate rejection, uncertain send, explicit asynchronous error, disconnect before completion, cancellation during send, and delayed acknowledgment occur. | Return/stream failures preserve their category; no fabricated success or automatic uncertain-input replay occurs. |
| Host publication | Native terminal arrives before host callback effects settle; stream activity continues after foreground return. | No broker settlement precedes required host effects, no duplicate terminal is synthesized, and late observations retain their owner. |
| Persistent Background | Two generation requests share native history; branch/model changes rotate the conversation; a failed request is followed by explicit retry. | Timeline, history, settings, cleanup fencing, and draft preservation remain intact. |
| Isolated generation | Success without complete ancestry notifications; native error; empty/malformed output; output validation failure; cleanup failure. | One call returns a valid string or specific error, and its temporary native resources are collected. |
| Codex lifecycle | Foreground/descendant overlap, native goal continuation, interrupt acknowledgment versus terminal completion, and background commands. | Normalized terminal events preserve existing ownership and cleanup behavior without Harness parsing native messages. |
| ACP lifecycle | Prompt-return completion, steering outcomes, compaction, cancellation, and protocol errors. | Native code supplies normalized outcomes; shared code has no `AcpPrompt` terminal exception. |
| Surface isolation | Public API review, compiler checks, concrete-name searches, and SDK dependency searches. | No native implementation type, raw protocol entry point, or provider-specific consumer branch remains outside the native crate. |

Retain the existing targeted regressions such as `copilot_burst_before_acknowledgment_preserves_answer_and_next_prompt` and `copilot_disconnect_collects_execution_and_resumes_for_next_prompt`, relocating them with the implementation as needed. Add the incident regression first and record its failure before changing production behavior.

Run Cargo from `nvim/rust/forge` with the pinned toolchain and `--profile dev --locked`. Follow check, targeted tests, formatting check, Clippy, and final `cargo build --profile dev --locked --bin forge`; enforce the repository's build deadline and keep Cargo-heavy jobs serial. Run a fresh mock `background_host.lua` fixture after the build, along with only the editor fixtures affected by the registry/UI changes. Live model requests require explicit authorization.

The lexical gate should inspect maintained Rust/Lua source and fixtures, including comments and declarations, excluding the native crate itself. Report matches in manifests, generated lockfiles, migration inventories, documentation, and user-supplied configuration separately; their unavoidable dependency names are not evidence that runtime code owns native behavior.

### Mandatory provider-leak search gate

Run the following searches from the repository root after each provider cutover and again before declaring consolidation complete. The final gate requires zero provider-name matches in maintained source outside `forge-providers`, including tests, comments, imports, enum variants, labels, and default commands.

**Mandatory instruction to the implementing LLM: execute the source-content grep command below, inspect its complete output and exit status, and report the result before claiming this task is complete. Do not merely quote the command, substitute a spot check, or infer cleanliness from compilation. Any remaining source match blocks completion until resolved. Then run the path, native-protocol, public-surface, and outward-contract checks in this section.**

**1. Search source contents outside the native crate.** Use case-insensitive substring matching so this catches `CopilotTurnEvent`, `CodexProvider`, and `AcpPrompt`, not just standalone words. Include `agy` and `gemini` to catch the adapter executable alias and native home/configuration vocabulary.

```sh
rg --line-number --ignore-case --hidden --no-ignore --glob '*.rs' --glob '*.lua' --glob '!**/target/**' --glob '!**/vendor/**' --glob '!**/.git/**' --glob '!**/forge-providers/**' 'codex|copilot|claude|antigravity|acp|agy|gemini' nvim/rust/forge nvim/lua nvim/tests
```

**Pass:** no output and ripgrep exit status `1` (no matches). **Fail:** exit status `0` with any matching source line. Exit status `2` or another execution error means validation did not complete, not that the source is clean. Do not pipe this gate through `head`, truncate its output, or exclude test directories to obtain a pass.

**2. Search file and directory names.** Empty provider-named wrappers and fixtures also violate the boundary, even when their contents no longer contain the name. Check the source inventory first, then filter its paths:

```sh
rg --files --hidden --no-ignore --glob '*.rs' --glob '*.lua' --glob '!**/target/**' --glob '!**/vendor/**' --glob '!**/.git/**' --glob '!**/forge-providers/**' nvim/rust/forge nvim/lua nvim/tests
rg --files --hidden --no-ignore --glob '*.rs' --glob '*.lua' --glob '!**/target/**' --glob '!**/vendor/**' --glob '!**/.git/**' --glob '!**/forge-providers/**' nvim/rust/forge nvim/lua nvim/tests | rg --line-number --ignore-case 'codex|copilot|claude|antigravity|acp|agy|gemini'
```

**Pass:** the inventory command succeeds and lists the expected maintained sources; the filtered command produces no matches. A failing inventory is not a pass merely because the final pipeline filter sees no input. Relocate native files to their owner or replace genuinely generic fixtures; do not rename a native wrapper while leaving its responsibility outside the crate.

**3. Search for native protocol behavior with provider-neutral names.** This catches the current certificate machinery and representative native wire spellings that the noun search cannot detect. Every match requires inspection; a generic host type may remain only if it no longer constructs or interprets native completion.

```sh
rg --line-number --hidden --no-ignore --glob '*.rs' --glob '*.lua' --glob '!**/target/**' --glob '!**/vendor/**' --glob '!**/.git/**' --glob '!**/forge-providers/**' 'ExecutionCertificate|NativeExecutionCertificate|TerminalBoundary|certified_executions|certified_submissions|foreground_anchor|parentAgentTaskId|parentToolCallId|completionEventId|session[./](idle|sendAndWait|eventLog|prompt)|assistant[.]turn_(start|end)|turn/(start|started|completed|interrupt|steer)|thread/(start|resume|unsubscribe)|stopReason|send_user_message_question_reply|localImage|mimeType' nvim/rust/forge nvim/lua nvim/tests
```

**Pass:** no remaining native protocol interpretation or certificate construction outside the native crate. Record a disposition for every hit; generic serialization elsewhere may legitimately use a spelling such as `mimeType`, but a native attachment encoder may not. Expand this pattern when the inventory or review finds additional native spellings; it is not an exhaustive protocol schema.

**4. Inspect the provider crate's public surface and external SDK dependencies.** These searches intentionally include the native crate or manifests, so their output requires review rather than blanket zero matches. Verify visibility from `lib.rs` through every public module and re-export; a `pub` item inside a private module is not automatically externally reachable.

```sh
rg --line-number '^\s*pub\s+(mod|use|struct|enum|trait|type)\b|^\s*pub\s+(async\s+)?(const\s+)?fn\b' nvim/rust/forge/crates/forge-providers/src --glob '*.rs'
rg --line-number --ignore-case --hidden --no-ignore --glob '*.rs' --glob 'Cargo.toml' --glob '!**/target/**' --glob '!**/vendor/**' --glob '!**/.git/**' --glob '!**/forge-providers/**' 'github[-_]copilot[-_]sdk|agent[-_]client[-_]protocol|forge_providers::(copilot|codex|acp)' nvim/rust/forge
```

**Pass:** externally reachable signatures expose only normalized contracts, not concrete native owners, SDK types, arbitrary native method/JSON operations, or native-state accessors. No consumer crate declares a direct native SDK dependency, including dev-dependencies. A workspace dependency declaration is build metadata to report separately, not permission for a consumer dependency; generated lockfile package names are likewise not runtime leaks.

For each run, record the commands, exit statuses, complete matching paths/lines, and review disposition. Source hits in the first two gates block completion; do not add source allowlists, suppressions, split-string spellings, or inferred SDK types merely to silence the grep. Metadata/documentation exclusions must remain explicit and must not grow to hide maintained application code.

A clean search is necessary but not sufficient. Confirm the public boundary with compilation and the lifecycle regressions above: renamed objects or generic JSON APIs can still leak native behavior without containing any provider noun.

**5. Audit every outward contract for complete normalization.** For each public operation, event variant, error, callback, and descriptor, identify its producer in `forge-providers` and its first consumer outside the crate. Verify that the consumer can act using only the normalized contract: no native field parsing, provider-name switch, SDK downcast, native status interpretation, protocol-specific ordering, or second completion/certification step.

**Pass:** every consumer receives an already-normalized meaning and needs no native implementation knowledge. Opaque provider-issued identities may cross as values, but consumers must not parse their internal format. Tool arguments and user content may remain payloads of normalized contracts; they must not serve as escape hatches for native protocol envelopes. Any contract that requires downstream normalization blocks completion even when all grep searches are clean.

## Rollout and failure boundaries

Do not deploy the partial history-reader attempt or an unverified stream relay. Complete the contract cutover and its targeted verification before rebuilding the executable that new editors launch.

Existing editor hosts retain their running executable; a passing build does not update them. Verify in a fresh isolated host first, then restart affected user editors only with authorization.

Stop rollout if a valid completed Background request remains stuck or fails, a stale/child event finishes the wrong input, host effects disappear, cancellation leaves native work running, or a subsequent prompt replays uncertain input. Keep the failing fixture and diagnostic metadata; revert only the isolated consolidation change using the normal reviewed Git workflow, preserving the earlier queue, recovery, input-recognition, and PR caller fixes.

Diagnostic records should identify the operation, session/invocation, normalized input identity, and failure category without recording prompt content or credentials. Report the first failure and distinct cleanup failures once rather than recursively embedding the same observation error.

## Definition of done

The work is complete only when the incident and the broader ownership gap are both resolved. A disappearing error string or a clean noun search alone is insufficient.

- Background retains its own persistent session and succeeds on the missing-notification regression without complete native ancestry reconstruction.
- Persistent providers expose one normalized lifecycle stream; callers never fetch or construct separate native completion certificates.
- Full normalization occurs before the provider boundary: no outward event, result, error, callback, or descriptor requires a consumer to interpret native semantics.
- Isolated `generate_text` returns a value or error and owns its wait/cleanup independently of Background.
- Provider-specific objects, encoders, defaults, and lifecycle policy are private to `forge-providers`; shared contracts and Lua consumers are provider-neutral.
- The mandatory provider-leak search gate passes: source contents and paths have zero external provider-name matches, native protocol hits are resolved, and public API/SDK dependency review finds no native exposure.
- Existing queue/recovery behavior, accepted-input correlation, host-effect ordering, cancellation, and explicit error reporting survive the cutover.
- Targeted regressions and fresh host verification pass against the rebuilt development executable, and no live-provider success is claimed without an authorized live run.
