# Session methods

The transport metadata comes from the shared contract. Each entry summarizes the inspected handler.

All calls inherit the [request lifecycle and failure contract](lua-client.md#requestspec-callback-progress). Method failures do not select another session or close an otherwise valid connection.

## `session.agent.followup`

Submit follow-up work to an existing agent.

- **Context:** Explicit existing session/provider.
- **Input:** target and message.
- **Result and effects:** Returns the follow-up operation result.
- **Boundary:** The agent's current lifecycle determines whether work starts or is queued.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

Requires nonempty `target` and `message` strings. The target resolves within this root session. Returns `{ "agent": Agent, "task": AgentTask, "status": "admitted" }` after durable admission, with `replayed: true` when the same identity and work were already admitted. The child retains its identity and receives a new task, queued if earlier work is active. Reusing an operation identity with different target or text fails. Unknown targets, invalid profiles, capacity, and persistence failures remain explicit. Supply a stable `operation_id` string for recovery. Omission generates a new UUID, so a repeated request without that identity cannot establish replay equivalence.

## `session.agent.interrupt`

Interrupt an explicitly identified agent.

- **Context:** Explicit existing session/provider.
- **Input:** target.
- **Result and effects:** Returns the interruption operation result.
- **Boundary:** Interrupting does not erase the agent's recorded history.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

Requires nonempty `target` (string). Returns `{ "status": "interrupting", "target": target }` after requesting interruption within the selected root session. This acknowledges the interruption request, not provider cleanup completion. Unknown targets and cancellation failures are explicit. The operation does not erase saved child history. Supply a stable `operation_id` string for recovery. Omission generates a new UUID, so a repeated request without that identity cannot establish replay equivalence.

## `session.agent.interrupt_all`

Interrupt the selected session's agents.

- **Context:** Explicit existing session/provider.
- **Input:** The explicit parent-session context.
- **Result and effects:** Returns the aggregate interruption operation result.
- **Boundary:** The scope is this session's agents, not unrelated host sessions.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

Accepts an empty parameter object. Returns `{ "status": "interrupting" }` after requesting interruption for each managed child in this root session. The primary agent and unrelated root sessions are outside that iteration. A child interruption failure stops the call and can leave earlier interruption requests already applied. The response does not establish provider cleanup completion. Supply a stable `operation_id` string for recovery. Omission generates a new UUID, so a repeated request without that identity cannot establish replay equivalence.

## `session.agent.message.send`

Send a message to an existing agent.

- **Context:** Explicit existing session/provider.
- **Input:** target and message.
- **Result and effects:** Returns the routed agent-operation result.
- **Boundary:** A message is distinct from starting a new agent or resuming an idle one.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

Requires nonempty `target` and `message` strings. Returns the saved message object with `id`, `ordinal`, `message`, `delivery`, `task_id`, and `created_at_ms`. Delivery is `steered` when an active provider accepts native steering, otherwise `queued` for an input boundary. This does not start an idle child or create a follow-up task. Repeating the same identity/text returns the saved message. Different text under that identity fails. Unknown targets and persistence failures are explicit. Provider steering rejection retains the message as queued. Supply a stable `operation_id` string for recovery. Omission generates a new UUID, so a repeated request without that identity cannot establish replay equivalence.

## `session.agent.model.list`

List models usable for a child agent.

- **Context:** Explicit existing session/provider.
- **Input:** provider and the explicit parent-session context.
- **Result and effects:** Returns the agent-model catalog.
- **Boundary:** Provider availability and discovery failure remain explicit.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

Requires `provider` (string), parsed as a supported backend kind. Returns an array of [BackendModel](runtime-schemas.md#backendmodel) for launching a child using the current session's workspace/access policy. Discovery may contact or launch that provider and has a 30-second deadline. It does not create a child agent or send a prompt. Unknown providers and discovery failures fail explicitly. It emits no request progress.

## `session.agent.start`

Start an agent within the selected session.

- **Context:** Explicit existing session/provider.
- **Input:** The agent launch request accepted by the coordinator.
- **Result and effects:** Returns launch information and ensuing agent state.
- **Boundary:** The agent receives its own execution identity under the parent session.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

Requires nonempty `text` (string) and accepts `profile` (string or null). Unknown fields other than the transport `operation_id` fail decoding. The profile selects the child provider/model and access configuration. This creates fresh child work without inheriting the parent transcript. Returns `{ "agent": Agent, "task": AgentTask, "status": "admitted" }` after durable admission. Repeating the same operation and captured work returns that result with `replayed: true`. Reusing the identity for different work fails. Admission may queue provider execution and does not mean the child finished. Profile/provider resolution, capacity, persistence, and ownership failures remain explicit. Supply a stable `operation_id` string for recovery. Omission generates a new UUID, so a repeated request without that identity cannot establish replay equivalence.

## `session.approval.continue`

Continue after the approval-wait lifecycle permits it.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** The explicit session approval-continuation context.
- **Result and effects:** Returns continuation state.
- **Boundary:** This follows the approval timeout/continuation rules rather than inventing an approval decision.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

No parameter fields. Returns [BrokerSnapshot](runtime-schemas.md#brokersnapshot). It consumes pending approval-timeout feedback for this session's root execution. Without feedback it returns the current snapshot. With a running/waiting selected task it reserves a continuation carrying that feedback; a paused task remains paused. Without a selected task it runs a new chat exchange with an approval-timeout continuation origin. This does not approve the timed-out operation. Provider/storage failures remain explicit, and task reservation is distinct from provider completion.

## `session.approval.resolve`

Resolve a pending approval.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** The approval identity and selected decision.
- **Result and effects:** Returns approval/control state.
- **Boundary:** The decision applies only to the identified live approval.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `control`. Recovery: `session_command`.

### Contract

Requires `approval_id` and `answer_list` (array of [ApprovalAnswer](runtime-schemas.md#approvalanswer), each with `item_id` and `choice_id`). Answers select choices from that pending approval's [ApprovalRequestView](runtime-schemas.md#approvalrequestview). Returns `{ "resolved": true, "approval": ... }` with the resolved view. The permission coordinator validates item/choice ownership, records permitted grants, removes the pending request, and resumes its waiting operation. Missing, expired, wrong-owner, or invalid choices fail. This can authorize the operation represented by the approval, so callers must not substitute choices from another request. Resolution is not proof that the subsequent operation completed.

## `session.attachment.import`

Import an attachment into the session's managed attachment storage.

- **Context:** Explicit existing session.
- **Input:** Source path and the attachment number used by the caller.
- **Result and effects:** Returns imported attachment information.
- **Boundary:** I/O failure does not mean the attachment was submitted to the provider.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

Requires `path` (an absolute local path string). Optional `number` is an unsigned integer defaulting to 1 and determines the initial display label. The source must be a regular file with a UTF-8 filename. Returns [AttachmentSelection](runtime-schemas.md#attachmentselection): retained metadata `id`, editable `label`, original `name`, detected `media_type`, and UTF-8 byte offsets `start`/`end` for inserting the label.

Import copies the complete file into the content-addressed object store and persists attachment metadata before returning. It does not submit the attachment, invoke a provider, or change the source file. Source changes during capture, I/O failures, invalid paths, and metadata publication failures are explicit. Later composer edits must preserve the selection identity and update its byte span. It emits no request progress.

## `session.check.cancel`

Cancel an identified active check run.

- **Context:** Explicit existing session/check owner.
- **Input:** run_id.
- **Result and effects:** Returns cancellation/status information.
- **Boundary:** Cancellation must collect owned processes before releasing their resources.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/checks/tools.rs`](../../../rust/forge/crates/forge-harness/src/checks/tools.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

Requires `run_id` (string), with optional `interrupt_execution` (false by default). Returns `{ "run_id": ..., "cancellation_requested": true }`. Active provider work or an explicit execution interrupt coordinates native cleanup before requesting cancellation; otherwise the check supervisor cancels the run directly. Recorded output and finished commands remain available. Cleanup contention, unknown runs, and native process failures are explicit. The acknowledgement does not erase history or imply every asynchronous observer has already seen completion.

## `session.check.complete`

Complete the automatic-check continuation boundary.

- **Context:** Explicit existing session/check owner.
- **Input:** The selected session/check continuation context.
- **Result and effects:** Returns the resulting session state and continuation events.
- **Boundary:** This can resume waiting workflow work rather than merely querying status.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/checks/tools.rs`](../../../rust/forge/crates/forge-harness/src/checks/tools.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

No parameter fields. Reconciles automatically owned check completion with the session's task lifecycle and returns [BrokerSnapshot](runtime-schemas.md#brokersnapshot). It starts no arbitrary caller-supplied command. Check outcomes can reserve additional task work, retain a blocked/paused execution, or retire a completed task. Required gate/configuration, storage, and cleanup failures remain explicit. The response describes settled native check state, not completion of later reserved provider work.

## `session.check.editing.set`

Set whether check configuration editing is enabled.

- **Context:** Explicit existing session/check owner.
- **Input:** enabled boolean.
- **Result and effects:** Returns the resulting check-editing state.
- **Boundary:** This toggles editing state, not check execution.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/checks/tools.rs`](../../../rust/forge/crates/forge-harness/src/checks/tools.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

Requires `enabled` (boolean). Returns `{ "check_editing": enabled }` after saving the conversation-scoped setting and publishing it to the observer. It does not run checks, start a prompt, or cancel an active turn. Concurrent settings revision changes and persistence failures fail the request.

## `session.check.list`

Read the session's check catalog and current selection.

- **Context:** Explicit existing session/check owner.
- **Input:** The check-list request accepted by the check service.
- **Result and effects:** Returns check definitions/selection and relevant current state.
- **Boundary:** Listing checks does not start them.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/checks/tools.rs`](../../../rust/forge/crates/forge-harness/src/checks/tools.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

No parameter fields. Returns a catalog with `configured`, configuration `revision`, optional `snapshot`/`error`, `overrides`, `history`, optional `active_run` and `task_id`, `changed_files`, and `applicability`. It also supplies effective `automatic_checks`/`check_timing` settings and, when available, `preview`, `coverage`, and automatic-run state. The service refreshes observed workspace changes and reads saved check configuration; it does not execute the returned commands. Configuration errors remain in the catalog when a usable catalog can be returned. Storage and change-capture failures can fail the request.

## `session.check.override`

Change session/task check selection overrides.

- **Context:** Explicit existing session/check owner.
- **Input:** enabled, auto, and scopes maps as applicable.
- **Result and effects:** Returns the updated check configuration view.
- **Boundary:** Overrides do not rewrite the project's entire check definition.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/checks/tools.rs`](../../../rust/forge/crates/forge-harness/src/checks/tools.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

Accepts [CheckOverrides](runtime-schemas.md#checkoverrides): `enabled`, `auto`, and `scopes` maps, each defaulting to empty and keyed by configured check ID. Returns the updated catalog shape documented for [session.check.list](#sessionchecklist). It replaces temporary overrides for the selected task entry, or the ad hoc conversation when no entry exists. It does not rewrite project check definitions or start a run. Unknown IDs, unsupported file/case scope, an active run, or persistence failure reject the update.

## `session.check.run`

Start the selected checks.

- **Context:** Explicit existing session/check owner.
- **Input:** The supported check selection and declared input values.
- **Result and effects:** Returns run admission/state and later progress.
- **Boundary:** Only selected work is admitted; failures and cancellation retain run identity.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/checks/tools.rs`](../../../rust/forge/crates/forge-harness/src/checks/tools.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

Accepts [CheckRunRequest](runtime-schemas.md#checkrunrequest): `check` is one configured check ID or an array of IDs; optional `inputs` supplies configured typed input values; `all` defaults to false. `inputs` and `all: true` are mutually exclusive. Returns `{ "run_id": ... }` after starting native check work. The service refreshes workspace changes and resolves configured commands without accepting arbitrary replacement command text. Planning, active writing agents, or active source execution prevent admission. Unknown checks, invalid inputs/selections, configuration errors, and process-admission failures are explicit. Completion is observed through check events/status, not the initial response.

## `session.check.status`

Read check-run status.

- **Context:** Explicit existing session/check owner.
- **Input:** The requested run/status selection.
- **Result and effects:** Returns current status/history for the requested run.
- **Boundary:** Running, failed, cancelled, and completed states remain distinct.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/checks/tools.rs`](../../../rust/forge/crates/forge-harness/src/checks/tools.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

Requires `run_id` (string). Optional `wait_for_completion` defaults to false; `wait_ms` defaults to zero, or 60000 when completion waiting is requested. Returns the owned [CheckRun](runtime-schemas.md#checkrun), including command states, output paths/preview, and recorded results. Waiting is bounded to 30 seconds for an update or 60 seconds for completion. The response can still report running when that deadline expires. Unknown runs fail instead of returning an empty success. This is read-only and does not rerun a command.

## `session.command.apply`

Apply a session command through its typed owner.

- **Context:** Explicit existing session.
- **Input:** operation_id, action, and variant fields. Task actions include goal, plan, execute, attach_plan, fork, replan, pause, resume, clear, and continue. The typed review variants include begin_acceptance, cancel_acceptance, request_changes, cancel_plan, and complete_acceptance. Existing configuration/compaction/rollback actions retain their separate service contracts.
- **Result and effects:** Task state commands return their completed receipt. Review variants return their defined snapshot/receipt response. Configuration and execution controls retain their operation lifecycle. Document each surviving action and response variant explicitly rather than assigning one shape to the whole dispatcher.
- **Boundary:** Task commands commit state and pending delivery. Pause suspends queued input but lets current execution finish. Explicit cancellation uses `session.turn.cancel` after pausing. With no live task, pause preserves the task slot rather than failing. Capture revision before preparation and validate task/artifact authority at commit. Configuration and explicit execution controls remain separate from task ownership.
- **Inspected source:** [task/command.rs](../../../rust/forge/crates/forge-harness/src/task/command.rs), [service/task_state.rs](../../../rust/forge/crates/forge-harness/src/service/task_state.rs), and [broker/configuration.rs](../../../rust/forge/crates/forge-harness/src/broker/configuration.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `control`. Recovery: `session_command`.

**Parameter schema:** [SessionAction](native-schemas.md#sessionaction).

**Configuration actions:** [permission, configuration, compaction, and rollback fields](session-command-configuration.md) share this carrier with task actions.

### Contract

Requires `action` and an explicit session. Task actions use [SessionAction](native-schemas.md#sessionaction): `goal`, `plan`, `execute`, `attach_plan`, `fork`, `replan`, `pause`, `resume`, `clear`, `continue`, `begin_acceptance`, `cancel_acceptance`, `request_changes`, `cancel_plan`, and `complete_acceptance`. Every variant's fields are in the linked declaration. Configuration actions `permission`, `configure`, `compact`, and `undo` use the separate [configuration contract](session-command-configuration.md).

Supply a stable `operation_id` string. Task actions generate one if absent, but that prevents recovery by a caller that loses the response. Optional `display_text` is presentation text. Internal `_command_response` selects `receipt` or `snapshot`; task starts default to receipt, and review acceptance actions default to snapshot. Lua owns the separate `_command` transport-recovery metadata. Do not generate new operation identities for retries.

A receipt contains string `id`, string `state`, integer `revision`, action name, and optional `error`. Snapshot mode returns [BrokerSnapshot](runtime-schemas.md#brokersnapshot) with that receipt in `task_operation`. Completion means the state transition committed, not that the provider has finished the new work. Task state, affected plan/question records, and the operation receipt commit together. Duplicate IDs with identical normalized actions return the saved receipt. Conflicting reuse fails.

The service publishes `session_changed` before the command response, then any action-specific event and `task_operation`, and wakes input delivery. Entry actions queue their generated prompt for the input coordinator. They can promote an active turn without cancelling it. Pause suspends future input admission and does not itself cancel the current provider turn. Empty or terminal task slots remain valid when pausing ordinary chat. Stale task revisions, invalid plan identities/digests, missing artifacts, and invalid transitions fail. Provider work can continue or fail after command acknowledgement, so observe the originating session rather than assuming the callback is final task completion.

## `session.command.operation.get`

Read a durable command operation's recorded outcome.

- **Context:** Explicit existing session.
- **Input:** operation_id.
- **Result and effects:** Returns the stored operation view when present, including id, state, revision, action, and error.
- **Boundary:** Use the durable operation identity for recovery, not the transport request ID or task revision. A recorded task command can be complete while its provider entry is still pending.
- **Inspected source:** [service.rs](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `control`. Recovery: `none`.

### Contract

Requires `operation_id` (string). Returns the durable task/configuration receipt with `id`, `state`, `revision`, `action`, and optional `error`, or JSON null if no receipt exists for this session. This is distinct from the Lua transport command ID queried by `session.command.status.get`. It reads local storage without running or replaying the command and emits no progress. Resolve an uncertain mutation with its original ID before deciding whether any retry is safe.

## `session.command.status.get`

Read the recorded outcome of a mutation command.

- **Context:** Explicit existing session.
- **Input:** The command id, distinct from this lookup request's correlation ID.
- **Result and effects:** Returns the command state and its recorded response when completed.
- **Boundary:** Unknown outcomes require inspection rather than blind replay.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

Requires `id` (string), the Lua transport command identity previously carried in `_command`. An absent record returns `{ "state": "not_admitted" }`. A retained record returns `state` equal to `running`, `completed`, or `outcome_unknown`, plus `response`, which is the original terminal response or null. It does not run or replay the original command and emits no progress. The identity belongs to the explicit session and its retained command journal. Use this transport-recovery result before replaying a lost response; task transition receipts use the different `operation_id` field and `session.command.operation.get`.

## `session.compact`

Request compaction of the selected session context.

- **Context:** Lifecycle: explicit target/source where applicable.
- **Input:** The selected session context and supported compaction options.
- **Result and effects:** Returns the resulting session state through the compaction lifecycle.
- **Boundary:** Provider work may be involved and the session ID remains the owner.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

No parameter fields. Requires provider-native compaction support and an existing provider conversation. Returns [BrokerSnapshot](runtime-schemas.md#brokersnapshot) after compaction finishes, saved capability/context metadata is updated, and the compaction lifecycle record is finalized. It publishes timeline changes and `context_compacted` on success. Unsupported providers, absent conversation, provider failure, or persistence failure reject the request; failed attempts remain recorded. Compaction is provider work and must acquire execution admission, so it cannot overlap another root turn.

## `session.configure`

Apply session configuration.

- **Context:** Lifecycle: explicit target/source where applicable.
- **Input:** The supported configuration fields, including ask_model where supplied.
- **Result and effects:** Returns updated configuration/session state.
- **Boundary:** Configuration uses coordinator/task boundaries and can require active work to settle.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

Accepts the fields of [session command configuration](session-command-configuration.md) directly, including model/effort/context window/service tier, access and approval policy, shell, executor policy, automatic checks, input timeouts, and tool exposure. Omitted fields retain their current value. `validate: true` validates requested model choices against provider discovery. `settings_revision` instead captures an exact saved settings revision and reconciles its desired state; superseded revisions fail. `session_policy: true` saves supported settings to session scope rather than global defaults. A sole `ask_model` field uses the provider-local [AskModel](runtime-schemas.md#askmodel) setting and returns `{ "ask_model": ... }`.

Ordinary configuration returns the saved [HarnessSession](runtime-schemas.md#harnesssession) and publishes `session_configured`. Provider/resource updates can emit `configuration_progress` tied to the operation ID. Unsupported capability/model/effort/window/tier, invalid access or timeout values, concurrent settings changes, and provider reconciliation failures are explicit. Configuration can change saved global or session preferences and can require provider cleanup. Use canonical `session.command.apply` when the action must coordinate interruption/resumption of active work.

## `session.delete`

Delete an explicitly selected saved session.

- **Context:** The envelope `session_id` identifies the calling session, captured when its picker opens.
- **Input:** `params.session_id` is the distinct saved session to delete. Neither identifier defaults to the active view. The target must differ from the calling session.
- **Result and effects:** Returns the deletion outcome.
- **Boundary:** Deletion is a persistence mutation and must not target the active session implicitly.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

The two session identities have different roles:

| Field | Type and requirement | Meaning |
| --- | --- | --- |
| Envelope `session_id` | Required session ID string | The session whose view initiated deletion. Capture it when opening the picker. |
| `params.session_id` | Required session ID string | The saved session to delete. It must differ from the calling session. |
| Result `deleted` | Session ID string | The deleted local session. |
| Result `provider_session_preserved` | Boolean, `true` on success | The provider's own conversation remains available. |

Deletion closes an idle cached controller, releases its lease, and removes the local session and plan files. Active execution, control work, queued input, agents, or checks reject deletion. An unknown target or a conflicting lease also fails. No provider request or request-specific progress event occurs. The caller must discard a late response after its picker closes. After a lost connection, refresh the session list before deciding whether to retry because this method has no operation receipt.

For an existing caller `caller` and a distinct idle target `saved`, the complete wire exchange is:

```json
{"id":42,"method":"session.delete","session_id":"caller","params":{"session_id":"saved"}}
```

```json
{"id":42,"result":{"deleted":"saved","provider_session_preserved":true}}
```

The [picker regression](../../../tests/forge/session_picker.lua) checks both identities, and the [built-host regression](../../../tests/forge/harness_host.lua) deletes an idle cached session while another session is running and rejects deletion of the caller itself.

## `session.document`

Read or operate on a session's native presentation document.

- **Context:** Session lifecycle or explicit existing session.
- **Input:** The presentation operation and its document/view/input fields, including open_diff where selected.
- **Result and effects:** Returns presentation patches, snapshots, or a saved source-diff document.
- **Boundary:** Only the explicit session owns the presentation request.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Operation fields:** [typed discriminator and parameter tables](operation-schemas.md).

### Contract

The `operation` discriminator selects a [PresentationRequest](operation-schemas.md), or `open_diff` with `document` and captured `input` ([DocumentInput](repository-schemas.md#documentinput)). The operation schema specifies every input field, including flattened node requests. Every document/view/target belongs to this explicit session; revisions, sequences, and node generations must come from the current projection. Stale callbacks must not substitute the active buffer's identity.

| Operation | Result and completion boundary |
| --- | --- |
| `open` | [PresentationOpen](runtime-schemas.md#presentationopen): separate status/transcript snapshots, syntax readiness, and startup timings. |
| `sync` | [PresentationSync](runtime-schemas.md#presentationsync): status snapshot, syntax readiness, patches, and optional replacement snapshot. A stale replica can require the complete snapshot. |
| `snapshot` | Complete [BufferSnapshot](repository-schemas.md#buffersnapshot) for the selected retained document. |
| `open_view`, `node`, `select_agent` | Empty object after the local selection/expansion state is committed. Changes are consumed through subsequent document synchronization. |
| `resize` | `{ "updated": true }` after view width and resulting projection are retained. |
| `close`, `close_view` | `{ "closed": true }` after the corresponding local document/view lifecycle ends. |
| `staging` | `{ "updated": true }` after newer composer queue/steering state is retained. This projects pending text; it does not submit it to the provider. |
| `input` | [TranscriptAction](runtime-schemas.md#transcriptaction), or `{ "kind": "diff" }` for a saved diff target. |
| `status_action` | The action captured by the separate status document, after status revision and input-sequence validation. |
| `navigate_prompt` | Navigation result from the captured transcript input and requested direction. |
| `tool_open` | `{ "snapshot": ..., "more": true, "title": ... }` for a new output document. |
| `tool_demand` | [OutputDelivery](runtime-schemas.md#outputdelivery): optional patch and whether more output remains. |
| `tool_export` | `{ "path": ... }` for a complete retained output exported under the session data root. |
| `highlight` | Empty object after captured syntax analysis is applied if its document generation is still current. |
| `open_diff` | [SourceDelivery](repository-schemas.md#sourcedelivery) for complete saved diff text. |
| `recap`, `session_name` | `{ "text": ... }` after generation using the requested model and captured conversation history. This can invoke the provider. |
| `background_terminals` | Provider terminal inventory, or `{ "supported": false, "reason": ..., "terminal": [] }`. |
| `terminate_terminal` | Empty object after provider termination completes, with an eight-second deadline. |
| `plan_open` | Review snapshot, `saved_source_digest`, `plan_id`, `version`, `path`, `public_only`, annotations, source-edit metadata, and check options. Requested digest/version and optional saved-source digest must match. |
| `plan_view` | Updated plan review presentation for the captured document/view and optional width. |
| `plan_action`, `plan_definition` | The selected review action/navigation result, such as a source target, reference/rename result, projection update, or schema snapshot. These actions retain their captured review identity and do not authorize a later mutation against a different version. |
| `plan_save_annotations` | `{ "saved": true }` after saving annotations against the captured source digest. |
| `plan_close` | Empty object after cancelling and collecting the review document's admitted local work. |

Document rendering and input projection do not mutate repository source. Provider generation/terminal operations and local tool-output export have the effects described above. Request completion is separate from future transcript events or previously admitted provider work. Invalid operation fields, closed documents, stale identity/revision/sequence, unavailable source/analysis/provider capability, and I/O failures remain explicit. Buffer patches require matching document and base revision; retain the prior usable view when validation fails.

## `session.exchange.comment.save`

Persist an exchange review comment.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** The target exchange/comment payload.
- **Result and effects:** Returns the saved comment result.
- **Boundary:** This saves review text rather than submitting a provider prompt.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

Accepts [ExchangeComment](runtime-schemas.md#exchangecomment): required string `id`, `exchange_id`, `file_path`, and `body`, optional/null integer `old_line` and `new_line`, and integer `created_at_ms`. The timestamp field is decoded but replaced with server time. The exchange must belong to the explicit session. Returns the saved comment. Local persistence upserts by comment ID, so another save with that ID replaces its payload. It emits no provider prompt or progress.

## `session.exchange.list`

List exchanges available for review.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** An empty parameter object.
- **Result and effects:** Returns the exchange-review inventory.
- **Boundary:** Listing does not roll back or resume an exchange.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

No parameter fields. Returns the selected session's saved exchange metadata as an array, including saved comments. Empty history returns `[]`. This call does not load full tool-output bodies, resume exchanges, mutate task selection, or invoke a provider. No progress is emitted. Timeline presentation remains owned by `session.document`.

## `session.exchange.recovery`

Inspect or resolve retained exchange recovery state.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** An optional supported recovery action such as undo.
- **Result and effects:** Returns recovery state or the action's result.
- **Boundary:** Recovery must preserve newer edits and report conflicts.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

Accepts `action`, defaulting to `status`. Status returns null when no restore journal exists, otherwise `{ "pending": ..., "applied": ..., "files": ... }`. `continue` requires an incomplete journal and settled exchange tree, resumes filesystem restoration, and returns `{ "restored": true }` after finalization. `undo` creates a reverse restore preview and returns the same preview-summary shape as rollback preparation; it does not apply that preview. Unknown actions, missing journals, changed workspace state, and I/O failures are explicit. This recovers workspace restore, not provider or GitHub mutation delivery.

## `session.exchange.rollback.apply`

Apply a prepared rollback.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** preview_id.
- **Result and effects:** Returns the rollback result.
- **Boundary:** Applying mutates repository/workflow state and must validate the captured preview.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

Accepts `scope`, defaulting to `workspace`. Workspace mode requires the captured `preview_id`, revalidates workspace identity, Git HEAD, current file content, and exchange history, then applies the restore and finalizes conversation rollback. Returns `{ "restored": true }`. Conversation mode requires `exchange_id` and captured `latest_exchange_id`, changes conversation history without restoring files, and returns `{ "restored": true, "scope": "conversation" }`. Both require settled exchange trees and no active children. A partial filesystem restore retains a journal and requires recovery before new workspace work. Stale previews, conflicts, unavailable checkpoints, and I/O failures remain errors.

## `session.exchange.rollback.prepare`

Capture a rollback preview for an exchange.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** exchange_id.
- **Result and effects:** Returns a preview identity and rollback information.
- **Boundary:** Preparation does not apply the rollback.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

Requires `exchange_id` in the current session. Resolves its checkpoint owner, requires settled target-and-later exchange trees, verifies that the target is still in current workspace history, and prepares a durable restore preview. Returns `preview_id`, total `files`, `warning_count`, first `warnings` page, and optional `next_offset`. Preparation reads current workspace/checkpoints and saves the preview but does not restore files. Missing Git/checkpoints, active child work, unresolved restore recovery, or I/O failure rejects preparation.

## `session.exchange.rollback.preview`

Read another page of a captured rollback preview.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** preview_id and offset.
- **Result and effects:** Returns the requested preview page.
- **Boundary:** Use the existing preview identity rather than recapturing unrelated state.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

Requires `preview_id`; optional `offset` defaults to zero. Returns the retained preview's ID, total file/warning counts, a warning page, and `next_offset` when more warnings remain. The preview belongs to this session and must exist. This is a read-only page lookup and does not apply the restore. Follow offsets to retain every warning rather than treating the first page as complete.

## `session.fork`

Create a new session from an explicitly selected source session.

- **Context:** Lifecycle: explicit target/source where applicable.
- **Input:** Source session and supported fork parameters.
- **Result and effects:** Returns the fork result/new session identity through the coordinator.
- **Boundary:** Provider-fork readiness and failure belong to the fork lifecycle.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

Accepts optional source `session_id` and `name` in parameters, with required outer source-session context. The source must use the configured backend and support native fork. Returns a child [BrokerSnapshot](runtime-schemas.md#brokersnapshot) with `fork_performance` containing `total_ms`, phase timings, and `provider_pending: true`. The child and inherited timeline are durably created before the provider-native fork finishes. A `session_created` event identifies the child; later fork events resolve its readiness gate. Child provider work must wait for that gate and can fail independently of the initial response. Live task selection is not inherited. This operation uses the same provider, not a cross-provider replay.

## `session.goal.clear`

Apply Clear to the session task slot through the retained goal alias.

- **Context:** Explicit existing session.
- **Input:** operation_id when supplied.
- **Result and effects:** Returns the completed command receipt. Retires owned questions and records the cleared task outcome.
- **Boundary:** Does not declare the objective achieved or cancel an active provider turn. Preserve the shared task command contract and response variant.
- **Inspected source:** [task/command.rs](../../../rust/forge/crates/forge-harness/src/task/command.rs), [service/task_state.rs](../../../rust/forge/crates/forge-harness/src/service/task_state.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

No action-specific fields. Supply optional `operation_id` for reliable recovery. This is the `action = "clear"` form of [session.command.apply](#sessioncommandapply), returning its transition receipt. It clears the selected live task through the task owner without deleting historical exchanges. It inherits canonical persistence, validation, and replay semantics and emits no request-progress stream.

## `session.goal.pause`

Apply Pause to the session task slot through the retained goal alias.

- **Context:** Explicit existing session.
- **Input:** operation_id when supplied.
- **Result and effects:** Returns the completed command receipt while preserving task input and questions.
- **Boundary:** Pauses a live task and suspends queued input through the shared command owner without stopping current execution. With no live task, the task slot remains unchanged. Preserve the shared task command contract and response variant.
- **Inspected source:** [task/command.rs](../../../rust/forge/crates/forge-harness/src/task/command.rs), [service/task_state.rs](../../../rust/forge/crates/forge-harness/src/service/task_state.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

No action-specific fields. Supply optional `operation_id` for reliable recovery. This is the `action = "pause"` form of [session.command.apply](#sessioncommandapply), returning its transition receipt. It pauses the live task and suspends queued input admission. An empty or terminal task slot remains valid. It does not interrupt the current turn; explicit cancellation uses `session.turn.cancel`. It emits the canonical session/task events and no request progress.

## `session.goal.resume`

Resume the session task through the retained goal alias.

- **Context:** Explicit existing session.
- **Input:** operation_id when supplied and optional message/text and existing resume metadata.
- **Result and effects:** Returns the command receipt. The controller prepares any required pending entry for later delivery.
- **Boundary:** Resume does not start a provider inline or turn configuration changes into task resumption. Preserve the shared task command contract and response variant.
- **Inspected source:** [task/command.rs](../../../rust/forge/crates/forge-harness/src/task/command.rs), [service/task_state.rs](../../../rust/forge/crates/forge-harness/src/service/task_state.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

Accepts [ResumeRequest](native-schemas.md#resumerequest): optional `message` (`text` alias), `lifecycle`, and `continuation`, plus optional stable `operation_id`. This is the `action = "resume"` form of [session.command.apply](#sessioncommandapply). It validates the current paused/recoverable task, commits its resumed state, and lets the input coordinator deliver eligible work. The receipt precedes completion of provider work. It emits no request progress and does not silently select another task or session.

## `session.goal.set`

Install a Goal task through the shared command boundary.

- **Context:** Explicit existing session.
- **Input:** objective, normalized to text, and operation_id when supplied.
- **Result and effects:** Returns the completed command receipt. Commits the Goal and its pending entry, selecting native support through shared preparation.
- **Boundary:** The coordinator delivers the entry. An active turn is preserved, with provider capabilities deciding steering versus a later turn. Preserve the shared task command contract and response variant.
- **Inspected source:** [task/command.rs](../../../rust/forge/crates/forge-harness/src/task/command.rs), [service/task_state.rs](../../../rust/forge/crates/forge-harness/src/service/task_state.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

Requires nonblank `text` (string); `objective` is accepted when text is absent. Optional `operation_id` is a stable mutation identity, and optional `display_text` controls presentation. This is the `action = "goal"` form of [session.command.apply](#sessioncommandapply) and inherits its receipt, persistence, event ordering, and recovery contract. It selects the session-level task and queues its start prompt. During active work it promotes the task without cancelling the provider turn. When idle it begins a new exchange through input delivery. It emits no request-progress stream.

## `session.health.get`

Check whether the selected session coordinator is responsive.

- **Context:** Explicit existing session.
- **Input:** An empty parameter object.
- **Result and effects:** Returns session_id and responsive status.
- **Boundary:** Responsiveness is not proof that the provider is idle or available.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `control`. Recovery: `none`.

### Contract

No parameter fields. Returns `{ "session_id": "…", "responsive": true }` after resolving the explicit session and checking that it is not closing. This tests the session service, not provider/network health. It uses the control-response channel and emits no progress, provider prompt, or mutation.

## `session.initialize`

Initialize the shared runtime and explicitly create or resume the requested session.

- **Context:** Session lifecycle or explicit existing session.
- **Input:** Storage/configuration paths, workspace, client_id, backend launch configuration, model/effort options, and explicit session/create/lease choices.
- **Result and effects:** Returns the initial session snapshot, including session.id.
- **Boundary:** The cleanup must distinguish runtime readiness from implicit default-session creation.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `runtime`. Admission: `control`. Recovery: `none`.

**Parameter schema:** [InitializeRequest](native-schemas.md#initializerequest).

### Contract

Accepts [InitializeRequest](runtime-schemas.md#initializerequest): required `data_root`, `workspace`, `client_id`, and backend launch descriptor; optional stored session ID, new-session name, configuration/permission paths, background identity, provider launch map, and lease-conflict action. `model` defaults to `default`, `effort` to `medium`, and `goal_max_turns` to 20. The outer session envelope is forbidden because initialization establishes ownership. The additional boolean `services_only` defaults to false.

Normal initialization returns [BrokerSnapshot](runtime-schemas.md#brokersnapshot) for the created/resumed session, including its stable ID and runtime epoch. Services-only initialization returns `initialized: true` and optional startup timing without selecting or creating a session. Repeating services initialization requires the same workspace/data root. Runtime mismatch, provider/configuration failure, and session lease conflicts are explicit. Initialization does not run a user prompt. Do not use the old host-handshake name for this operation.

## `session.input.apply`

Apply an input or queue lifecycle operation.

- **Context:** Explicit existing session.
- **Input:** Operation id, action, control_revision, and optional after_control ordering identity.
- **Result and effects:** Ordinary input operations return input changes and acknowledgements. A submitted User message containing recognized task syntax normalizes to the corresponding command response before queue/provider admission.
- **Boundary:** Preserve operation identity through retry and honor entry/control revisions. Committed queue edits are parsed again before delivery, using the stable message/revision command identity. TaskEntry and TaskContinuation instructions remain literal internal input and cannot be forged through this public endpoint.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

Accepts [InputOperation](runtime-schemas.md#inputoperation): nonempty `id`, integer `control_revision` defaulting to zero, optional string `after_control`, and a required tagged [InputAction](runtime-schemas.md#inputaction). The seven action variants are `submit`, `retry`, `resume`, `reserve`, `commit`, `release`, and `remove`. Their exact fields and nested message/edit types are in the linked declarations.

`submit` accepts user-purpose input only. It requires a new nonempty message identity and nonblank text unless model configuration is present. `intent` is `auto` or `queue`. Client configuration can select only `model`, `effort`, `service_tier`, and `context_window`. Harness alone generates task-entry and task-continuation purposes. `reserve` captures a message revision for editing, `commit` validates every edit before changing the queue, and `release` drops that reservation. `remove` and `retry` require the captured editable revision. `resume` requires the current control revision.

Returns [InputUpdate](runtime-schemas.md#inputupdate). Inspect its optional `error` even when the transport succeeded. Validation failures can be recorded as unsuccessful updates without changing messages. Successful admission publishes `input_changed` before the final response and wakes the input worker afterward. The worker may later publish delivering, accepted, failed, or unknown delivery state. Admission acknowledgement is not provider acceptance.

Reusing an operation ID with identical data returns its receipt and current snapshot. Different data with that ID fails. An uncertain submission is reconciled before retry. Confirmed acceptance removes it without resending, and confirmed non-delivery is reported before another explicit retry. Queue ownership ends when the session runtime is replaced. Retain the user draft and original operation/submission identities across a lost response.

## `session.input.get`

Read the authoritative composer/input state.

- **Context:** Explicit existing session.
- **Input:** An empty parameter object.
- **Result and effects:** Returns the current input snapshot and revisions.
- **Boundary:** The caller reconciles it with locally pending edits.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

No parameter fields. Returns [InputSnapshot](runtime-schemas.md#inputsnapshot) plus boolean `execution_busy`. An empty `messages` array means no queued input. `revision` orders queue snapshots, while `control_revision` and `control_operation_id` identify suspend/resume control. No provider invocation or mutation occurs. Subscription updates can follow immediately, so apply this snapshot only to its captured session and merge by revision.

## `session.input.operation.get`

Read the outcome of an identified input operation.

- **Context:** Explicit existing session.
- **Input:** The input operation id.
- **Result and effects:** Returns the retained operation outcome when available.
- **Boundary:** Absence is not proof that an external mutation succeeded.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

Requires `id` (string), the original input operation identity. Returns [InputUpdate](runtime-schemas.md#inputupdate) or JSON null when the current queue owner has no such operation. A found update includes a current snapshot so recovery can converge after a lost callback. This read emits no progress and must not be treated as permission to resend an unknown provider submission.

## `session.input.timeout.configure`

Update session input/approval timeout policy.

- **Context:** Explicit existing session.
- **Input:** The timeout settings accepted by the coordinator.
- **Result and effects:** Returns the resulting configuration/state.
- **Boundary:** This changes policy and does not imply an immediate timeout event.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

Accepts a nonempty object containing only `approval_timeout` and/or `answer_timeout`. Each supplied value is [InputTimeout](runtime-schemas.md#inputtimeout): integer `amount` from zero through the unsigned 32-bit range and `unit` equal to `"s"`, `"m"`, or `"h"`. Zero disables expiry. Omitted fields retain their saved setting. Null, unknown fields, and durations outside the platform timer range fail.

Returns both effective timeout values. This saves the supplied fields in global settings using the captured settings revision, then updates this session's permission coordinator. Concurrent settings changes can reject the write. It can run during provider work and does not send a prompt or emit request progress. Existing deadlines retain the semantics of their captured timer state.

## `session.list`

List persisted sessions according to the requested selection.

- **Context:** Lifecycle: explicit target/source where applicable.
- **Input:** scope and workspace filters used by the session picker.
- **Result and effects:** Returns session records including their IDs.
- **Boundary:** Discovery must work without implicitly selecting a session.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Session ID forbidden. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

Accepts optional `scope` (`"repo"` by default or `"all"`) and optional `workspace` (path string, defaulting to the initialized workspace). Unknown scopes fail. The service canonicalizes the workspace and returns an array of [HarnessSession](runtime-schemas.md#harnesssession), excluding background conversations. An empty array is valid. Shared initialization suffices and no existing session, provider invocation, or lease acquisition is required. The result describes saved metadata, not exclusive ownership of the listed sessions.

## `session.lsp.analyze`

Perform the authorized semantic analysis supported by the LSP service.

- **Context:** Explicit existing session.
- **Input:** id, path, source text, and optional source_offset/source_total supplied together.
- **Result and effects:** Returns analysis/transfer progress for that source part.
- **Boundary:** A partial source transfer requires both offset and total; malformed or missing fields fail explicitly.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

Requires `id` (pending editor-request string), `path` (source path string), and `source` (UTF-8 text). Optional `source_offset` and `source_total` are nonnegative byte counts and must appear together. Without them, the call analyzes the whole text. Chunked upload starts at offset zero and each later chunk must match the same path/total and the current accumulated byte count. Empty chunks are valid only for a zero-byte source. Incomplete upload returns `{ "pending": true }`.

Completion returns `declarations` and boolean `complete`. Each declaration includes name, kind, signature, visibility, line/column, start/end lines, parent/depth, and precise range start/end positions. Structural analysis supports Rust, TypeScript, TSX, JavaScript, and Lua. Authorization runs before analysis. Missing or wrong-session requests, revoked scope, malformed upload offsets, unsupported language, and syntax-worker failures are explicit. The analysis has a ten-second worker deadline and periodically rechecks request cancellation. It reads submitted source without writing files, invoking a provider, or emitting request progress.

## `session.lsp.authorize`

Authorize a language-server query for the selected workspace/session.

- **Context:** Explicit existing session.
- **Input:** id identifying the LSP bridge request, and paths to authorize.
- **Result and effects:** Returns authorization for the bridge request's selected source paths.
- **Boundary:** Authorization is distinct from performing a project mutation.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

Requires `id` (string) for a pending editor LSP request and `paths` (array of path strings). Returns an object mapping each supplied path to a boolean authorization result. Relative paths resolve within the captured request workspace. The service evaluates the request's retained read policy before the editor reads source bytes. Denial of one path returns false for that path; missing/expired request, wrong session, cancelled scope, or changed LSP configuration fails the call. No provider prompt, write, or request progress occurs. Approval applies only to this active request scope.

## `session.lsp.respond`

Return a language-server result requested by the Rust host.

- **Context:** Explicit existing session.
- **Input:** Request id, result, and optional failure.
- **Result and effects:** Acknowledges the returned result.
- **Boundary:** Use the LSP bridge request ID, not an unrelated wire request ID.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

Requires `id` (string) for a pending editor request. Supply `result` in the request's typed LSP response shape, or `failure` (string) to settle it as failed. The service validates session/scope ownership, removes exactly that pending request, and returns `{ "accepted": true }`. A malformed result settles the waiting operation as an invalid editor response. A missing/previously settled request or wrong session fails explicitly. This is completion of existing editor work, not a new provider prompt, file write, or request-progress stream.

## `session.new`

Create a new session using explicit creation/inheritance context.

- **Context:** Lifecycle: explicit target/source where applicable.
- **Input:** Creation fields such as name, with any source session provided explicitly.
- **Result and effects:** Returns a snapshot containing the new session and its ID.
- **Boundary:** Creation must not require the caller to already own the new ID.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Optional explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

Accepts optional `name` and `source_session_id`, with optional explicit outer session context. When both source and context are supplied they must match. With a source it creates a provider-empty conversation using that source's workspace/backend and effective saved preferences. Without a source it uses the initialized shared runtime. Returns [BrokerSnapshot](runtime-schemas.md#brokersnapshot) for the new session, then publishes `session_created` with that session's identity. It does not inherit live task state or provider history. Subsequent requests must use the returned `session.id`; creation never retargets an existing callback. Missing source, incompatible configured backend, and storage/lease failure are errors.

## `session.permission.set`

Apply the selected session's permission/access change.

- **Context:** Explicit existing session.
- **Input:** Permission intent fields accepted by the configuration service, with operation identity where supplied.
- **Result and effects:** Returns the transition result.
- **Boundary:** The service routes this through the separate configuration command path. Permission changes must preserve the complete active task and cannot resume it implicitly.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

Accepts `change` ([PermissionChange](native-schemas.md#permissionchange)), optional `resume` (false by default), and a stable `operation_id` for reconciliation. It routes through the canonical `permission` action of [session.command.apply](#sessioncommandapply), returning that action's durable receipt. Omission generates an identity from the session and transport request ID, which cannot identify a later retry using a new request ID. The permission change is captured before admission, applied to this session, and optionally reserves resumed work. Invalid modes/paths, stale operation identity, or persistence failures are explicit. A completed receipt proves the state transition, not completion of any resumed provider work.

## `session.plan.accept`

Install Execute from the reviewed Plan artifact.

- **Context:** Explicit existing session.
- **Input:** plan_id, reviewed digest, saved_source_digest, approval_mode, plan_executor/executor, plan_compact/compact, comment, and operation_id as accepted by ExecuteSelection.
- **Result and effects:** Returns a session snapshot with task_operation. Commits the execution task and required artifact effects.
- **Boundary:** Validate reviewed identity and digests atomically. Do not start provider execution inside acceptance. Preserve the shared task command contract and response variant.
- **Inspected source:** [task/command.rs](../../../rust/forge/crates/forge-harness/src/task/command.rs), [service/task_state.rs](../../../rust/forge/crates/forge-harness/src/service/task_state.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

Accepts [ExecuteSelection](native-schemas.md#executeselection), including optional captured `plan_id`, `digest`, `saved_source_digest`, approval mode, executor/compaction controls, and comment. The special plan ID `last` is normalized to omitted selection. Supply a stable `operation_id` for retries. This is the `action = "execute"` form of [session.command.apply](#sessioncommandapply), defaulting to snapshot response with `task_operation`. Acceptance validates the selected artifact and current review state before scheduling execution. It does not report the execution finished. Stale digests, unavailable plans, invalid approval settings, and incompatible task transitions fail without selecting an unrelated plan.

## `session.plan.acceptance.begin`

Begin acceptance review through the shared review command.

- **Context:** Explicit existing session.
- **Input:** PlanReviewRequest fields: plan_id, digest, saved_source_digest, approval_mode, comment, annotations, and operation_id when supplied.
- **Result and effects:** Returns a session snapshot with task_operation and the acceptance-question state.
- **Boundary:** Preserve independent execution-revision review routing. Main-task review commits without acquiring provider execution. Preserve the shared task command contract and response variant.
- **Inspected source:** [task/command.rs](../../../rust/forge/crates/forge-harness/src/task/command.rs), [service/task_state.rs](../../../rust/forge/crates/forge-harness/src/service/task_state.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

Accepts [PlanReviewRequest](native-schemas.md#planreviewrequest) and optional stable `operation_id`. This is `action = "begin_acceptance"` in [session.command.apply](#sessioncommandapply), defaulting to a snapshot response. It captures the reviewed plan/digests and starts the acceptance lifecycle, including required questions. Review capture may supply the plan fields from an owned document input. Invalid or stale review identity fails. The resulting question state and receipt persist together; follow-up acceptance must retain that captured identity.

## `session.plan.acceptance.cancel`

Cancel pending acceptance review.

- **Context:** Explicit existing session.
- **Input:** PlanReviewRequest fields and operation_id when supplied.
- **Result and effects:** Returns a session snapshot with task_operation after updating the owned acceptance state.
- **Boundary:** Cancellation of review is separate from cancelling a provider turn or clearing the entire task. Preserve the shared task command contract and response variant.
- **Inspected source:** [task/command.rs](../../../rust/forge/crates/forge-harness/src/task/command.rs), [service/task_state.rs](../../../rust/forge/crates/forge-harness/src/service/task_state.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

Accepts [PlanReviewRequest](native-schemas.md#planreviewrequest) and optional stable `operation_id`. This is `action = "cancel_acceptance"` in [session.command.apply](#sessioncommandapply), defaulting to a snapshot response. It cancels the identified acceptance lifecycle without deleting the plan or claiming that execution completed. Stale or mismatched acceptance state fails. Replay returns the original receipt rather than cancelling a later lifecycle.

## `session.plan.cancel`

Cancel the owned Plan workflow or its independent revision review.

- **Context:** Explicit existing session.
- **Input:** PlanReviewRequest fields and operation_id when supplied.
- **Result and effects:** Returns a session snapshot with task_operation for the main-task command. Finalizes an owned settled waiting exchange when applicable.
- **Boundary:** Do not finalize an active provider exchange. Preserve the independent revision-review branch and its response contract. Preserve the shared task command contract and response variant.
- **Inspected source:** [task/command.rs](../../../rust/forge/crates/forge-harness/src/task/command.rs), [service/task_state.rs](../../../rust/forge/crates/forge-harness/src/service/task_state.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

Accepts [PlanReviewRequest](native-schemas.md#planreviewrequest) and optional stable `operation_id`. This is `action = "cancel_plan"` in [session.command.apply](#sessioncommandapply), defaulting to a snapshot response. It cancels the identified planning lifecycle while retaining its historical records. It inherits atomic state/receipt persistence and duplicate-ID recovery. Mismatched current plan or review identity fails rather than cancelling another plan.

## `session.plan.changes.request`

Commit requested Plan changes and any pending revision entry.

- **Context:** Explicit existing session.
- **Input:** PlanReviewRequest fields, including captured plan/digests, comment, annotations, and operation_id when supplied.
- **Result and effects:** Returns a session snapshot with task_operation for the main-task command. The coordinator later delivers required provider input.
- **Boundary:** Preserve captured review/version validation and independent revision-review routing. The command itself does not cancel or run the provider. Preserve the shared task command contract and response variant.
- **Inspected source:** [task/command.rs](../../../rust/forge/crates/forge-harness/src/task/command.rs), [service/task_state.rs](../../../rust/forge/crates/forge-harness/src/service/task_state.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

Accepts [PlanReviewRequest](native-schemas.md#planreviewrequest): captured plan/digests, optional comment and annotations, approval mode, and optional stable `operation_id`. This is `action = "request_changes"` in [session.command.apply](#sessioncommandapply), defaulting to a snapshot response. It validates the captured review and commits requested revision work. Provider revision runs afterward through input delivery. Stale review data, unavailable artifacts, or incompatible task state fail. Preserve the operation identity across an uncertain response.

## `session.plan.check.update`

Update the reviewed plan's selected checks.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** The plan check-selection update.
- **Result and effects:** Returns the updated plan and selection state.
- **Boundary:** This does not execute the checks.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

Requires `plan_id`, current review `digest`, `expected_version`, and `enabled` (check-ID to boolean map). Optional `tests` is an array of [DesignTestSelection](runtime-schemas.md#designtestselection) to remove. Only a plan awaiting review in this session can change. Returns `{ "plan": ..., "content": ..., "document": ... }`, updates the saved version/digest, clears pending acceptance, appends a manual-change record, and publishes `plan_checks_updated`. Stale digest/version, invalid selections, or persistence/validation failure rejects the edit. It does not run the selected checks.

## `session.plan.declaration.get`

Resolve a source declaration referenced by a plan.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** The plan/declaration navigation target.
- **Result and effects:** Returns declaration/navigation data.
- **Boundary:** Generated plan coordinates must resolve against the recorded source identity.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

Requires `plan_id`, `path`, and submitted `revision` (unsigned 32-bit integer). `baseline` defaults to false; `document` defaults to false. For declarations, selects the baseline or proposed source at that revision. With `document: true`, path selects an overview section by title. Returns `{ "text": ..., "path": ..., "revision": ..., "baseline": ... }`. This is read-only and does not select/resume the plan task. Wrong session, missing revision/design/path, or unknown overview section fails.

## `session.plan.entity.rename`

Rename an entity in the reviewed plan model.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** The captured entity and replacement-name payload.
- **Result and effects:** Returns the updated plan projection and revision.
- **Boundary:** Validate the current plan/source identity and preserve unrelated draft content.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

Requires `plan_id` and nonempty new `name`. Supply either `symbol` for a declaration identity or `entity_name` for a planned added entity. A symbol rename requires current `expected_version`; optional `digest` validates the reviewed and working content. Returns `{ "plan": ..., "content": ..., "document": ..., "previous_name": ..., "name": ... }`, updates review identity, clears acceptance, and publishes `plan_entity_renamed`. Only a plan awaiting review can change. Missing entity/symbol, stale version/digest, invalid rename, and persistence failure reject the edit. This edits the planned declaration, not workspace source.

## `session.plan.get`

Read a selected saved plan.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** plan_id and optional revision.
- **Result and effects:** Returns the requested plan representation.
- **Boundary:** Selecting an old revision does not make it the current editable plan.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

Requires `plan_id` (string). Optional `revision` is a positive integer identifying a historical model revision. Returns [PlanRecord](runtime-schemas.md#planrecord). A historical result substitutes the captured title, working path, document version, model revision, and review digest, and adds `historical_revision`. Missing artifacts, plans owned by another session, plans without submitted reviews, and unavailable revisions fail. Reading does not select a task or resume execution and emits no progress.

## `session.plan.list`

List plans available for replanning/review.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** An empty parameter object.
- **Result and effects:** Returns the plan-choice inventory.
- **Boundary:** Listing does not accept a plan.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

No parameter fields. Returns an array of [PlanChoice](runtime-schemas.md#planchoice) for this session. Each choice identifies the plan, submitted digest if present, title, creation time, model revision count, current plan state, session name, and whether a completed execution exists. Empty results are valid. It does not select, approve, execute, or mutate a plan and emits no progress.

## `session.plan.question.answer`

Answer a question about a reviewed plan.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** The captured plan-question target and answer.
- **Result and effects:** Returns the coordinator's answer/result updates.
- **Boundary:** The active question/plan revision must still match the captured target.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

Requires `review` ([DocumentInput](repository-schemas.md#documentinput)), `draft_source_digest`, and `draft_annotation` (captured review annotations). It first saves annotations against that source digest, then collects questions owned by the captured review. Returns `{ "annotation": [...] }` after all selected questions have been answered. Nonempty question work creates a same-provider native fork, waits for fork readiness, and uses the provider-local Ask model selection or inherited planning model. The answer session has write access disabled and must submit through the dedicated answer tool. Stale review/source identity, unsupported native fork, unavailable model, provider failure, or invalid answer fails. The source session does not receive a new chat exchange for these questions.

## `session.plan.test.delete`

Remove selected tests from the reviewed plan inventory.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** The captured test identities and plan edit context.
- **Result and effects:** Returns the updated plan projection/revision.
- **Boundary:** This edits the plan inventory, not arbitrary source files.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

Requires `plan_id`, current review `digest`, `expected_version`, and a nonempty `tests` array of [DesignTestSelection](runtime-schemas.md#designtestselection). Each selected test must exist in the captured plan inventory. Removes those tests from a plan awaiting review and returns `{ "plan": ..., "content": ..., "document": ... }`. It updates version/digest, clears pending acceptance, retains a manual-change record, and publishes `plan_tests_deleted`. This changes planned test metadata, not source test files. Wrong owner/state, stale review, unknown tests, or persistence failure is explicit.

## `session.preview`

Read a saved session preview.

- **Context:** Lifecycle: explicit target/source where applicable.
- **Input:** The selected session_id in the method payload.
- **Result and effects:** Returns preview data for that saved session.
- **Boundary:** Previewing does not by itself resume execution.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

Requires parameter `session_id` (string) identifying the saved session to preview. This target is distinct from the required calling-session envelope. Returns [SessionPreview](runtime-schemas.md#sessionpreview): rendered `lines`, saved `session`, and child-agent state. Its skipped native exchange/timeline collections are not serialized. Missing saved sessions fail. Reading the preview does not acquire the target's lease, select it, resume it, or invoke a provider. It emits no progress.

## `session.prompt.history.record`

Record a prompt-history entry.

- **Context:** Explicit existing session.
- **Input:** The prompt-history draft and its associated attachment information.
- **Result and effects:** Returns the updated history representation.
- **Boundary:** This records history and does not itself submit a provider turn.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

Accepts [PromptDraft](runtime-schemas.md#promptdraft): text plus its attachment metadata. Returns the saved prompt-history array after recording server time. This writes local reusable draft history, not a session input submission, provider turn, or steering message. It emits no progress and does not require waiting for unrelated provider completion before its callback establishes persistence.

## `session.prompt.submit`

Submit a prompt through the retained direct-prompt service.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** The prompt input and supported submission/configuration fields.
- **Result and effects:** Returns the submission/session result and provider events.
- **Boundary:** Ordinary Lua submission uses input.apply. Recognized task text is normalized before provider admission and returns its command response, while ordinary text retains prompt submission behavior. Retain this endpoint only if the usage audit demonstrates a caller.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

Requires nonempty `text` and accepts attachment selections in [PromptDraft](runtime-schemas.md#promptdraft). `operation_id` supplies stable identity for command normalization. User task commands such as `/goal` and `/plan` normalize to canonical session actions before provider admission; their response acknowledges the task transition, and the input coordinator delivers its pending entry separately. Ordinary text starts an exchange and returns [BrokerSnapshot](runtime-schemas.md#brokersnapshot) when that interaction settles. Paused or not-yet-entered tasks do not turn ordinary chat into task continuation.

The native coordinator also accepts `queued: true` for owned queued delivery and internal prompt-admission metadata. UI callers should use [session.input.apply](#sessioninputapply) for queued text, preserving message/document identity rather than constructing that internal metadata. Provider events and document updates can precede the terminal response. Attachment, ownership, admission, provider, and finalization errors remain explicit. A response failure is not evidence that no durable input or partial provider output exists.

## `session.provider.authenticate`

Run the chosen provider authentication method.

- **Context:** Explicit existing session/provider.
- **Input:** method identifying an offered authentication action.
- **Result and effects:** Returns authentication completion status.
- **Boundary:** Authentication can require external interaction and is admitted separately from active work.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

Requires `method` (provider authentication method string). Returns `{ "authenticated": true }` after the selected provider's authentication flow completes. It uses this session's captured catalog request, may launch interactive/provider authentication work, and invalidates cached MCP discovery afterward. Unsupported methods, cancellation, provider failures, and missing credentials are errors. It does not submit a user prompt or grant unrelated command permissions.

## `session.provider.capabilities.get`

Read the selected provider's capability manifest.

- **Context:** Explicit existing session/provider.
- **Input:** The explicit session context.
- **Result and effects:** Returns supported provider capabilities.
- **Boundary:** A capability listing is not permission to execute a tool.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

No parameter fields. Returns [CapabilityManifest](runtime-schemas.md#capabilitymanifest), containing backend identity/label, schema revision, optional provider version, runtime availability/reason, notices, and capability entries. Each capability reports support independently of current provider availability. Discovery has a 30-second service deadline and may contact the provider. It does not run a user prompt or emit request progress. Unsupported operations must remain visible as unsupported rather than being inferred from a missing menu item.

## `session.provider.mcp.list`

Read MCP configuration/capabilities in the selected provider context.

- **Context:** Explicit existing session/provider.
- **Input:** The explicit session/provider context.
- **Result and effects:** Returns the MCP catalog/status.
- **Boundary:** Listing is distinct from changing settings or calling a server tool.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

No parameter fields. Returns an array of [McpDefinition](runtime-schemas.md#mcpdefinition), including each server's normalized state and available tool metadata. The session caches discovery under an MCP revision, which setup/authentication and resource changes invalidate. Server configuration can appear before handshake completion. Disabled, loading, connected, authentication-required, failed, and unavailable states are distinct. It may contact the provider or its servers but does not invoke a tool, mutate configuration, or emit request progress. Apply the result only to the originating session/provider generation.

## `session.provider.mock_response`

Control a test provider's response barrier.

- **Context:** Explicit existing session/provider.
- **Input:** A mock-response command such as hold, wait, release, or cancel, with its matching identity.
- **Result and effects:** Returns the command-specific mock-control result.
- **Boundary:** Only the mock provider supports this test interface.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

A live integration-control endpoint available only for the mock provider. `operation: "hold"` accepts optional prompt `mode` and `any_session` (false by default), then returns a new hold `id`. `wait` requires that ID and waits until a matching provider prompt arrives, returning `id`, provider `session_id`, `mode`, and full `prompt`. `release` requires an arrived hold; `cancel` can discard an armed or arrived hold. Both return `id`, `released`, and `cancelled`. IDs belong to the selected session even when matching allows any provider session. Unknown, released, cancelled, or unarrived holds fail according to the operation. These barriers affect test provider responses, not production providers.

## `session.provider.model.list`

List models available through the selected session's provider.

- **Context:** Explicit existing session/provider.
- **Input:** The explicit session context.
- **Result and effects:** Returns the provider model catalog.
- **Boundary:** Provider discovery can fail or exceed its discovery deadline.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

No parameter fields. Returns an array of [BackendModel](runtime-schemas.md#backendmodel), with provider-advertised reasoning/context controls and saved workspace preferences applied. Discovery may contact or launch the configured provider and has a 30-second service deadline. Resolving a default selection can emit session events before the response. No prompt or request-progress stream is produced. Catalog failure is explicit, not an empty-success fallback.

## `session.provider.setup`

Inspect the installed provider integration.

- **Context:** Explicit existing session/provider.
- **Input:** The explicit session/provider context.
- **Result and effects:** Returns setup outcome.
- **Boundary:** Provider inspection performs a handshake without installing software or creating a conversation.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

Calls the selected provider's inspection operation and returns runtime availability, separate native CLI and ACP adapter versions, and authentication methods. It never installs software. The operation requires idle source execution and no pending cleanup, and invalidates MCP discovery after success. Unsupported providers, active work, and handshake failures are errors. A missing adapter returns unavailable status. It does not send a chat prompt.

## `session.provider.skill.list`

Read skills available to the selected provider context.

- **Context:** Explicit existing session/provider.
- **Input:** The explicit session/provider context.
- **Result and effects:** Returns the skill catalog.
- **Boundary:** Listing does not toggle skills or run them.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

No parameter fields. Returns an array of [SkillDefinition](runtime-schemas.md#skilldefinition): name, description, enabled/user-invocable flags, optional resource binding/scope, path, source, and argument hint. Empty supported catalogs return `[]`. Unsupported skill discovery fails explicitly with `unsupported provider capability: skill_catalog`. The discovery deadline is 30 seconds. It may contact the selected provider but does not invoke a skill, change its enabled state, or emit request progress.

## `session.question.answer`

Commit an answer to a pending question.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** The question target, answer payload, and ordering/operation identity.
- **Result and effects:** Returns the updated question/session state.
- **Boundary:** Reject a target that no longer names the active question revision.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

Requires `question_id` and a typed [PlanQuestionResponse](runtime-schemas.md#planquestionresponse). For durable UI answers, also supply `operation_id` and `target` ([QuestionTarget](runtime-schemas.md#questiontarget)); optional `answer_order` is a `[writer, sequence]` pair. The target captures owner kind, owner ID, question-set ID, and revision. Returns `{ "operation_id": ..., "active_elicitation": ... }` after the answer transaction commits, then publishes `question_updated`. Exact replay returns the retained receipt; identity reuse with different content fails. Older answers from the same writer do not overwrite a newer sequence. The legacy no-target path returns a full session snapshot after answering the current elicitation. It does not continue provider work. Missing/stale owners, invalid choices, and persistence failures remain errors.

## `session.question.ask`

Submit a question through the session's question workflow.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** The question text and supported context.
- **Result and effects:** Returns the resulting question/workflow state.
- **Boundary:** This can involve provider work; it is not a local string lookup.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

Requires `question_id` and nonempty `text`. The ID must identify the pending question or its containing set. This records clarification input under that elicitation and runs a provider clarification turn, returning [BrokerSnapshot](runtime-schemas.md#brokersnapshot) after settlement. It can update or withdraw questions and, when no decision remains, consume the clarification and continue. It does not accept clarification against plan-acceptance choices. Missing ownership, mismatched questions, active-work admission, and provider failures remain explicit.

## `session.question.continue`

Continue after required answers have committed.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** Optional captured question target with owner, id, set_id, and revision.
- **Result and effects:** Returns continuation/session state.
- **Boundary:** The handler rejects changed targets and incomplete required answers. Acceptance completion first resolves a prior receipt or captures the Plan ID and review digest, then commits CompleteAcceptance without provider admission. Other question continuations retain their provider-execution requirements.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

Accepts optional captured `target` ([QuestionTarget](runtime-schemas.md#questiontarget)) and stable `operation_id`. The target must still identify the active elicitation and all required answers must have committed. For plan acceptance, normalization routes through the canonical acceptance command and returns its durable receipt. For an ordinary plan/interaction question set, it consumes the committed feedback and runs the corresponding provider continuation, returning the resulting snapshot. Continuation is a separate action from saving an answer. Missing answers, stale identity/revision, failed admission, and provider errors fail explicitly; already consumed feedback is not made pending again merely because a later provider turn fails.

## `session.question.skip`

Skip an explicitly targeted pending question.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** The question target and ordering/operation identity.
- **Result and effects:** Returns the updated question/session state.
- **Boundary:** Skipping is an explicit answer-state mutation.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

Requires `question_id`; the durable form also supplies `operation_id` and the captured [QuestionTarget](runtime-schemas.md#questiontarget). It follows [session.question.answer](#sessionquestionanswer) with response `{ "kind": "skipped" }`, returning the same receipt/elicitation shape and event. Skipping commits an explicit answer state; it does not implicitly continue the provider. A question that disallows skipping or a stale owner/revision fails.

## `session.rename`

Change a session's saved display name.

- **Context:** Lifecycle: explicit target/source where applicable.
- **Input:** The target session identity and new name.
- **Result and effects:** Returns the renamed session information.
- **Boundary:** Renaming does not change the session's stable ID.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

Requires `session_id` (string) equal to the calling-session envelope and `name` (string). The service trims the name. Optional `expected_name` (string) fences an edit against a concurrent rename. Returns the updated [HarnessSession](runtime-schemas.md#harnesssession), including its new update timestamp. The metadata write validates the owning lease and fails on a mismatched target, lost owner, or changed expected name. It emits no provider prompt or progress. A retry without `expected_name` is a new unconditional metadata write.

## `session.resume`

Open an explicitly identified saved session.

- **Context:** Lifecycle: explicit target/source where applicable.
- **Input:** The target session_id and supported resume options.
- **Result and effects:** Returns the resumed session snapshot.
- **Boundary:** Unknown sessions and ownership/lease conflicts are explicit failures.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

Requires the saved target `session_id` in parameters and explicit outer context for routing. Returns [BrokerSnapshot](runtime-schemas.md#brokersnapshot) from the target controller, resolving it from durable storage when necessary. It does not issue a provider prompt or change unrelated controllers. Unknown sessions, conflicting ownership/leases, and incompatible runtime configuration fail. The caller adopts the returned identity/epoch only if it still owns the original UI request.

## `session.state.get`

Read the current session snapshot.

- **Context:** Explicit existing session.
- **Input:** An empty parameter object.
- **Result and effects:** Returns session state for reconciliation and presentation.
- **Boundary:** Do not apply a stale snapshot over newer local input.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

No parameter fields. Returns [BrokerSnapshot](runtime-schemas.md#brokersnapshot), including the session, current task/plan, capabilities, approval/question state, runtime epoch, and monotonic snapshot/timeline revisions. The skipped native `exchange` and `timeline` fields are not part of this result. Reads current in-memory admission state without starting a provider turn or taking another session's lease. No request progress is emitted. Reject snapshots from an older epoch or lower revision before updating the buffer.

## `session.task.get`

Read the session's optional active task.

- **Context:** Explicit existing session.
- **Input:** An empty parameter object.
- **Result and effects:** Returns one serialized ActiveTask or null, never a list of historical task records.
- **Boundary:** This reads HarnessSession.active_task without selecting or resuming anything. Exchanges do not supply task authority.
- **Inspected source:** [broker/observation.rs](../../../rust/forge/crates/forge-harness/src/broker/observation.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

No parameter fields. Returns the selected session's [ActiveTask](runtime-schemas.md#activetask), or JSON null when no task is selected. Completed and paused states remain distinguishable in the task value. This is a read, not task resumption. It emits no progress and does not invoke the provider.

## `session.trace.clear`

Clear the selected session's trace records.

- **Context:** Explicit existing session.
- **Input:** The explicit session context.
- **Result and effects:** Clears the selected session log and its three rotated segments, then returns its trace status.
- **Boundary:** Other session records retain their own ownership.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

### Contract

No parameter fields. Clears the selected session's trace file and returns [TraceStatus](runtime-schemas.md#tracestatus): effective enabled state, path, and whether tracing was configured. This changes diagnostic storage only, not the session transcript or other sessions' logs. Filesystem failure is explicit. Use the returned path rather than inferring it from the current directory.

## `session.turn.cancel`

Cancel the selected session's current execution.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** The explicit session context and accepted control fields.
- **Result and effects:** Returns cancellation/control state.
- **Boundary:** Cancellation must not be replayed as a new prompt.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `control`. Recovery: `session_command`.

### Contract

Accepts optional root `target` and `restore_prompt_if_no_output` (boolean, default false). It validates root control ownership, quiesces managed children, cancels root approvals, and collects provider cleanup before acknowledging cancellation. Returns `{ "cancel_requested": true }` for an active provider turn, or the settled broker cancellation result when already idle. Prompt restoration only applies when the turn can be retracted before output. Cleanup failure remains an error and prevents pretending the provider stopped. Pausing the selected task/input queue is a separate state action used by the UI before cancellation.

## `session.turn.restart`

Restart work through the session coordinator.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** The restart/control payload accepted by the coordinator.
- **Result and effects:** Returns the new execution/control state.
- **Boundary:** Preserve task ownership and operation identity across the restart boundary.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `control`. Recovery: `session_command`.

### Contract

Accepts optional `mode`, which is echoed in `{ "restart_requested": true, "mode": ... }`. It quiesces children, cancels root approvals, and requests restart after provider cleanup. A cleanup failure is returned and retained instead of admitting overlapping provider work. This acknowledges restart scheduling, not a completed replacement turn. The session and provider generation remain the ownership boundary for later events.

## `session.turn.steer`

Steer active execution through the coordinator.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** The steering text and accepted control fields.
- **Result and effects:** Returns steering admission/result state.
- **Boundary:** Reject child targets before interpreting task syntax. Recognized task text uses shared command normalization before provider steering and returns the command result. Ordinary steering retains its accepted/unaccepted cancellation semantics.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

### Contract

Requires nonempty `text`, optional root `target`, and optional attachment input in the same shape as [session.prompt.submit](#sessionpromptsubmit). The service resolves attachments and persists a steering submission before delivering through the active parent-input boundary or provider-native steering. Returns `{ "steered": true, "delivery": "direct" }` when delivery is accepted. This is not a new completed exchange. Wrong target, missing attachment, unavailable steering, or provider rejection fails. A submitted goal/plan command should use the canonical task action rather than being treated as literal steering text.

