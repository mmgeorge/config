# Provider-specific boundary leaks

The current boundary still requires Harness to understand native provider behavior. This inventory records the objects that `forge-providers` exposes, the native objects defined outside that crate, and the remaining provider-name knowledge in shared code.

The required boundary is stricter than SDK dependency isolation: **only `forge-providers` should define or name Copilot, Codex, Claude, Antigravity, or ACP implementation concepts in Forge code.** Other components should select providers through normalized identity and descriptor data, supply host callbacks, and consume normalized operations and events.

[The normalized contract specification](leaks-plan.md#normalized-contract-specification) defines the replacement contracts used by the required fixes below. These are implementation requirements, not claims that the current code already satisfies them.

**Every outward event, result, error, callback request, capability, and descriptor must be fully normalized before leaving `forge-providers`.** Persistent Background work must keep its own maintained session and event stream; it must not switch to isolated `generate_text`. Isolated generation is one function returning a value or `ProviderError`, with native waiting, validation, and cleanup inside the provider crate.

The provider crate must produce all native stream completion through normalized execution events, without a completion-certificate side channel. It must correlate supported native signals itself, not reconstruct history merely to satisfy the old full-ancestry certificate.

## Scope and evidence

This inventory describes the working tree inspected on 2026-11-02, including the uncommitted provider repairs. It covers the Rust workspace under `nvim/rust/forge`, Forge Lua integration, and their tests; unrelated dotfile configuration for the same CLIs is outside its scope.

The lists distinguish **exposure**, where a provider-specific object is publicly reachable from `forge-providers`, from **misplaced ownership**, where another crate defines native behavior itself. A public object is an exposure even if only internal code currently uses it. Test-only usage and naming-only dependencies remain findings under the strict provider-noun requirement, but they do not establish production failures.

Paths in the tables are repository-relative. Symbols grouped in one row share the linked defining file; a file's line anchor identifies the first listed declaration or the relevant implementation.

Each finding's **Required fix** names the target owner and replacement behavior. "Provider owner" below means a private implementation in `forge-providers`; "API" means provider-neutral contracts in `forge-provider-api`. Reuse existing [`ProviderEvent`](nvim/rust/forge/crates/forge-provider-api/src/event.rs), [`ProviderEventStream`](nvim/rust/forge/crates/forge-provider-api/src/events.rs), [`ExecutionUpdate` and `ExecutionResult`](nvim/rust/forge/crates/forge-provider-api/src/lifecycle.rs), [`MessageInput` and `FileAttachment`](nvim/rust/forge/crates/forge-provider-api/src/input.rs), [`TextGenerationRequest`](nvim/rust/forge/crates/forge-provider-api/src/generation.rs), and [`ProviderError`](nvim/rust/forge/crates/forge-provider-api/src/error.rs) where they express the normalized contract. Reuse does not excuse a native payload or host-interpreted native status inside a generic type; required contract extensions are specified in the linked plan.

The source searches used for this inventory are reproducible:

```sh
rg -n -i 'copilot|codex|claude|antigravity|acp' \
  nvim/rust/forge -g '*.rs' -g '*.toml'

rg -n -i 'copilot|codex|claude|antigravity|acp' \
  nvim/lua/forge nvim/tests -g '*.lua'

rg -n '^[[:space:]]*(pub(\([^)]*\))? )?(struct|enum|trait|type) ' \
  nvim/rust/forge/crates/forge-providers/src \
  nvim/rust/forge/crates/forge-harness/src/provider

rg -n '^pub (async )?(fn|struct|enum|trait|type|mod|use)' \
  nvim/rust/forge/crates/forge-providers/src -g '*.rs'
```

Name searches alone are insufficient. Objects such as `ParentTurn`, `StopRequest`, `ReceivedMessage`, and `PromptEvidence` carry native semantics without naming a provider, so this inventory also follows public module exports, callback signatures, and their consumers.

## Public provider-specific objects defined inside forge-providers

The crate publicly exports its `acp`, `codex`, and `copilot` implementation namespaces from [`lib.rs`](nvim/rust/forge/crates/forge-providers/src/lib.rs#L3). These namespaces make the following types reachable outside the crate instead of keeping them behind a normalized provider interface.

All rows in this section are **defined inside `forge-providers`, but publicly exposed**. Public inherent methods and public fields on these objects are part of the same exposure, not separate ownership boundaries.

### Copilot objects

The public Copilot surface includes SDK wrappers, runtime and session owners, decoder state, and independent lifecycle controls. Harness consumes several of these objects together, which makes the caller responsible for preserving their ordering.

| Public object, relative to `forge_providers::copilot` | Defining file | Exposed responsibility | Required fix |
| --- | --- | --- | --- |
| `handle::CopilotClient`, `handle::CopilotSession`, `handle::SkillSelection` | [`handle.rs:19`](nvim/rust/forge/crates/forge-providers/src/copilot/handle.rs#L19) | Client/session handles and native skill selection; session methods expose `abort`, `disconnect`, `client_stopped`, `rewind`, sends, and steering independently. | Keep handles and skill resolution in the provider owner; accept normalized session/input operations and publish acceptance, output, and completion without caller-ordered SDK methods. |
| `runtime::CopilotRuntime` | [`runtime.rs:34`](nvim/rust/forge/crates/forge-providers/src/copilot/runtime.rs#L34) | Runtime admission, client acquisition, stream attachment, recovery, shutdown, and SDK future-size diagnostics. | Make the provider owner manage runtime admission, recovery, and shutdown; expose normalized runtime status, `ProviderError`, and a maintained `ProviderEventStream`, not client acquisition. |
| `sessions::SessionReuse`, `sessions::ClientShutdown`, `sessions::SessionSelection`, `sessions::PreparedSession<P>`, `sessions::CopilotSessions<P>` | [`sessions.rs:21`](nvim/rust/forge/crates/forge-providers/src/copilot/sessions.rs#L21) | Native binding policy, caller-supplied shutdown certainty, preparation, resource leases, submission marking, readers, retirement, and clearing. | Let the provider owner decide binding reuse, reader retirement, and shutdown certainty; Harness supplies admitted resource leases and normalized owner identity, never native preparation recipes. |
| `session::ResourceDiscovery`, `session::SessionOptions` | [`session.rs:14`](nvim/rust/forge/crates/forge-providers/src/copilot/session.rs#L14) | Native discovery mode, context tier, excluded native tools, and SDK session preparation choices. | Translate normalized resource/model/tool policy into these private options inside the provider owner; report unsupported choices through normalized capabilities or `ProviderError`. |
| `turn::CopilotRetraction`, `turn::CopilotTurnEvent`, `turn::StopRequest`, `turn::CopilotTurnEventGuard` | [`turn.rs:18`](nvim/rust/forge/crates/forge-providers/src/copilot/turn.rs#L18) | Send admission, input coverage, native event ancestry, foreground anchors, idle settlement, stop sequencing, and history retraction. | Keep correlation, cancellation, and retraction private; the provider owner emits `ExecutionUpdate` with normalized submission coverage, without host certificates or history reconstruction for missing ancestry. |
| `prompt::TurnOptions`, `prompt::TurnResult` | [`prompt.rs:16`](nvim/rust/forge/crates/forge-providers/src/copilot/prompt.rs#L16) | Native prompt assembly, repair caution, compaction-observer knowledge, and denial-response bookkeeping. | Accept normalized prompt input; the provider owner handles repair/denial state and emits execution results on the session stream, not native completion instructions in a return value. |
| `event_decoder::CopilotEventDecoder` | [`event_decoder.rs:33`](nvim/rust/forge/crates/forge-providers/src/copilot/event_decoder.rs#L33) | Raw SDK event decoding, task snapshots, and explicit success/failure finalization. | Keep decoding and finalization inside the provider owner; expose only fully normalized `ProviderEvent` values, including execution outcomes and observation loss. |
| `event_stream::SessionEventStream` | [`event_stream.rs:26`](nvim/rust/forge/crates/forge-providers/src/copilot/event_stream.rs#L26) | SDK subscription creation, native event filtering, and receipt of raw SDK events. | Keep the SDK subscription private and continuously owned by the provider session; callers receive `ProviderEventStream`, including during Background and idle periods. |
| `message_state::CopilotPhase`, `message_state::CopilotPhaseMetadata`, `message_state::CopilotMessageState`, `message_state::CopilotMessageStore` | [`message_state.rs:6`](nvim/rust/forge/crates/forge-providers/src/copilot/message_state.rs#L6) | Native phase vocabulary, mutable publication/completion state, and fallback-response selection. | Resolve phases, fallback text, and publication state inside the provider owner; expose `MessageUpdate` and normalized execution results, never mutable native message state. |
| `compaction::ManualCompactionMap`, `compaction::CompactionSubscription`, `compaction::CompactionMonitor` | [`compaction.rs:9`](nvim/rust/forge/crates/forge-providers/src/copilot/compaction.rs#L9) | Manual/native identity correlation and independent SDK compaction readers; the monitor publicly exposes its shared mutable correlation map. | Keep correlation maps and readers in the provider owner; accept a normalized compaction operation and publish `ProviderEvent::Compaction` without caller-managed native identities. |
| `terminal::CopilotTerminalMonitor` | [`terminal.rs:6`](nvim/rust/forge/crates/forge-providers/src/copilot/terminal.rs#L6) | Session-specific terminal observation and collection. | Keep monitor lifetime and collection inside the provider owner; return normalized `TerminalSnapshot` and terminal-operation errors. |
| `history_repair::HistoryRepair` | [`history_repair.rs:15`](nvim/rust/forge/crates/forge-providers/src/copilot/history_repair.rs#L15) | Native history-repair notice that Harness forwards into the next prompt. | Apply genuine repair notices and prompt adjustments inside the provider owner; publish a normalized diagnostic when needed, never require Harness to forward repair state or rebuild terminal ancestry. |
| `rejection::RejectionState` | [`rejection.rs:9`](nvim/rust/forge/crates/forge-providers/src/copilot/rejection.rs#L9) | Native denial observations and the requirement for a subsequent answer or host handoff. | Keep denial tracking in the provider owner; invoke normalized host permission/tool callbacks and publish the resulting normalized execution outcome without exposing denial sequencing. |
| `decode::DecodeError` | [`decode.rs:11`](nvim/rust/forge/crates/forge-providers/src/copilot/decode.rs#L11) | Provider-specific decoding failures rather than only normalized provider errors. | Convert decoding failures inside the provider owner to `ProviderError` for operations or normalized observation failure on the session stream; keep native error variants private. |

### Codex objects

The public Codex surface exposes a raw connection and a normalizer rather than an owner of a complete normalized execution. Harness therefore defines additional protocol and lifecycle owners around these objects.

| Public object, relative to `forge_providers::codex` | Defining file | Exposed responsibility | Required fix |
| --- | --- | --- | --- |
| `CodexConnectionError`, `ReceivedMessage`, `CodexConnection` | [`connection.rs:12`](nvim/rust/forge/crates/forge-providers/src/codex/connection.rs#L12), re-exported by [`mod.rs`](nvim/rust/forge/crates/forge-providers/src/codex/mod.rs#L3) | Socket lifecycle, encoded writes, raw JSON/parse errors, byte accounting, and native transport failure variants. | Keep transport and raw frames in the provider owner; expose normalized operations, `ProviderError`, and `ProviderEventStream`, including normalized progress rather than raw transport objects. |
| `runtime::CodexRuntime` | [`runtime.rs:45`](nvim/rust/forge/crates/forge-providers/src/codex/runtime.rs#L45) | App-server process ownership, raw connection creation, process identity, and shutdown. | Let the provider owner launch and collect processes; return normalized runtime descriptors and shutdown outcomes without exposing connections or native process controls. |
| `security::CodexSecurity` | [`security.rs:6`](nvim/rust/forge/crates/forge-providers/src/codex/security.rs#L6) | Mutating caller-owned JSON into native thread and turn approval/sandbox payloads. | Accept API access/approval policy and encode it privately in the provider owner; Harness must not supply native thread or turn JSON. |
| `normalization::CodexNormalization` | [`normalization.rs:14`](nvim/rust/forge/crates/forge-providers/src/codex/normalization.rs#L14) | Raw message normalization, retained/background ownership, interruption publication, and publicly mutable `replay_sensitive`. | Keep normalization and replay-safety decisions in the provider owner; emit normalized events and explicit retry eligibility instead of allowing callers to mutate decoder state. |
| `protocol::McpContext` | [`protocol.rs:9`](nvim/rust/forge/crates/forge-providers/src/codex/protocol.rs#L9) | Native MCP aliases and call metadata that the host must bind and normalize. | Resolve native aliases and call identity inside the provider owner before invoking normalized tool/trace callbacks; host code receives canonical tool identity only. |
| `approval::ApprovalContext`, `approval::PreparedApproval` | [`approval.rs:26`](nvim/rust/forge/crates/forge-providers/src/codex/approval.rs#L26) | Native file-proposal observations and caller-coordinated proposal validation before publishing approval. | The provider owner decodes proposals, revalidates native proposal identity/revision before approval publication, and encodes decisions; Harness retains host authority and policy behind normalized permission callbacks. |
| `decode::CodexToolSnapshot` | [`decode.rs:11`](nvim/rust/forge/crates/forge-providers/src/codex/decode.rs#L11) | Intermediate native tool decoding rather than the final shared event contract. | Finish tool decoding inside the provider owner and emit `ToolUpdate`/`ToolInvocation`; intermediate snapshots remain private. |

### ACP, Claude, and Antigravity objects

ACP's public conversation object still exposes its native session state and arbitrary method requests. Claude and Antigravity share these ACP objects; neither has a separate public Rust implementation type.

| Public object, relative to `forge_providers::acp` | Defining file | Exposed responsibility | Required fix |
| --- | --- | --- | --- |
| `AcpConnectionError`, `AcpHandler`, `AcpLaunch`, `AcpConnection` | [`connection.rs:14`](nvim/rust/forge/crates/forge-providers/src/acp/connection.rs#L14), re-exported by [`mod.rs`](nvim/rust/forge/crates/forge-providers/src/acp/mod.rs#L4) | Process launch, native method-name/JSON callbacks, arbitrary requests, cancellation, and transport-specific errors. | Keep transport callbacks and launch details in the provider owner; expose typed normalized operations, host callbacks, and `ProviderError`, never method-name/JSON dispatch. |
| `runtime::RuntimeStatus`, `runtime::AcpRuntime` | [`runtime.rs:12`](nvim/rust/forge/crates/forge-providers/src/acp/runtime.rs#L12) | Adapter resolution, launch command/environment, managed-installation knowledge, and runtime version discovery. | Let the provider owner resolve existing adapters and normalize setup/version results; callers consume typed runtime status and descriptors rather than native launch recipes. Do not introduce automatic runtime installation. |
| `conversation::ConversationOptions`, `conversation::AcpConversation` | [`conversation.rs:13`](nvim/rust/forge/crates/forge-providers/src/acp/conversation.rs#L13) | Native resource/configuration construction, raw requests, prompt/steering responses, and access to `Arc<AcpSessionState>`. | Keep conversation/configuration state private; the provider owner accepts normalized session, resource, and steering requests and returns acceptance/errors plus normalized events. |
| `session::AcpSessionState` | [`session.rs:12`](nvim/rust/forge/crates/forge-providers/src/acp/session.rs#L12) | Caller-driven begin/finish, compaction lifecycle, event emission, native catalog JSON, and completion extraction. | Move begin/finish and completion production into the provider owner; expose `ExecutionUpdate`, `Compaction`, and normalized catalogs without state access or a completion-extraction side channel. |
| `submission::AcpSubmission`, `submission::SteeringAcceptance` | [`submission.rs:11`](nvim/rust/forge/crates/forge-providers/src/acp/submission.rs#L11) | Native prompt/steering operation selection and protocol-specific steering observability. | Select native submission operations privately; the provider owner maps acceptance, rejection, and uncertain delivery to normalized receipts/errors and session events. |
| `children::AcpChildren` | [`children.rs:10`](nvim/rust/forge/crates/forge-providers/src/acp/children.rs#L10) | Native child lifecycle, session-to-turn addressing, and child finalization. | Keep child correlation/finalization in the provider owner; publish normalized agent and execution events while Harness retains task authority through callbacks. |

## Public helper entry points

Hiding only the types above would leave provider semantics reachable through free functions and public modules. The following list covers the public free-function entry points in those implementation namespaces, including helpers with generic-looking names.

These are **defined inside `forge-providers`, but exposed outside it**. Their implementation can remain native; callers should reach their behavior through normalized provider operations rather than invoke these helpers directly.

| Defining module | Exposed helper names | Boundary content | Required fix |
| --- | --- | --- | --- |
| [`copilot/catalog.rs`](nvim/rust/forge/crates/forge-providers/src/copilot/catalog.rs#L4) | `model_descriptor`, `order_models` | SDK model conversion and provider-specific catalog ordering. | The provider owner converts and orders models internally; model-list callers receive only normalized `ProviderModel` values. |
| [`copilot/control.rs`](nvim/rust/forge/crates/forge-providers/src/copilot/control.rs#L7) | `tool_list` | Host tool definitions converted into SDK `Tool` objects. | Keep SDK tool construction inside the provider owner; Harness supplies normalized `ProviderTools` definitions and execution callbacks. |
| [`copilot/decode.rs`](nvim/rust/forge/crates/forge-providers/src/copilot/decode.rs#L16) | `decode_compaction`, `timestamp_ms`, `session_error`, `provider_change_set`, `checklist_update`, `agent_lifecycle`, `string_field`, `unsigned_field`, `is_denied` | Native event/field decoding and supporting helpers. | The provider owner performs field extraction/classification and emits normalized `ProviderEvent`, change data, or `ProviderError`; external code must not call partial decoders. |
| [`copilot/file_change.rs`](nvim/rust/forge/crates/forge-providers/src/copilot/file_change.rs#L6) | `from_output` | Native tool-output diff parsing. | Parse output in the provider owner and deliver `ProviderChangeSet` through normalized tool/permission contracts; Harness retains change policy. |
| [`copilot/handle.rs`](nvim/rust/forge/crates/forge-providers/src/copilot/handle.rs#L801) | `default_model_descriptor`, `model_options` | Default model construction and SDK `SetModelOptions` creation. | The provider registry supplies normalized model defaults; the provider owner privately translates selected model/reasoning values into SDK options. |
| [`copilot/history_repair.rs`](nvim/rust/forge/crates/forge-providers/src/copilot/history_repair.rs#L25) | `repair` | Native session-history inspection and repair. | Keep explicit repair/recovery inside the provider owner with normalized diagnostics/errors; never expose history traversal as a caller prerequisite for completion. |
| [`copilot/input.rs`](nvim/rust/forge/crates/forge-providers/src/copilot/input.rs#L5) | `message` | Normalized input converted into SDK `MessageOptions`. | Accept `MessageInput` at the provider boundary and construct/consume SDK options internally, without returning them to Harness. |
| [`copilot/mcp.rs`](nvim/rust/forge/crates/forge-providers/src/copilot/mcp.rs#L13) | `start`, `launch_request` | Independent native MCP startup and SDK request construction. | The provider owner starts admitted normalized resources and returns `McpDefinition` status/errors; SDK launch requests remain private. |
| [`copilot/mock_stream.rs`](nvim/rust/forge/crates/forge-providers/src/copilot/mock_stream.rs#L5) | `replay` | Provider-specific mock decoding exposed in the normal library surface. | Move native replay behind provider-owned test support; host mocks inject normalized `ProviderEvent` values rather than replay native wire messages. |
| [`copilot/permission.rs`](nvim/rust/forge/crates/forge-providers/src/copilot/permission.rs#L7) | `permission_from_copilot`, `handler`, `checkpoint_hooks` | Wire permission parsing and SDK callback/hook trait objects. | The provider owner installs SDK hooks and converts both directions; Harness implements normalized permission/checkpoint callbacks without receiving SDK trait objects. |
| [`copilot/prompt.rs`](nvim/rust/forge/crates/forge-providers/src/copilot/prompt.rs#L35) | `run`, `completes_exchange` | Native prompt execution and SDK event classification. | Keep execution and terminal classification in the provider owner; normalized prompt returns acceptance/error and its session stream carries `ExecutionUpdate`, not a caller-classified completion. |
| [`copilot/retry.rs`](nvim/rust/forge/crates/forge-providers/src/copilot/retry.rs#L4) | `transient_code` | Native retry classification. | Classify native codes and replay safety in the provider owner; expose normalized retry eligibility/errors so Harness never matches native codes. |
| [`copilot/session.rs`](nvim/rust/forge/crates/forge-providers/src/copilot/session.rs#L37) | `open` | Native session creation/resumption outside a complete provider owner. | The provider owner creates/resumes sessions and retains their streams; callers initialize through normalized session operations and receive `ProviderSessionId` or `ProviderError`. |
| [`copilot/text_generation.rs`](nvim/rust/forge/crates/forge-providers/src/copilot/text_generation.rs#L9) | `generate` | An isolated native generation implementation selected by callers through its provider-specific namespace. | Dispatch normalized `TextGenerationRequest` through `generate_text`; the provider owner waits and cleans up before one string/error return, while Background keeps its maintained session. |
| [`copilot/tool_semantics.rs`](nvim/rust/forge/crates/forge-providers/src/copilot/tool_semantics.rs#L5) | `copilot::normalize_invocation` | Provider-native tool semantics, re-exported from the module root. | Resolve tool semantics inside the provider owner before emitting `ToolInvocation`/`ToolUpdate` or invoking host callbacks; remove caller-side normalization. |
| [`codex/approval.rs`](nvim/rust/forge/crates/forge-providers/src/codex/approval.rs#L208) | `permission_from_provider`, `codex_response` | Native request decoding and approval-response JSON encoding. | Decode and encode approval wire messages inside the provider owner; Harness receives normalized permission requests and returns normalized decisions. |
| [`codex/catalog.rs`](nvim/rust/forge/crates/forge-providers/src/codex/catalog.rs#L7) | `parse_skill_catalog`, `parse_mcp_catalog`, `parse_model_catalog`, `quoted_config_segment` | Native catalogs and configuration-key encoding. | Keep native parsing/configuration quoting in the provider owner; return `SkillDefinition`, `McpDefinition`, and `ProviderModel` through normalized catalog operations. |
| [`codex/decode.rs`](nvim/rust/forge/crates/forge-providers/src/codex/decode.rs#L36) | `normalize_agent_lifecycle`, `agent_lifecycle_list`, `pointer_string`, `context_usage`, `available_compact_update`, `usage_update`, `is_tool_lifecycle_event`, `normalize_checklist_update`, `normalize_tool_activity`, `successful_file_change`, `first_text` | Native message, tool, checklist, usage, and lifecycle decoding. | Finish every conversion inside the provider owner; consumers receive complete normalized events/change data, not raw fields or partially decoded lifecycle objects. |
| [`codex/normalization.rs`](nvim/rust/forge/crates/forge-providers/src/codex/normalization.rs#L843) | `native_goal_state` | Native goal notification classification. | Classify native goal state inside the provider owner and publish normalized goal/execution updates; Harness must not inspect native notification names or settlement states. |
| [`codex/protocol.rs`](nvim/rust/forge/crates/forge-providers/src/codex/protocol.rs#L92) | `response_id`, `request_result`, `receive` | JSON-RPC response correlation and native frame receipt. | Retain response matching and frame receipt in the provider owner; expose typed operation results/errors and normalized events, never JSON-RPC IDs or frames for host interpretation. |
| [`codex/retry.rs`](nvim/rust/forge/crates/forge-providers/src/codex/retry.rs#L4) | `transient_code` | Native retry classification. | The provider owner combines native error classification with replay-safety evidence and returns normalized retry eligibility instead of exporting a native code predicate. |
| [`codex/tool_semantics.rs`](nvim/rust/forge/crates/forge-providers/src/codex/tool_semantics.rs#L5) | `codex::normalize_invocation` | Provider-native tool semantics, re-exported from the module root. | Translate native tools inside the provider owner; host tool callbacks and events use normalized `ToolInvocation`/`ToolUpdate`. |
| [`acp/children.rs`](nvim/rust/forge/crates/forge-providers/src/acp/children.rs#L219) | `timestamp` | A generic clock helper exposed through a native implementation namespace. | Keep this implementation helper private to the provider owner; normalized child events carry timestamps, and independent host clock needs use a neutral utility rather than a native module. |
| [`acp/permission.rs`](nvim/rust/forge/crates/forge-providers/src/acp/permission.rs#L7) | `permission_request` | ACP tool metadata converted into host permission requests. | Convert tool metadata inside the provider owner before calling `ScopedPermissions`; Harness receives a normalized permission request, never native metadata to parse. |
| [`acp/response.rs`](nvim/rust/forge/crates/forge-providers/src/acp/response.rs#L4) | `acp::validate_prompt_response` | Raw native terminal-response validation, re-exported from the module root. | Validate terminal responses inside the provider owner and emit `ExecutionUpdate::Finished` or normalized failure; remove host-side raw-response validation and certificate branches. |
| [`acp/tool_semantics.rs`](nvim/rust/forge/crates/forge-providers/src/acp/tool_semantics.rs#L5) | `acp::normalize_invocation` | Provider-native tool semantics, re-exported from the module root. | The provider owner resolves native invocation semantics before normalized tool callbacks/events leave the crate. |
| [`terminal.rs`](nvim/rust/forge/crates/forge-providers/src/terminal.rs#L6) | `terminal::parse_snapshot` | A public `copilot: bool` chooses between native terminal inventory schemas. | Select the native parser inside each provider owner; callers request and receive `TerminalSnapshot` without passing schema-selection flags. |
| [`permission_input.rs`](nvim/rust/forge/crates/forge-providers/src/permission_input.rs#L6) | `provider_shell`, `provider_path`, `normalize_permission_payload` | Raw native permission-field extraction; these have no provider noun in their public names. | Extract shell/path fields inside the provider owner and call normalized permission interfaces; generic helper names must not preserve raw-payload interpretation in Harness. |

`copilot::rejection::is_denied` also re-exports `copilot::decode::is_denied`; remove that re-export and apply the same provider-owned denial normalization required above. The public child modules themselves remain leaks even if their individual helpers are renamed: make implementation modules private only after normalized operations replace their external call sites.

## SDK types still exposed by public signatures

Removing the Harness SDK dev-dependency did not remove SDK types from `forge-providers`' public API. The following signatures still accept or return SDK values, so an external caller can participate in SDK-level operations without declaring or naming the SDK directly.

These are **defined inside `forge-providers`**, but their signatures expose types defined in `github-copilot-sdk`. This is a separate issue from the already-removed direct Harness dependency.

| Entry point | SDK type crossing the API | Required fix |
| --- | --- | --- |
| `copilot::catalog::model_descriptor` | `github_copilot_sdk::Model` enters the converter. | The provider owner obtains SDK models internally and exposes normalized `ProviderModel` catalogs; no public converter accepts SDK models. |
| `copilot::control::tool_list` | `Vec<github_copilot_sdk::Tool>` leaves the converter. | The provider owner builds and consumes SDK tools from normalized host definitions/callbacks without returning SDK values. |
| `copilot::input::message` | `github_copilot_sdk::MessageOptions` leaves the converter. | The provider owner accepts `MessageInput` and consumes the encoded SDK message internally. |
| `copilot::handle::model_options` | `github_copilot_sdk::SetModelOptions` leaves the converter. | The provider owner encodes normalized model/reasoning selections internally and returns only operation success or `ProviderError`. |
| `copilot::mcp::launch_request` | SDK `McpStartServerRequest` leaves the converter. | The provider owner converts admitted normalized resources to SDK launch requests and exposes normalized resource status, not SDK requests. |
| `copilot::permission::handler`, `checkpoint_hooks` | SDK `PermissionHandler` and `SessionHooks` trait objects leave the adapter. | Install SDK handlers inside the provider owner around normalized permission/checkpoint callbacks; no SDK callback signature crosses the boundary. |
| `copilot::event_stream::SessionEventStream::new`, `recv` | SDK `Session` and event filters enter; SDK `SessionEvent` values leave. | The provider owner creates and drains SDK subscriptions; external consumers receive only `ProviderEventStream`. |
| `copilot::event_decoder::CopilotEventDecoder::decode` | SDK `SessionEvent` enters the decoder. | Call the decoder only inside the provider owner and publish fully normalized `ProviderEvent`; relocate SDK-inferred fixtures to provider tests. |
| `copilot::turn::CopilotTurnEvent::observe` | SDK `SessionEvent` enters the completion owner. | Observe native events inside the provider owner and produce normalized execution lifecycle events, with no host observation/certification method. |
| `copilot::prompt::completes_exchange` | SDK `SessionEvent` enters the terminal classifier. | The provider owner classifies and correlates terminal signals before publishing `ExecutionUpdate`; callers never classify SDK events. |
| `copilot::rejection::RejectionState::observe` | SDK `SessionEvent` enters the rejection tracker. | The provider owner tracks native rejection and publishes normalized permission/execution outcomes rather than exposing the tracker. |
| `copilot::decode::{decode_compaction, session_error, agent_lifecycle}` | SDK `SessionEvent` enters the converters. | Keep SDK converters in the provider owner; expose normalized compaction/agent events and `ProviderError` or observation failure. |

The remaining Harness decoder fixtures deserialize JSON into the decoder's inferred SDK event parameter. They no longer import SDK names, but still depend on this low-level decoder boundary; removing imports alone did not make those fixtures provider-independent.

## Native objects defined outside forge-providers

The following objects are **defined in `forge-harness`, not `forge-providers`**. Some mix native and host responsibilities; those require a split, not wholesale movement of Harness permissions, tools, or persistence into the native crate.

This list includes generic-named objects in native adapter modules, because moving only declarations with provider names would leave the same behavior outside the intended owner.

### Copilot objects in Harness

Harness defines the provider implementation and the host callback that mutates its native submission state. The callback's legitimate host responsibilities do not justify retaining native session ownership there.

| Object | Defining file | Native responsibility outside the provider crate | Required fix |
| --- | --- | --- | --- |
| `CopilotProvider`, `SharedCopilotSession` | [`provider/copilot/mod.rs:68`](nvim/rust/forge/crates/forge-harness/src/provider/copilot/mod.rs#L68) | Stores separate native runtime/session owners and implements native lifecycle orchestration. | Move orchestration, exclusions, correlation, and terminal production into the provider owner; Harness supplies normalized requests/callbacks and consumes `ExecutionObservation`, with no native anchor or certificate assembly. |
| `HostTurn` | [`provider/copilot/host_turn.rs:11`](nvim/rust/forge/crates/forge-harness/src/provider/copilot/host_turn.rs#L11) | Stores `CopilotSessions` and `CopilotSession`; `before_send` marks a native binding submitted. Tool-result accumulation and host task authority belong in Harness. | Split the owner: Harness retains tool results/task authority behind normalized callbacks; the provider owner retains native handles and submission marking, calling `InputReceipt` for host delivery persistence. |

`CopilotProvider` also defines native-agent exclusion spellings in `native_agent_exclusions`, selects session reuse/preparation, passes repair and observer state into `prompt::run`, coordinates abort/send/wait ordering, and converts native anchors and execution groups into completion certificates. These are responsibilities on the listed object, not additional exported types.

### Codex objects in Harness

Codex retains a native protocol layer and an execution state machine in Harness. Its objects define response correlation, child ancestry, background readers, approval publication, and terminal settlement rather than merely adapting host services.

| Object | Defining file | Native responsibility outside the provider crate | Required fix |
| --- | --- | --- | --- |
| `CodexProvider`, `CodexConnection`, `CodexThread`, `TerminalConnection`, `CodexTurnState` | [`provider/codex/mod.rs:43`](nvim/rust/forge/crates/forge-harness/src/provider/codex/mod.rs#L43) | Native thread preparation/submission, connections, active turns, terminal queries, request/notification matching, and shutdown. `CodexConnection` here is distinct from the provider crate's type of the same name. | Consolidate native connection/session/turn ownership in `forge-providers`; Harness uses normalized session operations, `TerminalSnapshot`, and event streams rather than native thread controls. |
| `CodexJsonRpc`, `ReceivedMessage`, `DynamicToolTraceContext` | [`provider/codex/json_rpc.rs:97`](nvim/rust/forge/crates/forge-harness/src/provider/codex/json_rpc.rs#L97) | Raw request/response dispatch, native message fields, tool-call identities, native background cleanup, and response publication. This `ReceivedMessage` is distinct from the provider crate's type of the same name. | Move dispatch, identity resolution, native response encoding, and cleanup into the provider owner; invoke `ProviderTools`/`ProviderTrace` with normalized requests and emit normalized events. |
| `CodexNormalization` | [`provider/codex/normalization.rs:7`](nvim/rust/forge/crates/forge-harness/src/provider/codex/normalization.rs#L7) | Wraps and exposes the native normalizer through `Deref`/`DerefMut`, while mixing normalized output with host control effects. | Remove the native normalizer wrapper from Harness; the provider owner completes normalization, and host control effects run through typed callbacks without decoder access. |
| `CodexResources` | [`provider/codex/resources.rs:12`](nvim/rust/forge/crates/forge-harness/src/provider/codex/resources.rs#L12) | Owns an extra native runtime/thread, native MCP aliases, Codex configuration JSON, skill roots, and catalog requests. Canonical resource selection remains host-owned. | Move runtime/configuration/alias/catalog translation into the provider owner; Harness supplies admitted normalized resources and receives `SkillDefinition`, `McpDefinition`, and catalog mutation results. |
| `CodexTurnCoordinator`, `ParentTurn`, `DescendantTracker`, `ObservedDescendantTurn` | [`provider/codex/turn_coordinator.rs:81`](nvim/rust/forge/crates/forge-harness/src/provider/codex/turn_coordinator.rs#L81) | Native turn/child ownership, reused identities, pending steering, terminal coverage, and parent continuation. | Move native correlation and steering placement into the provider owner; publish scoped `ExecutionUpdate` and agent events, with no host ancestry tracker or completion-certificate side channel. |
| `NativeGoalLifetime`, `NativeGoalSettlement`, `DescendantCleanup`, `CleanupRequest` | [`provider/codex/turn_coordinator.rs:13`](nvim/rust/forge/crates/forge-harness/src/provider/codex/turn_coordinator.rs#L13) | Native goal pause/settlement, interrupt acknowledgments, terminal inventory, and descendant process collection. | Let the provider owner perform native interruption/descendant collection and normalize goal/execution outcomes; Harness retains goal policy but does not interpret native settlement evidence. |
| `BackgroundDelivery`, `ReaderSlot`, `IdleReader` | [`provider/codex/background.rs:13`](nvim/rust/forge/crates/forge-harness/src/provider/codex/background.rs#L13) | Native reader retention and handoff around foreground work. Generic event delivery can remain host-owned, but socket-reader ownership belongs with the native runtime. | Keep one native reader lifetime in the provider owner across foreground/idle handoffs; Harness relays normalized events, and persistent Background retains its own session rather than using isolated generation. |
| `RequestExecutor`, `ExecutedRequest` | [`provider/codex/request_execution.rs:20`](nvim/rust/forge/crates/forge-harness/src/provider/codex/request_execution.rs#L20), [`ExecutedRequest:513`](nvim/rust/forge/crates/forge-harness/src/provider/codex/request_execution.rs#L513) | Mixed ownership: host tools/task authority alongside native JSON messages, JSON-RPC responses, `PreparedApproval`, and native publication metadata. | Split native request/response and approval translation into the provider owner; retain host tools/task policy behind normalized callbacks whose results require no native publication decisions. |

The free functions in [`provider/codex/text_generation.rs`](nvim/rust/forge/crates/forge-harness/src/provider/codex/text_generation.rs#L7) are another misplaced implementation: `tool_free_config` and `generate` construct native configuration, open a thread, parse terminal events, and unsubscribe. **Required fix:** move both into the provider owner behind `generate_text(TextGenerationRequest) -> Result<String, ProviderError>`, including native waiting and cleanup; retain Background on its separate persistent session/stream. The attachment encoder and native catalog/resource helpers are listed below.

### ACP, Claude, and Antigravity objects in Harness

Harness owns ACP conversation preparation and native completion decisions despite the native crate already having `AcpConversation`. Setup and authentication also use a separate raw transport path.

| Object | Defining file | Native responsibility outside the provider crate | Required fix |
| --- | --- | --- | --- |
| `AcpProvider`, `AcpSession`, `PromptEvidence` | [`provider/acp/provider.rs:22`](nvim/rust/forge/crates/forge-harness/src/provider/acp/provider.rs#L22) | Native conversation maps, prompt evidence, compaction request construction, raw `stopReason` interpretation, start/finish publication, configuration/catalog fields, and cleanup ordering. | Move conversation state, stop-reason decoding, compaction, and cleanup into the provider owner; Harness receives normalized catalogs and `ExecutionUpdate`, never `PromptEvidence` or raw completion fields. |
| `AcpTransport` | [`provider/acp/transport.rs:9`](nvim/rust/forge/crates/forge-harness/src/provider/acp/transport.rs#L9) | Raw ACP transport used for setup/authentication, with method-name/JSON request APIs. | Move setup/authentication transport into the provider owner and return typed normalized setup/authentication results and `ProviderError`, not generic method/JSON access. |
| `TerminalBoundary::AcpPrompt` | [`provider/session_events.rs:201`](nvim/rust/forge/crates/forge-harness/src/provider/session_events.rs#L201), [`terminal publication:725`](nvim/rust/forge/crates/forge-harness/src/provider/session_events.rs#L725) | The shared session execution owner has an ACP-specific terminal-evidence branch. ACP prompt return may establish coverage without the adapter certificate required on the other native paths; the native owner should supply normalized terminal evidence instead of selecting a provider-specific host rule. | Delete the provider-specific host branch and all certificate-based native completion production; the provider owner emits `ExecutionObservation`/`ExecutionUpdate::Finished`, and Harness consumes that same normalized contract for every provider. |
| `AcpPermissionContext`, `ActivePermission` | [`permissions/acp.rs:12`](nvim/rust/forge/crates/forge-harness/src/permissions/acp.rs#L12) | The type name violates noun isolation, but most behavior is host policy and should remain behind the normalized `ScopedPermissions` callback. Its special recognition of `forge-harness-control/` is an additional adapter-coupling point to separate from generic permission evaluation. | Preserve host permission state in Harness under a neutral owner; move native alias/prefix recognition into the provider owner so `ScopedPermissions` receives canonical tool targets rather than adapter names. |

`AcpProvider` manually calls `state().begin`, `begin_compaction`, `finish_compaction`, and `finish`; reads model configuration JSON; issues `/compact` through `session/prompt`; and constructs authentication handshakes. Those operations must become native-owner responsibilities rather than caller recipes.

## Native encoding outside provider-specific objects

Several generic-named host objects contain native encoders, so an object-name grep would miss them. The generic file/persistence owner can remain outside `forge-providers`; only the native encoding, constraints, or layout recipe needs to move.

These rows are **defined outside `forge-providers`**, even where their parent type has a provider-neutral name.

| Object or function | Definition | Leak | Required fix |
| --- | --- | --- | --- |
| `ResourceProfile::{copilot, claude, antigravity, native_home}` | Type: [`settings/profile.rs:5`](nvim/rust/forge/crates/forge-harness/src/settings/profile.rs#L5). Native methods: [`provider/copilot/profile.rs:5`](nvim/rust/forge/crates/forge-harness/src/provider/copilot/profile.rs#L5), [`provider/acp/profile.rs:5`](nvim/rust/forge/crates/forge-harness/src/provider/acp/profile.rs#L5). | Provider directory layouts, Claude plugin metadata, Antigravity private state, and native-home selection. Shared lease/link cleanup is not itself a leak. | Move native layout/home/metadata recipes into the provider owner; Harness supplies admitted resources and a normalized profile/lease capability, retaining generic file/link cleanup without knowing native paths. |
| `SessionResources::acp` and `SessionResources::launch` | [`settings/resources.rs:9`](nvim/rust/forge/crates/forge-harness/src/settings/resources.rs#L9) | ACP server JSON encoding, unsupported-field constraints, and resolved native launch maps rather than a normalized resource value. | Harness supplies typed normalized session resources; the provider owner validates adapter support and encodes server/launch payloads, returning normalized unsupported-capability errors. |
| `ServerResource::native`, `ResolvedLaunch::into_native`, `ResolvedLaunch`, `RemoteLaunch` | [`settings/catalog.rs:46`](nvim/rust/forge/crates/forge-harness/src/settings/catalog.rs#L46) | Credential resolution and canonical catalog ownership belong in Harness, but the result currently becomes native SDK-shaped JSON before reaching the provider. | Preserve catalog selection and credential resolution in Harness; pass typed normalized local/remote resource values to the provider owner, which alone creates native JSON and redacts outward status. |
| MCP import validation | [`settings/import.rs:403`](nvim/rust/forge/crates/forge-harness/src/settings/import.rs#L403) | Shared resource import rejects tool selection specifically because ACP cannot transfer it, applying one native adapter's limitation in the host import boundary. | Keep portable import validation in Harness; the selected provider owner evaluates native resource constraints during reconciliation and returns typed capability/unsupported results instead of restricting all imports. |
| `provider::codex::attachments::input` | [`provider/codex/attachments.rs:5`](nvim/rust/forge/crates/forge-harness/src/provider/codex/attachments.rs#L5) | Creates native `localImage` and text-item JSON. | Move native item encoding to the provider owner; Harness admits files and passes `MessageInput`/`FileAttachment` without native item tags. |
| `provider::acp::attachments::input` | [`provider/acp/attachments.rs:6`](nvim/rust/forge/crates/forge-harness/src/provider/acp/attachments.rs#L6) | Creates ACP image/base64 and text-item JSON. | Move wire image/text encoding to the provider owner; Harness retains attachment admission and sends normalized `FileAttachment` values. |
| `provider::copilot::attachments::input` | [`provider/copilot/attachments.rs:4`](nvim/rust/forge/crates/forge-harness/src/provider/copilot/attachments.rs#L4) | Prepares the provider-specific file/prompt representation outside the native crate, though its return type is normalized `MessageInput`. | Keep generic file admission in Harness; the provider owner derives native file/prompt representation from `MessageInput`, rather than merely returning a neutral type from host-native logic. |
| Question answer/correction rendering | [`question.rs:661`](nvim/rust/forge/crates/forge-harness/src/question.rs#L661) | Shared question processing constructs `<send_user_message_question_reply>` payloads; its tests explicitly identify this as the native Codex envelope. The host should retain structured answers, not encode one provider's representation. | Harness retains structured question identity, answers, and corrections; an API question-reply contract carries them to the provider owner, which alone renders native envelopes or provider-appropriate text. |

## Generic shared contracts that still expose native interpretation

Provider-neutral names do not establish provider-neutral semantics. These findings are **defined in `forge-provider-api`, outside `forge-providers`**; their replacement contracts belong in the API, but native interpretation belongs in the provider owner.

| Object or function | Defining file | Leak | Required fix |
| --- | --- | --- | --- |
| `ToolStatus::parse`, `ToolStatus`'s `Deref<str>` | [`tool.rs`](nvim/rust/forge/crates/forge-provider-api/src/tool.rs) | Shared parsing accepts native status spellings such as `inprogress`, `declined`, and `complete`; string dereferencing encourages consumers to interpret status as native text. | Move native spelling recognition into each provider owner; retain typed `ToolStatus`/`ToolOutcome` in the API, remove implicit string dereferencing, and let consumers match normalized variants with explicit display serialization only. |
| `SubmissionTarget::Native`, `native`, `native_checkpoint`, `conversation_id` | [`submission.rs`](nvim/rust/forge/crates/forge-provider-api/src/submission.rs) | The public recovery structure exposes native format/version, conversation identity, and checkpoint JSON, with an accessor requiring the expected native format. | Replace the outward structure with an opaque provider-owned recovery token; Harness persists and returns it through `InputReceipt` without interpretation, while the provider owner validates versions and resolves recovery. No missing acknowledgment authorizes replay. |
| `Provider::setup` and authentication discovery | [`provider.rs`](nvim/rust/forge/crates/forge-provider-api/src/provider.rs) | Setup returns `serde_json::Value`, leaving a boundary where native setup/authentication fields can require consumer interpretation. | Define proposed typed `ProviderSetup` and `AuthenticationMethod` in the API; the provider owner converts native discovery/results and errors before returning them, and consumers select opaque method IDs from those descriptors. |
| `ProviderHost` and `Provider`'s associated workflow request/result types | [`provider.rs`](nvim/rust/forge/crates/forge-provider-api/src/provider.rs) | Host-associated request, completion, output, settings, and control types leave provider integration dependent on Harness workflow structures instead of a complete normalized contract. | Replace crossing types with proposed `SessionRequest`, `PromptRequest`, and `SteeringRequest` plus existing normalized generation/events/errors; Harness captures workflow state in host callbacks, and the provider owner independently produces native completion on the stream. |

The generic-name remediation map is therefore behavioral, not lexical: question replies become structured input, `TerminalBoundary::AcpPrompt` becomes the shared execution event path, profile/resource payloads become typed admitted resources plus private native encoding, and recovery becomes opaque token round-tripping. The linked entries retain their original file ownership classifications; none can be repaired by renaming the containing type.

## Provider nouns outside the implementation crate

The strict requirement also rules out provider registries, enum variants, labels, and default commands defined outside `forge-providers`. These are not all native lifecycle bugs, but leaving them in place would still fail the requested noun-isolation goal.

A normalized descriptor may carry a provider's identity and display text as values supplied by the native registry. Consumers should not enumerate provider-specific variants or embed provider-specific defaults themselves.

| Definition outside `forge-providers` | File | Current provider knowledge | Required fix |
| --- | --- | --- | --- |
| `ProviderKind` and its parser | [`forge-provider-api/src/model.rs:11`](nvim/rust/forge/crates/forge-provider-api/src/model.rs#L11) | Defines `Codex`, `Copilot`, `Claude`, and `Antigravity` variants and parses their native names. Moving implementations alone does not remove this shared-crate knowledge. | Replace with proposed opaque `ProviderId` in the API; the `forge-providers` registry alone resolves concrete names/aliases, and consumers persist IDs and use registry descriptors without vendor branches. |
| `provider::{build, registered_capability}` | [`forge-harness/src/provider/contract.rs:244`](nvim/rust/forge/crates/forge-harness/src/provider/contract.rs#L244), [`registered_capability:1454`](nvim/rust/forge/crates/forge-harness/src/provider/contract.rs#L1454) | Selects concrete native implementations and capability declarations in Harness. | Move implementation selection/capability registration into `forge-providers`; Harness passes normalized identity and callbacks and receives an opaque provider interface plus normalized descriptor. |
| Native adapter `capability` functions | [`provider/copilot/mod.rs:319`](nvim/rust/forge/crates/forge-harness/src/provider/copilot/mod.rs#L319), [`provider/codex/mod.rs:103`](nvim/rust/forge/crates/forge-harness/src/provider/codex/mod.rs#L103), [`provider/acp/provider.rs:1201`](nvim/rust/forge/crates/forge-harness/src/provider/acp/provider.rs#L1201) | Provider-specific support, limitations, Rulesync targets, and isolation choices. | The provider registry/owner computes normalized `CapabilityManifest` and descriptors; Harness consumes capability values without inferring support or configuration targets from identity. |
| Provider module declarations | [`forge-harness/src/provider/mod.rs:1`](nvim/rust/forge/crates/forge-harness/src/provider/mod.rs#L1), [`permissions/mod.rs:1`](nvim/rust/forge/crates/forge-harness/src/permissions/mod.rs#L1) | Exposes or wires provider-named implementation namespaces outside the intended owner. | Move native modules into private provider-owned modules, remove host-native wiring, and retain neutral Harness callback/policy modules using the normalized API. |
| Agent provider selection mapping | [`agent/provider.rs:293`](nvim/rust/forge/crates/forge-harness/src/agent/provider.rs#L293) | Maps concrete provider enum variants to provider configuration names. | Harness selects opaque provider identity from registry descriptors; `forge-providers` resolves names/defaults, eliminating host enum-to-native-name mappings. |
| Subagent profile validation | [`agent/profile.rs:104`](nvim/rust/forge/crates/forge-harness/src/agent/profile.rs#L104) | Hardcodes the accepted provider-name set. | Validate selected IDs against the provider registry and normalized capability requirements; keep profile policy in Harness without a hardcoded vendor set. |
| Codex and Copilot Lua descriptors | [`codex.lua`](nvim/lua/forge/harness/providers/codex.lua), [`copilot.lua`](nvim/lua/forge/harness/providers/copilot.lua) | Provider-specific command defaults, keys, and permission descriptions. | Move concrete defaults/descriptions into provider registry descriptors; replace per-provider Lua modules with generic descriptor consumers and normalized configuration fields. |
| Lua configuration types/defaults | [`infra/config.lua:155`](nvim/lua/forge/infra/config.lua#L155), [`defaults:225`](nvim/lua/forge/infra/config.lua#L225) | Concrete provider union members, default selection, labels, launch commands, and generation defaults. | Lua accepts opaque provider IDs and registry-supplied defaults/labels/capabilities; the provider registry owns native launch and generation defaults, while user overrides remain data. |
| Lua subagent selector | [`subagent_selection.lua:20`](nvim/lua/forge/views/harness/subagent_selection.lua#L20) | Hardcoded provider-choice list instead of registry-supplied descriptors. | Populate choices from the provider registry's normalized descriptors and select their opaque IDs; remove the Lua vendor list. |
| Workspace SDK pin and migration configuration | [`Cargo.toml`](nvim/rust/forge/Cargo.toml), [`migration.toml`](nvim/rust/forge/migration.toml) | Provider-name/dependency matches outside the implementation crate; build/tooling metadata rather than runtime behavior. These must be accounted for separately if the lexical rule includes manifests. | Keep SDK dependency ownership in `forge-providers`; relocate hand-maintained native pins/migration configuration there where tooling permits, and explicitly scope unavoidable workspace metadata as build-only exceptions, never runtime provider selection. |

Generated lockfiles necessarily record dependency package names; hiding a symbol does not remove those names from `Cargo.lock`. Documentation, this audit, user-supplied configuration values, and generated dependency metadata need an explicit search scope rather than being mistaken for native code ownership.

## Test exposure and naming

Tests outside `forge-providers` still instantiate native objects, replay native wire messages, or name concrete providers. Some cover real cross-crate host integration and therefore cannot be moved wholesale without preserving equivalent host-boundary coverage.

Under the strict noun requirement, native protocol fixtures should live with native implementations, and generic Harness tests should use normalized mock observations or registry-selected fixtures. A rename to hide a provider noun while retaining native JSON/state dependencies would not repair the boundary.

| Outside location | Current dependency | Required fix |
| --- | --- | --- |
| `forge-harness/src/provider/copilot/{turn_test.rs,file_change_test.rs,event_decoder/,mcp/test.rs}` | Native Copilot JSON-RPC mocks, raw decoder fixtures, tool semantics, and MCP startup. | Move native fixtures/assertions into `forge-providers`; retain Harness integration tests using normalized events/callbacks, including incomplete live ancestry completing Chat and persistent Background without certificates. |
| `forge-harness/src/provider/acp/{runtime_test.rs,session_test.rs,submission_test.rs}` | ACP runtime/session/transport fixtures; `session_test.rs` defines its own test `AcpSessionState` wrapper. | Move native runtime/state/transport fixtures into `forge-providers`; replace Harness wrappers with normalized session mocks covering submission, cancellation, and stream completion. |
| Inline tests in `forge-harness/src/provider/codex/*.rs` | Native socket messages, turn coordination, resource encoding, background lifecycle, and isolated generation. | Relocate native protocol/race/encoding tests with provider owners; preserve host normalized lifecycle tests and distinguish maintained Background streams from one-return isolated generation. |
| `forge-harness/tests/live_provider/{codex_cli.rs,codex_real.rs,copilot_real.rs,catalog.rs,session_resume.rs,suite.rs,text_generation.rs,workflows.rs}` | Provider-specific live integration and prerequisite knowledge outside the implementation crate. | `forge-providers` owns native launch/prerequisite fixtures; Harness retains registry-selected normalized end-to-end workflows with opaque IDs and no wire/native-state assertions. |
| `forge-harness/src/{skill.rs,buffer/transcript.rs,attachment.rs}` | Test fixtures directly invoke provider-specific semantic/attachment helpers; these are not production renderer/skill calls. | Host tests supply normalized skill/tool/message inputs directly; provider-owned tests verify native semantic and attachment conversion separately, preserving host rendering assertions. |
| Shared Rust tests in `agent/`, `broker/`, `buffer/projection.rs`, `exchange/metrics.rs`, `input/`, `permissions/session_directory.rs`, `plan/render.rs`, `provider/{approval.rs,steering.rs,submission.rs}`, `service.rs`, `session/mod.rs`, `settings/`, `storage/`, and `tools/session/tests.rs` | Concrete provider IDs, capabilities, labels, example type names, or adapter fixtures. The lexical search finds these even when their tested behavior is generic. | Harness tests use neutral fixture identities and normalized capability/event data; move any actual native fixture behavior into `forge-providers`, preserving generic policy, persistence, and routing coverage. |
| `forge-harness/src/question.rs` tests | Assert the native Codex question-reply envelope emitted by shared production question processing. | Harness tests assert structured answers/corrections; provider-owned tests assert native envelope rendering from that normalized input, so moving tests does not leave the production encoder in Harness. |
| `forge-provider-api/src/{capability.rs,version.rs}`, `forge-protocol/src/contract.rs`, `forge-shell/src/process.rs` | Test-only concrete provider IDs, labels, or executable fixture names; generic logic need not know these names. | Keep generic tests in their owning crates with neutral fixture data; put any native naming/version interpretation tests in `forge-providers`. |
| `nvim/rust/forge/src/commit_message/settings.rs`, `nvim/rust/forge/tests/harness_broker_stdio.rs` | Provider-specific test data outside both provider crates. | Replace native names with normalized registry/mock descriptors in these host tests; retain Background selection/settings and broker transport assertions without native implementation knowledge. |
| `nvim/tests/forge/harness_{acp_children_host,acp_real,acp_submission_host,codex_activity,controller_native,provider_setup,provider_switch_host,skill_reads_host}.lua` | Native/editor integration fixtures with provider-specific behavior or launch assumptions. | Provider-owned tests cover native launch/protocol behavior; Lua retains normalized setup, lifecycle, tool, and selection integration tests driven by descriptors, with native fixtures removed rather than renamed. |
| Other matching `nvim/tests/forge/*.lua` and `nvim/tests/github_open_pr.lua` | Provider names in configuration, model catalogs, pins, preferences, routing assertions, and test registration. The second search above enumerates the current files. | Lua tests use neutral IDs and normalized registry/catalog fixtures, preserving preferences, routing, and persistent Background PR-generation behavior; native assumptions move to provider-owned tests. |

The removed `github-copilot-sdk` Harness dev-dependency is not an outstanding direct dependency leak. The exposed decoder signatures and the test coupling described above remain outstanding.

The adapter fixtures also define the following helper objects outside the native crate. Their test-only status separates them from production ownership errors, but their native fixtures still require relocation or replacement under the strict source requirement.

| Test-only object | Defining file | Fixture responsibility | Required fix |
| --- | --- | --- | --- |
| `DeniedToolObserved`, `NativeUserDelivery` | [`copilot/turn_test.rs:12`](nvim/rust/forge/crates/forge-harness/src/provider/copilot/turn_test.rs#L12), [`NativeUserDelivery:264`](nvim/rust/forge/crates/forge-harness/src/provider/copilot/turn_test.rs#L264) | Denial synchronization and native input delivery scenarios. | Move native fixtures into provider tests; retain host denial/delivery assertions using normalized permission decisions, receipts, and execution events. |
| `Fixture` | [`copilot/mcp/test.rs:48`](nvim/rust/forge/crates/forge-harness/src/provider/copilot/mcp/test.rs#L48) | Native MCP session startup fixture. | Move startup wire fixtures into `forge-providers`; Harness tests observe normalized resource status and admitted-resource ownership only. |
| `AcpSessionState` | [`acp/session_test.rs:10`](nvim/rust/forge/crates/forge-harness/src/provider/acp/session_test.rs#L10) | Host-side wrapper around the native session state. | Remove the host-native wrapper; provider tests exercise private state, while Harness tests consume normalized session operations/events. |
| `Fixture` | [`acp/submission_test.rs:15`](nvim/rust/forge/crates/forge-harness/src/provider/acp/submission_test.rs#L15) | Native ACP submission fixture. | Move native submission setup into provider tests; replace external use with normalized acceptance, uncertain-delivery, and completion fixtures. |
| `MessageRecord` | [`codex/json_rpc.rs:1349`](nvim/rust/forge/crates/forge-harness/src/provider/codex/json_rpc.rs#L1349) | Normalized-message assertions inside native JSON-RPC tests; the record itself is provider-neutral. | Preserve neutral message assertions; move their native JSON-RPC fixture into provider tests, and retain a Harness record only where it consumes normalized events without native setup. |
| `CompletionReader`, `ShutdownEvidence` | [`codex/background.rs:177`](nvim/rust/forge/crates/forge-harness/src/provider/codex/background.rs#L177) | Native reader handoff and shutdown evidence scenarios. | Move native reader/cleanup races into provider tests; Harness tests assert ordered normalized terminal delivery and stream closure without native shutdown evidence. |
| `CommandStartTiming`, `InterruptReply` | [`codex/turn_coordinator.rs:1567`](nvim/rust/forge/crates/forge-harness/src/provider/codex/turn_coordinator.rs#L1567) | Native command admission and interrupt acknowledgment races. | Move native admission/interrupt races into provider tests; host tests assert normalized acceptance/cancellation outcomes without constructing native acknowledgments. |
| `WorkflowService` | [`tests/live_provider/workflows.rs:19`](nvim/rust/forge/crates/forge-harness/tests/live_provider/workflows.rs#L19) | Live host/provider workflow fixture; host integration coverage must remain after native fixtures move. | Preserve host workflow integration in Harness through registry-selected normalized interfaces; relocate native launch/protocol prerequisites into provider-owned fixtures instead of deleting cross-boundary coverage. |

## Objects already private inside the native crate

Not every provider-specific declaration is a leak. The following examples are already unreachable through public module paths and should remain internal rather than receive another abstraction solely for this audit.

`copilot::intake` is private, including its intake ownership types. `CopilotTurnState`, native turn-delivery/send state, session slots/bindings, the runtime's retained shutdown/recovery state, ACP message state, and Codex process/socket command state are also private implementation details. **Preserve:** keep those owners private in `forge-providers`, retaining ordered intake, correlation, and cleanup behavior while exposing only their normalized effects; no replacement abstraction is required solely for their names.

The five `NativeCompaction*` structs in [`copilot/compaction_metrics.rs`](nvim/rust/forge/crates/forge-providers/src/copilot/compaction_metrics.rs#L5) use `pub` declarations, but their parent module is private. They are not publicly reachable exports and do not belong in the exposed-object list. **Preserve:** retain native metrics structures internally and convert their outward effects to normalized compaction/accounting fields; do not make these structs public API.

## Complete provider-name file matches outside forge-providers

These are the lexical search results outside the implementation crate, not an assertion that every matching file contains a production logic leak. The object and behavior tables above classify the findings; this appendix keeps test fixtures and generic-named shared files from disappearing behind those classifications.

Each listed path inherits the required fix of its classified object, behavior, or test group above; the appendix does not require duplicate per-file remediation. Generic-name API findings such as `tool.rs` and `submission.rs` need not appear in a provider-name search and remain covered by their explicit table entries.

The Rust paths below are relative to `nvim/rust/forge`. The search includes `.rs` and `.toml`, excludes `crates/forge-providers`, and does not search generated build output or lockfiles.

```text
Cargo.toml
crates/forge-harness/src/agent/coordinator_test.rs
crates/forge-harness/src/agent/profile.rs
crates/forge-harness/src/agent/provider.rs
crates/forge-harness/src/agent/runtime_test.rs
crates/forge-harness/src/attachment.rs
crates/forge-harness/src/broker/agent_test.rs
crates/forge-harness/src/broker/input_test.rs
crates/forge-harness/src/broker/mod.rs
crates/forge-harness/src/buffer/projection.rs
crates/forge-harness/src/buffer/transcript.rs
crates/forge-harness/src/exchange/metrics.rs
crates/forge-harness/src/input/durability_test.rs
crates/forge-harness/src/permissions/acp.rs
crates/forge-harness/src/permissions/mod.rs
crates/forge-harness/src/permissions/session_directory.rs
crates/forge-harness/src/plan/render.rs
crates/forge-harness/src/provider/acp/attachments.rs
crates/forge-harness/src/provider/acp/mod.rs
crates/forge-harness/src/provider/acp/profile.rs
crates/forge-harness/src/provider/acp/provider.rs
crates/forge-harness/src/provider/acp/runtime_test.rs
crates/forge-harness/src/provider/acp/session_test.rs
crates/forge-harness/src/provider/acp/submission_test.rs
crates/forge-harness/src/provider/acp/transport.rs
crates/forge-harness/src/provider/approval.rs
crates/forge-harness/src/provider/codex/approval.rs
crates/forge-harness/src/provider/codex/attachments.rs
crates/forge-harness/src/provider/codex/background.rs
crates/forge-harness/src/provider/codex/json_rpc.rs
crates/forge-harness/src/provider/codex/mod.rs
crates/forge-harness/src/provider/codex/normalization.rs
crates/forge-harness/src/provider/codex/request_execution.rs
crates/forge-harness/src/provider/codex/resources.rs
crates/forge-harness/src/provider/codex/retry.rs
crates/forge-harness/src/provider/codex/text_generation.rs
crates/forge-harness/src/provider/codex/turn_coordinator.rs
crates/forge-harness/src/provider/contract.rs
crates/forge-harness/src/provider/copilot/approval.rs
crates/forge-harness/src/provider/copilot/attachments.rs
crates/forge-harness/src/provider/copilot/event_decoder.rs
crates/forge-harness/src/provider/copilot/event_decoder/conformance_tests.rs
crates/forge-harness/src/provider/copilot/event_decoder/tests.rs
crates/forge-harness/src/provider/copilot/file_change_test.rs
crates/forge-harness/src/provider/copilot/host_turn.rs
crates/forge-harness/src/provider/copilot/mcp/test.rs
crates/forge-harness/src/provider/copilot/mock_stream.rs
crates/forge-harness/src/provider/copilot/mod.rs
crates/forge-harness/src/provider/copilot/profile.rs
crates/forge-harness/src/provider/copilot/turn_test.rs
crates/forge-harness/src/provider/mod.rs
crates/forge-harness/src/provider/session_events.rs
crates/forge-harness/src/provider/steering.rs
crates/forge-harness/src/provider/submission.rs
crates/forge-harness/src/question.rs
crates/forge-harness/src/service.rs
crates/forge-harness/src/session/mod.rs
crates/forge-harness/src/settings/catalog.rs
crates/forge-harness/src/settings/import.rs
crates/forge-harness/src/settings/profile.rs
crates/forge-harness/src/settings/resources.rs
crates/forge-harness/src/settings/store.rs
crates/forge-harness/src/skill.rs
crates/forge-harness/src/storage/mod.rs
crates/forge-harness/src/storage/model_pin.rs
crates/forge-harness/src/tools/registry.rs
crates/forge-harness/src/tools/session/tests.rs
crates/forge-harness/tests/live_provider/catalog.rs
crates/forge-harness/tests/live_provider/codex_cli.rs
crates/forge-harness/tests/live_provider/codex_real.rs
crates/forge-harness/tests/live_provider/copilot_real.rs
crates/forge-harness/tests/live_provider/session_resume.rs
crates/forge-harness/tests/live_provider/suite.rs
crates/forge-harness/tests/live_provider/text_generation.rs
crates/forge-harness/tests/live_provider/workflows.rs
crates/forge-protocol/src/contract.rs
crates/forge-provider-api/src/capability.rs
crates/forge-provider-api/src/model.rs
crates/forge-provider-api/src/version.rs
crates/forge-shell/src/process.rs
migration.toml
src/commit_message/settings.rs
tests/harness_broker_stdio.rs
```

The Lua paths below are relative to `nvim`. They include the matching editor and GitHub PR fixtures rather than silently exempting tests from the noun requirement.

```text
lua/forge/harness/providers/codex.lua
lua/forge/harness/providers/copilot.lua
lua/forge/infra/config.lua
lua/forge/views/harness/subagent_selection.lua
tests/forge/ai_commit.lua
tests/forge/commit_host.lua
tests/forge/commit_large_output_host.lua
tests/forge/commit_provider_preference.lua
tests/forge/harness_acp_children_host.lua
tests/forge/harness_acp_real.lua
tests/forge/harness_acp_submission_host.lua
tests/forge/harness_codex_activity.lua
tests/forge/harness_command_admission.lua
tests/forge/harness_commit_settings.lua
tests/forge/harness_completion.lua
tests/forge/harness_context_catalog.lua
tests/forge/harness_controller_native.lua
tests/forge/harness_effort_selection.lua
tests/forge/harness_model_order.lua
tests/forge/harness_model_pins.lua
tests/forge/harness_provider_catalog.lua
tests/forge/harness_provider_destination.lua
tests/forge/harness_provider_setup.lua
tests/forge/harness_provider_switch_host.lua
tests/forge/harness_provider_switch.lua
tests/forge/harness_scroll_boundary.lua
tests/forge/harness_settings.lua
tests/forge/harness_skill_reads_host.lua
tests/forge/lua_suite_selection.lua
tests/forge/picker_navigation.lua
tests/forge/picker.lua
tests/forge/protocol_contract.lua
tests/forge/shared_api_examples.lua
tests/forge/status_staging_host_edges.lua
tests/forge/status_staging_host_flows.lua
tests/forge/status_staging_source_changes.lua
tests/forge/status_untracked_optimistic.lua
tests/forge/subagent_profiles.lua
tests/forge/text_contractions_host.lua
tests/github_open_pr.lua
```

## Required boundary after consolidation

The normalized provider interface must be the only behavior boundary used outside `forge-providers`. Native runtime/session/decoder ownership, wire encoding, retry classification, execution correlation, terminal evidence, and cleanup sequencing must stay behind that interface.

Harness must retain its permissions policy, canonical tools, task authority, persistence, admitted resource ownership, and generic execution coordination. Those responsibilities should communicate through provider-neutral callback contracts such as `ProviderPermissions`, `ScopedPermissions`, `ProviderTools`, `InputReceipt`, `ProviderTrace`, and `TurnControl`; they must not retain concrete native sessions or prescribe native transition order.

Completion of this work requires both source-level and behavior-level evidence: no provider-specific implementation nouns outside the agreed source scope, no publicly reachable native implementation objects, and mocked integration coverage showing that moving ownership preserves acceptance, cancellation, recovery, and terminal delivery. This inventory identifies the current exposure; it does not claim that every exposed path has produced a runtime failure.
