# Providers and external resources

Provider adapters normalize transport-specific identities, capabilities, activity, and execution controls into the Harness contract. Shared settings preserve explicit provider differences rather than simulating unsupported operations.

Harness lifecycle and permission ownership remain in [Harness](harness.md). Transport admission and framing are described in [Host and protocol](host-and-protocol.md).

Model requests run through Harness provider adapters. Commit messages and PR descriptions
share the same persistent, branch-aware Background session, provider thread, and
Background model and thinking settings. Each exchange repeats its own task and output
requirements under a shared system message. Neovim owns request presentation and generated
text application, without a separate model HTTP client or CLI generation adapter.
Background speed persists with each provider's model preference and travels through
`TextGenerationConfig` into the Background exchange. The runner validates the selected
service tier against the same capability contract as Main before admitting model work.
It applies the captured tier on every exchange, including when reusing a conversation,
so selecting `default` clears an earlier accelerated tier. The `/config` Background
row exposes Speed only when the current provider advertises an accelerated tier.
Codex model discovery retains each model's `serviceTiers`, mapping its `priority`
ID to Forge's `fast` choice. Main, Background, and subagent selectors intersect
these model choices with provider capabilities. Missing model metadata permits
only `default`. Codex refreshes the catalog before an accelerated request and
rejects unsupported combinations before native thread or turn submission. The
[native model schema](https://github.com/openai/codex/blob/main/codex-rs/app-server-protocol/src/protocol/v2/model.rs)
defines this metadata. Provider-wide Fast or Ultrafast support alone does not
establish support for a particular model or account.

Input delivery uses a separate durable state machine from model retries.
`backend/submission.rs` commits input before host admission, native correlation
before dispatch, and provider acceptance before releasing the delivery obligation.
Unknown delivery blocks replay and retains the next prompt. The
[provider retry and submission contract](../provider-retries.md) defines recovery,
steering, and adapter extension requirements.
ACP's transport owns this boundary for both prompt and steering requests. Its v1
prompt response proves turn completion, so lost responses remain uncertain.
The ACP v2 draft introduces earlier message acceptance receipts, but those
semantics require explicit protocol negotiation and provider support.

The broker selects ordinary exchange content separately from provider control
events before polling either handler. This keeps control-event temporary state
out of content rendering during nested permission-resume and check-repair work.
The worker-stack regression test retains its one-megabyte limit.

## Backend contracts and ACP runtime

Attachment objects resolve against the Harness data root during request admission and
remain separate from `BackendInput` text. Provider adapters never discover attachments
from text. Initial prompts, queued follow-ups, and steering use the same typed objects.
Images receive textual `<image name=[Image #N] path="...">` framing generated from their
metadata. Codex receives that framing and native `localImage` items. Copilot receives the
same framing and its native file attachment list with matching display names.
ACP receives framing and base64 image content only
when the negotiated agent advertises `promptCapabilities.image`. Unsupported image input
fails before ACP activates the prompt. Text-only prompts bypass attachment worker setup.
PDFs and other files use explicit retained-file context in Codex and ACP, which directs
the agent to inspect the complete file through tools. These adapters do not claim native
PDF ingestion or silently substitute extracted text for the original file.

The feature-first Rust crate lives at `nvim/rust/forge/crates/forge-harness`. Its directories
name capabilities rather than layers: `broker`, `session`, `plan`, `goal`, `exchange`, `turn`, `timeline`,
`checkpoint`, `backend`, `storage`, `workspace`, `protocol`, and `control_tools`. `Backend` defines
the complete provider contract once, while `CodexBackend`, `CopilotBackend`, and `AcpBackend` own their private
transports and expose capability values that drive broker and editor behavior. `CodexJsonRpc`
remains private to the Codex implementation. `CopilotEventDecoder` remains private to the native
Copilot SDK implementation. Consumer-owned traits stay beside the feature that consumes them.

`AcpBackend` owns one stable ACP v1 connection and collected process tree per Harness root.
Claude Code uses the published Claude ACP adapter. Antigravity uses its standalone ACP server.
The official Rust ACP SDK frames transport requests and dispatches permission callbacks independently
of pending prompts. Harness control tools use a required, private loopback MCP endpoint. Its connection
tasks share the endpoint lifetime and admit at most 16 connections. Headers and bodies retain
complete requests without hardcoded byte-size admission limits.
Provider restart retains the native conversation identity and reloads that conversation. Failure to
load it fails configuration rather than creating replacement history.

`ControlToolRegistry` defines provider-neutral tool names such as `agent_spawn`,
`agent_send`, `agent_activity`, `design_patch`, and `repository_status`. Names use
a domain followed by an action or query, without a `harness_` prefix. The current
LSP query tool is `lsp`. Provider MCP namespaces identify the server separately.
Catalogs, dispatch, prompts, and activity handling use the same names. Old prefixed
names are not registered as aliases.

ACP tool updates merge by native tool-call identity, so sparse completion notifications retain
their original title, category, and content. Structured diff content becomes a `ProviderChangeSet`
with unified diffs from the provider's reported before and after text. A null before value identifies
a new file. Missing or invalid baselines do not fabricate a file change. Diff matching shares a
250-millisecond deadline per notification, after which the diff algorithm uses its valid coarse result.
Completed tool changes enter the existing exchange and timeline projections without reading the
workspace to reconstruct a provider baseline.
Antigravity edit tools can report completion without structured diff content, so `/capabilities`
marks provider file diffs as limited. Forge still records the completed file tool and retains its
independent workspace checkpoint review.
Tool output uses explicit text content when available and otherwise retains ACP `rawOutput`.
Antigravity command results retain their combined output and exit code without changing the
provider's completed status. Structured results and provider error text enter durable tool history.
Advertised ACP child sessions enter the shared agent registry through structured spawn and state
updates. Each native child generation receives one normalized Harness turn identity. Child message,
reasoning, and tool events route by their ACP session identity into that child's exchange, while
unknown or completed child content never reaches the root transcript. Nested children retain their
parent session. Child approval callbacks carry the child's execution address. Child model, command,
usage, and compaction metadata cannot replace the root session's metadata. A prompt admits at most
256 child sessions and clears its routing map on the next prompt. When the root prompt returns,
unclosed children become interrupted, with nested children finalized before their parents.
`/capabilities` reports child observation as limited because this boundary observes active
prompts and does not expose independent child control or background observation.

`AcpRuntime` resolves explicit commands and pinned Forge-managed installations. `/config` exposes
Provider setup for Claude and Antigravity. Installation writes into a temporary directory under
`provider-runtime`, validates the expected executable, then publishes the version directory atomically.
An OS file lock excludes concurrent installers. Failed installation preserves existing versions.
Claude uses its pinned npm package and requires Node.js 22 or newer. Antigravity uses Google's
platform-specific archive with complete download and extraction, rejecting escaping paths and symlinks.
Neither installer changes global CLI installations or shell configuration.
Setup and authentication hold session execution and cleanup admission, so they reject active work.
The setup handshake advertises terminal authentication support. Claude OAuth runs the advertised
CLI in a Neovim terminal, while Antigravity OAuth uses its ACP authentication request. Terminal
success refreshes the provider connection without creating a model conversation. Login failures and
cancellation release the editor configuration lock, and sign-in has a 120-second timeout.

Claude sessions use strict MCP configuration and session-scoped native ask rules. Forge evaluates
callbacks through `PermissionCoordinator` and returns native once-only decisions. Mandatory callbacks
remain answerable in Yolo. Native provider policy can still reject an action before a callback reaches
Forge. Antigravity enterprise terminal policy can also execute a command without an ACP callback, so
its approval capability remains limited even when personal OAuth approval checks pass.
`BackendKind::supports_sandbox` owns the OS sandbox allowlist, which currently includes only Codex.
Portable settings resolve sandbox to off for Copilot, ACP providers, and the test adapter on session
creation, recovery, and configuration. Shared requested preferences remain independent of the effective
provider policy, so switching to Codex preserves its requested isolation. Child profiles and their
requests resolve access against the child's provider rather than inheriting unsupported root isolation.
The configuration picker uses the provider's `os_sandbox` capability, displays `Disabled (unsupported)`,
and prevents enabling unsupported isolation.

## Settings, resources, capabilities, and models

`SettingsStore` owns revisioned global, workspace, and session layers in `settings.db`. Unspecified
fields inherit. Resource selections overlay by canonical source identity, and provider tuning remains
scoped to its backend. One SQLite statement reads the revision and every inherited layer from the
same committed snapshot. `settings.update` and `settings.reset` require the expected revision and route
application through task transition admission. `setting_application` distinguishes the desired revision
from the confirmed applied revision and runtime generation. A failed continuation does not invalidate
an already confirmed configuration. Configuration timeline records share the durable operation identity
and contain changed field names rather than settings values. Successful configuration records remain
in storage for auditing but do not produce timeline rows. Failed configuration records remain visible.
The service persists applying,
verifying, and resuming phases from ordered broker events before forwarding their task state.
Confirmation records the applied revision before any interrupted work resumes.
Settings mutation commits its revision and operation receipt in the same transaction. A retry with
the same operation ID and request recovers that revision before concurrency validation, including
after a process restart between preference persistence and task admission. A reused ID with different
content rejects without changing preferences. Task admission deduplicates the recovered operation,
so a completed retry does not restart the provider or append another configuration timeline record.
Only a pending task operation can supply continuation intent. Completed idle configuration does not
suppress a later active chat, while an explicit pending pause prevents a new setting from resuming it.
Both startup and `/new` resolve saved provider defaults through `SettingsSnapshot` before persisting
a new conversation. Resuming an existing conversation preserves its selected model and tuning, even
when another window has changed the defaults. Configuration admission and completion invalidate MCP
discovery, including pending jobs, so results from the previous runtime cannot replace the new catalog.
Native forks copy explicit session overrides into an independent preference layer while retaining
global and workspace inheritance. Claude's native fork carries the source session's selected MCP,
skill, and approval bindings. Loading the fork rebuilds its private profile for the new Forge session.
ACP initialization rejects agents without conversation loading, which the required resume contract
depends on. Planning selects an advertised plan or architect mode before provider execution.
Model discovery can create an empty ACP session before a prompt. Forge retains its conversation ID
for restart recovery only after prompt admission or a successful native history load, because
Claude cannot load an empty conversation that has never reached its history store.

`/config` exposes an explicit native resource import. `NativeImport` reads JSON or TOML MCP
definitions and native skill directories, then returns source identities, transport metadata,
collisions, and validation failures. The preview omits launch values and prompt bodies. Apply
requires the reviewed fingerprint, explicit selected identities, and the preference revision used
for conflict inspection. A changed source rejects before persistence and refreshes the preview.
Native source reads and skill-directory scans run on a blocking worker so filesystem latency does
not block the async provider control lane.
Imports support global and workspace scope. `setting_resource` stores definitions in the same
transaction as their disabled selections, revision, and retry receipt. `SettingsStore::catalog`
reads bindings and selection intent from one committed snapshot, so concurrent imports never
appear enabled between separate catalog and preference reads. Every backend uses that catalog.
Application uses the existing task transition coordinator and durable configuration timeline event.

Imported skills retain their source file and metadata. Imported MCP arguments, URL, environment,
and headers retain references into the original native file, so Forge does not copy literal
credentials into its database. Launch reads one complete snapshot per source, preserves literal
values, and validates the imported structure before resolving references. Credential rotation
preserves the binding. Added policy fields or changed launch structure require another explicit
import. MCP files, skill definitions, previews, and resource inventories retain their complete
content. Nonportable native options are blocked in the preview, including
tool restrictions, timeouts, and working directories that ACP cannot preserve. Imports never
write native configuration, authentication, or Rulesync-generated files.

`ResourceCatalog` reads MCP defaults from `config_dir/rulesync-global/.rulesync/mcp.json` and workspace
`.rulesync/mcp.json`. Skill catalogs read the corresponding Rulesync skill directories. Catalogs retain
definition identities and metadata without prompt bodies or launch credentials. Catalog entries
carry explicit `Global` or `Workspace` source scope into `/skills` and `/mcp`, independently of the
scope of enablement preferences. Native imports recover this scope from their stored owner, even
when their source path is outside that workspace. Provider-only entries show `Provider` rather than
inferring a scope. Both pickers expose the source column and expand the definition path with Tab.
MCP details retain the cached tool list and complete failure reason. The final Status
column shows that reason, and Tab expands it with the source and tools. Runtime status
refreshes preserve source metadata.
Routine discovery refreshes retain the last observed status and tool list while the
request runs. Only servers without an observation show starting. Configuration changes
invalidate that observation together with the session's discovery job.
Environment references resolve only into ephemeral provider launch payloads. Missing credentials mark
only the affected MCP resource unavailable and omit it from launch, including explicitly enabled
resources. The saved selection remains available for retry, and catalog, model discovery, and other
servers continue. Providers isolate startup failures after receiving a valid launch payload. Stdio MCP launch preparation
checks the executable against the server's working directory and effective PATH, including Windows
executable and command-script suffixes and Unix executable permissions. Missing commands stay visible
as unavailable rows and are omitted from every provider's launch payload without changing saved
enablement. Before a prompt and after configuration changes, the broker logs `mcp.start_skipped` and emits a warning notification for
each newly unavailable command. Repeated prompts suppress an unchanged warning until availability
or the failure reason changes. HTTP and SSE servers do not require a local command. Claude resource selection uses
strict MCP configuration and an ephemeral `forge-managed` plugin whose selected skill directories bind
to canonical sources. Its SDK skill filter admits only those qualified plugin names. Antigravity starts with a private `GEMINI_HOME`
configuration root. That root binds only selected skill definitions and links its private ACP state
directory to the native OAuth and conversation store. It does not copy credentials or inherit native
global MCP configuration. Windows directory bindings use NTFS junctions and Unix bindings use
directory symlinks. Cleanup removes bindings before the temporary profile and preserves the source.
`ResourceProfile` owns these bindings across adapters. Copilot receives an isolated skill directory
and an empty MCP set through its Rust SDK session options with native configuration discovery
disabled. It starts each selected MCP server through the session-scoped `session.mcp.startServer`
API using the canonical launch payload. This preserves required empty argument arrays that the
SDK's session-option serializer omits, without modifying native user configuration. It verifies
loaded skills and disables unselected runtime resources, and disconnects before
resuming the same provider conversation when selection changes. Selected MCP servers start with
at most four concurrent requests and an independent 30-second deadline per server. Startup errors,
timeouts, and missing or disabled registrations produce `copilot.mcp.start_failed` trace events
without rejecting the conversation. `/mcp` retains each failure until the provider reports recovery,
including failures before native registration, and exposes the existing enable and retry controls.
Failure to enforce skill or server selection still rejects initialization. `SessionResources` exposes source
identities and per-server credential failures independently from its ephemeral launch payloads.
Resource binding rejects names that traverse or alias profile directories before creating links.
Selected skills own execution-scoped read and script grants through the shared permission coordinator.
Each grant covers one canonical skill bundle, including its references and assets, rather than the
parent skill collection. Provider profile aliases resolve through directory bindings before containment
checks. Shell grants require absolute bundle paths because permission callbacks do not consistently
report each command's working directory. Escaped links, disabled bundles, inline interpreter code, and unrelated compound commands
receive no automatic grant. Explicit denials and native sandbox restrictions still apply. Dropping the
execution guard revokes the grants without writing the permission document.
The shared skill observer reconciles tool inputs and successful completions across provider delivery.
Recognized file reads and shell reads of selected `SKILL.md` files produce one durable skill-read node
per skill per agent turn. Failed reads, listings, filenames in output, and script execution alone do not.
The timeline nests `◇ Read skill · <name>` inside its exchange activity, alongside tool groups,
and opens the canonical source
through the existing file action. Original tool history remains available. Internal provider loads that
emit no observed read remain undetected. The current session payload retains these nodes across reload.
Codex conversations with shared settings own a private app-server runtime. Resource changes collect
that runtime and reload the same native thread with the newly resolved configuration. Other sessions
retain their provider processes. The unmanaged SDK constructor retains its shared-runtime behavior.
Each configured runtime disables inherited MCP definitions and plugin resources in the thread's
configuration, registers canonical MCP servers under stable private names, and maps runtime status
back to canonical catalog identities. Selected skill directories use runtime-local extra roots, with
explicit per-path enablement rules that exclude inherited skills. No native preference file is written.
MCP launch configuration is redacted from outgoing and incoming protocol traces, including arbitrary
environment-variable names and HTTP headers. Runtime launch payloads remain separate from catalog
metadata and saved settings.
Codex MCP callbacks accept the provider's `_meta` approval envelope. Active tool records supply
the tool identity when the callback omits it, and server aliases resolve to canonical names before
permission matching and timeline publication. Completed calls cannot supply identity to a later
request. Ambiguous concurrent calls retain a server-wide approval target.
Copilot turn addresses include the provider interaction identity because native turn counters can
repeat across prompts. Later messages and completion events resolve through the matching turn-start
record, so a reused counter cannot attach a new response to a completed execution.
Root session idle settles any unfinished root turns before exchange completion, including the empty
turn Copilot can start while an accepted execution-phase boundary is stopping the provider.
Copilot fork checkpoints reference persisted events. An ephemeral idle notification contributes
its durable parent ID, because the idle ID itself is absent from native history. Forking a
conversation without a recorded boundary still creates a distinct native conversation.
Copilot SDK subscriptions drain through independent `SessionEventStream` tasks before tracing,
provider RPCs, or presentation processing. The prompt stream excludes raw
`assistant.streaming_delta` and `assistant.tool_call_delta` telemetry, while retaining structured
message, reasoning, tool, usage, and lifecycle events. Compaction and terminal monitors admit only
their event types. Each intake uses a typed queue with the shared pending-frame count as its
capacity and preserves event order while waiting for space. SDK events do not pass through host
JSON frame encoding, so complete tool requests remain admissible regardless of payload size.
Dropping a consumer aborts its intake task.
Intake retains a client handle and session identity rather than owning the SDK session. Before a
tool-schema rebind, the adapter stops and joins the old terminal monitor, then disconnects and drops
the old session before registering its replacement. Catalog requests reuse an attached session
without changing its tool schema. Only a request with a turn control context can refresh the
schema, so catalog reads cannot disconnect an executing turn from its event stream.
SDK lag notices are logged without ending the turn. Intake compares the next queued event's
parent with its last durable event and replays any missing durable history before that event.
Missing transient deltas are superseded by completed message snapshots. Closed subscriptions,
missing recovery anchors, and failed history reads remain explicit errors. Intake records
subscription failures with their full cause chain and native session identity. After draining
accepted events, the consumer joins the intake task to distinguish subscription failure, panic,
cancellation, and an unexpected successful exit. Terminal and compaction monitors preserve that
cause chain. Provider session errors retain event identity and error code, and explicitly identify
missing provider diagnostics rather than substituting a generic failure.
Isolated text generation identifies failed SDK operations and deadlines. Cleanup collects abort,
disconnect, and deletion failures without replacing the generation error, and SDK shutdown retains
both errors when generation and shutdown fail together.

`CapabilityManifest` records required and optional operations, support, ownership, application boundary,
and runtime availability. The `Backend` trait requires explicit identity, streaming, shutdown,
session cancellation, execution cleanup, active-turn inspection, and retained conversation identity.
The broker persists the root conversation ID when it accepts a provider turn-start boundary,
before that turn finishes. Interrupting the first turn therefore retains the conversation even
when the adapter has already settled its active-turn state. Synthetic display addresses and
rejected child events cannot replace the root conversation ID.
Adapters cannot inherit a synthetic provider identity or silently omit lifecycle ownership. Unsupported
optional operations return typed unsupported-capability errors. Cancellation and identity queries carry
the Harness session ID, preventing unscoped operations from affecting a different conversation.
`/capabilities` displays that manifest in a shared picker. ACP discovery refines
native fork, steering, session loading, and effort support from the negotiated agent and session catalog.
The manifest separates MCP selection from connection-status and tool-inventory inspection. ACP can
run selected MCP servers without exposing either inspection operation. Its configured rows retain
an unavailable status with that explanation, and live workflow tests verify execution through an
actual tool result rather than treating configuration as readiness evidence.

`BackendModel` forms the provider-neutral model-picker contract. Each backend supplies the model
identifier, ordered reasoning choices, context-window tiers, vision capability, and optional
description. It never supplies a display label because the model identifier already owns that
identity in the UI. The broker overlays durable reasoning and context selections per model before
Lua receives the catalog. Opening the picker reloads this catalog so saved selections cannot remain
stale after a configuration change. `views/harness/model_picker.lua` owns only picker interaction
state, field cycling, and presentation order. Copilot maps the selected context tier into native
session creation and `set_model`, while Codex exposes only the controls returned by app-server.

Codex resolves the `default` selection through its model catalog before admitting a turn.
The resolved model is sent explicitly on thread creation, thread resume, and turn start,
and the same value supplies the runtime model shown in Harness. Omitting the model after
resolution can retain a different provider thread default while displaying the catalog default.

Codex requests concise reasoning summaries on thread creation, thread resume, and each turn start.
Copilot requests concise reasoning summaries on session creation, session resume, and model changes.
The setting only affects models that support configurable summaries. Claude and Antigravity's
current ACP integrations expose no corresponding summary-detail control. Summary preferences
do not change reasoning effort or assistant commentary.

Codex persists the control runtime's returned invocation after a successful tool call.
This preserves lowered operations such as adding plan checks as canonical design patches
and omits operations the runtime already committed. Subsequent patches therefore replay
against the same document version the model observed.

`views/harness/model_order.lua` applies one ordering pipeline to the model picker and
model completion. Copilot orders Auto first, then Claude, GPT, and Gemini with descending
numeric versions. Equal Claude versions use Opus, Sonnet, and Haiku order. Other equal-version
variants use identifier order, and unrecognized models retain provider order at the end.
Codex and other backends preserve provider order. A stable partition then puts pinned models
first without a separate section, retaining each backend's order within both groups. Lua keeps
the original catalog unchanged so unpinning restores provider order exactly.

Rust's `ModelPinStore` persists backend-wide membership in the `model_pin` table of the Harness
SQLite database, independently of workspace preferences, session leases, and session formats.
`backend.model_pins` reads membership, and `backend.model_pin` sets one explicit pin state
idempotently. The service runs pin storage outside the provider broker lock, allowing changes
during active execution without configuring or contacting the provider. Unavailable model IDs
remain pinned in storage but produce no synthetic catalog entries.

The picker refreshes persisted pins when opened and exposes `p` to pin or unpin its highlighted
model. The inline `*` marker and row order change only after storage acknowledgement. The shared
Lua pin cache serializes writes per backend and rejects reads superseded by acknowledged writes.
Picker updates preserve the current model identity and edited reasoning/context fields. Closing
or replacing the picker invalidates UI callbacks without undoing committed pins. Storage failures
retain the previous presentation and report the request error.

`BackendInput` distinguishes ordinary text from explicit skill invocation before either provider
sees the prompt. `SkillDefinition` normalizes discovery and enabled state, while each `McpDefinition`
owns its tool inventory so callers never issue a second tool-list operation against the backend.

## Provider activity normalization

Stable tool identities merge start, output, and completion events into one canonical tool record.
Codex `mcpToolCall` items follow that same path. `CodexJsonRpc` converts their app-server
`server`, `tool`, and compact JSON arguments into one `server.tool(arguments)` title. Before that
title enters durable timeline state, the transport recursively replaces values named `token`,
`api_key`, `key`, `secret`, `password`, `passphrase`, `authorization`, `bearer`, `cookie`,
`session`, `credential`, `access_token`, `refresh_token`, `client_secret`, or `private_key` with
`[REDACTED]`. It does not guess from value shape, preserving useful hashes and identifiers.
`item/mcpToolCall/progress` replaces mutable previews, while completed result JSON is pretty
printed before rendering. MCP payload details stop at the Codex transport boundary, so
`ToolActivity`, `Turn`, and the Rust timeline projection represent every provider action as a
generic tool.

Codex activity classification reads the top-level item type rather than searching serialized
payload text. Errors, messages, reasoning, plans, compaction, review transitions, and native
child lifecycle items remain separate from tools. Every other typed `item/started` or `item/completed`
with a nonempty item ID becomes a tool activity, including `webSearch` search and page-open
operations. Known web actions use `Web Search`, `Open web`, and `Find in page` titles with
their query, target, and additional-query count. Before Codex supplies an action, the title is
`Web Search`, with the query appended when available. Later events replace that pending title
with the specific action. Unknown actions retain the fallback title. Native sleeps use `Wait`
with the requested duration.
Activity types without a dedicated title use `type(compact JSON)` with the complete
item payload, including queries, actions, results, and provider metadata. The fallback title uses
the same credential redaction as MCP arguments and updates as lifecycle events supply details.
The renderer clips only the visible header to the window width. Completed items remain available
in full as JSON output. The provider thread, turn, and item IDs correlate live
updates with durable records. Main and child timelines, activity summaries, and
`agent_tool_output` consume those same records without provider-specific rendering.
Missing IDs or untyped payloads cannot create fallback tool records.

Shell command headings summarize simple file reads, directory listings, and text searches
after decoding PowerShell, Bash, or Nushell launchers. Summaries retain supported flags such
as line limits, recursion, and line-number output. `grep` uses `Search`, while `rg` retains
its command text. Compound commands, expressions, and unknown options keep their command
text. The durable command stays unchanged, and expanded output includes that original
command above its output. These display summaries do not alter approval or execution input.

## Provider execution and cancellation

Confirmed terminal network and capacity failures use one shared retry runner for
root, child, and Background requests. Adapters must prove that the attempt
produced no replay-sensitive output before opting in. Five retries wait 1, 5, 10,
30, and 60 seconds. Retry status remains inside the existing exchange, cancellation
uses its existing owner, and exhaustion exposes the final provider error.
[Provider retries](../provider-retries.md) defines the allowlist, native recovery
boundaries, diagnostics, and the procedure for enabling a newly observed error.

`Backend` uses async trait methods for native provider operations. That interface does not
provide execution isolation. Its default session cancellation delegates to execution cleanup,
so an adapter cannot accidentally use a weaker stop path by omitting an override.
`ControlToolExecutor` owns the shared lifecycle used by Codex,
Copilot, Claude, and Antigravity control callbacks. The raw `ControlToolRuntime` remains inside
the control-tools module, so production adapters cannot bypass this lifecycle.

Each executor owns one canonical runtime, a cancellation signal, and admission accounting.
Runtime mutations remain serialized. Cancellation revokes new admissions and interrupts both
the current tool wait and callers waiting for the runtime lock. A completed result retains its
admission until its adapter publishes or discards it. Cleanup collects those admissions before
reusing the provider turn. A new turn receives a fresh executor and cannot accept late results
from the prior generation. LSP cancellation uses the same path as checkpoint, check, repository,
and agent-tool waits.

Native adapters signal stop before collecting local callbacks. Codex sends `turn/interrupt`
before awaiting request-result cleanup. Copilot sends abort before collecting a pending SDK send
response, then repeats abort when needed to cover that send's commit race. ACP cancels callback
authority and stops its owned process tree before collecting tool calls. Adapter code retains
provider framing, native acknowledgements, and thread identities. Shared code owns tool execution
and cancellation semantics.

ACP session preparation holds a per-runtime admission lock. Its published session map remains
available while a provider launches or responds to its startup handshake. Cleanup can therefore
observe an unsubmitted session and return without waiting for startup.
Publication also checks the shared submission-cancellation flag, so a late startup
response cannot reactivate a cancelled prompt.

## Codex turn coordination

Codex callbacks execute outside the socket reader. The connection owns request tasks and
publishes their completed results through the same coordinator that reads provider frames.
Canonical control tools share an ordered executor. Each waits for the preceding result to
publish before executing, while approval callbacks have independent waits. An LSP lookup
or approval therefore cannot prevent the coordinator from receiving
steering, cancellation, or another provider event. Cleanup revokes pending request tasks,
sends native interruption, then collects callbacks and preserves completed tool results.
Turn completion and transport failure also collect the request tasks.

Background terminal inventory uses a separate passive connection per runtime owner. It
reads an already-loaded thread without resuming it. When loading is necessary, `thread/resume`
retains the session's canonical MCP and skill configuration, without overriding model, effort,
service tier, or instructions. Subsequent polls reuse that connection and read every inventory
page. A changed thread or failed request invalidates the cached connection. Passive traffic
does not execute broadcast tool callbacks or publish them as foreground activity.
Resource replacement clears the terminal connection while holding its poll lock,
before shutting down the old Codex runtime. Shared terminal observation pauses during
configuration and discards results from an older configuration revision. Lua retains
the previous terminal inventory during this pending state. Unrelated failures remain
visible and the next poll can reconnect.

The Codex `CodexTurnCoordinator` treats one user request as an exchange that may outlive
its first parent app-server turn. A `turn/start` acknowledgement admits its returned turn before
content reaches the exchange. This also covers input attached to an already-running native-goal
turn, where no second `turn/started` notification arrives. Admission excludes unrelated turn
events so cancelled activity cannot populate a later exchange. Cancellation pauses an observed
provider goal even when Harness did not create that goal, then waits for interruption evidence
and cleanup acknowledgements. The coordinator
retains the session's JSON-RPC connection while descendant threads remain active, accepts steering
as another parent turn on the same thread, and starts a bounded synthesis turn after the final child
completes. Child lifecycle updates change the child agent's exchange behind the existing
`AgentReference`, so they never move the child row. Codex `subAgentActivity` values enter that lifecycle only for explicit
start or terminal states. Directed `interacted` activity carries messages between agents without
creating a run, and the descendant tracker rejects the parent thread identity unconditionally.
`ActiveWait` drives only the current
Timeline Status at the end of the Main timeline. The status uses an animated spinner followed by
`Waiting for N subagents` and carries no duration because it represents current control state, not
history. A submitted plan in `AwaitingReview` renders `Waiting for plan review` with the configured
artifact key until the reviewer opens PlanReview. Clearing `ActiveWait` removes the status without
creating a timeline node. A normal
Harness submit while this status remains active uses the parent steering lane immediately. Ctrl-q
retains its explicit steering shortcut, and child timelines never project Main control state.

## Session startup and provider switching

Broker initialization selects the most recently updated session for the resolved repository
and configured backend, then restores its interaction timeline, plan, goal, model controls,
and provider session identity. Independent model, effort, and service-tier preferences remain available
when older sessions become invisible. `:ForgeHarness` therefore resumes current repository-local work across Neovim restarts,
while `/clear` remains the explicit boundary for creating a new session. Resumed sessions retain
their persisted execution mode. New and forked sessions establish a fresh Read boundary.

Preference loading requires both the current version and a nested `preference`
object. Records with an incompatible envelope are treated as absent without
rewriting or deleting their rows, including older flat records carrying the same
version number. Valid current preferences remain independent of session history.

The Provider row in `/config` and `/backend` open the same shared picker over selectable providers.
Selecting another provider requires an explicit New chat or Resume session destination. The resume
picker searches and previews only that provider's sessions in the current workspace. Switching is
unavailable during active turns, queued prompts or steering, pending configuration, and another switch.
Cancellation before startup preserves the active chat. Confirmation stops the host and initializes the
selected provider directly into a fresh session or the exact selected session, displayed in the current
tab. Rust validates the selected workspace and provider before acquiring its lease. Sessions retain
their own conversations, plans, goals, and settings without a cross-provider fork or context transfer.
An already-open destination moves its existing buffers into the current tab, preserving its unsent
draft and closing its previous tab. Switching back reuses retained session buffers without creating
duplicate buffer identities. Presentation callbacks reject obsolete session and host generations,
and shutdown events cannot start state reconciliation while a provider switch owns the host.
Neovim stores the last successful selection in
`stdpath("data")/forge/harness/backend.json` before the next editor launch. An explicit
`setup({ harness = { backend = ... } })` remains authoritative, and failed switches restore the
exact previous session and backend rather than persisting a broken default. Cancelling lease-conflict
recovery also restores that source session. Ordinary Harness startup still resumes the latest
same-workspace session for its configured provider.

## Provider startup and tools

Codex background-terminal requests resume their owning thread on an independent connection.
A newly created thread can become visible before its rollout metadata is written. For the
specific `-32603` empty-rollout response naming that thread, the backend retries resume at
500 ms intervals, up to four retries and two seconds of added delay. Exhausted retries preserve
the provider error for the terminal observer to report. Other provider and transport failures
propagate immediately. Recovery never substitutes an empty terminal inventory for a failure.

Harness defaults to the direct Codex app-server backend. Set `harness.backend = "copilot"` to use
the native Copilot SDK. An empty `harness.backends.copilot.command` delegates CLI discovery and
startup to the SDK. A nonempty command selects an explicit Copilot CLI executable and prefix
arguments. Persisted provider session IDs resume through the same backend, and backend mismatches
remain invalid session transitions.

Copilot sessions explicitly disable tool search on creation and resume. The runtime preloads
all enabled tool definitions, keeping Harness controls and MCP tools directly available instead
of routing them through deferred discovery. This increases schema context usage but prevents
an empty discovery result for an already-active tool from blocking planning.

Copilot's native session metadata supplies its session-state directory. The adapter
automatically approves file reads and writes confined to that directory for the
matching native session, including in Read mode. It resolves paths and symlinks
before checking containment. Other sessions, provider configuration, commands, and
mixed requests retain normal permission handling. Explicit denials and protected
Harness files remain denied. Missing or relative directory metadata grants no access.

## Native delegation exclusions

Codex disables `agents.enabled`, Copilot excludes its builtin-qualified native delegation tools,
and Claude ACP disallows `Agent` and `Task` on session creation and resumption. Antigravity ACP
exposes no equivalent native suppression selector, which `/capabilities` reports as unsupported.
Native lifecycle notifications remain diagnostics and cannot create or bind managed identities.
Cross-provider selection rejects any child provider unable to enforce the root's OS sandbox,
including Copilot and ACP providers, both at spawn and on later follow-up admission.

## Bounded normalized provider event delivery

`forge_harness::backend::events` owns normalized event delivery from providers to primary and
child reducers and from reducers to the root transport forwarder. The queues use the protocol's
encoded admission mechanism. Each queue admits at most 96 ordinary records with a 512 KiB
individual encoded limit and an 8 MiB budget that retains the existing 128 KiB control reserve.
Serialization reserves capacity before allocating the encoded record. Decoding occurs at the
consumer, so queued records do not retain arbitrary provider object graphs.

Send operations remain synchronous and never wait for presentation capacity. Exhaustion poisons
the affected queue and wakes its consumer. This failure remains observable when a provider
callback discards the send result. Healthy delivery preserves FIFO order without replacing deltas.
Saturation terminates delivery explicitly rather than promising lossless retention under unlimited
production. Approval responses and cancellation do not wait for room in these event queues.

Primary and child reducers observe delivery errors and stop the affected provider transport.
The primary reducer uses its existing failed-interaction and paused-goal persistence path.
After a provider returns, the reducer checks queue failure again before accepting completion,
including failure encountered while draining the remaining events. The root forwarder propagates
both receive and transport-send errors. Timeline emission also propagates admission failure.

These limits cover normalized event queues, not the entire provider runtime. Provider SDK buffers,
retained transcript/output collections, steering and Copilot control-tool queues still need their
own bounds. Credit-driven suspension of optional output generation, shared-host feature routing,
and complete snapshot-transfer routing remain separate work.
