# Providers

Each provider reports identities, phases, tools, and failures differently, and a consumer that decoded those differences itself would show one provider's quirks as another's behavior. Provider adapters therefore normalize every transport into one typed Harness contract, and shared settings record real capability differences instead of simulating unsupported operations.

[Harness](harness.md) covers the session lifecycle, [Permissions](permissions.md) covers approvals, and [Provider configuration](provider-configuration.md) covers settings, resources, and models. Input delivery is in [Provider submissions](../provider-submissions.md).

## Provider contract and ownership

The provider boundary spans three crates. `forge-provider-api` defines the object-safe `Provider` interface, normalized requests, events, errors, models, and capabilities. `forge-providers` owns native protocol decoding, process runtimes, SDK handles, resource profiles, and session state; its native modules are private behind the registry. `forge-harness::provider::ProviderBridge` prepares host resources and binds normalized callbacks to tools, permissions, task authority, and durable submissions. The API and native implementation crates do not depend on Harness, and the interface has no Harness-associated types.

Only `forge-providers` declares the Copilot SDK dependency; Harness has no direct SDK dependency, including in tests. Native wire, SDK ancestry, version, and permission conversion regressions live beside their private implementation. Harness integration tests implement the same normalized interface through `EventFixtureProvider` and the production bridge. Shared mocks exercise acceptance, completion, cancellation, permissions, and host effects without knowing a provider's wire format.

Every provider operation receives a host-issued `OperationContext` containing its identity, immutable deadline, and explicit cancellation observer. The host retains the matching cancellation source with the original worker; losing a result waiter never requests cancellation. Repeating the same context and typed request observes the original disposition, while changed identity/request combinations reject. Opening transfers the sole stream once; observing an already-taken open result cannot allocate another binding. Deadline and explicit cancellation failures retain their operation, phase, and effect disposition, and never imply that input was not consumed.

`Provider::session_open` atomically returns an opaque binding, its initial negotiated capabilities, and its sole ordered event stream. The bridge installs stream consumption before dispatching input. The engine can subscribe to the host stream before native initialization and retains it across result waits. `input_submit` and `input_steer` report input acceptance or delivery failure. Normalized `ExecutionUpdate` events carry execution start, terminal coverage, or lost observation; acceptance alone does not settle an exchange. Root, child, and Background callers use the same engine, while retaining their own presentation and result waits.

The native owner proves completion and exact accepted-input coverage before publishing a terminal update. A separate `Settled` observation proves native execution, readers, and invocation callbacks were collected. `ProviderHostEffects::settle_foreground` drains callback effects through that native boundary; Harness subsequently applies its retained tool results, plan and file effects, child results, and checkpoint work through its own commit ticket. Native settlement and host commit are separate barriers. Publication failure preserves the native result in a distinct typed update; a terminal response with no native execution has its own resolution variant. Harness never assembles native completion certificates. Every observation carries a monotonically increasing binding sequence. Configure and cancel results include watermarks; the owner consumes through them before dependent transitions. Root and Background brokers drain an engine-owned inbox; child runners attach their scoped presentation stream. Observation failure fences further admission and initiates native cancellation and collection. Retained task handles make cleanup failures observable, and accepted events drain before the first observation failure.

`ProviderEvent` is an exhaustive enum carrying invocation ownership. Compaction uses a separate session-operation scope instead of inventing an exchange. Production providers expose neither `prompt` nor `prompt_stream`; output-oriented fixtures use a test-only collector above submission and events.

`ProviderTools`, `ProviderPermissions`, `InputReceipt`, `ProviderTrace`, `TurnControl`, and `ProviderHostEffects` are callback boundaries into the host. Native code asks for a tool result, permission decision, or committed receipt without acquiring Harness storage or task state. Prompt requests carry full invocation configuration, the current tool allowlist, typed task commands, structured question replies, and ordered recovered input with its attachments. Harness owns goal scheduling and enqueues ordinary input for continuation; adapters never schedule native goal work. Initial tool registration remains stable; each invocation enforces its own allowlist. Where an adapter needs HTTP MCP transport, the request supplies a unique endpoint and retained lease for that invocation. Direct native tool callbacks use `ProviderTools`. A retired endpoint cannot route delayed calls to a newer invocation. `forge-shell` separately owns shell parsing and process primitives used by the policy layer and native runtimes.

Provider errors expose typed operations and categories, never native response codes, messages, payloads, or source chains. Diagnostic display messages are deliberately sanitized by the producer. Provider tracing accepts normalized lifecycle records only. Setup reports runtime readiness, provider and adapter version observations, and opaque authentication choices independently; a failed version probe does not remove otherwise available authentication actions.

`ProviderCapability.integration` declares optional operations, resource targets, isolation choices, and implementation limitations in each adapter. The compiled-in registry supplies identities, launch defaults, and declarations before a session starts; Lua does not maintain a duplicate provider catalog. `CapabilityManifest` records each operation's support, ownership, typed restrictions, and application boundary. Open and configuration results publish effective support; later changes arrive on the ordered binding stream. The bridge retains the snapshot consumed by the owner, and `/capabilities` reads that snapshot without calling native discovery. Schema and effective revisions have separate counters. Unsupported optional operations return a typed feature error. Cancellation identifies both the binding and exact invocation, so a repeated or delayed request cannot interrupt a successor. Terminal operations use a binding-scoped typed terminal identity.

## Sessions

Native initialization validates the selected runtime before opening or resuming its conversation. The current minimums are Codex 0.164.0, Copilot CLI 1.0.103, Claude ACP 0.99.1, and Antigravity ACP 1.4.5. The Copilot SDK dependency is pinned separately to 1.0.26; its version does not prove that a configured CLI override is supported. Codex reads its initialization user agent, Copilot queries the connected runtime status, and ACP reads the adapter version from the initialization handshake. Missing, malformed, and below-minimum versions produce typed initialization errors. A replacement runtime must validate again.

Conversations open or resume their provider session asynchronously, without creating an exchange or sending model input. The controller releases its broker lock during network work and rejects stale results after configuration changes. The broker persists the root conversation ID when it accepts a provider turn-start boundary, so interrupting the first turn still keeps the conversation.

Saved sessions use host contract version 61. Explicit resume rejects older or missing versions before decoding changed session state or opening native history, with a create-new-session diagnostic. Listing omits unsupported records while retaining their stored history. New-contract sessions remain resumable.

Harness defaults to the Codex app-server provider, and `harness.provider = "copilot"` selects the Copilot SDK. Broker initialization resumes the most recently updated session for the repository and provider. Switching providers requires choosing New chat or Resume session, is unavailable during active work, and never transfers context across providers. A failed switch restores the previous session.

Copilot's native session registry serializes preparation per execution owner. It retains repair notices, resource leases, terminal readers, and compaction subscriptions with the matching binding. Replacing submitted history disconnects its old SDK router before resuming the same ID; replacing a provisional session keeps the old binding until preparation succeeds. The host marks a specific binding as submitted before input can reach the provider, so an output-free cancellation cannot accidentally publish a new history boundary.

The published Copilot SDK still has internal unbounded queues and can hide router-level event loss. Forge-owned queue bounds do not remove that upstream limitation. The accepted scope and possible future SDK patch are documented in [limitations.md](../../../../limitations.md).

## Messages

Copilot reads `assistant.message_start` metadata before deltas and reconciles the matching `assistant.message` snapshot by message identity. Recognized phases are `commentary` and `final_answer`, and missing or unknown phases stay unclassified. If a successful invocation ends with commentary and no explicit final answer, the adapter emits that text again as a response with a new identity. An explicit empty final answer suppresses this, and failure or cancellation never produces a response. Root `session.idle` supplies a terminal candidate. It finishes execution only after in-flight sends settle and every accepted steering message is covered by native input observed before that idle boundary. An older idle cannot cover a later input. Foreground or child idle alone does not finish the root execution.

Copilot correlates each accepted message ID with a root-envelope `user.message`, its native turn, and the event ancestry leading to completion. Root inputs can carry `parentAgentTaskId`; that field does not identify a child subscription. Events with an `agentId` cannot supply root input coverage. Ephemeral streaming and idle events refer to durable ancestry without becoming the parent of subsequent durable events.

Codex keeps the phase from `item/started` for matching deltas and snapshots. ACP assigns message identities within its execution. Adapters never assign tools or checks to commentary, because `Exchange` captures each activity's owner at admission.

## Tools

Tool requests, input fragments, executions, output, reported changes, and outcomes are separate typed updates correlated by one scoped tool identity, so late output cannot move a tool or restart its execution. Each adapter also supplies `ToolSemantics`: a command with its shell and working directory, explicit read targets, or an unclassified operation. Skill attribution consumes that metadata and waits for successful completion. The broker and renderer never infer an operation from a native tool name or argument schema.

Codex classifies activity by top-level item type. Before titles enter durable state, the transport replaces values under credential-like keys, such as `token`, `api_key`, `password`, and `authorization`, with `[REDACTED]`. Shell headings summarize simple reads and searches after decoding the launcher, and the durable command never changes. ACP tool updates merge by tool-call identity, and structured diff content becomes a `ProviderChangeSet` without reading the workspace. Antigravity edits can complete without diff content, so `/capabilities` marks its file diffs as limited. Reported edits never replace checkpoint-based **Workspace changes**.

`ControlToolRegistry` assembles family catalogs under `tools/`. Each tool has its own file containing its description, schema, and handler. Shared schemas and services stay within their family; provider adapters use the same catalog and invocation boundary. `ControlToolExecutor` owns the shared lifecycle for every adapter's control callbacks. It serializes runtime mutations, and cancellation interrupts tool waits and lock waiters. A new turn gets a fresh executor, so late results from an earlier generation are rejected.

Tool families are organized by purpose:

```text
tools/
  agents/       child-agent tools
  checks/       check definitions and execution
  goals/        goal status and completion
  lsp/          editor source queries
  plan/         semantic file edits and the plan overview
  questions/    asynchronous questions and answers
  repository/   Git status and diffs
  session/      session history and inspection
  config.rs     Harness configuration inspection
```

The plan overview owns its check selection alongside planned tests and manual verification. Tool descriptions use multiline literals, and detailed field guidance belongs to the tool schema rather than the planning prompt. Domain modules such as `plan/`, `checks/`, and `agent/` retain their state and business operations.

Tool handlers return `ToolResult` with explicit `ToolError` variants and conversions from lower-level errors. One response renderer preserves structured failure details and retry information across providers. Turn admission and cancellation remain transport errors so adapters can stop stale work before publishing a tool response.

## Attachments

Attachments resolve against the Harness data root at admission and stay separate from prompt text. Images get each provider's native form: `localImage` items for Codex, the file attachment list for Copilot, and base64 content for ACP when the agent advertises `promptCapabilities.image`. Unsupported image input fails before the prompt activates. PDFs and other files are passed as retained-file context, never as silently extracted text.

## Execution and cancellation

Native adapters signal stop before collecting local callbacks. Codex sends `turn/interrupt`, Copilot sends abort and repeats it to cover a pending send, and ACP revokes callback authority and stops its process tree.

Codex callbacks run outside the socket reader, so an LSP lookup or approval cannot block steering or cancellation. Steering targets the exact active invocation. Shared Harness owns continuation after child completion and submits separately journaled ordinary input with a fresh invocation identity after native settlement and host commit. Adapters never schedule autonomous continuation or reopen settled work.

Copilot intake and shared Harness observation queues grow in memory while their consumers are delayed; they neither truncate valid output nor cancel a conversation because a fixed event count was reached. The encoded transport uses bounded frames and waits for byte capacity; larger events cross it in ordered chunks. Copilot's SDK retains its own finite broadcast buffer, so Forge drains that subscription independently of presentation. SDK lag remains observation loss rather than proof of native completion. Sustained backlog can increase memory use.

Copilot intake closes its foreground sender before acquiring the decoder context to report failure, allowing a prompt that holds that context to finish. The session task drops its adapter future before terminal publication, releasing execution guards during cancellation. A force-stopped client marks its shared runtime generation unavailable; recovery collects that generation before starting or attaching a replacement. Recovery resumes the saved native conversation and never replays an uncertain prompt automatically. `cancel` awaits settlement of the exact invocation while preserving session reuse. `session_close` immediately revokes admission and callback authority, then collects readers, terminals, profiles, and callbacks. Failed close retains its first failure and unfinished ownership for retry. Normal completion collects automatically and publishes `Settled`. An execution start after that boundary invalidates admission rather than reopening committed work; a delayed cancellation still names its original invocation.

Provider shutdown collects bound and provisional session intake before stopping the SDK client. Otherwise SDK detach closes a still-observed subscription, and intake mistakes orderly shutdown for observation loss and force-stops the transport while shutdown RPCs are pending. Successful SDK collection marks every shared session handle stopped, preventing later cleanup from issuing detach or abort RPCs against the closed transport. SDK stop errors remain explicit and retryable.

[Provider retries](../provider-retries.md) defines when a failed request is retried.

## ACP

`AcpConversation` in `forge-providers` owns each ACP v1 connection, handshake, model configuration, and native event state. The shared bridge owns the control endpoint and permission context; the native owner retains the resource lease with its binding. Claude Code uses the Claude ACP adapter, and Antigravity uses its standalone ACP server. Harness control tools reach ACP agents through a private loopback MCP endpoint of at most 16 connections.

Provider restart reloads the same native conversation, and failing to load it fails configuration instead of creating replacement history. Initialization rejects agents that cannot load conversations. Advertised child sessions enter the shared agent registry, at most 256 per prompt, and become interrupted when the root prompt returns. `AcpRuntime` resolves configured adapters and never installs provider runtimes.

## Approvals and sandboxing

All three approval modes keep identical native callback setup, and Harness applies Ask, Deny, or Allow after saved rules and reported-file checks. Native operations without callbacks stay outside that boundary. Antigravity enterprise terminal policy can run commands without a callback, so its approval capability is limited. After a Copilot denial, completion requires a later final answer or Harness handoff, and provider idle without either fails the exchange.

OS isolation is enabled only by an adapter's explicit `os_sandbox` declaration. Codex currently declares support and supplies its Windows isolation modes; the shared access policy retains only an optional opaque `isolation_mode`. Unsupported providers resolve isolation to disabled, and declared mode selections are validated before native execution. Native-agent suppression is a separate capability: integrations that cannot enforce it report that limitation.

## Test provider

The native mock implements normalized prompt scripts and session-scoped response holds through `ProviderTestControl`. `provider/mock_response.rs` translates the host test-control route into those typed commands. `session.provider.mock_response` works without the broker execution lock, so a test can control a response while its real turn owns that lock. [Editor tests](../editor-tests.md#control-provider-timing) shows how to use it.

## Catalog and accounting normalization

Adapters publish reasoning choices in ascending effort order and supply model catalog order. The picker only applies user pins; shared text generation selects the first advertised effort without recognizing native names. Isolated generation resolves model and reasoning from the normalized catalog before opening temporary history. The shared Harness helper applies `SessionHistory::IsolatedText`, which requires supplied instructions only and excludes inherited instructions, hooks, skills, MCP, tools, permission prompts, and autonomous goals. Unsupported isolation rejects before dispatch.

The helper creates independent input and attempt identities with temporary receipt storage, submits ordinary input, and collects final response messages through invocation settlement. It excludes reasoning, commentary, and tool output, limits collected message content to 1 MiB, and applies a 120-second generation deadline. Host validation runs before close. A deadline explicitly cancels the current provider operation; utility shutdown also explicitly cancels active utility work. A retained worker owns close even if the result waiter disappears. Close receives a fresh cleanup context and drains the sole reader while collection runs; its deadline is independent of generation cancellation. Provider shutdown joins utility work before collecting the native runtime.

Usage reports explicitly mark context-only accounting adjustments. Exchange metrics deduplicate those reports and update their cursor without counting them as model calls. Codex selects its context baseline in its decoder. Compaction decoding converts native fields and billing categories to normalized fields and labeled measurements with explicit units; presentation does not assume a billing currency.

Native attachment encoders and resource-profile recipes live in `forge-providers`. Harness resolves retained objects into normalized file content and selects portable resources. `session_configure` applies complete model, access, approval, and resource settings only to a settled binding. Private adapter replacement preserves the logical binding and event stream; results include effective capabilities and an event watermark the owner consumes before dependent work. Fork requests carry the target owner, workspace, configuration, resources, and system instructions; `session_fork` returns persisted history without opening the target, and the host opens that history separately. Discovery uses explicit workspace or exact-binding contexts. `mcp_list(Configured)` supplies prompt definitions without starting servers; `mcp_list(Observed)` reads readiness and tools without changing live configuration. A retained picker observation keeps the fast initial rows usable while readiness resolves.
