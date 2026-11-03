# Providers

Each provider reports identities, phases, tools, and failures differently, and a consumer that decoded those differences itself would show one provider's quirks as another's behavior. Provider adapters therefore normalize every transport into one typed Harness contract, and shared settings record real capability differences instead of simulating unsupported operations.

[Harness](harness.md) covers the session lifecycle, [Permissions](permissions.md) covers approvals, and [Provider configuration](provider-configuration.md) covers settings, resources, and models. Input delivery is in [Provider submissions](../provider-submissions.md).

## Provider contract and ownership

The provider boundary spans three crates. `forge-provider-api` defines normalized events, errors, models, capabilities, and the `Provider` interface. Its `ProviderHost` associated types keep Harness request and workflow results out of the native implementations. `forge-providers` owns native protocol decoding, process runtimes, SDK handles, and session state. `forge-harness` supplies host adapters that bind those implementations to tools, permissions, resources, task authority, and durable submissions. Neither native crate depends on Harness.

Only `forge-providers` declares the Copilot SDK dependency; Harness has no direct SDK dependency, including in tests. SDK model conversion, idle classification, and permission-response mapping tests live beside their native implementations. Harness integration tests supply serialized native event fixtures to the decoder and exercise host authorization through `ProviderPermissions`, asserting normalized events and host state.

`Provider::session_events(owner)` supplies the required ordered session stream. The shared session execution engine installs its reader before initialization or resume and retains it across result waits. `session_prompt` and `session_steer` report input acceptance or delivery failure. Normalized `ExecutionUpdate` events carry execution start, terminal coverage, or lost observation; acceptance alone does not settle an exchange. Root, child, and Background callers use the same engine, while retaining their own presentation and result waits.

Native completion remains separate from Harness plan and task effects; the persisted completion keeps its flat representation. The engine accounts for pending controls and the terminal event's covered submission IDs before releasing a result. Copilot intake, engine delivery, and the session inbox retain observations without an event-count limit. Root and Background brokers drain an engine-owned inbox; child runners attach their scoped presentation stream. Observation failure fences further admission and initiates native cancellation and collection. Accepted events drain before the first observation failure, including when a transport failure races with a waiting reader.

`ProviderEvent` is an exhaustive enum carrying invocation ownership. Compaction uses a separate session-operation scope instead of inventing an exchange. Production providers expose neither `prompt` nor `prompt_stream`; output-oriented fixtures use a test-only collector above submission and events.

`ProviderTools`, `ProviderPermissions`, `InputReceipt`, `ProviderTrace`, and `TurnControl` are callback boundaries into the host. Native code asks for a tool result, permission decision, or committed receipt without acquiring Harness storage or task state. `forge-shell` separately owns shell parsing and process primitives used by the policy layer and native runtimes.

`ProviderCapability.integration` declares optional operations, resource targets, isolation choices, and implementation limitations in each adapter. The compiled-in registry resolves those declarations for consumers that have not started a session. `CapabilityManifest` records each operation's support, ownership, and runtime availability, and `/capabilities` displays it. Unsupported optional operations return typed errors, and cancellation and identity queries carry the Harness session ID, so they cannot affect another conversation.

## Sessions

Native initialization validates the selected runtime before opening or resuming its conversation. The current minimums are Codex 0.164.0, Copilot CLI 1.0.103, Claude ACP 0.99.1, and Antigravity ACP 1.4.5. The Copilot SDK dependency is pinned separately to 1.0.26; its version does not prove that a configured CLI override is supported. Codex reads its initialization user agent, Copilot queries the connected runtime status, and ACP reads the adapter version from the initialization handshake. Missing, malformed, and below-minimum versions produce typed initialization errors. A replacement runtime must validate again.

Interactive Codex and Copilot conversations initialize their provider session asynchronously on open, creation, and resume, without creating an exchange or sending model input. The controller releases its broker lock during network work and rejects stale results after configuration changes. The broker persists the root conversation ID when it accepts a provider turn-start boundary, so interrupting the first turn still keeps the conversation.

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

Codex callbacks run outside the socket reader, so an LSP lookup or approval cannot block steering or cancellation. `CodexTurnCoordinator` treats one user request as an exchange that may outlive its first parent turn: steering becomes another parent turn on the same thread, and a bounded synthesis turn runs after the final child completes.

Copilot intake and shared Harness observation queues grow in memory while their consumers are delayed; they neither truncate valid output nor cancel a conversation because a fixed event count was reached. The encoded transport uses bounded frames and waits for byte capacity; larger events cross it in ordered chunks. Copilot's SDK retains its own finite broadcast buffer, so Forge drains that subscription independently of presentation. SDK lag remains observation loss rather than proof of native completion. Sustained backlog can increase memory use.

Copilot intake closes its foreground sender before acquiring the decoder context to report failure, allowing a prompt that holds that context to finish. The session task drops its adapter future before terminal publication, releasing execution guards during cancellation. A force-stopped client marks its shared runtime generation unavailable; recovery collects that generation before starting or attaching a replacement. Recovery resumes the saved native conversation and never replays an uncertain prompt automatically. Failed execution cleanup can be retried, but the owner stays fenced until fencing, cancellation, and collection all succeed.

Provider shutdown collects bound and provisional session intake before stopping the SDK client. Otherwise SDK detach closes a still-observed subscription, and intake mistakes orderly shutdown for observation loss and force-stops the transport while shutdown RPCs are pending. Successful SDK collection marks every shared session handle stopped, preventing later cleanup from issuing detach or abort RPCs against the closed transport. SDK stop errors remain explicit and retryable.

[Provider retries](../provider-retries.md) defines when a failed request is retried.

## ACP

`AcpConversation` in `forge-providers` owns each ACP v1 connection, handshake, model configuration, and native event state. Its Harness adapter owns the control endpoint, permission context, and resource lease for that conversation. Claude Code uses the Claude ACP adapter, and Antigravity uses its standalone ACP server. Harness control tools reach ACP agents through a private loopback MCP endpoint of at most 16 connections.

Provider restart reloads the same native conversation, and failing to load it fails configuration instead of creating replacement history. Initialization rejects agents that cannot load conversations. Advertised child sessions enter the shared agent registry, at most 256 per prompt, and become interrupted when the root prompt returns. `AcpRuntime` resolves configured adapters and never installs provider runtimes.

## Approvals and sandboxing

All three approval modes keep identical native callback setup, and Harness applies Ask, Deny, or Allow after saved rules and reported-file checks. Native operations without callbacks stay outside that boundary. Antigravity enterprise terminal policy can run commands without a callback, so its approval capability is limited. After a Copilot denial, completion requires a later final answer or Harness handoff, and provider idle without either fails the exchange.

OS isolation is enabled only by an adapter's explicit `os_sandbox` declaration. Codex currently declares support and supplies its Windows isolation modes; the shared access policy retains only an optional opaque `isolation_mode`. Unsupported providers resolve isolation to disabled, and declared mode selections are validated before native execution. Native-agent suppression is a separate capability: integrations that cannot enforce it report that limitation.

## Test provider

`provider/mock_response.rs` stores session-scoped, one-response holds. The `session.provider.mock_response` route works without the broker execution lock, so a test can control a response while its real turn owns that lock. [Editor tests](../editor-tests.md#control-provider-timing) shows how to use it.

## Catalog and accounting normalization

Adapters publish reasoning choices in ascending effort order and supply model catalog order. The picker only applies user pins; shared text generation selects the first advertised effort without recognizing native names. Background generation requires a declared capability and validates model choices at the provider boundary.

Usage reports explicitly mark context-only accounting adjustments. Exchange metrics deduplicate those reports and update their cursor without counting them as model calls. Codex selects its context baseline in its decoder. Compaction decoding converts native fields and billing categories to normalized fields and labeled measurements with explicit units; presentation does not assume a billing currency.

Native attachment encoders and resource-profile recipes live under the corresponding Harness adapter. The shared attachment and profile owners retain object access, containment checks, link cleanup, and lease lifetimes.
