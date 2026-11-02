# Providers

Each provider reports identities, phases, tools, and failures differently, and a consumer that decoded those differences itself would show one provider's quirks as another's behavior. Provider adapters therefore normalize every transport into one typed Harness contract, and shared settings record real capability differences instead of simulating unsupported operations.

[Harness](harness.md) covers the session lifecycle, [Permissions](permissions.md) covers approvals, and [Provider configuration](provider-configuration.md) covers settings, resources, and models. Input delivery is in [Provider submissions](../provider-submissions.md).

## Provider contract and ownership

The provider boundary spans three crates. `forge-provider-api` defines normalized events, errors, models, capabilities, and the `Provider` interface. Its `ProviderHost` associated types keep Harness request and workflow results out of the native implementations. `forge-providers` owns native protocol decoding, process runtimes, SDK handles, and session state. `forge-harness` supplies host adapters that bind those implementations to tools, permissions, resources, task authority, and durable submissions. Neither native crate depends on Harness.

`Provider::prompt_stream` writes to an ordered event sink and returns completion metadata. Native completion is separate from Harness plan and task effects; the persisted completion keeps its flat representation. The broker drains accepted updates before settling the exchange. `ProviderEvent` is an exhaustive enum carrying invocation ownership. Compaction uses a separate session-operation scope instead of inventing an exchange.

`ProviderTools`, `ProviderPermissions`, `InputReceipt`, `ProviderTrace`, and `TurnControl` are callback boundaries into the host. Native code asks for a tool result, permission decision, or committed receipt without acquiring Harness storage or task state. `forge-shell` separately owns shell parsing and process primitives used by the policy layer and native runtimes.

`ProviderCapability.integration` declares optional operations, resource targets, isolation choices, and implementation limitations in each adapter. The compiled-in registry resolves those declarations for consumers that have not started a session. `CapabilityManifest` records each operation's support, ownership, and runtime availability, and `/capabilities` displays it. Unsupported optional operations return typed errors, and cancellation and identity queries carry the Harness session ID, so they cannot affect another conversation.

## Sessions

Interactive Codex and Copilot conversations initialize their provider session asynchronously on open, creation, and resume, without creating an exchange or sending model input. The controller releases its broker lock during network work and rejects stale results after configuration changes. The broker persists the root conversation ID when it accepts a provider turn-start boundary, so interrupting the first turn still keeps the conversation.

Harness defaults to the Codex app-server provider, and `harness.provider = "copilot"` selects the Copilot SDK. Broker initialization resumes the most recently updated session for the repository and provider. Switching providers requires choosing New chat or Resume session, is unavailable during active work, and never transfers context across providers. A failed switch restores the previous session.

Copilot's native session registry serializes preparation per execution owner. It retains repair notices, resource leases, terminal readers, and compaction subscriptions with the matching binding. Replacing submitted history disconnects its old SDK router before resuming the same ID; replacing a provisional session keeps the old binding until preparation succeeds. The host marks a specific binding as submitted before input can reach the provider, so an output-free cancellation cannot accidentally publish a new history boundary.

## Messages

Copilot reads `assistant.message_start` metadata before deltas and reconciles the matching `assistant.message` snapshot by message identity. Recognized phases are `commentary` and `final_answer`, and missing or unknown phases stay unclassified. If a successful invocation ends with commentary and no explicit final answer, the adapter emits that text again as a response with a new identity. An explicit empty final answer suppresses this, and failure or cancellation never produces a response. Root `session.idle` ends processing, while foreground or child idle does not.

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

Normalized events flow through queues of at most 96 records, each at most 512 KiB encoded, within an 8 MiB budget. Sends never wait for capacity. Exhaustion poisons the queue, the reducer stops the transport, and the interaction records a failure. After a Copilot SDK lag notice, intake replays missing durable history from the last durable parent.

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
