# Providers

Each provider reports identities, phases, tools, and failures differently, and a consumer that decoded those differences itself would show one provider's quirks as another's behavior. Provider adapters therefore normalize every transport into one typed Harness contract, and shared settings record real capability differences instead of simulating unsupported operations.

[Harness](harness.md) covers the session lifecycle, [Permissions](permissions.md) covers approvals, and [Provider configuration](provider-configuration.md) covers settings, resources, and models. Input delivery is in [Provider submissions](../provider-submissions.md).

## Backend contract

The crate lives at `nvim/rust/forge/crates/forge-harness`, organized by capability: `broker`, `session`, `plan`, `goal`, `exchange`, `turn`, `timeline`, `checkpoint`, `backend`, `storage`, `workspace`, `protocol`, and `control_tools`. `Backend` defines the complete provider contract once. `CodexBackend`, `CopilotBackend`, and `AcpBackend` own private transports, and `MockBackend` implements the same boundary deterministically.

`Backend::prompt_stream` writes to an ordered event sink and returns `BackendCompletion`, with provider metadata and control results but no event history. The broker drains accepted updates before settling the exchange. `BackendEvent` is an exhaustive enum with typed payloads, and every update carries the invocation scope of its owner, so no shared consumer decodes provider JSON. Compaction uses a separate session-operation scope instead of inventing an exchange.

`CapabilityManifest` records each operation's support, ownership, and runtime availability, and `/capabilities` displays it. Unsupported optional operations return typed errors, and cancellation and identity queries carry the Harness session ID, so they cannot affect another conversation.

## Sessions

Interactive Codex and Copilot conversations initialize their provider session asynchronously on open, creation, and resume, without creating an exchange or sending model input. The controller releases its broker lock during network work and rejects stale results after configuration changes. The broker persists the root conversation ID when it accepts a provider turn-start boundary, so interrupting the first turn still keeps the conversation.

Harness defaults to the Codex app-server backend, and `harness.backend = "copilot"` selects the Copilot SDK. Broker initialization resumes the most recently updated session for the repository and backend. Switching providers requires choosing New chat or Resume session, is unavailable during active work, and never transfers context across providers. A failed switch restores the previous session.

## Messages

Copilot reads `assistant.message_start` metadata before deltas and reconciles the matching `assistant.message` snapshot by message identity. Recognized phases are `commentary` and `final_answer`, and missing or unknown phases stay unclassified. If a successful invocation ends with commentary and no explicit final answer, the adapter emits that text again as a response with a new identity. An explicit empty final answer suppresses this, and failure or cancellation never produces a response. Root `session.idle` ends processing, while foreground or child idle does not.

Codex keeps the phase from `item/started` for matching deltas and snapshots. ACP assigns message identities within its execution. Adapters never assign tools or checks to commentary, because `Exchange` captures each activity's owner at admission.

## Tools

Tool requests, input fragments, executions, output, reported changes, and outcomes are separate typed updates correlated by one scoped tool identity, so late output cannot move a tool or restart its execution.

Codex classifies activity by top-level item type. Before titles enter durable state, the transport replaces values under credential-like keys, such as `token`, `api_key`, `password`, and `authorization`, with `[REDACTED]`. Shell headings summarize simple reads and searches after decoding the launcher, and the durable command never changes. ACP tool updates merge by tool-call identity, and structured diff content becomes a `ProviderChangeSet` without reading the workspace. Antigravity edits can complete without diff content, so `/capabilities` marks its file diffs as limited. Reported edits never replace checkpoint-based **Workspace changes**.

`ControlToolRegistry` names tools by domain and action, without a `harness_` prefix or aliases. `ControlToolExecutor` owns the shared lifecycle for every adapter's control callbacks. It serializes runtime mutations, and cancellation interrupts tool waits and lock waiters. A new turn gets a fresh executor, so late results from an earlier generation are rejected.

## Attachments

Attachments resolve against the Harness data root at admission and stay separate from prompt text. Images get each provider's native form: `localImage` items for Codex, the file attachment list for Copilot, and base64 content for ACP when the agent advertises `promptCapabilities.image`. Unsupported image input fails before the prompt activates. PDFs and other files are passed as retained-file context, never as silently extracted text.

## Execution and cancellation

Native adapters signal stop before collecting local callbacks. Codex sends `turn/interrupt`, Copilot sends abort and repeats it to cover a pending send, and ACP revokes callback authority and stops its process tree.

Codex callbacks run outside the socket reader, so an LSP lookup or approval cannot block steering or cancellation. `CodexTurnCoordinator` treats one user request as an exchange that may outlive its first parent turn: steering becomes another parent turn on the same thread, and a bounded synthesis turn runs after the final child completes.

Normalized events flow through queues of at most 96 records, each at most 512 KiB encoded, within an 8 MiB budget. Sends never wait for capacity. Exhaustion poisons the queue, the reducer stops the transport, and the interaction records a failure. After a Copilot SDK lag notice, intake replays missing durable history from the last durable parent.

[Provider retries](../provider-retries.md) defines when a failed request is retried.

## ACP

`AcpBackend` owns one ACP v1 connection and its process tree per Harness root. Claude Code uses the Claude ACP adapter, and Antigravity uses its standalone ACP server. Harness control tools reach ACP agents through a private loopback MCP endpoint of at most 16 connections.

Provider restart reloads the same native conversation, and failing to load it fails configuration instead of creating replacement history. Initialization rejects agents that cannot load conversations. Advertised child sessions enter the shared agent registry, at most 256 per prompt, and become interrupted when the root prompt returns. `AcpRuntime` resolves configured adapters and never installs provider runtimes.

## Approvals and sandboxing

All three approval modes keep identical native callback setup, and Harness applies Ask, Deny, or Allow after saved rules and reported-file checks. Native operations without callbacks stay outside that boundary. Antigravity enterprise terminal policy can run commands without a callback, so its approval capability is limited. After a Copilot denial, completion requires a later final answer or Harness handoff, and provider idle without either fails the exchange.

`BackendKind::supports_sandbox` allows the OS sandbox only for Codex, and child providers that cannot enforce the root's sandbox are rejected. Each provider's native delegation is disabled, so delegation goes through managed Harness agents.

## Test provider

`backend/mock_response.rs` stores session-scoped, one-response holds. The `session.provider.mock_response` route works without the broker execution lock, so a test can control a response while its real turn owns that lock. [Editor tests](../editor-tests.md#control-provider-timing) shows how to use it.
