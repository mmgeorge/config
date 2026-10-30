# Providers and external resources

Each provider transport reports identities, phases, tools, and failures differently, and a consumer that decoded those differences itself would render one provider's quirks as another's behavior. Provider adapters therefore normalize every transport into one typed Harness contract, and shared settings record real capability differences instead of simulating unsupported operations.

Harness lifecycle and permission ownership are described in [Harness](harness.md). Transport admission and framing are described in [Host and protocol](host-and-protocol.md).

## Backend contract

The feature-first crate lives at `nvim/rust/forge/crates/forge-harness`, with capability directories: `broker`, `session`, `plan`, `goal`, `exchange`, `turn`, `timeline`, `checkpoint`, `backend`, `storage`, `workspace`, `protocol`, and `control_tools`. `Backend` defines the complete provider contract once. `CodexBackend`, `CopilotBackend`, and `AcpBackend` own private transports, such as `CodexJsonRpc` and `CopilotEventDecoder`, and expose capability values that drive broker and editor behavior. `MockBackend` implements the same boundary deterministically.

`Backend::prompt_stream` writes to an ordered event sink and returns `BackendCompletion` with provider metadata and control results but no event history. The broker drains accepted updates before settling the exchange, and provider turn completion never admits a new exchange. `BackendEvent` is an exhaustive enum with typed payloads for messages, tools, turns, usage, runtime, compaction, agents, checklists, approvals, directory access, acknowledged input, retries, diagnostics, and parent boundaries. Every update carries the invocation scope of its admitted owner, so no shared consumer decodes provider JSON. Compaction uses a separate session-operation scope instead of inventing an exchange.

`CapabilityManifest` records each operation's support, ownership, application boundary, and runtime availability, and `/capabilities` displays it. Unsupported optional operations return typed unsupported-capability errors, and cancellation and identity queries carry the Harness session ID so they cannot affect another conversation. The broker persists the root conversation ID when it accepts a provider turn-start boundary, so interrupting the first turn still keeps the conversation.

## Sessions and initialization

Interactive Codex and Copilot conversations initialize their provider session asynchronously on open, creation, and resume, without creating an exchange or sending model input. The controller releases its broker lock during network work and rejects stale results after configuration changes, and shutdown cancels outstanding initialization. Codex registers root tool schemas before the first prompt and resumes the thread with its current model, policy, and instructions. Copilot reuses its initialized SDK session.

Harness defaults to the direct Codex app-server backend, and `harness.backend = "copilot"` selects the native Copilot SDK. An empty `harness.backends.copilot.command` lets the SDK discover the CLI. Broker initialization resumes the most recently updated session for the repository and backend, so `:ForgeHarness` continues work across restarts, while `/clear` starts a new session. Preference records with an incompatible envelope are treated as absent without being rewritten.

The Provider row in `/config` and `/backend` open one picker. Switching requires choosing New chat or Resume session, is unavailable during active turns, queued input, or pending configuration, and never transfers context across providers. Confirmation restarts the host into the selected provider and session in the current tab, reusing an already-open session's buffers and unsent draft. Neovim records the last successful choice in `stdpath("data")/forge/harness/backend.json`, an explicit `setup({ harness = { backend = ... } })` stays authoritative, and a failed switch restores the exact previous session.

## Messages and phases

Copilot reads `assistant.message_start` metadata before deltas and reconciles the matching `assistant.message` snapshot by message identity. Recognized phases are `commentary` and `final_answer`. Missing or unknown phases stay unclassified, and unknown values are reported as diagnostics. If a successful invocation ends with commentary and no explicit final answer, the adapter emits that text again as a response with a new identity, leaving the commentary unchanged. An explicit empty final answer suppresses this, and failure or cancellation never synthesizes success. Copilot reasoning uses `reasoningId` independently of phase. Root `session.idle` ends processing, while foreground or child idle does not.

Codex keeps the phase from `item/started` for matching deltas and snapshots. ACP assigns message identities within its execution and completes accumulated text before terminal events. Adapters never assign tools or checks to commentary: `Exchange` captures each activity's owner at admission.

## Tools and activity

Tool requests, input fragments, executions, output, reported changes, and outcomes are separate typed updates correlated by one scoped tool identity. Later output cannot move a tool or restart its execution. Codex classifies activity by top-level item type. Errors, messages, reasoning, plans, compaction, and child lifecycle stay separate, and every other typed item with an ID becomes a tool. `mcpToolCall` items render as `server.tool(arguments)`. Web searches render as `Web Search`, `Open web`, or `Find in page`, native sleeps as `Wait`, and other types as `type(compact JSON)`. Before titles enter durable state, the transport replaces values under credential-like keys, such as `token`, `api_key`, `secret`, `password`, `authorization`, `cookie`, `session`, and `private_key`, with `[REDACTED]`.

Shell headings summarize simple reads, listings, and searches after decoding PowerShell, Bash, or Nushell launchers. Compound commands and unknown options keep their command text, and the durable command never changes. Copilot reports absent diff sides as `a/dev/null` and `b/dev/null`, which the adapter recognizes before path conversion. It also normalizes `create file mode` to Git's `new file mode`. ACP tool updates merge by tool-call identity, and structured diff content becomes a `ProviderChangeSet` without reading the workspace. Antigravity edits can complete without diff content, so `/capabilities` marks its file diffs as limited. Reported edits never replace the exchange's checkpoint-based **Workspace changes**.

`ControlToolRegistry` names tools by domain and action, such as `agent_spawn`, `agent_send`, `agent_activity`, `design_patch`, `repository_status`, and `lsp`, without a `harness_` prefix or aliases. `ControlToolExecutor` owns the shared lifecycle for every adapter's control callbacks. It serializes runtime mutations, and cancellation interrupts tool waits and lock waiters. A new turn receives a fresh executor, so late results from an earlier generation are rejected.

## Input and attachments

`backend/submission.rs` commits input before host admission, records native correlation before dispatch, and releases the delivery obligation only after provider acceptance. Unknown delivery blocks replay, and the [provider retry and submission contract](../provider-retries.md) defines recovery. `BackendInput` distinguishes text from explicit skill invocation.

Attachments resolve against the Harness data root at admission and stay separate from prompt text. Images receive `<image name=[Image #N] path="...">` framing plus each provider's native form: `localImage` items for Codex, the file attachment list for Copilot, and base64 content for ACP when the agent advertises `promptCapabilities.image`. Unsupported image input fails before the prompt activates. PDFs and other files are passed as retained-file context that directs the agent to inspect them through tools, never as silently extracted text.

## Execution and cancellation

Confirmed terminal network and capacity failures use one shared retry runner for root, child, and Background requests, with waits of 1, 5, 10, 30, and 60 seconds. Adapters opt in only after proving that the attempt produced no replay-sensitive output. Native adapters signal stop before collecting local callbacks: Codex sends `turn/interrupt`, Copilot sends abort and repeats it to cover a pending send, and ACP revokes callback authority and stops its process tree.

Codex callbacks run outside the socket reader, and control tools share an ordered executor, so an LSP lookup or approval cannot block steering or cancellation. `CodexTurnCoordinator` treats one user request as an exchange that may outlive its first parent turn. A `turn/start` acknowledgement admits its turn before content arrives, steering becomes another parent turn on the same thread, and a bounded synthesis turn runs after the final child completes. `ActiveWait` drives the Timeline Status, showing `Waiting for N subagents` or `Waiting for plan review` without creating history. Background terminal inventory uses a separate passive connection that reads a loaded thread without resuming it. For the specific `-32603` empty-rollout response, it retries resume every 500 ms at most four times.

Normalized provider events flow through `forge_harness::backend::events` queues of at most 96 records, each record at most 512 KiB encoded, within an 8 MiB budget. Sends never wait for capacity, and exhaustion poisons the queue, after which the reducer stops the transport and records a failed interaction. Copilot SDK subscriptions drain through independent `SessionEventStream` tasks into ordered typed queues, excluding raw streaming telemetry. After an SDK lag notice, intake replays missing durable history from the last durable parent.

## ACP

`AcpBackend` owns one ACP v1 connection and its collected process tree per Harness root. Claude Code uses the Claude ACP adapter, and Antigravity uses its standalone ACP server. Harness control tools reach ACP agents through a private loopback MCP endpoint admitting at most 16 connections. Provider restart reloads the same native conversation, and failing to load it fails configuration instead of creating replacement history. ACP initialization rejects agents that cannot load conversations, and planning selects an advertised plan or architect mode. Advertised child sessions enter the shared agent registry, at most 256 per prompt, and become interrupted when the root prompt returns.

`AcpRuntime` resolves configured adapters and never installs or downloads provider runtimes. Claude uses the installed `claude` executable, preserving `CLAUDE_CODE_EXECUTABLE`. Setup and authentication reject active work. Claude OAuth runs in a Neovim terminal, Antigravity OAuth uses its ACP request, and sign-in has a 120-second timeout.

## Permissions and sandboxing

All three approval modes keep identical native callback setup: Codex uses `untrusted`, Claude keeps native ask rules and its default permission mode, and Copilot keeps its SDK callback. Harness applies Ask, Deny, or Allow after saved rules and reported-file checks, and native operations without callbacks stay outside that boundary. Antigravity enterprise terminal policy can execute commands without a callback, so its approval capability is limited. Copilot permission responses include denial feedback that distinguishes automatic denials from user choices. After a denial, completion requires a later final answer or Harness handoff, and provider idle without either fails the exchange instead of retrying.

`BackendKind::supports_sandbox` allows the OS sandbox only for Codex, and other providers resolve sandbox to off while the shared preference stays independent. Child providers that cannot enforce the root's sandbox are rejected. Copilot's session-state directory is automatically readable and writable for its own session when writes are enabled, after path and symlink resolution. Codex disables `agents.enabled`, Copilot excludes its native delegation tools, and Claude ACP disallows `Agent` and `Task`, so delegation goes through managed Harness agents. Copilot sessions disable tool search so Harness controls and MCP tools are always directly available.

## Settings

`SettingsStore` owns revisioned global, workspace, and session layers in `settings.db`, read from one committed snapshot. Unspecified fields inherit, and provider tuning stays scoped to its backend. `settings.update` and `settings.reset` require the expected revision and apply through task transition admission. A mutation commits its revision and operation receipt together, so a retry with the same operation ID recovers instead of applying twice, and a reused ID with different content is rejected. Configuration timeline records name changed fields, never values, and only failures appear in the timeline. New sessions resolve saved defaults, and resumed sessions keep their own model and tuning. Native forks copy explicit session overrides into an independent layer.

## Resources

`ResourceCatalog` reads MCP definitions from `config_dir/rulesync-global/.rulesync/mcp.json` and the workspace `.rulesync/mcp.json`, and skills from the matching Rulesync skill directories, keeping identities and metadata without prompt bodies or credentials. Each entry carries `Global`, `Workspace`, or `Provider` scope into `/skills` and `/mcp`, where Tab expands the source path, tools, and complete failure reason. `/config` also offers an explicit native import. Its preview omits launch values and bodies and blocks nonportable options, and applying it requires the reviewed fingerprint and preference revision. Imported MCP values keep references into the original native file, so Forge never copies literal credentials.

Environment references resolve only into ephemeral launch payloads. A missing credential or missing stdio command marks only that server unavailable and omits it from launch, logging `mcp.start_skipped` with one warning per change. Each provider receives selected resources through an isolated profile: Claude uses strict MCP configuration and an ephemeral `forge-managed` plugin, Antigravity uses a private `GEMINI_HOME`, Copilot uses an isolated skill directory and `session.mcp.startServer` with at most four concurrent starts and a 30-second deadline each, and Codex uses a private app-server runtime with inherited MCP and plugin resources disabled. `ResourceProfile` links directories with NTFS junctions or symlinks and rejects traversing names. No native configuration file is written, and MCP launch values are redacted from traces.

Selected skills receive execution-scoped read and script grants covering one canonical bundle, which are revoked when the execution ends. A recognized read of a selected `SKILL.md` produces one `◇ Read skill · <name>` node per skill per agent turn.

## Models

`BackendModel` is the provider-neutral picker contract: model identifier, ordered reasoning choices, context-window tiers, vision capability, and optional description. Opening the picker reloads the catalog with saved per-model selections. Codex resolves `default` through its catalog and sends the resolved model explicitly on thread creation, resume, and turn start, so the displayed and running models match. Copilot takes context capacity from `capabilities.limits.max_context_window_tokens`, never from billing thresholds. Codex and Copilot request concise reasoning summaries.

Background speed travels through `TextGenerationConfig`. Codex maps each model's `serviceTiers` `priority` ID to Forge's `fast` choice, and selectors offer only tiers the model advertises. Missing metadata permits only `default`. `views/harness/model_order.lua` orders Copilot models Auto, Claude, GPT, then Gemini by descending version, keeps provider order elsewhere, and moves pinned models first. `ModelPinStore` keeps pins per backend in the `model_pin` table, changed through `provider.model.pin.set` outside the broker lock, and `p` toggles a pin in the picker.

## Test provider

`MockBackend`'s `backend/mock_response.rs` stores session-scoped, one-response holds. The `session.provider.mock_response` route handles hold, wait, release, and cancel without taking the broker execution lock, so a test can control a response while its real turn owns that lock. A hold matches the next prompt in the session before any output begins. Prompt drop, cleanup, and shutdown invalidate it. Editor tests use it through `nvim/tests/forge/support/mock_provider.lua`.
