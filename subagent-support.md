# Subagent Support Research

## Scope

This document records the current subagent capabilities relevant to the DiffReview Harness. It separates the portable ACP baseline from provider-native orchestration and focuses on the controls required for a reliable Neovim experience: explicit agent creation, automatic delegation, streaming lifecycle state, cancellation, timeline selection, and attributable workspace changes.

The central finding is that subagent support cannot live behind one lowest-common-denominator protocol. The Harness should expose one provider-neutral agent model while allowing native backends to advertise richer capabilities.

## Implemented Harness behavior

The Harness needs to represent every parent and child execution as an identifiable agent run rather than flattening all activity into one transcript. That identity drives timeline selection, tool attribution, provider-reported file attribution, and cancellation.

The Codex backend now provides:

- `/agent` for a centered timeline selector and `/agent <definition> <task>` for explicit creation.
- Backend-initiated subagents when a provider delegates automatically.
- A centered agent selector that switches the visible `:Harness` timeline between the parent and a child.
- Parent and child status including queued, running, waiting, completed, failed, and cancelled.
- Stable attribution for thoughts, tools, tasks, responses, and changed files.
- Capability-gated controls. Unsupported actions should not appear in the UI.
- Multiple simultaneous instances of the same definition.
- Direct child prompts through the selected child thread.
- Direct steering and interruption when Codex reports the child thread and active turn identifiers.
- Project, personal, and built-in Codex definition discovery with project-first precedence. Personal definitions resolve from `CODEX_HOME/agents`, then fall back to the conventional home-directory `.codex/agents` path.

## Portable ACP baseline

ACP provides a useful transport for sessions, prompts, streaming updates, tool activity, permissions, and filesystem or terminal requests. It does not define a portable subagent orchestration contract that gives the Harness stable child identities, direct spawn control, child tool policies, or a cross-provider write lease.

An ACP provider may internally create subagents, but the Harness can only render them as first-class children when that provider emits distinguishable lifecycle events. Therefore ACP should advertise subagent support only after runtime capability discovery. The Harness must not infer children from assistant prose.

The ACP backend should initially expose:

- `subagents = false` unless the connected provider advertises a concrete extension.
- Parent-session streaming and cancellation through the existing ACP path.
- Provider-specific extension adapters only when their event and lifecycle contracts remain stable enough to test.

This preserves ACP portability without pretending that every ACP implementation provides the same orchestration semantics.

## Codex backend

Codex supports native child agents and preserves parent workspace and permission context when creating them. That makes automatic delegation and child lifecycle tracking feasible through the native backend, although ACP does not provide the same guarantee.

The Codex backend translates native child events into generic Harness records:

```text
AgentRun
├─ id
├─ parent_thread_id
├─ provider_thread_id
├─ active_turn_id
├─ definition and nickname
├─ task and status
└─ AgentTurnRecord 0..*
```

Codex children sharing the parent working directory also share its mutable filesystem. Permission inheritance prevents a child from gaining more authority than its parent, while Codex owns scheduling and patch-conflict behavior among those children. Harness attribution should consume structured child lifecycle and `fileChange` events rather than attempting to infer ownership from filesystem timing.

The adapter consumes `collabAgentToolCall` and `subAgentActivity` app-server events. It never derives child identity from rendered transcript text. Codex exposes no direct client-side spawn RPC, so explicit creation remains parent-mediated. Once the provider reports a child thread, ordinary prompts use `turn/start`, steering uses `turn/steer`, and interruption uses `turn/interrupt` against that child identity.

Harness catalog names remain user-facing and durable. The Codex strategy translates hyphenated names such as `local-code-explorer` into the native `local_code_explorer` identifier required by `spawn_agent`, preventing display conventions from leaking into provider protocol values.

## Antigravity CLI

Antigravity CLI exposes agent-oriented commands and can invoke subagents, but its external orchestration and lifecycle surface needs source-level verification before the Harness can promise first-class child control. Visible CLI commands do not prove that an embedding protocol exposes stable child identifiers, cancellation, or tool-level policy hooks.

Treat Antigravity support as a later native backend investigation. Keep it separate from ACP until its protocol proves the required lifecycle and attribution guarantees.

## Copilot SDK changes the control boundary

The GitHub Copilot SDK provides substantially more control than Copilot CLI through ACP. It offers a first-class Rust SDK that manages the Copilot CLI process and communicates with `copilot --server --stdio` over JSON-RPC. This fits directly into the existing Rust Harness broker without a Node sidecar.

The Rust SDK exposes:

- Session creation, resume, abort, model selection, streaming, and typed RPC access.
- Custom agents with individual prompts, descriptions, tool lists, skills, and MCP servers.
- Automatic runtime delegation to inferred custom agents.
- Session-level agent selection.
- Fleet mode for parallel subagent orchestration.
- Subagent lifecycle events with stable `toolCallId` values.
- Session-wide `availableTools` and `excludedTools` filters.
- Per-agent tool allowlists.
- Synchronous pre-tool hooks that can allow, deny, or rewrite tool calls.
- Post-tool hooks for successful and failed calls.
- Structured elicitation and user-input handlers.
- Plan, task, session-fork, and workspace RPC namespaces.

Primary references:

- [Rust SDK](https://github.com/github/copilot-sdk/blob/main/rust/README.md)
- [Custom agents and subagent orchestration](https://github.com/github/copilot-sdk/blob/main/docs/features/custom-agents.md)
- [Fleet mode](https://github.com/github/copilot-sdk/blob/main/docs/features/fleet-mode.md)
- [Pre-tool use hook](https://github.com/github/copilot-sdk/blob/main/docs/hooks/pre-tool-use.md)
- [Post-tool use hook](https://github.com/github/copilot-sdk/blob/main/docs/hooks/post-tool-use.md)

### Custom agents

Custom agents provide the cleanest mapping to a Harness `/agent` model. Each definition can restrict its tools and disable inference when the user should invoke it explicitly.

Example roles include:

- A `researcher` with `grep`, `glob`, and `view` only.
- An `implementer` with `view`, `edit`, and controlled shell access.
- A `reviewer` with diff and test tools but no direct edit capability.

The Copilot runtime can select inferred agents automatically. Setting `infer = false` reserves an agent for explicit use. The Harness should preserve this distinction in configuration so `/agent` can list explicit agents without allowing the runtime to choose them unexpectedly.

### Fleet mode

Fleet mode dispatches multiple subagents through Copilot's task mechanism and coordinates work through a shared todo model. The SDK exposes `session.fleet.start`, and the event stream reports `subagent.started`, `subagent.completed`, and `subagent.failed` with `toolCallId` identifiers.

These events provide enough information to construct an agent tree and switch the Harness timeline between its nodes. They also avoid transcript parsing because lifecycle state arrives as structured data.

Fleet mode remains experimental in the generated RPC surface. The Harness should pin the Rust SDK and its compatible Copilot CLI runtime, advertise fleet only after capability discovery, and retain tests against the pinned pair.

### Explicit single-agent invocation

The SDK clearly supports selecting an agent for a session and starting a fleet. The reviewed documentation does not establish a stable high-level RPC that directly spawns exactly one named child and returns a child handle.

The Harness has two viable implementations for explicit `/agent`:

1. Create or select a dedicated SDK session configured with the requested custom agent.
2. Ask the parent runtime to dispatch the selected non-inferred custom agent through its task mechanism.

The first option gives the Harness direct ownership, independent cancellation, and a stable timeline root. The second preserves Copilot's parent-child result integration. We should verify the underlying task RPC before choosing between them.

## Change attribution and shared-workspace policy

The Harness needs to explain which edits an agent explicitly performed without pretending that filesystem timing proves authorship. Structured provider `fileChange` events provide that evidence at the thought or child-run boundary.

```text
Provider parent or child
├─ successful fileChange events
│  └─ ordered provider patch set -> attributed thought diff
└─ interaction completion
   └─ Git baseline vs final snapshot -> complete interaction diff
```

The attributed thought diff merges repeated edits to one path in provider event order. Failed or declined file changes contribute no attributed diff. Shell commands, formatters, Git hooks, and detached processes do not gain an agent attribution unless the provider reports them as structured file changes.

The complete interaction diff compares one Git baseline with one terminal snapshot. It includes command and formatter effects that reached tracked or nonignored untracked paths, while `git ls-files --cached --others --exclude-standard` excludes ignored build artifacts. This aggregate remains authoritative for review and rollback even when a provider omits structured file changes.

This design removes the filesystem watcher, per-tool Git scans, repository writer lease, and cross-process lock from the attribution path. Provider runtimes own concurrent scheduling and patch-conflict behavior. The Harness surfaces overlapping-edit failures as tool failures instead of serializing every writer around a speculative mutation classifier.

Exact child attribution still requires provider lifecycle correlation. Codex file-change events identify their thread, turn, and item, but the Harness must verify which native child lifecycle field connects that turn to a stable `agent_id`. Copilot lifecycle and `toolCallId` events provide a similar correlation point. A provider that cannot expose the connection can still show the aggregate interaction diff without claiming child ownership.

The existing Harness **session lease** remains unchanged. It protects SQLite and backend-session ownership between Neovim instances. It does not coordinate workspace writers and should never appear as a file-mutation lock.

Separate worktrees remain an optional future isolation mode for workflows that need deterministic concurrent mutation. They no longer block initial shared-workspace subagent support because attribution and final review do not depend on writer serialization.

## Generic Harness architecture

The generic layer should model capabilities and normalized events. It should not encode Copilot, Codex, or ACP wire types directly.

```text
Provider backend
├─ ACP
├─ Codex native
└─ Copilot SDK native
       │
       ▼
HarnessAgentEvent
├─ AgentDiscovered
├─ AgentStarted
├─ AgentStatusChanged
├─ TimelineEventAttributed
├─ AgentCompleted
├─ AgentFailed
└─ AgentCancelled
       │
       ▼
AgentRegistry
├─ parent-child graph
├─ active timeline selection
├─ provider capability and attribution state
└─ provider capability set
       │
       ▼
:Harness timeline and agent selector
```

Implemented capability fields:

```text
observe
catalog
spawn = unsupported | parent_mediated | direct
input = unsupported | parent_mediated | direct
interrupt = unsupported | parent_mediated | direct
parallel
```

The UI should derive available actions from these capabilities. For example, `/agent` may create a dedicated Copilot session, ask Codex to spawn a native child, or remain hidden for a generic ACP provider.

## Timeline behavior

Each parent interaction persists an ordered `InteractionNode` list. A `MainSegment` freezes one uninterrupted stretch of parent work, an `AgentReference` fixes a child at its spawn position, and a `SteeringPrompt` records acknowledged input where the user redirected the logical interaction. Child status updates replace the referenced `AgentRun` without moving its row. Final parent synthesis appends another `MainSegment`, so response streaming never pushes a completed child to a temporary tail position.

Codex keeps one app-server process alive after an early parent `turn/completed` notification whenever descendant threads remain active. A logical-turn coordinator continues consuming child lifecycle events, accepts Ctrl-q steering as a new parent turn on the same thread, and starts a bounded final synthesis turn after the last descendant completes. `Waiting on N subagents` derives from transient coordinator state. It disappears when parent work resumes or every child finishes and never enters SQLite history.

The Lua projection consumes only the ordered representation. Sessions carry an exact format version, and the store hides every noncurrent session instead of migrating or partially decoding its timeline.

The selector should show:

- Agent display name and provider.
- Parent relationship.
- Current status.
- Read or write policy.
- Tool and failure counts.
- Changed-file count.
- Active task when available.

Switching timelines must not start, stop, or focus the underlying agent. It only changes the projection rendered in `:Harness`. A separate action should cancel an agent or return focus to its input channel.

## Follow-up implementation order

1. Add Copilot SDK as a native Rust backend with custom-agent and lifecycle-event support.
2. Correlate structured provider file changes with child lifecycle identities.
3. Add optional worktree-backed isolation and reconciliation.
4. Investigate Antigravity adapters against pinned protocol versions.

This ordering establishes attribution and lifecycle correctness before presenting concurrent mutation. The UI can then distinguish provider-attributed edits from the complete interaction result without inventing ownership for formatter or shell side effects.

## Verification requirements

Unit tests should cover:

- Agent graph insertion, replacement, completion, and failure.
- Provider event normalization.
- Timeline filtering by current cursor-selected agent.
- Capability-gated command visibility.
- Ordered provider file changes merging into the owning thought or child run.
- Failed and declined file changes producing no attributed diff.
- Shell and formatter effects remaining absent from attributed diffs.
- Git baseline and terminal snapshots including tracked and nonignored untracked effects.
- Gitignored nested artifacts remaining absent from the aggregate interaction diff.
- Tool filtering for Copilot custom agents.
- Stable segment, child, steering, and response order across child completion updates.
- Transient wait start, count replacement, and removal without a durable timeline node.
- Block-level renderer transactions preserving cursor identity and fold state while adjacent nodes stream.

Integration tests should cover:

- A real pinned Copilot SDK and CLI session creating a custom read-only agent.
- Fleet mode producing multiple structured child lifecycle events.
- Child tools appearing only in the selected child's timeline.
- Multiple child edits retaining provider event order and stable child attribution.
- Overlapping provider patches surfacing their native success or failure result without a Harness lock.
- A command or formatter mutation appearing only in the final Git interaction diff.
- Cancellation preserving completed provider attribution and the terminal interaction snapshot.
- A nested ignored build artifact remaining absent from review and rollback state.
- A parent turn ending before its child, followed by child completion and final parent synthesis.
- Steering while the parent waits, with the steering prompt preceding its answer and the child retaining its spawn position.

The terminal verification path should exercise the actual Neovim selector, live streaming state, cancellation, and window switching. Mock snapshots alone cannot detect cursor jumps, stale status, rendering glitches, or timeline attribution errors.

## Current boundary

The Harness now builds Codex support around a provider-neutral `AgentRegistry`. Rust owns definition discovery, run identity, lifecycle, child turns, persistence, and provider control. Lua owns only selected timeline state and the centered selector.

ACP remains the portable single-agent baseline and hides `/agent`. Native providers schedule shared-workspace writers, Git snapshots remain the aggregate interaction review authority, and separate worktrees remain optional isolation rather than a prerequisite.
