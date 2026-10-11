# Replace native-agent orchestration with Harness-owned subagents

## 1. Outcome and agreed behavior

Harness will create and manage independent provider conversations for subagents. Providers will execute individual agent turns, while Harness owns delegation, identity, scheduling, communication, cancellation, persistence, and presentation.

This replaces the current parent-mediated implementation. Today, `/spawn` instructs the parent model to invoke its native spawning tool and waits for provider events to identify the resulting child. The existing agent records, timeline rendering, and picker provide reusable foundations.

The initial implementation has these fixed decisions:

| Area | Decision |
| --- | --- |
| Providers | Support Codex, Copilot, Claude Code, and Antigravity, including children using a different provider from their parent. |
| Context | Every newly spawned child starts a fresh provider conversation. No parent-history copying or forking. |
| Roles | Generic agents only. Do not load native or Rulesync subagent definitions in this version. |
| Workspace | Children share the root session's workspace. No worktree creation or automatic change merging. |
| Concurrency | Permit eight simultaneously active children per root session, excluding the root. |
| Delegation authority | Only the root can spawn and manage children. Children receive none of the six agent tools. |
| Tool setting | Model-facing subagent tools default to off. Manual `/spawn` remains available. |
| Native spawning | Disable it for every root and child wherever the provider supports suppression. |
| Model-delegated completion | Continue the parent's unfinished request automatically to process child results. |
| Manual completion | Display and save the result without automatically running the main agent. |

Follow-up tasks reuse the child's own conversation. "Fresh context" applies when creating a new agent, not when continuing an existing one.

Automatic `explore` orchestration, plan decomposition, verification-agent selection, custom roles, and nested spawning remain outside this implementation.

## 2. Runtime ownership and durable state

### Coordinator and execution boundaries

Introduce a root-session `AgentCoordinator` in the existing agent subsystem. Both model tools and user commands call this coordinator.

The coordinator owns:

- Agent identities, canonical task names, and their relationship to the root.
- Admission of child tasks and the eight-child concurrency limit.
- Per-child task queues, inbound messages, completion notifications, and cancellation.
- Child runtime handles and delivery of lifecycle, thought, tool, usage, and result events into the existing timeline pipeline.
- Scheduling of parent continuation after model-delegated work finishes.

Keep the coordinator callable while the root provider is running. A tool callback must not acquire the same broker lock held by the root's in-flight prompt. Use typed asynchronous requests and responses, following the existing execution-control approach.

Separate short state mutations from long provider operations. Do not hold coordinator, database, presentation, or registry locks while waiting for model output, child completion, or provider cleanup.

Give each child an independent runtime owner with at most one active provider turn. Reuse provider transports and session creation facilities instead of constructing another copy of the full user-facing Harness workflow.

### Identity and provider routing

Preserve one visible Harness session containing its root and children. Children must not appear as unrelated sessions in `/sessions`.

Extend execution ownership to include both the Harness session ID and agent ID. Update provider connection maps, control-tool contexts, permission callbacks, compaction subscriptions, cancellation, and event routing to use that ownership.

The current adapters frequently index runtime state by Harness session ID alone. Adding child execution without changing those keys risks sharing a provider connection or cancellation target between siblings.

Persist, for each child:

- Its stable Harness ID, task name, root ownership, and creation source: model or user.
- Its selected provider, resolved model, reasoning settings, and provider conversation ID.
- Its effective execution configuration.
- Its lifecycle and latest task outcome.

Continue using `Delegation`, `Exchange`, and `Turn` for causal relationships and execution history. A reusable child identity must remain distinct from each task performed through it.

Use `/root/<task_name>` as the canonical task path. Require unique task names within a root session. Resolve tool targets by stable ID, canonical path, or an unambiguous task name within that session. Reject cross-session targets.

### Admission and lifecycle

Implement this creation sequence:

1. Validate the request, provider configuration, task name, policy compatibility, and available capacity.
2. Atomically persist the child identity, delegation, and initial queued exchange.
3. Return the stable child identity and launch its fresh provider conversation asynchronously.
4. Publish starting, running, waiting, and terminal state through the existing event and timeline mechanisms.

Reserve capacity before launching. Starting children and children waiting for approval count toward the limit. Release capacity only after execution and required cleanup settle. A ninth concurrent spawn returns a capacity error without creating an orphaned child.

A failed launch becomes a visible failed child task. Do not automatically retry operations that may already have reached a provider.

Completed and interrupted agents remain inspectable and reusable through follow-up tasks. Their availability must not depend on retaining a running CLI process indefinitely.

### Timeline event contract

Subagents must produce live timeline events through the same Harness event, storage, and rendering pipeline used for root execution. A final tool response or an agent-status notification alone does not satisfy this requirement.

Route child provider events with their Harness session, agent, exchange, and turn ownership. Preserve stable message and tool-call identities across streaming updates. The event flow must cover:

- Agent admission, startup, execution, waiting, interruption, failure, and completion.
- Thought and message creation, streaming updates, and completion, using the existing provider-supported content semantics.
- Tool-call creation, input updates, execution state, output updates, and completion.
- Usage and timing updates needed for the existing exchange heading metrics.
- Final child responses and subsequent follow-up exchanges.

Normalize each backend's available events into the existing Harness contracts. Do not fabricate thought content or usage values that a provider does not expose. Any protocol additions must extend this shared pipeline rather than create an independent agent-only UI event system.

Apply events to the correct child state before publishing the corresponding UI update. Serialize mutations per child and preserve causal order. Events from siblings may interleave without overwriting each other's timeline state. Repeated delivery must update the existing message or tool call rather than append duplicates, and late events from an ended turn must not reopen that turn or mutate a newer follow-up.

The main timeline derives each child's heading and latest-thought preview from this event-fed state. Tool events update child history and heading metrics without appearing in the main timeline preview. The selected child's full timeline receives its normal live thought, message, and tool updates. Both views must remain current while the root is running, waiting, or idle, without requiring another root model turn or manual refresh.

Persist child history and terminal outcomes through the existing storage lifecycle so reopening the session reconstructs the same timelines. Preserve the existing handling of transient streaming updates. Elapsed time may use the existing display clock, but child activity must come from execution events rather than polling provider transcripts.

### Parent continuation

A model-delegated child belongs to the parent exchange that created it. Ending one parent model turn does not complete that exchange while delegated work remains unresolved.

When results arrive:

- Persist the result before publishing its notification.
- Deliver it to an active wait or queue it for the parent's next safe input boundary.
- If the parent has stopped generating while delegated work remains, schedule a continuation within the same exchange.
- Serialize parent turns and coalesce already-available results. Concurrent child completions must not start concurrent parent turns.
- Include child identity, task, terminal outcome, and final response. Preserve the full child transcript separately.

Use the existing continuation limits and error reporting. Never continue after explicit user cancellation, pause, or an unresolved user decision.

Manual children publish the same lifecycle and result data, but completion alone never schedules a root model turn.

## 3. Tools, backend integration, and policy

### Model-facing contract

Register these through the existing Harness control-tool infrastructure, using the established `harness_` naming convention. One schema and dispatcher must serve Codex dynamic tools, Copilot custom tools, and ACP's private MCP endpoint.

| Tool | Contract |
| --- | --- |
| `harness_spawn_agent` | Requires `task_name` and `message`. Accepts optional `provider`, `model`, and `reasoning_effort`. Returns the child identity, canonical task path, and resolved execution selection. |
| `harness_send_message` | Requires `target` and `message`. Supplies additional information without independently starting a new child turn. Reports whether delivery was queued or accepted by an active provider turn. |
| `harness_followup_task` | Requires `target` and `message`. Creates another task for an existing child, retaining that child's conversation and provider selection. |
| `harness_interrupt_agent` | Requires `target`. Stops that child's current work and leaves its identity available for a later explicit follow-up. |
| `harness_list_agents` | Lists the root and children, optionally filtered by task-path prefix, with lifecycle, provider/model, and latest task outcome. |
| `harness_wait_agent` | Waits for child activity, completion, user interruption, or timeout. Returns available notifications and results without requiring another retrieval tool. |

Do not expose `agent_type` or `fork_turns`.

The six operations reproduce the needed v2 capabilities, not Codex's exact wire format. In particular, `wait_agent` should return available result content directly so every backend can consume it through an ordinary tool response.

Use Codex's familiar wait bounds: 30 seconds by default, a 10-second minimum, and a one-hour maximum. Waiting must remain asynchronous and cancellable.

### Message and follow-up semantics

Maintain a durable, ordered child inbox.

For a running child, use native steering only when that adapter supports delivery to the correct active conversation. Otherwise retain the message for its next turn. Do not interrupt and restart a child merely to emulate immediate delivery.

An idle child receiving `send_message` remains idle. Its queued messages accompany its next explicit follow-up task.

Serialize follow-ups for a busy child. Bound the pending follow-up queue to 32 entries per child and return an explicit error when full. Do not silently replace tasks. An idle follow-up that cannot acquire a child execution slot returns a capacity error.

Interruption cancels the active task and queued follow-up tasks for that target. Retain their cancellation outcomes and any informational messages. No queued task may immediately restart an agent the user just interrupted.

Bind all tool authority to the authenticated caller context. Reject forged caller identities, tools invoked after their context expires, and attempts by child sessions to call agent tools even if they guess a tool name.

### Fresh sessions and configuration

Resolve selection before launch:

- Without an override, use the parent's provider and resolved model/settings.
- When another provider is selected, use that provider's configured defaults.
- When an explicit model changes, resolve its supported reasoning settings rather than copying an incompatible value.
- Reject unsupported explicit selections with actionable errors. Do not silently substitute a different provider or model.

Children receive normal Harness execution instructions, workspace instructions, selected resources, and the explicit task. They receive no parent transcript, hidden planning conversation, or root plan/goal authority.

Inherit the root's workspace, execution mode, access policy, and applicable resource selections. Cross-provider creation must not silently weaken sandboxing. If the selected backend cannot satisfy the effective policy, reject creation.

Keep permissions attributable to the exact child exchange and turn. Replace broad cleanup of shared pending approvals with owner-scoped cleanup so interrupting one child does not cancel a sibling's approval.

### Native spawning suppression

Apply native suppression on creation, resume, and configuration refresh, independently of the Subagent tools setting.

| Backend | Implementation |
| --- | --- |
| Codex | Apply session/runtime configuration with `agents.enabled=false`. This is the documented control for disabling multi-agent tools. |
| Copilot | Set exclusions on both create and resume configurations for native delegation and agent communication tools, including `task`, `list_agents`, `read_agent`, `write_agent`, and `send_inbox`. Use built-in-qualified filters so exclusions cannot hide Harness custom tools. |
| Claude Code | Pass native agent-tool exclusions through the ACP adapter's Claude options, covering the pinned adapter's `Agent` tool and applicable legacy alias. |
| Antigravity | Report native suppression as unsupported until the stock ACP integration exposes a verified control. Do not substitute SDK-only configuration or preserve native orchestration as a fallback. |

Codex's `agents.enabled` behavior is documented in the [official subagent settings](https://learn.chatgpt.com/docs/agent-configuration/subagents).

Represent Harness execution support and native suppression as separate capabilities. Unsupported suppression does not turn an AGY child into a native-managed child. If a backend claims suppression support but applying it fails, fail that launch rather than silently enabling native spawning.

Remove native-agent events as a source of managed child identities. Retain ordinary provider cleanup and diagnostic handling where needed, without letting them recreate a second agent registry.

## 4. User interface, settings, and persistence

### `/spawn`

Replace the current native-definition picker with a generic spawn composer using existing picker controls.

Provide:

- Task name.
- Task message.
- Provider.
- Model.
- Reasoning selection where supported.

Default selections from the current root configuration. Present the actual resolved provider/model and report launch failures in the normal notification and agent-status surfaces.

Support `/spawn` for the composer and `/spawn <task_name> <message>` for direct creation using defaults. Remove the previous interpretation of the first argument as a native role definition.

The command returns control after admission. It must not hold the UI busy until the child finishes.

### `/agents`

Replace the singular `/agent` entry point with `/agents`. Extend the existing picker and timeline selection instead of introducing a separate agent viewer.

Show Main, active children, and finished children. Each child exposes its task name, provider/model, execution state, and latest outcome.

Provide actions to:

- Open its conversation and final result.
- Send informational input.
- Assign a follow-up task.
- Interrupt that child.
- Interrupt all children in the current root session.

Keep the list refreshed while open. Preserve selection and scroll position when statuses change. Continue using incremental timeline updates and the shared picker interaction conventions.

Make agent inspection and interruption available when model-facing tools are off. Keep configuration actions rooted in the main session rather than accidentally applying them to the selected child.

### Agent entries in the main timeline

Reuse the existing exchange heading and thought renderer for agent previews. Do not introduce separate agent-card styling or custom text summaries for thoughts.

Each agent entry uses the existing exchange heading format with the agent name, provider, lifecycle, elapsed time, tool metrics, and token metrics. All metrics belong to that child and retain the existing formatting and semantics.

Beneath each running agent's heading, show only its most recent thought using the same UI that renders that content in an ordinary executing exchange. Update the preview in place when a newer thought arrives. Keep the latest thought visible while tools execute, and show no thought preview until the child has emitted one. Do not display tool calls or tool output in the main timeline's agent preview. Tool activity continues to update the heading's existing metrics.

The accepted presentation uses the existing `●` exchange heading and `○` thought rendering. The example values and thought text below are illustrative. Preserve normal wrapping and styling rather than introducing a separate preview format.

```text
● Agent parser-review · Codex · Running 6s │ Tools 2s · 3/3 pass │ Tokens 28.3k (0%) -> 136 · 20 tps

  ○ The parser returns early here. I’m checking whether cleanup still runs.

● Agent test-review · Claude Code · Running 9s │ Tools 4s · 4/5 pass │ Tokens 19.2k (42%) -> 284 · 32 tps

  ○ The existing tests cover malformed input, but none exercise cancellation.
```

Enter on the agent heading opens the corresponding child's full timeline. That timeline renders the child's complete history with the ordinary exchange, thought, and tool UI. Preserve existing thought and tool interactions within their rendered content.

Use the agent entry as the presentation of the spawn operation rather than adding a duplicate raw spawn-tool entry. Reuse this presentation for manually spawned children. Keep the full child transcript available through navigation rather than appending every child event to the main timeline.

### `/config` and capability presentation

Add `PortableSettings.subagent_tools: Option<bool>` through the existing revisioned global, workspace, and session settings pipeline. Resolve an unset value to false.

The toggle governs the root's six model-facing tools. It does not govern manual `/spawn`, the existence of the coordinator, or native suppression. Child conversations always receive zero agent tools.

Refresh root tool exposure through the existing configuration transition machinery. A tool-visibility-only change must preserve admitted children and deliver their eventual results. Reject new hidden tool calls after the change takes effect.

For access-policy changes, quiesce affected execution before applying the new policy. Do not allow children to keep running under authority the user has revoked.

Expose native-suppression support and its actual limitation through `/capabilities`. Avoid confirmation dialogs or claims that unsupported suppression succeeded.

### Cancellation, recovery, and shared files

Root cancellation stops model-delegated continuation and cancels active children. Individual interruption affects only its target. Preserve provider cleanup failures as retryable state, and do not report successful interruption merely because a request was sent.

On process restart, restore identities, transcripts, results, and selections. Mark unfinished work interrupted rather than automatically replaying model calls. An explicit follow-up may resume a retained child conversation.

Retain existing aggregate workspace checkpoints. Require children to settle before root undo or verification finalization. Do not expose per-child rollback as though its edits were isolated from sibling edits.

Keep token and turn metrics attributable to individual agents. Any tree aggregate must be labeled separately from root-only usage.

Bump the internal session format for changed ownership records. Follow the repository's existing policy: hide incompatible old sessions without migration or deletion, and preserve independently compatible preferences.

Explicit root `/fork` remains a separate feature. It must not transfer live child runtimes or actionable child identities into the new root.

## 5. Implementation sequence and verification

### Affected boundaries and modules

Keep runtime ownership in Rust and reuse the existing Lua presentation. The principal change boundaries are:

| Boundary | Modules and required changes |
| --- | --- |
| Agent state | `nvim/rust/forge/crates/forge-harness/src/agent/`: add the coordinator, revise agent and delegation state, replace native control capabilities, and remove native catalog discovery from the active path. |
| Execution ownership | The Harness service, session controller, broker, and backend request types: add agent-scoped runtime ownership and route child operations without reacquiring the active root prompt's broker lock. |
| Provider adapters | Codex, Copilot, and ACP backends: isolate connection/session maps, apply native suppression, and scope event routing, cancellation, and permissions. |
| Tool contract | `control_tools/` and ACP's private control server: register the six schemas, dispatch authenticated requests, and filter tool exposure per root/child context and setting. |
| Settings and storage | `settings/settings.rs`, storage records, and session format: persist the toggle and child state, preserve compatible preferences, and hide incompatible sessions. |
| User interaction | `nvim/lua/forge/views/harness/controller.lua`, `spawn_picker.lua`, and `agent_picker.lua`: replace native-role spawning, add `/agents`, and retain live timeline selection. |
| Verification and documentation | Existing Rust tests, `nvim/tests/forge/agent_picker.lua`, controller/settings tests, `nvim/lua/forge/docs/architecture.md`, and `nvim/tests/forge/plans/harness-owned-agents.md`: verify the new ownership model and replace obsolete native-fallback guidance. |

### Delivery sequence

1. **Establish execution ownership.** Add the coordinator, agent-scoped provider keys, durable task state, and fake-backend tests. Reuse existing records and rendering code.
2. **Implement generic fresh children.** Support independent conversations and provider selection across all four adapters, with scoped permissions and cancellation.
3. **Replace orchestration and expose tools.** Route the six tools through the coordinator, implement inboxes and parent continuation, and remove parent-mediated spawning.
4. **Update the user controls.** Replace the native-role `/spawn` workflow, introduce `/agents`, and add the persisted tool toggle.
5. **Complete the cutover.** Remove obsolete native-agent control modes and catalog discovery, update capability reporting, documentation, and tests.

Preserve unrelated in-progress ACP and settings changes. Do not edit generated provider instruction files. Update the architecture document and replace the obsolete native-fallback recommendation in the existing agent design notes.

### Automated acceptance tests

Use deterministic fake backends and controlled completion barriers for lifecycle tests.

**Creation and ownership**

- Every root/child provider pairing routes to the correct adapter.
- A new child receives no parent history.
- A follow-up retains only that child's existing history.
- Eight children can run concurrently. A ninth spawn fails without leaking records or processes.
- Duplicate delivery of the same admitted request does not create another child.
- Duplicate task names and foreign-session targets fail predictably.

**Communication and continuation**

- Informational messages never start idle children.
- Supported steering reaches the intended active child.
- Unsupported steering retains the message for the next follow-up.
- Follow-ups serialize and enforce the queue bound.
- Child completion during `wait_agent` returns its result.
- A stopped parent turn automatically continues its unfinished model-delegated request.
- Simultaneous results produce serialized, coalesced continuation.
- Manual child completion never starts a root model turn.
- Explicit pause, cancellation, or a pending user decision prevents automatic continuation.

**Isolation and cleanup**

- Root and children cannot overwrite each other's connection or control contexts.
- Cancelling one child leaves siblings and their approvals intact.
- Root cancellation collects all active children and suppresses late restarts.
- Cleanup failure remains visible and retryable.
- Reopening restores completed results and marks unfinished work interrupted.
- Root forking does not duplicate running children.

**Tool and provider configuration**

- Root tools are absent by default and present when enabled.
- Children receive no agent tools under either setting.
- Manual spawning works with the toggle off.
- Native suppression is applied to create and resume paths independently of the toggle.
- Unsupported AGY suppression is reported accurately.
- Supported-suppression failures do not fall back to native orchestration.
- Cross-provider selection cannot weaken the root's required access policy.

**UI and persistence**

- Child lifecycle, thought, tool, usage, and result events pass through the shared timeline pipeline with the correct ownership.
- Streaming thought updates refresh the same preview, and tool output streams into the child's full timeline without appearing in the main preview.
- Interleaved sibling events, repeated delivery, and late events preserve ordering and never duplicate content or update another child or a newer turn.
- Child updates remain visible with the root running, waiting, or idle, and reopening reconstructs the saved child history and outcomes.
- `/spawn` admission, failure, and completion update `/agents`.
- Main and child conversations retain correct attribution.
- Picker selection survives live updates.
- Agent headings reuse the existing exchange layout and show only that child's metrics.
- Each running agent previews only its latest thought through the existing renderer and updates in place when a newer thought arrives.
- Tool execution preserves the latest thought preview, updates heading metrics, and remains inspectable in the child's full timeline.
- Enter on an agent heading opens the correct child timeline without changing existing thought or tool interactions.
- Messaging, follow-up, individual interruption, and interrupt-all reach the intended targets.
- Settings survive reopening at the correct scope.
- Current-format sessions reopen and incompatible sessions remain hidden.

### Runtime verification

After targeted Rust and Neovim tests pass, rebuild the release executable that Neovim actually launches. Use the existing Forge build configuration and target directory, not a separate debug build.

In a fresh Neovim host, verify:

1. Manual spawn and result inspection with the tool toggle off.
2. Model-initiated spawn, active waiting, and automatic parent synthesis with it on.
3. Two simultaneous children, individual interruption, and interrupt-all.
4. Same-provider execution for each configured backend.
5. Cross-provider execution covering every adapter as both parent and child.
6. Completion and selection updates while `/agents` remains open.
7. Reopening a session with completed and interrupted child history.

Run real-provider checks in a temporary workspace with bounded tasks. Report authentication or capability blockers separately from passes. Record commands, timeout bounds, exit statuses, and the rebuilt executable path. Automated success and manual interaction verification remain separate acceptance evidence.

### Release and completion criteria

Keep model-facing tools disabled by default when the implementation ships. Manual `/spawn` provides the initial inspection path without enabling autonomous delegation.

Record child admission, task start, task completion, interruption, cleanup failure, and parent continuation with Harness session, agent, exchange, and turn identity. A missing child outcome, incorrect provider routing, cross-agent cancellation, or duplicate parent continuation fails acceptance.

Do not restore native orchestration as a runtime fallback. If runtime verification fails, keep the replacement unreleased and repair the failing ownership or lifecycle boundary. If the deployed change requires reversal, revert its implementation as a unit and retain stored sessions without deletion, subject to the restored binary's format compatibility rules.

The work is complete when all six tools, `/spawn`, `/agents`, the settings toggle, native-suppression capability reporting, durable child recovery, and parent-result continuation satisfy the automated and runtime checks above. Documentation must describe the Harness-owned model, and no native-agent event may create a managed child identity.
