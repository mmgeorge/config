# Replace native-agent orchestration with Harness-owned subagents

## 1. Outcome and agreed behavior

Harness will create and manage independent provider conversations for subagents. Providers will execute individual agent turns, while Harness owns delegation, identity, scheduling, communication, cancellation, persistence, and presentation.

This replaces the prior parent-mediated implementation, which instructed the parent model to invoke its native spawning tool and waited for provider events to identify the resulting child. The existing agent records, timeline rendering, and picker provide reusable foundations.

The initial implementation has these fixed decisions:

| Area | Decision |
| --- | --- |
| Providers | Support Codex, Copilot, Claude Code, and Antigravity, including children using a different provider from their parent. |
| Context | Every newly spawned child starts a fresh provider conversation. No parent-history copying or forking. |
| Profiles | User-owned global profiles select provider, model, thinking, speed, instructions, and a permission ceiling. Do not load native or Rulesync subagent definitions. |
| Workspace | Children share the root session's workspace. No worktree creation or automatic change merging. |
| Concurrency | Permit eight simultaneously active children per root session, excluding the root. |
| Delegation authority | Only the root can spawn and manage children. Children receive none of the seven agent tools. |
| Tool setting | Model-facing subagent tools default to off. Manual `/agents spawn` remains available. |
| Native spawning | Disable it for every root and child wherever the provider supports suppression. |
| Model-delegated completion | Continue the parent's unfinished request automatically to process child results. |
| Manual completion | Display and save the result without automatically running the main agent. |

Follow-up tasks reuse the child's own conversation. "Fresh context" applies when creating a new agent, not when continuing an existing one.

Automatic `explore` orchestration, plan decomposition, verification-agent selection, automatic role selection, and nested spawning remain outside this implementation.

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

Use `/root/<task_name>` as the canonical task path. Allocate unique instance names from the selected profile within a root session. Repeated use of one profile creates independent instances. Resolve tool targets by stable ID, canonical path, or an unambiguous task name within that session. Reject cross-session targets.

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

The main timeline derives each child's heading, latest non-final thought, and latest tool heading from this event-fed state. Tool output stays out of the running preview. Completion replaces the preview with the delegated task's complete timeline inside an automatically collapsed fold. The selected child's full timeline receives its normal live thought, message, and tool updates. Both views must remain current while the root is running, waiting, or idle, without requiring another root model turn or manual refresh.

Persist child history and terminal outcomes through the existing storage lifecycle so reopening the session reconstructs the same timelines. Preserve the existing handling of transient streaming updates. Elapsed time may use the existing display clock, but child activity must come from execution events rather than polling provider transcripts.

### Parent continuation

A model-delegated child belongs to the parent exchange that created it. Ending one parent model turn does not complete that exchange while delegated work remains unresolved.

When results arrive:

- Persist the result before publishing its notification.
- Queue one compact, attributed parent message containing the child path, task ID, status, response, and error. Deliver it to an active provider through supported steering without rendering it as a user prompt.
- If the parent has stopped generating while delegated work remains, keep the exchange open and suspend provider generation. Resume automatically after all model-delegated children for that exchange settle, or immediately when the user submits new input. New input retains the same exchange and leaves children running.
- Serialize parent turns and coalesce already-available results. Concurrent child completions must not start concurrent parent turns.
- Include child identity, task, terminal outcome, and final response. Preserve the full child transcript separately.

Use the existing continuation limits and error reporting. Never continue after explicit user cancellation, pause, or an unresolved user decision.

Manual children publish the same lifecycle and result data, but completion alone never schedules a root model turn.

## 3. Tools, backend integration, and policy

### Model-facing contract

Register these through the existing Harness control-tool infrastructure, using the established `harness_` naming convention. One schema and dispatcher must serve Codex dynamic tools, Copilot custom tools, and ACP's private MCP endpoint.

| Tool | Contract |
| --- | --- |
| `harness_spawn_agent` | Requires `text` and accepts optional `profile`, defaulting to the configured default profile. The profile owns all tuning. Returns the child identity, canonical task path, profile, and resolved execution selection. |
| `harness_send_message` | Requires `target` and `message`. Supplies additional information without independently starting a new child turn. Reports whether delivery was queued or accepted by an active provider turn. |
| `harness_followup_task` | Requires `target` and `message`. Creates another task for an existing child, retaining that child's conversation and provider selection. |
| `harness_interrupt_agent` | Requires `target`. Stops that child's current work and leaves its identity available for a later explicit follow-up. |
| `harness_list_agents` | Lists the root and children, optionally filtered by task-path prefix, with lifecycle, provider/model, and latest task outcome. |
| `harness_agent_activity` | Requires `target`, with optional `limit` from 1–20. Returns recent thoughts and tool metadata, timing and approval state, without tool output or final responses. Selects the active run before queued follow-ups. |
| `harness_agent_tool_output` | Requires the opaque `call_ref` returned by activity inspection. Reads existing output with zero-based Unicode character `offset` and `limit` (default 8000, maximum 32000). Returns current status, failure flag, total character count, and next offset. Never re-executes a call. |

Do not expose `agent_type` or `fork_turns`.

The seven operations expose child management without a model-facing wait tool. Harness owns waiting after a provider turn ends. The broker acknowledges a child report only after the provider accepts it. Unsupported or rejected active delivery retains the report for the next provider input boundary. An unfinished delegated exchange can continue after its provider turn ends. Completion alone never starts an idle root conversation. Pending reports also accompany the next explicit parent input without replaying already delivered reports.

Waiting remains asynchronous and cancellable. A child completion wakes the coordinator without starting another provider turn until all relevant children settle. New user input resumes Main even on providers without native steering. Keep the existing Working, Planning, Implementing, and other activity labels. Append the running-child count independently. Show the existing spinner with `Waiting for agents` only when Main has no active work. Explicit pause, cancellation, and unresolved user decisions take precedence and must not trigger automatic continuation.

### Message and follow-up semantics

Maintain a durable, ordered child inbox.

For a running child, use native steering only when that adapter supports delivery to the correct active conversation. Otherwise retain the message for its next turn. Do not interrupt and restart a child merely to emulate immediate delivery.

An idle child receiving `send_message` remains idle. Its queued messages accompany its next explicit follow-up task.

Serialize follow-ups for a busy child. Bound the pending follow-up queue to 32 entries per child and return an explicit error when full. Do not silently replace tasks. An idle follow-up that cannot acquire a child execution slot returns a capacity error.

Interruption cancels the active task and queued follow-up tasks for that target. Retain their cancellation outcomes and any informational messages. No queued task may immediately restart an agent the user just interrupted.

Bind all tool authority to the authenticated caller context. Reject forged caller identities, tools invoked after their context expires, and attempts by child sessions to call agent tools even if they guess a tool name.

### Fresh sessions and configuration

Resolve an enabled profile from the global `stdpath("config")/forge/subagents.json` before launch.
The document contains `default_profile`, `activity_limit` (default 5, range 1-20), and a named `profiles` map. Each profile supplies
`enabled`, `description`, `instructions`, `provider`, `model`, `reasoning_effort`, `service_tier`,
and `permission_mode`. No duplicate editable profile state belongs in SQLite.

- Resolve `inherit` independently against the root configuration at admission.
- Resolve model/thinking/speed `default` against the selected provider's defaults.
- Validate explicit and cross-provider model/thinking combinations without silently substituting another model.
- Persist the profile name, resolved tuning, child instructions, and permission ceiling on the child.
- Retain that snapshot for follow-ups even if the global profile changes or disappears.
- Apply the stricter of captured and current root permissions at every launch.

`/agents edit` opens profile selection, and `/agents edit <profile>` opens the named profile.
`/agents on` and `/agents off` set the persisted `subagent_tools` setting explicitly through
the same configuration path as `/config`. Repeated commands retain the requested state and
remain local controls while a child timeline is selected or work is running.
`/agent edit [profile]` is an alias. Both edit pickers include enabled and disabled profiles.
Tab selects Enabled, Provider, Model, Thinking, or Speed. Left/Right (also r/h in pickers)
cycles the selected field and saves immediately without moving the profile selection. Provider
changes select the first available model in provider order. Provider/model changes retain valid
thinking and speed or reset them to default. Thinking choices come from the model catalog, and
speed choices come from provider capabilities. Keep compatible inherit choices with resolved
parent values visible. Apply the same dependency rules in the full-page editor. Failed or empty
catalog loads leave all fields unchanged. Save dependent changes atomically and reject stale
callbacks. Failed saves retain the previous value. The default profile cannot be disabled
until another enabled profile becomes the default.
The final `Open config file` row opens the global `subagents.json` in a normal editable tab
on Enter. It does not expose profile toggle or delete actions.
Selecting or creating a profile opens a full-page form using the shared editable-region and
choice controls used by existing Forge UI. Keep text edits local, cycle choices inline, and save
the complete form with `:write` or `<C-s>`. Preserve newer edits across asynchronous saves and
retain drafts after failures. Reload discards local edits. Keep the profile editor out of `/config`.
The form edits the same JSON file. Validate saves, detect stale revisions,
and atomically replace the file. Reload external edits on access. Retain the last valid catalog
and display errors for invalid external edits. A newly started host with no valid catalog rejects
spawning until repair. Model tool schemas advertise only enabled profile names and purposes.

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

### `/agents spawn`

Use the existing picker to select an enabled profile, then compose its task text.
`/agents spawn <profile> <text>` starts directly. `/agents spawn <profile>` opens task entry with that profile
selected. Completion lists enabled profiles. The parent cannot choose provider, model, thinking,
or speed through spawn arguments. Repeated spawns create independently named instances.

The command returns control after admission. It must not hold the UI busy until the child finishes.

### `/agents`

Replace the singular `/agent` entry point with `/agents`. Extend the existing picker and timeline selection instead of introducing a separate agent viewer.

Show Main, active children, and finished children. Each child exposes its instance name, profile, resolved provider/model/thinking/speed, execution state, and latest outcome.

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

Beneath each running agent's heading, show its most recent non-final thought and latest tool heading using the ordinary thought and tool renderers. Update each preview in place when newer content arrives. Keep the thought visible while tools execute, and omit it until the child emits one. Do not display tool output or final answers in the running preview. Tool activity continues to update the heading's metrics.

Nest agent folds inside their owning parent Thought fold. Use the existing fold marker, `○` thought rendering, and `•` tool heading. The example values and thought text below are illustrative. Preserve normal wrapping and styling.

```text
▾ Thought 9s │ Tools 4s · 4/5 pass
  ▾ Agent parser-review · codex · gpt-6.1-sol ++ · Running 6s │ Tokens 28.3k (0%) -> 136 · 20 tps
    ○ The parser returns early here. I’m checking whether cleanup still runs.
    •  0.2s sem.sem_context(...)
  ▸ Agent test-review · claude · Completed 9s │ Tokens 19.2k (42%) -> 284 · 32 tps
```

On completion, replace the live preview with the delegated exchange's full timeline, including its prompt, thoughts, tools, and final answer, and automatically collapse the agent fold. Keep normal child Thought and tool folds inside it. Explicit expansion after completion persists across later updates. Keep the parent's final answer outside its Thought fold.

Enter on the first line of the agent fold opens the corresponding child's full timeline, whether running or completed. Fold controls expand and collapse inline content separately. The child timeline includes the agent's complete conversation across follow-ups with the ordinary exchange, thought, and tool UI.

Use the agent entry as the presentation of the spawn operation rather than adding a duplicate raw spawn-tool entry. Reuse this presentation for manually spawned children. Keep completed task history inside its child fold and the complete child conversation available through navigation.

### `/config` and capability presentation

Add `PortableSettings.subagent_tools: Option<bool>` through the existing revisioned global, workspace, and session settings pipeline. Resolve an unset value to false.

The toggle governs the root's seven model-facing tools. It does not govern manual `/agents spawn`, the existence of the coordinator, or native suppression. Child conversations always receive zero agent tools.

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
| Tool contract | `control_tools/` and ACP's private control server: register the seven schemas, dispatch authenticated requests, and filter tool exposure per root/child context and setting. |
| Settings and storage | `settings/settings.rs`, storage records, and session format: persist the toggle and child state, preserve compatible preferences, and hide incompatible sessions. |
| User interaction | `nvim/lua/forge/views/harness/controller.lua`, `spawn_picker.lua`, and `agent_picker.lua`: replace native-role spawning, add `/agents`, and retain live timeline selection. |
| Verification and documentation | Existing Rust tests, `nvim/tests/forge/agent_picker.lua`, controller/settings tests, `nvim/lua/forge/docs/architecture.md`, and `nvim/tests/forge/plans/harness-owned-agents.md`: verify the new ownership model and replace obsolete native-fallback guidance. |

### Delivery sequence

1. **Establish execution ownership.** Add the coordinator, agent-scoped provider keys, durable task state, and fake-backend tests. Reuse existing records and rendering code.
2. **Implement generic fresh children.** Support independent conversations and provider selection across all four adapters, with scoped permissions and cancellation.
3. **Replace orchestration and expose tools.** Route the seven tools through the coordinator, implement inboxes and parent continuation, and remove parent-mediated spawning.
4. **Update the user controls.** Replace the native-role `/agents spawn` workflow, introduce `/agents`, and add the persisted tool toggle.
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
- Child completion delivers its result separately exactly once after provider acceptance. An idle parent resumes after all relevant children settle, or on new user input. Cancellation and user-decision boundaries prevent automatic continuation.
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
- `/agents spawn` admission, failure, and completion update `/agents`.
- Main and child conversations retain correct attribution.
- Picker selection survives live updates.
- Agent headings reuse the existing exchange layout and show only that child's metrics.
- Each running agent previews its latest non-final thought and tool heading through the existing renderers, without tool output or final answers.
- Completion collapses the agent fold over its full task timeline. Explicit expansion after completion persists.
- Agent folds stay inside the owning parent Thought fold, and the parent's final answer stays outside it.
- Enter on an agent heading opens the correct child timeline while running or completed without changing fold controls.
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

Keep model-facing tools disabled by default when the implementation ships. Manual `/agents spawn` provides the initial inspection path without enabling autonomous delegation.

Record child admission, task start, task completion, interruption, cleanup failure, and parent continuation with Harness session, agent, exchange, and turn identity. A missing child outcome, incorrect provider routing, cross-agent cancellation, or duplicate parent continuation fails acceptance.

Do not restore native orchestration as a runtime fallback. If runtime verification fails, keep the replacement unreleased and repair the failing ownership or lifecycle boundary. If the deployed change requires reversal, revert its implementation as a unit and retain stored sessions without deletion, subject to the restored binary's format compatibility rules.

The work is complete when all seven tools, `/agents spawn`, `/agents`, the settings toggle, native-suppression capability reporting, durable child recovery, and parent-result continuation satisfy the automated and runtime checks above. Documentation must describe the Harness-owned model, and no native-agent event may create a managed child identity.
