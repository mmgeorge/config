# Harness-Owned Subagents

Forge owns child creation, task scheduling, cancellation, persistence, and result delivery across
Codex, Copilot, Claude ACP, and Antigravity ACP. Native delegation events never create managed
child identities. The accepted contract is [plans/subagent.md](plans/subagent.md), and the runtime
boundaries are documented in [the Harness architecture](nvim/lua/forge/docs/architecture.md).

## User controls

| Control | Outcome on all four providers |
| --- | --- |
| `/agents spawn` | Select an enabled profile and compose task text. `/agents spawn <profile> <text>` starts immediately with a fresh conversation. |
| `/agents` | Inspect Main, Running, and Done conversations with profile and resolved tuning. Select a child, send information, queue a follow-up, or interrupt work. |
| `/agents on` / `/agents off` | Enable or disable subagent tools through the same persisted setting as `/config`. Repeating a command retains its requested state. |
| `/agents edit [profile]` | Open global profile selection, or open the named profile in a full-page form. |
| `/agent edit [profile]` | Alias for `/agents edit`, including disabled profiles in the picker. |
| Child informational message | Persist the information without starting an idle child. Steer an active provider turn when supported, otherwise attach the information to its next explicit follow-up. |
| Child follow-up | Reuse that child's own provider history and serialize behind its active task. |
| Child interruption | Cancel active and queued tasks. Retain identity, transcript, selection, and inbox. Keep failed cleanup visible and retryable. |
| Root cancellation | Cancel active children and prevent automatic parent continuation. |
| Root result delivery | Queue one attributed child-result message. Deliver it through supported active steering without a user-prompt row, or at the next provider input boundary. Harness resumes an unfinished delegated exchange after all its children settle, or immediately on new user input. Manual completion never starts a root model turn. |
| Reopen | Restore retained identities, selections, transcripts, and results. Mark unfinished tasks interrupted without replaying provider calls. |
| Main preview | Nest child folds under the parent Thought fold. While running, show the latest non-final thought and latest tool heading without output. On completion, collapse the fold over the delegated task's complete timeline. Enter on the heading opens the child timeline in either state. |
| Tool toggle | Persist `subagent_tools` through Forge settings. Default off. Manual spawning remains available. A visibility change preserves admitted children. |

The enabled root receives `harness_spawn_agent`, `harness_send_message`, `harness_followup_task`,
`harness_interrupt_agent`, `harness_list_agents`, `harness_agent_activity`, and
`harness_agent_tool_output`. Children receive zero
agent tools and no root planning authority. Successful spawn calls appear as agent entries,
while failed calls retain their tool error rows.

Main retains its existing activity label and appends the running-agent count. When Main has no
active work, the existing spinner shows `Waiting for agents`. A new prompt resumes the same
exchange without cancelling children, including when native steering is unavailable. Explicit
pause, cancellation, and unresolved user decisions prevent automatic continuation. There is no
model-facing wait tool.

Agent headings append `+` after the model for fast speed and `++` for ultrafast. Standard speed
has no suffix. The heading uses the child's captured settings during execution and after completion.

## Provider differences

| Boundary | Codex | Copilot | Claude ACP | Antigravity ACP |
| --- | --- | --- | --- | --- |
| Native delegation suppression | Set `agents.enabled=false` on create and resume. | Exclude builtin-qualified delegation tools on create and resume. | Disallow `Agent` and `Task` on create and resume. | No exposed suppression control. `/capabilities` reports unsupported. |
| Active child informational message | Use native steering against the owned runtime. | Use native steering against the owned runtime. | Use the advertised steering extension when accepted, otherwise retain for the next explicit follow-up. | Retain for the next explicit follow-up. |
| Required OS sandbox | Preserve the root policy. | Preserve the root policy. | Reject admission when the root requires an OS sandbox that ACP cannot enforce. | Reject admission when the root requires an OS sandbox that ACP cannot enforce. |

Global profiles live in `stdpath("config")/forge/subagents.json` beside permissions. Edit them
through `/agents edit` or directly as JSON. The editor supports creation, renaming, deletion,
purpose, instructions, provider, model, thinking, speed, permission mode, enabled state, and the
default profile. File saves reject stale revisions, and invalid external edits retain the host's
last valid catalog with a diagnostic.

Selecting a profile opens a dedicated full-page buffer using the shared editable-region and
field controls. In the profile picker, Left/Right or `r`/`h` toggles the selected profile's Enabled value
and saves immediately while retaining the selection. The default profile must remain enabled
until another profile becomes the default. In the form, edit text in place, use Left/Right for
choices, and save all fields with `:write`
or `<C-s>`. Unsaved fields show `*`. `R` reloads the saved profile and discards local edits.
`q` returns to profile selection after saving or reloading. `/config` retains the subagent-tools
toggle but does not contain the profile editor. The standalone `/spawn` command is removed.

The final **Open config file** row in `/agent edit` and `/agents edit` opens the global
`subagents.json` in a normal editable tab when you press Enter.

The **Activity entries** row changes the global `activity_limit` with Left/Right or `r`/`h`.
It defaults to five and accepts 1–20. `harness_agent_activity` can override that count for one
request. Results contain recent thoughts and tool metadata, with no tool output or final
answers. Thoughts retain up to 4000 characters and report truncation explicitly.

Each tool entry includes a `call_ref`. Main can pass it to `harness_agent_tool_output` to read
the recorded result in pages, including while the child runs or after a follow-up begins.
`offset` and `limit` count Unicode characters. The default page size is 8000 and the maximum
is 32000. Use `next_offset` to continue. Reads never execute the tool again.

A profile's provider can inherit the root provider. Model, thinking, and speed can independently
use `inherit`, `default`, or an explicit selection. `inherit` captures the root value at spawn.
`default` uses the selected provider's default. Mixed combinations must pass the target provider's
catalog validation. An inherited model unavailable on another provider produces an error.
Profile permission mode only narrows the root mode. All children retain the root OS access policy.

The spawn tool accepts only optional `profile` and required `text`. It cannot override tuning.
Each child captures its profile and resolved settings, including instructions. Later profile edits
apply to new children. Follow-ups retain the existing child's settings, constrained by the current
root permissions. All children start without parent history.

A minimal catalog can keep every selection inherited. `default` can replace `inherit` for
model, thinking, or speed when the profile should use the selected provider's default.

```json
{
  "default_profile": "worker",
  "activity_limit": 5,
  "profiles": {
    "worker": {
      "enabled": true,
      "description": "Implement a scoped task",
      "instructions": "Complete the assigned task and report the result.",
      "provider": "inherit",
      "model": "inherit",
      "reasoning_effort": "inherit",
      "service_tier": "inherit",
      "permission_mode": "inherit"
    }
  }
}
```

## Identity, durability, and bounds

`ExecutionOwner` combines the Harness session and agent identifiers. Adapters use the owner for
connection maps, control contexts, event delivery, cancellation, and permissions. Settings and
resources retain the root session scope. Identical native request identifiers across siblings do
not grant one child authority over another.

Each child has a UUID and a unique `/root/<profile>-<instance>` path. Admission reserves capacity before
one database transaction writes the Agent, Delegation, queued Exchange, and AgentTask. Replaying
an admitted operation returns the existing identity. Conflicting operation reuse fails.

The root admits at most eight active children, including starting, approval-blocked, and cleanup
states. Each child retains at most 32 pending follow-ups. The coordinator request channel holds
at most 64 requests. Cleanup receives a 30-second bound, and failed cleanup retains its capacity
slot until an explicit retry succeeds.

Children stream through the shared Exchange and Turn pipeline. Completed tasks ignore late
content and duplicate completion. Result acknowledgment follows successful parent dispatch.
Undo and verification finalization require child execution to quiesce because all agents share
the workspace and the root's aggregate checkpoint.

Session format 42 retains the new ownership records. Incompatible sessions remain stored and
hidden. Preference format 40 remains independent. Explicit root forking copies preferences and
root history without copying actionable child identities or live runtimes.

## Verification

[Harness-owned agent verification](nvim/tests/forge/plans/harness-owned-agents.md) records the
automated and real-provider evidence. The real fixture launches a fresh rebuilt host in a
temporary workspace with bounded tasks. Automated checks cover all 16 provider pairings. Live
checks cover every provider as a parent and a child, with authentication and native-suppression
limitations reported separately from successful execution.
