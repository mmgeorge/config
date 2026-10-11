# Harness-Owned Subagents

Forge owns child creation, task scheduling, cancellation, persistence, and result delivery across
Codex, Copilot, Claude ACP, and Antigravity ACP. Native delegation events never create managed
child identities. The accepted contract is [plans/subagent.md](plans/subagent.md), and the runtime
boundaries are documented in [the Harness architecture](nvim/lua/forge/docs/architecture.md).

## User controls

| Control | Outcome on all four providers |
| --- | --- |
| `/spawn` | Open a composer for a unique task name, task message, provider, model, and reasoning effort. Admit a fresh independent conversation in the current workspace. |
| `/agents` | Inspect Main, Running, and Done conversations. Select a child, send information, queue a follow-up, interrupt one child, or interrupt all children. |
| Child informational message | Persist the information without starting an idle child. Steer an active provider turn when supported, otherwise attach the information to its next explicit follow-up. |
| Child follow-up | Reuse that child's own provider history and serialize behind its active task. |
| Child interruption | Cancel active and queued tasks. Retain identity, transcript, selection, and inbox. Keep failed cleanup visible and retryable. |
| Root cancellation | Cancel active children and prevent automatic parent continuation. |
| Root result delivery | Return completed results through the active wait call or continue the same unfinished parent Exchange after its provider turn ends. Manual completion never starts a root model turn. |
| Reopen | Restore retained identities, selections, transcripts, and results. Mark unfinished tasks interrupted without replaying provider calls. |
| Main preview | Show the latest child thought and the child's heading metrics. Keep child tool output and earlier thoughts in the full child timeline. Enter on the heading opens that timeline. |
| Tool toggle | Persist `subagent_tools` through Forge settings. Default off. Manual spawning remains available. A visibility change preserves admitted children. |

The enabled root receives `harness_spawn_agent`, `harness_send_message`, `harness_followup_task`,
`harness_interrupt_agent`, `harness_list_agents`, and `harness_wait_agent`. Children receive zero
agent tools and no root planning authority. Successful spawn calls appear as agent entries,
while failed calls retain their tool error rows.

## Provider differences

| Boundary | Codex | Copilot | Claude ACP | Antigravity ACP |
| --- | --- | --- | --- | --- |
| Native delegation suppression | Set `agents.enabled=false` on create and resume. | Exclude builtin-qualified delegation tools on create and resume. | Disallow `Agent` and `Task` on create and resume. | No exposed suppression control. `/capabilities` reports unsupported. |
| Active child informational message | Use native steering against the owned runtime. | Use native steering against the owned runtime. | Use the advertised steering extension when accepted, otherwise retain for the next explicit follow-up. | Retain for the next explicit follow-up. |
| Required OS sandbox | Preserve the root policy. | Preserve the root policy. | Reject admission when the root requires an OS sandbox that ACP cannot enforce. | Reject admission when the root requires an OS sandbox that ACP cannot enforce. |

Provider selection does not translate model identifiers. Same-provider children inherit the
root's resolved tuning unless explicitly overridden. Cross-provider children resolve the target
provider's saved model, reasoning effort, context window, and service tier against its catalog.
Every child uses the root's current access policy and permission mode.

## Identity, durability, and bounds

`ExecutionOwner` combines the Harness session and agent identifiers. Adapters use the owner for
connection maps, control contexts, event delivery, cancellation, and permissions. Settings and
resources retain the root session scope. Identical native request identifiers across siblings do
not grant one child authority over another.

Each child has a UUID and a unique `/root/<task_name>` path. Admission reserves capacity before
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

Session format 41 retains the new ownership records. Incompatible sessions remain stored and
hidden. Preference format 40 remains independent. Explicit root forking copies preferences and
root history without copying actionable child identities or live runtimes.

## Verification

[Harness-owned agent verification](nvim/tests/forge/plans/harness-owned-agents.md) records the
automated and real-provider evidence. The real fixture launches a fresh rebuilt host in a
temporary workspace with bounded tasks. Automated checks cover all 16 provider pairings. Live
checks cover every provider as a parent and a child, with authentication and native-suppression
limitations reported separately from successful execution.
