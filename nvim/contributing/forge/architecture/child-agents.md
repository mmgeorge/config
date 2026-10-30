# Harness child agents and inspection

Delegated work must not outlive its parent's authority or replay after a crash, and an agent reading session history must not gain that session's permissions. This chapter covers both: child conversations that run under a root session, and the read-only tools an agent uses to inspect sessions.

## Child ownership

`AgentCoordinator` owns child conversations per root session: unique `/root/<name>` paths, durable `AgentTask` records, inboxes, follow-up queues, and cancellation. Admission reserves one of eight active slots, then commits the agent, the parent `Delegation`, the queued child `Exchange`, and the task in one transaction, so a crash cannot leave a child without its parent record. Each child runs one task at a time, with up to 32 pending follow-ups, and starts with fresh provider history in the shared workspace.

Provider tool callbacks reach the coordinator through a 64-entry channel instead of the broker lock, and tool authority ends with the root turn. Children never receive delegation or root planning tools.

## Tools

The portable `subagent_tools` setting is off by default, and `/agents on` and `/agents off` change it. Enabling it exposes `agent_spawn`, `agent_send`, `agent_followup`, `agent_interrupt`, `agent_list`, `agent_activity`, and `agent_tool_output`. There is no wait tool. When a root turn ends with unfinished children, the exchange stays open, and Harness starts one continuation with their reports once they settle. New user input resumes the exchange immediately.

`agent_activity` returns at most 20 recent entries, five by default, without tool output. `agent_tool_output` reads at most 32,000 characters per call through references that validate ownership.

## Profiles

`AgentProfileStore` owns the global `stdpath("config")/forge/subagents.json`. A profile holds a purpose, instructions, enabled state, provider, model, thinking level, speed, and permission mode. `inherit` copies the root's choice at admission, and `default` selects the provider default. Saves are revision-checked atomic replacements.

Each child persists its resolved profile, so editing a profile never changes existing children. A child's effective permissions are the stricter of its snapshot and the current root.

## Interruption

Child approvals cancel only that child's requests on interruption. Root cancellation and access changes collect child execution first, with a 30-second cleanup bound. On reopen, unfinished child tasks become interrupted and never replay.

## Session inspection

The root interaction exposes `harness_config` and seven `harness_session_*` tools through `control_tools/harness`, and the `harness-sessions` skill documents them. The portable `session_tools` setting is on by default, and turning it off removes the history tools but keeps `harness_config`.

Inspection uses read-only SQLite connections without migrations or leases. Reading another session requires authorization against its workspace, and that session's saved permissions never authorize the caller. Results go to a private temporary snapshot with opaque continuations, and page budgets bound each delivery, not the total size. Log inspection exposes only allowlisted diagnostic metadata, never prompts, credentials, or private reasoning.
