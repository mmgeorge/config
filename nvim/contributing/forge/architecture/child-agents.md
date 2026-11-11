# Harness child agents and inspection

Delegated work must not outlive its parent's authority or replay after a crash, and an agent reading session history must not gain that session's permissions. This chapter covers both: child conversations that run under a root session, and the read-only tools an agent uses to inspect sessions.

## Child ownership

`AgentCoordinator` owns child conversations per root session: unique `/root/<name>` paths, durable `AgentTask` records, inboxes, follow-up queues, and cancellation. Admission reserves one of eight active slots, then commits the agent, the parent `Delegation`, the queued child `Exchange`, and the task in one transaction, so a crash cannot leave a child without its parent record. Each child runs one task at a time, with up to 32 pending follow-ups, and starts with fresh provider history in the shared workspace.

Provider tool callbacks reach the coordinator through a 64-entry channel instead of the broker lock, and tool authority ends with the root turn. Children never receive delegation or root planning tools.

Two drains read that channel: the active root turn and the idle controller task. The idle task takes the broker lock for one command at a time through `drain_next_agent_command`, so a slow command, such as a child cleanup retry, does not also hold back the session requests and root events waiting behind the rest of the queue. The idle drain still applies child events and completions while a root turn runs, but it has no parent exchange to authenticate a model request against. It therefore hands any model request whose authority is still live to the turn through `AgentCoordinator::defer_to_foreground`, which the turn reads first. `AgentForegroundClaim`, held by each `ForegroundTransaction`, marks that a turn owns those requests. When the claim drops, the idle drain receives the leftovers and rejects them as unauthenticated. Before this hand-off, the idle drain could win the race and reject every `agent_spawn` with "agent request has no active authenticated root authority".

The idle drain settles each command independently. A child event or completion that fails is published as an error notice among the batch's events, and a completion is applied even when publishing its approval cancellations fails, because an unapplied completion leaves an execution with no runtime. `drain_next_agent_command` settles one command at a time so a caller can release the broker lock between commands. Once the lane is empty and no root turn holds the foreground claim, the drain launches every queued child that capacity now admits; the active root turn was previously the only poller, so an admitted child waited for the next root prompt. A queued task whose child requires user authorization cannot launch, so `has_active_task` and `has_unresolved_task` exclude it and a waiting parent does not park on it.

Managed children use the same retained native runner and `Turn.Completed` collection boundary as roots. Provider-observed children use `AgentEvent` summaries without acquiring managed execution authority. `Started` and `Updated` carry an `AgentActiveState`; `Finished` carries an `AgentOutcome`, so a finished observation cannot carry a running state. A summary alone does not certify child resource collection or invent child transcript items.

A managed child's native outcome also controls automatic continuation. A collected stop that requires authorization ends the current task without starting queued follow-ups or permission-timeout feedback turns. The reusable `Agent` retains `AgentContinuationPolicy::RequiresAuthorization` pointing to that original task; its native result and any host observation failure remain in the original exchange. Explicit user admission, or an accepted original user input that carried that stopped child report, can acknowledge the exact stopped task. Report acknowledgement atomically marks the captured task reported and clears only its still-matching policy; it preserves the original failure and rejects a newer stop. Admission compares the captured authorization with the latest persisted policy inside the same transaction that admits the next task, so a stale reservation cannot clear a newer stop. Live admission rejects a policy that names a missing task or a task owned by another session or agent. Startup instead restores through `AgentCoordinator::restore`, which loads every task before it validates any policy, because an agent's policy can name a task listed after that agent's earlier tasks. Restore resets a policy whose task is missing or foreign to `Ready`, persists the repaired agent, and continues startup; the policy is derived continuation state, and a stale one previously made the session unopenable.

`ObservedAgent` persists each native child under the composite original `BindingId` and public `AgentId`. Its public agent identity can differ from the positively learned `ThreadOwner` identity, which Harness resolves from the child thread the adapter derived with `ThreadId::child`. The bridge supplies a registry-created `ObservedAgentAssociation`; neither storage nor the broker constructs a child owner from an ID or selects the newest turn. `observed_agent_record` stores validated native ancestry, typed host observations, and independent `Exchange::observed` histories without any managed `AgentTask` row. Those exchanges retain their own received `CompletedTurn`; a root or child lifecycle summary cannot supply one.

For positively observed native processing, the adapter retains a separate child driver with its original reader and callback collection. Parent-turn completion does not wait for that independent child or close its callback authority. A successor root turn can start while late child content continues into the original child history. Only that child's own native terminal evidence and collection can publish its completion.

`ObservedAgentAvailability` records loss of host observation separately from the last native lifecycle. Binding failure or exclusive startup recovery marks observation unavailable and freezes the host display clock at its last native publication boundary. It does not invent a native interruption, completion timestamp, or cleanup certificate. The binding fence also prevents late host-only effects from recreating an available record for a retired generation. Native child history and nested ancestry render read-only, without managed send, follow-up, or interrupt controls.

Each child has its own checked `AgentId` and, when resolved, its own `ThreadId`. `AgentDescription` may receive parent, delegation, and thread metadata independently. A delegation names the existing parent thread, turn, and optional item; missing metadata never permits attaching late child output to the parent's current turn. `ConversationResolved` associates the child with its original conversation, and host ownership checks reject conflicting relationships.

## Tools

The portable `subagent_tools` setting is off by default, and `/agents on` and `/agents off` change it. Enabling it exposes `agent_spawn`, `agent_send`, `agent_followup`, `agent_interrupt`, `agent_list`, `agent_activity`, and `agent_tool_output`. There is no wait tool. When a root turn ends with unfinished children, the exchange stays open, and Harness starts one continuation with their reports once they settle. New user input resumes the exchange immediately. The waiting root parks its deferred native result, as it does while child quiescence holds launches; only a coordinator revision (child activity or quiescence release), user input, or collection re-examines it. Before parking, the reducer re-read that result on every loop iteration, which spun the host while children or quiescence persisted. A stopped child no longer marks the parent exchange interrupted; only the parent's own stopped turn does. A stopped child that requires authorization suppresses that automatic continuation but not the wait: the exchange still stays open until its running siblings settle, because finalization captures a checkpoint that requires a settled descendant tree. The unreported sibling reports then travel with the next user input.

`agent_activity` returns at most 20 recent entries, five by default, without tool output. `agent_tool_output` reads at most 32,000 characters per call through references that validate ownership.

## Profiles

`AgentProfileStore` owns the global `stdpath("config")/forge/subagents.json`. A profile holds a purpose, instructions, enabled state, provider, model, thinking level, speed, and permission mode. `inherit` copies the root's choice at admission, and `default` selects the provider default. Saves are revision-checked atomic replacements.

Each child persists its resolved profile, so editing a profile never changes existing children. A child's effective permissions are the stricter of its snapshot and the current root.

## Interruption

Child approvals cancel only that child's requests on interruption, and the `approval_cancelled` events name exactly the requests that cancellation removed rather than an earlier snapshot of the pending list. Interrupting several children flags every target before any cleanup retry runs, then retries each retained process cleanup independently and reports every failure together; stopping at the first failed retry left later children editing the workspace under a cancel that had already reported failure. `/fork` rejects a root whose children still execute before it creates the forked session, because the native fork refuses an active thread and the copy would otherwise inherit live child rows that no runtime settles. Root cancellation and access changes collect child execution first, with a 30-second cleanup bound. On reopen, unfinished child tasks become interrupted and never replay.

## Session inspection

The root interaction exposes `harness_config` and seven `harness_session_*` tools through `tools/session/` and `tools/config.rs`, and the `harness-sessions` skill documents them. The portable `session_tools` setting is on by default, and turning it off removes the history tools but keeps `harness_config`.

Inspection uses read-only SQLite connections without migrations or leases. Reading another session requires authorization against its workspace, and that session's saved permissions never authorize the caller. Results go to a private temporary snapshot with opaque continuations, and page budgets bound each delivery, not the total size. Log inspection exposes only allowlisted diagnostic metadata, never prompts, credentials, or private reasoning.

The log reader captures each retained segment's byte length and keeps its open file
handle. It reads backwards from the current file into older rotations. Each request
decodes at most `limit` entries, defaulting to 1,000; continuations consume the next
older batch without scanning the full log first. Each log batch is returned complete,
including large Unicode records, independently of other inspection tools' byte budget.
Completed batches retain their boundaries so replaying a cursor returns the same page.
Concurrent appends stay outside the captured extent, and rotation renames files so
readers retain their original segments. Cancellation is checked between records;
live settings and read authority are rechecked before publication. Truncation or a
decode failure invalidates the cursor and fails explicitly instead of skipping entries.
