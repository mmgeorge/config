# Harness sessions and tasks

An agent session that loses a prompt, replays a write after a crash, or lets the editor guess its lifecycle state cannot be trusted with a repository. Harness therefore keeps sessions, exchanges, tasks, goals, and recovery state in Rust. Lua presents that state and captures explicit user intent, and it never decides a lifecycle transition itself.

This chapter covers the session lifecycle, from process ownership through input delivery, cancellation, and recovery. Read the related chapters for neighboring concerns:

| Concern | Chapter |
| --- | --- |
| Approvals, access scopes, and shell analysis | [Permissions](permissions.md) |
| Delegated child conversations and session inspection tools | [Child agents](child-agents.md) |
| Transcript rendering, status, and metrics | [Timeline and presentation](timeline.md) |
| Provider transports and capabilities | [Providers](providers.md) |
| Plans, review, and checks | [Planning](planning.md), [Plan review](plan-review.md), and [Project checks](project-checks.md) |

## Terms

A **session** is one durable conversation with its settings and history. An **exchange** covers one admitted request from admission to resolution. A **turn** is one provider invocation inside an exchange, so an exchange can contain several turns. A **task** is a Plan, Execute, or Goal workflow within a session, and a session selects one current task through `current_task_id`.

## Process and session ownership

The broker runs once per Neovim process, inside the shared Forge host. `client.lua` owns the JSONL connection to that host. Every Harness request and event carries a session ID, so sessions run concurrently and there is no globally active session. On the Lua side, `session.lua` keeps each session's transcript buffer, composer, windows, and subscriptions in `harness_by_id`.

Each session has a `SessionCoordinator`. Provider work needs its `ExecutionPermit`, while control commands and input admission do not, so one session never waits behind another's provider work and a busy session can still accept input. The permit lasts from request preparation through response delivery. Cleanup takes a separate control permit that blocks new execution until cleanup finishes. The host reserves four of its 64 request slots for control methods such as health, status, cancellation, and approvals, so a saturated host can still be cancelled.

`/new`, `/fork`, and `:ForgeHarnessNew` open their tab before provider work finishes. A fork first persists a child with `provider_fork_state = preparing`, the source's last completed provider turn, and a snapshot of the source's visible history. Provider preparation then runs outside the parent's lock, so a slow fork cannot stall the parent. A prompt sent early waits only on that child. The child inherits history for display only, without tasks, questions, or live agents.

## Commands and operations

A mutation that times out leaves the client unsure whether it ran, so retrying it blindly can apply it twice. `forge.command` prevents this by giving each mutation an immutable identity bound to the session, host epoch, method, and arguments. Rust records the response before delivering it, so a duplicate request returns the recorded response, and a client that lost an acknowledgement can ask `session.command.status.get`.

`session.command.apply` persists an operation ID before dispatch, and a compare-and-set claim lets each intent run once. `session.command.operation.get` can report an uncertain outcome but never replays it, and a restarted host never authorizes replay. A request rejected locally carries `not_admitted`, so the UI never reports an unknown outcome for a request that never left Neovim.

Settings, permission, model, resource, and compaction changes go through one coordinator in admission order. It stops conflicting execution, collects the provider attempt, finishes checkpoint finalization, applies the latest intent, and resumes the same exchange if its task is still active. A later pause prevents an earlier change from restarting work. Permission toggles resolve against the preceding pending selection, so rapid commands compose.

`forge.replica` keeps confirmed state, optimistic intents, and editor drafts apart. A reply removes only its matching intent and rebuilds the view over the newest confirmed state. Snapshots are scoped to their conversation, host generation, runtime epoch, and revision, so an older snapshot cannot replace newer state. A revision gap keeps the previous view until replacement state arrives. Failed state reads stay visible and retry after one, two, and four seconds.

## Durable storage

SQLite runs in WAL mode with FULL synchronization at `stdpath("data")/forge/harness/harness.sqlite3`. File content lives in a SHA-256 object store, and plans keep canonical JSON and Markdown under `plans/`. Mutating transactions take write admission before reading their baselines. If the five-second busy timeout runs out, the operation fails visibly and provider work is not replayed. The Harness and settings databases each keep a version in `PRAGMA user_version`, and a newer unsupported schema is left untouched.

Each live session owner holds an operating-system file lock for the controller's lifetime. The operating system releases the lock when the process exits, so recovery needs no heartbeat. Owner-checked saves stop a stale broker from overwriting its replacement. A lock collision returns structured lease metadata, and Neovim then offers Retry and Start New Session, plus Fork Session when the backend supports it. Deleting a session takes the lock and commits an intent before filesystem cleanup, and interrupted cleanup resumes at startup.

Sessions and preferences load only at the exact current format, defined by `SESSION_FORMAT_VERSION` and `PREFERENCE_FORMAT_VERSION` in `storage/mod.rs`. Older records stay stored but hidden, with no migration. Tool output is stored as ordered `tool_output_chunk` rows keyed by exchange, turn, call, and byte offset. Each chunk commits before it is published, so a crash cannot show output that was never saved. Observer connections are read-only, so renames, exchange comments, declaration reads, and rollback previews work during a provider turn.

## Input delivery

Harness file completion leaves candidate filtering and insertion with Blink.
`completion/file_rank.lua` creates a query-scoped comparator for `@` searches in
the composer, ranking full workspace-relative paths before Blink's generic score.
It caches each candidate's rank for that sorting pass. Accepted file selections
are recorded per workspace for the editor lifetime. Shortened labels remain a
separate presentation concern, and other buffers retain Blink's default sorting.

Accepted input must survive a closed window or a crash without ever being sent twice. `SessionCoordinator` owns accepted messages through `SessionInputQueue`, and the `session.input.*` methods work without the provider execution lock. Ctrl-S sends input that steers an active turn as soon as the provider allows, and Ctrl-Q queues an explicit follow-up. Each group stays in FIFO order. Steering never bypasses uncertain delivery, pending configuration, or a pause.

Ctrl-Q retains `/model` arguments as configuration on the queued entry, including
configuration-only entries with empty prompt text. Lua reconstructs the command
for queue display and editing. Rust applies the configuration only when that entry
is eligible for delivery and consumes an empty prompt without starting a turn.
Ctrl-S settings-only model commands use the immediate configuration coordinator.

The queue commits accepted state and idempotency receipts to its SQLite journal before acknowledging a mutation. Recovery releases abandoned edit reservations and restores retained input suspended. It reconciles linked submission evidence: accepted input leaves the queue, uncertain sends stay blocked, and proven unsent work requires explicit resumption or retry. An empty recovered queue preserves readiness instead of creating a pause.

Provider acceptance and queue acknowledgement are separate facts. `SubmissionStore` persists a `Prepared → Sending → Accepted` journal, and restart recovery never treats `Sending` as permission to replay. A turn that ends before dispatch keeps the message for the next eligible interaction. A failed message stays visible and blocks later follow-ups until the user retries or removes it. `SessionAdmission` keeps a compact task, operation, question, and exchange projection in memory, republished after every committed mutation, so submission never reopens SQLite.

Lua owns the unsent draft, and typing sends nothing to Rust. Ctrl-S captures the draft and sends one `session.prompt.submit` with its text, typed attachments, and a monotonically increasing token. Acceptance clears only the captured draft if it is unchanged, rejection keeps it, and newer typing always survives. Rust publishes a durable running exchange before provider preparation. Editing a queued prompt reserves its entry, so the queue drains only up to that entry.

Pasted file paths import through `session.attachment.import` into the object store. The composer keeps attachments separate from the text, so text never carries a hidden attachment URL.

## Exchanges and cancellation

`ExchangeNode` preserves the order of turn content, delegations, clarifications, steering, plan resolution, and artifact changes. The foreground provider invocation runs as a task in the broker's `JoinSet`, and every exit aborts and joins that task before transport cleanup. A native command can have a longer lifetime than the foreground invocation that started it.

The adapter marks a known running command with `ToolUpdate::Backgrounded` before reporting normal foreground completion. `ToolStore` persists that lifetime on the original invocation and keeps the command running until its native completion arrives. Unmarked unfinished tools still settle as interrupted when the foreground turn ends. A turn ending in cancellation, interruption, or failure settles its running tools even when they were marked as background work, and a later completion cannot reopen a terminal result.

`Backend::session_event_stream` carries observations that outlive a foreground turn, including background-command output and provider-initiated compaction. Its sinks retain the original execution owner and invocation address. The session collector queues events without waiting for the broker lock, so foreground work cannot block intake; the active broker drains invocation events before session events so lifetime admission precedes late completion. Delivery resolves the original stored tool and permits completion only while that exact background execution is still running. Output chunks commit before publication, historical updates preserve the current foreground view, and a failed subscription is reported rather than treated as an empty stream.

Managed children consume that same session-event contract alongside their invocation stream. `AgentRuntime` continues forwarding observations during provider identity lookup and shutdown, then drains the final events before reporting child completion. Every forwarded invocation must belong to the current child and task; a foreign observation fails the task without bypassing cleanup. This ordering prevents a stopped child command from remaining marked as running after its child has completed.

`session.turn.cancel` bypasses the serialized request queue so Ctrl-C can interrupt an active turn. A newly submitted prompt stays retractable until the provider produces a message, tool activity, task update, or workspace change. Retraction uses Codex `thread/rollback` or Copilot `session.history.truncate`, then the broker deletes the provisional interaction and Lua restores the exact prompt. After visible activity, cancellation persists the partial exchange as cancelled.

Typed provider cancellation enters the same settlement path for root and managed-child requests. It stops retries and cancels unsent child submissions while retaining evidence for uncertain or accepted sends. Provider error text does not determine cancellation, and a child cleanup failure remains a failure that requires collection.

Changing mode interrupts the provider and pauses the exchange, which keeps its request, plan, goal, and initial checkpoint. A cleanup or checkpoint failure leaves the exchange finalizing and blocks new admission, and Ctrl-C, new prompts, and goal resume retry finalization first. The terminal checkpoint and the exchange outcome commit together. When the host exits, Lua shows a reconnect error over the retained transcript, and only reopening Harness reconnects.

## Tasks and recovery

`TaskRecord` holds a task's saved access and approval, phase, status, generation, and attempt, and the task owns its plan through `plan_id`. Read requests never settle tasks or clear waits. Interrupting keeps the selected task, and the next ordinary message resumes a paused task with that message as added instructions.

Workflow changes never enable writing or widen filesystem scope. When a permission change resumes work, Harness tells the model the previous and current approval modes and asks it to inspect interrupted operations before retrying.

Restart recovery keeps committed phases and completion evidence, suspends unfinished attempts, marks unsettled operations outcome-unknown, and never starts a provider automatically. Pause operations recover as cancelled. Failure, disconnect, and unknown outcomes stop automatic queue draining, so queued work cannot run on top of a state nobody has confirmed.

After exclusive runtime ownership is acquired, recovery also settles detached background tools inside already-completed exchanges. It preserves the foreground outcome, output, and last observed duration. On a fresh Copilot binding, native history is checked for unmatched tool calls; only a verified incomplete suffix can be truncated, and the retained prefix is checked again before new input is sent. The repaired session's provider prompt cautions that prior actions may have completed and must be inspected before repeating writes. Successful recovery does not emit an editor warning; ambiguous history blocks admission rather than discarding completed work.

## Plan generation

The broker owns plan generation as one bounded continuation loop for every backend. Each turn applies plan edits to the canonical document, and only three outcomes move forward: a submitted plan, a new unresolved question, or another bounded turn. `ContinuationBudget` provides the budget for both goals and planning, and `PlanGeneration` stops after a turn limit or after consecutive turns without document progress. `/plan retry` resets only that budget.

`PlanStateMachine` validates plan transitions. `PlanQuestionLedger` records answers, skips, and withdrawals by both provider ID and content digest, so a resolved decision cannot reopen when a provider retries with a different ID. Automatic retries and goal continuations admit new exchanges without synthetic user prompts, so each attempt keeps its own metrics.

## Context and compaction

Provider usage normalizes into durable `ContextUsage`. `AutoCompaction` is a per-session policy with an enabled flag and a token threshold. It is off for ordinary sessions, and Background sessions enable it at 128,000 tokens. It runs after completed provider work and never estimates tokens from bytes.

`Compaction` is a separate lifecycle with an operation identity, observed start and completion, and an outcome. Its after-compaction token count is accepted only once the provider confirms completion, and a turn that ends first records an unconfirmed outcome. `native_compact` gates manual `/compact`, which uses Codex `thread/compact/start` or Copilot `session.history.compact` and creates no user exchange. Copilot keeps a `Backend::session_event_stream` subscription across turns because it can start compaction on its own.

## Rollback

`/undo` offers conversation-only rollback, or rollback with file restoration when a checkpoint exists. Rollback collects execution, pauses the task, replaces the provider conversation, and restores the reverted prompt to the composer. Rolled-back exchanges leave the timeline but keep their records. Sessions outside Git cannot be restored, so enabling writes there requires confirmation.

## Out of scope

Harness adds providers only through the typed `Backend` contract and never scrapes terminals. The broker runs per Neovim process, so controlling a session from another editor would need a shared daemon with stronger lease coordination. Provider-local hooks and command policies can still reject a tool under Harness Allow, because backend-native policy stays authoritative outside the Harness protocol.
