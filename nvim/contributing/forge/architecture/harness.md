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

A **session** is one durable conversation with its settings and history. An **exchange** covers one admitted request from admission to resolution. A **turn** is one host-admitted provider response cycle inside an exchange, identified before native acknowledgement, so an exchange can contain several turns. A **task** is the durable owner of a Plan, Execute, or Goal workflow in the session's `active_task` slot. Its `TaskId` survives Plan becoming Execute, while `TaskRevision` changes whenever control admission changes.

The exchange owner (`ExchangeDriver`) uses the following terms, each with one meaning. Code names and trace event names follow them, while stable failure codes such as `execution_collection_timeout` keep their original spellings, because the editor matches on them.

| Term | Meaning | Code |
| --- | --- | --- |
| Active turn | The turn that currently owns provider execution. | `ActiveTurn` |
| Queued turn | An admitted turn waiting for the active turn to finish. | `QueuedTurn` |
| Dispatch | Sending a turn's input to the provider. It ends when the adapter confirms the send, or is lost when the send ends without confirmation. | `TurnDispatch`, `DispatchStage` |
| Stop | Interrupting work and waiting until its native and host work has finished. | `ExchangeStop`, `StopIntent`, `running_stops` |
| Idle | No turn or stop is running. | `TurnStatus::Idle` |
| Restart | Replacing the provider session and initializing it again. | `ObservationRecovery` |
| Execution state | Whether the owner may start provider execution: ready, stopping, restarting, or failed. | `ExecutionState` |
| Delivery uncertain | The journal records `Sending`, so the provider may hold the input and it is never sent again. | `SubmissionState::Sending` |

`exchange/owner.rs` holds only the handle that callers use: `ExchangeOwner`, `QuestionEvents`, and the `OwnerEvent` messages they send. `exchange/driver.rs` holds `ExchangeDriver`, whose `run` loop is a fixed sequence of named phases in `exchange/driver/`: intake (`intake.rs`), publication, storage, and delivery bookkeeping (`step.rs`), the effect phase that starts admitted work (`step.rs`, `check_admission.rs`), and wake selection and application (`wake.rs`, with one handler file per wake family under `handlers/`). Every phase reads its decisions from `Admission::evaluate` and changes the execution state only through `ExecutionState`'s transition methods (`execution_state.rs`), so no phase keeps its own copy of a rule or pairs the execution fence by hand. `RunningTasks::is_idle` is the only definition of "no background job is running", which restarts, shutdown, and approval timeouts all wait on, and `ExchangeDriver::turn_status` is the only computation of the turn status that stops and steering read.

### Publication outbox

`SessionPublications` (`exchange/publication.rs`) commits every sequenced owner notification to `owner_publication` before any output, and offers rows strictly in sequence order, so a stale row offered out of turn would corrupt the editor's view of the epoch. Four rules keep the outbox from stalling, leaking, or pacing a stream by the disk:

- **Document wakes.** `document_changed` only tells the editor that a newer transcript revision exists, so it never enters `owner_publication` and carries no `_publication` sequence. `SessionPublications` keeps the newest wake in memory, overlaying each wake on the one it supersedes so a status field from the older wake survives. Delivery sends the wake once no unadmitted row and no reconnect response precedes it, and a direct sequenced send flushes it first. A wake lost with the process or a closed transport heals through the reconnect snapshot's `timeline_revision`. An in-memory flag records whether an unadmitted row can exist, so delivery opens no transaction while the outbox is drained.

- **Owner start.** `recover_interrupted_offers` reclassifies every unadmitted row from the earlier epoch, `pending` as well as `offering`, to `uncertain`. Live delivery never sends an uncertain row, so the previous epoch's timeline revisions cannot raise the editor's floor; the first reconnect page carries those rows and admits them.
- **Direct offers.** A direct send commits the row, hands it to the transport, and queues the `admitted` update as a write-behind command, so the send waits on one commit instead of three and writes no intermediate `offering` state. A crash before the queued update commits leaves the row `pending`, which the next owner start reclassifies as `uncertain` and the reconnect page then reconciles. An update that fails fences the session scope: `SessionPublications` refuses every later publication with `Unavailable` rather than send behind a row whose state it could not record.
- **Pruning.** Owner start and each confirmed reconnect page delete admitted history that no reader still needs. They keep every unadmitted row and everything after the first one, the highest sequence (sequence allocation and cursor validation read the maximum), rows after the confirmed page, and rows whose origin is a durable producer identity, which a retried producer finds instead of publishing again. A cursorless reconnect page starts after the admitted prefix, because a newly attached view adopts current state without replaying history; the response still echoes the caller's absent cursor as `after`.

## Process and session ownership

The broker runs once per Neovim process, inside the shared Forge host. `client.lua` owns the JSONL connection to that host. Every Harness request and event carries a session ID, so sessions run concurrently and there is no globally active session. On the Lua side, `session.lua` keeps each session's transcript buffer, composer, windows, and subscriptions in `harness_by_id`.

Each session has a `SessionCoordinator`. Provider work needs its `ExecutionPermit`, while control commands and input admission do not, so one session never waits behind another's provider work and a busy session can still accept input. The permit lasts from request preparation through execution finalization and is released before the final response or input update publishes readiness. A delayed input snapshot must not leave the editor busy after native work has settled. The exchange owner serializes mutation commands and retains their asynchronous jobs, so dropping a request waiter cannot abandon cleanup or publish a later result out of order. Transport admission reserves capacity for control requests during saturation.

Controller construction resolves inherited model tuning against the provider's workspace catalog before publishing the controller or its initial snapshot. An unavailable model fails construction. Unavailable saved reasoning, context-window, and service-tier choices fall back to advertised defaults; absence of a context-window override stays absent. This applies equally to initial opening, new conversations, and resumed controllers, so an early prompt cannot race native initialization with unchecked preferences. Explicit configuration commands continue to reject unsupported choices before committing them.

`/new`, `/fork`, and `:ForgeHarnessNew` open their tab before provider work finishes. A fork first persists a child with `provider_fork_state = preparing`, the source's last completed provider turn, and a snapshot of the source's visible history. Provider preparation then runs outside the parent's lock, so a slow fork cannot stall the parent. A prompt sent early waits only on that child's fork gate. The host registers the gate before it opens the child and starts the fork task before it sends the response or `session_created`, so a failed delivery cannot leave the gate pending; the task holds its outcome event until that announcement finishes, which keeps the outcome after it. A `preparing` child with no gate belongs to a fork that a previous host quit or that failed before its task started, so resolving it records the fork as failed. The child inherits history for display only, without tasks, questions, or live agents.

A provider copies a live conversation only while it is idle, so a fork of a session whose turn is running cannot copy that conversation: the native fork fails with `ExecutionActive`, and a child that had already copied the source's timeline would show the unfinished exchange frozen forever. `prepare_provider_fork` therefore selects a `ForkBoundary` before it writes anything, from `ForkOptions::source_is_executing`, which `start_session_fork` answers from the source's execution permit. It asks while it holds the history writer: a turn that starts later cannot commit its exchange until the writer returns, so the boundary and the history the fork copies describe the same moment. `start_session_fork` also reads the permit before preparation and keeps an executing answer, because a turn that ends while the fork waits for the writer may not have saved its finished exchange. `Current`, for an idle source, copies the whole conversation through the source's binding. `Settled`, for an executing source, stops at the last finished exchange: the child's timeline omits the unfinished exchange, its `forked` event records `unfinished_exchange_omitted`, and the native fork reads the provider checkpoint that exchange recorded, which stays readable while the source runs. A source that neither ran nor inherited a provider turn has no native history at either boundary, because a provider writes a conversation when its first turn runs: the Claude CLI and the Codex app-server both reject a native fork of a session that ran none. Its child therefore starts `ready` on a fresh provider conversation with no provider work, whether the source is idle before its first prompt or executing its first turn. `prepare_provider_fork` reads inherited turns from the source's own inherited timeline entries, since a fork's exchange list never shows the turns its native conversation copied. Two cases have no safe boundary and reject before any session exists: the last finished exchange recorded no checkpoint, or the source is a fork that inherited provider turns and is inside its first turn, whose inherited history lives only in a conversation that is busy.

The native fork runs after preparation returns the writer, so a source can still start a turn between a `Current` decision and the provider's copy. The fork task then retries once against the checkpoint recorded in the child's fork point, which names the same history the child inherited. The fork marker's text puts an omitted exchange first, "Forked before the unfinished exchange of", because a session ID and name fill most of a transcript row and a wrapped event folds every row after its first. A fork that still fails leaves the child `failed`, and that state is not terminal: `SessionControllerRegistry::await_provider_fork` starts a new native attempt, under a new operation identity, for the next provider-bound request that finds the child `failed`, and concurrent requests share that attempt through the fork gate. A request that waited on an attempt reports that attempt's failure and leaves the retry to the next request.

A native fork of an idle source whose binding is not open yet opens that binding only as a fork source, and no owner reads its event stream until the source runs a turn. `ProviderBridge::retire_owner` drops a stream that no owner took before it waits for the stream's published events to be consumed; otherwise closing that session, and every close queued behind it, would wait forever.

### Aside sessions

`/btw <question>` answers a question about a session without interrupting it. It sends `session.fork` with `aside: true`, which creates an ordinary fork whose `HarnessSession::aside` names the origin session; a fork of an aside keeps the same origin. The marker changes three things for the aside's primary agent, and nothing else about the session. `ProviderRequest::standing_sections` adds an `<aside>` section that names the origin and states the aside's role, delivered once per binding and again after compaction like every standing section, so the provider's cached system prompt is unchanged. `session_tools_enabled` keeps the `harness_session_*` tools on regardless of `/config`, because inspecting the origin is the aside's purpose, and `harness_session_exchange` reports an unfinished exchange's commentary so the aside can read a turn that is still running. The registry offers `origin_message`, the aside's only write to its origin.

`origin_message` delivers through `SessionMessageRouter`, which `BrokerRuntime` owns and `SessionControllerRegistry` binds, because a broker owns one session and cannot reach another session's controller. `SessionControllerRegistry::deliver_session_message` resolves the origin, waits for its own fork, attaches its publishers to the editor connection retained from the latest request, and submits the message as a `TurnKind::SessionMessage` that names the sending aside. The queue shows the turn's label, and the origin's model receives a `<session_message>` data element whose fixed instructions let it act on the message only where it agrees with the origin user's requests, so an aside's words never arrive with the origin user's authority. `CommandRequest::from_host_input` attaches the typed turn after the editor's own decoding, because `CommandRequest::from_input` never accepts one from an editor request. `queue` delivery, the default, waits behind the origin's running turn and queued input and never resumes a queue the user suspended; `steer` delivery carries the queue's control revision and enters the running turn as the user's own steering would. The registry releases the retained connection once shutdown has quiesced every request, so the outbound channel still closes with the editor.

## Commands and operations

A mutation that times out leaves the client unsure whether it ran, so retrying it blindly can apply it twice. `forge.command` prevents this by giving each mutation an immutable identity bound to the session, host epoch, method, and arguments. Rust records the response before delivering it, so a duplicate request returns the recorded response, and a client that lost an acknowledgement can ask `session.command.status.get`.

`session.command.apply` persists an operation ID before dispatch, and a compare-and-set claim lets each intent run once. `session.command.operation.get` can report an uncertain outcome but never replays it, and a restarted host never authorizes replay. A request rejected locally carries `not_admitted`, so the UI never reports an unknown outcome for a request that never left Neovim.

Settings, permission, model, resource, compaction, and cancellation commands enter the exchange owner's FIFO in admission order. It stops conflicting execution, collects the provider attempt, finishes checkpoint finalization, applies the latest intent, and resumes the same exchange if its task is still active. A later pause prevents an earlier change from restarting work. Permission toggles resolve against the preceding pending selection, so rapid commands compose. The interrupted attempt of a configuration restart is not a model turn: it never records goal or execution evidence, so restarts neither spend the total-turn budget nor advance the no-progress count, and only the replayed turn that completes normally counts.

`Admission::evaluate` (`exchange/driver/admission.rs`) holds every rule for what the driver may start: commands, turns, turn effects, question and check tools, automatic and task checks, task delivery, deferred configuration retries, restarts, and shutdown. It is a pure function of an `AdmissionFacts` snapshot that each phase gathers fresh, so no phase applies its own copy of a rule, and a table test pins the dispatch race cases. Its `start_command` rule decides when the owner starts its next queued command, and the effect phase and the step-restart gate both read it. No command starts while a provider restart or a stop runs, while the session's observation stream is closed, or while the current turn's send was lost (`DispatchStage::Lost`). A failed observation stream reports its failure to every later reader until a restart replaces it, so a command started then would fail on evidence that belongs to the failed turn. Commands other than cancellation also wait while the turn's send is unsettled, from host admission until the adapter confirms the send (`TurnTransaction::dispatch_stage`). Stopping a turn whose send is in flight leaves its delivery unknown. Stopping it before the journal records `Sending` prevents the send, after which both the restarted exchange and the input queue would deliver the prompt again. A model change made at the start of a turn therefore applies once the provider holds the prompt, then restarts the turn on the new model. Cancellation stays exempt from that wait, because it prevents an unsent prompt and restores the draft, or waits inside the provider runtime for an in-flight send before stopping the turn, and it never restarts the exchange. Cancellation that arrives after a failing turn has closed its engine pins no invocation.

`forge.replica` keeps confirmed state, optimistic intents, and editor drafts apart. A reply removes only its matching intent and rebuilds the view over the newest confirmed state. Snapshots are scoped to their conversation, host generation, runtime epoch, and revision, so an older snapshot cannot replace newer state. A revision gap keeps the previous view until replacement state arrives. Failed state reads stay visible and retry after one, two, and four seconds (`controller.state_sync_retry_delays_ms`); a later successful read clears the failure and resets the sequence. A view keeps one `session.state.get` in flight per session and host generation, so an in-place session switch issues its own read, and a reply for a session the view no longer shows triggers a read of the current one. A `document_changed` patch does not discard an in-flight read, because the read's `timeline_revision` already orders it against the patch; a patch stamped with another runtime epoch or host generation is ignored.

## Tool execution deadlines

`ToolWatchdog` consumes typed item lifecycle observations from the ordered native stream and captures
each invocation's deadline once. The root broker and `AgentRuntime` stop provider
execution when the earliest unfinished tool expires and retain the timeout as a
failure. Output, title changes, and replayed start events cannot extend a deadline.

While a foreground attempt runs, `ForegroundWait` holds a `ForegroundToolOwner` on its
watchdog, and the foreground is the sole collector of a timed-out turn. The inbox tool
monitor only signals expiry and waits for that owner to release before it collects
background work; two collectors racing for one native turn previously let either win, so
the exchange settled nondeterministically as failed or stranded in finalization. Foreground
timeout cleanup stops only the timed-out exchange through `cancel_exchange`, as owner
cancellation does, instead of closing the engine. Closing the engine dropped the interrupted
turn's terminal event and recorded the turn as a disconnected failure rather than
`Interrupted`. Because scoped collection leaves the stopped invocation registered with its
execution owner, failure settlement commits that invocation's settlement ticket after every
failure checkpoint and publication drains; an uncommitted ticket would reject the next prompt
with "execution owner is unavailable or already active". A cleanly collected timeout then finishes as a
settled `tool_timeout` response, as collected cancellation finishes as `turn_cancelled`.
A deadline found elapsed only after a cancel or restart already began collecting the turn
does not convert it into a timeout: the collected outcome settles the exchange, so a cancelled
prompt is still restored, and the foreground claims the timeout report so the inbox monitor
cannot later publish it against the settled invocation.

`SessionObservationInbox` routes tool observations to watchdogs by owner and exchange, not by
invocation. A steering message, child result, or check repair continues the exchange under a
successor invocation, and its events reach the inbox before the foreground adopts that scope;
a registry keyed by invocation never delivered them, so a hung tool in any continuation had no
deadline. Registering a new foreground's watchdog retires the previous one, which keeps its
admitted background deadlines and then lets its monitor end instead of parking for the
session's lifetime.

A failure that leaves provider state unknown restarts the provider instead of stranding the
session. A turn that fails, a stop that cannot prove the provider idle, a collection task that
ends without its result, and an observation stream that closes each make the owner request a
restart on its own (`ExchangeDriver::restart_after_failure` and `restart_advance`), without
waiting for the next prompt. The owner holds every command and turn until the restart finishes.
Success reopens execution, lifts the collection fence, and traces `exchange.restart.finished`. Failure closes execution for the
session's lifetime and publishes a `provider_restart_failed` error, after which cancellation
fails immediately rather than waiting. A failed owner with no remaining work reports its turn
status as idle, so session shutdown still finishes; its observation listener then reports the
stream failure that no restart replaced. Owner storage failures stay terminal without a restart,
because a new provider generation cannot repair the database. Only failures sourced from SQLite,
file I/O, or the owner's journal count as storage failures (`exchange::failure::storage_failure`);
a domain rule that refuses a command, such as `/goal continue` on a paused task, answers that
command with a typed failure and leaves the session usable. A cancellation whose target pinning
fails for any other reason is answered as a failed stop and collected like one. Shutdown that
cancels a requested but unstarted restart fails execution instead of leaving the owner
`Restarting`, so its turn status reaches idle and shutdown finishes.

A stop's retained transition outlives the stop as evidence, so once its host jobs join, the driver
releases both its control permit and its child quiescence; a held quiescence parked every later
root turn at completion. Child collection treats a child whose cleanup failed as settled with an
error, which makes the stop uncertain and restarts the provider, because that child keeps its
execution slot and the active count never reaches zero. A cancellation settlement that fails, or
whose command fails or runs overdue, releases the host-settlement fence of the exchange it was
finalizing (`abort_prior_finalization`); the exchange stays finalizing with the recorded error for
the next admission to retry.

The `tool_timeout` setting defaults to ten minutes and applies to shell commands as
well as tool calls. `SessionEventInbox` retains watchdogs for confirmed background
commands after foreground completion and routes their later terminal events to the
original watchdog. `CheckSupervisor` applies the same setting to each subprocess,
collects the terminated process tree, and stops the run on timeout. The `/config`
timeout endpoint saves this policy without interrupting the current provider turn.

## Durable storage

Harness separates persistent settings from disposable execution state beneath `stdpath("data")/forge/harness/`. `storage::state::VERSION` selects `state/vN/` for history, submission journals, captured objects, plans, checkpoints, traces, check artifacts, and private provider runtimes. An incompatible contract increments that version; new hosts start with fresh state without opening or migrating older sessions.

`settings.db`, `preferences.sqlite3`, permissions, and trace configuration remain outside `state/`. The preference store imports compatible workspace preferences and model pins once from the former unversioned history database; it never imports sessions. A record whose payload no longer decodes into the current preference schema, or whose format version is unsupported, stays behind, so one obsolete field such as the removed `windows_sandbox` access flag cannot block the store from opening. Imports commit atomically, and SQLite failures abort the import without marking migration complete. Subsequent opens preserve the independent preference store rather than recopying old settings. The former unversioned database and files remain untouched.

`storage::state` owns version cleanup. State stores retain shared lifetime leases, with stable lock files outside the deletable directories. `:ForgeHistoryClear` stops and collects its editor's host, fences new requests, and invokes `forge history-clear --data-root <directory>`. Cleanup tries every version, requiring exclusive version ownership and preflighting its SQLite databases before deleting dependent files. It preserves locked versions, reports each failure, clears unlocked siblings, and removes `state/` when empty. Clearing the current version forgets local session identities and transcripts even if an older version remains locked; unsent composer text survives. Filesystem deletion errors can leave a partially removed version and are reported.

History and the independent provider-submission journal retain SQLite WAL mode with FULL synchronization. A configuration transaction captures its preference update with the session's recovery intent; completion applies that preference in the persistent store before acknowledging the intent. A persistent receipt makes replay idempotent if the process exits between those commits. Mutating transactions take write admission before reading baselines, while history readers can retain snapshots without blocking commits. If the five-second busy timeout expires, the operation fails visibly without replaying provider work.

`HistoryWriter` (`storage/writer.rs`) owns one named thread per state root and commits on behalf of its callers, because a `FULL` commit waits on the disk and a caller that commits inline paces every task behind it by that disk. The thread drains its queue, runs each queued command in its own savepoint inside one transaction, and commits once, so a burst of writes shares one sync. An awaited command returns after its batch commits and receives its own error. A write-behind command returns a ticket at once. Streamed message, reasoning, and tool-output text is write-behind: the broker shows the text first and queues the save, and a newer whole-exchange save replaces one still waiting at the tail of the queue. A crash or a shutdown that outruns its deadline therefore loses the queued tail of streamed text; the saved text stays a prefix of what was shown. A write-behind command that fails fences its session scope: the writer skips that scope's later commands, the next exchange save on the broker's handle returns `WriteBehindFailure`, and the driver's per-step check (`history_failure_apply`) marks owner storage unavailable. A lock timeout at `BEGIN` fails the awaited commands of that batch and leaves the write-behind commands queued for the next attempt; at 256 queued write-behind commands `enqueue` waits for the writer, so nothing is dropped. The broker's own connection (`OrderedConnection`) waits for the work it queued before its next direct statement, which keeps its reads and writes ordered after the queued saves. The broker keeps that connection by design. A WAL reader on its own connection never waits for another session's commit, and the broker's wait covers only the saves its own session queued. A broker that borrowed the writer's store for each read would queue every session's reads behind every other session's disk sync, and would move the per-session lock and exchange cache that its store holds onto a store shared by the whole root.

Every other history owner borrows the writer's store instead of opening a handle. `HistoryWriter::lease` hands the store to the calling thread in queue order and the writer thread waits until the lease returns, so the holder's statements are ordered after everything queued before it; the command journal, the input journal, the question controller, and the publication outbox each take a lease per operation, and `HistoryWriter::lease_for` serves one-off writes. A lease stays on its thread and cannot cross an `await`, and a thread holds at most one, so code that runs another owner inside its own transaction (session deletion) keeps a connection of its own. `CommandJournal::append_core` uses `HistoryWriter::commit`, which joins the next batch and returns after that batch commits. `HistoryReader::current` (`storage/reader.rs`) waits for the write-behind queue and then lends a standing read-only handle from the root's pool, which closes with the last writer handle. Four direct opens remain, each marked with the reason at its `#[expect(clippy::disallowed_methods)]`: the broker's session-long store, Background preparation, session deletion, and history clearing.

The reducer step runs off the runtime workers. `ForegroundRuntime::apply_update`, `finish_foreground`, and the reconnect capture take the broker with `lock_owned` and run inside `spawn_blocking`, so the step's remaining synchronous commits and its projection work never hold a worker that the transport and other sessions queue behind. `off_worker` gives the same protection to a lease or an awaited commit taken on a multi-thread runtime worker.

Each live session owner holds an operating-system file lock for the controller's lifetime. The operating system releases the lock when the process exits, so recovery needs no heartbeat. Owner-checked saves stop a stale broker from overwriting its replacement. A lock collision returns structured lease metadata, and Neovim then offers Retry and Start New Session, plus Fork Session when the backend supports it. Deleting a session takes the lock and commits an intent before filesystem cleanup, and interrupted cleanup resumes at startup.

Sessions and preferences decode at the current format, defined by `SESSION_FORMAT_VERSION` and `PREFERENCE_FORMAT_VERSION` in `storage/mod.rs`; the one-time settings import converts compatible older preference envelopes. The version-directory boundary excludes incompatible history before domain decoding; record-version checks still reject malformed or mismatched session records in an otherwise current database. Tool output is stored as ordered `tool_output_chunk` rows keyed by exchange, turn, call, and byte offset. Streamed chunks are saved after they are shown: the writer appends each one only where it continues the saved byte offset, so saved output is always a prefix of shown output. Observer connections are read-only, so renames, exchange comments, declaration reads, and rollback previews work during a provider turn.

## Input delivery

Harness file completion leaves candidate filtering and insertion with Blink.
`completion/file_rank.lua` creates a query-scoped comparator for `@` searches in
the composer, ranking full workspace-relative paths before Blink's generic score.
It caches each candidate's rank for that sorting pass. Accepted file selections
are recorded per workspace for the editor lifetime. Shortened labels remain a
separate presentation concern, and other buffers retain Blink's default sorting.

Accepted input must survive a closed window or a crash without ever being sent twice. `SessionCoordinator` owns accepted messages through `SessionInputQueue`, and the `session.input.*` methods work without the provider execution lock. Ctrl-S sends input that steers an active turn as soon as the provider allows, and Ctrl-Q queues an explicit follow-up. Each group stays in FIFO order. Steering never bypasses uncertain delivery, pending configuration, or a pause.

An active `SteeringLane` accepts consecutive steering messages in FIFO order
without waiting for earlier provider acknowledgments. The input worker holds an
ordering permit through attachment preparation and queue handoff, then releases
it while retaining each message's independent delivery receipt. The number of
workers is bounded by the steering lane capacity. Adapters without a queue
handoff signal retain the permit until their steering call completes.
`SteeringFlight` distinguishes these live workers from abandoned `Sending`
records: overlapping live sends are permitted, but an unacknowledged send whose
worker has ended continues to block further dispatch and cannot be replayed.

Ctrl-Q retains `/model` arguments as configuration on the queued entry, including
configuration-only entries with empty prompt text. Lua reconstructs the command
for queue display and editing. Rust applies the configuration only when that entry
is eligible for delivery and consumes an empty prompt without starting a turn.
Ctrl-S settings-only model commands use the immediate configuration coordinator.

Lua refuses edits of pending submissions: their immutable command is already in
flight, but their local display entry cannot yet acquire a host reservation. Once
acknowledged, edits follow the host reservation path. A commit that removes an entry blocks reopening that
entry until its result settles, even while the confirmed queue still displays it.
The block clears on success or failure. Delivery can still win before
a reservation is granted. Submitting or navigating an edit whose target has left
the queue releases editor ownership and preserves nonempty composer text as a
normal draft without resending it. Transport events update queue state immediately;
their presentation work is coalesced into one scheduled render per event-loop burst.

The queue commits accepted state and idempotency receipts to its SQLite journal before acknowledging a mutation. Recovery releases abandoned edit reservations and restores retained input suspended. It reconciles linked submission evidence: accepted input leaves the queue, uncertain sends stay blocked, and proven unsent work requires explicit resumption or retry. An empty recovered queue preserves readiness instead of creating a pause. Lua reserves a queued message for editing at the revision its last committed edit published: a reservation requested while that commit awaits its host result waits for the result, because the displayed revision is stale until then and the host rejects it as `RevisionChanged`.

Provider acceptance and queue acknowledgement are separate facts. `SubmissionStore` persists a `Prepared → Sending → Accepted` journal, and restart recovery never treats `Sending` as permission to replay. A turn that ends before dispatch keeps the message for the next eligible interaction: settlement retires undispatched steering to `Cancelled`, and unless the user cancelled the turn the input worker returns that message to the queue rather than retracting it. A failed message stays visible and blocks later follow-ups until the user retries or removes it, including across a host restart, because recovery reads a `Failed` queue entry whose record settlement retired as a failure rather than a retraction. Uncertain (`Unknown`) delivery blocks the queue until reconciliation proves an outcome or the user removes the message; `session.input` removal then retires its record to the terminal `Discarded` state, which is never replayed and no longer blocks later dispatch. `SessionAdmission` keeps a compact task, operation, question, and exchange projection in memory, republished after every committed mutation, so submission never reopens SQLite.

Lua owns the unsent draft, and typing sends nothing to Rust. Ctrl-S captures the draft and sends one `session.prompt.submit` with its text, typed attachments, and a monotonically increasing token. `composer_draft.take` clears the captured draft at submission, for prompts and for task commands such as `/plan` and `/goal`, because `session.command.apply` replies only after the task starts its provider turn. A rejection restores the draft only into an untouched or empty composer, so newer typing always survives. Rust publishes a durable running exchange before provider preparation. Editing a queued prompt reserves its entry, so the queue drains only up to that entry.

Pasted file paths import through `session.attachment.import` into the object store. The composer keeps attachments separate from the text, so text never carries a hidden attachment URL.

## Exchanges and cancellation

`ExchangeNode` preserves the order of turn content, delegations, clarifications, steering, plan resolution, and artifact changes. Steering takes its place at acknowledgement; `ExchangeInput` retains the steering submission, and the input's native echo moves the row to the timeline tail, because a provider such as Copilot can consume steering only after its current model call ends. The shared session execution engine owns each admitted invocation and retains native submission and collection independently of foreground result waiters. The shared native runner retains terminal evidence and completes provider collection before the authoritative `Turn.Completed` event. Harness then applies its host commit ticket. An RPC acceptance, a message completion, or an interruption acknowledgement cannot replace that event. A native command can have a longer lifetime than the foreground invocation that started it.

The adapter marks a known running command with `ToolItemEvent::Backgrounded` before reporting normal foreground completion. `ToolStore` persists that lifetime on the original invocation and keeps the command running until its native completion arrives. Unmarked unfinished tools still settle as interrupted when the foreground turn ends. A turn ending in cancellation, interruption, or failure settles its running tools even when they were marked as background work, and a later completion cannot reopen a terminal result.

`thread_open` returns the binding's sole ordered observation stream atomically with its identity and effective capabilities. That stream also carries background-command output and provider-initiated compaction after foreground settlement. The binding registry resolves normalized thread, turn, and item identities to their original execution owner before forwarding them. The session collector queues events without waiting for the broker lock, so foreground work cannot block intake; the active broker drains invocation events before session events so lifetime admission precedes late completion. Delivery resolves the original stored tool and permits completion only while that exact background execution is still running. Output chunks are queued for the history writer when they are published, historical updates preserve the current foreground view, and a failed subscription is reported rather than treated as an empty stream.

Managed children consume that same session-event contract alongside their invocation stream. `AgentRuntime` consumes identity observations and open results from the same retained owner, continues forwarding during shutdown, and drains final events before reporting child completion. No active-session identity polling roundtrip runs alongside that owner. Every forwarded invocation must belong to the current child and task; a foreign observation fails the task without bypassing cleanup. This ordering prevents a stopped child command from remaining marked as running after its child has completed.

A correlated `Turn.Error` received after collection remains append-only diagnostic history on its original turn. Harness deduplicates that observation and persists it without reopening execution or replacing the immutable `CompletedTurn`, native outcome, or retry decision. A valid diagnostic alone does not fence a healthy binding.

A completed turn whose actual model remains unknown can accept its first positively correlated `ModelResolved` observation. That refinement belongs only to the original turn and survives reload; it does not confirm a successor's model or replace configured defaults. A completed turn with a known actual model rejects a conflicting late change.

The winbar displays `Session.resolved_model` when the provider has reported one and the configured model otherwise, because the user's selection is the model the provider runs. It names that model by its catalog label, and until the catalog loads it uses `Session.model_label`, the label recorded when the model was selected and kept in the provider preference's `ModelSetting.label`. An ACP catalog appears only after a binding opens, so without the saved label a new session would show the raw alias `opus` instead of "Opus 5.5". The label is presentation only: `HarnessSession::select_model` replaces it with every model change, and configuration with a catalog refreshes it because an alias can later name a newer model. A provider-reported model takes precedence only because it can name what an alias such as `default` selected, or a substitution the provider made. A started turn has no model evidence until its own `ModelResolved` arrives, so the broker keeps the previous resolved model when a turn starts and replaces it only with the new turn's resolved model; configuration that selects a different model clears it.

`session.turn.cancel` enters the same exchange command FIFO as configuration and other mutations. The owner signals the exact invocation while retaining asynchronous settlement and host finalization; it does not wait for the original result request to finish before processing cancellation. A newly submitted prompt stays retractable until the provider produces a message, reasoning, tool activity, task update, or workspace change. Reasoning counts as visible model work because the user can read it. The provider's echo of the submitted prompt does not count: Codex reports the prompt as a native user-message item before any assistant output, so `RetractionEligibility` records each user-message item identity at its start and ignores that item's later deltas and completion. Owner-collected cancellation and foreground failure settlement both decide restoration through `cancellation_restores_prompt`, which requires output-free history and the `RestoreIfUnconsumed` input policy; a clean cancellation that restores its prompt enters failure settlement rather than ordinary finalization, and settlement waits until the owner's collection completes before deleting the exchange. A collection that fails ends that wait as well: the stop's finish handler sends the turn `ForegroundSignal::CollectionFailed`, which `ForegroundAdmission::fail_owner_collection` records, and settlement then keeps the exchange without restoring the prompt, because nothing proved the provider left it unconsumed. Without that signal the failed turn waited forever for a `CollectionCompleted` that a failed collection never sends; the provider restart that the failed stop requested starts only once no foreground turn remains, so it never ran, and every later command, including each repeated cancel, queued behind it unanswered. An admission the owner was collecting ends the same way and collects its native owner itself. Retraction is available both before the checkpoint baseline and after a committed baseline; after the baseline, a retained retraction result continues to the ordinary failure checkpoint. After settlement, the owner authorizes `input_retract` for the exact cancelled submission, delivery attempt, and checked history boundary. Only proven nonconsumption or confirmed history retraction permits removing the provisional interaction and restoring the exact draft. An unknown retraction result retains the draft boundary for reconciliation and never authorizes blind rollback; tool effects are never undone by history retraction. A confirmed retraction answers the cancel command with `CancellationOutcome::Retracted`, answers the prompt request with `turn_retracted`, and publishes the input update with `retracted` set, including for a submission the provider had already accepted. After visible activity, cancellation persists the partial exchange as cancelled.

Collection also stops plan-execution progress scans. A scan is ordinary foreground work, and a collecting command keeps the effect gate closed until it ends, so a scan requested after collection began stayed staged while finalization waited for its result; Ctrl-C with a pending approval then waited for the 30-second collection deadline. `ForegroundTransaction::next` therefore yields no `PlanScan` while `collection` is set, and the step ignores a tick that was already selected. A plan question staged by a finalizing planning turn is settlement work, like a plan submission, so `ForegroundJob::is_collection` lets it run behind a closed effect gate instead of waiting out the command's deadline. When the reducer settles a turn while an observation or scan read during the settlement commit has started work, the driver drops effects that never started and joins the running ones, publishing any output they forward, before it reports the turn finished.

Failure finalization settles every turn still `Running` as the exchange outcome after native cleanup joins (`Exchange::settle_running_turns`), because a terminal event that cleanup dropped will not arrive later; without that step the failure checkpoint rejected the exchange and left it finalizing. A Codex interrupt registered before `turn/start` is acknowledged waits for the acknowledgment, then interrupts the acknowledged turn.

Typed provider cancellation enters the same collection path for root and managed-child requests. A native interruption remains `TurnOutcome::Interrupted` after a settings change; finishing host cleanup cannot turn it into normal completion. `Completed { stop_reason: Normal }` alone permits automatic continuation. Token limits, request limits, and refusals retain their exact stop reason and stop automatic retry or task/agent continuation until the user authorizes more work. It stops retries and cancels unsent child submissions while retaining evidence for uncertain or accepted sends. Provider error text does not determine cancellation, and a child cleanup failure remains a failure that requires collection.

Changing mode interrupts the provider and pauses the exchange, which keeps its request, plan, goal, and initial checkpoint. Task controls collect running work through the same parking. The next admission closes a parked exchange, so a control that leaves no current task, such as Clear, closes it before releasing control. Collection whose provider turn already completed instead leaves the exchange finalizing, with its final checkpoint deferred to that next admission. When the control leaves no current task, the task command therefore runs a follow-up cancellation settlement (`TaskStage::SettlingFinalization`) that captures the checkpoint and finishes the exchange before the control reports completion; otherwise the transcript showed **Saving exchange** until unrelated input arrived. A closed exchange that never started a provider turn is cancelled rather than reported as completed work. A cleanup or checkpoint failure leaves the exchange finalizing and blocks new admission, and Ctrl-C, new prompts, and goal resume retry finalization first. The terminal checkpoint and the exchange outcome commit together. When the host exits, Lua shows a reconnect error over the retained transcript, and only reopening Harness reconnects.

## Tasks and recovery

`Task` owns a private `TaskLifecycle`: Active, Suspended, or Finished. `WorkflowState` contains a planning, execution, or goal state, and each workflow defines its own phase enum. Plan has Draft and Review, Execute has Implement, Check, Resolve, and Verify, and Goal has only Work. Suspension preserves that phase, and read requests never settle tasks or clear waits.

The controller validates candidate updates before replacing the task and committing its dependent artifacts. Artifact snapshots remain available for the plan, timeline, and transport adapters, but consumers cannot obtain a mutable borrow of the live task. Loading a task also validates its workflow against the serialized phase and lifecycle summary.

Gate phases contain an `ExecutionContract` whose private construction requires an accepted revision and a workspace baseline. Verify additionally requires accepted manual actions and passing automated evidence for that revision. `SuspensionReason` distinguishes user pause, no progress, total turn exhaustion, provider limits, and reported failures while retaining the workflow variant.

Control mutations require the exact `TaskPermit` revision. Completed-turn accounting uses a separate observation fence that survives legal phase handoffs and changes on user control or explicit recovery. This lets a turn consume its allowance after advancing the phase while preventing its late callback from affecting a resumed task. `TaskTurnId` deduplicates observations, and terminal outcomes retain the stable task identity.

`TaskNextAction` derives provider work, local checks, or a wait from the current lifecycle and phase. Durable continuations use a `TaskWork` enum: Provider carries an instruction, and Checks carries no synthetic prompt. A continuation identifies its exact completed source exchange, so a newer chat exchange cannot strand it. Resuming Check reserves local work instead of starting a provider turn.

Workflow changes never enable writing or widen filesystem scope. When a permission change resumes work, Harness tells the model the previous and current approval modes and asks it to inspect interrupted operations before retrying.

Restart recovery keeps committed phases and completion evidence, suspends pending entries and continuations, and never starts a provider automatically. Explicit resume reactivates the retained task; a proven pending entry keeps its provider-input identity. Uncertain provider delivery remains governed by the submission journal rather than by the task's phase. Failure, disconnect, and unknown outcomes stop automatic queue draining.

New conversations start in Ask; current conversations retain their explicit approval mode, including across Plan → Execute. The plan-review answer to "How should execution handle tool approval?" is such an explicit choice. Entry admission commits it to the session through the same permission authority as `/allow` and publishes the configuration, so Implement continuations, Verify, and resumed phases run under it, the winbar shows it, and a later `/ask` replaces it. Only the executor model stays a per-invocation `TaskInvocationConfiguration` override. When approval was also an override of the entry turn alone, continuations reverted to the pre-acceptance mode and raised review popups the user had already declined. Database replacement discards obsolete task state rather than reconstructing it. The current task decoder validates phase payloads instead of guessing transitions from an older representation.

After exclusive runtime ownership is acquired, recovery also settles detached background tools inside already-completed exchanges. It preserves the foreground outcome, output, and last observed duration. On a fresh Copilot binding, native history is checked for unmatched tool calls; only a verified incomplete suffix can be truncated, and the retained prefix is checked again before new input is sent. The repaired session's provider prompt cautions that prior actions may have completed and must be inspected before repeating writes. Successful recovery does not emit an editor warning; ambiguous history blocks admission rather than discarding completed work.

## Plan generation

The broker owns plan generation as one bounded continuation loop for every backend. Each turn applies plan edits to the canonical document and either submits the plan or reserves another bounded turn. Required questions stop execution while preserving the continuation; optional questions leave that loop running. A required-question stop does not count as a failed progress attempt, even when the user answers before the turn finishes settling. `ContinuationBudget` uses a default total allowance of 20 model turns and stops after five consecutive turns without observable progress. Plan acceptance carries consumed allowance into Execute; review revisions do not replenish the total allowance. Explicit resume or retry resets the continuation allowance while retaining task history.

Execution compares saved workspace checkpoints and plan document versions when observing progress. Phase changes, reads, failed edits, and repeated findings do not reset its no-progress count; new completed verification evidence can advance a manual verification attempt. Local check operations and human waits do not consume model turns. The native task summary supplies workflow, phase, and status to the winbar, so Lua does not reconstruct lifecycle state from artifact records.

`PlanStateMachine` validates plan transitions. `PlanQuestionLedger` records answers, skips, and withdrawals by both provider ID and content digest, so a resolved decision cannot reopen when a provider retries with a different ID. Automatic retries and goal continuations admit new exchanges without synthetic user prompts, so each attempt keeps its own metrics.

## Context and compaction

Provider usage normalizes into durable `ContextUsage`. `AutoCompaction` is a per-session policy with an enabled flag and a token threshold. It is off for ordinary sessions, and Background sessions enable it at 128,000 tokens. It runs after completed provider work and never estimates tokens from bytes. The threshold compares with `ContextUsage.used`, the latest request's context occupancy, not the summed input of every request in a turn, so a tool-using turn can report more input than the context holds without becoming due. `AutoCompaction::validate` rejects a threshold at or above the reported capacity, and `compaction_threshold.lua` offers only presets below that capacity so cycling past the smallest preset cannot wrap to an invalid one.

`Compaction` is a separate lifecycle with an operation identity, observed start and completion, and an outcome. Its after-compaction token count is accepted only once the provider confirms completion, and a turn that ends first records an unconfirmed outcome. `native_compact` gates manual `/compact`, which uses Codex `thread/compact/start` or Copilot `session.history.compact` and creates no user exchange. The sole stream returned by `thread_open` remains attached across turns for provider-initiated compaction. Manual `thread_compact` requires settled foreground work and correlated completion evidence; its result carries an observation watermark.

Codex runs `thread/compact/start` as an ordinary turn on the compacted thread. While its normalizer is bound to the compaction's `SessionOperation`, that turn belongs to the operation: it admits no host invocation, its turn and item notifications contribute only thread-level facts, and an error on it settles the attempt as failed. Admitting it as an invocation would publish activity under a scope Harness never created, which `validate_native_event` rejects as an ownership conflict. The native `contextCompaction` item also takes the host operation ID as its compaction ID, so the host attempt and native progress settle one session-event record. A manual compaction releases the binding afterward, so later provider-initiated compaction keeps its native identity. Because `SessionPhase::Compacting` follows any Running compaction record, an attempt that ends without completion evidence, through a failed result, unconfirmed evidence, or lost worker observation, marks every Running root-thread compaction Unconfirmed. The `provider.observation_failed` trace records the failing stage as `error_code` and the normalized `ObservationFailure` as `kind`, because the trace policy drops free-form error text.

## Rollback

`/undo` offers conversation-only rollback, or rollback with file restoration when a checkpoint exists. Rollback collects execution, pauses the task, rewinds the provider conversation natively, and restores the reverted prompt to the composer when the user wrote it.

Harness never rebuilds a conversation by replaying retained exchange records into a fresh provider session, because those records include full tool output and a long session exceeds the model's context limit in a single prompt. Rollback therefore keeps the provider session and calls `ThreadRewind`, which discards every native turn after one checkpoint. Each exchange records `provider_checkpoint_id`, the checkpoint its last completed turn reported, and `HarnessBroker::native_rewind` selects the checkpoint of the last retained exchange. A retained exchange with provider turns but no checkpoint has no safe boundary, so rollback rejects it. Codex maps the rewind to `thread/revert` before the turn after the checkpoint, and Copilot to `history.truncate` at the first primary `user.message` after it. Claude Code restarts its idle process at the checkpoint with `--resume-session-at`; it has no transcript entry before the first interaction, so it reports rewinding there as unsupported, and rollback presents that as unsupported rather than retryable. Antigravity's ACP integration lacks the `NativeConversationRewind` capability: the editor reports rollback as unsupported and the host rejects it before changing anything.

The rewind is idempotent, because an absent successor turn makes it a successful no-op, so a failed or uncertain attempt leaves history uncommitted and undo repeats the same request. File restoration stays with Harness checkpoints: `RollbackEffect::run` restores files under their journal first and rewinds afterward. A rewind failure after restoration leaves the journal incomplete, and restore recovery repeats both steps. Neither step undoes command, network, or other tool side effects. A task start or continuation carries Harness-authored instructions, so the transcript and the `/undo` picker show only its notice, such as `Execution started: <title>` or `The task was resumed`, and rollback leaves the composer untouched. Rolled-back exchanges leave the timeline but keep their records. Sessions outside Git cannot be restored, so enabling writes there requires confirmation.

## Out of scope

Harness adds providers only through the typed `Backend` contract and never scrapes terminals. The broker runs per Neovim process, so controlling a session from another editor would need a shared daemon with stronger lease coordination. Provider-local hooks and command policies can still reject a tool under Harness Allow, because backend-native policy stays authoritative outside the Harness protocol.


## Question ownership

`QuestionController` stores each session's questions, resolution, discussion,
operation receipts, and pending outbox in `session_question_state` in
`harness.sqlite3`. Creation and answer/outbox persistence occur before publication,
without the broker's execution lock. Independent SQLite connections serialize
mutations with immediate transactions.

The input worker transfers each outbox entry into the existing input journal using
a stable operation ID, then acknowledges that transfer. Provider acknowledgment
remains owned by the submission journal. A crash before transfer repeats admission;
a crash after admission reuses its receipt. Unknown provider delivery requires
reconciliation and never authorizes blind replay.

The question tools own their usage guidance; general prompts do not duplicate it.
`question_ask` defaults to returning Harness-generated question IDs immediately,
while answers arrive through normal user-input delivery. With `wait: true`, it
keeps the tool call pending until every requested question is answered or withdrawn.
These questions appear as required, but do not trigger provider cleanup or a second
chat continuation. The tool returns their resolved records, without also sending
the answers through the input journal. Its live registration ends on completion or
cancellation; a later answer without a waiting tool uses ordinary message delivery.
When a wait ends without returning its answers, because the tool was cancelled, including
after it reported them, or because the owner closed, the owner moves every unreturned
answer to the outbox. A committed question change requests outbox admission, so a live
owner admits those answers at once; a closing owner leaves them for the next owner, which
admits the outbox at startup.
The model supplies neither question IDs nor mutation operation IDs. Existing IDs
and revisions select questions for replies, revisions, and withdrawals.

Legacy required questions stop active provider work through the shared cleanup
boundary and block automatic input and checks. Answers and clarification can run
discussion-only turns with writes disabled; task continuation waits for required
resolution and outstanding answer delivery.
Exchange completion retains questions. Task cancellation/replacement retires its
pending questions, and late answers cannot target the replacement task.

A legacy required question in plain chat retains one continuation for its original
exchange. Once required decisions resolve, the input worker admits user responses
first and then queues that continuation through the same journal. It cannot steer
into a read-only discussion turn or replay after acknowledgment. An optional
answer never creates a continuation for completed work. Each idle question delivery
admits a new discussion exchange instead of reopening the completed source exchange.
Cancellation revokes unfinished chat continuations and retires their required
questions. Optional chat questions remain answerable after cancellation or normal
completion; late answers enter discussion without resuming the original work.

### UI observation subscriptions

Terminal inventories arrive through normalized provider events. Check catalog views subscribe to `checks_changed`, emitted after committed check updates, so ordinary streamed text does not reload the catalog. Resource toggles subscribe before command admission, match completion by operation identity, and perform one durable read after the receipt to cover attachment races. They do not repeatedly query settings. Host exits and failed requests provide connection diagnostics; the UI does not send health heartbeat queries. Local animation clocks, operation deadlines, and delayed retries after an actual failed request remain separate from state observation.

Picker `on_dispose` releases owned subscriptions on every close, including confirmation and silent replacement. `on_close(resume, reason)` remains the optional user-workflow callback; `reason` is `"user"` only for the user's own close key and `"programmatic"` for every other close, including replacement by `Picker.open`, so an owner can tell a dismissal from a displacement. `Picker.when_idle` runs a callback once no picker is open.

LSP requests observe cancellation, scope lease release, and tool-selection revisions through watch notifications. Structural analysis also observes the pending request lifetime, so dropping an editor request cancels its analysis without a periodic map lookup. Check workers consume the owner's cancellation stream and publish output only after new bytes arrive; the existing output coalescing delay does not wake or rewrite idle progress.
