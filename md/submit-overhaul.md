# Harness submission overhaul: context and handoff

Updated October 27, 2026. This document records the current working-tree changes,
the user's final requirements, observed failures, and remaining boundaries. It
does not authorize replaying user messages or modifying running sessions.

## Why this started

The `symbol-store-2` session showed `draft`, `waiting`, and `stopped` while Rust
continued planning. Submitting "are you stuck?" failed with
`a Harness execution request is already running`.

The inspected Harness session was
`64e5f236-3d52-4c4a-8440-d76e85163a45`. A question appeared at 1:56:30 PM, timed
out at 2:06:30 PM, and resumed planning automatically. The provider completed and
submitted plan version 23 at approximately 2:33 PM.

The editor had received the completed plan and review status. An old submission
diagnostic covered the actual "Waiting for plan review" status with virtual text.
That diagnostic also forced the winbar to say `stopped` and blocked Lua queue
draining.

The source problem combined independent Lua busy tracking with Rust-owned
automatic continuation and execution admission. Lua guessed that execution was
idle, submitted a new execution, and treated its rejection as a session failure.

## Final requirements agreed with the user

1. Rust owns actual message ordering and start/steer/queue decisions.
2. Normal execution contention means steering or queueing, not "Not submitted."
3. Neovim immediately displays optimistic actions. Typing, cursor movement,
   selection, and draft navigation do not require a host round trip.
4. Acknowledgements resolve their own operation. Rollback must not overwrite
   newer typing or remove newer queued operations.
5. Existing queue editing, deletion, unqueue, attachments, model selection,
   task ownership, pause/resume, retraction, and check scheduling must remain.
6. Do not display a `[sending]` state. Claimed messages leave the visible queue.
7. Keep the queue in memory. Persisting queued work across restart was not
   requested and was explicitly rejected.
8. Keep existing durable session history and provider-delivery evidence.
   Never retry an uncertain provider send as though non-delivery were proven.
9. Shared SQLite writers must acquire write admission before reading state they
   modify, allowing ordinary cross-process lock waits instead of transaction
   upgrade failures.

## Implementation history and corrections

The first implementation added `input_message`, `input_operation`, and
`input_state` tables to the shared `harness.sqlite3`. This was unnecessary scope.
It introduced queue-related SQLite contention and retained failed entries at the
head of the runnable queue.

The user then reported long-lived `Queued: [sending]` entries. The implementation
had confused provider acceptance with completion of the entire agent response.
An acceptance notification now separates those lifetimes. Execution still owns
its permit until response finalization, but a claimed or accepted message is not
displayed as pending queued work.

A subsequent screenshot showed an idle session with:

```text
Queued follow-up inputs
[failed: database is locked: Error code 5: database is locked] show example of each
```

The current correction removes all queue database access. It does not merely
increase a database timeout or hide the failed label.

## Current ownership

### Rust session memory

`SessionInputQueue` in
`nvim/rust/forge/crates/forge-harness/src/input.rs` owns ordered messages,
reservations, revisions, pause barriers, and idempotent operation results.
`SessionController` owns one queue for its lifetime.

Queue operations use a short mutex-protected memory transition. They do not open
SQLite, write queue files, or hold the lock during provider work. Complete edit
batches validate before mutation, preserving all-or-nothing edit behavior.

The queue and operation outcomes disappear with the host. A fresh controller
starts empty. The old queue tables are no longer read or created by this code.
Previously created tables have not been destructively removed from the user's
database, and old stored messages have not been automatically replayed.

Failed messages remain available for correction but are excluded from runnable
FIFO admission. Uncertain delivery remains a safety boundary rather than
permission to replay.

### Rust delivery and execution

`nvim/rust/forge/crates/forge-harness/src/service/input.rs` owns the input worker
and routes messages through existing execution admission and provider steering.
It retains task boundaries, edit reservations, and control revisions.

`backend/submission.rs` retains the existing persistent provider journal. A
watch notification fires after provider acceptance commits. It is not a second
queue or a claim that the agent response finished.

`broker/interaction.rs` accepts the worker's existing submission handle.
`broker/mod.rs` wakes the queue at execution-start boundaries and updates the
task when automatic question continuation resumes.

The previous `queue.update` boolean publication and Lua `check_queue.lua` owner
were removed. Completion checks derive pending work from the host queue.

### Neovim optimism

`nvim/lua/forge/views/harness/input_state.lua` retains confirmed host state,
ordered pending operations, stable local entry objects, and rejected local
input. It immediately applies optimistic changes and reconciles operation IDs.

`prompt_submission.lua` clears only the captured composer draft and restores it
only if later typing has not replaced it. `queue_editor.lua` retains drafts and
requests edit reservations without sending per-keystroke requests.

On a host runtime-epoch change, Lua resets the previous host's volatile queue
instead of discarding a lower revision as stale or replaying old operations.
An active edit draft remains available for explicit recovery.

`controller.lua`, `snapshot.lua`, and `session.lua` distinguish local request
activity from host execution activity. `health.lua` and `status_hint.lua` keep
message diagnostics separate from canonical task status and review actions.

## SQLite changes and limits

Shared history still uses `harness.sqlite3`. Provider delivery still uses
`provider-submissions.sqlite3`, and settings use `settings.db`.

The storage audit found remaining deferred mutating transactions in
`storage/mod.rs` and `storage/model_pin.rs`. These now begin with
`TransactionBehavior::Immediate`. Existing submission-journal, settings,
ownership, and tool-output mutating transactions already use immediate
admission. Read-only `read_snapshot` intentionally remains deferred.

The existing five-second busy timeout remains. This is bounded waiting, not an
indefinite cross-process lock wait or a guarantee that storage can never fail.
Exhausted waits and real I/O failures still surface.

This change does not convert the entire synchronous SQLite storage subsystem to
dedicated asynchronous workers. Do not claim that all existing storage waits
have been removed from Tokio workers. That broader architecture requires a
separate implementation if needed.

## Source map

All paths are relative to the dotfiles repository.

| Boundary | Main paths |
| --- | --- |
| Queue state and operations | `nvim/rust/forge/crates/forge-harness/src/input.rs`, `input/test.rs` |
| Input worker and control integration | `nvim/rust/forge/crates/forge-harness/src/service/input.rs`, `service.rs` |
| Provider acceptance evidence | `nvim/rust/forge/crates/forge-harness/src/backend/submission.rs` |
| Exchange and automatic continuation | `nvim/rust/forge/crates/forge-harness/src/broker/interaction.rs`, `broker/mod.rs` |
| Shared write transactions | `nvim/rust/forge/crates/forge-harness/src/storage/mod.rs`, `storage/model_pin.rs` |
| Protocol | `nvim/lua/forge/protocol_contract.json`, Rust `protocol/method.rs`, `forge-protocol/src/lib.rs`, host `src/router.rs` |
| Optimistic editor state | `nvim/lua/forge/views/harness/input_state.lua`, `prompt_submission.lua`, `queue_editor.lua` |
| Lifecycle and diagnostics | Lua `controller.lua`, `snapshot.lua`, `health.lua`, `status_hint.lua`, `nvim/lua/forge/session.lua` |
| Documentation | `nvim/contributing/forge/architecture/harness.md`, `planning.md`, `nvim/docs/forge/commands.md` |

The protocol version is 25. Existing running editors were not hot-reloaded or
restarted. Loading changed Lua against an older process can violate the wire
contract, so restart Neovim after the final development executable is rebuilt.

## Evidence and current progress

Before the memory-only correction, the targeted queue/check handoff, timeout
integration, and real-host queue viewport fixtures passed. Eleven focused Lua
fixtures and the targeted Rust input/service cases also passed.

After the memory-only correction, these exact Rust cases passed:

- `input::tests::operations_preserve_delivery_and_edit_boundaries`
- `service::tests::host_owned_input_steers_and_preserves_edit_and_pause_barriers`
- `storage::model_pin::test::model_pin_write_waits_for_another_connection`
- `storage::model_pin::test::model_pins_concurrent_writes_preserve_other_models`

The lock-wait regression holds an immediate write transaction on one connection,
proves that a second writer has not completed, releases the first transaction,
and observes the second write succeed. The input regression covers edit barriers,
idempotency, task isolation, uncertain delivery, pause revisions, fresh empty
queues, and failed-entry bypass.

The memory-only follow-up also passed six focused Lua fixtures, including host
runtime-epoch reset and optimistic draft recovery. The development executable
was rebuilt successfully with the selected Rust 1.94.0 toolchain using
`cargo build --profile dev --locked --bin forge` in `nvim/rust/forge`.
The isolated real-host queue/check and viewport fixtures then passed against
that executable. Running user editors have not been restarted.

Recorded follow-up runs:

| Coverage | Run ID | Result |
| --- | --- | --- |
| Memory queue invariants | `68c0157f-f23f-47b8-874b-2fe935dd6c15` | Passed |
| Active execution and pause/edit boundaries | `f56c412b-1028-43ab-acd5-e747ea75d552` | Passed |
| SQLite write waiting | `844fda58-498a-45c1-8e24-997c49228e19` | Passed |
| Concurrent pin writers | `140c4bb6-66b2-467c-b745-5d23f5db9577` | Passed |
| Six focused Lua fixtures | `b84c77a5-2c6d-4075-91bd-bff5ed076b67` | Passed |
| Final development build | `7cf05fb0-96a0-4c40-94f2-c5b12705e146` | Passed, 24.23 seconds |
| Real-host queue/check and viewport | `6a4fce0c-becd-40b4-a074-ea34755c33ce` | Passed |

An earlier Clippy run failed in pre-existing `forge-buffer` and `forge-protocol`
code. Those unrelated diagnostics were not suppressed or modified.

The saved `lua-test-mock-integration` registry does not include
`harness_check_queue_host.lua`. The smaller `harness_input_queue_host.lua` selects
the first queue/check scenario through the ordinary exact-fixture runner.
The full multi-scenario fixture can exceed that runner's 30-second deadline.
Do not treat that timeout as proof of a provider deadlock.

## Remaining review boundaries

Check memory queue lifetime, provider acceptance versus response completion,
pause-before-delivery races, cross-task ordering, edit/delivery conflicts,
host-epoch reset, attachment retention, and captured model configuration together.
Preserve uncertain provider evidence even though the visible queue is volatile.

The user-reported database-lock screenshot was observed, but its exact failing
stack was not recovered from the inspected logs. Deferred queue transactions
were a concrete defect, not proof that every historical lock error came from the
same call site.

No automatic recovery of the screenshot's old failed message has been performed.
Never silently resend it during a restart or reinterpret a possibly delivered
message as safe to replay.
