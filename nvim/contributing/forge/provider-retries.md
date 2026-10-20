# Provider retries

A failed model request can leave completed tools or uncertain remote work behind.
Forge retries only a confirmed terminal network or capacity failure whose attempt
produced no tools, model text, plan mutation, or control result. Transport loss
without a terminal provider response remains a failure. Forge never infers retry
safety from words in an error message.

## Ownership and lifecycle

`backend/retry.rs` owns the shared request runner used by root, child, and
Background sessions. Provider adapters classify native errors and return a
`RetryableFailure` only after checking `replay_safe`. The runner retains the
provider thread, selected model, settings, and control context. It submits a
continuation of the retained request, rather than appending the original user
message again. Retries remain in the same Harness exchange.

The initial attempt has no delay. Five retries wait 1, 5, 10, 30, and 60 seconds,
for 106 seconds of added delay at most. Provider execution time is additional.
The budget belongs to one runner invocation and does not reset after each error.
After the fifth retry, the original final provider error propagates through the
normal exchange failure path. A non-retryable error stops immediately, including
one encountered after earlier retryable errors.

The runner publishes `provider_retry` with the retry number, maximum, and an
optional delay deadline. `Exchange` accepts it only for a known native turn in
the active exchange. `SessionPhase::RetryingProvider` renders, for example,
`Working · retry 2/5 · retrying in 5s`. During the new attempt the countdown
disappears but `retry 2/5` remains. Success and finalization clear retry state.
Cancellation drops the runner's asynchronous wait and the existing execution
owner performs native cleanup. No background timer can dispatch a later retry.

## Current allowlist

Provider metadata was checked against the sources below on 2026-10-20.

| Provider | Accepted metadata | Additional boundary |
| --- | --- | --- |
| Codex | `codexErrorInfo: "serverOverloaded"` | The native turn must finish with `status: "failed"`. |
| Codex | `httpConnectionFailed`, `responseStreamConnectionFailed`, or `responseStreamDisconnected` | The variant must have `httpStatusCode: null`, 408, 500, 502, 503, or 504, and the native turn must finish as failed. |
| Copilot | `errorType: "query"` with `statusCode` 408, 500, 502, 503, or 504 | The root session must subsequently become idle within 30 seconds, and the complete attempt must remain replay-safe. |

All other errors fail without Forge retry. This includes authentication,
authorization, quota, policy, context limits, generic internal errors, HTTP 429,
unknown codes, and tool errors. A generic internal error or HTTP failure alone
does not prove a temporary network or capacity problem.

Copilot SDK 1.0.7 classifies `errorType: "model_call"` as a native transient error.
Its agent loop continues after that event. Forge keeps reading that loop and does
not create an overlapping retry. Codex nonterminal error notifications likewise
do not start Forge retries. Only terminal turn failure reaches the shared runner.

Sources:

- [Codex error variants](https://github.com/openai/codex/blob/main/codex-rs/app-server-protocol/src/protocol/v2/shared.rs), inspected file blob `65c7db5aebe8bb5e3d5653a9109d412f099e1edb`.
- [Codex turn input](https://github.com/openai/codex/blob/main/codex-rs/app-server-protocol/src/protocol/v2/turn.rs), inspected file blob `38b00228a6d4c0693c13daa03734bb8f121007dc`.
- [Copilot session error schema](https://github.com/github/copilot-sdk/blob/main/nodejs/src/generated/session-events.ts), inspected file blob `dc403c7b0f8fbeb7ff6c755e72810ef447727ecd`.
- The installed `github-copilot-sdk` 1.0.7 `SessionEvent::is_transient_error`
  implementation defines Forge's current native retry ownership boundary.

## Submission and acceptance boundaries

The [Codex App Server reference](https://developers.openai.com/codex/app-server#turns)
defines `turn/start` as returning the initial turn and `turn/steer` as returning
the accepted turn ID. Its [API overview](https://developers.openai.com/codex/app-server#api-overview)
defines persisted item pagination. Forge uses these native responses and history
identities as acceptance evidence, not a successful local transport write.

A timeout while sending input does not prove that the provider rejected it.
The installed Copilot SDK 1.0.7 documents that cancellation after its writer
commits a frame can lose the caller's response while the CLI still processes the
message. Its `MessageOptions` exposes no caller-supplied message identity.
The returned message ID correlates an acknowledged send with later events.
[Copilot's send implementation](https://github.com/github/copilot-sdk/blob/main/rust/src/session.rs)
and [agent-loop guide](https://github.com/github/copilot-sdk/blob/main/docs/features/agent-loop.md)
support treating `session.idle` as the ready boundary for another request.
The [error hook guide](https://github.com/github/copilot-sdk/blob/main/docs/hooks/error-handling.md)
allows native retry handling, but its general `recoverable` flag does not prove
that repeating a user submission is safe.

Codex's [startup recovery](https://github.com/openai/codex/blob/main/codex-rs/tui/src/startup_recovery.rs)
keeps its initial draft in memory until the matching client message is acknowledged.
Its [submission handler](https://github.com/openai/codex/blob/main/codex-rs/tui/src/app/user_turn_submission.rs)
acknowledges that draft only after `turn/start` or `turn/steer` succeeds.
`clientUserMessageId` reaches `ThreadItem::UserMessage.clientId`, but that
correlation field is not itself evidence that duplicate sends are deduplicated.
Codex's [turn runtime](https://github.com/openai/codex/blob/main/codex-rs/tui/src/chatwidget/turn_runtime.rs)
finalizes a capacity failure, displays its error, and admits the next queued input.
It does not replay the failed request in that handler.

Claude and Antigravity currently use
[ACP v1 prompt turns](https://agentclientprotocol.com/protocol/v1/prompt-turn).
The `session/prompt` response ends the turn and includes a `stopReason`. It is not
an early acceptance receipt. Forge validates that response and commits acceptance
before local output cleanup, which can fail independently. A transport failure,
timeout, or malformed response leaves delivery uncertain. Neither v1's optional
message IDs nor replayed session updates provide a mandatory client-assigned
correlation ID that proves this particular input was accepted.

Claude's pinned 0.89.1 adapter also advertises the `_session/steering` extension.
Its [steering implementation](https://github.com/agentclientprotocol/claude-agent-acp/blob/v0.89.1/src/acp-agent.ts)
defines `injected` and `startedNewTurn` as accepted outcomes. With Forge's
`idleBehavior: "promptRequired"` opt-in, the exact
`promptRequired` / `noRunningTurn` response guarantees it did not enqueue the
input. Only that response returns the journal entry to `Prepared`. Unknown
outcomes and generic protocol errors cannot enter the fallback queue.
Antigravity does not advertise native steering through Forge's current adapter.

The [ACP v2 draft prompt lifecycle](https://agentclientprotocol.com/protocol/v2/prompt-lifecycle)
changes `session/prompt` to return when the message enters the conversation and
includes its `messageId`. That enables an earlier acceptance boundary once the
protocol and each provider adapter support it. Forge still negotiates v1 and
must not apply v2 response semantics to a v1 connection. A lost v2 response will
still require documented correlation or reconciliation before replay is safe.

| Adapter | Acceptance evidence | Lost-response recovery |
| --- | --- | --- |
| Codex | Successful native start or steer response | Exact client UUID in fully paginated native history. |
| Copilot | Native `session.send` message ID | One exact unclaimed primary user message after the saved history cursor. |
| Claude ACP v1 | Valid prompt completion or documented steering acceptance | Retain uncertainty and block replay. Only documented idle steering rejection permits fallback. |
| Antigravity ACP v1 | Valid prompt completion | Retain uncertainty and block replay. |

The Mock backend has no remote delivery boundary. It records local acceptance
before processing initial input or adding steering to its active turn.

## Durable submission state

`backend/submission.rs` owns the input journal in
`provider-submissions.sqlite3` under the Harness data root. SQLite uses WAL and
FULL synchronous commits. Input persistence remains enabled when diagnostic
tracing is disabled. Root admission saves the complete typed input and attachment
references before acknowledging the composer. Live steering saves its input
before entering the active-turn queue. Child and Background provider requests
use the same journal with their own execution-owner identity.

| State | Evidence and permitted transition |
| --- | --- |
| `Prepared` | No adapter has entered dispatch. A later prompt can include this input as earlier context. |
| `Sending` | The adapter committed its native correlation target before polling the write. A timeout or disconnect leaves this state uncertain. |
| `Accepted` | A native response or positively correlated history proves acceptance. The input is never replayed as missing context. |
| `Cancelled` | An explicit user cancellation excludes the input from automatic recovery. |

Codex commits `Sending` before `turn/start` or `turn/steer`, supplies the journal
UUID as `clientUserMessageId`, and commits `Accepted` on the successful native
response. Read-only reconciliation paginates `thread/items/list` and looks for
that exact `clientId`. Native goal resume has its own acknowledgment boundary.
Its uncertain control result remains uncertain because it has no user-message
correlation item. Internal child synthesis and capacity-retry continuations do
not create another user-input obligation.

Copilot captures a native event-log cursor and the exact rendered prompt before
`session.send`. Its returned message ID acknowledges acceptance. Reconciliation
reads the primary user-message history after that cursor and requires one exact
match with a native message ID that Forge has not already acknowledged. It
excludes native automatic continuations and messages with another source.
Multiple matches remain uncertain. A journal transaction prevents another native
dispatch while an earlier dispatch remains unacknowledged. Unsupported history,
expired cursors, unmatched input, and transport errors do not establish absence.
The default adapter reconciliation also preserves uncertainty.

Both ACP providers commit the native session ID and request method as `Sending`
immediately before the input transport call. Startup, configuration, and attachment
preparation failures leave input `Prepared`. The shared runner does not mark ACP
input as dispatched before those preparations. ACP steering uses the same journal
boundary, including caller cancellation while waiting for the response.

Before a later prompt dispatches, the runner reconciles older uncertain entries.
It includes only definitely undelivered inputs as earlier user context, retaining
their order, skill selections, and attachments. The newest instruction can amend
or cancel that context. Later queued inputs cannot be included in an earlier
submission. Acceptance of a combined message commits all included input IDs in
one transaction. Repeated acknowledgments preserve that committed receipt and
do not settle the included inputs again. A failed acknowledgment commit leaves every input recoverable
without authorizing duplicate execution.

Live steering carries the journal handle through both queues and native
acknowledgment. A timeout after dispatch cannot enter the fallback queue. A
fallback accepted by the host still remains `Prepared` until the subsequent
native request is acknowledged. Failure to reconcile blocks dispatch and reports
the last error while keeping both the earlier input and the newest prompt saved.
A pre-admission Codex JSON-RPC rejection (`-32600`, `-32601`, or `-32602`) returns
the input to `Prepared`. Generic provider errors do not.

This contract prevents silent input loss under the recorded disk and protocol
boundaries. It does not promise exactly-once remote execution. Neither provider
exposes a demonstrated idempotent submission API, and missing history cannot
prove that a delayed send will never arrive. Disk errors propagate before host
admission or native dispatch. Accepted but unfinished work remains a failed or
interrupted exchange rather than being relabeled as missing input.

## Extend submission evidence

A new adapter must persist its correlation target before its first possible
write and acknowledge only a successful native submission response. Implement
`Backend::reconcile_submission` with read-only, fully paginated history. Return a
receipt only for a positively identified input. Return `None` when acceptance
cannot be proved. Never convert a timeout, an empty history page, or a message
substring into a rejection. Test the write-before-ack crash, acknowledgment
commit failure, expired history, cancellation, attachments, and later queued
input before enabling recovery. Submission evidence and transient error
classification are independent contracts.

## Enable a newly observed error

1. Capture the final error and its preceding provider lifecycle in the session
   log. Preserve the provider version, exact code, structured HTTP status, native
   turn identity, and whether the provider continues independently.
2. Verify the metadata against that provider's schema or implementation. Determine
   whether the failure is transient, whether native execution has ended, and
   whether any tool or model output can already have occurred. Do not use a
   message substring, a generic `recoverable` flag, or a broad HTTP range as proof.
3. Add the smallest exact match to `backend/codex/retry.rs` or
   `backend/copilot/retry.rs`. Keep unknown codes rejected. If safe termination or
   replay cannot be established, leave the error outside the allowlist and add a
   separate reconciliation design before enabling it.
4. Add a positive classifier case and neighboring negative cases. Exercise the
   native adapter's terminal event, recovery, exhausted budget, cancellation, and
   output-before-failure boundaries. Keep the shared schedule and status logic
   unchanged when adding an error code.
5. Update the allowlist above with its evidence and provider version. Run the
   targeted retry tests, rebuild the development executable, and verify the status
   in a fresh host using the procedure in [Building and testing](building-and-testing.md).

## Verification

`cargo test --profile dev --locked -p forge-harness --lib backend:: -- --test-threads=1`
covers the provider classifiers, submission journal, and adapter boundaries.
ACP tests include preparation failures, disconnects, malformed responses,
cancelled waits, database failures, and steering acceptance versus idle rejection.
They use simulated peers and require Node.js, without provider credentials.

After rebuilding, run `nvim/tests/forge/harness_acp_submission_host.lua` in fresh
headless Neovim processes with `vim.g.forge_acp_fixture_kind` set to `"claude"`
and `"antigravity"`, and `vim.g.forge_acp_fixture_lost_ack` set to `false` and
`true` for each provider. The fixture checks accepted follow-ups and a peer that
exits after receiving input but before acknowledging it. It asserts that uncertain
delivery blocks the next native send and preserves the original error.

## Diagnostics

With Harness tracing enabled, `/log` retains `provider.retry` entries for failed,
scheduled, started, recovered, stopped, exhausted, and cancelled retries.
`provider.submission` records each committed delivery transition, input ID,
execution owner, included input IDs, native target or receipt, and retained error.
Retry entries include the owning agent, current retry count, and maximum. Failed entries retain the
complete structured error, message, and provider thread/turn identity. Scheduled
entries include the delay and deadline. Existing credential redaction applies.
Detailed records are not truncated to the log rotation size. Rotation controls
retained files rather than admission of an individual diagnostic record.

The final UI error remains the actual provider error, not a generic retry-limit
message. Retryable intermediate errors appear through retry status and logs.
Permanent errors retain their normal immediate failure presentation.
