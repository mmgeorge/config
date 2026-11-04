# Provider submissions

A send that times out does not prove the provider rejected the input, so resending it can run the same request twice, and dropping it can lose the user's message. Forge therefore records each input in a durable journal before dispatch, and treats an unconfirmed send as uncertain rather than failed. This prevents silent input loss within the recorded disk and protocol limits. It does not promise exactly-once remote execution, because no provider exposes a demonstrated idempotent submission API.

This reference explains what counts as acceptance for each provider, and how to add one. For retrying failed requests, see [Provider retries](provider-retries.md).

## Journal states

`provider/submission.rs` owns the journal in `provider-submissions.sqlite3` under the Harness data root, using WAL and FULL synchronous commits. It stays enabled when tracing is disabled. Root admission saves the complete typed input before acknowledging the composer, and live steering saves its input before entering the active-turn queue. Child and Background requests use the same journal with their own owner identity.

Owner-targeted steering is registered with the shared session execution engine before dispatch. The adapter reports delivery separately from execution completion. Its 30-second acknowledgment deadline produces an uncertain-delivery error; it never silently queues another copy. Cancelling the owning turn still interrupts delivery.

An explicit `NotConsumed` result proves that the addressed delivery did not consume input; it carries no native continuation authority. The host may retain that input for a later explicitly owned admission after the previous invocation settles. That admission uses ordinary `input_submit`, a fresh invocation scope, and a fresh delivery attempt for the original journal entry. An accepted result, disconnect, or unknown outcome never permits replay. Native terminal coverage, native resource settlement, and host commit all precede the completed host result.

| State | Meaning |
| --- | --- |
| `Prepared` | No adapter has entered dispatch. A later prompt can include this input as earlier context. |
| `Sending` | The adapter committed its native correlation target before the write. A timeout or disconnect leaves this state uncertain. |
| `Accepted` | A native response or positively correlated history proves acceptance. The input is never replayed. |
| `Cancelled` | The user cancelled, so automatic recovery skips the input. |

## Recovery checkpoints

Each logical `SubmissionId` retains a separate `DeliveryAttemptId`, rotated only after positive rejection proves another attempt is eligible. A `Sending` record retains that exact attempt, an opaque conversation identity, and a versioned adapter checkpoint. The journal does not decode native thread, cursor, or prompt fields. Only the owning adapter recognizes the checkpoint format and exact version, then checks native history for positive acceptance evidence. Unknown versions remain uncertain and cannot authorize replay. `input_reconcile` observes the original submission and attempt without sending input or opening a writable recovery binding. Lookup failures retain typed causes; inconclusive evidence remains unknown. Once reconciliation succeeds, the journal can restore the normalized conversation identity without matching a provider-specific variant.

Tracking and local acceptance are explicit integration capabilities. Mock declares local acceptance at its own boundary; real adapters declare tracking before dispatch. Adding a provider does not require a journal allowlist change.

## Acceptance evidence

A successful local write is not evidence of acceptance. Each adapter relies on what its protocol documents:

| Adapter | Acceptance evidence | Lost-response recovery |
| --- | --- | --- |
| Codex | Successful native `turn/start` or `turn/steer` response | Exact client UUID in fully paginated native history. |
| Copilot | Native `session.send` message ID | One exact unclaimed primary user message after the saved history cursor. |
| Claude ACP v1 | Valid prompt completion, or documented steering acceptance | Keep the uncertainty and block replay. Only documented idle steering rejection proves nonconsumption. |
| Antigravity ACP v1 | Valid prompt completion | Keep the uncertainty and block replay. |

Mock has no remote delivery boundary, and records local acceptance before processing.

### Codex

Codex commits `Sending` before `turn/start` or `turn/steer`, and supplies a correlation identity derived from both the journal entry and its delivery attempt as `clientUserMessageId`. Read-only reconciliation paginates `thread/items/list` for that exact attempt's `clientId`. A pre-admission JSON-RPC rejection (`-32600`, `-32601`, or `-32602`) returns the input to `Prepared`, and generic provider errors do not. Harness goal continuation is an ordinary separately journaled host input; adapters never invoke native goal continuation.

The [Codex App Server reference](https://developers.openai.com/codex/app-server#turns) defines these responses. `clientId` correlates a message but is not evidence that duplicate sends are deduplicated.

### Copilot

Copilot captures a native event-log cursor and the exact rendered prompt before `session.send`, and its returned message ID acknowledges acceptance. Reconciliation requires one exact match after that cursor, with a native message ID Forge has not already acknowledged. Multiple matches stay uncertain, and unsupported history, expired cursors, and transport errors do not establish absence.

SDK 1.0.26 documents that cancelling after its writer commits a frame can lose the response while the CLI still processes the message, and `MessageOptions` has no caller-supplied identity. See [Copilot's send implementation](https://github.com/github/copilot-sdk/blob/main/rust/src/session.rs).

### ACP

Claude and Antigravity use [ACP v1 prompt turns](https://agentclientprotocol.com/protocol/v1/prompt-turn). The `session/prompt` response ends the turn, so it is not an early acceptance receipt, and v1 has no mandatory client-assigned ID to prove delivery. Both providers commit the session ID and method as `Sending` immediately before the transport call. Startup and attachment failures leave input `Prepared`.

Claude's minimum supported 0.99.1 adapter advertises the [`_session/steering` extension](https://github.com/agentclientprotocol/claude-agent-acp/blob/v0.99.1/src/acp-agent.ts). With `idleBehavior: "promptRequired"`, only the exact `promptRequired` / `noRunningTurn` response proves the input was not queued, so only that response returns it to `Prepared`. Antigravity does not advertise steering.

The [ACP v2 draft](https://agentclientprotocol.com/protocol/v2/prompt-lifecycle) makes `session/prompt` return when the message enters the conversation. Forge negotiates v1, and must not apply v2 semantics to a v1 connection.

## Recovery before the next prompt

Before a later prompt dispatches, the runner reconciles older uncertain entries. It includes only definitely undelivered inputs as earlier context, keeping their order, skills, and attachments. Later queued inputs cannot be included in an earlier submission. Acceptance of a combined message commits all included inputs in one transaction, so a failed commit leaves them recoverable without authorizing duplicate execution.

Failure to reconcile blocks dispatch and reports the last error, keeping both the earlier input and the newest prompt saved. A timeout after dispatch cannot enter the steering fallback queue. Accepted but unfinished work stays a failed or interrupted exchange rather than being relabeled as missing input.

## Add a provider

Persist the opaque correlation token before the first possible write, and acknowledge only a successful native response. Implement `Provider::input_reconcile` with read-only, fully paginated history. Return `SubmissionEvidence::Accepted` only for a positively identified input, `NotAccepted` only when positive evidence proves that exact attempt cannot later be consumed, and `Unknown` otherwise. Never turn a timeout, an empty history page, or a message substring into a rejection.

Test the write-before-acknowledge crash, acknowledgment commit failure, expired history, cancellation, attachments, and later queued input before enabling recovery.

## Verification and diagnostics

Verify the journal and generic lifecycle fixtures in `forge-harness` and native classifiers and wire fixtures in `forge-providers`. Start with the affected compiler checks, then select the exact regression names from the changed module. ACP simulated peers need Node.js, without provider credentials.

After rebuilding, run `nvim/rust/forge/crates/forge-providers/tests/fixtures/submission_host.lua` in fresh headless Neovim processes, with `vim.g.forge_acp_fixture_kind` set to `"claude"` and `"antigravity"`, and `vim.g.forge_acp_fixture_lost_ack` set to `false` and `true`. It asserts that uncertain delivery blocks the next native send and keeps the original error.

With tracing enabled, `provider.submission` log entries record delivery transitions, input IDs, attempt IDs, and owners. Native correlation tokens and receipt bytes remain private and are not logged.
