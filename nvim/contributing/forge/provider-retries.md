# Provider retries

Retrying a failed model request can repeat completed tool work, so an error message that sounds temporary is not evidence that a retry is safe. Forge retries only a confirmed terminal network or capacity failure whose attempt produced no tools, model text, plan mutation, or control result. Transport loss without a terminal provider response stays a failure.

This guide explains the retry contract and how to allow a new error. For the delivery of user input, see [Provider submissions](provider-submissions.md).

## Lifecycle

`backend/retry.rs` owns the request runner shared by root, child, and Background sessions. Adapters classify native errors and return a `RetryableFailure` only after checking `replay_safe`. The runner keeps the provider thread, model, settings, and control context, and submits a continuation rather than appending the user's message again. Retries stay in the same exchange.

The first attempt has no delay. Five retries then wait 1, 5, 10, 30, and 60 seconds. The budget belongs to one runner invocation and does not reset after each error. After the fifth retry, or on any non-retryable error, the actual provider error goes through the normal failure path.

The runner publishes `provider_retry`, and `SessionPhase::RetryingProvider` shows it, for example `Working · retry 2/5 · retrying in 5s`. Cancellation drops the runner's wait, so no background timer can send a later retry.

## Allowlist

Provider metadata was checked against the sources below on 2026-10-20.

| Provider | Accepted metadata | Additional condition |
| --- | --- | --- |
| Codex | `codexErrorInfo: "serverOverloaded"` | The native turn finishes with `status: "failed"`. |
| Codex | `httpConnectionFailed`, `responseStreamConnectionFailed`, or `responseStreamDisconnected` | `httpStatusCode` is null, 408, 500, 502, 503, or 504, and the native turn finishes as failed. |
| Copilot | `errorType: "query"` with `statusCode` 408, 500, 502, 503, or 504 | The root session becomes idle within 30 seconds, and the attempt stays replay-safe. |

All other errors fail without a Forge retry, including authentication, authorization, quota, policy, context limits, generic internal errors, HTTP 429, unknown codes, and tool errors. A generic internal error alone does not prove a temporary problem.

Copilot SDK 1.0.7 classifies `errorType: "model_call"` as a native transient error and its agent loop continues, so Forge keeps reading and does not start an overlapping retry. Codex nonterminal error notifications likewise start no retry.

Sources:

- [Codex error variants](https://github.com/openai/codex/blob/main/codex-rs/app-server-protocol/src/protocol/v2/shared.rs), file blob `65c7db5aebe8bb5e3d5653a9109d412f099e1edb`.
- [Codex turn input](https://github.com/openai/codex/blob/main/codex-rs/app-server-protocol/src/protocol/v2/turn.rs), file blob `38b00228a6d4c0693c13daa03734bb8f121007dc`.
- [Copilot session error schema](https://github.com/github/copilot-sdk/blob/main/nodejs/src/generated/session-events.ts), file blob `dc403c7b0f8fbeb7ff6c755e72810ef447727ecd`.
- The installed `github-copilot-sdk` 1.0.7 `SessionEvent::is_transient_error` implementation.

## Allow a newly observed error

1. Capture the final error and the provider lifecycle before it from the session log, including the provider version, exact code, HTTP status, turn identity, and whether the provider continues on its own.
2. Verify the metadata against the provider's schema or implementation. Establish whether the failure is temporary, whether native execution has ended, and whether any tool or model output can already have occurred. A message substring, a generic `recoverable` flag, or a broad HTTP range is not proof.
3. Add the smallest exact match to `backend/codex/retry.rs` or `backend/copilot/retry.rs`, and keep unknown codes rejected. If safe termination or replay cannot be established, leave the error out and design reconciliation separately.
4. Add a positive classifier case and neighboring negative cases. Exercise the adapter's terminal event, recovery, exhausted budget, cancellation, and output-before-failure paths. Leave the shared schedule and status logic unchanged.
5. Update the allowlist with its evidence and provider version, run the targeted tests, rebuild, and verify the status in a fresh host.

## Diagnostics

With Harness tracing enabled, `/log` keeps `provider.retry` entries for failed, scheduled, started, recovered, stopped, exhausted, and cancelled retries. Failed entries keep the complete structured error and provider thread and turn identity, and existing credential redaction applies.

The final UI error is always the actual provider error, not a generic retry-limit message.
