# Typed provider interface follow-up

The work addressed duplicate transcript block identities and empty commentary entries caused by provider message/tool envelopes. The provider-interface implementation is in place and has passed targeted Rust, real-host, and terminal verification. Final sign-off remains open because the last mechanical timestamp-helper cleanup happened after those passing checks.

## Current state

The last edit removed the forwarding-only `CopilotEventDecoder::now_ms` method and changed four call sites to use the existing `super::timestamp_ms` helper directly:

`nvim/rust/forge/crates/forge-harness/src/backend/copilot/event_decoder/stream.rs`

No behavior change was intended. **This exact final edit has not been formatted, checked, tested, or rebuilt yet.** The current runtime executable predates it.

The implementation changes remain uncommitted. Preserve the unrelated user edit in `nvim/lua/plugins/lsp.lua`. Do not reset the worktree or stage unrelated files.

## What was implemented

- Replaced the optional/string/JSON provider envelope with an exhaustive `BackendEvent` enum. Message, tool, turn, usage, runtime, compaction, agent, checklist, approval, directory access, input acknowledgement, retry, diagnostic, and parent-boundary updates have explicit typed payloads. There is no catch-all `Other` event.
- Added required invocation and message/tool/turn addresses, typed backend and delivery errors, and explicit completion metadata. Provider completion and accepted-event draining remain separate from stream closure.
- Migrated Copilot, Codex, ACP, mock implementations, broker consumers, permissions, steering, checks, skills, background generation, persistence, and directly dependent fixtures.
- Added typed Copilot phase metadata for recognized `commentary` and `final_answer` values, plus explicit missing and unknown states. Message starts establish classification before text deltas when the provider supplies it. Unknown text remains visible without becoming a commentary owner.
- Kept provider-only starts and empty tool-request messages inside the adapter. Normalized message starts publish the first actual text. Tool-only envelopes emit tool events without empty commentary entries.
- Retained commentary while streaming it immediately. On successful Copilot completion without an explicit final answer, the adapter emits the last eligible commentary as a new response with a distinct synthetic identity. The original commentary and its tool ownership remain unchanged.
- Suppressed that fallback after cancellation, failure, unresolved denial, or any explicit final-answer phase, including an empty final answer. Message-start order selects the candidate, not snapshot arrival order.
- Separated reasoning identities and lifecycle from commentary. Late reasoning snapshots reconcile existing content without restarting active reasoning. Generic byte progress and tool-argument deltas are not reasoning text.
- Separated tool requests, execution attempts, input, output, reported changes, skill annotations, and terminal outcomes. Late output preserves the original tool owner and cannot restart execution.
- Preserved structured file-change evidence across sparse notifications. Failed patch output and read-only diffs do not become successful workspace-change evidence.
- Changed Copilot completion to root `session.idle`. Foreground `assistant.idle` and child idle do not finish the invocation.
- Added a separate session-operation scope for manual/background compaction rather than inventing an exchange.
- Separated provider observations from typed session/editor notifications. Host testing found private timeline events leaking onto the editor wire protocol. Removed those notification variants and used internal `LiveUpdate::StateChanged` reconciliation instead. The broker regression now serializes emitted notifications through the real wire validator.
- Bumped the private session format from 54 to 55. Current-format sessions reopen. Other versions remain hidden, without migration or deletion.

Contributor and user documentation was updated in:

- `nvim/contributing/forge/architecture/providers.md`
- `nvim/contributing/forge/architecture/harness.md`
- `nvim/contributing/forge/architecture/host-and-protocol.md`
- `nvim/docs/forge/commands.md`

## Verification already recorded

All results below predate the final timestamp-helper cleanup.

| Boundary | Recorded result |
| --- | --- |
| Integrated production library | `cargo check` passed. |
| Production lint | Clippy passed with only the two recorded baseline warnings. New enum-size warnings were fixed with typed boxed payloads, not suppressed. |
| Targeted Rust matrix | 78 tests passed, zero failed. No full workspace suite was run. |
| Public integration callers | The named `live_provider` integration target compiled. Ignored live tests were not executed. |
| Runtime executable | The development `forge` binary built successfully. |
| Real host/editor | The registered message-fold fixture passed at 80x36 and invoked the native-stream support fixture at 100x36. |
| Native-stream workflow | Three native turns, three tools, an empty parallel tool envelope, reasoning, commentary fallback, a following invocation, expansion, cursor/viewport preservation, and close/explicit resume passed. |
| Terminal MCP | Dedicated editors passed at 100x36 and 160x48. Both showed distinct commentary/response copies, correctly owned three-tool groups, no empty commentary headings, idle status, and an unchanged reading cursor during expansion. |
| Cleanup | Both owned terminal editors and hosts were collected, and their terminal sessions were destroyed. |

The native host fixture is:

`nvim/tests/forge/support/provider_stream_host.lua`

It runs through the existing registered entrypoint:

`nvim/tests/forge/harness_message_folds_host.lua`

No check registry or new test runner was added. The fixture uses explicit mock launch mode `provider-stream` and the real Copilot decoder, without authenticated provider requests.

## Remaining steps

Use the existing pinned Rust 1.94.0 toolchain, development profile, feature selection, and default artifact directory. Keep Cargo jobs serial. Allow 300 seconds for compilation/build commands, 30 seconds for formatting/whitespace checks, and 120 seconds for the host runner.

1. **Check the final cleanup and formatting.** Confirm the only implementation edit after the last passing verification is the timestamp forwarding-method removal. The full refactor also remains uncommitted. From `nvim/rust/forge`, check the affected file:

   ```console
   rustfmt +1.94.0 --edition 2024 --config skip_children=true --check crates/forge-harness/src/backend/copilot/event_decoder/stream.rs
   git diff --check
   ```

2. **Recheck production lint and types.** From the same Rust workspace, require exit status 0 and no new warnings:

   ```console
   cargo clippy --profile dev --locked -p forge-harness --lib --no-deps --message-format short
   ```

3. **Run the exact fallback regression first.** If compilation fails, correct the compiler error and retry this same test before starting other tests:

   ```console
   cargo test --profile dev --locked -p forge-harness --lib backend::copilot::event_decoder::tests::idle_synthesizes_latest_commentary_as_a_distinct_response_once -- --exact
   ```

4. **Verify the affected decoder and timing boundaries.** After the exact regression passes, run the related decoder and compaction cases:

   ```console
   cargo test --profile dev --locked -p forge-harness --lib -- backend::copilot::event_decoder:: backend::copilot::compaction::tests::
   ```

   The exact command for the previously passing 78-test matrix is preserved in `typed-contract-final-targeted-2.json`. Reuse passing results for unaffected boundaries. Do not expand to the full workspace merely to reconfirm them.

5. **Rebuild the runtime executable.** From `nvim/rust/forge`, require a recorded passing build:

   ```console
   cargo build --manifest-path Cargo.toml --profile dev --bin forge --locked
   ```

   The artifact is `nvim/rust/forge/target/debug/forge`. Existing editors retain their process-owned executable copies, so runtime verification must start a fresh test host.

6. **Repeat the registered host fixture against that build.** From the repository root, use the exact runner command and stdin request recorded in `typed-contract-host-final-2.json`. Select only `nvim/tests/forge/harness_message_folds_host.lua`. The fixture deadline is 90 seconds and the outer command deadline is 120 seconds. Require exit status 0 and `{"passed":1,"failed":0}`.

   The passing close/resume path uses `session_navigation.resume(session_id)` and refreshes the current presentation owner. A no-argument `open_harness()` attempt reused a stale initial snapshot and did not recreate the presentation. That separate path was not changed or claimed as verified.

7. **Finalize the handoff records.** Update the execution checkpoint in the session `plan.md`, which still contains stale in-progress text. Assemble the final validation summary from the existing logs and the new recheck results. Mark `empty-transcript-verify` and `provider-contract-docs` done only after the final checks and fresh-host run pass. No commit was requested.

If any compiled source changes during these steps, repeat affected checks and rebuild before using host-test results as proof.

## Existing failures and limits

Do not silently treat these as new regressions or fix them outside the requested scope:

- Clippy retains the existing `too_many_arguments` warning at `buffer/projection.rs:1675` and `nonminimal_bool` warning at `buffer/transcript.rs:352`.
- `broker::test::native_goal_turns_start_separate_exchanges_and_route_late_usage` still fails with `left: 0`, `right: 2`. The recorded original baseline has the identical assertion failure. It was not included in the passing 78-test matrix.
- The standalone SDK probe established observations for SDK 1.0.7, CLI 1.0.98, and `gpt-6.1-terra`, not a closed phase vocabulary for every Copilot model. It covered 36 executions, 18 scenarios, 38 prompts, and 44 tool attempts.
- No authenticated live-provider calls were made during implementation. Do not run ignored live tests without authorization.
- StyLua was unavailable on PATH and at the checked Mason path. No formatter was installed. The Lua fixtures were syntax-checked and executed through the real host runner.

## Evidence and source locations

The approved plan is stored at:

`~/.copilot/session-state/6b91bc77-dfd4-4ed0-995f-01a76a21b0c3/plan.md`

All evidence filenames below are relative to that session's `files/` directory. Command reports contain exact argument arrays, deadlines, exit statuses, durations, and log paths.

- `typed-contract-final-targeted-2.json` and `.log`: passing 78-test matrix.
- `typed-contract-wire-regression-2.json` and `.log`: broker event draining plus real editor-wire serialization.
- `typed-contract-fixture-lint.json` and `.log`: latest passing production lint before the cleanup.
- `typed-contract-public-callers.json` and `.log`: compilation of the ignored integration caller.
- `typed-contract-runtime-build-final-2.json` and `.log`: latest passing build before the cleanup.
- `typed-contract-host-final-2.json` and `.log`: latest passing registered host fixture.
- `typed-contract-format-complete.json`: formatting result for 80 changed Rust files. Whitespace was subsequently corrected and `git diff --check` passed.
- `typed-contract-terminal-verified-68070.json` and `typed-contract-terminal-verified-68501.json`: owned editor identities, mappings, and transcript state.
- `typed-contract-terminal-expanded-68070.json` and `typed-contract-terminal-expanded-68501.json`: expanded tool groups and preserved cursor positions.
- `typed-contract-terminal-cleanup-68070.json` and `typed-contract-terminal-cleanup-68501.json`: collected host/editor confirmation.
- `provider-contract-baseline-failures.json` and `provider-contract-baseline.log`: original baseline failures. The native-goal assertion appears around lines 1560-1565 of the log.
- `copilot-sdk-phase-probe/observed-contract.json` and `copilot-sdk-phase-probe/runs/`: direct SDK evidence.

The principal implementation files are `backend/contract.rs`, `backend/execution.rs`, `backend/message.rs`, `backend/tool_status.rs`, `backend/events.rs`, `backend/copilot/event_decoder/`, `backend/codex/normalization.rs`, `backend/acp/session.rs`, `broker/publication.rs`, `exchange/content.rs`, and `turn/execution.rs`, all under `nvim/rust/forge/crates/forge-harness/src/`.
