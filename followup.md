# Typed provider interface follow-up

The provider-interface follow-up is complete as of 2026-10-29. The rebuilt runtime passes the targeted Rust checks, fresh-host fixtures, and interactive terminal verification after the timestamp-helper cleanup. The two stash conflicts are resolved and their fixtures pass.

## Current state

`nvim/rust/forge/crates/forge-harness/src/backend/copilot/event_decoder/stream.rs` contains four direct `super::timestamp_ms` calls and no forwarding `CopilotEventDecoder::now_ms` method. This cleanup is already present in HEAD and has no remaining working-tree diff. Fresh checks and the development build cover the current source.

The stash resolutions preserve the upstream question-progress, response identity, late-answer, failure, retry, and wiped-buffer recovery coverage together with the stashed `toggle_visibility` command and recovery changes. Only the two resolved test files were staged by this follow-up. The pre-existing staged and untracked implementation work remains intact. No commit was created, no stash was dropped, and no edit was made to `nvim/lua/plugins/lsp.lua`.

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

## Completed verification

Fresh evidence is saved under `nvim/rust/forge/target/followup-verification/`. JSON reports contain exact commands, exit statuses, durations, and log paths. All commands below exited 0. No compiled source changed after the successful build.

Rust commands ran from `nvim/rust/forge` with Rust 1.94.0, the development profile, the lockfile, and one Cargo job. Validation ran in check, test, read-only format, lint, build order.

| Verification | Result | Duration | Evidence |
| --- | --- | --- | --- |
| Production `cargo check` | Passed | 35.59 s | `rust.json`, `rust-0.log` |
| Exact commentary-fallback regression | 1 passed | 163.38 s | `rust.json`, `rust-1.log` |
| Related decoder and compaction tests | 28 passed, 0 failed, including the exact regression | 0.65 s | `rust.json`, `rust-2.log` |
| Read-only rustfmt for `stream.rs` | Passed | 0.04 s | `rust.json`, `rust-3.log` |
| Working-tree and staged whitespace checks | Passed | 0.03 s and 0.06 s | `rust.json`, `rust-4.log`, `rust-5.log` |
| Production Clippy | Passed with no warnings | 45.28 s | `rust.json`, `rust-6.log` |
| Development `forge` executable | Built successfully | 122.20 s | `rust.json`, `rust-7.log` |
| Public `live_provider` integration target | Compiled, no live tests executed | 34.95 s | `public-callers.json` |
| Registered message-fold host fixture | Passed at 80×36, including embedded provider-stream fixture at 100×36 | 10.76 s | `host.json`, `host.log` |
| Real-host PlanReview question fixture | All 18 scenarios passed | 18.10 s | `plan-question-host.json` |
| Resolved stash fixtures | Syntax check, questions, and recovery passed | Recorded per command | `stash-conflicts.json` |

The Rust commands were:

```console
cargo +1.94.0 check --profile dev --locked -p forge-harness --lib --jobs 1
cargo +1.94.0 test --profile dev --locked -p forge-harness --lib --jobs 1 backend::copilot::event_decoder::tests::idle_synthesizes_latest_commentary_as_a_distinct_response_once -- --exact
cargo +1.94.0 test --profile dev --locked -p forge-harness --lib --jobs 1 -- backend::copilot::event_decoder:: backend::copilot::compaction::tests::
rustfmt +1.94.0 --edition 2024 --config skip_children=true --check crates/forge-harness/src/backend/copilot/event_decoder/stream.rs
git diff --check
git diff --cached --check
cargo +1.94.0 clippy --profile dev --locked -p forge-harness --lib --jobs 1 --no-deps --message-format short
cargo +1.94.0 build --manifest-path Cargo.toml --profile dev --bin forge --locked --jobs 1
cargo +1.94.0 check --profile dev --locked -p forge-harness --test live_provider --jobs 1
```

The artifact is `nvim/rust/forge/target/debug/forge.exe`. The one build had already started through the terminal wrapper before the user changed the workflow. Future builds run outside Terminal MCP only when source changes require rebuilding. The initial Rust commands had the previously requested 300-second compilation and 30-second formatting deadlines. After the user prohibited command timeouts, subsequent commands ran without them. Existing fixture-internal bounded waits were not changed.

The fresh-host commands ran from the repository root:

```console
nvim --headless -i NONE -u NONE --cmd "set rtp^=D:/config/nvim" --cmd "lua vim.loader.enable(false)" -l nvim/tests/forge/harness_message_folds_host.lua
nvim --headless -i NONE -u NONE --cmd "set rtp^=D:/config/nvim" -l nvim/tests/forge/plan_review_question_response_host.lua
```

The registered host fixture invokes `nvim/tests/forge/support/provider_stream_host.lua` through an embedded editor. Its success messages are consumed by RPC, so the report uses the parent success marker and exit status. No check registry or runner was changed.

Dedicated Terminal MCP editors at 100×36 and 160×48 used that same native provider-stream fixture with the rebuilt executable and explicit mock launch mode `provider-stream`. Both completed two invocations and close/explicit resume, retained distinct commentary and synthetic responses, rejected duplicate block identities and empty commentary headings, and reached idle status. After inspecting the effective Normal-mode `<Tab>` mapping, two Tab presses expanded the commentary and its three-tool group. The cursor stayed on the selected heading. Screenshots and transcript-node records confirm all three tools remain attached to the original commentary group.

The 100-column launch initially lacked plugin paths. Adding the existing `D:/.local/share/nvim-data` plugin and parser paths to that isolated editor fixed setup. A 160-column launch initially passed literal single quotes to the Windows command interpreter. Correcting command quoting fixed startup. Neither issue required repository changes. The corrected startup command uses a distinct pipe per editor:

```console
nvim -u NONE -i NONE --listen \\.\pipe\forge-provider-followup-160 --cmd "set rtp^=D:/config/nvim" --cmd "set rtp+=D:/.local/share/nvim-data/lazy/render-markdown.nvim,D:/.local/share/nvim-data/lazy/nvim-treesitter,D:/.local/share/nvim-data/site" -c "lua vim.g.forge_provider_stream_grid=true; vim.g.forge_provider_stream_keep_open=true; dofile('nvim/tests/forge/support/provider_stream_host.lua')"
```

Terminal records are `terminal-100-*.json` and `terminal-160-*.json`, including identities, expanded nodes, screens, and cleanup. Both owned hosts were collected, both owned editors exited, and their dedicated terminal sessions were destroyed. The user's editor was not controlled or closed.

## Execution checkpoint

- [x] Resolve both stash conflicts and verify the resolved fixtures.
- [x] Verify the timestamp-helper cleanup, production types, focused tests, formatting, and Clippy.
- [x] Rebuild once and verify fresh hosts against the new executable.
- [x] `empty-transcript-verify`: complete, including both terminal dimensions, expansion, cursor preservation, and explicit close/resume.
- [x] `provider-contract-docs`: complete. Existing provider, Harness, host/protocol, and command documentation remains in place. This file records current validation and limits.
- [x] Preserve the stash and unrelated work. Record the final whitespace and unmerged-index audit in `final-audit.json`.

The original `~/.copilot/session-state/6b91bc77-dfd4-4ed0-995f-01a76a21b0c3/plan.md` and its evidence directory could not be found. This file serves as the available completion checkpoint. The missing historical plan was not recreated.

## Existing failures and limits

- The previous handoff reported 78 passing targeted Rust tests. This follow-up reran the affected decoder and compaction boundary, not that entire historical matrix or the full workspace.
- The previous Clippy warnings at `buffer/projection.rs:1675` and `buffer/transcript.rs:352` did not reproduce in the fresh lint run.
- The previously recorded `broker::test::native_goal_turns_start_separate_exchanges_and_route_late_usage` failure (`left: 0`, `right: 2`) remains outside this task. It was not rerun or claimed fixed.
- The standalone SDK observations remain limited to SDK 1.0.7, CLI 1.0.98, and `gpt-6.1-terra`. They do not establish a closed phase vocabulary for every Copilot model.
- No authenticated live-provider calls or ignored live tests were run. The public integration target was compiled only.
- Close/resume uses `session_navigation.resume(session_id)`. No-argument `open_harness()` after closing was not changed or claimed verified.
- StyLua was previously unavailable and no formatter was installed. The resolved Lua fixtures passed syntax checks and execution.

## Historical evidence and source locations


The previous handoff referenced this approved plan:

`~/.copilot/session-state/6b91bc77-dfd4-4ed0-995f-01a76a21b0c3/plan.md`

That session directory is unavailable in this checkout and the checked user profile. Its plan and logs were not recreated or modified. The filenames below preserve the historical references under its `files/` directory. Their previously reported results were not independently reread during this follow-up.

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
