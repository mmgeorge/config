# Verification by area

Different evidence proves different things: source-only syntax analysis, a mock LSP response, and a real project server test are not interchangeable, so report them separately. This reference lists what to run for each area of Forge, and what each test proves. It assumes you have read [Building and testing](building-and-testing.md) and [Editor tests](editor-tests.md).

Run Rust filters as `cargo nextest run --cargo-profile dev --locked -p <package> --lib <filter>` from `nvim/rust/forge`. Rebuild `forge` before running host fixtures. For a manual check, record the exact UI action and the result you observed.

## Summary

| Area | Rust filters (`forge-harness` unless noted) | Editor fixtures |
| --- | --- | --- |
| Project checks | `checks::` | None |
| Plan review | `plan::` | `document::document`, `checks::plan_check_selection`, `workspace_definition::plan_workspace_definition` |
| Provider lifecycle | Exact selectors and package ownership below | `harness::prompt_lifecycle` |
| Input and queues | `broker::test::input::` | None |
| Transcript folds | None | `harness::message_folds` |
| Agent LSP tools | `tools::lsp::request::tests::every_semantic_tool_accepts_lua_selection`, `tools::lsp::rename::tests::rename_tools_accept_lua_project_selection`, `tools::lsp::render::tests::workspace_listing_uses_paths_and_language_keys` | `workspace::lua_workspace_uses_configured_root_without_starting_server`, `root::marker_priority_matches_editor_configuration`, `semantic::mismatched_language_fails_before_document_requests`, `lsp::lua_project_selection_reaches_only_the_configured_server` |
| Background generation | `compaction` and `commit_message::` in `forge` | `harness::background`, `commit_settings` |
| Configuration controls | None | `picker::picker`, `settings::settings`, `commit_settings` |
| Provider registry | None | `client::provider_registry_config_reload` |
| Harness tab labels | None | `harness::controller_native` |

## Project checks

Check resolution affects declared inputs, concrete commands, and the evidence the automatic gate accepts. The `checks::` tests cover durable tool receipts and revoked configuration callbacks. Check command execution has no Rust test, because `CheckSupervisor` launches every command as a real process; verify a change to command execution manually and record the command and result.

See [Extending project checks](extending-project-checks.md) when changing input binding or repository callbacks.

## Rust fixture setup

Broker tests take their workspace from `broker::test_support`. `repository()` returns a plain directory, which the broker opens without Git checkpoints, so most tests launch no Git process. Only tests that exercise checkpoints, rollback, or workspace changes use `git_repository()`. Under nextest, the `fixture-root` setup script ([`.config/fixture_root.nu`](../../rust/forge/.config/fixture_root.nu)) builds that seeded repository once per run and exports its path, so each test copies it instead of running `git init` and `git commit`. Setup scripts are an experimental nextest feature, enabled in `.config/nextest.toml`. Outside nextest, `git_repository()` builds the repository itself.

The same script gives every nextest run its own temporary-directory root, `<OS temp>/forge-tests/<first eight characters of the run ID>/`, and exports it as `TMP`, `TEMP` and `TMPDIR`, so every `tempfile` fixture and `std::env::temp_dir()` path in a test lands there. `tempfile::TempDir` ignores deletion errors, and Windows refuses to delete a file that is still open, so a fixture dropped before its SQLite stores and lock files close leaves a remnant. The per-run root keeps those remnants out of the shared temporary directory, and the next run removes run directories unmodified for a day. The root stays short because checkpoint recovery files nest two 64-character digests and Windows still limits many paths to 260 characters. Declare a fixture's `TempDir` after every store, controller, or provider that holds files inside it, because Rust drops fields in declaration order.

`nvim/rust/forge/clippy.toml` makes `rust-lint` reject three calls: a direct `rusqlite::Connection` open, `SqliteStore::open` or `SqliteStore::read_only`, and `std::thread::sleep`. History goes through `HistoryWriter` and `HistoryReader`, and a thread that must wait uses a channel, a condition variable, or the Tokio clock. A production call that cannot comply carries `#[expect(clippy::disallowed_methods, reason = "..")]` stating why; test modules, examples, and stdio fixtures are exempt because they build storage through handles of their own. A Harness test that needs a second connection to the history database, to install a failing trigger or to seed and observe rows, opens it through `storage::test::raw_connection`.

## Provider lifecycle

Compile the affected package first, with a 300-second compile deadline. Select the exact regression for the changed boundary; source files and broad module filters are not substitutes for a named regression. These selectors name current test functions and describe their coverage, not recorded passing results.

- `forge-providers`: `codex::provider::continuation_tests::initialization_accepts_newer_versions_and_reports_invalid_versions` checks minimum-only version validation, newer CLI support, and repeated connection cleanup.
- `forge-providers`: `codex::provider::continuation_tests::cancellation_before_start_acknowledgment_interrupts_after_admission` checks that temporary Background input reaches native dispatch with read-only access and denied approvals, then cancels the exact invocation.
- `forge-harness`: `provider::text_generation::isolated::tests::dropped_waiter_and_validator_rejection_collect_temporary_sessions` checks retained cleanup and final-answer selection when native activity accompanies the response.
- `forge-harness`: `provider::text_generation::isolated::tests::validator_rejection_preserves_cleanup_failure` retains both causes when validation and cleanup fail; `shutdown_cancels_and_collects_temporary_session_once` checks cancellation and repeated shutdown.
- `forge-harness`: `service::tests::isolated_generation_uses_configured_model_without_a_conversation` checks the caller's model, reasoning, and speed selection through the shared generation lifecycle.

- `forge-providers`: `turn_runner::tests::cleanup_failure_retains_native_result_and_fences_without_completion` checks unavailable publication after failed collection without ordinary completion.
- `forge-provider-api`: `turn::tests::only_normal_completion_permits_automatic_continuation` checks normal completion versus token limit, request limit, refusal, failure, and interruption.

- `forge-harness`: `storage::agent_policy::tests::only_matching_user_authorization_clears_latest_durable_policy` checks that stale reservations and automatic work cannot clear a stopped managed child's durable authorization policy.
- `forge-harness`: `storage::agent_policy::tests::accepted_parent_report_acknowledgement_is_atomic_and_preserves_child_failure` checks actual user-report acknowledgement without clearing a stale policy or replacing the child failure.

For example, from `nvim/rust/forge`:

```nu
cargo check --profile dev --locked -p forge-providers --lib
cargo nextest run --cargo-profile dev --locked -p forge-providers --lib -E 'test(=turn_runner::tests::cleanup_failure_retains_native_result_and_fences_without_completion)'
```

Native mappings live under `codex::`, `copilot::`, and `acp::` in `forge-providers`; the former `backend::*` selectors do not select those tests. Use `rg -n "fn <regression>" crates/forge-providers/src` to confirm a native fixture's current name, then run that exact test in its owning package. Harness callback/executor tests remain in `forge-harness`. Real-host fixtures require a rebuilt executable and a fresh host with the current disposable-state version; a source or mock test does not establish editor integration or coexistence verification.

## Configuration controls

Run the picker and settings fixtures through the Lua runner after changing field layout, focus, or configuration actions, then inspect `/config` in a fresh host at wide and narrow terminal sizes.
