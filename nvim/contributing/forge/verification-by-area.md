# Verification by area

Different evidence proves different things: source-only syntax analysis, a mock LSP response, and a real project server test are not interchangeable, so report them separately. This reference lists what to run for each area of Forge, and what each test proves. It assumes you have read [Building and testing](building-and-testing.md) and [Editor tests](editor-tests.md).

Run Rust filters as `cargo nextest run --cargo-profile dev --locked -p <package> --lib <filter>` from `nvim/rust/forge`. Rebuild `forge` before running host fixtures. For a manual check, record the exact UI action and the result you observed.

## Summary

| Area | Rust filters (`forge-harness` unless noted) | Editor fixtures |
| --- | --- | --- |
| Project checks | `checks::`, `plan::design_review::tests`, `selected_plan_checks_are_versioned` | None |
| Plan review | `plan::` | `document::document`, `checks::plan_check_selection`, `workspace_definition::plan_workspace_definition` |
| Provider lifecycle | Exact selectors and package ownership below | `harness::prompt_lifecycle` |
| Input and queues | `broker::input_test::deferred_steering_cancellation_preserves_acceptance_boundary` | None |
| Transcript folds | None | `harness::message_folds` |
| Agent LSP tools | None | `lsp/semantic.test.lua::lsp_query`, `startup::startup`, `startup::lazy_startup`, `semantic::workspace` |
| Background generation | `background::tests`, `compaction`, and `commit_message::` in `forge` | `harness::background`, `commit_settings` |
| Configuration controls | None | `picker::picker`, `settings::settings`, `commit_settings` |
| Provider registry | None | `client::provider_registry_config_reload` |
| Harness tab labels | None | `harness::controller_native` |

## Project checks

Check resolution affects declared inputs, concrete commands, and the evidence the automatic gate accepts. The `checks::` tests cover typed inputs, literal argv substitution, callback receipts, evidence identity, cancellation, and prerequisite failures. These Rust tests own check behavior; no editor fixture repeats it.

See [Extending project checks](extending-project-checks.md) when changing input binding or repository callbacks.

## Provider lifecycle

Compile the affected package first, with a 300-second compile deadline. Select the exact regression for the changed boundary; source files and broad module filters are not substitutes for a named regression. These selectors name current test functions and describe their coverage, not recorded passing results.

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
