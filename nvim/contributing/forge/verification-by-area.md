# Verification by area

Different evidence proves different things: source-only syntax analysis, a mock LSP response, and a real project server test are not interchangeable, so report them separately. This reference lists what to run for each area of Forge, and what each test proves. It assumes you have read [Building and testing](building-and-testing.md) and [Editor tests](editor-tests.md).

Run Rust filters as `cargo nextest run --cargo-profile dev --locked -p <package> --lib <filter>` from `nvim/rust/forge`. Rebuild `forge` before running host fixtures. For a manual check, record the exact UI action and the result you observed.

## Summary

| Area | Rust filters (`forge-harness` unless noted) | Editor fixtures |
| --- | --- | --- |
| Project checks | `checks::`, `plan::design_review::tests`, `selected_plan_checks_are_versioned` | `harness_automatic_checks`, `harness_check_queue_host`, `harness_check_repair_resume_host` |
| Plan review | `plan::` | `plan_review_document`, `plan_check_selection`, `plan_checks_host`, `plan_review_calls_host`, `plan_workspace_definition` |
| Provider lifecycle | Exact selectors and package ownership below | `harness_prompt_lifecycle`, `harness_configuration_host` |
| Input and queues | `broker::input_test::deferred_steering_cancellation_preserves_acceptance_boundary` | `harness_host` |
| Transcript folds | None | `harness_message_folds_host` |
| Agent LSP tools | None | `lsp_query`, `lsp_startup`, `lsp_lazy_startup`, `lsp_workspace` |
| Background generation | `background::tests`, `compaction`, and `commit_message::` in `forge` | `background_host`, `commit_settings_host` |
| Configuration controls | None | `picker`, `harness_settings`, `commit_settings_host` |
| Provider registry | None | `provider_registry_config_reload` |
| Harness tab labels | None | `harness_controller_native` |

## Project checks

Check resolution affects declared inputs, concrete commands, and the evidence the automatic gate accepts. The `checks::` tests cover typed inputs, literal argv substitution, callback receipts, evidence identity, cancellation, and prerequisite failures. They do not replace running the service through a real host.

The host fixtures use a temporary workspace, and each proves a different boundary:

| Fixture | Proves |
| --- | --- |
| `harness_automatic_checks` | The completion gate records one executed failure, one reused failure, a repair turn that stops without progress, and multi-check execution. |
| `harness_check_queue_host` | Both timing choices, persistence, and validation after the final queued item is deleted. |
| `harness_check_repair_resume_host` | A permission change that resumes an exchange keeps one exchange through its repair attempt. |

See [Extending project checks](extending-project-checks.md) when changing input binding or repository callbacks.

## Plan rendering

Build `forge` and the `plan_navigation_fixture` example, then run `plan_review_calls_host.lua` with a setting that selects what to verify. `forge_test_plan_behavior` verifies `Algorithm` and `Change` blocks and collapsed signatures. `forge_test_plan_flows` verifies flow rendering, symbol highlighting, and definition jumps.

```nu
nu .forge/checks/lua-tests.nu --lua "vim.g.forge_test_plan_behavior = true" nvim/tests/forge/plan_review_calls_host.lua
```

## Language navigation

`plan_navigation_languages_host.lua` starts a mock Harness session and one real language server against a tiny copied project, then verifies definitions through PlanReview, including return navigation and that no extra server starts. Build its generator first:

```nu
cargo build --profile dev --locked -p forge-harness --example plan_navigation_fixture
```

Pass these globals through the Lua runner's `--lua` setup, and repeat with `'rust'` and `'lua'`:

```nu
nu .forge/checks/lua-tests.nu --lua "vim.g.forge_test_language = 'typescript'; local binary = require('forge.builder').binary_path(); vim.g.forge_test_executable = binary; vim.g.forge_test_fixture_generator = vim.fs.dirname(binary) .. '/examples/plan_navigation_fixture' .. (vim.fn.has('win32') == 1 and '.exe' or ''); vim.g.forge_test_tsgo = vim.fn.exepath('tsgo')" nvim/tests/forge/plan_navigation_languages_host.lua
```

Set `forge_test_rust_analyzer` when rustup cannot find the analyzer. The fixture uses temporary directories by default. Set `forge_test_scratch` only to keep a failed workspace, and delete it afterward.

Rust fixture manifests are stored as `Cargo.toml.fixture` so the checked-in test
sources do not become live Cargo workspaces. The shared fixture copier materializes
them as `Cargo.toml` only in the temporary test directory and skips build output.

## Provider lifecycle

Compile the affected package first, with a 300-second compile deadline. Select the exact regression for the changed boundary; source files and broad module filters are not substitutes for a named regression. These selectors name current test functions and describe their coverage, not recorded passing results.

- `forge-providers`: `turn_runner::tests::dropped_waiter_retains_native_outcome_and_publishes_once_after_collection` checks retained work, one native outcome, collection ordering, and one terminal publication.
- `forge-providers`: `turn_runner::tests::cleanup_failure_retains_native_result_and_fences_without_completion` checks unavailable publication after failed collection without ordinary completion.
- `forge-providers`: `turn_runner::tests::prestart_cancellation_collects_without_manufacturing_a_turn` checks cancellation before native start.
- `forge-providers`: `mock::tests::automatic_settlement_and_exact_cancel_preserve_successor` checks canonical completion and interruption of the exact original turn.
- `forge-providers`: `mock::tests::grouped_prompt_completion_covers_recovered_and_current_attempts` checks original input-attempt coverage across grouped acceptance.
- `forge-provider-api`: `turn::tests::only_normal_completion_permits_automatic_continuation` checks normal completion versus token limit, request limit, refusal, failure, and interruption.
- `forge-provider-api`: `turn_usage::tests::report_merge_is_idempotent_complementary_and_atomic` checks identified partial accounting and conflicting replay.
- `forge-provider-api`: `compaction::tests::exact_terminal_metadata_refines_unknown_counts_without_retargeting_or_overwriting` checks exact operation/thread metadata refinement while preserving known counts.
- `forge-harness`: `turn::tool::tests::tool_final_snapshots_and_refinements_are_atomic_and_replay_safe` checks descriptor refinements, final output replacement, reload, and conflicting final replay.
- `forge-harness`: `buffer::projection::tests::native_user_item_retains_its_sender_and_original_identity` checks native user-item history and sender rendering.

- `forge-harness`: `agent::observed::tests::child_items_and_completed_turn_retain_original_ownership_after_summary_finish` checks independent native child ownership, partial content, and its own collected terminal result.
- `forge-harness`: `agent::observed::tests::summary_only_nested_agents_keep_explicit_hierarchy_and_reject_cycles` checks native ancestry without invented managed tasks or child turns.
- `forge-harness`: `storage::observed_agent::tests::unavailable_binding_preserves_native_uncertainty_and_does_not_touch_replacement` checks persisted observation loss without replacing native lifecycle evidence or touching another generation.
- `forge-harness`: `storage::agent_policy::tests::only_matching_user_authorization_clears_latest_durable_policy` checks that stale reservations and automatic work cannot clear a stopped managed child's durable authorization policy.
- `forge-harness`: `storage::agent_policy::tests::stopped_task_pointer_rejects_unknown_and_foreign_ownership` checks retained stopped-task pointers against persisted ownership.
- `forge-harness`: `storage::agent_policy::tests::accepted_parent_report_acknowledgement_is_atomic_and_preserves_child_failure` checks actual user-report acknowledgement without clearing a stale policy or replacing the child failure.

For example, from `nvim/rust/forge`:

```nu
cargo check --profile dev --locked -p forge-providers --lib
cargo nextest run --cargo-profile dev --locked -p forge-providers --lib -E 'test(=turn_runner::tests::cleanup_failure_retains_native_result_and_fences_without_completion)'
```

Native mappings live under `codex::`, `copilot::`, and `acp::` in `forge-providers`; the former `backend::*` selectors do not select those tests. Use `rg -n "fn <regression>" crates/forge-providers/src` to confirm a native fixture's current name, then run that exact test in its owning package. Harness callback/executor tests remain in `forge-harness`. Real-host fixtures require a rebuilt executable and a fresh host with the current disposable-state version; a source or mock test does not establish editor integration or coexistence verification.

## Configuration controls

Run the picker and settings fixtures through the Lua runner after changing field layout, focus, or configuration actions, then inspect `/config` in a fresh host at wide and narrow terminal sizes.
