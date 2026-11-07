# Disabled slow tests

The Lua runner, `.forge/checks/lua-tests.nu`, kills any fixture that runs longer than five
seconds. A fixture that is correct but cannot finish inside that budget because of launch and
sequencing overhead stays out of the default selection. Each entry in `disabled_fixture_list`
carries the comment `exceeds the 5 s test limit; tracked in nvim/tests/forge/plans/disabled-slow-tests.md`,
and an explicit path still selects the fixture, though it fails when it runs past five seconds.

The durations below come from running each fixture alone, so they exclude the contention of a
parallel run. Every wait in these fixtures is a condition wait on real host work (Harness broker
startup, `git` subprocesses, check commands). None is a fixed sleep, so the time cannot be removed
without splitting the fixture.

| Fixture | Alone | Where the time goes |
| --- | --- | --- |
| `harness_automatic_checks.lua` | 13.7 s | Sequential check scenarios, each starting a Nushell check process through the host. |
| `harness_check_queue_host.lua` | 13.8 s | Sequential check queue scenarios against a real host. |
| `status_staging_host_flows.lua` | 12.1 s | Many staging flows, each running real `git` subprocesses. |
| `harness_tasks_host.lua` | 11.3 s | Sequential task lifecycle scenarios against a real host. |
| `plan_checks_host.lua` | 8.0 s | Plan check scenarios with real check commands. |
| `status_staging_admission_host.lua` | 7.9 s | Staging admission scenarios with real `git` subprocesses. |
| `harness_host.lua` | 7.4 s | Submission race scenarios, each launching its own broker. |
| `semantic_execution_host.lua` | 7.3 s | Semantic execution scenarios against a real host. |
| `harness_checks_host.lua` | 6.9 s | Check cancellation and shutdown scenarios with real processes. |
| `harness_plan_checks.lua` | 6.4 s | Plan check selection against a real host. |
| `plan_review_host.lua` | 6.0 s | Plan review scenarios against a real host. |
| `harness_check_activity_host.lua` | 5.9 s | Check activity scenarios with a compiled fixture program. |
| `harness_message_folds_host.lua` | 5.7 s | Message fold scenarios against a real host. |
| `status_staging_host_edges.lua` | 5.5 s | Staging edge cases with real `git` subprocesses. |
| `harness_replan_host.lua` | 5.4 s | Replan scenarios against a real host. |
| `harness_publication_restart_host.lua` | 4.7 s | Publication restart scenarios with two host launches; exceeds 5 s under any contention. |
| `harness_admission_host.lua` | 4.3 s | Admission scenarios against a real host; exceeds 5 s under any contention. |
| `github_api_examples.lua` | 3.5 s | Compiles two Rust fixture programs with `rustc` before running; exceeds 5 s under any contention. |
| `status_staging_source_changes.lua` | 3.8 s | Stages a 9 MiB file through real `git`; exceeds 5 s under any contention. |
| `plan_review_calls_host.lua` | 3.0 s | Plan review call scenarios against a real host; exceeds 5 s under contention. |
| `plan_review_definition_host.lua` | 2.5 s | Runs the call scenarios' definition subset against a real host; exceeds 5 s under contention. |
| `harness_approval_resume_host.lua` | 3.3 s | Approval resume scenarios against a real host; exceeds 5 s under contention. |

Re-enable a fixture by removing its entry once it is split into scenarios under five seconds, as
`plan_review_question_response_host.lua` is: the runner lists a fixture that declares
`{ name = "...", run = ... }` scenario rows once per scenario.

## Disabled Rust tests

Nextest, configured in `nvim/rust/forge/.config/nextest.toml`, terminates any test that runs past five seconds. Each test below carries `#[ignore = "exceeds the 5 s test limit; tracked in nvim/tests/forge/plans/disabled-slow-tests.md"]`, so the default run skips it and `cargo nextest run --run-ignored only` still selects it. Every one of them spends its time on real process or service work rather than a fixed sleep. Re-enabling one means replacing that work with a mock or splitting the test.

| File | Tests | Where the time goes |
| --- | --- | --- |
| `crates/forge-diff/src/syntax/declaration_formatting_tests.rs` | `typescript_declaration_corpus_preserves_meaning`, `large_files_preserve_meaning_across_repeated_passes` | Formats a large declaration corpus through tree-sitter parsing repeated across passes. |
| `crates/forge-git/src/revision/comparison.rs` | `conflicting_ranges_stop_without_changing_checkout_state` | Builds real repositories and runs a `git` subprocess for every staging, patch, or conversion step. |
| `crates/forge-git/src/writer/tests.rs` | `stage_unstage_and_discard_use_exact_targets_and_settlement`, `patch_stages_one_raw_hunk_and_unstages_it`, `patch_groups_batch_distinct_files_and_preserve_unselected_hunks`, `staged_hunk_discard_preserves_other_staged_and_unstaged_changes`, `staged_hunk_conflict_fails_preflight_without_modifying_index`, `parent_intent_stages_and_unstages_more_than_256_captured_paths`, `batch_preserves_per_target_actions_under_one_receipt`, `small_path_writes_batch_literal_paths_and_preserve_unselected_files`, `mixed_batch_groups_adjacent_actions_and_stops_after_failed_group`, `mixed_discard_batches_tracked_paths_and_deletes_only_selected_untracked_paths`, `source_selection_preserves_staged_text_or_restores_head_explicitly`, `head_discard_handles_staged_addition_and_deletion`, `patch_addition_uses_missing_source_and_round_trips`, `publish_branch_sets_missing_upstream_and_rejects_changed_head`, `compound_discard_reverses_overlap_and_preserves_unselected_changes`, `compound_discard_reports_completed_unstaged_step_before_staged_conflict`, `scoped_patches_preserve_excluded_bytes_for_each_write_direction`, `scoped_partial_creation_and_deletion_preserve_remaining_file_presence` | Builds real repositories and runs a `git` subprocess for every staging, patch, or conversion step. |
| `crates/forge-git/tests/conversion.rs` | `native_builtin_conversion_matches_git_object_identity` | Builds real repositories and runs a `git` subprocess for every staging, patch, or conversion step. |
| `crates/forge-github/tests/gh_client.rs` | `pending_submission_capture_and_preflight_reject_changed_membership_or_text`, `native_issue_assignee_deltas_preserve_unrelated_members_and_partial_results` | Drives the `gh` client fixture through many sequential request and preflight rounds. |
| `crates/forge-harness/src/broker/cancellation_test.rs` | `foreground_prefix_transfer_preserves_order_and_waits_for_last_publication` | Starts a full service and broker, then runs several sequential configuration or collection rounds. |
| `crates/forge-harness/src/service/tests/task_state.rs` | `planning_permission_restarts_preserve_no_progress_budget`, `planning_effort_restarts_preserve_no_progress_budget`, `goal_continuation_uses_durable_input_and_yields_to_user_queue_without_user_input`, `goal_continuation_uses_durable_input_and_yields_to_user_queue_with_user_input`, `accepted_inline_feedback_revises_before_execution_without_changing_policy_request_on`, `execute_collects_then_clear_preserves_receipt_transition`, `execute_collects_then_clear_preserves_receipt_alias`, `execute_collects_then_clear_preserves_receipt_review` | Starts a full service and broker, then runs several sequential configuration or collection rounds. |
| `crates/forge-status/src/tests.rs` | `staging_with_many_open_syntax_bodies_preserves_highlighting`, `stale_discard_and_unstage_reject_external_source_and_index_changes`, `observed_rename_rejects_recreated_index_origin`, `comparisons_reuse_native_bodies_and_reject_mutations`, `compound_discard_selection_supports_whole_and_mixed_hunks`, `compound_discard_selection_keeps_unselected_staged_and_unstaged_hunks`, `display_group_context_stages_nearby_raw_hunks_only`, `semantic_display_group_stages_and_unstages_only_its_raw_hunks`, `staging_hunks_in_reverse_order_keeps_unique_projection_ids`, `staging_one_raw_hunk_splits_shared_context_and_unstaging_merges_it_again`, `ignoring_staged_changes_unstages_and_persists_without_changing_worktree`, `ignoring_staged_changes_does_not_hide_a_failed_unstage`, `redundant_stage_and_unstage_are_successful_noops`, `forge_only_ignore_unignore_and_collected_stage_preserve_git_state`, `optimistic_hunks_accept_stage_stage_unstage_before_git_and_preserve_context`, `optimistic_failed_insertion_rebases_later_hunk_without_staging_failed_bytes`, `queued_hunks_reject_edit_outside_selection_and_allow_independent_whole_file`, `mixed_hunk_and_whole_file_selection_validates_each_target_policy`, `collapsed_file_counts_survive_pending_stage_and_unstage`, `collapsed_file_counts_survive_queued_stage_unstage_stage`, `status_full_round_trip_preserves_partial_and_complete_loaded_pages`, `public_collapsed_file_actions_preserve_excluded_changes_for_all_mutations`, `semantic_heading_actions_scope_shared_raw_hunks_and_preserve_presentation_after_staging`, `public_collapsed_actions_reject_parser_service_failure_without_including_hidden_bytes`, `mixed_raw_and_declaration_selection_preserves_each_hunks_scope` | Builds real repositories and runs `git` subprocesses for each staging, discard, and refresh step. |
