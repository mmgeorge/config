# The single runner for Forge Lua tests. Each fixture runs in a fresh headless Neovim, in
# parallel with bounded concurrency, and any fixture still running after five seconds has its
# whole process tree killed and fails as a timeout.
#
#   nu .forge/checks/lua-tests.nu                         every fixture
#   nu .forge/checks/lua-tests.nu nvim/tests/forge/x.lua  exact fixtures
#   nu .forge/checks/lua-tests.nu --suite walkthrough     one feature suite
#   nu .forge/checks/lua-tests.nu --affected <paths>      suites covering changed source paths
#   nu .forge/checks/lua-tests.nu --lua "<setup>" <path>  parameterized fixtures, setup run first
#   nu --stdin .forge/checks/lua-tests.nu check <suite>   Harness check callback
const repository_root = path self | path dirname | path dirname | path dirname
const runner_path = path self
const per_test_timeout = 5sec
const startup_argument_list = [
  "--headless" "-i" "NONE" "-u" "NONE" "--cmd" "set rtp^=./nvim" "--cmd" "lua vim.loader.enable(false)"
]

def fixture_registry [] {
  {
    "shared": ["nvim/tests/smart_select.lua" "nvim/tests/git_diff_compact.lua" "nvim/tests/forge/block_sequence.lua" "nvim/tests/forge/forge_completion.lua" "nvim/tests/forge/permissions.lua" "nvim/tests/forge/notifications_adapter.lua" "nvim/tests/forge/mutation_coordinator.lua" "nvim/tests/forge/quoted_diff_paths.lua"]
    "rendering": ["nvim/tests/forge/local_diff_native.lua"]
    "github": ["nvim/tests/github_repo_cache.lua" "nvim/tests/github_open_pr.lua" "nvim/tests/github_issue_index.lua" "nvim/tests/forge/github_review_draft.lua" "nvim/tests/forge/github_mutation.lua" "nvim/tests/forge/github_metadata.lua"]
    "buffer": ["nvim/tests/forge/buffer_transfer.lua" "nvim/tests/forge/buffer_replica.lua" "nvim/tests/forge/buffer_lifecycle.lua" "nvim/tests/forge/buffer_view.lua" "nvim/tests/forge/cooperative_snapshot.lua" "nvim/tests/forge/fold_failure_recovery.lua" "nvim/tests/forge/source_overlay.lua" "nvim/tests/forge/source_document_stale.lua" "nvim/tests/forge/row_prefix.lua"]
    "status-git": ["nvim/tests/forge/ai_commit.lua" "nvim/tests/forge/commit_prompt_preview.lua" "nvim/tests/forge/commit_parent_exit.lua" "nvim/tests/forge/file_revision.lua" "nvim/tests/forge/index_mutation_real_git.lua" "nvim/tests/forge/native_git_write.lua" "nvim/tests/forge/status_fold_boundaries.lua" "nvim/tests/forge/status_document_events.lua" "nvim/tests/forge/status_context.lua" "nvim/tests/forge/status_body_recovery.lua" "nvim/tests/forge/status_body_ordering.lua" "nvim/tests/forge/revision_commands_real.lua" "nvim/tests/forge/revision_commands.lua" "nvim/tests/forge/status_recent_commits.lua" "nvim/tests/forge/status_pr_cache.lua" "nvim/tests/forge/status_optimistic.lua" "nvim/tests/forge/status_open_lifecycle.lua" "nvim/tests/forge/status_native.lua" "nvim/tests/forge/status_header_width.lua" "nvim/tests/forge/status_gutter_relocation.lua" "nvim/tests/forge/status_semantic.lua" "nvim/tests/forge/status_remote_progress.lua" "nvim/tests/forge/status_refresh.lua"]
    "mock-integration": ["nvim/tests/forge/harness_admission_host.lua" "nvim/tests/forge/harness_provider_checklist_host.lua" "nvim/tests/forge/background_host.lua" "nvim/tests/forge/client_executable_copy.lua" "nvim/tests/forge/client_callback_isolation.lua" "nvim/tests/forge/commit_host.lua" "nvim/tests/forge/commit_large_output_host.lua" "nvim/tests/forge/commit_status_reuse.lua" "nvim/tests/forge/commit_settings_host.lua" "nvim/tests/forge/forge_client.lua" "nvim/tests/forge/harness_control_responsiveness.lua" "nvim/tests/forge/harness_checks_host.lua" "nvim/tests/forge/harness_check_activity_host.lua" "nvim/tests/forge/harness_checkpoint_host.lua" "nvim/tests/forge/harness_automatic_checks.lua" "nvim/tests/forge/harness_attachments_host.lua" "nvim/tests/forge/harness_approval_resume_host.lua" "nvim/rust/forge/crates/forge-providers/tests/fixtures/submission_host.lua" "nvim/tests/forge/harness_message_folds_host.lua" "nvim/tests/forge/harness_interrupt_host.lua" "nvim/tests/forge/harness_input_timeout.lua" "nvim/tests/forge/harness_host.lua" "nvim/tests/forge/harness_tasks_host.lua" "nvim/tests/forge/walkthrough_host.lua" "nvim/tests/forge/harness_replan_host.lua" "nvim/tests/forge/language_syntax_host.lua" "nvim/tests/forge/plan_review_host.lua" "nvim/tests/forge/harness_publication_restart_host.lua" "nvim/tests/forge/markdown_viewport.lua" "nvim/tests/forge/plan_review_calls_host.lua" "nvim/tests/forge/plan_checks_host.lua" "nvim/tests/forge/receive_initialize_host.lua" "nvim/tests/forge/pr_context_host.lua" "nvim/tests/forge/source_host.lua" "nvim/tests/forge/semantic_execution_host.lua" "nvim/tests/forge/status_recent_commits_host.lua" "nvim/tests/forge/status_push_host.lua" "nvim/tests/forge/status_popup_host.lua" "nvim/tests/forge/status_ignore_staged_host.lua" "nvim/tests/forge/status_host.lua" "nvim/tests/forge/status_staging_host_flows.lua" "nvim/tests/forge/status_staging_host_edges.lua" "nvim/tests/forge/status_virtual_hunk_merge.lua" "nvim/tests/forge/status_untracked_optimistic.lua" "nvim/tests/forge/status_staging_source_changes.lua" "nvim/tests/forge/status_word_wrap_host.lua" "nvim/tests/forge/empty_diff_cursor.lua" "nvim/tests/forge/harness_plan_checks.lua" "nvim/tests/forge/harness_new.lua" "nvim/tests/forge/plan_review_question_response_host.lua"]
    "sessions": ["nvim/tests/forge/harness_session_geometry.lua" "nvim/tests/forge/harness_controller_native.lua" "nvim/tests/forge/harness_continuation_sync.lua" "nvim/tests/forge/harness_mode_restart.lua" "nvim/tests/forge/harness_file_paging.lua" "nvim/tests/forge/harness_snapshot.lua" "nvim/tests/forge/harness_session_name.lua" "nvim/tests/forge/harness_receive.lua" "nvim/tests/forge/harness_workspace.lua" "nvim/tests/forge/harness_transport_failure.lua" "nvim/tests/forge/session_picker.lua" "nvim/tests/forge/harness_session_picker_tabs.lua" "nvim/tests/forge/subagent_profiles.lua"]
    "controls": ["nvim/tests/forge/popup_parity.lua" "nvim/tests/forge/picker.lua"]
    "composer": ["nvim/tests/forge/harness_input_state.lua" "nvim/tests/forge/harness_retracted_input.lua" "nvim/tests/forge/composer_interleave.lua" "nvim/tests/forge/harness_child_steering.lua" "nvim/tests/forge/harness_attachments.lua" "nvim/tests/forge/harness_submit_steering.lua" "nvim/tests/forge/harness_queued_model.lua" "nvim/tests/forge/harness_prompt_lifecycle.lua"]
    "review-documents": ["nvim/tests/forge/draft_source.lua" "nvim/tests/forge/draft_comment_threads.lua" "nvim/tests/forge/issue_snapshot_recovery.lua" "nvim/tests/forge/issue_snapshot.lua" "nvim/tests/forge/issue_rebind.lua" "nvim/tests/forge/issue_document.lua" "nvim/tests/forge/review_comments.lua" "nvim/tests/forge/review_capture.lua" "nvim/tests/forge/review_rebind.lua" "nvim/tests/forge/review_operation_queue.lua" "nvim/tests/forge/review_editing.lua" "nvim/tests/forge/review_document.lua"]
    "editing": ["nvim/tests/forge/document_host_generation.lua" "nvim/tests/forge/document_commands.lua" "nvim/tests/forge/editable_ordering.lua" "nvim/tests/forge/editable_native.lua" "nvim/tests/forge/effects_native.lua" "nvim/tests/forge/mixed_edit_patch.lua"]
    "timeline-layout": ["nvim/tests/forge/harness_markdown_layout.lua" "nvim/tests/forge/harness_gutter_selection.lua" "nvim/tests/forge/harness_fold_lifecycle.lua" "nvim/tests/forge/timeline_status.lua"]
    "providers": ["nvim/tests/forge/provider_registry_config_reload.lua" "nvim/tests/forge/harness_effort_selection.lua" "nvim/tests/forge/harness_provider_switch.lua" "nvim/tests/forge/harness_model_pins.lua" "nvim/tests/forge/harness_settings.lua" "nvim/tests/forge/harness_provider_destination.lua" "nvim/tests/forge/harness_provider_catalog.lua"]
    "permissions": ["nvim/tests/forge/harness_approval.lua" "nvim/tests/forge/harness_access.lua"]
    "timeline-stream": ["nvim/tests/forge/harness_presentation.lua" "nvim/tests/forge/harness_status_view.lua" "nvim/tests/forge/harness_sections.lua" "nvim/tests/forge/harness_recap.lua" "nvim/tests/forge/harness_publication.lua" "nvim/tests/forge/harness_update_scheduling.lua" "nvim/tests/forge/harness_transcript_follow.lua" "nvim/tests/forge/harness_timeline_position.lua"]
    "plan-editor": ["nvim/tests/forge/plan_review_drafts.lua" "nvim/tests/forge/plan_review_document.lua" "nvim/tests/forge/plan_review_delivery.lua" "nvim/tests/forge/plan_review_controller.lua" "nvim/tests/forge/plan_optimistic_edit.lua" "nvim/tests/forge/plan_review_workspace_symbol.lua" "nvim/tests/forge/plan_review_submission_close.lua" "nvim/tests/forge/plan_review_recovery.lua"]
    "plan-workflow": ["nvim/tests/forge/plan_check_selection.lua" "nvim/tests/forge/plan_test_delete.lua" "nvim/tests/forge/plan_review_questions.lua"]
    "walkthrough": ["nvim/tests/forge/walkthrough_panes.lua" "nvim/tests/forge/walkthrough_composite.lua"]
    "lsp": ["nvim/tests/forge/lsp_diagnostics.lua" "nvim/tests/forge/lsp_startup_manifests.lua" "nvim/tests/forge/lsp_startup.lua" "nvim/tests/forge/lsp_source_read.lua" "nvim/tests/forge/lsp_query.lua" "nvim/tests/forge/plan_workspace_definition.lua"]
    "transport": ["nvim/tests/forge/client_admission_queue.lua" "nvim/tests/forge/client_publication_replay.lua" "nvim/tests/forge/result_transfer.lua" "nvim/tests/forge/receive_queue.lua" "nvim/tests/forge/protocol_contract.lua" "nvim/tests/forge/snapshot_transfer.lua"]
  }
}

# Fixtures that need explicit platform, language, provider, or Rust-emitted snapshot parameters.
def specialized_fixture_list [] {
  ["nvim/rust/forge/crates/forge-providers/tests/fixtures/session_workflows_host.lua" "nvim/tests/forge/harness_owned_agents_real.lua" "nvim/tests/forge/plan_navigation_languages_host.lua" "nvim/tests/forge/sidecar_manual.lua"]
}

# Fixtures removed from every default selection. Each entry carries its reason, and an explicit
# path still runs it.
def disabled_fixture_list [] {
  [
    "nvim/tests/forge/github_api_examples.lua" # exceeds the 5 s test limit; tracked in nvim/tests/forge/plans/disabled-slow-tests.md
    "nvim/tests/forge/harness_admission_host.lua" # exceeds the 5 s test limit; tracked in nvim/tests/forge/plans/disabled-slow-tests.md
    "nvim/tests/forge/harness_approval_resume_host.lua" # exceeds the 5 s test limit; tracked in nvim/tests/forge/plans/disabled-slow-tests.md
    "nvim/tests/forge/harness_publication_restart_host.lua" # exceeds the 5 s test limit; tracked in nvim/tests/forge/plans/disabled-slow-tests.md
    "nvim/tests/forge/plan_review_calls_host.lua" # exceeds the 5 s test limit; tracked in nvim/tests/forge/plans/disabled-slow-tests.md
    # exceeds the 5 s test limit; tracked in nvim/tests/forge/plans/disabled-slow-tests.md
    "nvim/tests/forge/status_staging_source_changes.lua" # exceeds the 5 s test limit; tracked in nvim/tests/forge/plans/disabled-slow-tests.md
    "nvim/tests/forge/harness_automatic_checks.lua" # exceeds the 5 s test limit; tracked in nvim/tests/forge/plans/disabled-slow-tests.md
    "nvim/tests/forge/harness_check_activity_host.lua" # exceeds the 5 s test limit; tracked in nvim/tests/forge/plans/disabled-slow-tests.md
    "nvim/tests/forge/harness_check_queue_host.lua" # exceeds the 5 s test limit; tracked in nvim/tests/forge/plans/disabled-slow-tests.md
    "nvim/tests/forge/harness_checks_host.lua" # exceeds the 5 s test limit; tracked in nvim/tests/forge/plans/disabled-slow-tests.md
    "nvim/tests/forge/harness_host.lua" # exceeds the 5 s test limit; tracked in nvim/tests/forge/plans/disabled-slow-tests.md
    "nvim/tests/forge/harness_message_folds_host.lua" # exceeds the 5 s test limit; tracked in nvim/tests/forge/plans/disabled-slow-tests.md
    "nvim/tests/forge/harness_plan_checks.lua" # exceeds the 5 s test limit; tracked in nvim/tests/forge/plans/disabled-slow-tests.md
    "nvim/tests/forge/harness_replan_host.lua" # exceeds the 5 s test limit; tracked in nvim/tests/forge/plans/disabled-slow-tests.md
    "nvim/tests/forge/harness_tasks_host.lua" # exceeds the 5 s test limit; tracked in nvim/tests/forge/plans/disabled-slow-tests.md
    "nvim/tests/forge/plan_checks_host.lua" # exceeds the 5 s test limit; tracked in nvim/tests/forge/plans/disabled-slow-tests.md
    "nvim/tests/forge/plan_review_host.lua" # exceeds the 5 s test limit; tracked in nvim/tests/forge/plans/disabled-slow-tests.md
    "nvim/tests/forge/semantic_execution_host.lua" # exceeds the 5 s test limit; tracked in nvim/tests/forge/plans/disabled-slow-tests.md
    # exceeds the 5 s test limit; tracked in nvim/tests/forge/plans/disabled-slow-tests.md
    "nvim/tests/forge/status_staging_host_edges.lua" # exceeds the 5 s test limit; tracked in nvim/tests/forge/plans/disabled-slow-tests.md
    "nvim/tests/forge/status_staging_host_flows.lua" # exceeds the 5 s test limit; tracked in nvim/tests/forge/plans/disabled-slow-tests.md
  ]
}

def affected_rule_list [] {
  [
    {suite: "shared", pattern: '^nvim/lua/forge/infra/.*$|^nvim/lua/forge/[^/]*\.lua$|^nvim/lua/forge/integrations/.*$|^nvim/lua/plugins/forge\.lua$|^nvim/lua/treesitter_smart_select\.lua$|^nvim/lua/git/diff\.lua$|^nvim/lua/forge/block_sequence\.lua$|^nvim/lua/forge/builder\.lua$|^nvim/lua/forge/buffer\.lua$|^nvim/lua/forge/decorations\.lua$|^nvim/lua/forge/gutter\.lua$|^nvim/lua/forge/nodes\.lua$|^nvim/lua/vim_options\.lua$|^nvim/lua/forge/views/commands\.lua$|^nvim/lua/forge/git/git_backend\.lua$|^nvim/lua/forge/completion\.lua$|^nvim/lua/forge/document_commands\.lua$|^nvim/lua/forge/editable\.lua$|^nvim/lua/forge/views/permissions\.lua$|^nvim/lua/forge/render/diff_parse\.lua$|^nvim/lua/forge/infra/perf\.lua$|^nvim/lua/forge/notifications\.lua$|^nvim/lua/github/notifications\.lua$|^nvim/lua/forge/input\.lua$|^nvim/lua/forge/window_presentation\.lua$|^nvim/lua/forge/width\.lua$|^nvim/lua/forge/init\.lua$|^nvim/lua/forge/git/mutation_coordinator\.lua$|^nvim/lua/forge/status\.lua$|^nvim/lua/forge/client\.lua$|^nvim/lua/forge/session\.lua$|^nvim/lua/forge/types\.lua$|^nvim/lua/forge/protocol_contract\.json$|^nvim/lua/forge/shared/.*$|^\.forge/checks/lua-tests\.nu$'} 
    {suite: "rendering", pattern: '^nvim/lua/forge/render/.*$|^nvim/lua/forge/queries/.*$|^nvim/lua/markdown_math/.*$|^nvim/lua/markdown_code\.lua$|^nvim/lua/markdown_math/blocks\.lua$|^nvim/lua/markdown_math/block_store\.lua$|^nvim/lua/plugins/markdown\.lua$|^nvim/lua/forge/buffer\.lua$|^nvim/lua/forge/render/annotations\.lua$|^nvim/lua/forge/render/hunk_index\.lua$|^nvim/lua/forge/render/layout\.lua$|^nvim/lua/forge/render/row_tree\.lua$|^nvim/lua/forge/render/source\.lua$|^nvim/lua/forge/render/source_loader\.lua$|^nvim/lua/forge/render/syntax_context\.lua$|^nvim/lua/forge/render/text_snapshot\.lua$|^nvim/lua/forge/render/decoration\.lua$|^nvim/lua/forge/shared/view_command_set\.lua$|^nvim/lua/forge/shared/view_controller\.lua$|^nvim/lua/forge/render/region\.lua$|^nvim/lua/plugins/join\.lua$|^nvim/lua/forge/init\.lua$|^nvim/lua/forge/git/git_data\.lua$|^nvim/lua/forge/render/syntax_engine\.lua$|^nvim/lua/forge/status\.lua$|^nvim/lua/forge/local_diff\.lua$|^nvim/lua/forge/render/intraline_diff\.lua$|^nvim/lua/forge/native_syntax\.lua$|^nvim/lua/markdown_math/dependency\.lua$|^nvim/lua/forge/client\.lua$|^nvim/lua/forge/session\.lua$|^nvim/lua/forge/types\.lua$|^nvim/lua/forge/protocol_contract\.json$|^nvim/lua/forge/shared/.*$|^\.forge/checks/lua-tests\.nu$|^nvim/tests/forge/support/status_fixture\.lua$'} 
    {suite: "github", pattern: '^nvim/lua/github/.*$|^nvim/lua/forge/views/pr/.*$|^nvim/lua/github/repo_cache\.lua$|^nvim/lua/github/gh\.lua$|^nvim/lua/github/issue_index\.lua$|^nvim/lua/forge/client\.lua$|^nvim/lua/forge/views/pr/reviewer_source\.lua$|^nvim/lua/github/issue_source\.lua$|^nvim/lua/github/open_pr\.lua$|^nvim/lua/forge/init\.lua$|^nvim/lua/forge/integrations/gh\.lua$|^nvim/lua/forge/git/write\.lua$|^nvim/lua/github/pr_creation\.lua$|^nvim/lua/github/mutation\.lua$|^nvim/lua/plugins/github\.lua$|^nvim/lua/forge/builder\.lua$|^nvim/lua/github/repo_users\.lua$|^nvim/lua/github/review_draft\.lua$|^nvim/lua/github/pull_request\.lua$|^nvim/lua/forge/review_document\.lua$|^nvim/lua/forge/views/commands\.lua$|^nvim/lua/forge/session\.lua$|^nvim/lua/forge/types\.lua$|^nvim/lua/forge/protocol_contract\.json$|^nvim/lua/forge/shared/.*$|^\.forge/checks/lua-tests\.nu$|^nvim/tests/forge/support/review_host\.lua$'} 
    {suite: "buffer", pattern: '^nvim/lua/forge/buffer[^/]*\.lua$|^nvim/lua/forge/node[^/]*\.lua$|^nvim/lua/forge/cooperative\.lua$|^nvim/lua/forge/source_document\.lua$|^nvim/lua/forge/buffer\.lua$|^nvim/lua/forge/editable\.lua$|^nvim/lua/forge/nodes\.lua$|^nvim/lua/forge/input\.lua$|^nvim/lua/forge/document_commands\.lua$|^nvim/lua/forge/gutter\.lua$|^nvim/lua/forge/draft_comments\.lua$|^nvim/lua/forge/draft_source\.lua$|^nvim/lua/forge/native_syntax\.lua$|^nvim/lua/forge/init\.lua$|^nvim/lua/forge/client\.lua$|^nvim/lua/forge/session\.lua$|^nvim/lua/forge/types\.lua$|^nvim/lua/forge/protocol_contract\.json$|^nvim/lua/forge/shared/.*$|^\.forge/checks/lua-tests\.nu$|^nvim/tests/forge/support/status_fixture\.lua$'} 
    {suite: "status-git", pattern: '^nvim/lua/forge/views/status/.*$|^nvim/lua/forge/git/.*$|^nvim/lua/forge/status[^/]*\.lua$|^nvim/lua/forge/init\.lua$|^nvim/lua/forge/status\.lua$|^nvim/lua/forge/source_document\.lua$|^nvim/lua/forge/integrations/ai_commit\.lua$|^nvim/lua/forge/client\.lua$|^nvim/lua/forge/builder\.lua$|^nvim/lua/forge/infra/config\.lua$|^nvim/lua/forge/harness/backend_preference\.lua$|^nvim/lua/forge/session\.lua$|^nvim/lua/forge/protocol\.lua$|^nvim/lua/forge/document_commands\.lua$|^nvim/lua/forge/buffer\.lua$|^nvim/lua/forge/integrations/commit\.lua$|^nvim/lua/forge/git/write\.lua$|^nvim/lua/forge/git/file_body\.lua$|^nvim/lua/forge/git/git_backend\.lua$|^nvim/lua/forge/integrations/gh\.lua$|^nvim/lua/forge/git/index_mutation\.lua$|^nvim/lua/forge/git/status_snapshot\.lua$|^nvim/lua/forge/infra/perf\.lua$|^nvim/lua/forge/status_render\.lua$|^nvim/lua/forge/input\.lua$|^nvim/lua/forge/nodes\.lua$|^nvim/lua/forge/infra/notifications\.lua$|^nvim/lua/forge/views/status/status_context\.lua$|^nvim/lua/forge/status_history\.lua$|^nvim/lua/forge/views/commands\.lua$|^nvim/lua/forge/startup_log\.lua$|^nvim/lua/forge/infra/confirm\.lua$|^nvim/lua/forge/width\.lua$|^nvim/lua/forge/views/status/issues_editor\.lua$|^nvim/lua/forge/types\.lua$|^nvim/lua/forge/protocol_contract\.json$|^nvim/lua/forge/shared/.*$|^\.forge/checks/lua-tests\.nu$|^nvim/tests/forge/support/status_fixture\.lua$'} 
    {suite: "sessions", pattern: '^nvim/lua/forge/views/harness/cleanup_progress\.lua$|^nvim/lua/forge/views/harness/tabline\.lua$|^nvim/lua/forge/views/harness/workspace\.lua$|^nvim/lua/forge/views/harness/session[^/]*\.lua$|^nvim/lua/forge/views/harness/task\.lua$|^nvim/lua/forge/views/harness/[^/]*agent[^/]*\.lua$|^nvim/lua/forge/views/harness/spawn_picker\.lua$|^nvim/lua/forge/harness/.*$|^nvim/lua/forge/views/harness/controller\.lua$|^nvim/lua/forge/views/harness/snapshot\.lua$|^nvim/lua/forge/init\.lua$|^nvim/lua/forge/views/picker/init\.lua$|^nvim/lua/forge/views/picker/state\.lua$|^nvim/lua/forge/views/harness/agent_picker\.lua$|^nvim/lua/forge/views/harness/agent_catalog\.lua$|^nvim/lua/forge/client\.lua$|^nvim/lua/forge/session\.lua$|^nvim/lua/forge/views/harness/layout\.lua$|^nvim/lua/forge/views/harness/approval\.lua$|^nvim/lua/forge/views/harness/health\.lua$|^nvim/lua/forge/buffer\.lua$|^nvim/lua/forge/shared/keymaps\.lua$|^nvim/lua/forge/views/harness/subagent_profiles\.lua$|^nvim/lua/forge/views/harness/plan_question/init\.lua$|^nvim/lua/forge/render/harness/command\.lua$|^nvim/lua/forge/render/harness/tool\.lua$|^nvim/lua/forge/views/harness/completion/command_source\.lua$|^nvim/lua/forge/infra/notifications\.lua$|^nvim/lua/forge/views/harness/prompt_history\.lua$|^nvim/lua/forge/builder\.lua$|^nvim/lua/forge/protocol\.lua$|^nvim/lua/forge/infra/highlights\.lua$|^nvim/lua/forge/views/harness/presentation\.lua$|^nvim/lua/forge/views/harness/terminals\.lua$|^nvim/lua/forge/render/task_tree\.lua$|^nvim/lua/forge/render/task_tree_style\.lua$|^nvim/lua/forge/render/harness/interaction_tree\.lua$|^nvim/lua/forge/infra/config\.lua$|^nvim/lua/forge/views/harness/session_navigation\.lua$|^nvim/lua/forge/source_document\.lua$|^nvim/lua/forge/views/harness/session_picker\.lua$|^nvim/lua/forge/integrations/datetime\.lua$|^nvim/lua/forge/shared/form\.lua$|^nvim/lua/forge/views/picker/layout\.lua$|^nvim/lua/forge/types\.lua$|^nvim/lua/forge/protocol_contract\.json$|^nvim/lua/forge/shared/.*$|^\.forge/checks/lua-tests\.nu$'} 
    {suite: "controls", pattern: '^nvim/lua/forge/views/picker/.*$|^nvim/lua/forge/shared/.*$|^nvim/lua/forge/input\.lua$|^nvim/lua/forge/infra/config\.lua$|^nvim/lua/forge/buffer\.lua$|^nvim/lua/forge/document_commands\.lua$|^nvim/lua/forge/shared/view_command_set\.lua$|^nvim/lua/forge/shared/keymaps\.lua$|^nvim/lua/forge/views/commands\.lua$|^nvim/lua/plugins/forge\.lua$|^nvim/lua/plugins/github\.lua$|^nvim/lua/forge/infra/popup_window\.lua$|^nvim/lua/forge/status\.lua$|^nvim/lua/forge/status_render\.lua$|^nvim/lua/forge/views/status/dialogs\.lua$|^nvim/lua/forge/infra/confirm\.lua$|^nvim/lua/forge/views/status/issues_editor\.lua$|^nvim/lua/forge/init\.lua$|^nvim/lua/forge/views/picker/layout\.lua$|^nvim/lua/forge/views/picker/input\.lua$|^nvim/lua/forge/views/picker/state\.lua$|^nvim/lua/forge/views/picker/init\.lua$|^nvim/lua/forge/views/picker/render\.lua$|^nvim/lua/forge/views/harness/model_picker\.lua$|^nvim/lua/forge/views/picker/search_input\.lua$|^nvim/lua/forge/views/harness/plan_question/init\.lua$|^nvim/lua/forge/client\.lua$|^nvim/lua/forge/session\.lua$|^nvim/lua/forge/types\.lua$|^nvim/lua/forge/protocol_contract\.json$|^\.forge/checks/lua-tests\.nu$'} 
    {suite: "composer", pattern: '^nvim/lua/forge/views/harness/input_state\.lua$|^nvim/lua/forge/views/harness/queue_editor\.lua$|^nvim/lua/forge/views/harness/prompt_submission\.lua$|^nvim/lua/forge/views/harness/prompt_history\.lua$|^nvim/lua/forge/views/harness/attachment\.lua$|^nvim/lua/forge/views/harness/completion/.*$|^nvim/lua/forge/views/harness/controller\.lua$|^nvim/lua/forge/views/harness/snapshot\.lua$|^nvim/lua/forge/views/harness/layout\.lua$|^nvim/lua/forge/buffer\.lua$|^nvim/lua/forge/editable\.lua$|^nvim/lua/forge/infra/config\.lua$|^nvim/lua/forge/shared/keymaps\.lua$|^nvim/lua/forge/shared/view_command_set\.lua$|^nvim/lua/forge/client\.lua$|^nvim/lua/forge/session\.lua$|^nvim/lua/forge/infra/notifications\.lua$|^nvim/lua/forge/views/harness/model_picker\.lua$|^nvim/lua/forge/views/harness/agent_picker\.lua$|^nvim/lua/forge/init\.lua$|^nvim/lua/forge/types\.lua$|^nvim/lua/forge/protocol_contract\.json$|^nvim/lua/forge/shared/.*$|^\.forge/checks/lua-tests\.nu$'} 
    {suite: "review-documents", pattern: '^nvim/lua/forge/review[^/]*\.lua$|^nvim/lua/forge/draft[^/]*\.lua$|^nvim/lua/forge/views/pr/.*$|^nvim/lua/forge/draft_comments\.lua$|^nvim/lua/forge/draft_source\.lua$|^nvim/lua/github/issue_index\.lua$|^nvim/lua/github/repo_cache\.lua$|^nvim/lua/github/issue_snapshot\.lua$|^nvim/lua/github/issue_document\.lua$|^nvim/lua/forge/editable\.lua$|^nvim/lua/forge/client\.lua$|^nvim/lua/github/issue_preview\.lua$|^nvim/lua/github/gh\.lua$|^nvim/lua/github/pickers\.lua$|^nvim/lua/github/issue_create\.lua$|^nvim/lua/github/issue_markdown\.lua$|^nvim/lua/github/issue_gutter\.lua$|^nvim/lua/forge/review_document\.lua$|^nvim/lua/forge/nodes\.lua$|^nvim/lua/forge/buffer\.lua$|^nvim/lua/forge/review_comments\.lua$|^nvim/lua/forge/infra/config\.lua$|^nvim/lua/forge/integrations/gh\.lua$|^nvim/lua/forge/views/commands\.lua$|^nvim/lua/forge/views/status/status_context\.lua$|^nvim/lua/forge/views/pr/reviewer_source\.lua$|^nvim/lua/github/issue_source\.lua$|^nvim/lua/forge/document_commands\.lua$|^nvim/lua/forge/init\.lua$|^nvim/lua/forge/session\.lua$|^nvim/lua/forge/types\.lua$|^nvim/lua/forge/protocol_contract\.json$|^nvim/lua/forge/shared/.*$|^\.forge/checks/lua-tests\.nu$|^nvim/tests/forge/support/review_host\.lua$'} 
    {suite: "editing", pattern: '^nvim/lua/forge/editable[^/]*\.lua$|^nvim/lua/forge/document_commands\.lua$|^nvim/lua/forge/effects\.lua$|^nvim/lua/forge/patch_spans\.lua$|^nvim/lua/forge/buffer\.lua$|^nvim/lua/forge/infra/config\.lua$|^nvim/lua/forge/status\.lua$|^nvim/lua/forge/source_document\.lua$|^nvim/lua/forge/input\.lua$|^nvim/lua/forge/editable_shadow\.lua$|^nvim/lua/forge/render/region\.lua$|^nvim/lua/forge/editable\.lua$|^nvim/lua/forge/shared/input_gutter\.lua$|^nvim/lua/forge/init\.lua$|^nvim/lua/forge/client\.lua$|^nvim/lua/forge/session\.lua$|^nvim/lua/forge/types\.lua$|^nvim/lua/forge/protocol_contract\.json$|^nvim/lua/forge/shared/.*$|^\.forge/checks/lua-tests\.nu$'} 
    {suite: "timeline-layout", pattern: '^nvim/lua/forge/views/harness/layout\.lua$|^nvim/lua/forge/content_layout\.lua$|^nvim/lua/forge/render/harness/.*$|^nvim/lua/forge/views/harness/controller\.lua$|^nvim/lua/forge/views/harness/snapshot\.lua$|^nvim/lua/forge/buffer\.lua$|^nvim/lua/forge/nodes\.lua$|^nvim/lua/forge/input\.lua$|^nvim/lua/forge/render/harness/markdown_text\.lua$|^nvim/lua/markdown_math/blocks\.lua$|^nvim/lua/forge/render/harness/markdown\.lua$|^nvim/lua/forge/block_sequence\.lua$|^nvim/lua/forge/infra/config\.lua$|^nvim/lua/forge/client\.lua$|^nvim/lua/forge/session\.lua$|^nvim/lua/forge/views/harness/presentation\.lua$|^nvim/lua/forge/gutter\.lua$|^nvim/lua/forge/views/harness/status_hint\.lua$|^nvim/lua/forge/shared/view_command_set\.lua$|^nvim/lua/forge/views/harness/health\.lua$|^nvim/lua/forge/render/harness/timeline_status\.lua$|^nvim/lua/forge/init\.lua$|^nvim/lua/forge/types\.lua$|^nvim/lua/forge/protocol_contract\.json$|^nvim/lua/forge/shared/.*$|^\.forge/checks/lua-tests\.nu$'} 
    {suite: "providers", pattern: '^nvim/lua/forge/views/harness/[^/]*provider[^/]*\.lua$|^nvim/lua/forge/views/harness/[^/]*model[^/]*\.lua$|^nvim/lua/forge/views/harness/settings\.lua$|^nvim/lua/forge/views/harness/background_configuration\.lua$|^nvim/lua/forge/views/harness/controller\.lua$|^nvim/lua/forge/views/harness/snapshot\.lua$|^nvim/lua/forge/client\.lua$|^nvim/lua/forge/session\.lua$|^nvim/lua/forge/views/harness/task\.lua$|^nvim/lua/forge/shared/keymaps\.lua$|^nvim/lua/forge/infra/notifications\.lua$|^nvim/lua/forge/init\.lua$|^nvim/lua/forge/integrations/background_settings\.lua$|^nvim/lua/forge/views/picker/init\.lua$|^nvim/lua/forge/infra/config\.lua$|^nvim/lua/forge/harness/backend_preference\.lua$|^nvim/lua/forge/views/harness/session_navigation\.lua$|^nvim/lua/forge/views/harness/init\.lua$|^nvim/lua/forge/views/harness/model_pins\.lua$|^nvim/lua/forge/views/harness/model_picker\.lua$|^nvim/lua/forge/views/picker/state\.lua$|^nvim/lua/forge/views/harness/completion/command_source\.lua$|^nvim/lua/forge/views/harness/model_order\.lua$|^nvim/lua/forge/infra/perf\.lua$|^nvim/lua/forge/views/harness/provider_setup\.lua$|^nvim/lua/forge/views/harness/session_picker\.lua$|^nvim/lua/forge/views/harness/session_preview\.lua$|^nvim/lua/forge/views/harness/provider_catalog\.lua$|^nvim/lua/forge/views/harness/provider_picker\.lua$|^nvim/lua/forge/types\.lua$|^nvim/lua/forge/protocol_contract\.json$|^nvim/lua/forge/shared/.*$|^\.forge/checks/lua-tests\.nu$'} 
    {suite: "permissions", pattern: '^nvim/lua/forge/views/harness/access\.lua$|^nvim/lua/forge/views/harness/approval\.lua$|^nvim/lua/forge/views/harness/question_presentation\.lua$|^nvim/lua/forge/permissions\.lua$|^nvim/lua/forge/views/harness/controller\.lua$|^nvim/lua/forge/views/harness/snapshot\.lua$|^nvim/lua/forge/views/picker/init\.lua$|^nvim/lua/forge/views/harness/command_detail\.lua$|^nvim/lua/forge/infra/highlights\.lua$|^nvim/lua/forge/views/picker/layout\.lua$|^nvim/lua/forge/infra/notifications\.lua$|^nvim/lua/forge/render/harness/interaction_tree\.lua$|^nvim/lua/forge/session\.lua$|^nvim/lua/forge/init\.lua$|^nvim/lua/forge/client\.lua$|^nvim/lua/forge/types\.lua$|^nvim/lua/forge/protocol_contract\.json$|^nvim/lua/forge/shared/.*$|^\.forge/checks/lua-tests\.nu$'} 
    {suite: "timeline-stream", pattern: '^nvim/lua/forge/views/harness/presentation\.lua$|^nvim/lua/forge/views/harness/tool_output\.lua$|^nvim/lua/forge/views/harness/tail_follow\.lua$|^nvim/lua/forge/views/harness/status_hint\.lua$|^nvim/lua/forge/views/harness/recap\.lua$|^nvim/lua/forge/views/harness/controller\.lua$|^nvim/lua/forge/views/harness/snapshot\.lua$|^nvim/lua/forge/client\.lua$|^nvim/lua/forge/views/harness/prompt_submission\.lua$|^nvim/lua/forge/init\.lua$|^nvim/lua/forge/render/harness/interaction_tree\.lua$|^nvim/lua/forge/buffer\.lua$|^nvim/lua/forge/shared/view_command_set\.lua$|^nvim/lua/forge/infra/config\.lua$|^nvim/lua/forge/session\.lua$|^nvim/lua/forge/infra/highlights\.lua$|^nvim/lua/forge/views/harness/health\.lua$|^nvim/lua/forge/views/harness/layout\.lua$|^nvim/lua/forge/infra/notifications\.lua$|^nvim/lua/forge/infra/perf\.lua$|^nvim/lua/forge/render/harness/tool\.lua$|^nvim/lua/forge/types\.lua$|^nvim/lua/forge/protocol_contract\.json$|^nvim/lua/forge/shared/.*$|^\.forge/checks/lua-tests\.nu$'} 
    {suite: "plan-editor", pattern: '^nvim/lua/forge/views/plan_review/.*$|^nvim/lua/forge/views/harness/plan_picker\.lua$|^nvim/lua/forge/init\.lua$|^nvim/lua/forge/views/picker/init\.lua$|^nvim/lua/forge/integrations/datetime\.lua$|^nvim/lua/forge/buffer\.lua$|^nvim/lua/forge/nodes\.lua$|^nvim/lua/forge/session\.lua$|^nvim/lua/forge/views/harness/controller\.lua$|^nvim/lua/forge/client\.lua$|^nvim/lua/forge/views/plan_review/entity_rename\.lua$|^nvim/lua/forge/views/plan_review/entity_navigation\.lua$|^nvim/lua/forge/views/plan_review/task_model\.lua$|^nvim/lua/forge/views/plan_review/entity_info\.lua$|^nvim/lua/forge/draft_comments\.lua$|^nvim/lua/forge/views/plan_review/document\.lua$|^nvim/lua/forge/buffer_view\.lua$|^nvim/lua/forge/input\.lua$|^nvim/lua/forge/views/plan_review/delivery\.lua$|^nvim/lua/forge/views/plan_review/native_controller\.lua$|^nvim/lua/forge/render/harness/interaction_tree\.lua$|^nvim/lua/forge/views/plan_review/references\.lua$|^nvim/lua/forge/views/plan_review/test_preview\.lua$|^nvim/lua/forge/infra/popup_window\.lua$|^nvim/lua/forge/render/harness/markdown\.lua$|^nvim/lua/forge/views/plan_review/markdown\.lua$|^nvim/lua/forge/views/plan_review/workspace_symbol\.lua$|^nvim/lua/forge/decorations\.lua$|^nvim/lua/forge/infra/highlights\.lua$|^nvim/lua/forge/views/plan_review/init\.lua$|^nvim/lua/forge/types\.lua$|^nvim/lua/forge/protocol_contract\.json$|^nvim/lua/forge/shared/.*$|^\.forge/checks/lua-tests\.nu$|^nvim/tests/forge/support/plan_delivery\.lua$'} 
    {suite: "plan-workflow", pattern: '^nvim/lua/forge/views/harness/plan_question/.*$|^nvim/lua/forge/views/harness/checks\.lua$|^nvim/lua/forge/views/plan_review/checks\.lua$|^nvim/lua/forge/views/plan_review/task_model\.lua$|^nvim/lua/forge/init\.lua$|^nvim/lua/forge/views/harness/plan_question/init\.lua$|^nvim/lua/forge/render/harness/interaction_tree\.lua$|^nvim/lua/forge/views/plan_review/decision\.lua$|^nvim/lua/forge/client\.lua$|^nvim/lua/forge/session\.lua$|^nvim/lua/forge/views/plan_review/native_controller\.lua$|^nvim/lua/forge/views/picker/init\.lua$|^nvim/lua/forge/views/plan_review/uml_style\.lua$|^nvim/lua/forge/infra/highlights\.lua$|^nvim/lua/forge/draft_comments\.lua$|^nvim/lua/forge/infra/popup_window\.lua$|^nvim/lua/forge/views/plan_review/entity_info\.lua$|^nvim/lua/forge/render/task_tree\.lua$|^nvim/lua/forge/views/plan_review/document\.lua$|^nvim/lua/forge/types\.lua$|^nvim/lua/forge/protocol_contract\.json$|^nvim/lua/forge/shared/.*$|^\.forge/checks/lua-tests\.nu$|^nvim/tests/forge/support/plan_delivery\.lua$'} 
    {suite: "walkthrough", pattern: '^nvim/lua/forge/walkthrough\.lua$|^nvim/lua/forge/walkthrough\.schema\.json$|^nvim/lua/forge/source_document\.lua$|^nvim/lua/forge/width\.lua$|^nvim/lua/forge/infra/config\.lua$|^nvim/lua/forge/init\.lua$|^nvim/lua/forge/client\.lua$|^nvim/lua/forge/session\.lua$|^nvim/lua/forge/types\.lua$|^nvim/lua/forge/protocol_contract\.json$|^nvim/lua/forge/shared/.*$|^\.forge/checks/lua-tests\.nu$'} 
    {suite: "lsp", pattern: '^nvim/lua/forge/lsp/.*$|^nvim/lua/forge/lsp/paths\.lua$|^nvim/lua/forge/lsp/source\.lua$|^nvim/lua/forge/lsp/startup\.lua$|^nvim/lua/forge/lsp/query\.lua$|^nvim/lua/forge/lsp/diagnostics\.lua$|^nvim/lua/forge/lsp/target\.lua$|^nvim/lua/forge/lsp/semantic\.lua$|^nvim/lua/forge/lsp/bridge\.lua$|^nvim/lua/forge/views/plan_review/workspace_definition\.lua$|^nvim/lua/forge/init\.lua$|^nvim/lua/forge/client\.lua$|^nvim/lua/forge/session\.lua$|^nvim/lua/forge/types\.lua$|^nvim/lua/forge/protocol_contract\.json$|^nvim/lua/forge/shared/.*$|^\.forge/checks/lua-tests\.nu$'} 
    {suite: "transport", pattern: '^nvim/lua/forge/client\.lua$|^nvim/lua/forge/receive\.lua$|^nvim/lua/forge/protocol[^/]*\.lua$|^nvim/lua/forge/json_transfer\.lua$|^nvim/lua/rust_sidecar/.*$|^nvim/lua/forge/builder\.lua$|^nvim/lua/forge/infra/notifications\.lua$|^nvim/lua/forge/protocol\.lua$|^nvim/lua/forge/init\.lua$|^nvim/lua/forge/status\.lua$|^nvim/lua/forge/startup_log\.lua$|^nvim/lua/plugins/forge\.lua$|^nvim/lua/forge/integrations/commit\.lua$|^nvim/lua/forge/buffer\.lua$|^nvim/lua/forge/snapshot\.lua$|^nvim/lua/forge/session\.lua$|^nvim/lua/forge/types\.lua$|^nvim/lua/forge/protocol_contract\.json$|^nvim/lua/forge/shared/.*$|^\.forge/checks/lua-tests\.nu$'} 
  ]
}

# A fixture entry is a repository path, optionally followed by `::scenario` for fixtures that
# take a scenario name through `arg[1]`.
def split_fixture_entry [entry: string] {
  let separator_index = $entry | str index-of "::"
  if $separator_index < 0 {
    {path: $entry, scenario: null}
  } else {
    {path: ($entry | str substring 0..<$separator_index), scenario: ($entry | str substring ($separator_index + 2)..)}
  }
}

def repository_relative [entry: string] {
  let parts = split_fixture_entry $entry
  let relative = $parts.path | path expand | path relative-to $repository_root | str replace --all '\' '/'
  if $parts.scenario == null { $relative } else { $"($relative)::($parts.scenario)" }
}

# Fixtures that run one scenario per Neovim process. Each declares scenarios as
# `{ name = "...", run = ...}` rows, and selecting the bare path runs every scenario separately so
# no scenario shares the five-second budget with another.
def scenario_fixture_path_list [] {
  ["nvim/tests/forge/plan_review_question_response_host.lua"]
}

def expand_scenarios [relative: string] {
  if not ($relative in (scenario_fixture_path_list)) { return [$relative] }
  open --raw ($repository_root | path join $relative) | lines
  | parse --regex '^  \{ name = "(?<name>[^"]+)", run = '
  | get name | each {|name| $"($relative)::($name)" }
}

def native_fixture_list [] {
  fixture_registry | values | flatten | append (specialized_fixture_list) | where {|file|
    $file | str starts-with "nvim/rust/forge/crates/forge-providers/tests/fixtures/"
  }
}

# Fixtures on disk that no suite lists; they run with the shared suite.
def unclassified_fixture_list [] {
  let registry = fixture_registry
  let known = $registry | values | flatten | append (specialized_fixture_list) | each {|entry| (split_fixture_entry $entry).path }
  (glob nvim/tests/forge/*.lua) | append (glob nvim/tests/*.lua) | each {|file| repository_relative $file }
  | where {|file| not ($file in $known) }
}

# Ordinary fixtures: every suite except the mock integration suite.
def ordinary_fixture_list [] {
  fixture_registry | reject mock-integration | values | flatten
  | append (unclassified_fixture_list)
}

def suite_fixture_list [suite: string] {
  let registry = fixture_registry
  let disabled = disabled_fixture_list
  let fixture_list = if $suite == "all" {
    $registry | values | flatten | append (unclassified_fixture_list)
  } else if $suite == "ordinary" {
    ordinary_fixture_list
  } else if $suite == "shared" {
    $registry.shared | append (unclassified_fixture_list)
  } else if $suite in ($registry | columns) {
    $registry | get $suite
  } else {
    error make {msg: $"Unknown Lua suite: ($suite)"}
  }
  $fixture_list | where {|file| not ($file in $disabled) }
}

# Map changed repository paths to the fixtures that cover them.
def affected_fixture_list [path_list: list<string>] {
  let registry = fixture_registry
  let specialized = specialized_fixture_list
  let native_fixtures = native_fixture_list
  let ordinary = ordinary_fixture_list
  let build_dependent = $registry | get mock-integration
  $path_list | each {|file|
    let fixture = ($file in $native_fixtures) or (($file | str starts-with "nvim/tests/") and not ($file | str contains "/support/") and not ($file | str contains "/fixtures/"))
    if $fixture {
      if $file in $specialized or $file in $build_dependent {
        error make {msg: $"Use the explicit integration check for ($file)"}
      }
      if ($file | path exists) { [$file] } else { [] }
    } else {
      let matches = affected_rule_list | where {|rule| $file =~ $rule.pattern } | get suite | uniq
      if ($matches | is-empty) { error make {msg: $"No audited test suite covers ($file)"} }
      $matches | each {|name| suite_fixture_list $name } | flatten
    }
  } | flatten
}

def validate_fixture [entry: string, suite: string, parameterized: bool] {
  let registry = fixture_registry
  let native_fixtures = native_fixture_list
  let entry_relative = repository_relative $entry
  let relative = (split_fixture_entry $entry_relative).path
  if (not ($relative | str starts-with "nvim/tests/") and not ($relative in $native_fixtures)) or not ($relative | str ends-with ".lua") {
    error make {msg: $"Not a repository Lua fixture: ($entry)"}
  }
  if not ($relative in $native_fixtures) and (($relative | str contains "/support/") or ($relative | str contains "/fixtures/")) {
    error make {msg: $"Support files are not independent test entrypoints: ($relative)"}
  }
  if $suite == "mock-integration" and not ($relative in ($registry | get $suite)) {
    error make {msg: $"Fixture does not belong to ($suite): ($relative)"}
  }
  if not $parameterized and $relative in (specialized_fixture_list) {
    error make {msg: $"Explicit platform or provider parameters are required for ($relative); pass them with --lua"}
  }
  if ($relative | path type) != "file" { error make {msg: $"Fixture does not exist: ($relative)"} }
  if (split_fixture_entry $entry_relative).scenario != null {
    if not ($relative in (scenario_fixture_path_list)) { error make {msg: $"Fixture takes no scenario: ($relative)"} }
    if not ($entry_relative in (expand_scenarios $relative)) { error make {msg: $"Unknown scenario: ($entry_relative)"} }
  }
  $entry_relative
}

# Resolve a request into the sorted fixture list it names. Explicit paths are fixtures unless
# `affected` marks them as changed repository paths. `parameterized` admits fixtures that need
# setup statements.
def select_fixture_list [suite: string, path_list: list<string>, affected: bool, parameterized: bool = false] {
  let relative_path_list = $path_list | each {|entry| repository_relative $entry } | uniq
  let selected = if ($relative_path_list | is-empty) {
    suite_fixture_list $suite
  } else if $affected {
    affected_fixture_list $relative_path_list
  } else {
    $relative_path_list
  }
  if ($selected | is-empty) { error make {msg: "No retained Lua fixtures cover this selection"} }
  $selected | each {|entry| validate_fixture $entry $suite $parameterized }
  | each {|entry| if ($entry | str contains "::") { [$entry] } else { expand_scenarios $entry } } | flatten | uniq | sort
}

def publish_record [value: record, target_path: path] {
  let pending_path = $"($target_path).pending"
  $value | to json --raw | save --force $pending_path
  mv --force $pending_path $target_path
}

# Every live process below `root_pid`, deepest first, from a single process-table snapshot.
def descendant_pid [root_pid: int] {
  let process_table = ps | select pid ppid
  mut frontier = [$root_pid]
  mut descendant_list = []
  while not ($frontier | is-empty) {
    let child_list = $process_table | where {|row| $row.ppid in $frontier and $row.pid != $root_pid } | get pid
    $descendant_list = $descendant_list | append $child_list
    $frontier = $child_list
  }
  $descendant_list | reverse
}

def live_pid_list [candidate_list: list<int>] {
  let live = ps | get pid
  $candidate_list | where {|candidate| $candidate in $live }
}

# Kill one process by id. Both platforms report "no such process" with a nonzero status (taskkill
# returns 128), and a process that already exited is the outcome the kill wanted, so liveness
# after the attempt decides success rather than the status.
def kill_process [process_id: int] {
  if $nu.os-info.family == "windows" {
    ^taskkill /PID ($process_id | into string) /F | complete | ignore
  } else {
    ^kill -KILL ($process_id | into string) | complete | ignore
  }
}

# Kill the worker and every process below it. The worker is the runner's own child, so the kill
# never reaches processes another run started. Each pass snapshots the live tree before killing
# it, because a dead parent drops its children from the parent-id chain, and repeats until the
# whole snapshot is gone.
def terminate_owned_tree [pid: int] {
  mut survivor_list = [$pid]
  mut attempt = 0
  while not ($survivor_list | is-empty) and $attempt < 5 {
    let tree = ($survivor_list | each {|root| descendant_pid $root } | flatten) | append $survivor_list | uniq
    for process_id in (live_pid_list $tree) { kill_process $process_id }
    sleep 50ms
    $survivor_list = live_pid_list $tree
    $attempt += 1
  }
  if ($survivor_list | is-empty) {
    {method: "process tree kill", exit_status: 0, stderr: ""}
  } else {
    {method: "process tree kill", exit_status: 1, stderr: $"Still running: ($survivor_list | str join ', ')"}
  }
}

# Run one fixture under the deadline. A child Nushell worker publishes its PID, runs Neovim with
# output redirected to files, and publishes the exit status, so a timeout can kill the tree and
# still report the output written so far.
def run_fixture [test: string, setup_list: list<string>] {
  let scratch_directory = mktemp -d
  let spec_path = $scratch_directory | path join "spec.json"
  let result_path = $scratch_directory | path join "result.json"
  let pid_path = $scratch_directory | path join "worker.json"
  let output_path = $scratch_directory | path join "output.txt"
  let setup_argument_list = $setup_list | each {|statement| ["--cmd" $"lua ($statement)"] } | flatten
  let fixture = split_fixture_entry $test
  let scenario_argument_list = if $fixture.scenario == null { [] } else { [$fixture.scenario] }
  let command = ["nvim"] | append $startup_argument_list | append $setup_argument_list | append ["-l" $fixture.path] | append $scenario_argument_list
  {command: $command, output_path: $output_path} | to json --raw | save --force $spec_path
  let started = date now
  let worker_job = job spawn {
    try {
      run-external $nu.current-exe "--no-config-file" $runner_path "worker" $spec_path $result_path $pid_path | complete | ignore
    } catch { null }
  }
  mut worker_result: any = null
  while $worker_result == null and (date now) - $started < $per_test_timeout {
    if ($result_path | path exists) {
      $worker_result = try { open $result_path } catch {|failure|
        {exit_code: 127, failure: $"Worker result is unreadable: ($failure.msg)"}
      }
    } else {
      sleep 20ms
    }
  }
  # The worker can publish its result between the last poll and the deadline, so read it once
  # more before declaring a timeout; a fixture that finished in time must not be killed.
  if $worker_result == null and ($result_path | path exists) {
    $worker_result = try { open $result_path } catch {|failure|
      {exit_code: 127, failure: $"Worker result is unreadable: ($failure.msg)"}
    }
  }
  let elapsed = (date now) - $started
  let outcome = if $worker_result != null {
    let fixture_output = try { open --raw $output_path } catch { "" }
    let fixture_error = $fixture_output =~ 'E[0-9]{3,}: (Lua chunk|Error executing lua|Vim)'
    let exit_status = if $worker_result.exit_code == 0 and $fixture_error { 1 } else { $worker_result.exit_code }
    let failure = $worker_result.failure? | default ""
    {state: (if $exit_status == 0 { "passed" } else { "failed" }), exit_status: $exit_status, output: ($fixture_output + $failure)}
  } else {
    let worker_pid = try { open $pid_path | get pid | into int } catch { null }
    let termination = if $worker_pid == null {
      {method: "unavailable", exit_status: 127, stderr: "Worker PID was not published"}
    } else {
      terminate_owned_tree $worker_pid
    }
    let fixture_output = try { open --raw $output_path } catch { "" }
    let note = $"Timed out after ($per_test_timeout); killed the process tree with ($termination.method), status ($termination.exit_status). ($termination.stderr)"
    {state: "timed_out", exit_status: 124, output: ($fixture_output + "\n" + $note)}
  }
  try { job kill $worker_job } catch { null }
  try { rm -rf $scratch_directory } catch { null }
  $outcome | merge {test: $test, command: $command, elapsed: $elapsed}
}

def format_outcome [outcome: record] {
  let label = match $outcome.state { "passed" => "PASS   ", "failed" => "FAIL   ", _ => "TIMEOUT" }
  let seconds = $outcome.elapsed / 1sec | math round --precision 3
  let heading = $"($label) ($seconds | fill --alignment right --width 7)s  ($outcome.test)"
  if $outcome.state == "passed" { $heading } else {
    let detail = $outcome.output | str trim | lines | each {|line| $"    ($line)" } | str join "\n"
    $"($heading)\n    exit status ($outcome.exit_status): ($outcome.command | str join ' ')\n($detail)"
  }
}

def run_fixture_list [test_list: list<string>, jobs: int, setup_list: list<string> = []] {
  cd $repository_root
  let outcome_list = $test_list | par-each --threads $jobs {|test|
    let outcome = run_fixture $test $setup_list
    print (format_outcome $outcome)
    $outcome
  }
  let failed = $outcome_list | where state != "passed"
  let timed_out = $outcome_list | where state == "timed_out"
  print ""
  print "Slowest fixtures:"
  $outcome_list | sort-by elapsed --reverse | first 10 | each {|outcome|
    print $"  ($outcome.elapsed / 1sec | math round --precision 3)s  ($outcome.test)"
  } | ignore
  if not ($failed | is-empty) {
    print ""
    print "Failed fixtures:"
    $failed | sort-by test | each {|outcome| print $"  ($outcome.state)  ($outcome.test)" } | ignore
  }
  print $"Lua tests: ($outcome_list | length) run, (($outcome_list | length) - ($failed | length)) passed, ($failed | length) failed, including ($timed_out | length) timed out."
  $outcome_list
}

def default_jobs [] {
  [((sys cpu | length) / 6 | math floor) 1] | math max
}

# Run Forge Lua fixtures in parallel, each under a hard five-second deadline.
def main [
  ...test_path: string  # Fixture paths, or changed source paths with --affected; omitted runs the suite
  --suite: string = "all"  # Suite to run or to validate paths against
  --affected  # Treat the paths as changed repository paths and run the suites that cover them
  --jobs: int  # Maximum concurrent fixtures; defaults to a sixth of the logical CPUs
  --list  # Print the selected fixtures as JSON without running them
  --lua: string  # Lua each fixture runs before loading, such as "vim.g.forge_test_language = 'rust'"
] {
  cd $repository_root
  let setup_list = if $lua == null { [] } else { [$lua] }
  let test_list = select_fixture_list $suite $test_path $affected (not ($setup_list | is-empty))
  if $list {
    print ({suite: $suite, fixtures: $test_list} | to json --raw)
    return
  }
  let outcome_list = run_fixture_list $test_list ($jobs | default (default_jobs)) $setup_list
  if ($outcome_list | any {|outcome| $outcome.state != "passed" }) { exit 1 }
}

# Harness check callback: read the version 1 request from stdin, run its fixtures, and write the
# report and receipt the request names.
def "main check" [
  suite: string  # affected for lua-test, or the mock integration suite
  --full  # Run the whole suite unless the request names explicit fixture inputs
  --jobs: int
] {
  let request = $in | from json
  if $request.version != 1 { error make {msg: "Unsupported check request version"} }
  cd $repository_root
  let selected_suite = $request.inputs?.suite? | default $suite
  let fixture = $request.inputs?.fixture? | default ""
  let selection_kind = $request.selection?.kind? | default "all"
  if $suite == "affected" and $selected_suite == "mock-integration" {
    error make {msg: "Use lua-test-mock-integration for build-dependent coverage"}
  }
  if $fixture != "" and ($selected_suite != "affected" or $full or $selection_kind == "all") {
    error make {msg: "Choose an exact fixture or a full suite, not both"}
  }
  let input_files = $request.inputs?.files? | default ($request.changed_files? | default [])
  let explicit_files = $suite != "affected" and $selection_kind == "inputs" and not ($request.selection?.inputs?.files? | default [] | is-empty)
  let test_list = if $fixture != "" {
    select_fixture_list "ordinary" [$fixture] false
  } else if $explicit_files {
    select_fixture_list $selected_suite $input_files false
  } else if $full or $selection_kind == "all" or $selected_suite != "affected" {
    select_fixture_list (if $selected_suite == "affected" { "ordinary" } else { $selected_suite }) [] false
  } else {
    select_fixture_list "ordinary" $input_files true
  }
  let outcome_list = run_fixture_list $test_list ($jobs | default (default_jobs))
  let report = $outcome_list | sort-by test | each {|outcome|
    {
      file: $outcome.test
      state: (if $outcome.state == "passed" { "passed" } else { "failed" })
      duration_ms: ($outcome.elapsed / 1ms | into int)
      command: ($outcome.command | str join " ")
      output: $outcome.output
    }
  }
  {version: 1, result: $report} | to json --raw | save --force $request.report_path
  {version: 1, selection: $request.selection} | to json --raw | save --force $request.receipt_path
  if ($outcome_list | any {|outcome| $outcome.state != "passed" }) { exit 1 }
}

# Internal: run one fixture's Neovim process for run_fixture.
def "main worker" [spec_path: path, result_path: path, pid_path: path] {
  publish_record {pid: $nu.pid} $pid_path
  cd $repository_root
  let spec = open $spec_path
  let program = $spec.command | first
  let argument_list = $spec.command | skip 1
  let worker_result = try {
    run-external $program ...$argument_list o+e> $spec.output_path
    {exit_code: 0}
  } catch {|failure|
    {exit_code: ($env.LAST_EXIT_CODE? | default 127), failure: (if ($env.LAST_EXIT_CODE? | default 0) == 0 { $failure.msg } else { "" })}
  }
  publish_record $worker_result $result_path
}
