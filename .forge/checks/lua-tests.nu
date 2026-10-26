const repository_root = path self | path dirname | path dirname | path dirname
const runner_path = path self

def worker [spec_path: path, result_path: path, pid_path: path] {
  {pid: $nu.pid} | to json --raw | save --force $pid_path
  let spec = open $spec_path
  let result = try {
    run-external $spec.program ...$spec.arguments | complete
  } catch {|failure|
    {exit_code: 127, stdout: "", stderr: $failure.msg}
  }
  $result | to json --raw | save --force $result_path
}

def descendant_pid [pid: int] {
  let result = try { ^pgrep -P ($pid | into string) | complete } catch { null }
  if $result == null or $result.exit_code != 0 { return [] }
  $result.stdout
  | lines
  | each {|value| $value | into int }
  | each {|child| [$child] | append (descendant_pid $child) }
  | flatten
}

def terminate_owned_tree [pid: int] {
  if $nu.os-info.family == "windows" {
    let result = try { ^taskkill /PID ($pid | into string) /T /F | complete } catch {|failure|
      {exit_code: 127, stdout: "", stderr: $failure.msg}
    }
    return {
      method: "taskkill /T /F"
      exit_status: $result.exit_code
      stdout: $result.stdout
      stderr: $result.stderr
    }
  }
  let descendant = descendant_pid $pid
  let process = [$pid] | append $descendant | reverse
  for process_id in $process {
    ^kill -TERM ($process_id | into string) | complete | ignore
  }
  sleep 200ms
  for process_id in $process {
    ^kill -KILL ($process_id | into string) | complete | ignore
  }
  {method: "recursive kill", exit_status: 0, stdout: "", stderr: ""}
}

def run_test [script_path: path, test: path, startup: list<string>, timeout: duration] {
  let directory = mktemp -d
  let spec_path = $directory | path join "spec.json"
  let result_path = $directory | path join "result.json"
  let pid_path = $directory | path join "worker.json"
  {program: "nvim", arguments: ($startup | append ["-l", $test])} | to json --raw | save --force $spec_path
  let worker = job spawn {
    let worker_argument = [$script_path, "--worker", "--worker-spec", $spec_path, "--worker-result", $result_path, "--worker-pid", $pid_path]
    let result = try {
      run-external $nu.current-exe ...$worker_argument | complete
    } catch {|failure|
      {exit_code: 127, stdout: "", stderr: $failure.msg}
    }
    $result | ignore
  }
  let started = date now
  mut result: any = null
  while $result == null and (date now) - $started < $timeout {
    if ($result_path | path exists) {
      $result = try { open $result_path } catch {|failure|
        {exit_code: 127, stdout: "", stderr: $"Worker result is unreadable: ($failure.msg)"}
      }
    } else {
      sleep 50ms
    }
  }
  let result = if $result != null {
    try { job kill $worker } catch { null }
    $result
  } else {
    let tree = if ($pid_path | path exists) {
      let worker_pid = try { open $pid_path | get pid | into int } catch { null }
      if $worker_pid == null {
        {method: "unavailable", exit_status: 127, stdout: "", stderr: "Worker PID is unreadable"}
      } else {
        terminate_owned_tree $worker_pid
      }
    } else {
      {method: "unavailable", exit_status: 127, stdout: "", stderr: "Worker PID was not published"}
    }
    try { job kill $worker } catch { null }
    {
      exit_code: 124
      stdout: ""
      stderr: $"Test deadline exceeded before the fixture exited. Tree termination ($tree.method) returned ($tree.exit_status). ($tree.stderr)"
    }
  }
  try { rm -rf $directory } catch { null }
  $result
}


def main [
  suite: string = "all"
  --all
  --timeout: duration = 30sec
  --worker
  --worker-spec: path
  --worker-result: path
  --worker-pid: path
] {
  let request_text = $in
  if $worker {
    worker $worker_spec $worker_result $worker_pid
    return
  }
  cd $repository_root
  let registry = {
    "shared": ["nvim/tests/smart_select.lua" "nvim/tests/git_diff_compact.lua" "nvim/tests/forge/block_sequence.lua" "nvim/tests/forge/build_command.lua" "nvim/tests/forge/decoration_refresh.lua" "nvim/tests/forge/decoration_precedence.lua" "nvim/tests/forge/declaration_fold_navigation.lua" "nvim/tests/forge/compact_preview_lifecycle.lua" "nvim/tests/forge/forge_completion.lua" "nvim/tests/forge/large_gutter_selection.lua" "nvim/tests/forge/permissions.lua" "nvim/tests/forge/parser_and_logging.lua" "nvim/tests/forge/notifications_subject_open.lua" "nvim/tests/forge/notifications_public.lua" "nvim/tests/forge/notifications_adapter.lua" "nvim/tests/forge/native_window_layout.lua" "nvim/tests/forge/native_pr_runtime_boundary.lua" "nvim/tests/forge/mutation_coordinator.lua" "nvim/tests/forge/mock_backend.lua" "nvim/tests/forge/quoted_diff_paths.lua"]
    "rendering": ["nvim/tests/markdown_math.lua" "nvim/tests/markdown_code.lua" "nvim/tests/forge/diff_presentation_grid.lua" "nvim/tests/forge/diff_architecture.lua" "nvim/tests/forge/treesj_native.lua" "nvim/tests/forge/treesitter_context.lua" "nvim/tests/forge/syntax_rerender.lua" "nvim/tests/forge/local_diff_native.lua" "nvim/tests/forge/intraline_diff.lua" "nvim/tests/forge/native_syntax_bypass.lua" "nvim/tests/forge/markdown_math_dependency.lua"]
    "github": ["nvim/tests/github_repo_cache.lua" "nvim/tests/github_open_pr_prompt.lua" "nvim/tests/github_open_pr.lua" "nvim/tests/github_link.lua" "nvim/tests/github_issue_index.lua" "nvim/tests/github_integration.lua" "nvim/tests/forge/github_review_draft.lua" "nvim/tests/forge/github_pull_request.lua" "nvim/tests/forge/github_mutation.lua" "nvim/tests/forge/github_metadata.lua" "nvim/tests/forge/pr_snapshot.lua" "nvim/tests/forge/pr_entry_cutover.lua"]
    "buffer": ["nvim/tests/forge/buffer_transfer.lua" "nvim/tests/forge/buffer_replica.lua" "nvim/tests/forge/buffer_lifecycle.lua" "nvim/tests/forge/buffer_incremental.lua" "nvim/tests/forge/buffer_gutter_shift.lua" "nvim/tests/forge/buffer_view.lua" "nvim/tests/forge/cooperative_snapshot.lua" "nvim/tests/forge/conceal.lua" "nvim/tests/forge/generated_adoption.lua" "nvim/tests/forge/fold_subtree_updates.lua" "nvim/tests/forge/fold_streaming_boundary.lua" "nvim/tests/forge/fold_restore.lua" "nvim/tests/forge/fold_reflow.lua" "nvim/tests/forge/fold_failure_recovery.lua" "nvim/tests/forge/folds_native.lua" "nvim/tests/forge/folds_highlights.lua" "nvim/tests/forge/gutter_cursor.lua" "nvim/tests/forge/node_projection.lua" "nvim/tests/forge/node_contract.lua" "nvim/tests/forge/restored_region_sequence.lua" "nvim/tests/forge/projected_diff_gutters.lua" "nvim/tests/forge/source_overlay.lua" "nvim/tests/forge/source_document_stale.lua" "nvim/tests/forge/row_prefix.lua" "nvim/tests/forge/physical_document.lua"]
    "status-git": ["nvim/tests/forge/branch_diff.lua" "nvim/tests/forge/ai_commit.lua" "nvim/tests/forge/commit_backend_preference.lua" "nvim/tests/forge/commit_amend_command.lua" "nvim/tests/forge/commit_editor_abort.lua" "nvim/tests/forge/commit_lifecycle.lua" "nvim/tests/forge/commit_prompt_preview.lua" "nvim/tests/forge/commit_parent_exit.lua" "nvim/tests/forge/file_body.lua" "nvim/tests/forge/file_revision.lua" "nvim/tests/forge/index_mutation_real_git.lua" "nvim/tests/forge/index_mutation.lua" "nvim/tests/forge/native_git_write.lua" "nvim/tests/forge/status_fold_boundaries.lua" "nvim/tests/forge/status_document_events.lua" "nvim/tests/forge/status_deferred_folds.lua" "nvim/tests/forge/status_context.lua" "nvim/tests/forge/status_collapsed_demand.lua" "nvim/tests/forge/status_body_recovery.lua" "nvim/tests/forge/status_body_ordering.lua" "nvim/tests/forge/revision_commands_real.lua" "nvim/tests/forge/revision_commands.lua" "nvim/tests/forge/status_recent_commits.lua" "nvim/tests/forge/status_pr_cache.lua" "nvim/tests/forge/status_popup_context.lua" "nvim/tests/forge/status_optimistic.lua" "nvim/tests/forge/status_open_lifecycle.lua" "nvim/tests/forge/status_native.lua" "nvim/tests/forge/status_issues_projection.lua" "nvim/tests/forge/status_header_width.lua" "nvim/tests/forge/status_gutter_relocation.lua" "nvim/tests/forge/status_fold_return.lua" "nvim/tests/forge/status_semantic_runtime.lua" "nvim/tests/forge/status_semantic.lua" "nvim/tests/forge/status_remote_progress.lua" "nvim/tests/forge/status_refresh.lua" "nvim/tests/forge/status_visual_capture.lua"]
    "extended": ["nvim/tests/forge/harness_admission_host.lua" "nvim/tests/forge/background_host.lua" "nvim/tests/forge/command_clue.lua" "nvim/tests/forge/client_initialize_host.lua" "nvim/tests/forge/client_executable_copy.lua" "nvim/tests/forge/client_callback_isolation.lua" "nvim/tests/forge/commit_generation_host.lua" "nvim/tests/forge/commit_host.lua" "nvim/tests/forge/commit_large_output_host.lua" "nvim/tests/forge/composer_autopair.lua" "nvim/tests/forge/commit_status_reuse.lua" "nvim/tests/forge/commit_settings_host.lua" "nvim/tests/forge/exchange_history.lua" "nvim/tests/forge/forge_client.lua" "nvim/tests/forge/harness_control_responsiveness.lua" "nvim/tests/forge/harness_completion.lua" "nvim/tests/forge/harness_checks_host.lua" "nvim/tests/forge/harness_check_activity_host.lua" "nvim/tests/forge/harness_checks.lua" "nvim/tests/forge/harness_checkpoint_host.lua" "nvim/tests/forge/harness_automatic_checks.lua" "nvim/tests/forge/harness_attachments_host.lua" "nvim/tests/forge/harness_approval_resume_host.lua" "nvim/tests/forge/harness_acp_children_host.lua" "nvim/tests/forge/harness_access_host.lua" "nvim/tests/forge/harness_prompt_history_host.lua" "nvim/tests/forge/harness_plan_browsing_host.lua" "nvim/tests/forge/harness_package_checks.lua" "nvim/tests/forge/harness_native_import_host.lua" "nvim/tests/forge/harness_model_pins_host.lua" "nvim/tests/forge/harness_message_folds_host.lua" "nvim/tests/forge/harness_markdown_render.lua" "nvim/tests/forge/harness_markdown_folds.lua" "nvim/tests/forge/harness_interrupt_host.lua" "nvim/tests/forge/harness_input_timeout_host.lua" "nvim/tests/forge/harness_input_timeout.lua" "nvim/tests/forge/harness_host.lua" "nvim/tests/forge/harness_task_recovery_host.lua" "nvim/tests/forge/harness_tasks_host.lua" "nvim/tests/forge/walkthrough_host.lua" "nvim/tests/forge/harness_skill_reads_host.lua" "nvim/tests/forge/harness_replan_host.lua" "nvim/tests/forge/harness_provider_switch_host.lua" "nvim/tests/forge/language_syntax_host.lua" "nvim/tests/forge/plan_review_host.lua" "nvim/tests/forge/markdown_viewport.lua" "nvim/tests/forge/markdown_math_render.lua" "nvim/tests/forge/markdown_math_async.lua" "nvim/tests/forge/lsp_tools_host.lua" "nvim/tests/forge/plan_review_definition_host.lua" "nvim/tests/forge/plan_review_calls_host.lua" "nvim/tests/forge/plan_declaration_presentation_host.lua" "nvim/tests/forge/plan_checks_host.lua" "nvim/tests/forge/receive_initialize_host.lua" "nvim/tests/forge/pr_context_host.lua" "nvim/tests/forge/source_host.lua" "nvim/tests/forge/source_document_presentation.lua" "nvim/tests/forge/semantic_execution_host.lua" "nvim/tests/forge/status_recent_commits_host.lua" "nvim/tests/forge/status_push_host.lua" "nvim/tests/forge/status_popup_host.lua" "nvim/tests/forge/status_ignore_staged_host.lua" "nvim/tests/forge/status_host.lua" "nvim/tests/forge/status_staging_host_gutters.lua" "nvim/tests/forge/status_staging_host_flows.lua" "nvim/tests/forge/status_staging_host_edges.lua" "nvim/tests/forge/status_virtual_hunk_merge.lua" "nvim/tests/forge/status_untracked_optimistic.lua" "nvim/tests/forge/status_staging_source_changes.lua" "nvim/tests/forge/status_word_wrap_host.lua" "nvim/tests/forge/per_edit_routes_host.lua" "nvim/tests/forge/empty_diff_cursor.lua" "nvim/tests/forge/harness_codex_activity.lua" "nvim/tests/forge/harness_plan_checks.lua" "nvim/tests/forge/harness_new.lua" "nvim/tests/forge/host_initialize.lua"]
    "sessions": ["nvim/tests/forge/harness_question_admission.lua" "nvim/tests/forge/agent_picker.lua" "nvim/tests/forge/harness_controller_native.lua" "nvim/tests/forge/harness_continuation_sync.lua" "nvim/tests/forge/harness_command_display.lua" "nvim/tests/forge/harness_command_admission.lua" "nvim/tests/forge/harness_change_navigation.lua" "nvim/tests/forge/harness_active_startup.lua" "nvim/tests/forge/harness_notifications.lua" "nvim/tests/forge/harness_mode_restart.lua" "nvim/tests/forge/harness_health.lua" "nvim/tests/forge/harness_file_paging.lua" "nvim/tests/forge/harness_terminals.lua" "nvim/tests/forge/harness_tasks.lua" "nvim/tests/forge/task_tree.lua" "nvim/tests/forge/harness_snapshot.lua" "nvim/tests/forge/harness_session_name.lua" "nvim/tests/forge/harness_session_identity.lua" "nvim/tests/forge/harness_saved_diff.lua" "nvim/tests/forge/harness_rollback_choice.lua" "nvim/tests/forge/harness_receive.lua" "nvim/tests/forge/harness_workspace_exit.lua" "nvim/tests/forge/harness_workspace.lua" "nvim/tests/forge/harness_wire_version.lua" "nvim/tests/forge/harness_transport_failure.lua" "nvim/tests/forge/session_picker.lua" "nvim/tests/forge/subagent_profiles.lua"]
    "controls": ["nvim/tests/forge/command_prefix.lua" "nvim/tests/forge/native_command_facade.lua" "nvim/tests/forge/public_commands.lua" "nvim/tests/forge/popup_window.lua" "nvim/tests/forge/popup_parity.lua" "nvim/tests/forge/picker.lua" "nvim/tests/forge/picker_buffer_lock.lua" "nvim/tests/forge/picker_wrapping.lua"]
    "composer": ["nvim/tests/forge/composer_newlines.lua" "nvim/tests/forge/composer_interleave.lua" "nvim/tests/forge/harness_composer_keymaps.lua" "nvim/tests/forge/harness_composer_hint.lua" "nvim/tests/forge/harness_child_steering.lua" "nvim/tests/forge/harness_attachments.lua" "nvim/tests/forge/harness_submit_steering.lua" "nvim/tests/forge/harness_queue_edit.lua" "nvim/tests/forge/harness_queued_model.lua" "nvim/tests/forge/harness_prompt_lifecycle.lua"]
    "review-documents": ["nvim/tests/forge/draft_source.lua" "nvim/tests/forge/draft_comment_threads.lua" "nvim/tests/forge/draft_comments.lua" "nvim/tests/forge/issue_snapshot_recovery.lua" "nvim/tests/forge/issue_snapshot.lua" "nvim/tests/forge/issue_response_generation.lua" "nvim/tests/forge/issue_rebind.lua" "nvim/tests/forge/issue_preview.lua" "nvim/tests/forge/issue_picker_cutover.lua" "nvim/tests/forge/issue_markdown.lua" "nvim/tests/forge/issue_gutter.lua" "nvim/tests/forge/issue_editability.lua" "nvim/tests/forge/issue_document.lua" "nvim/tests/forge/review_comment_folds.lua" "nvim/tests/forge/review_comments.lua" "nvim/tests/forge/review_capture.lua" "nvim/tests/forge/review_uncached_close.lua" "nvim/tests/forge/review_rebind.lua" "nvim/tests/forge/review_operation_queue.lua" "nvim/tests/forge/review_loading.lua" "nvim/tests/forge/review_editing.lua" "nvim/tests/forge/review_document.lua" "nvim/tests/forge/review_cursor.lua" "nvim/tests/forge/review_comment_recovery.lua"]
    "editing": ["nvim/tests/forge/document_winbar.lua" "nvim/tests/forge/document_host_generation.lua" "nvim/tests/forge/document_commands.lua" "nvim/tests/forge/editable_shadow.lua" "nvim/tests/forge/editable_region_append.lua" "nvim/tests/forge/editable_presentation.lua" "nvim/tests/forge/editable_ordering.lua" "nvim/tests/forge/editable_native.lua" "nvim/tests/forge/effects_native.lua" "nvim/tests/forge/editable_trim.lua" "nvim/tests/forge/local_edit_patch.lua" "nvim/tests/forge/input_gutter_lifecycle.lua" "nvim/tests/forge/mixed_edit_patch.lua"]
    "timeline-layout": ["nvim/tests/forge/harness_exchange_folds.lua" "nvim/tests/forge/harness_content_layout.lua" "nvim/tests/forge/harness_change_folds.lua" "nvim/tests/forge/harness_markdown_text.lua" "nvim/tests/forge/harness_markdown_regions.lua" "nvim/tests/forge/harness_markdown_layout.lua" "nvim/tests/forge/harness_layout.lua" "nvim/tests/forge/harness_gutter_selection.lua" "nvim/tests/forge/harness_fold_markers.lua" "nvim/tests/forge/harness_fold_lifecycle.lua" "nvim/tests/forge/harness_streaming_folds.lua" "nvim/tests/forge/timeline_status.lua"]
    "providers": ["nvim/tests/forge/harness_effort_selection.lua" "nvim/tests/forge/harness_commit_settings.lua" "nvim/tests/forge/harness_background_picker.lua" "nvim/tests/forge/harness_backend_switch.lua" "nvim/tests/forge/harness_model_pins.lua" "nvim/tests/forge/harness_model_order.lua" "nvim/tests/forge/harness_settings.lua" "nvim/tests/forge/harness_provider_setup.lua" "nvim/tests/forge/harness_provider_destination.lua" "nvim/tests/forge/harness_provider_catalog.lua"]
    "permissions": ["nvim/tests/forge/harness_approval.lua" "nvim/tests/forge/harness_access.lua" "nvim/tests/forge/harness_question_preview.lua" "nvim/tests/forge/harness_question_navigation.lua"]
    "timeline-stream": ["nvim/tests/forge/harness_presentation.lua" "nvim/tests/forge/harness_summary_metrics.lua" "nvim/tests/forge/harness_status_hint_async.lua" "nvim/tests/forge/harness_status_hint.lua" "nvim/tests/forge/harness_status_region.lua" "nvim/tests/forge/harness_sections.lua" "nvim/tests/forge/harness_scroll_boundary.lua" "nvim/tests/forge/harness_recap_controller.lua" "nvim/tests/forge/harness_recap.lua" "nvim/tests/forge/harness_publication.lua" "nvim/tests/forge/harness_update_scheduling.lua" "nvim/tests/forge/harness_transcript_follow.lua" "nvim/tests/forge/harness_tool_toggle.lua" "nvim/tests/forge/harness_tool_timing.lua" "nvim/tests/forge/harness_tool_output.lua" "nvim/tests/forge/harness_timeline_position.lua"]
    "plan-editor": ["nvim/tests/forge/harness_plan_picker.lua" "nvim/tests/forge/harness_plan_folds.lua" "nvim/tests/forge/harness_plan_browsing.lua" "nvim/tests/forge/plan_review_entity_rename.lua" "nvim/tests/forge/plan_review_entity_navigation.lua" "nvim/tests/forge/plan_review_entity_info.lua" "nvim/tests/forge/plan_review_drafts.lua" "nvim/tests/forge/plan_review_document.lua" "nvim/tests/forge/plan_review_delivery.lua" "nvim/tests/forge/plan_review_controller.lua" "nvim/tests/forge/plan_review_comment.lua" "nvim/tests/forge/plan_resolution.lua" "nvim/tests/forge/plan_references.lua" "nvim/tests/forge/plan_optimistic_edit.lua" "nvim/tests/forge/plan_markdown_render.lua" "nvim/tests/forge/plan_markdown_regions.lua" "nvim/tests/forge/plan_file_tree.lua" "nvim/tests/forge/plan_comment_boundaries.lua" "nvim/tests/forge/plan_annotation_edit.lua" "nvim/tests/forge/plan_review_wrapping.lua" "nvim/tests/forge/plan_review_workspace_symbol.lua" "nvim/tests/forge/plan_review_type_access_render.lua" "nvim/tests/forge/plan_review_submission_close.lua" "nvim/tests/forge/plan_review_rustdoc.lua" "nvim/tests/forge/plan_review_recovery.lua" "nvim/tests/forge/plan_review_public_import.lua" "nvim/tests/forge/plan_review_native_failure.lua" "nvim/tests/forge/plan_review_narrative_render.lua"]
    "plan-workflow": ["nvim/tests/forge/plan_question.lua" "nvim/tests/forge/plan_execution_timeline.lua" "nvim/tests/forge/plan_decision.lua" "nvim/tests/forge/plan_check_selection.lua" "nvim/tests/forge/plan_uml_style.lua" "nvim/tests/forge/plan_test_delete.lua" "nvim/tests/forge/plan_task_model_entity_reference.lua" "nvim/tests/forge/plan_task_model.lua" "nvim/tests/forge/plan_review_questions.lua"]
    "walkthrough": ["nvim/tests/forge/walkthrough_source.lua" "nvim/tests/forge/walkthrough_panes.lua" "nvim/tests/forge/walkthrough_composite.lua" "nvim/tests/forge/walkthrough_adapter.lua"]
    "lsp": ["nvim/tests/forge/lsp_paths.lua" "nvim/tests/forge/lsp_failures.lua" "nvim/tests/forge/lsp_diagnostics.lua" "nvim/tests/forge/lsp_targets.lua" "nvim/tests/forge/lsp_startup_roots.lua" "nvim/tests/forge/lsp_startup_manifests.lua" "nvim/tests/forge/lsp_startup.lua" "nvim/tests/forge/lsp_source_transfer.lua" "nvim/tests/forge/lsp_source_read.lua" "nvim/tests/forge/lsp_query.lua" "nvim/tests/forge/lsp_presentation.lua" "nvim/tests/forge/plan_workspace_definition.lua"]
    "transport": ["nvim/tests/forge/result_transfer.lua" "nvim/tests/forge/receive_queue.lua" "nvim/tests/forge/protocol_contract.lua" "nvim/tests/forge/process_stdio.lua" "nvim/tests/forge/startup_log.lua" "nvim/tests/forge/startup_command.lua" "nvim/tests/forge/startup.lua" "nvim/tests/forge/snapshot_transfer.lua" "nvim/tests/forge/sidecar_profile.lua"]
    "benchmarks": ["nvim/tests/forge/syntax_resource_acceptance.lua" "nvim/tests/forge/performance_acceptance.lua" "nvim/tests/forge/perf.lua" "nvim/tests/forge/streaming_performance.lua"]
  }
  let specialized = ["nvim/tests/forge/harness_acp_real.lua" "nvim/tests/forge/harness_owned_agents_real.lua" "nvim/tests/forge/windows_ui_parity.lua" "nvim/tests/forge/unix_client_shutdown.lua" "nvim/tests/forge/plan_navigation_languages_host.lua" "nvim/tests/forge/sidecar_manual.lua"]
  let automatic = $registry | reject extended benchmarks
  let known = $registry | values | flatten
  let discovered = (glob nvim/tests/forge/*.lua) | append (glob nvim/tests/*.lua) | each {|file|
    $file | path relative-to $repository_root | str replace --all '\' '/'
  }
  let unclassified = $discovered | where {|file| not ($file in $known) and not ($file in $specialized) }
  let rule_list = [
    {suite: "shared", pattern: '^nvim/lua/forge/infra/.*$|^nvim/lua/forge/[^/]*\.lua$|^nvim/lua/forge/integrations/.*$|^nvim/lua/plugins/forge\.lua$|^nvim/lua/treesitter_smart_select\.lua$|^nvim/lua/git/diff\.lua$|^nvim/lua/forge/block_sequence\.lua$|^nvim/lua/forge/builder\.lua$|^nvim/lua/forge/buffer\.lua$|^nvim/lua/forge/decorations\.lua$|^nvim/lua/forge/gutter\.lua$|^nvim/lua/forge/nodes\.lua$|^nvim/lua/vim_options\.lua$|^nvim/lua/forge/views/commands\.lua$|^nvim/lua/forge/git/git_backend\.lua$|^nvim/lua/forge/completion\.lua$|^nvim/lua/forge/document_commands\.lua$|^nvim/lua/forge/editable\.lua$|^nvim/lua/forge/views/permissions\.lua$|^nvim/lua/forge/render/diff_parse\.lua$|^nvim/lua/forge/infra/perf\.lua$|^nvim/lua/forge/notifications\.lua$|^nvim/lua/github/notifications\.lua$|^nvim/lua/forge/input\.lua$|^nvim/lua/forge/window_presentation\.lua$|^nvim/lua/forge/width\.lua$|^nvim/lua/forge/init\.lua$|^nvim/lua/forge/git/mutation_coordinator\.lua$|^nvim/lua/forge/status\.lua$|^nvim/lua/forge/client\.lua$|^nvim/lua/forge/session\.lua$|^nvim/lua/forge/types\.lua$|^nvim/lua/forge/protocol_contract\.json$|^nvim/lua/forge/shared/.*$|^\.forge/checks/lua-tests\.nu$'} 
    {suite: "rendering", pattern: '^nvim/lua/forge/render/.*$|^nvim/lua/forge/queries/.*$|^nvim/lua/markdown_math/.*$|^nvim/lua/markdown_code\.lua$|^nvim/lua/markdown_math/blocks\.lua$|^nvim/lua/markdown_math/block_store\.lua$|^nvim/lua/plugins/markdown\.lua$|^nvim/lua/forge/buffer\.lua$|^nvim/lua/forge/render/annotations\.lua$|^nvim/lua/forge/render/hunk_index\.lua$|^nvim/lua/forge/render/layout\.lua$|^nvim/lua/forge/render/row_tree\.lua$|^nvim/lua/forge/render/source\.lua$|^nvim/lua/forge/render/source_loader\.lua$|^nvim/lua/forge/render/syntax_context\.lua$|^nvim/lua/forge/render/text_snapshot\.lua$|^nvim/lua/forge/render/decoration\.lua$|^nvim/lua/forge/shared/view_command_set\.lua$|^nvim/lua/forge/shared/view_controller\.lua$|^nvim/lua/forge/render/region\.lua$|^nvim/lua/plugins/join\.lua$|^nvim/lua/forge/init\.lua$|^nvim/lua/forge/git/git_data\.lua$|^nvim/lua/forge/render/syntax_engine\.lua$|^nvim/lua/forge/status\.lua$|^nvim/lua/forge/local_diff\.lua$|^nvim/lua/forge/render/intraline_diff\.lua$|^nvim/lua/forge/native_syntax\.lua$|^nvim/lua/markdown_math/dependency\.lua$|^nvim/lua/forge/client\.lua$|^nvim/lua/forge/session\.lua$|^nvim/lua/forge/types\.lua$|^nvim/lua/forge/protocol_contract\.json$|^nvim/lua/forge/shared/.*$|^\.forge/checks/lua-tests\.nu$|^nvim/tests/forge/support/status_fixture\.lua$'} 
    {suite: "github", pattern: '^nvim/lua/github/.*$|^nvim/lua/forge/views/pr/.*$|^nvim/lua/github/repo_cache\.lua$|^nvim/lua/github/gh\.lua$|^nvim/lua/github/issue_index\.lua$|^nvim/lua/forge/client\.lua$|^nvim/lua/forge/views/pr/reviewer_source\.lua$|^nvim/lua/github/issue_source\.lua$|^nvim/lua/github/open_pr\.lua$|^nvim/lua/forge/init\.lua$|^nvim/lua/forge/integrations/gh\.lua$|^nvim/lua/forge/git/write\.lua$|^nvim/lua/github/pr_creation\.lua$|^nvim/lua/github/mutation\.lua$|^nvim/lua/plugins/github\.lua$|^nvim/lua/forge/builder\.lua$|^nvim/lua/github/repo_users\.lua$|^nvim/lua/github/review_draft\.lua$|^nvim/lua/github/pull_request\.lua$|^nvim/lua/forge/review_document\.lua$|^nvim/lua/forge/views/commands\.lua$|^nvim/lua/forge/session\.lua$|^nvim/lua/forge/types\.lua$|^nvim/lua/forge/protocol_contract\.json$|^nvim/lua/forge/shared/.*$|^\.forge/checks/lua-tests\.nu$|^nvim/tests/forge/support/review_host\.lua$'} 
    {suite: "buffer", pattern: '^nvim/lua/forge/buffer[^/]*\.lua$|^nvim/lua/forge/node[^/]*\.lua$|^nvim/lua/forge/cooperative\.lua$|^nvim/lua/forge/source_document\.lua$|^nvim/lua/forge/buffer\.lua$|^nvim/lua/forge/editable\.lua$|^nvim/lua/forge/nodes\.lua$|^nvim/lua/forge/input\.lua$|^nvim/lua/forge/document_commands\.lua$|^nvim/lua/forge/gutter\.lua$|^nvim/lua/forge/draft_comments\.lua$|^nvim/lua/forge/draft_source\.lua$|^nvim/lua/forge/native_syntax\.lua$|^nvim/lua/forge/init\.lua$|^nvim/lua/forge/client\.lua$|^nvim/lua/forge/session\.lua$|^nvim/lua/forge/types\.lua$|^nvim/lua/forge/protocol_contract\.json$|^nvim/lua/forge/shared/.*$|^\.forge/checks/lua-tests\.nu$|^nvim/tests/forge/support/status_fixture\.lua$'} 
    {suite: "status-git", pattern: '^nvim/lua/forge/views/status/.*$|^nvim/lua/forge/git/.*$|^nvim/lua/forge/status[^/]*\.lua$|^nvim/lua/forge/init\.lua$|^nvim/lua/forge/status\.lua$|^nvim/lua/forge/source_document\.lua$|^nvim/lua/forge/integrations/ai_commit\.lua$|^nvim/lua/forge/client\.lua$|^nvim/lua/forge/builder\.lua$|^nvim/lua/forge/infra/config\.lua$|^nvim/lua/forge/harness/backend_preference\.lua$|^nvim/lua/forge/session\.lua$|^nvim/lua/forge/protocol\.lua$|^nvim/lua/forge/document_commands\.lua$|^nvim/lua/forge/buffer\.lua$|^nvim/lua/forge/integrations/commit\.lua$|^nvim/lua/forge/git/write\.lua$|^nvim/lua/forge/git/file_body\.lua$|^nvim/lua/forge/git/git_backend\.lua$|^nvim/lua/forge/integrations/gh\.lua$|^nvim/lua/forge/git/index_mutation\.lua$|^nvim/lua/forge/git/status_snapshot\.lua$|^nvim/lua/forge/infra/perf\.lua$|^nvim/lua/forge/status_render\.lua$|^nvim/lua/forge/input\.lua$|^nvim/lua/forge/nodes\.lua$|^nvim/lua/forge/infra/notifications\.lua$|^nvim/lua/forge/views/status/status_context\.lua$|^nvim/lua/forge/status_history\.lua$|^nvim/lua/forge/views/commands\.lua$|^nvim/lua/forge/startup_log\.lua$|^nvim/lua/forge/infra/confirm\.lua$|^nvim/lua/forge/width\.lua$|^nvim/lua/forge/views/status/issues_editor\.lua$|^nvim/lua/forge/types\.lua$|^nvim/lua/forge/protocol_contract\.json$|^nvim/lua/forge/shared/.*$|^\.forge/checks/lua-tests\.nu$|^nvim/tests/forge/support/status_fixture\.lua$'} 
    {suite: "sessions", pattern: '^nvim/lua/forge/views/harness/workspace\.lua$|^nvim/lua/forge/views/harness/session[^/]*\.lua$|^nvim/lua/forge/views/harness/task\.lua$|^nvim/lua/forge/views/harness/[^/]*agent[^/]*\.lua$|^nvim/lua/forge/views/harness/spawn_picker\.lua$|^nvim/lua/forge/harness/.*$|^nvim/lua/forge/views/harness/controller\.lua$|^nvim/lua/forge/views/harness/snapshot\.lua$|^nvim/lua/forge/init\.lua$|^nvim/lua/forge/views/picker/init\.lua$|^nvim/lua/forge/views/picker/state\.lua$|^nvim/lua/forge/views/harness/agent_picker\.lua$|^nvim/lua/forge/views/harness/agent_catalog\.lua$|^nvim/lua/forge/client\.lua$|^nvim/lua/forge/session\.lua$|^nvim/lua/forge/views/harness/layout\.lua$|^nvim/lua/forge/views/harness/approval\.lua$|^nvim/lua/forge/views/harness/health\.lua$|^nvim/lua/forge/buffer\.lua$|^nvim/lua/forge/shared/keymaps\.lua$|^nvim/lua/forge/views/harness/subagent_profiles\.lua$|^nvim/lua/forge/views/harness/plan_question/init\.lua$|^nvim/lua/forge/render/harness/command\.lua$|^nvim/lua/forge/render/harness/tool\.lua$|^nvim/lua/forge/views/harness/completion/command_source\.lua$|^nvim/lua/forge/infra/notifications\.lua$|^nvim/lua/forge/views/harness/prompt_history\.lua$|^nvim/lua/forge/builder\.lua$|^nvim/lua/forge/protocol\.lua$|^nvim/lua/forge/infra/highlights\.lua$|^nvim/lua/forge/views/harness/presentation\.lua$|^nvim/lua/forge/views/harness/terminals\.lua$|^nvim/lua/forge/render/task_tree\.lua$|^nvim/lua/forge/render/task_tree_style\.lua$|^nvim/lua/forge/render/harness/interaction_tree\.lua$|^nvim/lua/forge/infra/config\.lua$|^nvim/lua/forge/views/harness/session_navigation\.lua$|^nvim/lua/forge/source_document\.lua$|^nvim/lua/forge/views/harness/session_picker\.lua$|^nvim/lua/forge/integrations/datetime\.lua$|^nvim/lua/forge/shared/form\.lua$|^nvim/lua/forge/views/picker/layout\.lua$|^nvim/lua/forge/types\.lua$|^nvim/lua/forge/protocol_contract\.json$|^nvim/lua/forge/shared/.*$|^\.forge/checks/lua-tests\.nu$'} 
    {suite: "controls", pattern: '^nvim/lua/forge/views/picker/.*$|^nvim/lua/forge/shared/.*$|^nvim/lua/forge/input\.lua$|^nvim/lua/forge/infra/config\.lua$|^nvim/lua/forge/buffer\.lua$|^nvim/lua/forge/document_commands\.lua$|^nvim/lua/forge/shared/view_command_set\.lua$|^nvim/lua/forge/shared/keymaps\.lua$|^nvim/lua/forge/views/commands\.lua$|^nvim/lua/plugins/forge\.lua$|^nvim/lua/plugins/github\.lua$|^nvim/lua/forge/infra/popup_window\.lua$|^nvim/lua/forge/status\.lua$|^nvim/lua/forge/status_render\.lua$|^nvim/lua/forge/views/status/dialogs\.lua$|^nvim/lua/forge/infra/confirm\.lua$|^nvim/lua/forge/views/status/issues_editor\.lua$|^nvim/lua/forge/init\.lua$|^nvim/lua/forge/views/picker/layout\.lua$|^nvim/lua/forge/views/picker/input\.lua$|^nvim/lua/forge/views/picker/state\.lua$|^nvim/lua/forge/views/picker/init\.lua$|^nvim/lua/forge/views/picker/render\.lua$|^nvim/lua/forge/views/harness/model_picker\.lua$|^nvim/lua/forge/views/picker/search_input\.lua$|^nvim/lua/forge/views/harness/plan_question/init\.lua$|^nvim/lua/forge/client\.lua$|^nvim/lua/forge/session\.lua$|^nvim/lua/forge/types\.lua$|^nvim/lua/forge/protocol_contract\.json$|^\.forge/checks/lua-tests\.nu$'} 
    {suite: "composer", pattern: '^nvim/lua/forge/views/harness/queue_editor\.lua$|^nvim/lua/forge/views/harness/prompt_submission\.lua$|^nvim/lua/forge/views/harness/prompt_history\.lua$|^nvim/lua/forge/views/harness/attachment\.lua$|^nvim/lua/forge/views/harness/completion/.*$|^nvim/lua/forge/views/harness/controller\.lua$|^nvim/lua/forge/views/harness/snapshot\.lua$|^nvim/lua/forge/views/harness/layout\.lua$|^nvim/lua/forge/buffer\.lua$|^nvim/lua/forge/editable\.lua$|^nvim/lua/forge/infra/config\.lua$|^nvim/lua/forge/shared/keymaps\.lua$|^nvim/lua/forge/shared/view_command_set\.lua$|^nvim/lua/forge/client\.lua$|^nvim/lua/forge/session\.lua$|^nvim/lua/forge/infra/notifications\.lua$|^nvim/lua/forge/views/harness/model_picker\.lua$|^nvim/lua/forge/views/harness/agent_picker\.lua$|^nvim/lua/forge/init\.lua$|^nvim/lua/forge/types\.lua$|^nvim/lua/forge/protocol_contract\.json$|^nvim/lua/forge/shared/.*$|^\.forge/checks/lua-tests\.nu$'} 
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
  let request = if $all { null } else { $request_text | from json }
  let full = $all or ($request != null and $request.selection.kind == "all")
  if not $full and ($request.version != 1 or $request.selection.kind != "files") {
    error make {msg: "Lua selection requires a version 1 files request"}
  }
  let input_paths = if $full { [] } else {
    $request.selection.files | each {|entry|
      $entry | path expand | path relative-to $repository_root | str replace --all '\' '/'
    } | uniq
  }
  let selected = if $full {
    if $suite == "all" or $suite == "affected" { $automatic | values | flatten | append $unclassified } else if $suite == "shared" {
      $registry.shared | append $unclassified
    } else { $registry | get $suite }
  } else if $suite == "affected" {
    $input_paths | each {|file|
      let fixture = ($file | str starts-with "nvim/tests/") and not ($file | str contains "/support/") and not ($file | str contains "/fixtures/")
      if $fixture {
        if $file in $specialized or $file in $registry.extended or $file in $registry.benchmarks {
          error make {msg: $"Use the explicit integration or benchmark check for ($file)"}
        }
        if ($file | path exists) { [$file] } else {
          $automatic | values | where {|files| $file in $files } | flatten | where {|retained| $retained != $file }
        }
      } else {
        let matches = $rule_list | where {|rule| $file =~ $rule.pattern } | get suite | uniq
        if ($matches | is-empty) { error make {msg: $"No audited test suite covers ($file)"} }
        $matches | each {|name|
          if $name == "shared" { $registry.shared | append $unclassified } else { $registry | get $name }
        } | flatten
      }
    } | flatten
  } else {
    $input_paths
  }
  if ($selected | is-empty) { error make {msg: "No retained Lua fixtures cover this selection"} }
  let test_list = $selected | each {|entry|
    let relative = $entry | path expand | path relative-to $repository_root | str replace --all '\' '/'
    if not ($relative | str starts-with "nvim/tests/") or not ($relative | str ends-with ".lua") {
      error make {msg: $"Not a repository Lua fixture: ($entry)"}
    }
    if ($relative | str contains "/support/") or ($relative | str contains "/fixtures/") {
      error make {msg: $"Support files are not independent test entrypoints: ($relative)"}
    }
    if $relative in $specialized { error make {msg: $"Explicit platform or provider parameters are required for ($relative)"} }
    if ($relative | path type) != "file" { error make {msg: $"Fixture does not exist: ($relative)"} }
    $relative
  } | uniq | sort
  print ({suite: $suite, source: $input_paths, fixtures: $test_list} | to json --raw)
  let startup = [--headless -i NONE -u NONE --cmd "set rtp^=./nvim" --cmd "lua vim.loader.enable(false)"]
  mut failure_count = 0
  for test in $test_list {
    let started = date now
    let result = run_test $runner_path $test $startup $timeout
    let fixture_error = $result.stderr =~ 'E[0-9]{3,}: (Lua chunk|Error executing lua|Vim)'
    let exit_status = if $result.exit_code == 0 and $fixture_error { 1 } else { $result.exit_code }
    print ({
      test: $test
      command: ([nvim] | append $startup | append [-l $test])
      timeout: ($timeout | into string)
      elapsed: ((date now) - $started | into string)
      exit_status: $exit_status
      stdout: $result.stdout
      stderr: $result.stderr
    } | to json --raw)
    if $exit_status != 0 { $failure_count += 1 }
  }
  if not $all {
    {version: 1, selection: {kind: "files", files: $input_paths}} | to json --raw | save --force $request.receipt_path
  }
  print ({passed: (($test_list | length) - $failure_count), failed: $failure_count} | to json --raw)
  if $failure_count > 0 { exit 1 }
}

