--- DiffReview standalone git review UI.
--- Usage: :DiffReview

---@class DiffReviewModule
---@field config DiffReviewConfig?
---@field _git_backend DiffReviewGitBackend?
---@field _diff_uses_file_syntax fun(hunk_staged?: boolean[], opts?: table): boolean
---@field _prewarm_diff_syntax fun(filename: string, diff_text: string, hunk_staged?: boolean[], callback_key: string, on_update?: fun(syntax?: DiffReviewTreeSitterSyntax), opts?: table)
---@field _status_prewarm_hunk_budget fun(hunk_count: integer, options?: DiffReviewConfig): integer
---@field _diff_mutation_queue_model table
---@field _collect_items_from_git fun(cwd: string, cb: fun(items: table[]), ctx?: table)
---@field [string] any Re-export functions wired onto M by the `for _, fn in pairs(module)` loops below are invisible to static analysis; this index keeps `dr()._x` seam reads from flagging undefined-field.
local M = {}

-- Register the plugin's bundled treesitter queries on the runtimepath (see query_runtime).
require("diff_review.query_runtime")

M._comment_icon = "󰅺"
M._reply_icon = "↳"
M._pending_review_icon = "◷"
M._codeowner_review_icon = "⚠"
M._milestone_icon = "◆"

M._pr_overview = require("diff_review.views.pr.pr_overview")
M._datetime = require("diff_review.integrations.datetime")
M._conventional_commit = require("diff_review.integrations.conventional_commit")
M._status_conventional_commit_type_end = M._conventional_commit.type_end
M._status_conventional_commit_subject_segments = M._conventional_commit.subject_segments
M._comment_rows = require("github.comment_rows")

M._hunk_header_ns = vim.api.nvim_create_namespace("diff_review_headers")
M._active_hunk_header_ns = vim.api.nvim_create_namespace("diff_review_active_hunk")
M._status_ns = vim.api.nvim_create_namespace("diff_review_status")
M._status_decorate_ns = vim.api.nvim_create_namespace("diff_review_status_decorate")
M._hunk_header_priority = 20
M._active_hunk_header_priority = 200

local config = require("diff_review.infra.config")
local highlights = require("diff_review.infra.highlights")
local syntax_engine = require("diff_review.render.syntax_engine")
local diff_render = require("diff_review.render.diff_render")
local status_render = require("diff_review.views.status.status_render")
local keymaps = require("diff_review.shared.keymaps")
local hunk_model = require("diff_review.render.hunk_model")
-- Re-expose the pure hunk model under its original M._hunk_* names so init render
-- helpers and tests reach it unchanged.
for _hunk_name, _hunk_fn in pairs(hunk_model) do M["_hunk_" .. _hunk_name] = _hunk_fn end
M._diff_perf = require("diff_review.infra.perf")
M._diff_source_model = require("diff_review.render.source")
M._diff_annotation_model = require("diff_review.render.annotations")
M._diff_layout_model = require("diff_review.render.layout")
M._diff_mutation_queue_model = require("diff_review.render.mutation_queue")
M._diff_hunk_index_model = require("diff_review.render.hunk_index")
M._diff_row_tree_model = require("diff_review.render.row_tree")
M._diff_source_loader_model = require("diff_review.render.source_loader")
M._diff_syntax_context_model = require("diff_review.render.syntax_context")
M._diff_decoration_model = require("diff_review.render.decoration")
M._diff_view_controller_model = require("diff_review.shared.view_controller")
M._diff_view_command_set_model = require("diff_review.shared.view_command_set")
M._intraline_diff = require("diff_review.render.intraline_diff")
M._inventory = require("diff_review.infra.inventory")
M._window_options = require("diff_review.views.status.window_options")
-- Re-expose window option helpers under their original names so existing callers stay unchanged.
M._status_window_option_with_pair = M._window_options.option_with_pair
M._hide_line_numbers = M._window_options.hide_line_numbers
M._apply_status_window_options = M._window_options.apply
M._restore_line_numbers = M._window_options.restore
M._status_keys = require("diff_review.views.status.status_keys")
M._diff_buffer = require("diff_review.views.diff_buffer")
-- Re-expose every diff buffer view function under its original name (open_diff_buffer,
-- refresh_open_diff_buffer, the _diff_gutter_*/cursor helpers) so existing callers stay unchanged.
for _diff_buffer_name, _diff_buffer_fn in pairs(M._diff_buffer) do M[_diff_buffer_name] = _diff_buffer_fn end
M._status_issues = require("diff_review.views.status.status_issues")
M._status_head = require("diff_review.views.status.status_head")
-- Re-expose every head-line builder under its original name so existing callers stay unchanged.
for _status_head_name, _status_head_fn in pairs(M._status_head) do M[_status_head_name] = _status_head_fn end
-- The gitstatus debug/diagnostics table; callers reach it as M._gitstatus_debug.event/dump/perf_span.
M._gitstatus_debug = require("diff_review.views.status.status_debug")
M._size_gate = require("diff_review.views.status.size_gate")
-- Re-expose every size-gate estimator under its original name so existing callers stay unchanged.
for _size_gate_name, _size_gate_fn in pairs(M._size_gate) do M[_size_gate_name] = _size_gate_fn end
M._fold_state = require("diff_review.views.status.fold_state")
-- Re-expose fold accessors/appliers under their original names so existing callers stay unchanged.
for _fold_state_name, _fold_state_fn in pairs(M._fold_state) do M[_fold_state_name] = _fold_state_fn end
M._section_builder = require("diff_review.views.status.section_builder")
M._section_map = require("diff_review.views.status.section_map")
-- Re-expose section assembly + the section-map structure ops under their original names.
for _section_map_name, _section_map_fn in pairs(M._section_map) do M[_section_map_name] = _section_map_fn end
M._diff_source_state = require("diff_review.views.status.diff_source_state")
-- Re-expose per-file diff-source state functions so status_render's dr() calls resolve unchanged.
for _diff_source_state_name, _diff_source_state_fn in pairs(M._diff_source_state) do M[_diff_source_state_name] = _diff_source_state_fn end
M._entry_nav = require("diff_review.views.status.entry_nav")
-- Re-expose cursor/entry navigation helpers under their original names so existing callers stay unchanged.
for _entry_nav_name, _entry_nav_fn in pairs(M._entry_nav) do M[_entry_nav_name] = _entry_nav_fn end
M._git_data = require("diff_review.git.git_data")
-- Re-expose git execution + diff parsing + async syntax compute under their original names
-- (stage_patch/compute_diff_syntax_async/M._parse_diff/etc) so existing callers and test seams stay unchanged.
for _git_data_name, _git_data_fn in pairs(M._git_data) do M[_git_data_name] = _git_data_fn end
M._commit_view = require("diff_review.views.status.commit_view")
-- Re-expose the commit-message/dialog/action handlers under their original names so keymaps and
-- other modules' dr() dispatch resolves unchanged.
for _commit_view_name, _commit_view_fn in pairs(M._commit_view) do M[_commit_view_name] = _commit_view_fn end

highlights.setup()

-- status_helpers: misc status-view helpers (notifications, git command builders, commit
-- metadata, branch UI, popups) moved off init's local budget; seams resolve unchanged.
M._status_helpers = require("diff_review.views.status.status_helpers")
M._setup_bg_highlights = M._status_helpers.setup_bg_highlights
M._notify_error = M._status_helpers.notify_error
M._notify_debug = M._status_helpers.notify_debug
M.notify_git_failures = M._status_helpers.notify_git_failures
M._git_diff_command = M._status_helpers.git_diff_command
M._git_show_diff_command = M._status_helpers.git_show_diff_command
M._status_patch_head_line = M._status_helpers.status_patch_head_line
M._status_commit_relative_date = M._status_helpers.status_commit_relative_date
M._status_load_commit_files = M._status_helpers.status_load_commit_files
M._confirm = M._status_helpers.confirm
M._prompt_branch_name = M._status_helpers.prompt_branch_name
M._create_branch = M._status_helpers.create_branch
M._closest_current_line_for_deleted_diff_line = M._status_helpers.closest_current_line_for_deleted_diff_line

-- pr_state: PR-lookup + about-state async lifecycle moved off init; seams resolve unchanged.
M._status_pr_state = require("diff_review.views.status.pr_state")
M._status_start_pr_lookup = M._status_pr_state.status_start_pr_lookup
M._status_ensure_pr_state = M._status_pr_state.status_ensure_pr_state
M._status_has_changes = M._status_pr_state.status_has_changes
M._status_ensure_about_state = M._status_pr_state.status_ensure_about_state
M._status_open_pr = M._status_pr_state.open_pr

-- state: per-buffer status state lifecycle (attach/registry/autocmds) + perf instrumentation,
-- moved off init; init keeps only the _status/_main_status/_status_states pointer fields it writes.
M._status_state = require("diff_review.views.status.state")
M._attach_status_state = M._status_state.attach
M._status_register_view_controllers = M._status_state.register_view_controllers
M._cleanup_diff_buffers = M._status_state.cleanup_diff_buffers
M._status_restore_initial_folds = M._status_state.restore_initial_folds
M._status_first_grouping_id = M._status_state.first_grouping_id
M._status_perf_event = M._status_state.perf_event
M._status_perf_span = M._status_state.perf_span
M._status_perf_payload = M._status_state.perf_payload

-- render_orchestrator: the render_status dispatch + PR render/diff-load + after-render hooks
-- + cursor/hunk resolution, moved off init. The central render pipeline reaches state, pr_state,
-- and helpers through dr() seams already wired above.
M._render_orchestrator = require("diff_review.views.status.render_orchestrator")
M.render_status = M._render_orchestrator.render_status
M._render_status_or_notify = M._render_orchestrator.render_status_or_notify
M._render_pr_status = M._render_orchestrator.render_pr_status
M._load_pr_diff = M._render_orchestrator.load_pr_diff
M._status_after_render_pr = M._render_orchestrator.after_render_pr
M._status_after_render_review = M._render_orchestrator.after_render_review
M._status_after_render_status = M._render_orchestrator.after_render_status
M._status_after_render_default = M._render_orchestrator.after_render_default
M._status_stop_markdown_highlighter = M._render_orchestrator.stop_markdown_highlighter
M._status_cursor_target = M._render_orchestrator.cursor_target
M._status_diff_hunks_for_file = M._render_orchestrator.diff_hunks_for_file

local git_backend = require("diff_review.git.git_backend")
M.set_git_backend = git_backend.set_backend
M.reset_git_backend = git_backend.reset_backend
M._system_output = git_backend.system_output

local paths = require("diff_review.infra.paths")
M._repo_relative_for_test = paths.repo_relative_for_test

-- Render accumulators live in status_buffer (state-passing). The locals below stay as
-- thin orchestration wrappers that thread the active M._status into each module call,
-- so existing call sites render into the active buffer's state unchanged.
local status_buffer = require("diff_review.views.status.status_buffer")
M._status_buffer = status_buffer

---@class DiffReviewGutterSpec
---@field width integer
---@field old_width integer
---@field new_width integer


-- Window option save/restore now lives in diff_review.window_options (M._window_options),
-- re-exposed as M._hide_line_numbers / M._apply_status_window_options / M._restore_line_numbers.

--- Register a view controller per status-like view kind so command vocabulary and
--- render hooks resolve through a narrow per-view boundary instead of inline branches.

---@param opts? DiffReviewConfig
function M.setup(opts)
  M.config = config.setup(opts)
  M._diff_perf.configure_from_diff_review_options(M.config)
  M._setup_bg_highlights()
  M._status_register_view_controllers()
end

---@param cb fun(items: table[])
---@param _ctx table?
function M.get(cb, _ctx)
  git_backend.git_root_async(function(cwd)
    if not cwd then
      cb({})
      return
    end
    M._collect_items_from_git(cwd, cb, _ctx)
  end)
end

-- Namespace for diff preview/status rendering
M._ns = vim.api.nvim_create_namespace("diff_review_preview")
M._diff_gutter_visual_ns = vim.api.nvim_create_namespace("diff_review_visual_gutter")

---@class DiffReviewHunkChangeRegion
---@field first_item integer
---@field last_item integer
---@field context_line integer?
---@field context DiffReviewHunkTreeSitterContext|string?
---@field context_key string?
---@field changed_line integer?
---@field after_line integer?
---@field added integer
---@field removed integer

---@alias DiffReviewDiffSyntaxSide "old"|"new"

---@class DiffReviewHunkContextPaddingLine
---@field line_number integer
---@field old_line? integer
---@field new_line? integer
---@field text string

---@class DiffReviewHunkRenderPlan
---@field block DiffReviewParsedBlock
---@field hunk DiffReviewParsedHunk
---@field hunk_index integer
---@field region DiffReviewHunkChangeRegion
---@field region_index integer
---@field region_count integer
---@field render_items table[]
---@field include_render_line fun(parsed_line: DiffReviewParsedHunkLine): boolean
---@field gutter DiffReviewGutterSpec
---@field source_lines string[]?
---@field occupied_lines table<integer, boolean>
---@field syntax_by_item table<integer, { syntax?: DiffReviewTreeSitterSyntax, row?: integer }>
---@field visible_source_lines table<string, boolean>
---@field before_padding_lines DiffReviewHunkContextPaddingLine[]
---@field after_padding_lines DiffReviewHunkContextPaddingLine[]
---@field changed_lines table<integer, boolean>
---@field display_start integer?
---@field display_end integer?

---@class DiffReviewHunkDisplayGroup
---@field plans DiffReviewHunkRenderPlan[]
---@field gutter DiffReviewGutterSpec
---@field added integer
---@field removed integer
---@field display_start integer?
---@field display_end integer?
---@field changed_lines table<integer, boolean>


---@class DiffReviewSectionConfig
---@field name string
---@field title string
---@field default_folded boolean

---@type DiffReviewSectionConfig[]
local status_section_order = {
  { name = "unstaged", title = "Unstaged changes", default_folded = false },
  { name = "staged", title = "Staged changes", default_folded = false },
}

---@class DiffReviewStatusCommandSpec
---@field id string
---@field label string
---@field desc string
---@field modes string|string[]
---@field keymap? "status"|"review"
---@field visual? boolean
---@field pinned boolean
---@field views? table<DiffReviewStatusViewKind, boolean>

---@type DiffReviewStatusCommandSpec[]

M._status_hint_command_ids_by_view = require("diff_review.shared.command_specs").hint_command_ids_by_view

local status_command_specs_by_id = require("diff_review.shared.command_specs").by_id

---@type table<string, DiffReviewSectionConfig>
local status_section_by_name = {}
for _, section in ipairs(status_section_order) do
  status_section_by_name[section.name] = section
end

-- Status entry key builders now live in diff_review.status_keys (M._status_keys), re-exposed below.

M._status_segment_line_parts = status_buffer.segment_line_parts

-- status_primary_key / status_key_text now live in diff_review.status_keys.

--- Resolve the gitstatus debug log path; kept in init as a test override seam
--- (tests swap M._gitstatus_debug_log_path to redirect the log file).
---@return string
function M._gitstatus_debug_log_path()
  if M._gitstatus_debug_log_file then return M._gitstatus_debug_log_file end
  local dir = (vim.fn.stdpath("state") or ".") .. "/diff_review"
  pcall(vim.fn.mkdir, dir, "p")
  M._gitstatus_debug_log_file = dir .. "/gitstatus-debug.log"
  return M._gitstatus_debug_log_file
end

-- PR review mode (":or" in the PR/status view). A review buffer
-- (view_kind = "review") shows the PR title, an editable review summary, and
-- the changed files split into Unviewed/Viewed sections. `S`/`U` move a file
-- between them, `C` posts an inline comment to GitHub at the cursor/selection,
-- `y`/`n` jump between comments, and the submit key posts the review verdict.
-- Everything hangs off the module table because init.lua is at Lua's
-- 200-local limit.
M._review = require("diff_review.views.pr.review")
M._review.comment_icon = M._comment_icon
M._review.reply_icon = M._reply_icon

-- Per-repository config, read from `<repo root>/.diffreview.json`. Currently
-- only `branch_prefix` (used by the `bc` branch-create action). On the module
-- table because init.lua is at Lua's 200-local limit.
M._repo_config = require("diff_review.git.repo_config")

-- Branch-diff view (":GitDiff <branch>") helpers. Grouped on the module
-- table rather than as locals: this chunk is at Lua's hard limit of 200
-- local variables, so new file-scope `local function`s break the module.
-- Seam: expose the render orchestrator for the extracted branch_diff module.
M._status_render_loaded = status_render.status_render_loaded
M._branch_diff = require("diff_review.views.branch_diff")

-- PR title/description editing in the PR view: insert mode is gated to the
-- title line and description block, ":w" syncs via `gh pr edit`, and a "*"
-- marks unsynced fields. Functions hang off the module table because
-- init.lua is at Lua's 200-local limit.
-- Render/confirm seams used by the extracted pr_edit module (all in scope here).
-- Seams shared with the extracted pr_overview module.
-- Fold state seam reached by the extracted status_head module through dr().
M._status_provider_file_key = M._status_keys.provider_file_key
M._status_provider_hunk_key = M._status_keys.provider_hunk_key
M._status_render_file = status_render.status_render_file
-- Seams shared with the extracted review module.
M._status_command_visible = keymaps.status_command_visible
M._status_keys_for = keymaps.status_keys_for
M._status_command_specs_by_id = status_command_specs_by_id
-- Seams for the extracted syntax_engine module.
M._file_source_lines = syntax_engine.file_source_lines
M._same_hunk_ancestor_scope = syntax_engine.same_hunk_ancestor_scope
M._status_hunk_context_line = syntax_engine.status_hunk_context_line
M._old_file_syntax_source_lines = syntax_engine.old_file_syntax_source_lines
M._diff_new_side_matches_file = syntax_engine.diff_new_side_matches_file
M._cached_old_file_syntax = syntax_engine.cached_old_file_syntax
M._diff_uses_file_syntax = syntax_engine.diff_uses_file_syntax
M._prewarm_diff_syntax = syntax_engine.prewarm_diff_syntax
M._status_file_syntax_diff_text = syntax_engine.status_file_syntax_diff_text
M._status_prewarm_hunk_budget = syntax_engine.status_prewarm_hunk_budget
M._status_syntax_source_for_entry_kind = syntax_engine.status_syntax_source_for_entry_kind
-- Seams the git_data module reaches through dr() for notifications and stat counting.
-- Seam the commit_view module reaches through dr() to load a commit's files.
-- Seams the diff_buffer module reaches through dr() for its diff sourcing.
-- Seams the section_map module reaches through dr() for section ordering and git collection.
M._status_section_order = status_section_order
M._status_section_by_name = status_section_by_name
M._status_render_current_model = status_render.status_render_current_model
-- Seams for the extracted diff_render module.
M._merge_hunk_gutter_specs = diff_render.merge_hunk_gutter_specs
M._add_plan_to_hunk_display_group = diff_render.add_plan_to_hunk_display_group
M._merge_hunk_render_plans = diff_render.merge_hunk_render_plans
-- Seams for the extracted status_render module.
M._status_decorate_visible = status_render.status_decorate_visible
M._status_emit_row_spans = status_render.status_emit_row_spans
M._status_decorate_rows = status_render.status_decorate_rows
M._status_register_decoration_provider = status_render.status_register_decoration_provider
M._status_write_rendered_buffer = status_render.status_write_rendered_buffer
M._status_apply_rendered_extmarks = status_render.status_apply_rendered_extmarks
M._status_after_buffer_render = status_render.status_after_buffer_render
M._status_hunk_key = M._status_keys.hunk_key
M._status_commit_key = M._status_keys.commit_key
M._status_commit_hunk_key = M._status_keys.commit_hunk_key
M._status_commit_file_key = M._status_keys.commit_file_key
M._status_file_key = M._status_keys.file_key
M._status_section_key = M._status_keys.section_key
-- Seams shared with the extracted hunk_model module.
M._hunk_add_gutter = diff_render.hunk_add_gutter
M._treesitter_line_segments = syntax_engine.treesitter_line_segments
M._hunk_context_scope_key = syntax_engine.hunk_context_scope_key
M._same_hunk_context_scope = syntax_engine.same_hunk_context_scope
M._hunk_line_visible_in_context_scope = syntax_engine.hunk_line_visible_in_context_scope
M._pr_edit = require("diff_review.views.pr.pr_edit")
M._status_actions = require("diff_review.views.status.actions")
for _action_name, _action_fn in pairs(M._status_actions) do M[_action_name] = _action_fn end
M._commands = require("diff_review.views.commands")
for _command_name, _command_fn in pairs(M._commands) do M[_command_name] = _command_fn end

M._file_revision = require("diff_review.views.file_revision")

-- Seams for the extracted keymaps module.
-- Seams the commit_view module reaches through dr() for about-state and deleted-line jump resolution.
M._status_primary_key = M._status_keys.primary_key
M._status_command_visible_for_view = keymaps.status_command_visible_for_view
M._status_hint_segments = keymaps.status_hint_segments
M._status_hint_title = keymaps.status_hint_title
M._status_hint_winbar = keymaps.status_hint_winbar
M._status_apply_hint_bar = keymaps.status_apply_hint_bar
M._status_clear_hint_bar = keymaps.status_clear_hint_bar

return M
