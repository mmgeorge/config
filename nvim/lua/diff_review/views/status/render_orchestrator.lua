--- Drives status/PR/review rendering: the async git-root + status-load pipeline behind
--- render_status, the PR-detail and PR-diff render passes, the per-view after-render hooks,
--- and the cursor-target and untracked-hunk helpers those render passes depend on.
---
--- Reads live status state, the sibling helper/state/pr seams, and the perf/debug spans via session.lua and direct requires; uses the git backend, gh integration, and status_render as direct requires.

local git_backend = require("diff_review.git.git_backend")
local gh = require("diff_review.integrations.gh")
-- status_render edge kept lazy to avoid a load-time cycle.
local function status_render() return require("diff_review.views.status.status_render") end
local status_debug = require("diff_review.views.status.status_debug")
local pr_edit = require("diff_review.views.pr.pr_edit")
-- review edge kept lazy to avoid a load-time cycle.
local function review() return require("diff_review.views.pr.review") end
local status_issues = require("diff_review.views.status.status_issues")
local state = require("diff_review.views.status.state")
local pr_overview = require("diff_review.views.pr.pr_overview")
local actions = require("diff_review.views.status.actions")
local git_data = require("diff_review.git.git_data")
local section_builder = require("diff_review.views.status.section_builder")
local pr_state = require("diff_review.views.status.pr_state")
local status_head = require("diff_review.views.status.status_head")
local section_map = require("diff_review.views.status.section_map")
local entry_nav = require("diff_review.views.status.entry_nav")
local fold_state = require("diff_review.views.status.fold_state")
local status_helpers = require("diff_review.views.status.status_helpers")
local notifications = require("diff_review.infra.notifications")
local trace = require("diff_review.infra.perf_trace")
local session = require("diff_review.session")

local M = {}

---@param buf integer?
---@return string? target_id
---@return integer? fallback_line
local function cursor_target(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return nil, nil end
  local ok_current_buf, current_buf = pcall(vim.api.nvim_get_current_buf)
  if not ok_current_buf or current_buf ~= buf then return nil, nil end
  local ok_cursor, cursor = pcall(vim.api.nvim_win_get_cursor, 0)
  if not ok_cursor then return nil, nil end

  local line = cursor[1]
  local status = session.status
  local entry = status and status.entries and status.entries[line] or nil
  return entry and entry.id or nil, line
end

---@param file DiffReviewStatusFile
---@return DiffReviewHunk[]
local function diff_hunks_for_file(file)
  return trace.span("status.diff_hunks_for_file", session.status and session.status.buf or nil, {
    file = file and file.filename or nil,
    relpath = file and file.relpath or nil,
    existing_hunk_count = file and file.hunks and #file.hunks or nil,
    untracked = file and file.untracked or nil,
  }, function()
    if #file.hunks > 0 then return file.hunks end
    if not file.untracked then return {} end

    local relpath = session.untracked and session.untracked[file.filename]
    local diff_text = relpath and git_data._build_untracked_diff(file.filename, relpath) or nil
    if not diff_text then return {} end

    local hunks = {}
    for _, parsed_hunk in ipairs(git_data._parse_diff(diff_text, false)) do
      hunks[#hunks + 1] = {
        filename = file.filename,
        section_name = file.section_name,
        pos = parsed_hunk.pos,
        diff = parsed_hunk.diff,
        staged = false,
        context_text = parsed_hunk.context or "",
        added = parsed_hunk.added or 0,
        removed = parsed_hunk.removed or 0,
      }
    end
    return hunks
  end)
end

---@param buf integer
local function stop_markdown_highlighter(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  if vim.bo[buf].filetype ~= "GitStatus" then return end
  if vim.treesitter and type(vim.treesitter.stop) == "function" then
    pcall(vim.treesitter.stop, buf, "markdown")
  end
end

--- Run the PR overview view's after-render work (its view controller's after_render hook).
---@param buf integer
local function after_render_pr(buf)
  trace.span("status.after_render.pr_edit_on_render", buf, nil, function() pr_edit.on_render(buf) end)
  trace.span("status.after_render.review_on_render", buf, nil, function() review().on_render(buf) end)
  trace.span("status.after_render.markdown", buf, nil, function() pr_edit.render_markdown_regions(buf) end)
  trace.span("status.after_render.native_folds", buf, nil, function() fold_state._status_apply_native_folds(buf) end)
  trace.span("status.after_render.pr_sync_modifiable", buf, nil, function() pr_edit.sync_modifiable(buf) end)
  trace.span("status.after_render.schedule_native_folds", buf, nil, function() fold_state._status_schedule_native_folds(buf) end)
end

--- Run the PR review view's after-render work.
---@param buf integer
local function after_render_review(buf)
  trace.span("status.after_render.review_on_render", buf, nil, function() review().on_render(buf) end)
  trace.span("status.after_render.markdown", buf, nil, function() pr_edit.render_markdown_regions(buf) end)
  trace.span("status.after_render.native_folds", buf, nil, function() fold_state._status_apply_native_folds(buf) end)
  trace.span("status.after_render.review_sync_modifiable", buf, nil, function() review().sync_modifiable(buf) end)
  trace.span("status.after_render.schedule_native_folds", buf, nil, function() fold_state._status_schedule_native_folds(buf) end)
end

--- Run the GitStatus view's after-render work.
---@param buf integer
local function after_render_status(buf)
  trace.span("status.after_render.stop_markdown", buf, nil, function() stop_markdown_highlighter(buf) end)
  trace.span("status.after_render.native_folds", buf, nil, function() fold_state._status_apply_native_folds(buf) end)
  trace.span("status.after_render.issues_sync_modifiable", buf, nil, function() status_issues.sync_modifiable(buf) end)
end

--- Run the after-render work for branch diff and any other status-like view.
---@param buf integer
local function after_render_default(buf)
  trace.span("status.after_render.native_folds", buf, nil, function() fold_state._status_apply_native_folds(buf) end)
end

function M.render_status(buf, target_id, fallback_line, opts)
  opts = opts or {}
  status_helpers.setup_bg_highlights()
  if session.states and session.states[buf] then
    session.status = session.states[buf]
  end
  session.status = session.status or {}
  session.status.buf = buf
  session.status.reconcile_generation = (session.status.reconcile_generation or 0) + 1
  local render_state = session.status
  local preserve_current_cursor = target_id == nil and fallback_line == nil

  if opts.reuse_sections and session.status.head_lines and session.status.sections then
    if preserve_current_cursor then
      target_id, fallback_line = cursor_target(buf)
    end
    status_render().status_render_loaded(buf, target_id, fallback_line, opts, session.status.head_lines, session.status.sections)
    return
  end

  session.status.request_id = (session.status.request_id or 0) + 1
  local request_id = session.status.request_id
  local has_existing_view = session.status.head_lines ~= nil or session.status.sections ~= nil
  if not has_existing_view then
    entry_nav._status_set_plain_lines(buf, { "Loading DiffReview..." })
  end

  git_backend.git_root_async(function(cwd, root_err)
    local latest_status = session.states and session.states[buf] or render_state
    if not (latest_status and latest_status.request_id == request_id) then return end
    session.status = latest_status
    if not cwd then
      notifications.error(root_err or "Unable to find git root")
      if not has_existing_view then
        entry_nav._status_set_plain_lines(buf, { "Not a git repository" })
      end
      return
    end

    latest_status.cwd = cwd
    status_issues.ensure_state(latest_status, cwd)
    local pr_request_id = pr_state.status_ensure_pr_state(cwd, buf, opts.refresh_pr)

    section_map._status_load_async(cwd, function(result)
      local current_status = session.states and session.states[buf] or render_state
      if not (current_status and current_status.request_id == request_id) then return end
      session.status = current_status
      if not vim.api.nvim_buf_is_valid(buf) then return end
      if actions._status_operations_pending() then
        actions._status_request_reconcile(buf, target_id)
        return
      end
      if opts.restore_initial_folds then
        state.restore_initial_folds(result.sections)
        target_id = state.first_grouping_id(result.sections)
        fallback_line = nil
      end
      pr_state.status_ensure_about_state(cwd, buf, pr_state.status_has_changes(result.sections), opts.refresh_about)
      status_issues.ensure_state(current_status, cwd)
      result.head_lines = status_head._status_build_head_lines(
        result.head_values or {},
        current_status.pr,
        current_status.about,
        current_status.issues
      )
      current_status.head_lines = result.head_lines
      current_status.head_values = result.head_values
      current_status.sections = result.sections
      current_status.fancy_rows = {}
      if preserve_current_cursor and not opts.restore_initial_folds then
        target_id, fallback_line = cursor_target(buf)
      end
      status_render().status_render_loaded(buf, target_id, fallback_line, opts, result.head_lines, result.sections)
      vim.schedule(function()
        local metadata_status = session.states and session.states[buf] or render_state
        if not (
          metadata_status
          and metadata_status.request_id == request_id
          and metadata_status.cwd == cwd
          and vim.api.nvim_buf_is_valid(buf)
        ) then return end
        status_head.github_load_repo_metadata(cwd, require("github.repo_cache").repo_for_cwd(cwd))
      end)
      if pr_request_id then
        vim.defer_fn(function()
          pr_state.status_start_pr_lookup(cwd, buf, pr_request_id)
        end, 50)
      end
    end)
  end)
end

---@param buf integer
---@param target_id? string
---@param fallback_line? integer
---@param opts? { reuse_sections?: boolean, refresh_pr?: boolean, refresh_about?: boolean, restore_initial_folds?: boolean }
local function render_status_or_notify(buf, target_id, fallback_line, opts)
  local ok, err = xpcall(function()
    M.render_status(buf, target_id, fallback_line, opts)
  end, debug.traceback)
  if not ok then
    notifications.error("DiffReview render failed: " .. tostring(err))
  end
end

---@param pr DiffReviewGhPR
---@param cwd string
---@param buf integer
---@param diff_text? string
local function render_pr_status(pr, cwd, buf, diff_text)
  local status = session.status
  if not (status and status.buf == buf) then return end
  if pr_edit.blocks_render(buf) then return end
  diff_text = diff_text or status.pr_diff_text
  status_debug.event("render_pr_status.start", {
    buf = buf,
    cwd = cwd,
    pr_number = pr and pr.number or nil,
    repo = pr and pr.repo or nil,
    diff_len = #(diff_text or ""),
    comment_count = status.pr_comments and #status.pr_comments or nil,
    standalone_comment_count = status.pr_standalone_comments and #status.pr_standalone_comments or nil,
    regular_comment_count = status.pr_regular_comments and #status.pr_regular_comments or nil,
  })
  status.pr_standalone_comments = status.pr_standalone_comments or {}
  status.pr_regular_comments = status.pr_regular_comments or {}
  pr_overview.refresh_editable_comments(status)
  status.review_comment_anchor_index = section_builder.comment_anchor_index(status.review_comments)
  status.head_lines = status_head._status_pr_detail_head_lines(pr, status)
  status.sections = section_map._status_pr_sections(cwd, pr, diff_text, status.pr_comments, status.pr_standalone_comments, status.pr_regular_comments)
  local section_file_count = 0
  for _, section in ipairs(status.sections or {}) do
    section_file_count = section_file_count + #(section.files or {})
  end
  status_debug.event("render_pr_status.sections", {
    buf = buf,
    pr_number = pr and pr.number or nil,
    section_count = status.sections and #status.sections or nil,
    file_count = section_file_count,
  })
  status.fancy_rows = {}
  status.pr_code_comments_by_anchor = section_builder.comment_anchor_index_from_sections(status.sections, { field = "pr_comments" })
  status.comment_after_row = function(diff_line, indent)
    section_builder.emit_anchored_comments(status, diff_line, indent, { index_field = "pr_code_comments_by_anchor" })
  end
  status_render().status_render_loaded(buf, nil, nil, { reuse_sections = true }, status.head_lines, status.sections)
end

---@param pr DiffReviewGhPR
---@param cwd string
---@param buf integer
local function load_pr_diff(pr, cwd, buf)
  local status = session.status
  if not (status and status.buf == buf) then return end
  status.pr_diff_request_id = (status.pr_diff_request_id or 0) + 1
  local request_id = status.pr_diff_request_id
  status_debug.event("load_pr_diff.start", {
    buf = buf,
    cwd = cwd,
    request_id = request_id,
    pr_number = pr and pr.number or nil,
    repo = pr and pr.repo or nil,
  })
  gh.pr_diff_async(cwd, pr.number, pr.repo, function(result)
    local latest_status = session.states and session.states[buf] or nil
    if not (
      latest_status
      and latest_status.pr_diff_request_id == request_id
      and latest_status.buf == buf
      and vim.api.nvim_buf_is_valid(buf)
    ) then return end
    session.status = latest_status
    status_debug.event("load_pr_diff.done", {
      buf = buf,
      cwd = cwd,
      request_id = request_id,
      pr_number = pr and pr.number or nil,
      repo = pr and pr.repo or nil,
      code = result and result.code or nil,
      stdout_len = result and result.stdout and #result.stdout or nil,
      stdout_preview = result and result.stdout and result.stdout:sub(1, 800) or nil,
      stderr = result and result.stderr ~= "" and result.stderr or nil,
      output = result and result.code ~= 0 and result.output or nil,
    })
    if result.code ~= 0 then
      notifications.error("GitHub PR diff failed: " .. (result.output ~= "" and result.output or ("gh exited " .. result.code)), "DiffReview")
      return
    end
    latest_status.pr_diff_text = result.stdout or ""
    render_pr_status(pr, cwd, buf, latest_status.pr_diff_text)
  end)
end

M.render_status_or_notify = render_status_or_notify
M.render_pr_status = render_pr_status
M.load_pr_diff = load_pr_diff
M.after_render_pr = after_render_pr
M.after_render_review = after_render_review
M.after_render_status = after_render_status
M.after_render_default = after_render_default
M.stop_markdown_highlighter = stop_markdown_highlighter
M.cursor_target = cursor_target
M.diff_hunks_for_file = diff_hunks_for_file

return M
