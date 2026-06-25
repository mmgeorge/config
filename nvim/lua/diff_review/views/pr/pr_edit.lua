--- Edits a PR-view buffer's title, description, reviewers, and milestone in place,
--- queuing GitHub mutations and keeping extmark markers and markdown ranges in sync
--- as the user types.
---
--- Owns the pr_edit namespace and per-buffer edit state (markers, dirty flags, mutation
--- queue) stored on the status object, routing renders back through the active view.
---@class DiffReviewPrEditModule
local M = { ns = vim.api.nvim_create_namespace("diff_review.views.pr.pr_edit") }

local gh = require("diff_review.integrations.gh")
local config = require("diff_review.infra.config")
local notifications = require("diff_review.infra.notifications")
local region = require("diff_review.render.region")

--- Resolve the diff_review root module lazily, avoiding a load-time require cycle while
--- letting pr_edit reach status state, perf spans, and sibling views at call time.
local function dr()
  return require("diff_review")
end


---@class DiffReviewPrEditState
---@field queue fun(done: fun())[]
---@field running boolean
---@field title_mark? integer
---@field review_mark? integer
---@field milestone_mark? integer
---@field status_mark? integer
---@field desc_region? DiffReviewRenderedRegion description body region (end-exclusive bounds)
---@field title_marker_id? integer
---@field review_marker_id? integer
---@field milestone_marker_id? integer
---@field desc_marker_id? integer
---@field lock_initial? boolean
---@field initial_cursor_row? integer
---@field pending_reviewers? DiffReviewGhRequestedReviewer[]

---@param buf integer
---@param id integer?
---@return integer? row0
function M.mark_row(buf, id)
  if not id then return nil end
  local pos = vim.api.nvim_buf_get_extmark_by_id(buf, M.ns, id, {})
  return pos and pos[1] or nil
end

---@param buf integer
---@param label_pattern string
---@return integer? row0
function M.label_row(buf, label_pattern)
  if not vim.api.nvim_buf_is_valid(buf) then return nil end
  for index, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    if line:match(label_pattern) then return index - 1 end
  end
  return nil
end

---@param buf integer
---@param id integer?
---@param label_pattern string
---@return integer? row0
function M.field_row(buf, id, label_pattern)
  return M.label_row(buf, label_pattern) or M.mark_row(buf, id)
end

---@return integer namespace
function M.markdown_namespace()
  if M.render_markdown_ns then return M.render_markdown_ns end
  local ok, ui = pcall(require, "render-markdown.core.ui")
  M.render_markdown_ns = ok and ui and ui.ns or vim.api.nvim_create_namespace("render-markdown.nvim")
  return M.render_markdown_ns
end

---@param buf integer
---@return integer? first0
---@return integer? after0
function M.description_range0(buf)
  local status = dr()._status_states and dr()._status_states[buf] or nil
  local state = status and status.pr_edit or nil
  if not (state and state.desc_region) then return nil, nil end
  return region.bounds(state.desc_region)
end

---@param buf integer
---@param ranges table[]
function M.prune_markdown_ranges(buf, ranges)
  if not vim.api.nvim_buf_is_valid(buf) then return end
  local namespace = M.markdown_namespace()
  if type(ranges) ~= "table" or #ranges == 0 then
    vim.api.nvim_buf_clear_namespace(buf, namespace, 0, -1)
    return
  end
  local function row_in_range(row0)
    for _, range in ipairs(ranges) do
      if row0 >= range.first0 and row0 < range.after0 then return true end
    end
    return false
  end
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(buf, namespace, 0, -1, {})) do
    local row0 = mark[2]
    if not row_in_range(row0) then pcall(vim.api.nvim_buf_del_extmark, buf, namespace, mark[1]) end
  end
end

---@param row0 integer
---@param ranges table[]
---@return boolean
function M.row_in_markdown_range(row0, ranges)
  for _, range in ipairs(ranges or {}) do
    if row0 >= range.first0 and row0 < range.after0 then return true end
  end
  return false
end

---@param ranges table[]
---@return table[]
function M.markdown_parser_regions(ranges)
  local region = {}
  for _, range in ipairs(ranges or {}) do
    if range.first0 and range.after0 and range.after0 > range.first0 then
      region[#region + 1] = { range.first0, 0, range.after0, 0 }
    end
  end
  return #region > 0 and { region } or {}
end

---@param parser table
---@param ranges table[]
function M.apply_markdown_parser_regions(parser, ranges)
  if type(parser) ~= "table" or type(parser.set_included_regions) ~= "function" then return end
  pcall(parser.set_included_regions, parser, M.markdown_parser_regions(ranges))
end

---@param buf integer
function M.prune_description_markdown(buf)
  local first0, after0 = M.description_range0(buf)
  if first0 == nil or after0 == nil or after0 <= first0 then
    M.prune_markdown_ranges(buf, {})
    return
  end
  M.prune_markdown_ranges(buf, { { first0 = first0, after0 = after0 } })
end

---@param ranges table[]?
---@return table[]
function M.normalized_markdown_ranges(ranges)
  local normalized = {}
  for _, range in ipairs(ranges or {}) do
    local first0 = tonumber(range and range.first0)
    local after0 = tonumber(range and range.after0)
    if first0 and after0 and after0 > first0 then
      normalized[#normalized + 1] = { first0 = first0, after0 = after0 }
    end
  end
  table.sort(normalized, function(left, right)
    if left.first0 == right.first0 then return left.after0 < right.after0 end
    return left.first0 < right.first0
  end)
  local merged = {}
  for _, range in ipairs(normalized) do
    local previous = merged[#merged]
    if previous and range.first0 <= previous.after0 then
      previous.after0 = math.max(previous.after0, range.after0)
    else
      merged[#merged + 1] = range
    end
  end
  return merged
end

---@param buf integer
---@return table[]
function M.markdown_ranges0(buf)
  local ranges = {}
  local function add_range(first0, after0)
    if first0 ~= nil and after0 ~= nil and after0 > first0 then
      ranges[#ranges + 1] = { first0 = first0, after0 = after0 }
    end
  end

  local first0, after0 = M.description_range0(buf)
  add_range(first0, after0)

  local state = dr()._review.state(buf)
  if state then
    add_range(dr()._review.mark_row(buf, state.review_comment_start), dr()._review.mark_row(buf, state.review_comment_end))
    for _, comment in ipairs(state.review_comments or {}) do
      add_range(dr()._review.comment_body_range0(buf, comment))
      for _, range in ipairs(comment.review_markdown_ranges or {}) do
        add_range(dr()._review.mark_row(buf, range.start_mark), dr()._review.mark_row(buf, range.end_mark))
      end
    end
  end

  return M.normalized_markdown_ranges(ranges)
end

---@param buf integer
---@param ranges table[]
---@return fun()
function M.begin_markdown_extmark_scope(buf, ranges)
  M.markdown_extmark_scopes = M.markdown_extmark_scopes or {}
  local scope = M.markdown_extmark_scopes[buf] or { count = 0, ranges = {} }
  scope.count = scope.count + 1
  scope.ranges = M.normalized_markdown_ranges(ranges)
  M.markdown_extmark_scopes[buf] = scope

  if not M.markdown_extmark_set_extmark then
    local original_set_extmark = vim.api.nvim_buf_set_extmark
    M.markdown_extmark_original_set_extmark = original_set_extmark
    M.markdown_extmark_set_extmark = function(mark_buf, namespace, row0, col0, opts)
      local active_scope = M.markdown_extmark_scopes and M.markdown_extmark_scopes[mark_buf] or nil
      if active_scope and active_scope.count > 0 and namespace == M.markdown_namespace() then
        local mark_row0 = tonumber(row0)
        if mark_row0 and not M.row_in_markdown_range(mark_row0, active_scope.ranges) then return nil end
      end
      return original_set_extmark(mark_buf, namespace, row0, col0, opts)
    end
    vim.api.nvim_buf_set_extmark = M.markdown_extmark_set_extmark
  end

  local restored = false
  return function()
    if restored then return end
    restored = true
    local current_scope = M.markdown_extmark_scopes and M.markdown_extmark_scopes[buf] or nil
    if current_scope then
      current_scope.count = current_scope.count - 1
      if current_scope.count > 0 then
        M.markdown_extmark_scopes[buf] = current_scope
      else
        M.markdown_extmark_scopes[buf] = nil
      end
    end
    if M.markdown_extmark_scopes and next(M.markdown_extmark_scopes) == nil then
      if vim.api.nvim_buf_set_extmark == M.markdown_extmark_set_extmark then
        vim.api.nvim_buf_set_extmark = M.markdown_extmark_original_set_extmark
      end
      M.markdown_extmark_set_extmark = nil
      M.markdown_extmark_original_set_extmark = nil
    end
  end
end

---@param buf integer
---@param win integer
---@param ranges table[]
---@return table
function M.markdown_region_config(buf, win, ranges)
  local conceallevel = vim.api.nvim_get_option_value("conceallevel", { scope = "local", win = win })
  local concealcursor = vim.api.nvim_get_option_value("concealcursor", { scope = "local", win = win })
  return {
    enabled = true,
    render_modes = true,
    debounce = 0,
    completions = { lsp = { enabled = false } },
    sign = { enabled = false },
    win_options = {
      conceallevel = { default = conceallevel, rendered = conceallevel },
      concealcursor = { default = concealcursor, rendered = concealcursor },
    },
    on = {
      render = function(ctx)
        M.prune_markdown_ranges(ctx.buf, ranges)
      end,
    },
  }
end

---@param buf integer
function M.reset_render_markdown_config(buf)
  local ok, state = pcall(require, "render-markdown.state")
  if ok and type(state) == "table" and type(state.cache) == "table" then
    state.cache[buf] = nil
  end
end

---@param buf integer
---@param ranges table[]
---@return fun()
function M.begin_markdown_parser_scope(buf, ranges)
  M.markdown_parser_scopes = M.markdown_parser_scopes or {}
  M.markdown_parser_ranges = M.markdown_parser_ranges or {}
  M.markdown_parser_scopes[buf] = (M.markdown_parser_scopes[buf] or 0) + 1
  M.markdown_parser_ranges[buf] = M.normalized_markdown_ranges(ranges)
  if M.markdown_parser_get_parser then
    local restored = false
    return function()
      if restored then return end
      restored = true
      local count = (M.markdown_parser_scopes[buf] or 1) - 1
      M.markdown_parser_scopes[buf] = count > 0 and count or nil
      if count <= 0 and M.markdown_parser_ranges then M.markdown_parser_ranges[buf] = nil end
    end
  end

  local original_get_parser = vim.treesitter.get_parser
  M.markdown_parser_original_get_parser = original_get_parser
  M.markdown_parser_get_parser = function(parser_buf, lang, opts)
    local scopes = M.markdown_parser_scopes or {}
    if lang == nil and (scopes[parser_buf] or 0) > 0 then
      local parser = original_get_parser(parser_buf, "markdown", opts)
      local parser_ranges = M.markdown_parser_ranges and M.markdown_parser_ranges[parser_buf] or {}
      M.apply_markdown_parser_regions(parser, parser_ranges)
      return parser
    end
    return original_get_parser(parser_buf, lang, opts)
  end
  vim.treesitter.get_parser = M.markdown_parser_get_parser

  local restored = false
  return function()
    if restored then return end
    restored = true
    local count = (M.markdown_parser_scopes[buf] or 1) - 1
    M.markdown_parser_scopes[buf] = count > 0 and count or nil
    if count <= 0 and M.markdown_parser_ranges then M.markdown_parser_ranges[buf] = nil end
  end
end

---@param buf integer
---@param win integer
---@return table
function M.description_markdown_config(buf, win)
  local first0, after0 = M.description_range0(buf)
  return M.markdown_region_config(buf, win, M.normalized_markdown_ranges({
    { first0 = first0, after0 = after0 },
  }))
end

---@param buf integer
---@return integer?
function M.markdown_window(buf)
  if vim.api.nvim_get_current_buf() == buf then return vim.api.nvim_get_current_win() end
  local wins = vim.fn.win_findbuf(buf)
  for _, win in ipairs(wins) do
    if vim.api.nvim_win_is_valid(win) then return win end
  end
  return nil
end

---@param buf integer
function M.activate_markdown_code(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  local ok, markdown_code = pcall(require, "markdown_code")
  if ok and type(markdown_code) == "table" and type(markdown_code.activate) == "function" then
    markdown_code.activate(buf, {
      filetype = "markdown",
      notify_title = "DiffReview",
    })
  end
end

---@param buf integer
function M.render_markdown_regions(buf)
  return dr()._status_perf_span("markdown.render_regions", buf, nil, function()
    if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end

    dr()._status_perf_span("markdown.activate_code", buf, nil, function()
      M.activate_markdown_code(buf)
    end)

    local ranges = dr()._status_perf_span("markdown.compute_ranges", buf, nil, function()
      return M.markdown_ranges0(buf)
    end)
    dr()._status_perf_event("markdown.render_regions.ranges", buf, { ranges = #ranges })
    if #ranges == 0 then
      dr()._status_perf_span("markdown.prune_empty", buf, nil, function()
        M.prune_markdown_ranges(buf, ranges)
        dr()._status_stop_markdown_highlighter(buf)
      end)
      return
    end

    if M.render_markdown_unavailable then return end
    local ok, render_markdown = dr()._status_perf_span("markdown.require_render_markdown", buf, nil, function()
      return pcall(require, "render-markdown")
    end)
    if not ok or type(render_markdown) ~= "table" or type(render_markdown.render) ~= "function" then
      M.render_markdown_unavailable = true
      return
    end

    local win = dr()._status_perf_span("markdown.resolve_window", buf, nil, function()
      return M.markdown_window(buf)
    end)
    if not win then return end
    dr()._status_perf_span("markdown.reset_config", buf, { ranges = #ranges }, function()
      M.reset_render_markdown_config(buf)
    end)
    local restore_parser = dr()._status_perf_span("markdown.begin_parser_scope", buf, { ranges = #ranges }, function()
      return M.begin_markdown_parser_scope(buf, ranges)
    end)
    local restore_extmarks = dr()._status_perf_span("markdown.begin_extmark_scope", buf, { ranges = #ranges }, function()
      return M.begin_markdown_extmark_scope(buf, ranges)
    end)
    local function restore_scope()
      restore_extmarks()
      restore_parser()
    end
    local config = dr()._status_perf_span("markdown.build_config", buf, { ranges = #ranges }, function()
      return M.markdown_region_config(buf, win, ranges)
    end)
    local on_render = config.on.render
    config.on.render = function(ctx)
      if on_render then on_render(ctx) end
      restore_scope()
    end
    local render_ok, err = dr()._status_perf_span("markdown.render_markdown_render", buf, { ranges = #ranges }, function()
      return pcall(render_markdown.render, {
        buf = buf,
        win = win,
        config = config,
      })
    end)
    if render_ok then
      vim.defer_fn(function()
        dr()._status_perf_span("markdown.prune_deferred", buf, nil, function()
          M.prune_markdown_ranges(buf, M.markdown_ranges0(buf))
          restore_scope()
        end)
      end, 100)
      dr()._status_perf_span("markdown.stop_status_highlighter", buf, nil, function()
        dr()._status_stop_markdown_highlighter(buf)
      end)
    else
      restore_scope()
      if not M.render_markdown_error_notified then
        M.render_markdown_error_notified = true
        vim.notify("DiffReview markdown render failed: " .. tostring(err), vim.log.levels.WARN, { title = "DiffReview" })
      end
    end
  end)
end

---@param buf integer
function M.render_description_markdown(buf)
  M.render_markdown_regions(buf)
end

---@param buf integer
---@param status table
---@return string? title without the "Title:" label
---@return string? desc description block joined with newlines
---@return string? review reviewer-request field without the "Review:" label
---@return string? milestone milestone field without the "Release:" label or marker icon
function M.current_values(buf, status)
  local state = status.pr_edit
  if not state then return nil end
  local title_row0 = M.field_row(buf, state.title_mark, "^Title:")
  local review_row0 = M.field_row(buf, state.review_mark, "^Review:")
  local milestone_row0 = M.field_row(buf, state.milestone_mark, "^Release:")
  local first0, after0 = M.description_range0(buf)
  if not (title_row0 and review_row0 and milestone_row0 and first0 and after0) then return nil end
  local title_line = vim.api.nvim_buf_get_lines(buf, title_row0, title_row0 + 1, false)[1] or ""
  local review_line = vim.api.nvim_buf_get_lines(buf, review_row0, review_row0 + 1, false)[1] or ""
  local milestone_line = vim.api.nvim_buf_get_lines(buf, milestone_row0, milestone_row0 + 1, false)[1] or ""
  local title = vim.trim((title_line:gsub("^Title:%s*", "")))
  local review = vim.trim((review_line:gsub("^Review:%s*", "")))
  local milestone = M.milestone_value(milestone_line)
  local desc = table.concat(vim.api.nvim_buf_get_lines(buf, first0, after0, false), "\n")
  return title, desc, review, milestone
end

---@param buf integer
---@param status table
---@return boolean title_dirty
---@return boolean desc_dirty
---@return boolean review_dirty
---@return boolean milestone_dirty
function M.dirty_flags(buf, status)
  local title, desc, review, milestone = M.current_values(buf, status)
  if title == nil or not status.pr then return false, false, false, false end
  local baseline = table.concat(dr()._status_markdown_lines(status.pr.body), "\n")
  return title ~= status.pr.title,
    desc ~= baseline,
    M.reviewer_change(status, review).changed,
    M.milestone_change(status, milestone).changed
end

---@param buf integer
---@param row integer 1-based cursor row
---@return "title"|"review"|"milestone"|"desc"|nil
function M.region_kind_at(buf, row)
  local status = dr()._status_states and dr()._status_states[buf] or nil
  local state = status and status.pr_edit or nil
  if not state then return nil end
  local title_row0 = M.field_row(buf, state.title_mark, "^Title:")
  local review_row0 = M.field_row(buf, state.review_mark, "^Review:")
  local milestone_row0 = M.field_row(buf, state.milestone_mark, "^Release:")
  local first0, after0 = M.description_range0(buf)
  if title_row0 and row == title_row0 + 1 then return "title" end
  if review_row0 and row == review_row0 + 1 then return "review" end
  if milestone_row0 and row == milestone_row0 + 1 then return "milestone" end
  if first0 and after0 and row >= first0 + 1 and row <= after0 then return "desc" end
  return nil
end

---@param buf integer
---@param state DiffReviewPrEditState
function M.clear_markers(buf, state)
  for _, key in ipairs({ "title_marker_id", "review_marker_id", "milestone_marker_id", "desc_marker_id" }) do
    if state[key] then
      pcall(vim.api.nvim_buf_del_extmark, buf, M.ns, state[key])
      state[key] = nil
    end
  end
end

---Show/hide the "*" out-of-sync markers on the Title/Description labels.
---@param buf integer
function M.refresh_markers(buf)
  local status = dr()._status_states and dr()._status_states[buf] or nil
  local state = status and status.pr_edit or nil
  if not (state and vim.api.nvim_buf_is_valid(buf)) then return end
  local title_dirty, desc_dirty, review_dirty, milestone_dirty = M.dirty_flags(buf, status)
  local title_row0 = M.field_row(buf, state.title_mark, "^Title:")
  local review_row0 = M.field_row(buf, state.review_mark, "^Review:")
  local milestone_row0 = M.field_row(buf, state.milestone_mark, "^Release:")
  local first0 = M.description_range0(buf)

  local function set_marker(key, wanted, row0)
    if wanted and not state[key] and row0 then
      state[key] = vim.api.nvim_buf_set_extmark(buf, M.ns, row0, 0, {
        virt_text = { { "*", "DiffReviewPrDirty" } },
        virt_text_pos = "inline",
        right_gravity = false,
      })
    elseif not wanted and state[key] then
      pcall(vim.api.nvim_buf_del_extmark, buf, M.ns, state[key])
      state[key] = nil
    end
  end

  set_marker("title_marker_id", title_dirty, title_row0)
  set_marker("review_marker_id", review_dirty, review_row0)
  set_marker("milestone_marker_id", milestone_dirty, milestone_row0)
  set_marker("desc_marker_id", desc_dirty, first0 and first0 - 1 or nil)
end

---Re-renders are blocked while the buffer holds unsynced PR edits; they
---would clobber them.
---@param buf integer
---@return boolean
function M.blocks_render(buf)
  local status = dr()._status_states and dr()._status_states[buf] or nil
  local state = status and status.pr_edit or nil
  if not (state and status.pr and vim.api.nvim_buf_is_valid(buf)) then return false end
  local title_dirty, desc_dirty, review_dirty, milestone_dirty = M.dirty_flags(buf, status)
  if not (title_dirty or desc_dirty or review_dirty or milestone_dirty) then return false end
  vim.notify("Unsynced PR edits — :w to sync before refreshing", vim.log.levels.WARN, { title = "DiffReview" })
  return true
end

---Locate the title/description regions after a render and re-anchor the
---tracking extmarks. Rendering wipes the buffer, so this runs on every
---PR-view render.
---@param buf integer
function M.on_render(buf)
  local status = dr()._status_states and dr()._status_states[buf] or nil
  local state = status and status.pr_edit or nil
  if not (state and status.pr and vim.api.nvim_buf_is_valid(buf)) then return end
  vim.api.nvim_buf_clear_namespace(buf, M.ns, 0, -1)
  if state.desc_region then region.clear(state.desc_region) end
  state.title_mark, state.review_mark, state.milestone_mark, state.status_mark, state.desc_region = nil, nil, nil, nil, nil
  state.title_marker_id, state.review_marker_id, state.milestone_marker_id, state.desc_marker_id = nil, nil, nil, nil

  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local title_row, review_row, milestone_row, status_row, label_row
  for index, line in ipairs(lines) do
    if not title_row and line:match("^Title:") then title_row = index end
    if not review_row and line:match("^Review:") then review_row = index end
    if not milestone_row and line:match("^Release:") then milestone_row = index end
    if not status_row and line:match("^Status:") then status_row = index end
    if not label_row and line == "Description:" then label_row = index end
    if title_row and review_row and milestone_row and status_row and label_row then break end
  end
  if not (title_row and review_row and milestone_row and status_row and label_row) then return end

  -- right_gravity=false: a mark with right gravity slides to the next line
  -- when its own line is replaced (retyped), losing the region.
  local body_count = #dr()._status_markdown_lines(status.pr.body)
  state.title_mark = vim.api.nvim_buf_set_extmark(buf, M.ns, title_row - 1, 0, { right_gravity = false })
  state.review_mark = vim.api.nvim_buf_set_extmark(buf, M.ns, review_row - 1, 0, { right_gravity = false })
  state.milestone_mark = vim.api.nvim_buf_set_extmark(buf, M.ns, milestone_row - 1, 0, { right_gravity = false })
  state.status_mark = vim.api.nvim_buf_set_extmark(buf, M.ns, status_row - 1, 0, { right_gravity = false })
  state.desc_region = region.new(buf, M.ns, label_row, label_row + body_count, { end_exclusive = true, region_kind = "markdown", editable = true })
  vim.bo[buf].modified = false
  M.refresh_markers(buf)
  M.sync_modifiable(buf)
  if vim.api.nvim_get_current_buf() ~= buf then M.render_description_markdown(buf) end
end

---@param line string?
---@return string
function M.milestone_value(line)
  local value = vim.trim(tostring(line or ""):gsub("^Release:%s*", ""):gsub("^Milestone:%s*", ""))
  value = vim.trim(value:gsub("^" .. vim.pesc(dr()._milestone_icon) .. "%s*", ""))
  return value
end

---@param status table
---@param milestone string?
---@return { current: string, desired: string, changed: boolean }
function M.milestone_change(status, milestone)
  local current = dr()._pr_overview.milestone_title(status and status.pr or nil)
  local desired = vim.trim(tostring(milestone or ""))
  return {
    current = current,
    desired = desired,
    changed = current ~= desired,
  }
end

---@param milestones DiffReviewGhMilestone[]?
---@param title string
---@return DiffReviewGhMilestone?
function M.find_milestone(milestones, title)
  local desired = vim.trim(tostring(title or ""))
  if desired == "" then return nil end
  local desired_key = desired:lower()
  for _, milestone in ipairs(milestones or {}) do
    if vim.trim(tostring(milestone.title or "")):lower() == desired_key then return milestone end
  end
  return nil
end

---@param text string?
---@return string[]
function M.reviewer_usernames(text)
  local usernames = {}
  local seen = {}
  for token in tostring(text or ""):gmatch("@?[%w][%w_-]*") do
    local username = token:gsub("^@", "")
    local key = username:lower()
    if username ~= "" and not seen[key] then
      seen[key] = true
      usernames[#usernames + 1] = username
    end
  end
  return usernames
end

---@param reviewers any[]?
---@return table<string, boolean>
function M.reviewer_set(reviewers)
  local set = {}
  for _, reviewer in ipairs(reviewers or {}) do
    local username = dr()._pr_overview.reviewer_login(reviewer)
    if username ~= "" then set[username:lower()] = true end
  end
  return set
end

---@param status table
---@param review string?
---@return { current: DiffReviewGhRequestedReviewer[], desired: string[], add: string[], remove: string[], changed: boolean }
function M.reviewer_change(status, review)
  local current = status and status.pr and dr()._pr_overview.pending_reviewers(status.pr, status) or {}
  local desired = M.reviewer_usernames(review)
  local current_set = M.reviewer_set(current)
  local desired_set = M.reviewer_set(desired)
  local add = {}
  local remove = {}
  for _, reviewer in ipairs(desired) do
    if not current_set[reviewer:lower()] then add[#add + 1] = reviewer end
  end
  for _, reviewer in ipairs(current) do
    local username = dr()._pr_overview.reviewer_login(reviewer)
    if username ~= "" and not desired_set[username:lower()] then remove[#remove + 1] = username end
  end
  return {
    current = current,
    desired = desired,
    add = add,
    remove = remove,
    changed = #add > 0 or #remove > 0,
  }
end

---@param buf integer
---@param status table
---@param mark_id integer?
---@param segments table[]
---@param label_pattern string?
---@return integer? row0
function M.patch_head_field_row(buf, status, mark_id, segments, label_pattern)
  local state = status and status.pr_edit or nil
  local row0 = state and M.mark_row(buf, mark_id) or nil
  if label_pattern and vim.api.nvim_buf_is_valid(buf) then
    for index, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
      if line:match(label_pattern) then
        row0 = index - 1
        break
      end
    end
  end
  if not (row0 and status and status.pr and vim.api.nvim_buf_is_valid(buf)) then return end
  local text, segment_highlights = dr()._status_segment_line_parts(segments)
  local was_modifiable = vim.bo[buf].modifiable
  local was_modified = vim.bo[buf].modified
  local old_text = vim.api.nvim_buf_get_lines(buf, row0, row0 + 1, false)[1] or ""
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_text(buf, row0, 0, row0, #old_text, { text })
  vim.bo[buf].modifiable = was_modifiable
  vim.bo[buf].modified = was_modified
  if status.lines then status.lines[row0 + 1] = text end
  vim.api.nvim_buf_clear_namespace(buf, dr()._status_ns, row0, row0 + 1)
  for _, highlight in ipairs(segment_highlights) do
    pcall(vim.api.nvim_buf_set_extmark, buf, dr()._status_ns, row0, highlight.start_col, {
      end_col = highlight.end_col,
      hl_group = highlight.hl_group,
      priority = 90,
    })
  end
  return row0
end

---@param buf integer
---@param status table
function M.patch_review_row(buf, status)
  local state = status and status.pr_edit or nil
  local row0 = M.patch_head_field_row(buf, status, state and state.review_mark, {
    { "Review: ", "DiffReviewStatusLabel" },
    { dr()._pr_overview.pending_review_text(status and status.pr or nil, status), "DiffReviewReviewPending" },
  }, "^Review:")
  if state and row0 then
    if state.review_mark then pcall(vim.api.nvim_buf_del_extmark, buf, M.ns, state.review_mark) end
    state.review_mark = vim.api.nvim_buf_set_extmark(buf, M.ns, row0, 0, { right_gravity = false })
  end
end

---@param buf integer
---@param status table
function M.patch_milestone_row(buf, status)
  local state = status and status.pr_edit or nil
  local row0 = M.patch_head_field_row(buf, status, state and state.milestone_mark, {
    { "Release: ", "DiffReviewStatusLabel" },
    { dr()._pr_overview.milestone_text(status and status.pr or nil), "DiffReviewStatusBranch" },
  }, "^Release:")
  if state and row0 then
    if state.milestone_mark then pcall(vim.api.nvim_buf_del_extmark, buf, M.ns, state.milestone_mark) end
    state.milestone_mark = vim.api.nvim_buf_set_extmark(buf, M.ns, row0, 0, { right_gravity = false })
  end
end

---@param buf integer
---@param status table
function M.patch_status_row(buf, status)
  local state = status and status.pr_edit or nil
  local pr = status and status.pr or nil
  local row0 = M.patch_head_field_row(buf, status, state and state.status_mark, {
    { "Status: ", "DiffReviewStatusLabel" },
    { dr()._pr_overview.status_text(pr), pr and pr.isDraft and "DiffReviewStatusFetching" or "DiffReviewStatusBranch" },
  }, "^Status:")
  if state and row0 then
    if state.status_mark then pcall(vim.api.nvim_buf_del_extmark, buf, M.ns, state.status_mark) end
    state.status_mark = vim.api.nvim_buf_set_extmark(buf, M.ns, row0, 0, { right_gravity = false })
  end
end

---@param buf integer
---@param status table?
---@return integer? row0
function M.status_row(buf, status)
  local state = status and status.pr_edit or nil
  return M.field_row(buf, state and state.status_mark, "^Status:")
end

---@param buf integer
---@return boolean
function M.cursor_on_status_value(buf)
  local status = dr()._status_states and dr()._status_states[buf] or dr()._status
  local row0 = M.status_row(buf, status)
  if not row0 then return false end
  local cursor = vim.api.nvim_win_get_cursor(0)
  if cursor[1] ~= row0 + 1 then return false end
  local line = vim.api.nvim_buf_get_lines(buf, row0, row0 + 1, false)[1] or ""
  local value_start = line:find(dr()._pr_overview.status_text(status and status.pr or nil), 1, true)
  return value_start ~= nil and cursor[2] >= value_start - 1
end

---@param change { add: string[], remove: string[] }
---@return string[]
function M.reviewer_change_usernames(change)
  local usernames = {}
  local seen = {}
  for _, list in ipairs({ change.add or {}, change.remove or {} }) do
    for _, reviewer in ipairs(list) do
      local username = dr()._pr_overview.reviewer_login(reviewer)
      local key = username:lower()
      if username ~= "" and not seen[key] then
        seen[key] = true
        usernames[#usernames + 1] = username
      end
    end
  end
  return usernames
end

---@param username string
---@param users? table<string, DiffReviewGhUser>
---@return string
function M.reviewer_display(username, users)
  local user = users and users[username:lower()] or nil
  local name = user and vim.trim(user.name or "") or ""
  if name ~= "" then
    return ("@%s (%s)"):format(username, name)
  end
  return "@" .. username
end

---@param change { add: string[], remove: string[] }
---@param users? table<string, DiffReviewGhUser>
---@return string[]
function M.review_change_confirmation_lines(change, users)
  local lines = { "Confirm review request changes:" }
  if #(change.remove or {}) > 0 then
    lines[#lines + 1] = "Remove review request:"
    for _, username in ipairs(change.remove) do
      lines[#lines + 1] = "  - " .. M.reviewer_display(username, users)
    end
  end
  if #(change.add or {}) > 0 then
    lines[#lines + 1] = "Request review:"
    for _, username in ipairs(change.add) do
      lines[#lines + 1] = "  + " .. M.reviewer_display(username, users)
    end
  end
  return lines
end

---@param buf integer
function M.restore_dirty(buf)
  if not vim.api.nvim_buf_is_valid(buf) then return end
  vim.bo[buf].modified = true
  M.refresh_markers(buf)
end

---@param change { desired: string[], add: string[], remove: string[] }
---@return string
function M.review_change_summary(change)
  local parts = {}
  if #(change.remove or {}) > 0 then
    parts[#parts + 1] = "removed " .. table.concat(vim.tbl_map(function(username) return "@" .. username end, change.remove), ", ")
  end
  if #(change.add or {}) > 0 then
    parts[#parts + 1] = "requested " .. table.concat(vim.tbl_map(function(username) return "@" .. username end, change.add), ", ")
  end
  return table.concat(parts, "; ")
end

---@param buf integer
---@param status table
function M.revert_review_row(buf, status)
  if not (status and status.pr) then return end
  M.patch_review_row(buf, status)
  M.refresh_markers(buf)
  local title_dirty, desc_dirty, review_dirty, milestone_dirty = M.dirty_flags(buf, status)
  vim.bo[buf].modified = title_dirty or desc_dirty or review_dirty or milestone_dirty
end

---@param status table
---@param reviewers any[]?
function M.set_pending_reviewers(status, reviewers)
  if not (status and status.pr) then return end
  local desired = {}
  local seen = {}
  dr()._pr_overview.add_reviewers(reviewers, desired, seen)
  status.pr.requestedReviewers = desired
  if status.pr_edit then status.pr_edit.pending_reviewers = nil end
end

---@param buf integer
---@param status table
function M.revert_milestone_row(buf, status)
  if not (status and status.pr) then return end
  M.patch_milestone_row(buf, status)
  M.refresh_markers(buf)
  local title_dirty, desc_dirty, review_dirty, milestone_dirty = M.dirty_flags(buf, status)
  vim.bo[buf].modified = title_dirty or desc_dirty or review_dirty or milestone_dirty
end

---@param status table
---@param milestone DiffReviewGhMilestone?
function M.set_pr_milestone(status, milestone)
  if not (status and status.pr) then return end
  status.pr.milestone = milestone
end

---@param buf integer
---@param status table
---@param change { desired: string[], add: string[], remove: string[] }
---@param done fun(ok: boolean)
function M.apply_reviewer_change(buf, status, change, done)
  local latest = dr()._status_states and dr()._status_states[buf] or status
  if not (latest and latest.pr) then
    done(false)
    return
  end

  local function fail(message)
    notifications.error(message, "DiffReview")
    M.restore_dirty(buf)
    done(false)
  end

  local function request_added()
    if #(change.add or {}) == 0 then
      M.set_pending_reviewers(latest, change.desired)
      M.patch_review_row(buf, latest)
      vim.bo[buf].modified = false
      M.refresh_markers(buf)
      vim.notify(("Review requests updated: %s"):format(M.review_change_summary(change)), vim.log.levels.INFO, { title = "DiffReview" })
      done(true)
      return
    end
    gh.request_reviewers_async(latest.cwd, latest.pr.number, latest.pr.repo, change.add, function(result)
      if not vim.api.nvim_buf_is_valid(buf) then
        done(false)
        return
      end
      if not result.ok then
        fail("GitHub reviewer request failed: " .. (result.message or "gh failed"))
        return
      end
      M.set_pending_reviewers(latest, change.desired)
      M.patch_review_row(buf, latest)
      vim.bo[buf].modified = false
      M.refresh_markers(buf)
      vim.notify(("Review requests updated: %s"):format(M.review_change_summary(change)), vim.log.levels.INFO, { title = "DiffReview" })
      done(true)
    end)
  end

  if #(change.remove or {}) == 0 then
    request_added()
    return
  end
  gh.remove_reviewers_async(latest.cwd, latest.pr.number, latest.pr.repo, change.remove, function(result)
    if not vim.api.nvim_buf_is_valid(buf) then
      done(false)
      return
    end
    if not result.ok then
      fail("GitHub reviewer removal failed: " .. (result.message or "gh failed"))
      return
    end
    request_added()
  end)
end

---@param change { desired: string[], add: string[], remove: string[] }
---@param done fun(ok: boolean)
function M.confirm_and_apply_reviewer_change(buf, status, change, done)
  if not change.changed then
    done(false)
    return
  end

  local usernames = M.reviewer_change_usernames(change)
  gh.resolve_users_async(status.cwd, usernames, function(user_result)
    if not vim.api.nvim_buf_is_valid(buf) then
      done(false)
      return
    end
    local lines = M.review_change_confirmation_lines(change, user_result.users or {})
    dr()._confirm(lines, function()
      M.apply_reviewer_change(buf, status, change, done)
    end, function()
      local latest = dr()._status_states and dr()._status_states[buf] or status
      M.revert_review_row(buf, latest)
      done(false)
    end)
  end)
end

---@param change { desired: string }
---@return string[]
function M.milestone_create_confirmation_lines(change)
  return {
    "Release not found:",
    "  " .. change.desired,
    "",
    "Create it now?",
  }
end

---@param change { desired: string }
---@return string
function M.milestone_change_summary(change)
  if change.desired == "" then return "cleared" end
  return dr()._milestone_icon .. " " .. change.desired
end

---@param buf integer
---@param status table
---@param milestone DiffReviewGhMilestone?
---@param done fun(ok: boolean)
function M.apply_milestone(buf, status, milestone, done)
  local latest = dr()._status_states and dr()._status_states[buf] or status
  if not (latest and latest.pr) then
    done(false)
    return
  end

  local milestone_number = milestone and tonumber(milestone.number) or nil
  if milestone and (not milestone_number or milestone_number <= 0) then
    notifications.error("GitHub milestone is missing its number", "DiffReview")
    M.restore_dirty(buf)
    done(false)
    return
  end

  gh.set_pr_milestone_async(latest.cwd, latest.pr.number, latest.pr.repo, milestone_number, function(result)
    if not vim.api.nvim_buf_is_valid(buf) then
      done(false)
      return
    end
    if not result.ok then
      notifications.error("GitHub milestone update failed: " .. (result.message or "gh failed"), "DiffReview")
      M.restore_dirty(buf)
      done(false)
      return
    end
    M.set_pr_milestone(latest, milestone or result.milestone)
    M.patch_milestone_row(buf, latest)
    vim.bo[buf].modified = false
    M.refresh_markers(buf)
    vim.notify(("Release updated: %s"):format(M.milestone_change_summary({
      desired = dr()._pr_overview.milestone_title(latest.pr),
    })), vim.log.levels.INFO, { title = "DiffReview" })
    done(true)
  end)
end

---@param buf integer
---@param status table
---@param change { current: string, desired: string, changed: boolean }
---@param done fun(ok: boolean)
function M.confirm_and_apply_milestone_change(buf, status, change, done)
  if not change.changed then
    done(false)
    return
  end

  if change.desired == "" then
    M.apply_milestone(buf, status, nil, done)
    return
  end

  gh.repo_milestones_async(status.cwd, status.pr.repo, function(result)
    if not vim.api.nvim_buf_is_valid(buf) then
      done(false)
      return
    end
    if not result.ok then
      notifications.error("GitHub milestones lookup failed: " .. (result.message or "gh failed"), "DiffReview")
      M.restore_dirty(buf)
      done(false)
      return
    end

    local milestone = M.find_milestone(result.milestones or {}, change.desired)
    if milestone then
      M.apply_milestone(buf, status, milestone, done)
      return
    end

    dr()._confirm(M.milestone_create_confirmation_lines(change), function()
      local latest = dr()._status_states and dr()._status_states[buf] or status
      if not (latest and latest.pr) then
        done(false)
        return
      end
      gh.create_milestone_async(latest.cwd, latest.pr.repo, change.desired, function(create_result)
        if not vim.api.nvim_buf_is_valid(buf) then
          done(false)
          return
        end
        if not create_result.ok or not create_result.milestone then
          notifications.error("GitHub milestone create failed: " .. (create_result.message or "gh failed"), "DiffReview")
          M.restore_dirty(buf)
          done(false)
          return
        end
        M.apply_milestone(buf, latest, create_result.milestone, done)
      end)
    end, function()
      local latest = dr()._status_states and dr()._status_states[buf] or status
      M.revert_milestone_row(buf, latest)
      done(false)
    end)
  end)
end

---@param pr DiffReviewGhPR
---@param desired_draft boolean
---@return string[]
function M.draft_status_confirmation_lines(pr, desired_draft)
  if desired_draft then
    return {
      ("Move PR #%s back to draft?"):format(tostring(pr.number)),
      "",
      "Status will change from READY to DRAFT.",
    }
  end
  return {
    ("Mark PR #%s ready for review?"):format(tostring(pr.number)),
    "",
    "Status will change from DRAFT to READY.",
  }
end

---@param status table
---@return string
function M.pr_node_id(status)
  local pr = status and status.pr or nil
  return type(pr and pr.id) == "string" and pr.id or ""
end

---@param buf integer
---@param status table
---@param desired_draft boolean
---@param done fun(ok: boolean)
function M.apply_draft_status(buf, status, desired_draft, done)
  local latest = dr()._status_states and dr()._status_states[buf] or status
  if not (latest and latest.pr) then
    done(false)
    return
  end

  local pr_node_id = M.pr_node_id(latest)
  if pr_node_id == "" then
    notifications.error("GitHub draft status update failed: missing pull request node id", "DiffReview")
    done(false)
    return
  end

  gh.set_pr_draft_async(latest.cwd, pr_node_id, desired_draft, function(result)
    if not vim.api.nvim_buf_is_valid(buf) then
      done(false)
      return
    end
    if not result.ok then
      notifications.error("GitHub draft status update failed: " .. (result.message or "gh failed"), "DiffReview")
      done(false)
      return
    end
    latest.pr.isDraft = type(result.is_draft) == "boolean" and result.is_draft or desired_draft
    M.patch_status_row(buf, latest)
    vim.notify(("PR #%s status updated: %s"):format(tostring(latest.pr.number), dr()._pr_overview.status_text(latest.pr)), vim.log.levels.INFO, { title = "DiffReview" })
    done(true)
  end)
end

---@param buf integer
---@return boolean
function M.toggle_draft_status_under_cursor(buf)
  if not M.cursor_on_status_value(buf) then return false end
  local status = dr()._status_states and dr()._status_states[buf] or dr()._status
  local state = status and status.pr_edit or nil
  if not (status and status.pr and state) then return true end
  local desired_draft = not status.pr.isDraft
  dr()._confirm(M.draft_status_confirmation_lines(status.pr, desired_draft), function()
    M.enqueue(state, function(done)
      M.apply_draft_status(buf, status, desired_draft, function()
        done()
      end)
    end)
  end)
  return true
end

---Sequential sync queue, mirroring status_enqueue_operation without its
---status-view reconcile tail (which would rebuild the PR buffer as a
---status view).
---@param state DiffReviewPrEditState
---@param operation fun(done: fun())
function M.enqueue(state, operation)
  state.queue[#state.queue + 1] = operation
  local function run_next()
    if state.running then return end
    local next_operation = table.remove(state.queue, 1)
    if not next_operation then return end
    state.running = true
    next_operation(function()
      state.running = false
      run_next()
    end)
  end
  run_next()
end

---BufWriteCmd handler: clear the "*" markers immediately, then sync the
---dirty fields to GitHub through the queue.
---@param buf integer
function M.sync(buf)
  local status = dr()._status_states and dr()._status_states[buf] or nil
  local state = status and status.pr_edit or nil
  if not (status and state and status.pr) then return end
  dr()._status = status
  local title, desc, review, milestone = M.current_values(buf, status)
  local title_dirty, desc_dirty, review_dirty, milestone_dirty = M.dirty_flags(buf, status)
  vim.bo[buf].modified = false
  if not (title_dirty or desc_dirty or review_dirty or milestone_dirty) then
    M.patch_review_row(buf, status)
    M.patch_milestone_row(buf, status)
    M.patch_status_row(buf, status)
    return
  end

  M.clear_markers(buf, state)
  local pr = status.pr
  local function sync_title_and_description()
    if not (title_dirty or desc_dirty) then return end
    local edit = {
      title = title_dirty and title or nil,
      body = desc_dirty and desc or nil,
    }
    M.enqueue(state, function(done)
      gh.update_pr_async(status.cwd, pr.number, pr.repo, edit, function(result)
        if not vim.api.nvim_buf_is_valid(buf) then
          done()
          return
        end
        if result.code ~= 0 then
          notifications.error(
            "GitHub PR update failed: " .. (result.output ~= "" and result.output or ("gh exited " .. tostring(result.code))),
            "DiffReview"
          )
          vim.bo[buf].modified = true
          M.refresh_markers(buf)
          done()
          return
        end
        if edit.title then pr.title = edit.title end
        if edit.body then pr.body = edit.body end
        local latest = dr()._status_states and dr()._status_states[buf] or nil
        if latest == status then
          dr()._status = status
          dr()._render_pr_status(pr, status.cwd, buf, status.pr_diff_text)
        end
        vim.notify(("PR #%s updated"):format(tostring(pr.number)), vim.log.levels.INFO, { title = "DiffReview" })
        done()
      end)
    end)
  end

  local function sync_milestone_then_title_and_description()
    if milestone_dirty then
      M.confirm_and_apply_milestone_change(buf, status, M.milestone_change(status, milestone), function(confirmed)
        if confirmed then sync_title_and_description() end
      end)
    else
      M.patch_milestone_row(buf, status)
      sync_title_and_description()
    end
  end

  if review_dirty then
    M.confirm_and_apply_reviewer_change(buf, status, M.reviewer_change(status, review), function(confirmed)
      if confirmed then sync_milestone_then_title_and_description() end
    end)
  else
    M.patch_review_row(buf, status)
    sync_milestone_then_title_and_description()
  end
end

---Unlock the buffer exactly when the cursor sits in an editable region, so
---every native editing command works there and nowhere else.
---@param buf integer
function M.sync_modifiable(buf)
  if not vim.api.nvim_buf_is_valid(buf) then return end
  if vim.api.nvim_get_current_buf() ~= buf then return end
  return dr()._status_perf_span("pr_edit.sync_modifiable", buf, nil, function()
    local row = dr()._status_perf_span("pr_edit.normalize_status_cursor", buf, nil, function()
      return dr()._normalize_status_cursor(buf)
    end) or vim.api.nvim_win_get_cursor(0)[1]
    local status = dr()._status_states and dr()._status_states[buf] or nil
    local state = status and status.pr_edit or nil
    local region_kind = dr()._status_perf_span("pr_edit.region_kind_at", buf, { row = row }, function()
      return M.region_kind_at(buf, row)
    end)
    local review_editable = dr()._status_perf_span("pr_edit.review_in_editable_region", buf, { row = row }, function()
      return dr()._review.in_editable_region(buf, row)
    end)
    local wanted = region_kind ~= nil or review_editable
    if state and state.lock_initial then
      if state.initial_cursor_row == nil then state.initial_cursor_row = row end
      if row == state.initial_cursor_row then
        wanted = false
      else
        state.lock_initial = false
      end
    end
    if vim.bo[buf].modifiable ~= wanted then
      vim.bo[buf].modifiable = wanted
    end
    dr()._status_perf_span("pr_edit.render_description_markdown", buf, {
      row = row,
      editable = wanted,
      region_kind = region_kind,
    }, function()
      M.render_description_markdown(buf)
    end)
  end)
end

---Wire editing into a freshly created PR-view buffer: acwrite, the
---cursor-follows-modifiable lock, and lifecycle autocmds.
---@param buf integer
function M.attach(buf)
  local status = dr()._status_states and dr()._status_states[buf] or nil
  if not status then return end
  status.pr_edit = { queue = {}, running = false, lock_initial = true }
  vim.bo[buf].buftype = "acwrite"
  require("github.repo_cache").enable_user_completion(buf, status.pr and status.pr.repo or nil)

  -- The title, reviewer request, and milestone fields are single-line: swallow newline attempts there.
  vim.keymap.set("i", "<CR>", function()
    local row = vim.api.nvim_win_get_cursor(0)[1]
    local region = M.region_kind_at(buf, row)
    if region == "title" or region == "review" or region == "milestone" then return "" end
    return vim.api.nvim_replace_termcodes("<CR>", true, false, true)
  end, { buffer = buf, expr = true })

  local group = vim.api.nvim_create_augroup("DiffReviewPrEdit" .. buf, { clear = true })
  vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI", "BufEnter" }, {
    group = group,
    buffer = buf,
    callback = function()
      dr()._status_perf_span("pr_edit.autocmd_sync_modifiable", buf, nil, function()
        M.sync_modifiable(buf)
      end)
    end,
  })
  vim.api.nvim_create_autocmd({ "InsertLeave", "TextChanged", "TextChangedI" }, {
    group = group,
    buffer = buf,
    callback = function()
      dr()._status_perf_span("pr_edit.autocmd_text_changed", buf, nil, function()
        dr()._review.sync_inline_comment_text(buf)
        M.refresh_markers(buf)
        M.render_description_markdown(buf)
      end)
    end,
  })
  vim.api.nvim_create_autocmd("BufWriteCmd", {
    group = group,
    buffer = buf,
    callback = function()
      dr()._review.sync_inline_comment_text(buf)
      dr()._pr_overview.sync_standalone_comments(buf)
      M.sync(buf)
    end,
  })
end

return M
