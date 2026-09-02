--- Owns cursor and entry navigation for the status views: resolving the entry under the cursor,
--- parent/file/hunk relationships, source-policy gates, visual-selection entry sets, action-target
--- resolution, decoration-row prewarm/scheduling, and cursor restore after a re-render.
---
--- Reads live status state, the diff-source/key/decoration models, and the syntax prewarm seam
--- from session.lua and sibling modules.

local syntax_engine = require("diff_review.render.syntax_engine")

local git_data = require("diff_review.git.git_data")
local source = require("diff_review.render.source")
-- diff_source_state edge kept lazy to avoid a load-time cycle.
local function diff_source_state() return require("diff_review.views.status.diff_source_state") end
-- status_render edge kept lazy to avoid a load-time cycle.
local function status_render() return require("diff_review.views.status.status_render") end
-- commit_view edge kept lazy to avoid a load-time cycle.
local function commit_view() return require("diff_review.views.status.commit_view") end
local diff_buffer = require("diff_review.views.diff_buffer")
local status_keys = require("diff_review.views.status.status_keys")
local status_buffer = require("diff_review.views.status.status_buffer")
-- keymaps edge kept lazy to avoid a load-time cycle.
local function keymaps() return require("diff_review.shared.keymaps") end
local notifications = require("diff_review.infra.notifications")
local trace = require("diff_review.infra.perf_trace")
local ui = require("diff_review.infra.ui")
local session = require("diff_review.session")

local M = {}

local status_cursor_prewarm_delta_limit = 100

--- Resolves the status entry descriptor at the current cursor line.
---@param state? table Optional status session state table.
---@return DiffReviewStatusEntry? entry Matching status entry descriptor, or nil.
local function status_entry_under_cursor(state)
  local status = state or session.status
  if not status then return nil end
  local line = vim.api.nvim_win_get_cursor(0)[1]
  return status.entries[line]
end

--- Scans upwards from cursor line to find the nearest defined status entry and line index.
---@return integer? line One-based buffer line index.
---@return DiffReviewStatusEntry? entry Matching status entry descriptor.
function M._status_entry_line_under_cursor()
  local status = session.status
  if not (status and status.entries and status.buf and vim.api.nvim_buf_is_valid(status.buf)) then return nil, nil end
  local cursor_line = vim.api.nvim_win_get_cursor(0)[1]
  local entries = status.entries
  local max_line = vim.api.nvim_buf_line_count(status.buf)
  for line = math.min(cursor_line, max_line), 1, -1 do
    local entry = entries[line]
    if entry then return line, entry end
  end
  return nil, nil
end

--- Resolves the action capability policy for an entry's underlying diff source.
---@param status table? Status session state table.
---@param entry DiffReviewStatusEntry? Status entry descriptor.
---@return table? policy Action capability policy map, or nil.
function M._status_source_policy_for_entry(status, entry)
  if not (status and status.diff_source_registry and entry) then return nil end
  local file = entry.file
  local source_id = file and file.diff_source_id or nil
  if not source_id and entry.hunk and entry.hunk.section_name then source_id = entry.hunk.section_name end
  if not source_id and entry.kind == "pr_hunk" and status.pr then source_id = "pr:" .. tostring(status.pr.number) .. ":changes" end
  if not source_id and entry.kind == "pr_review_hunk" then source_id = "review:unviewed" end
  if not source_id then return nil end
  return source.policy(status.diff_source_registry, source_id)
end

--- Checks whether diff source policy permits the specified command for the cursor entry.
---@param status table? Status session state table.
---@param command string Command identifier string.
---@return boolean allowed True if command is permitted by source policy.
function M._status_source_policy_allows_cursor(status, command)
  if not (status and status.entries) then return true end
  local _, entry = M._status_entry_line_under_cursor()
  local policy = M._status_source_policy_for_entry(status, entry)
  if not policy then return true end
  return policy[command] == true
end

--- Checks whether an entry represents a file or file header row.
---@param entry DiffReviewStatusEntry? Status entry descriptor.
---@return boolean is_file True if entry represents a file item.
function M._status_entry_is_file_like(entry)
  return entry ~= nil
    and (entry.kind == "file" or entry.kind == "commit_file" or entry.kind == "pr_file" or entry.kind == "pr_review_file")
end

--- Determines whether cursor hover may prewarm file diff syntax based on delta thresholds.
---@param entry DiffReviewStatusEntry Status entry descriptor.
---@return boolean allowed True if syntax prewarming is allowed.
local function status_file_cursor_prewarm_allowed(entry)
  local file = entry.file
  if not file then return false end
  if status_buffer.folded(session.status or {}, entry.id, entry.default_folded) == false then return true end
  if file.line_stats_complete == false then return false end

  local added = tonumber(file.added)
  local removed = tonumber(file.removed)
  if not (added and removed) then return false end
  local delta = math.max(0, math.floor(added)) + math.max(0, math.floor(removed))
  return delta < status_cursor_prewarm_delta_limit
end

--- Checks whether an entry represents a diff hunk row.
---@param entry DiffReviewStatusEntry? Status entry descriptor.
---@return boolean is_hunk True if entry represents a diff hunk.
function M._status_entry_is_hunk_like(entry)
  return entry ~= nil
    and (entry.kind == "hunk" or entry.kind == "commit_hunk" or entry.kind == "pr_hunk" or entry.kind == "pr_review_hunk")
end

--- Searches upward to find the parent file, section, or commit containing current entry.
---@param current_line integer One-based line index of current entry.
---@param entry DiffReviewStatusEntry Current status entry descriptor.
---@return DiffReviewStatusEntry? parent Parent entry descriptor, or nil.
function M._status_parent_entry(current_line, entry)
  local status = session.status
  if not (status and status.entries) then return nil end
  local viewport = status.diff_viewport
  local entries = viewport and viewport.enabled and viewport.logical_entries or status.entries
  for line = current_line - 1, 1, -1 do
    local candidate = entries[line]
    if entry.fold_target_id and candidate and candidate.id == entry.fold_target_id then return candidate end
    if entry.kind == "commit_hunk" and candidate and candidate.kind == "commit_file" then return candidate end
    if M._status_entry_is_hunk_like(entry) and M._status_entry_is_file_like(candidate) then return candidate end
    if M._status_entry_is_file_like(entry) and candidate and candidate.kind == "pr_review" then return candidate end
    if M._status_entry_is_file_like(entry) and candidate and candidate.kind == "commit" then return candidate end
    if (M._status_entry_is_file_like(entry) or entry.kind == "commit" or entry.kind == "pr_review") and candidate and candidate.kind == "section" then
      return candidate
    end
  end
  return nil
end

--- Prewarms Tree-sitter syntax highlighting for an entry's diff contents.
---@param entry DiffReviewStatusEntry? Status entry descriptor.
local function status_prewarm_entry_syntax(entry)
  return trace.span("status.prewarm_entry_syntax", session.status and session.status.buf or nil, {
    entry_id = entry and entry.id or nil,
    entry_kind = entry and entry.kind or nil,
    file = entry and entry.file and entry.file.filename or nil,
  }, function()
    if not entry then return end
    if entry.preview_omitted then return end
    if entry.file and git_data._status_file_is_deleted(entry.file) then return end
    if M._status_entry_is_file_like(entry) and entry.file then
      if not status_file_cursor_prewarm_allowed(entry) then return end
      local syntax_source = syntax_engine.status_syntax_source_for_entry_kind(entry.kind)
      syntax_engine.prewarm_file_diff_syntax(entry.file, "status-cursor-prewarm:" .. (entry.id or entry.file.filename), nil, { syntax_source = syntax_source })
    elseif M._status_entry_is_hunk_like(entry) and entry.file and entry.hunk then
      local callback_key = "status-cursor-prewarm:" .. (entry.id or entry.file.filename)
      local syntax_source = syntax_engine.status_syntax_source_for_entry_kind(entry.kind)
      local syntax_diff_text = nil
      if syntax_source == "file" then
        syntax_diff_text = trace.span("status.prewarm_entry_syntax.hunk_combined_diff", session.status and session.status.buf or nil, {
          entry_id = entry.id,
          entry_kind = entry.kind,
          file = entry.file.filename,
        }, function()
          return syntax_engine.status_file_syntax_diff_text(entry.file)
        end)
      end
      syntax_engine.prewarm_diff_syntax(entry.file.filename, entry.hunk.diff, { entry.hunk.staged }, callback_key, nil, {
        syntax_source = syntax_source,
        syntax_diff_text = syntax_diff_text,
      })
    end
  end)
end

--- Maps a one-based buffer row to a row decoration request record.
---@param buf integer Buffer handle.
---@param row integer One-based buffer line index.
---@return DiffReviewRowDecorationRequest? request Decoration request table, or nil.
function M._status_resolve_decoration_row(buf, row)
  local status = session.states and session.states[buf] or nil
  if not (status and status.entries) then return nil end
  local entry = status.entries[row]
  if not entry then return nil end
  local diff_line = (entry.diff_lines and entry.diff_lines[1]) or entry.diff_line
  local file = entry.file
  if not (diff_line and file) then return nil end
  local source_id = diff_source_state()._status_diff_source_id(file, entry.kind)
  return {
    file_key = source.file_key(source_id, file.relpath or file.filename),
    revision = status.render_revision or 0,
    line = diff_line.line,
    side = diff_line.side == "left" and "old" or "new",
    kind = entry.kind,
  }
end

--- Debounces a visible window syntax decoration request across a row range.
---@param buf integer Buffer handle.
---@param first_row integer One-based starting buffer line index.
---@param last_row integer One-based ending buffer line index.
function M._status_schedule_decorate_visible(buf, first_row, last_row)
  local status = session.states and session.states[buf] or nil
  if not status then return end
  if status.decorate_first == first_row and status.decorate_last == last_row then return end
  status.decorate_first = first_row
  status.decorate_last = last_row
  status.decorate_request_id = (status.decorate_request_id or 0) + 1
  local request_id = status.decorate_request_id
  vim.defer_fn(function()
    local current = session.states and session.states[buf] or nil
    if not (current and current.decorate_request_id == request_id) then return end
    if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
    status_render().status_decorate_visible(buf, first_row, last_row)
  end, 30)
end

--- Schedules deferred syntax prewarming for the entry currently under cursor.
---@param buf integer Buffer handle.
local function status_defer_prewarm_under_cursor(buf)
  local status = session.states and session.states[buf] or session.status
  if not status then return end
  status.cursor_prewarm_request_id = (status.cursor_prewarm_request_id or 0) + 1
  local request_id = status.cursor_prewarm_request_id
  local entry = status_entry_under_cursor()
  local entry_id = entry and entry.id or nil
  trace.event("status.cursor_prewarm_schedule", buf, {
    request_id = request_id,
    entry_id = entry_id,
    entry_kind = entry and entry.kind or nil,
  })

  vim.defer_fn(function()
    local latest_status = session.states and session.states[buf] or session.status
    if not (latest_status and latest_status.cursor_prewarm_request_id == request_id) then return end
    if not (buf and vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_get_current_buf() == buf) then return end
    trace.span("status.cursor_prewarm_run", buf, {
      request_id = request_id,
      scheduled_entry_id = entry_id,
    }, function()
      session.status = latest_status
      local current_entry = status_entry_under_cursor()
      if entry_id and current_entry and current_entry.id == entry_id then
        status_prewarm_entry_syntax(current_entry)
      end
    end)
  end, 35)
end

local status_files_from_set


--- Collects unique status entries across a line range in the status buffer.
---@param start_line integer Starting buffer line number.
---@param end_line integer Ending buffer line number.
---@return DiffReviewStatusEntry[] entries Array of unique status entries.
local function status_entries_for_lines(start_line, end_line)
  local status = session.status
  if not status then return {} end
  if start_line > end_line then
    start_line, end_line = end_line, start_line
  end

  local entries_by_line = status.entries

  local entries = {}
  local seen = {}
  for line = start_line, end_line do
    local entry = entries_by_line[line]
    if entry and entry.id and not seen[entry.id] then
      seen[entry.id] = true
      entries[#entries + 1] = entry
    end
  end
  return entries
end

--- Captures the active visual selection line range and associated status entries.
---@return DiffReviewVisualSelection selection Visual selection record.
local function status_visual_selection()
  local mode = vim.fn.mode()
  local in_visual_mode = mode == "v" or mode == "V" or mode:byte() == 22
  local start_line
  local end_line
  if in_visual_mode then
    start_line = vim.fn.line("v")
    end_line = vim.api.nvim_win_get_cursor(0)[1]
  else
    local start_pos = vim.fn.getpos("'<")
    local end_pos = vim.fn.getpos("'>")
    start_line = start_pos[2]
    end_line = end_pos[2]
  end
  if start_line > end_line then start_line, end_line = end_line, start_line end
  return {
    buf = vim.api.nvim_get_current_buf(),
    entries = status_entries_for_lines(start_line, end_line),
    start_line = start_line,
    end_line = end_line,
  }
end

--- Exits Neovim visual mode and clears selection marks.
local function status_leave_visual_mode()
  local mode = vim.api.nvim_get_mode().mode
  if mode == "v" or mode == "V" or mode:byte() == 22 then
    local esc = vim.api.nvim_replace_termcodes("<Esc>", true, false, true)
    vim.api.nvim_feedkeys(esc, "nx", false)
  end
end

--- Expands a section entry into individual file entries, or returns the entry as a single-element list.
---@param entry DiffReviewStatusEntry? Status entry descriptor.
---@return DiffReviewStatusEntry[] entries Array of status file entries.
local function status_file_entries_for_entry(entry)
  if not entry then return {} end
  if entry.kind == "section" then
    local entries = {}
    for _, file in ipairs(entry.section.files or {}) do
      entries[#entries + 1] = { id = status_keys.file_key(file.section_name, file.filename), kind = "file", file = file }
    end
    return entries
  end
  return { entry }
end

--- Resolves the unique file scoping identifier for an entry.
---@param entry DiffReviewStatusEntry Status entry descriptor.
---@return string? scope Unique file scope string, or nil.
local function status_file_scope(entry)
  local file = entry.file
  if not (file and file.filename and file.section_name) then return nil end
  return status_keys.file_key(file.section_name, file.filename)
end

--- Builds a de-duplicated action entry list from selected rows, expanding sections and filtering covered hunks.
---@param entries DiffReviewStatusEntry[] Raw selected entry array.
---@return DiffReviewStatusEntry[] action_entries Normalized action entries array.
local function status_action_entries(entries)
  local expanded_entries = {}
  local seen = {}
  for _, selected_entry in ipairs(entries or {}) do
    for _, entry in ipairs(status_file_entries_for_entry(selected_entry)) do
      local id = entry.id or ("%s:%s"):format(entry.kind or "entry", (entry.file and entry.file.filename) or "")
      if not seen[id] then
        seen[id] = true
        expanded_entries[#expanded_entries + 1] = entry
      end
    end
  end

  local selected_file_scope = {}
  for _, entry in ipairs(expanded_entries) do
    local file_scope = entry.kind == "file" and status_file_scope(entry) or nil
    if file_scope then selected_file_scope[file_scope] = true end
  end

  local action_entries = {}
  for _, entry in ipairs(expanded_entries) do
    local covered_by_file = entry.kind == "hunk" and selected_file_scope[status_file_scope(entry) or ""] == true
    if not covered_by_file then action_entries[#action_entries + 1] = entry end
  end
  return action_entries
end

--- Extracts a sorted array of filenames from a set dictionary table.
---@param file_set table<string, boolean> Dictionary of filename keys.
---@return string[] files Sorted array of filename strings.
function status_files_from_set(file_set)
  local files = {}
  for filename in pairs(file_set) do
    files[#files + 1] = filename
  end
  table.sort(files)
  return files
end

--- Counts total key-value pairs in a table.
---@param items table<any, any> Hash table.
---@return integer count Number of entries.
local function status_count_set(items)
  local count = 0
  for _ in pairs(items) do
    count = count + 1
  end
  return count
end

--- Emits a debug notification summarizing the number of affected hunks and files.
---@param action string Action label prefix.
---@param hunk_count integer Number of hunks modified.
---@param file_count integer Number of files modified.
local function status_notify_action(action, hunk_count, file_count)
  if hunk_count <= 0 and file_count <= 0 then return end
  local parts = {}
  if hunk_count > 0 then
    parts[#parts + 1] = ("%d hunk(s)"):format(hunk_count)
  end
  if file_count > 0 then
    parts[#parts + 1] = ("%d file(s)"):format(file_count)
  end
  notifications.debug(("%s %s"):format(action, table.concat(parts, ", ")), vim.log.levels.INFO, { title = "DiffReview" })
end

--- Identifies whether an entry acts at the file or hunk granularity level.
---@param entry DiffReviewStatusEntry? Status entry descriptor.
---@return "file"|"hunk"|nil granularity Action granularity category.
local function status_entry_granularity(entry)
  if M._status_entry_is_file_like(entry) then return "file" end
  if M._status_entry_is_hunk_like(entry) then return "hunk" end
  return nil
end

--- Extracts the section category name from a file or section entry.
---@param entry DiffReviewStatusEntry? Status entry descriptor.
---@return string? section_name Section name string, or nil.
local function status_entry_section_name(entry)
  return entry and ((entry.file and entry.file.section_name) or (entry.section and entry.section.name)) or nil
end

--- Resolves the next surviving semantic sibling entry after a visual selection action.
---@param selection DiffReviewVisualSelection Visual selection record.
---@param action_entries DiffReviewStatusEntry[] Array of entries targeted by action.
---@return DiffReviewListCursorTarget? target Next cursor target descriptor, or nil.
local function status_visual_action_cursor_target(selection, action_entries)
  local status = session.status
  if not (status and status.entries and status.buf == selection.buf) then return nil end

  local action_ids = {}
  local action_file_set = {}
  local granularity = nil
  for _, entry in ipairs(action_entries or {}) do
    if entry.id then action_ids[entry.id] = true end
    if M._status_entry_is_file_like(entry) then
      granularity = "file"
      if entry.file and entry.file.filename then action_file_set[entry.file.filename] = true end
    end
    if not granularity and M._status_entry_is_hunk_like(entry) then granularity = "hunk" end
  end
  if not granularity then return nil end

  local function entry_is_action(entry)
    if not entry then return false end
    if entry.id and action_ids[entry.id] then return true end
    return granularity == "file"
      and entry.file ~= nil
      and action_file_set[entry.file.filename] == true
  end

  local last_section = nil
  for line = selection.end_line, selection.start_line, -1 do
    local entry = status.entries[line]
    if entry_is_action(entry) and status_entry_granularity(entry) == granularity then
      last_section = status_entry_section_name(entry)
      break
    end
  end
  local first_section = nil
  for line = selection.start_line, selection.end_line do
    local entry = status.entries[line]
    if entry_is_action(entry) and status_entry_granularity(entry) == granularity then
      first_section = status_entry_section_name(entry)
      break
    end
  end

  local max_line = vim.api.nvim_buf_line_count(status.buf)
  for line = selection.end_line + 1, max_line do
    local entry = status.entries[line]
    if entry and entry.id and not entry_is_action(entry)
        and status_entry_granularity(entry) == granularity
        and status_entry_section_name(entry) == last_section then
      return { buf = selection.buf, id = entry.id, fallback_line = line }
    end
  end
  for line = selection.start_line - 1, 1, -1 do
    local entry = status.entries[line]
    if entry and entry.id and not entry_is_action(entry)
        and status_entry_granularity(entry) == granularity
        and status_entry_section_name(entry) == first_section then
      return { buf = selection.buf, id = entry.id, fallback_line = line }
    end
  end
  return nil
end

--- Checks whether all entries in the given array represent headers or file rows.
---@param entries DiffReviewStatusEntry[] Array of status entries.
---@return boolean is_headers True if all entries are header-like.
local function status_entries_are_headers(entries)
  if #entries == 0 then return false end
  for _, entry in ipairs(entries) do
    if not (
      entry.kind == "section"
      or entry.kind == "file"
      or entry.kind == "commit"
      or entry.kind == "commit_file"
      or entry.kind == "pr_file"
      or entry.kind == "pr_head_section"
      or entry.kind == "pr_review"
      or entry.kind == "pr_review_file"
    ) then return false end
  end
  return true
end

--- Resolves the post-action target anchor entry identifier.
---@param selected_entries DiffReviewStatusEntry[] Selected entries array.
---@param action_entries DiffReviewStatusEntry[] Action entries array.
---@return string? id Target entry identifier string, or nil.
local function status_action_target_id(selected_entries, action_entries)
  if status_entries_are_headers(selected_entries) then return nil end
  return action_entries[1] and action_entries[1].id or nil
end

--- Determines whether an entry identifier prefix matches a section or file header.
---@param target_id? string Entry identifier string.
---@return boolean is_header True if target represents a header row.
local function status_target_is_header(target_id)
  return type(target_id) == "string"
    and (
      target_id:find("^section:") ~= nil
      or target_id:find("^file:") ~= nil
      or target_id:find("^commit:") ~= nil
      or target_id:find("^commit%-file:") ~= nil
      or target_id:find("^provider%-file:") ~= nil
      or target_id:find("^pr%-head%-section:") ~= nil
      or target_id:find("^pr%-review:") ~= nil
    )
end

--- Locates the nearest section or file header line relative to a fallback line.
---@param fallback_line integer Target baseline line number.
---@return integer? line Line index of nearest header, or nil.
local function status_nearest_header_line(fallback_line)
  local status = session.status
  local viewport = status and status.diff_viewport or nil
  local entries = viewport and viewport.enabled and viewport.logical_entries or (status and status.entries)
  if not (status and entries and status.buf and vim.api.nvim_buf_is_valid(status.buf)) then return nil end
  if viewport and viewport.enabled and viewport.logical_entry_line_by_id then
    local best_line = nil
    local best_distance = nil
    for entry_id, line in pairs(viewport.logical_entry_line_by_id) do
      local entry = commit_view()._status_entry_by_id(entry_id)
      if entry and (
        entry.kind == "section"
        or entry.kind == "file"
        or entry.kind == "commit"
        or entry.kind == "commit_file"
        or entry.kind == "pr_file"
        or entry.kind == "pr_head_section"
      ) then
        local distance = math.abs((tonumber(line) or 0) - fallback_line)
        if not best_distance or distance < best_distance then
          best_distance = distance
          best_line = line
        end
      end
    end
    if best_line then return best_line end
  end
  local max_line = viewport and viewport.enabled and viewport.total or vim.api.nvim_buf_line_count(status.buf)
  local line = math.min(math.max(fallback_line, 1), max_line)
  local max_offset = math.max(line - 1, max_line - line)
  for offset = 0, max_offset do
    local previous_line = line - offset
    local previous_entry = entries[previous_line]
    if previous_entry and (
      previous_entry.kind == "section"
      or previous_entry.kind == "file"
      or previous_entry.kind == "commit"
      or previous_entry.kind == "commit_file"
      or previous_entry.kind == "pr_file"
      or previous_entry.kind == "pr_head_section"
    ) then return previous_line end
    local next_line = line + offset
    local next_entry = entries[next_line]
    if offset > 0 and next_entry and (
      next_entry.kind == "section"
      or next_entry.kind == "file"
      or next_entry.kind == "commit"
      or next_entry.kind == "commit_file"
      or next_entry.kind == "pr_file"
      or next_entry.kind == "pr_head_section"
    ) then return next_line end
  end
  return nil
end

--- Resolves the buffer line number for a status entry identifier, falling back to a default line.
---@param entries table<integer, DiffReviewStatusEntry>? Status entries dictionary by line.
---@param entry_id string? Target entry identifier string.
---@param fallback_line integer? Fallback line number.
---@return integer? line Resolved line number.
function M._status_find_entry_line(entries, entry_id, fallback_line)
  if not entries then return fallback_line end
  if entry_id and fallback_line then
    local fallback_entry = entries[fallback_line]
    if fallback_entry and fallback_entry.id == entry_id then return fallback_line end
  end
  if entry_id then
    for line, entry in pairs(entries) do
      if entry and entry.id == entry_id then return line end
    end
  end
  return fallback_line
end

--- Positions the window cursor on the specified entry identifier or fallback line.
---@param buf integer Status buffer handle.
---@param target_id? string Target entry identifier string.
---@param fallback_line? integer Fallback buffer line number.
local function status_restore_cursor(buf, target_id, fallback_line)
  local target_line = nil
  local entries = session.status and session.status.entries
  if target_id then
    target_line = M._status_find_entry_line(entries, target_id, fallback_line)
  end
  if not target_line and not fallback_line then return end
  if not target_line and fallback_line and status_target_is_header(target_id) then
    target_line = status_nearest_header_line(fallback_line)
  end
  target_line = target_line or fallback_line or 1
  target_line = math.min(target_line, vim.api.nvim_buf_line_count(buf))
  pcall(vim.api.nvim_win_set_cursor, 0, { math.max(target_line, 1), 0 })
end

--- Replaces status buffer content with plain text lines and applies keymap hint bars.
---@param buf integer Status buffer handle.
---@param lines string[] Array of content lines.
local function status_set_plain_lines(buf, lines)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  if session.diff_line_content_lengths then session.diff_line_content_lengths[buf] = nil end
  local state = session.states and session.states[buf] or (session.status and session.status.buf == buf and session.status) or nil
  if state then state.diff_viewport = nil end
  diff_buffer._clear_diff_gutter_visual_line(buf)
  status_buffer.with_writable(buf, function()
    vim.api.nvim_buf_clear_namespace(buf, ui.status_ns, 0, -1)
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  end)
  keymaps().status_apply_hint_bar(buf)
end

-- Expose the bare-local navigation helpers that init action handlers and other modules call by name.
M._status_entry_under_cursor = status_entry_under_cursor
M._status_prewarm_entry_syntax = status_prewarm_entry_syntax
M._status_restore_cursor = status_restore_cursor
M._status_leave_visual_mode = status_leave_visual_mode
M._status_visual_selection = status_visual_selection
M._status_visual_action_cursor_target = status_visual_action_cursor_target
M._status_action_target_id = status_action_target_id
M._status_action_entries = status_action_entries
M._status_files_from_set = status_files_from_set
M._status_count_set = status_count_set
M._status_notify_action = status_notify_action
M._status_set_plain_lines = status_set_plain_lines
M._status_defer_prewarm_under_cursor = status_defer_prewarm_under_cursor

return M
