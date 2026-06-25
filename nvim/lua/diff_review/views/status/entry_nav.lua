--- Owns cursor and entry navigation for the status views: resolving the entry under the cursor,
--- parent/file/hunk relationships, source-policy gates, visual-selection entry sets, action-target
--- resolution, decoration-row prewarm/scheduling, and cursor restore after a re-render.
---
--- Reads live status state, the diff-source/key/decoration models, and the syntax prewarm seam
--- through the init module via dr().

local syntax_engine = require("diff_review.render.syntax_engine")

--- Resolve the init module lazily so navigation can reach orchestrator state and the shared
--- source/key/prewarm seams without a load-time circular require.
local function dr()
  return require("diff_review")
end

local M = {}

local function status_entry_under_cursor(state)
  local status = state or dr()._status
  if not status then return nil end
  local line = vim.api.nvim_win_get_cursor(0)[1]
  return status.entries[line]
end


---@return integer? line
---@return DiffReviewStatusEntry? entry
function M._status_entry_line_under_cursor()
  local status = dr()._status
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

---@param status table?
---@param entry DiffReviewStatusEntry?
---@return table?
function M._status_source_policy_for_entry(status, entry)
  if not (status and status.diff_source_registry and entry) then return nil end
  local file = entry.file
  local source_id = file and file.diff_source_id or nil
  if not source_id and entry.hunk and entry.hunk.section_name then source_id = entry.hunk.section_name end
  if not source_id and entry.kind == "pr_hunk" and status.pr then source_id = "pr:" .. tostring(status.pr.number) .. ":changes" end
  if not source_id and entry.kind == "pr_review_hunk" then source_id = "review:unviewed" end
  if not source_id then return nil end
  return dr()._diff_source_model.policy(status.diff_source_registry, source_id)
end

---@param status table?
---@param command string
---@return boolean
function M._status_source_policy_allows_cursor(status, command)
  if not (status and status.entries) then return true end
  local _, entry = dr()._status_entry_line_under_cursor()
  local policy = dr()._status_source_policy_for_entry(status, entry)
  if not policy then return true end
  return policy[command] == true
end

---@param entry DiffReviewStatusEntry?
---@return boolean
function M._status_entry_is_file_like(entry)
  return entry ~= nil
    and (entry.kind == "file" or entry.kind == "commit_file" or entry.kind == "pr_file" or entry.kind == "pr_review_file")
end

---@param entry DiffReviewStatusEntry?
---@return boolean
function M._status_entry_is_hunk_like(entry)
  return entry ~= nil
    and (entry.kind == "hunk" or entry.kind == "commit_hunk" or entry.kind == "pr_hunk" or entry.kind == "pr_review_hunk")
end

---@param current_line integer
---@param entry DiffReviewStatusEntry
---@return DiffReviewStatusEntry?
function M._status_parent_entry(current_line, entry)
  local status = dr()._status
  if not (status and status.entries) then return nil end
  local viewport = status.diff_viewport
  local entries = viewport and viewport.enabled and viewport.logical_entries or status.entries
  for line = current_line - 1, 1, -1 do
    local candidate = entries[line]
    if entry.fold_target_id and candidate and candidate.id == entry.fold_target_id then return candidate end
    if entry.kind == "commit_hunk" and candidate and candidate.kind == "commit_file" then return candidate end
    if dr()._status_entry_is_hunk_like(entry) and dr()._status_entry_is_file_like(candidate) then return candidate end
    if dr()._status_entry_is_file_like(entry) and candidate and candidate.kind == "pr_review" then return candidate end
    if dr()._status_entry_is_file_like(entry) and candidate and candidate.kind == "commit" then return candidate end
    if (dr()._status_entry_is_file_like(entry) or entry.kind == "commit" or entry.kind == "pr_review") and candidate and candidate.kind == "section" then
      return candidate
    end
  end
  return nil
end

---@param entry DiffReviewStatusEntry?
local function status_prewarm_entry_syntax(entry)
  return dr()._status_perf_span("status.prewarm_entry_syntax", dr()._status and dr()._status.buf or nil, {
    entry_id = entry and entry.id or nil,
    entry_kind = entry and entry.kind or nil,
    file = entry and entry.file and entry.file.filename or nil,
  }, function()
    if not entry then return end
    if dr()._status_entry_is_file_like(entry) and entry.file then
      local syntax_source = dr()._status_syntax_source_for_entry_kind(entry.kind)
      syntax_engine.prewarm_file_diff_syntax(entry.file, "status-cursor-prewarm:" .. (entry.id or entry.file.filename), nil, { syntax_source = syntax_source })
    elseif dr()._status_entry_is_hunk_like(entry) and entry.file and entry.hunk then
      local callback_key = "status-cursor-prewarm:" .. (entry.id or entry.file.filename)
      local syntax_source = dr()._status_syntax_source_for_entry_kind(entry.kind)
      local syntax_diff_text = nil
      if syntax_source == "file" then
        syntax_diff_text = dr()._status_perf_span("status.prewarm_entry_syntax.hunk_combined_diff", dr()._status and dr()._status.buf or nil, {
          entry_id = entry.id,
          entry_kind = entry.kind,
          file = entry.file.filename,
        }, function()
          return dr()._status_file_syntax_diff_text(entry.file)
        end)
      end
      dr()._prewarm_diff_syntax(entry.file.filename, entry.hunk.diff, { entry.hunk.staged }, callback_key, nil, {
        syntax_source = syntax_source,
        syntax_diff_text = syntax_diff_text,
      })
    end
  end)
end

--- Map a 1-based status buffer row to a decoration request, or nil for chrome rows.
---@param buf integer
---@param row integer 1-based buffer line
---@return DiffReviewRowDecorationRequest?
function M._status_resolve_decoration_row(buf, row)
  local status = dr()._status_states and dr()._status_states[buf] or nil
  if not (status and status.entries) then return nil end
  local entry = status.entries[row]
  if not entry then return nil end
  local diff_line = (entry.diff_lines and entry.diff_lines[1]) or entry.diff_line
  local file = entry.file
  if not (diff_line and file) then return nil end
  local source_id = dr()._status_diff_source_id(file, entry.kind)
  return {
    file_key = dr()._diff_source_model.file_key(source_id, file.relpath or file.filename),
    revision = status.render_revision or 0,
    line = diff_line.line,
    side = diff_line.side == "left" and "old" or "new",
    kind = entry.kind,
  }
end

--- Prewarm Tree-sitter syntax for the file and hunk entries visible in a row range.
--- Scope syntax work to the viewport so off-screen hunks of a large diff stay cheap.
---@param buf integer
---@param first_row integer 1-based
---@param last_row integer 1-based

--- Debounce a visible-window syntax prewarm so the redraw callback never works inline.
---@param buf integer
---@param first_row integer 1-based
---@param last_row integer 1-based
function M._status_schedule_decorate_visible(buf, first_row, last_row)
  local status = dr()._status_states and dr()._status_states[buf] or nil
  if not status then return end
  if status.decorate_first == first_row and status.decorate_last == last_row then return end
  status.decorate_first = first_row
  status.decorate_last = last_row
  status.decorate_request_id = (status.decorate_request_id or 0) + 1
  local request_id = status.decorate_request_id
  vim.defer_fn(function()
    local current = dr()._status_states and dr()._status_states[buf] or nil
    if not (current and current.decorate_request_id == request_id) then return end
    if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
    dr()._status_decorate_visible(buf, first_row, last_row)
  end, 30)
end

--- Place one diff row's cached decoration spans into a namespace.
--- Pass ephemeral=true for the decoration provider's on_line; omit it (persistent)
--- for the test seam so headless tests can read the marks back.
---@param buf integer
---@param namespace integer
---@param row integer 0-based buffer row
---@param spans DiffReviewRowSpans?
---@param ephemeral boolean?

--- Apply a buffer row range's diff decoration into the decorate namespace as
--- persistent marks, for the test seam and non-redraw refreshes.
--- Returns the spans it applied so callers can assert per-row decoration.
---@param buf integer
---@param first_row integer? 1-based inclusive
---@param last_row integer? 1-based inclusive
---@return table<integer, DiffReviewRowSpans>

--- Register the global diff decoration provider once so diff-body decoration
--- (syntax/gutter/intraline/bg) is emitted ephemerally for visible rows only.
--- Skip non-status windows so unrelated redraws stay cheap.

---@param buf integer
local function status_defer_prewarm_under_cursor(buf)
  local status = dr()._status_states and dr()._status_states[buf] or dr()._status
  if not status then return end
  status.cursor_prewarm_request_id = (status.cursor_prewarm_request_id or 0) + 1
  local request_id = status.cursor_prewarm_request_id
  local entry = status_entry_under_cursor()
  local entry_id = entry and entry.id or nil
  dr()._status_perf_event("status.cursor_prewarm_schedule", buf, {
    request_id = request_id,
    entry_id = entry_id,
    entry_kind = entry and entry.kind or nil,
  })

  vim.defer_fn(function()
    local latest_status = dr()._status_states and dr()._status_states[buf] or dr()._status
    if not (latest_status and latest_status.cursor_prewarm_request_id == request_id) then return end
    if not (buf and vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_get_current_buf() == buf) then return end
    dr()._status_perf_span("status.cursor_prewarm_run", buf, {
      request_id = request_id,
      scheduled_entry_id = entry_id,
    }, function()
      dr()._status = latest_status
      local current_entry = status_entry_under_cursor()
      if entry_id and current_entry and current_entry.id == entry_id then
        status_prewarm_entry_syntax(current_entry)
      end
    end)
  end, 35)
end

local status_files_from_set


---@param start_line integer
---@param end_line integer
---@return DiffReviewStatusEntry[]
local function status_entries_for_lines(start_line, end_line)
  local status = dr()._status
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

---@return DiffReviewStatusEntry[]
local function status_entries_from_visual_selection()
  local mode = vim.fn.mode()
  local in_visual_mode = mode == "v" or mode == "V" or mode:byte() == 22
  if in_visual_mode then
    return status_entries_for_lines(vim.fn.line("v"), vim.api.nvim_win_get_cursor(0)[1])
  end

  local start_pos = vim.fn.getpos("'<")
  local end_pos = vim.fn.getpos("'>")
  return status_entries_for_lines(start_pos[2], end_pos[2])
end

local function status_leave_visual_mode()
  local mode = vim.api.nvim_get_mode().mode
  if mode == "v" or mode == "V" or mode:byte() == 22 then
    local esc = vim.api.nvim_replace_termcodes("<Esc>", true, false, true)
    vim.api.nvim_feedkeys(esc, "nx", false)
  end
end

---@param entry DiffReviewStatusEntry?
---@return DiffReviewStatusEntry[]
local function status_file_entries_for_entry(entry)
  if not entry then return {} end
  if entry.kind == "section" then
    local entries = {}
    for _, file in ipairs(entry.section.files or {}) do
      entries[#entries + 1] = { id = dr()._status_keys.file_key(file.section_name, file.filename), kind = "file", file = file }
    end
    return entries
  end
  return { entry }
end

---@param entries DiffReviewStatusEntry[]
---@return DiffReviewStatusEntry[]
local function status_expanded_entries(entries)
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
  return expanded_entries
end

---@param file_set table<string, boolean>
---@return string[]
function status_files_from_set(file_set)
  local files = {}
  for filename in pairs(file_set) do
    files[#files + 1] = filename
  end
  table.sort(files)
  return files
end

---@param items table<any, any>
---@return integer
local function status_count_set(items)
  local count = 0
  for _ in pairs(items) do
    count = count + 1
  end
  return count
end

---@param action string
---@param hunk_count integer
---@param file_count integer
local function status_notify_action(action, hunk_count, file_count)
  if hunk_count <= 0 and file_count <= 0 then return end
  local parts = {}
  if hunk_count > 0 then
    parts[#parts + 1] = ("%d hunk(s)"):format(hunk_count)
  end
  if file_count > 0 then
    parts[#parts + 1] = ("%d file(s)"):format(file_count)
  end
  dr()._notify_debug(("%s %s"):format(action, table.concat(parts, ", ")), vim.log.levels.INFO, { title = "DiffReview" })
end

---@param entries DiffReviewStatusEntry[]
---@return string?
local function status_hunk_action_target_id(entries)
  local status = dr()._status
  if not (status and status.entries) then return nil end

  local action_ids = {}
  local has_hunk = false
  for _, entry in ipairs(entries or {}) do
    if entry.kind == "hunk" and entry.id then
      has_hunk = true
      action_ids[entry.id] = true
    end
  end
  if not has_hunk then return nil end

  local cursor_line = vim.api.nvim_win_get_cursor(0)[1]
  if not (status.buf and vim.api.nvim_buf_is_valid(status.buf)) then return nil end
  local max_line = vim.api.nvim_buf_line_count(status.buf)
  for line = cursor_line + 1, max_line do
    local entry = status.entries[line]
    if entry and entry.kind == "hunk" and entry.id and not action_ids[entry.id] then
      return entry.id
    end
  end
  for line = cursor_line - 1, 1, -1 do
    local entry = status.entries[line]
    if entry and entry.kind == "hunk" and entry.id and not action_ids[entry.id] then
      return entry.id
    end
  end
  return nil
end

---@param entries DiffReviewStatusEntry[]
---@return boolean
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

---@param selected_entries DiffReviewStatusEntry[]
---@param action_entries DiffReviewStatusEntry[]
---@param target_section? DiffReviewStatusSectionName
---@param opts? { file_target?: "destination"|"next" }
---@return string?
local function status_action_target_id(selected_entries, action_entries, target_section, opts)
  if status_entries_are_headers(selected_entries) then
    return nil
  end
  return status_hunk_action_target_id(action_entries) or (action_entries[1] and action_entries[1].id or nil)
end

---@param target_id? string
---@return boolean
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

---@param fallback_line integer
---@return integer?
local function status_nearest_header_line(fallback_line)
  local status = dr()._status
  local viewport = status and status.diff_viewport or nil
  local entries = viewport and viewport.enabled and viewport.logical_entries or (status and status.entries)
  if not (status and entries and status.buf and vim.api.nvim_buf_is_valid(status.buf)) then return nil end
  if viewport and viewport.enabled and viewport.logical_entry_line_by_id then
    local best_line = nil
    local best_distance = nil
    for entry_id, line in pairs(viewport.logical_entry_line_by_id) do
      local entry = dr()._status_entry_by_id(entry_id)
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

--- Find the buffer line of a status entry by id, preferring the fallback line.
---@param entries table<integer, DiffReviewStatusEntry>?
---@param entry_id string?
---@param fallback_line integer?
---@return integer?
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

local function status_restore_cursor(buf, target_id, fallback_line)
  local target_line = nil
  local entries = dr()._status and dr()._status.entries
  if target_id then
    target_line = dr()._status_find_entry_line(entries, target_id, fallback_line)
  end
  if not target_line and not fallback_line then return end
  if not target_line and fallback_line and status_target_is_header(target_id) then
    target_line = status_nearest_header_line(fallback_line)
  end
  target_line = target_line or fallback_line or 1
  target_line = math.min(target_line, vim.api.nvim_buf_line_count(buf))
  pcall(vim.api.nvim_win_set_cursor, 0, { math.max(target_line, 1), 0 })
end

---@param buf integer
---@param lines string[]
local function status_set_plain_lines(buf, lines)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  if dr()._diff_line_content_lengths then dr()._diff_line_content_lengths[buf] = nil end
  local state = dr()._status_states and dr()._status_states[buf] or (dr()._status and dr()._status.buf == buf and dr()._status) or nil
  if state then state.diff_viewport = nil end
  dr()._clear_diff_gutter_visual_line(buf)
  local was_rendering = vim.b[buf].diff_review_status_rendering
  vim.b[buf].diff_review_status_rendering = true
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_clear_namespace(buf, dr()._status_ns, 0, -1)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false
  vim.b[buf].diff_review_status_rendering = was_rendering
  dr()._status_apply_hint_bar(buf)
end

-- Expose the bare-local navigation helpers that init action handlers and other modules call by name.
M._status_entry_under_cursor = status_entry_under_cursor
M._status_prewarm_entry_syntax = status_prewarm_entry_syntax
M._status_restore_cursor = status_restore_cursor
M._status_leave_visual_mode = status_leave_visual_mode
M._status_entries_from_visual_selection = status_entries_from_visual_selection
M._status_action_target_id = status_action_target_id
M._status_expanded_entries = status_expanded_entries
M._status_files_from_set = status_files_from_set
M._status_count_set = status_count_set
M._status_notify_action = status_notify_action
M._status_set_plain_lines = status_set_plain_lines
M._status_defer_prewarm_under_cursor = status_defer_prewarm_under_cursor

return M
