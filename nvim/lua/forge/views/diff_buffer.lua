--- Owns the standalone diff:// buffer view: a real, navigable buffer that previews one file's
--- unified diff, plus the gutter visual-line yank overlay and the cursor normalization that keeps
--- the caret off the inline gutter virtual text in both diff and status buffers.
---
--- Owns the per-buffer diff caches (the diff:// registry, per-buffer hunks, and saved cursors) as
--- module-locals and reads the shared per-session caches from session.lua, so this module keeps its
--- own view state while reaching render/parse seams and orchestrator functions via direct requires.

local diff_render = require("forge.render.diff_render")
local git_backend = require("forge.git.git_backend")
local notifications = require("forge.infra.notifications")

-- state edge kept lazy to avoid a load-time cycle.
-- git_data edge kept lazy to avoid a load-time cycle.
local function git_data() return require("forge.git.git_data") end
-- Keep the status_snapshot edge lazy because it reaches git_data.
local function status_snapshot() return require("forge.git.status_snapshot") end
local trace = require("forge.infra.perf_trace")
local ui = require("forge.infra.ui")
local session = require("forge.session")

-- Gutter visual-line mode + cursor-normalizing state, keyed by buffer. Private to this module
-- (previously parked on the init table as shared state during the monolith era).
local gutter_visual_yank_maps = {}
local gutter_visual_selections = {}
local cursor_normalizing = {}

-- Per-diff-buffer view caches, owned here and keyed by buffer handle (diff_bufs by
-- "diff:"..filename). Cleared together by cleanup_diff_buffers on session teardown.
local diff_bufs = {}
local buf_hunks = {}
local buf_filename = {}
local buf_saved_cursor = {}
local untracked_diff_waiter_by_file = {}
local untracked_diff_generation = 0

local M = {}

--- Deletes all open diff preview buffers and resets per-buffer view caches during teardown.
function M.cleanup_diff_buffers()
  require("forge.local_diff").close_all()
  for _, buf in pairs(diff_bufs) do
    if vim.api.nvim_buf_is_valid(buf) then
      vim.api.nvim_buf_delete(buf, { force = true })
    end
  end
  diff_bufs = {}
  buf_hunks = {}
  buf_filename = {}
  buf_saved_cursor = {}
  untracked_diff_waiter_by_file = {}
  untracked_diff_generation = untracked_diff_generation + 1
end

--- Computes cumulative display column width for an array of virtual text chunks.
---@param virt_text table[]? Array of virtual text chunk tuples.
---@return integer width Total display column width.
function M._inline_virtual_text_width(virt_text)
  local width = 0
  for _, chunk in ipairs(virt_text or {}) do
    width = width + vim.fn.strdisplaywidth(chunk[1] or "")
  end
  return width
end

---@class ForgeGutterCursorBounds
---@field line string
---@field gutter_col integer 0-based buffer column where the inline gutter starts
---@field gutter_width integer virtual columns occupied by the inline gutter
---@field content_length integer real buffer text length before visual highlight padding
---@field virt_text table[]

--- Locates inline gutter virtual text extmark on a row and computes its column bounds.
---@param buf integer Target buffer handle.
---@param row integer One-based line number.
---@param namespace integer Extmark namespace identifier.
---@return ForgeGutterCursorBounds? bounds Extmark boundary descriptor or nil.
function M._diff_gutter_cursor_bounds(buf, row, namespace)
  local line = vim.api.nvim_buf_get_lines(buf, row - 1, row, false)[1]
  if line == nil then return nil end
  local content_lengths = session.diff_line_content_lengths and session.diff_line_content_lengths[buf] or nil
  local content_length = content_lengths and content_lengths[row] or #line
  local marks = vim.api.nvim_buf_get_extmarks(buf, namespace, { row - 1, 0 }, { row - 1, -1 }, { details = true })
  for _, mark in ipairs(marks) do
    local col = mark[3] or 0
    local details = mark[4] or {}
    if details.virt_text and details.virt_text_pos == "inline" and col <= content_length then
      local width = M._inline_virtual_text_width(details.virt_text)
      if width > 0 then
        return {
          line = line,
          gutter_col = col,
          gutter_width = width,
          content_length = content_length,
          virt_text = details.virt_text,
        }
      end
    end
  end
  return nil
end

--- Clones virtual text chunks and sets their highlight group to Visual.
---@param chunks table[]? Array of virtual text chunk tuples.
---@return table[] visual_chunks Array of styled chunk tuples.
function M._diff_gutter_visual_chunks(chunks)
  local visual_chunks = {}
  for _, chunk in ipairs(chunks or {}) do
    visual_chunks[#visual_chunks + 1] = { chunk[1] or "", "Visual" }
  end
  return visual_chunks
end

--- Concatenates raw text chunks from a virtual text structure.
---@param chunks table[]? Array of virtual text chunk tuples.
---@return string text Concatenated string.
function M._diff_gutter_text(chunks)
  local parts = {}
  for _, chunk in ipairs(chunks or {}) do
    parts[#parts + 1] = chunk[1] or ""
  end
  return table.concat(parts)
end

--- Checks whether the current editor mode is visual or visual-line.
---@param mode? string Optional mode string to test.
---@return boolean is_visual True if mode is visual.
function M._is_visual_mode(mode)
  mode = mode or vim.api.nvim_get_mode().mode
  return mode == "v" or mode == "V" or mode:byte() == 22
end

--- Clears gutter visual overlay extmarks from the target buffer.
---@param buf integer Target buffer handle.
function M._clear_diff_gutter_visual_overlay(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  pcall(vim.api.nvim_buf_clear_namespace, buf, ui.gutter_visual_ns, 0, -1)
end

--- Clears gutter visual line extmarks and removes temporary visual-line keymaps.
---@param buf integer Target buffer handle.
function M._clear_diff_gutter_visual_line(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  if gutter_visual_yank_maps and gutter_visual_yank_maps[buf] then
    pcall(vim.keymap.del, "x", "<Space>l", { buffer = buf })
    gutter_visual_yank_maps[buf] = nil
  end
  M._clear_diff_gutter_visual_overlay(buf)
end

--- Installs buffer keymaps for yanking gutter visual line selections.
---@param buf integer Target buffer handle.
function M._install_diff_gutter_visual_line_yank_maps(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  gutter_visual_yank_maps = gutter_visual_yank_maps or {}
  if gutter_visual_yank_maps[buf] then return end
  gutter_visual_yank_maps[buf] = true
  vim.keymap.set("x", "<Space>l", function()
    if M._yank_diff_gutter_visual_line(buf, "+") then return end
    M._clear_diff_gutter_visual_line(buf)
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Space>l", true, false, true), "m", false)
  end, { buffer = buf, nowait = true, silent = true, desc = "Yank selection to clipboard" })
end

--- Checks if gutter visual-line mode is active for the specified buffer.
---@param buf integer Target buffer handle.
---@return boolean active True if gutter visual-line mode is active.
function M._diff_gutter_visual_line_active(buf)
  local selections = gutter_visual_selections
  if not (selections and selections[buf]) then return false end
  if selections[buf] == "starting" then return true end
  if M._is_visual_mode() then return true end
  selections[buf] = nil
  M._clear_diff_gutter_visual_line(buf)
  return false
end

--- Resolves the appropriate diff extmark namespace for a buffer.
---@param buf integer Target buffer handle.
---@return integer namespace Namespace identifier.
function M._diff_gutter_namespace(buf)
  local status = session.states and session.states[buf] or nil
  return status and ui.status_ns or ui.preview_ns
end

--- Updates visual overlay extmarks spanning the active visual-line range.
---@param buf integer Target buffer handle.
function M._refresh_diff_gutter_visual_line(buf)
  if not M._diff_gutter_visual_line_active(buf) then return end
  M._clear_diff_gutter_visual_overlay(buf)
  local cursor_row = vim.api.nvim_win_get_cursor(0)[1]
  local start_pos = vim.fn.getpos("v")
  local start_row = start_pos and start_pos[2] or cursor_row
  if start_row == 0 then start_row = cursor_row end
  local first_row = math.min(start_row, cursor_row)
  local last_row = math.max(start_row, cursor_row)
  local namespace = M._diff_gutter_namespace(buf)
  for row = first_row, last_row do
    local bounds = M._diff_gutter_cursor_bounds(buf, row, namespace)
    if bounds and bounds.virt_text then
      pcall(vim.api.nvim_buf_set_extmark, buf, ui.gutter_visual_ns, row - 1, 0, {
        virt_text = M._diff_gutter_visual_chunks(bounds.virt_text),
        virt_text_pos = "overlay",
        virt_text_win_col = bounds.gutter_col,
        hl_mode = "replace",
        priority = 250,
      })
    end
  end
end

--- Enters visual-line selection mode with gutter overlay decorations.
---@param buf integer Target buffer handle.
function M._start_diff_gutter_visual_line(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_get_current_buf() == buf) then return end
  gutter_visual_selections = gutter_visual_selections or {}
  gutter_visual_selections[buf] = "starting"
  M._install_diff_gutter_visual_line_yank_maps(buf)
  local row = vim.api.nvim_win_get_cursor(0)[1]
  vim.fn.setpos(".", { 0, row, 1, 0 })
  vim.cmd("normal! V")
  gutter_visual_selections[buf] = true
  M._refresh_diff_gutter_visual_line(buf)
end

--- Extracts combined gutter and line text across the active visual-line selection.
---@param buf integer Target buffer handle.
---@return string[] lines Array of concatenated line text strings.
function M._diff_gutter_visual_line_text(buf)
  local cursor_row = vim.api.nvim_win_get_cursor(0)[1]
  local start_pos = vim.fn.getpos("v")
  local start_row = start_pos and start_pos[2] or cursor_row
  if start_row == 0 then start_row = cursor_row end
  local first_row = math.min(start_row, cursor_row)
  local last_row = math.max(start_row, cursor_row)
  local namespace = M._diff_gutter_namespace(buf)
  local lines = {}
  for row = first_row, last_row do
    local line = vim.api.nvim_buf_get_lines(buf, row - 1, row, false)[1] or ""
    local bounds = M._diff_gutter_cursor_bounds(buf, row, namespace)
    if bounds and bounds.virt_text then
      lines[#lines + 1] = M._diff_gutter_text(bounds.virt_text) .. line:sub(1, bounds.content_length)
    else
      lines[#lines + 1] = line
    end
  end
  return lines
end

--- Yanks selected visual lines including gutter markers into the target register.
---@param buf integer Target buffer handle.
---@param register? string Target register character or string.
---@return boolean handled True if yank operation succeeded.
function M._yank_diff_gutter_visual_line(buf, register)
  if not M._diff_gutter_visual_line_active(buf) then return false end
  local lines = M._diff_gutter_visual_line_text(buf)
  register = register or vim.v.register
  if register == nil or register == "" then register = '"' end
  vim.fn.setreg(register, lines, "V")
  if register == '"' and vim.o.clipboard:find("unnamedplus", 1, true) then
    pcall(vim.fn.setreg, "+", lines, "V")
  end
  if register == '"' and vim.o.clipboard:find("unnamed", 1, true) then
    pcall(vim.fn.setreg, "*", lines, "V")
  end
  gutter_visual_selections[buf] = nil
  M._clear_diff_gutter_visual_line(buf)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Esc>", true, false, true), "nx", false)
  return true
end

--- Clamps cursor position to prevent landing inside inline gutter virtual text.
---@param buf integer Target buffer handle.
---@param namespace integer Extmark namespace identifier.
---@return boolean handled True if cursor was adjusted.
function M._normalize_diff_gutter_cursor(buf, namespace)
  if not (buf and vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_get_current_buf() == buf) then return false end
  local row = vim.api.nvim_win_get_cursor(0)[1]
  local bounds = M._diff_gutter_cursor_bounds(buf, row, namespace)
  if not bounds then return false end

  local pos = vim.fn.getcurpos()
  local current_col = math.max((pos[3] or 1) - 1, 0)
  local current_coladd = pos[4] or 0
  local line_length = bounds.content_length or #bounds.line
  local target_col = current_col
  local target_coladd = current_coladd

  if line_length <= bounds.gutter_col then
    target_col = bounds.gutter_col
    target_coladd = bounds.gutter_width
  else
    local last_text_col = line_length - 1
    if current_col <= bounds.gutter_col then
      target_col = bounds.gutter_col
      target_coladd = bounds.gutter_width
    elseif current_col > last_text_col then
      target_col = last_text_col
      target_coladd = 0
    else
      target_col = current_col
      target_coladd = 0
    end
  end

  if not (pos[2] == row and current_col == target_col and current_coladd == target_coladd) then
    vim.fn.setpos(".", { 0, row, target_col + 1, target_coladd })
  end
  return true
end

--- Normalizes cursor column for diff preview or status buffers.
---@param buf integer Target buffer handle.
---@return boolean handled True if cursor normalization was performed.
function M._align_diff_cursor(buf)
  local status = session.states and session.states[buf] or nil
  if status then
    return M._normalize_diff_gutter_cursor(buf, ui.status_ns)
  end
  return M._normalize_diff_gutter_cursor(buf, ui.preview_ns)
end

--- Clamps cursor column to valid line content width.
---@param buf integer Target buffer handle.
---@return integer? row Active buffer row index.
function M._clamp_buffer_text_cursor(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_get_current_buf() == buf) then return nil end
  local row = vim.api.nvim_win_get_cursor(0)[1]
  local line = vim.api.nvim_buf_get_lines(buf, row - 1, row, false)[1]
  if line == nil then return row end
  local content_lengths = session.diff_line_content_lengths and session.diff_line_content_lengths[buf] or nil
  local line_length = content_lengths and content_lengths[row] or #line
  local pos = vim.fn.getcurpos()
  local current_col = math.max((pos[3] or 1) - 1, 0)
  local current_coladd = pos[4] or 0
  local target_col = math.min(current_col, line_length)
  if current_col ~= target_col or current_coladd ~= 0 then
    vim.fn.setpos(".", { 0, row, target_col + 1, 0 })
  end
  return row
end

--- Normalizes cursor positioning across diff and status view buffers.
---@param buf integer Target buffer handle.
---@return integer? row Normalized buffer row index.
function M._normalize_status_cursor(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_get_current_buf() == buf) then return nil end
  cursor_normalizing = cursor_normalizing or {}
  if cursor_normalizing[buf] then return vim.api.nvim_win_get_cursor(0)[1] end
  cursor_normalizing[buf] = true
  if M._diff_gutter_visual_line_active(buf) then
    M._refresh_diff_gutter_visual_line(buf)
    cursor_normalizing[buf] = nil
    return vim.api.nvim_win_get_cursor(0)[1]
  end
  local handled = M._align_diff_cursor(buf)
  if not handled then M._clamp_buffer_text_cursor(buf) end
  cursor_normalizing[buf] = nil
  return vim.api.nvim_win_get_cursor(0)[1]
end

--- Normalizes cursor positioning in empty diff buffers.
---@param buf integer Target buffer handle.
function M._align_empty_diff_cursor(buf)
  M._normalize_status_cursor(buf)
end

--- Creates or focuses a standalone diff buffer for a specific file.
---@param filename string Target file path string.
---@return integer buf Created or existing buffer handle.
function M.open_diff_buffer(filename)
  return require("forge.local_diff").open(filename)
end

--- Computes line ranges and hunk mapping from unified diff text.
---@param diff_text string Unified diff body text.
---@return table[] hunk_map Array of hunk range descriptors.
function M._compute_hunk_map(diff_text)
  local raw_hunks = git_data()._parse_diff(diff_text, false)
  local rendered_line = 0
  local hunk_map = {}
  for _, h in ipairs(raw_hunks) do
    -- Count code lines the same way the local formatter does: lines after @@,
    -- stripping trailing empty/whitespace lines.
    local code_lines_list = {}
    local found_hunk_header = false
    for diff_line in h.diff:gmatch("[^\n]+") do
      if found_hunk_header then
        code_lines_list[#code_lines_list + 1] = diff_line
      elseif diff_line:match("^@@") then
        found_hunk_header = true
      end
    end
    -- Strip trailing empty lines to match render.diff_parse.parse_hunk_body().
    while #code_lines_list > 0 and code_lines_list[#code_lines_list]:match("^%s*$") do
      table.remove(code_lines_list)
    end
    local code_lines = #code_lines_list
    -- Rendered: 1 line (@@ separator) + code_lines
    -- end_line is the LAST line of this hunk (exclusive of next hunk's @@)
    local start_line = rendered_line + 1
    local end_line = start_line + code_lines
    hunk_map[#hunk_map + 1] = {
      start_line = start_line,
      end_line = end_line,
      diff = h.diff,
      folded = false,
    }
    rendered_line = end_line
  end
  return hunk_map
end

--- Applies manual folds to staged or folded hunks in the diff buffer.
---@param buf integer Target buffer handle.
function M._render_with_folds(buf)
  local hunks = buf_hunks[buf]
  if not hunks then return end

  -- Find the window showing this buffer
  local win = nil
  for _, w in ipairs(vim.api.nvim_list_wins()) do
    if vim.api.nvim_win_is_valid(w) and vim.api.nvim_win_get_buf(w) == buf then
      win = w
      break
    end
  end
  if not win then return end

  local line_count = vim.api.nvim_buf_line_count(buf)
  -- Ensure fold settings are on the correct window
  vim.wo[win].foldmethod = "manual"
  vim.wo[win].foldenable = true
  vim.api.nvim_win_call(win, function()
    -- Save view to prevent jumping
    local view = vim.fn.winsaveview()
    pcall(vim.cmd, "normal! zE") -- delete all folds
    for _, h in ipairs(hunks) do
      if h.folded then
        local fold_start = h.start_line + 1
        local fold_end = math.min(h.end_line, line_count)
        if fold_end >= fold_start and fold_start <= line_count then
          pcall(vim.cmd, fold_start .. "," .. fold_end .. "fold")
        end
      end
    end
    vim.fn.winrestview(view)
  end)
end

--- Re-renders diff buffer content and applies hunk highlights and folds.
---@param buf integer Target buffer handle.
---@param filename string Associated file path string.
function M._refresh_diff_buffer(buf, filename)
  if require("forge.local_diff").owner(buf) then return end
  -- Use cached diff data from M.get() instead of re-running git
  local diff_text = session.file_diffs and session.file_diffs[filename]
  local staged_flags = session.file_hunk_staged and session.file_hunk_staged[filename]

  if diff_text and diff_text ~= "" then
    -- Skip re-render if already rendered with the same data, but still
    -- (re)apply folds: the initial pre-render happens off-screen, where
    -- _render_with_folds is a no-op (no window shows the buffer yet), so the
    -- staged-hunk folds must be applied once the buffer becomes visible.
    if session.buf_last_rendered[buf] == diff_text and buf_hunks[buf] then
      M._render_with_folds(buf)
      return
    end
    session.buf_last_rendered[buf] = diff_text

    diff_render.render_fancy_diff(buf, diff_text, staged_flags, filename)
    local hunk_map = M._compute_hunk_map(diff_text)
    -- Auto-fold staged hunks
    if staged_flags then
      for i, h in ipairs(hunk_map) do
        if staged_flags[i] then
          h.folded = true
        end
      end
    end
    buf_hunks[buf] = hunk_map
    -- Highlight @@ header lines with subtle gray background
    vim.api.nvim_buf_clear_namespace(buf, ui.hunk_header_ns, 0, -1)
    for _, h in ipairs(hunk_map) do
      pcall(vim.api.nvim_buf_set_extmark, buf, ui.hunk_header_ns, h.start_line - 1, 0, {
        line_hl_group = "ForgeHunkHeader",
        priority = ui.hunk_header_priority,
      })
    end
    M._render_with_folds(buf)
  else
    vim.bo[buf].modifiable = true
    local message = diff_text == false and "No textual diff" or "No changes"
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, { message })
    vim.bo[buf].modifiable = false
    buf_hunks[buf] = {}
    if session.empty_diff_rows then session.empty_diff_rows[buf] = nil end
    if session.diff_line_content_lengths then session.diff_line_content_lengths[buf] = nil end
    vim.api.nvim_buf_clear_namespace(buf, ui.hunk_header_ns, 0, -1)
    vim.api.nvim_buf_clear_namespace(buf, ui.active_hunk_header_ns, 0, -1)
  end
end

--- Applies highlight decoration to the active diff hunk header.
---@param buf integer Target buffer handle.
---@param item_diff string? Optional diff hunk text to match.
---@return table? hunk Matching hunk map record or nil.
function M._highlight_active_hunk(buf, item_diff)
  vim.api.nvim_buf_clear_namespace(buf, ui.active_hunk_header_ns, 0, -1)
  if not item_diff then return nil end

  local hunks = buf_hunks[buf]
  if not hunks then return nil end

  for _, hunk in ipairs(hunks) do
    if hunk.diff == item_diff then
      pcall(vim.api.nvim_buf_set_extmark, buf, ui.active_hunk_header_ns, hunk.start_line - 1, 0, {
        line_hl_group = "ForgeActiveHunkHeader",
        priority = ui.active_hunk_header_priority,
      })
      return hunk
    end
  end
end

--- Asynchronously re-fetches diff and staged flags for a single file.
---@param filename string Target file path string.
---@param cb? fun() Optional callback invoked upon completion.
function M._update_file_diff_cache_async(filename, cb)
  if require("forge.local_diff").refresh(filename, cb) then return end
  session.file_diffs = session.file_diffs or {}
  session.file_hunk_staged = session.file_hunk_staged or {}
  -- Untracked files: build the diff from disk, never from git. Cache `false`
  -- (not nil) for empty/binary so the preview guard treats it as "checked"
  -- and doesn't re-run this on every cursor move.
  local relpath = session.untracked and session.untracked[filename]
  if relpath then
    local waiter_list = untracked_diff_waiter_by_file[filename]
    if waiter_list then
      if cb then waiter_list[#waiter_list + 1] = cb end
      return
    end

    waiter_list = {}
    if cb then waiter_list[#waiter_list + 1] = cb end
    untracked_diff_waiter_by_file[filename] = waiter_list
    local read_generation = untracked_diff_generation
    status_snapshot().read_untracked_diff_async(filename, relpath, function(diff_text)
      if read_generation ~= untracked_diff_generation then return end
      local current_relpath = session.untracked and session.untracked[filename]
      if current_relpath == relpath then
        session.file_diffs[filename] = diff_text or false
        session.file_hunk_staged[filename] = diff_text and { false } or nil
      end
      local completed_waiter_list = untracked_diff_waiter_by_file[filename] or {}
      untracked_diff_waiter_by_file[filename] = nil
      for _, waiter in ipairs(completed_waiter_list) do waiter() end
    end)
    return
  end
  git_backend.git_root_async(function(cwd)
    if not cwd then
      if cb then cb() end
      return
    end
    git_data()._file_diff_and_flags_async(cwd, filename, function(diff_text, flags)
      session.file_diffs[filename] = diff_text or false
      session.file_hunk_staged[filename] = flags
      if cb then cb() end
    end)
  end)
end

--- Re-renders an open per-file diff buffer from current session cache.
---@param filename string Target file path string.
function M.refresh_open_diff_buffer_from_cache(filename)
  if require("forge.local_diff").refresh(filename) then return end
  local key = "diff:" .. filename
  diff_bufs = diff_bufs or {}
  local buf = diff_bufs[key]
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end

  session.buf_last_rendered[buf] = nil
  M._refresh_diff_buffer(buf, filename)
end

--- Reloads one open per-file diff buffer from Git.
---@param filename string Target file path string.
function M.refresh_open_diff_buffer(filename)
  if require("forge.local_diff").refresh(filename) then return end
  local key = "diff:" .. filename
  diff_bufs = diff_bufs or {}
  local buf = diff_bufs[key]
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end

  -- Re-fetch diff data for this file only (cache is stale after staging)
  M._update_file_diff_cache_async(filename, function()
    if not vim.api.nvim_buf_is_valid(buf) then return end
    M.refresh_open_diff_buffer_from_cache(filename)
  end)
end

return M
