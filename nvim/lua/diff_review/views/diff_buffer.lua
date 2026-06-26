--- Owns the standalone diff:// buffer view: a real, navigable buffer that previews one file's
--- unified diff, plus the gutter visual-line yank overlay and the cursor normalization that keeps
--- the caret off the inline gutter virtual text in both diff and status buffers.
---
--- Owns the per-buffer diff caches (the diff:// registry, per-buffer hunks, and saved cursors) as
--- module-locals and reads the shared per-session caches from session.lua, so this module keeps its
--- own view state while reaching render/parse seams and orchestrator functions via direct requires.

local diff_render = require("diff_review.render.diff_render")
local git_backend = require("diff_review.git.git_backend")

-- state edge kept lazy to avoid a load-time cycle.
local function state() return require("diff_review.views.status.state") end
-- git_data edge kept lazy to avoid a load-time cycle.
local function git_data() return require("diff_review.git.git_data") end
local window_options = require("diff_review.views.status.window_options")
local trace = require("diff_review.infra.perf_trace")
local ui = require("diff_review.infra.ui")
local session = require("diff_review.session")

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

local M = {}

--- Delete every open diff:// buffer and clear the per-buffer caches, called by the
--- status state teardown when a review session ends.
function M.cleanup_diff_buffers()
  for _, buf in pairs(diff_bufs) do
    if vim.api.nvim_buf_is_valid(buf) then
      vim.api.nvim_buf_delete(buf, { force = true })
    end
  end
  diff_bufs = {}
  buf_hunks = {}
  buf_filename = {}
  buf_saved_cursor = {}
end

function M._diff_visual_fill_width(buf)
  local width = vim.o.columns
  for _, win in ipairs(vim.fn.win_findbuf(buf)) do
    if vim.api.nvim_win_is_valid(win) then
      width = math.max(width, vim.api.nvim_win_get_width(win))
    end
  end
  return math.max(width + 8, 160)
end

function M._diff_pad_highlighted_line(line_text, buf)
  local content_length = #line_text
  local fill_width = M._diff_visual_fill_width(buf)
  local display_width = vim.fn.strdisplaywidth(line_text)
  if display_width < fill_width then
    line_text = line_text .. string.rep(" ", fill_width - display_width)
  end
  return line_text, content_length
end

---@param virt_text table[]?
---@return integer
function M._inline_virtual_text_width(virt_text)
  local width = 0
  for _, chunk in ipairs(virt_text or {}) do
    width = width + vim.fn.strdisplaywidth(chunk[1] or "")
  end
  return width
end

---@class DiffReviewGutterCursorBounds
---@field line string
---@field gutter_col integer 0-based buffer column where the inline gutter starts
---@field gutter_width integer virtual columns occupied by the inline gutter
---@field content_length integer real buffer text length before visual highlight padding
---@field virt_text table[]

---@param buf integer
---@param row integer 1-based
---@param namespace integer
---@return DiffReviewGutterCursorBounds?
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

---@param chunks table[]?
---@return table[]
function M._diff_gutter_visual_chunks(chunks)
  local visual_chunks = {}
  for _, chunk in ipairs(chunks or {}) do
    visual_chunks[#visual_chunks + 1] = { chunk[1] or "", "Visual" }
  end
  return visual_chunks
end

---@param chunks table[]?
---@return string
function M._diff_gutter_text(chunks)
  local parts = {}
  for _, chunk in ipairs(chunks or {}) do
    parts[#parts + 1] = chunk[1] or ""
  end
  return table.concat(parts)
end

---@param mode? string
---@return boolean
function M._is_visual_mode(mode)
  mode = mode or vim.api.nvim_get_mode().mode
  return mode == "v" or mode == "V" or mode:byte() == 22
end

---@param buf integer
function M._clear_diff_gutter_visual_overlay(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  pcall(vim.api.nvim_buf_clear_namespace, buf, ui.gutter_visual_ns, 0, -1)
end

---@param buf integer
function M._clear_diff_gutter_visual_line(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  if gutter_visual_yank_maps and gutter_visual_yank_maps[buf] then
    pcall(vim.keymap.del, "x", "<Space>l", { buffer = buf })
    gutter_visual_yank_maps[buf] = nil
  end
  M._clear_diff_gutter_visual_overlay(buf)
end

---@param buf integer
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

---@param buf integer
---@return boolean
function M._diff_gutter_visual_line_active(buf)
  local selections = gutter_visual_selections
  if not (selections and selections[buf]) then return false end
  if selections[buf] == "starting" then return true end
  if M._is_visual_mode() then return true end
  selections[buf] = nil
  M._clear_diff_gutter_visual_line(buf)
  return false
end

---@param buf integer
---@return integer
function M._diff_gutter_namespace(buf)
  local status = session.states and session.states[buf] or nil
  return status and ui.status_ns or ui.preview_ns
end

---@param buf integer
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

---@param buf integer
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

---@param buf integer
---@return string[]
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

---@param buf integer
---@param register? string
---@return boolean handled
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

---@param buf integer
---@param namespace integer
---@return boolean handled
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

---@param buf integer
---@return boolean handled
function M._align_diff_cursor(buf)
  local status = session.states and session.states[buf] or nil
  if status then
    return M._normalize_diff_gutter_cursor(buf, ui.status_ns)
  end
  return M._normalize_diff_gutter_cursor(buf, ui.preview_ns)
end

---@param buf integer
---@return integer? row
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

---@param buf integer
---@return integer? row
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

---@param buf integer
function M._align_empty_diff_cursor(buf)
  M._normalize_status_cursor(buf)
end
--- Find which hunk the cursor is in within a diff buffer.
--- Returns the hunk's complete diff patch (with file header) or nil.
---@param buf number
---@return string? diff_patch
---@return number? hunk_start_line
local function get_hunk_at_cursor(buf)
  local hunks = buf_hunks[buf]
  if not hunks then return end
  local cursor = vim.api.nvim_win_get_cursor(0)[1]
  for _, h in ipairs(hunks) do
    if cursor >= h.start_line and cursor <= h.end_line then
      return h.diff, h.start_line
    end
  end
end

--- Create (or reuse) a real diff buffer with keymaps.
--- Call _refresh_diff_buffer after setting the buffer on a window.
---@param filename string
---@return number buf
function M.open_diff_buffer(filename)
  local key = "diff:" .. filename
  diff_bufs = diff_bufs or {}
  local buf = diff_bufs[key]

  if not buf or not vim.api.nvim_buf_is_valid(buf) then
    buf = vim.api.nvim_create_buf(true, false)
    vim.bo[buf].bufhidden = "hide"
    vim.bo[buf].buftype = "nofile"
    vim.bo[buf].swapfile = false
    local short = vim.fn.fnamemodify(filename, ":t")
    -- Basenames collide across directories (E95); fall back to a unique name.
    local name = "diff://" .. short
    if not pcall(vim.api.nvim_buf_set_name, buf, name) then
      pcall(vim.api.nvim_buf_set_name, buf, name .. "#" .. buf)
    end
    diff_bufs[key] = buf
    buf_filename[buf] = filename

    -- Save cursor + invalidate caches on leaving the diff buffer
    vim.api.nvim_create_autocmd("BufLeave", {
      buffer = buf,
      callback = function()
        if not vim.api.nvim_buf_is_valid(buf) then return end
        -- Defer cache invalidation: only invalidate if we switched to a real
        -- file buffer (not Trouble or another diff buffer). This avoids
        -- re-running git on every s/t movement in the Trouble list.
        vim.schedule(function()
          if not vim.api.nvim_buf_is_valid(buf) then return end
          local cur_win_buf = vim.api.nvim_get_current_buf()
          local ft = vim.bo[cur_win_buf].filetype
          local bt = vim.bo[cur_win_buf].buftype
          if ft ~= "trouble" and bt ~= "nofile" then
            session.buf_last_rendered[buf] = nil
            if session.file_diffs then session.file_diffs[filename] = nil end
          end
        end)
        local cur = vim.api.nvim_win_get_cursor(0)
        local hunks = buf_hunks[buf]
        if hunks then
          for si, sh in ipairs(hunks) do
            if cur[1] >= sh.start_line and cur[1] <= sh.end_line then
              local header = sh.diff and sh.diff:match("(@@[^@]+@@)") or ""
              buf_saved_cursor[buf] = {
                hunk_index = si,
                offset = cur[1] - sh.start_line,
                col = cur[2],
                header = header,
              }
              return
            end
          end
        end
        -- Not in any hunk, save raw position as fallback
        buf_saved_cursor[buf] = {
          hunk_index = 1,
          offset = 0,
          col = cur[2],
          header = "",
          raw_line = cur[1],
        }
      end,
    })



    -- Refresh and restore cursor when entering the diff buffer directly
    -- (e.g., via window-switch keybind after editing the real file)
    vim.api.nvim_create_autocmd("BufEnter", {
      buffer = buf,
      callback = function()
        if not vim.api.nvim_buf_is_valid(buf) or not buf_filename[buf] then return end
        -- Re-hide the number column: re-entering the diff buffer (e.g. a
        -- window switch back from the real file) restores it otherwise.
        window_options.hide_line_numbers(vim.api.nvim_get_current_win())
        vim.schedule(function()
          if not vim.api.nvim_buf_is_valid(buf) then return end
          -- Re-fetch diff if cache was invalidated
          local function render_and_restore()
            if not vim.api.nvim_buf_is_valid(buf) then return end
            M._refresh_diff_buffer(buf, filename)
            -- Restore saved cursor
            local saved = buf_saved_cursor[buf]
            local new_hunks = buf_hunks[buf]
            if not (saved and new_hunks) then return end
            local target_hunk = nil
            if saved.header and saved.header ~= "" then
              for _, h in ipairs(new_hunks) do
                local hh = h.diff and h.diff:match("(@@[^@]+@@)") or ""
                if hh == saved.header then
                  target_hunk = h
                  break
                end
              end
            end
            if not target_hunk and saved.hunk_index and saved.hunk_index <= #new_hunks then
              target_hunk = new_hunks[saved.hunk_index]
            end
            if target_hunk then
              local line = target_hunk.start_line + (saved.offset or 0)
              local max = vim.api.nvim_buf_line_count(buf)
              line = math.min(math.max(1, line), max)
              pcall(vim.api.nvim_win_set_cursor, 0, { line, saved.col or 0 })
              pcall(vim.cmd, "normal! zv")
            elseif saved.raw_line then
              local max = vim.api.nvim_buf_line_count(buf)
              pcall(vim.api.nvim_win_set_cursor, 0, { math.min(saved.raw_line, max), saved.col or 0 })
            end
            buf_saved_cursor[buf] = nil
          end
          if not session.file_diffs or session.file_diffs[filename] == nil then
            M._update_file_diff_cache_async(filename, render_and_restore)
          else
            render_and_restore()
          end
        end)
      end,
    })
    vim.api.nvim_create_autocmd("CursorMoved", {
      buffer = buf,
      callback = function()
        trace.span("diff.autocmd_cursor_moved", buf, nil, function()
          M._normalize_status_cursor(buf)
        end)
      end,
    })

    local kopts = { buffer = buf, silent = true }

    -- Close both diff buffer and trouble
    vim.keymap.set("n", "q", function()
      if vim.api.nvim_buf_is_valid(buf) then
        pcall(vim.api.nvim_buf_delete, buf, { force = true })
      end
      state().cleanup_diff_buffers()
    end, vim.tbl_extend("force", kopts, { desc = "Close DiffReview", nowait = true }))


    -- Jump to next/prev hunk header
    vim.keymap.set("n", "]c", function()
      local cursor = vim.api.nvim_win_get_cursor(0)
      local lines = vim.api.nvim_buf_get_lines(buf, cursor[1], -1, false)
      for i, line in ipairs(lines) do
        if line:match("^@@") then
          vim.api.nvim_win_set_cursor(0, { cursor[1] + i, 0 })
          return
        end
      end
    end, vim.tbl_extend("force", kopts, { desc = "Next hunk" }))

    vim.keymap.set("n", "[c", function()
      local cursor = vim.api.nvim_win_get_cursor(0)
      local lines = vim.api.nvim_buf_get_lines(buf, 0, cursor[1] - 1, false)
      for i = #lines, 1, -1 do
        if lines[i]:match("^@@") then
          vim.api.nvim_win_set_cursor(0, { i, 0 })
          return
        end
      end
    end, vim.tbl_extend("force", kopts, { desc = "Prev hunk" }))

    -- Jump to the corresponding line in the actual file
    vim.keymap.set("n", "<CR>", function()
      local target_file = buf_filename[buf]
      if not target_file then return end

      -- Leaving the diff preview for the real file: restore its number column.
      window_options.restore(vim.api.nvim_get_current_win())

      -- Save cursor as hunk-relative position so we can restore after re-render.
      -- Store: { hunk_index, offset_within_hunk, col, hunk_header }
      local save_cursor = vim.api.nvim_win_get_cursor(0)
      local save_hunks = buf_hunks[buf]
      if save_hunks then
        for si, sh in ipairs(save_hunks) do
          if save_cursor[1] >= sh.start_line and save_cursor[1] <= sh.end_line then
            local header = sh.diff and sh.diff:match("(@@[^@]+@@)") or ""
            buf_saved_cursor[buf] = {
              hunk_index = si,
              offset = save_cursor[1] - sh.start_line,
              col = save_cursor[2],
              header = header,
            }
            break
          end
        end
      end
      if not buf_saved_cursor[buf] or type(buf_saved_cursor[buf]) ~= "table" or not buf_saved_cursor[buf].hunk_index then
        buf_saved_cursor[buf] = { hunk_index = 1, offset = 0, col = 0, header = "" }
      end

      local hunks = buf_hunks[buf]
      local cursor = save_cursor[1]
      local raw_col = save_cursor[2] or 0
      -- Adjust column for the rendered diff gutter.
      -- The gutter (line numbers + prefix) is stored as real buffer text.
      -- The padding width = all leading spaces on a code line.
      -- To distinguish gutter padding from code indentation, check a line
      -- that has actual code (non-space after the gutter). The gutter width
      -- is the same for all lines in a hunk, so find it from any code line.
      -- Compute gutter width from the line number + prefix columns. The width can be computed from
      -- any code line by finding where the code content starts in the buffer text
      -- vs where it starts in the raw diff line (after stripping the +/-/space prefix).
      local gutter_width = 0
      if hunks then
        for _, h in ipairs(hunks) do
          if cursor >= h.start_line and cursor <= h.end_line then
            -- Parse diff lines for this hunk
            local diff_lines_list = {}
            local found_hdr = false
            for dl in h.diff:gmatch("[^\n]+") do
              if found_hdr then
                diff_lines_list[#diff_lines_list + 1] = dl
              elseif dl:match("^@@") then
                found_hdr = true
              end
            end
            -- Find a non-empty code line to measure gutter
            for l = h.start_line + 1, h.end_line do
              local buf_line = vim.api.nvim_buf_get_lines(buf, l - 1, l, false)[1] or ""
              local dl_idx = l - h.start_line
              if dl_idx >= 1 and dl_idx <= #diff_lines_list then
                local diff_line = diff_lines_list[dl_idx]
                local code = diff_line:sub(2) -- strip +/-/space prefix
                -- Find first non-space char in code
                local code_indent = code:find("%S")
                if code_indent then
                  -- Find same char in buffer line
                  local buf_indent = buf_line:find("%S")
                  if buf_indent then
                    -- Gutter = buf_indent - code_indent
                    gutter_width = buf_indent - code_indent
                    break
                  end
                end
              end
            end
            break
          end
        end
      end
      local cursor_col = math.max(0, raw_col - gutter_width)

      -- No hunks (e.g. "No changes" / empty new file): just open the file
      if not hunks or #hunks == 0 then
        vim.cmd.edit(target_file)
        vim.cmd("normal! zz")
        return
      end

      -- Find which hunk we're in and compute file line number
      for _, h in ipairs(hunks) do
        if cursor >= h.start_line and cursor <= h.end_line then
          -- Parse the @@ header to get the new-file start line
          local new_start = h.diff:match("%+(%d+)")
          new_start = tonumber(new_start) or 1

          -- Walk diff lines to find file line at cursor position
          -- Cursor offset within the hunk (0 = @@ line, 1 = first code line)
          local offset_in_hunk = cursor - h.start_line
          if offset_in_hunk == 0 then
            -- On the @@ line itself, jump to hunk start
            vim.cmd.edit(target_file)
            pcall(vim.api.nvim_win_set_cursor, 0, { new_start, cursor_col })
            vim.cmd("normal! zz")
            return
          end

          -- Walk through diff code lines counting file lines
          local diff_lines = {}
          local found_header = false
          for diff_line in h.diff:gmatch("[^\n]+") do
            if found_header then
              diff_lines[#diff_lines + 1] = diff_line
            elseif diff_line:match("^@@") then
              found_header = true
            end
          end

          local file_line = new_start
          local last_valid_line = new_start
          for i = 1, math.min(offset_in_hunk, #diff_lines) do
            local prefix = diff_lines[i]:sub(1, 1)
            if prefix == " " or prefix == "+" then
              last_valid_line = file_line
              if i < offset_in_hunk then
                file_line = file_line + 1
              end
            elseif prefix == "-" then
              -- Deleted line: doesn't exist in new file
              -- last_valid_line stays as-is
            end
          end

          vim.cmd.edit(target_file)
          local max_line = vim.api.nvim_buf_line_count(0)
          local target_line = math.min(last_valid_line, max_line)
          pcall(vim.api.nvim_win_set_cursor, 0, { target_line, cursor_col })
          vim.cmd("normal! zz")
          return
        end
      end
      -- Cursor not in any hunk: just open the file
      vim.cmd.edit(target_file)
      vim.cmd("normal! zz")
    end, vim.tbl_extend("force", kopts, { desc = "Jump to file", nowait = true }))

    -- Stage the hunk under cursor, then jump to the next hunk
    vim.keymap.set("n", "S", function()
      local patch, hunk_start = get_hunk_at_cursor(buf)
      if not patch then
        vim.notify("No hunk under cursor", vim.log.levels.WARN)
        return
      end
      -- Find the index of the current hunk so we can jump to the next one
      local cur_hunk_idx = nil
      local hunks = buf_hunks[buf]
      if hunks and hunk_start then
        for i, h in ipairs(hunks) do
          if h.start_line == hunk_start then
            cur_hunk_idx = i
            break
          end
        end
      end
      git_data().stage_patch_async(patch, function(ok)
        if not ok then return end
        notify_debug("Hunk staged")
        local win = vim.api.nvim_get_current_win()
        local filename = buf_filename[buf]
        -- Move to the next hunk. The staged hunk keeps its position (it only
        -- folds), so "next" is the following index in the rebuilt hunk map.
        local function goto_next()
          if not vim.api.nvim_win_is_valid(win) or vim.api.nvim_win_get_buf(win) ~= buf then
            return
          end
          local new_hunks = buf_hunks[buf]
          if new_hunks and cur_hunk_idx then
            local target = new_hunks[cur_hunk_idx + 1] or new_hunks[cur_hunk_idx]
            if target then
              local max = vim.api.nvim_buf_line_count(buf)
              pcall(vim.api.nvim_win_set_cursor, win, { math.min(target.start_line, max), 0 })
            end
          end
        end
        -- Re-render this file's diff buffer so the staged hunk folds in place.
        if filename then
          M.refresh_open_diff_buffer(filename)
        end
        goto_next()
        -- The async list refresh re-renders the buffer (resetting the
        -- cursor), so re-assert the next-hunk position once it settles.
        vim.defer_fn(goto_next, 60)
      end)
    end, vim.tbl_extend("force", kopts, { desc = "Stage hunk", nowait = true }))

    -- Unstage the hunk under cursor
    vim.keymap.set("n", "U", function()
      local patch, _ = get_hunk_at_cursor(buf)
      if not patch then
        vim.notify("No hunk under cursor", vim.log.levels.WARN)
        return
      end
      git_data().unstage_patch_async(patch, function(ok)
        if not ok then return end
        notify_debug("Hunk unstaged")
        local win = vim.api.nvim_get_current_win()
        local filename = buf_filename[buf]
        -- Find the current hunk so we can stay on it after the re-render
        -- (it expands in place; unstaging must not jump to the buffer top).
        local cur_idx, cur_offset = nil, 0
        local cursor_line = vim.api.nvim_win_get_cursor(0)[1]
        local hunks = buf_hunks[buf]
        if hunks then
          for i, h in ipairs(hunks) do
            if cursor_line >= h.start_line and cursor_line <= h.end_line then
              cur_idx = i
              cur_offset = cursor_line - h.start_line
              break
            end
          end
        end
        local function stay()
          if not vim.api.nvim_win_is_valid(win) or vim.api.nvim_win_get_buf(win) ~= buf then
            return
          end
          local new_hunks = buf_hunks[buf]
          if new_hunks and cur_idx and new_hunks[cur_idx] then
            local max = vim.api.nvim_buf_line_count(buf)
            local line = math.min(new_hunks[cur_idx].start_line + cur_offset, max)
            pcall(vim.api.nvim_win_set_cursor, win, { line, 0 })
          end
        end
        -- Re-render this file's diff buffer so the hunk expands in place.
        if filename then
          M.refresh_open_diff_buffer(filename)
        end
        stay()
        -- The async list refresh re-renders the buffer (resetting the
        -- cursor), so re-assert the current-hunk position once it settles.
        vim.defer_fn(stay, 60)
      end)
    end, vim.tbl_extend("force", kopts, { desc = "Unstage hunk", nowait = true }))

    -- Toggle fold (collapse/expand) the hunk under cursor
    vim.keymap.set("n", "<Tab>", function()
      local hunks = buf_hunks[buf]
      if not hunks then
        vim.notify("No hunk map for buffer", vim.log.levels.WARN)
        return
      end
      local cursor = vim.api.nvim_win_get_cursor(0)[1]
      local found = false
      for i, h in ipairs(hunks) do
        if cursor >= h.start_line and cursor <= h.end_line then
          found = true
          h.folded = not h.folded
          notify_debug("Hunk " .. i .. " folded=" .. tostring(h.folded) .. " range=" .. h.start_line .. "-" .. h.end_line)
          M._render_with_folds(buf)
          pcall(vim.api.nvim_win_set_cursor, 0, { h.start_line, 0 })
          return
        end
      end
      if not found then
        local ranges = {}
        for i, h in ipairs(hunks) do
          ranges[#ranges + 1] = i .. ":[" .. h.start_line .. "-" .. h.end_line .. "]"
        end
        vim.notify("Cursor " .. cursor .. " not in any hunk: " .. table.concat(ranges, ", "), vim.log.levels.WARN)
      end
    end, vim.tbl_extend("force", kopts, { desc = "Toggle hunk fold", nowait = true }))
  end

  return buf
end

--- Compute hunk map from diff text. Each hunk's rendered lines are:
--- 1 line for @@ separator + N code lines.
--- N = number of lines in hunk.diff AFTER the @@ header, EXCLUDING
--- the file header lines (diff --git, index, ---, +++).
---@param diff_text string
---@return table[]
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

--- Re-render buffer respecting fold state (placeholder — full fold
--- support would need tracking which hunks to hide/show)
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

--- Re-fetch diff data and re-render a diff buffer after staging/unstaging.
--- Staged hunks get auto-folded.
---@param buf number
---@param filename string
function M._refresh_diff_buffer(buf, filename)
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
        line_hl_group = "DiffReviewHunkHeader",
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

---@param buf number
---@param item_diff string?
---@return table?
function M._highlight_active_hunk(buf, item_diff)
  vim.api.nvim_buf_clear_namespace(buf, ui.active_hunk_header_ns, 0, -1)
  if not item_diff then return nil end

  local hunks = buf_hunks[buf]
  if not hunks then return nil end

  for _, hunk in ipairs(hunks) do
    if hunk.diff == item_diff then
      pcall(vim.api.nvim_buf_set_extmark, buf, ui.active_hunk_header_ns, hunk.start_line - 1, 0, {
        line_hl_group = "DiffReviewActiveHunkHeader",
        priority = ui.active_hunk_header_priority,
      })
      return hunk
    end
  end
end

--- Re-fetch the diff for a single file and update the cache.
--- Called before _refresh_diff_buffer to pick up file edits.
---@param filename string
---@param cb? fun()
function M._update_file_diff_cache_async(filename, cb)
  session.file_diffs = session.file_diffs or {}
  session.file_hunk_staged = session.file_hunk_staged or {}
  -- Untracked files: build the diff from disk, never from git. Cache `false`
  -- (not nil) for empty/binary so the preview guard treats it as "checked"
  -- and doesn't re-run this on every cursor move.
  local relpath = session.untracked and session.untracked[filename]
  if relpath then
    local diff_text = git_data()._build_untracked_diff(filename, relpath)
    -- Cache `false` (not nil) as a "checked, no diff" sentinel; cast to satisfy the
    -- string-valued field type while preserving the boolean sentinel at runtime.
    session.file_diffs[filename] = diff_text or false
    session.file_hunk_staged[filename] = diff_text and { false } or nil
    if cb then cb() end
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

--- Refresh an open diff buffer for the given filename (if one exists).
--- Called from Trouble S/U actions to sync the diff buffer.
---@param filename string
function M.refresh_open_diff_buffer(filename)
  local key = "diff:" .. filename
  diff_bufs = diff_bufs or {}
  local buf = diff_bufs[key]
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end

  -- Re-fetch diff data for this file only (cache is stale after staging)
  M._update_file_diff_cache_async(filename, function()
    if not vim.api.nvim_buf_is_valid(buf) then return end
    session.buf_last_rendered[buf] = nil  -- force re-render
    M._refresh_diff_buffer(buf, filename)
  end)
end

return M
