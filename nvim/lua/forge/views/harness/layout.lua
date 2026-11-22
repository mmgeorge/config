local M = {}
local composer_observer = {}

---@param window integer
---@param callback fun()
---@return fun()
function M.observe_composer(window, callback)
  assert(not composer_observer[window], "composer geometry already has an owner")
  composer_observer[window] = callback
  return function() if composer_observer[window] == callback then composer_observer[window] = nil end end
end

local composer_mode_group = vim.api.nvim_create_augroup("ForgeHarnessComposerMode", { clear = false })

local config = require("forge.infra.config")
local input_gutter = require("forge.shared.input_gutter")

-- Transcript windows soft-wrap raw Markdown, so `list:-1` hangs a wrapped list item under its text
-- by indenting continuations to the width of the `formatlistpat` match on the item's first row.
M.transcript_wrapping = { indent = true, options = "shift:0,list:-1" }
-- The match spans leading indentation, the bullet or number, and the spaces before the item text.
M.list_item_pattern = [[^\s*\%([-*+]\|\d\+[.)]\)\s\+]]

---Recognize Markdown list items as hanging-indent headers in a transcript buffer.
---@param buf integer
function M.configure_transcript_buffer(buf)
  vim.bo[buf].formatlistpat = M.list_item_pattern
end

---@param win integer
function M.configure_gutterless_window(win)
  vim.wo[win].number = false
  vim.wo[win].relativenumber = false
  vim.wo[win].signcolumn = "no"
  vim.wo[win].foldcolumn = "0"
  vim.wo[win].statuscolumn = ""
end

---Depth of deliberate Insert-mode focus changes in progress, during which composer entry keeps
---the mode. Neovim ignores `startinsert` while Insert mode is still active, so a guard that stopped
---Insert mode could not be undone by the caller that wants the composer to stay in it.
local insert_entry_depth = 0

---Move focus with `action` while composer entry keeps Insert mode, for a caller that returns the
---user to Insert mode in the composer it focuses.
---@param action fun()
function M.enter_composer_in_insert(action)
  insert_entry_depth = insert_entry_depth + 1
  local ok, failure = pcall(action)
  insert_entry_depth = insert_entry_depth - 1
  if not ok then error(failure, 0) end
end

---Leave Insert mode whenever focus enters `buf`, unless a caller enters the composer in Insert
---mode on purpose or a closing popup returns the user to the place they were typing. The composer
---starts each visit in Normal mode; the transcript never takes typed edits, which a mouse click
---from an Insert-mode composer would otherwise send to its rows.
---@param buf integer
---@param description string
local function keep_normal_on_entry(buf, description)
  vim.api.nvim_clear_autocmds({ group = composer_mode_group, buffer = buf })
  vim.api.nvim_create_autocmd({ "BufEnter", "WinEnter" }, {
    group = composer_mode_group,
    buffer = buf,
    callback = function()
      if insert_entry_depth == 0 and not require("forge.infra.popup_window").returning_to_insert then
        vim.cmd("stopinsert")
      end
    end,
    desc = description,
  })
end

---@param name string
---@return integer
local function named_buffer(name)
  local existing = vim.fn.bufnr(name)
  if existing >= 0 and vim.api.nvim_buf_is_valid(existing) then return existing end
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_name(buf, name)
  return buf
end

---@param session_id? string
---@return string
local function transcript_name(session_id)
  local name = config.options.harness.buffer_name
  if session_id and session_id ~= "" then name = name .. "://" .. session_id end
  return name
end

---@param session_id? string
---@return integer
function M.create_transcript_buffer(session_id)
  local buf = named_buffer(transcript_name(session_id))
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].bufhidden = "hide"
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = "ForgeHarness"
  vim.bo[buf].modifiable = false
  M.configure_transcript_buffer(buf)
  keep_normal_on_entry(buf, "Keep the Harness transcript out of Insert mode")
  return buf
end

---@param transcript_buffer integer
---@param composer_buffer integer
---@param session_id string
function M.rename_session_buffers(transcript_buffer, composer_buffer, session_id)
  M.rename_buffer(transcript_buffer, transcript_name(session_id))
  M.rename_buffer(composer_buffer, config.options.harness.composer_name .. "://" .. session_id)
end

---Rename `buffer` without leaving its former name behind. Neovim keeps a renamed buffer's former
---name on a new unloaded, unlisted buffer, the alternate file, so a view opened under a
---placeholder name and then bound to its session left one such buffer per pane in `:ls!`.
---@param buffer integer
---@param name string
function M.rename_buffer(buffer, name)
  local previous_name = vim.api.nvim_buf_get_name(buffer)
  if previous_name == name then return end
  vim.api.nvim_buf_set_name(buffer, name)
  if previous_name == "" then return end
  for _, candidate in ipairs(vim.api.nvim_list_bufs()) do
    if candidate ~= buffer and vim.api.nvim_buf_get_name(candidate) == previous_name
      and not vim.api.nvim_buf_is_loaded(candidate) and not vim.bo[candidate].buflisted then
      vim.api.nvim_buf_delete(candidate, { force = true })
    end
  end
end

---@param win integer
function M.configure_transcript_window(win)
  vim.wo[win].winfixheight = false
  vim.wo[win].wrap = true
  vim.wo[win].linebreak = true
  vim.wo[win].showbreak = ""
  vim.wo[win].breakindent = M.transcript_wrapping.indent
  vim.wo[win].breakindentopt = M.transcript_wrapping.options
  vim.wo[win].foldmethod = "manual"
  vim.wo[win].foldenable = false
  -- Tail following positions the window in whole lines; smooth scrolling would let Neovim's own
  -- cursor scroll start the window inside a wrapped line, and the two would alternate per update.
  vim.api.nvim_set_option_value("smoothscroll", false, { scope = "local", win = win })
  M.configure_gutterless_window(win)
end

---@param timeline_key? string
---@param open_mode? "current"|"tab"
---@return integer, integer, integer, integer, integer
function M.open(timeline_key, open_mode)
  local options = config.options.harness
  local placeholder_buffer
  if open_mode ~= "current" then
    vim.cmd("tabnew")
    -- `tabnew` shows a fresh empty buffer, which the transcript replaces below; deleting it keeps
    -- each opened tab from leaving an unnamed buffer behind.
    placeholder_buffer = vim.api.nvim_get_current_buf()
  elseif vim.wo[vim.api.nvim_get_current_win()].winfixbuf then
    -- Keep buffer-locked views intact while opening Harness in the same tab.
    vim.cmd("aboveleft split")
    vim.wo[vim.api.nvim_get_current_win()].winfixbuf = false
  end
  local tabpage = vim.api.nvim_get_current_tabpage()
  local transcript_win = vim.api.nvim_get_current_win()
  local transcript_buf = M.create_transcript_buffer(timeline_key)
  vim.api.nvim_win_set_buf(transcript_win, transcript_buf)
  if placeholder_buffer and placeholder_buffer ~= transcript_buf and vim.api.nvim_buf_is_valid(placeholder_buffer)
    and vim.api.nvim_buf_get_name(placeholder_buffer) == "" and not vim.bo[placeholder_buffer].modified
    and #vim.fn.win_findbuf(placeholder_buffer) == 0 then
    vim.api.nvim_buf_delete(placeholder_buffer, { force = true })
  end
  M.configure_transcript_window(transcript_win)

  vim.cmd("belowright " .. tostring(options.composer_min_height) .. "split")
  local composer_win = vim.api.nvim_get_current_win()
  local composer_buf = M.create_composer_buffer(timeline_key)
  vim.api.nvim_win_set_buf(composer_win, composer_buf)
  vim.wo[composer_win].wrap = true
  vim.wo[composer_win].linebreak = true
  vim.wo[composer_win].showbreak = ""
  -- Local scope only: assigning through `vim.wo[win]` also sets the global value, which every
  -- later window, including another Harness tab's transcript, would inherit.
  vim.api.nvim_set_option_value("smoothscroll", true, { scope = "local", win = composer_win })
  vim.wo[composer_win].winfixheight = true
  input_gutter.apply(composer_win)
  return transcript_buf, transcript_win, composer_buf, composer_win, tabpage
end

---@param timeline_key? string
---@return integer
function M.create_composer_buffer(timeline_key)
  local options = config.options.harness
  local buf = named_buffer(timeline_key and (options.composer_name .. "://" .. timeline_key) or options.composer_name)
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].bufhidden = "hide"
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = "ForgeHarnessInput"
  vim.bo[buf].modifiable = true
  keep_normal_on_entry(buf, "Keep Harness input in Normal mode when focused")
  return buf
end

---Open a hidden window over `replaced_window` that shows `transcript_buf` with the transcript's
---window options and the same width, so a session can be opened, rendered, and positioned there
---while `replaced_window` still shows the session being left. `place_transcript` then puts it on
---screen in one step.
---@param transcript_buf integer
---@param replaced_window integer
---@param window_options? table<string, any> Local options that differ from the replaced window's,
---  such as its separator before a status split changed it.
---@return integer
function M.open_hidden_transcript(transcript_buf, replaced_window, window_options)
  -- A new window copies the current window's local options, so it is opened from the window it
  -- replaces rather than from the composer that usually has focus.
  local window = vim.api.nvim_win_call(replaced_window, function()
    return vim.api.nvim_open_win(transcript_buf, false, {
      relative = "win", win = replaced_window, row = 0, col = 0, hide = true,
      width = vim.api.nvim_win_get_width(replaced_window), height = vim.api.nvim_win_get_height(replaced_window),
    })
  end)
  vim.api.nvim_set_option_value("winfixbuf", false, { scope = "local", win = window })
  for name, value in pairs(window_options or {}) do
    vim.api.nvim_set_option_value(name, value, { scope = "local", win = window })
  end
  M.configure_transcript_window(window)
  return window
end

---Put a window from `open_hidden_transcript` in place of the window it covered, keeping its
---buffer, view, and window identity, and close the covered window.
---@param window integer
---@param replaced_window integer
function M.place_transcript(window, replaced_window)
  local view = vim.api.nvim_win_call(window, vim.fn.winsaveview)
  vim.api.nvim_win_set_config(window, { split = "above", win = replaced_window })
  vim.api.nvim_win_close(replaced_window, true)
  vim.api.nvim_win_call(window, function() vim.fn.winrestview(view) end)
end

---Remove the autocommand groups `attach_auto_height` and `attach_scroll_boundary` created for a
---view's buffers. The composer group also holds a global resize autocommand, which deleting the
---buffer alone would leave behind.
---@param transcript_buf integer
---@param composer_buf integer
function M.detach(transcript_buf, composer_buf)
  pcall(vim.api.nvim_del_augroup_by_name, "ForgeHarnessComposer" .. composer_buf)
  M.detach_transcript(transcript_buf)
end

---Remove the autocommand group `attach_scroll_boundary` created for a transcript buffer, as a
---view does when it replaces its transcript buffer and keeps its composer.
---@param transcript_buf integer
function M.detach_transcript(transcript_buf)
  pcall(vim.api.nvim_del_augroup_by_name, "ForgeHarnessScroll" .. transcript_buf)
end

---@param composer_buf integer
---@param composer_win integer
function M.attach_auto_height(composer_buf, composer_win)
  local group = vim.api.nvim_create_augroup("ForgeHarnessComposer" .. composer_buf, { clear = true })
  local function resize()
    M.resize_composer(composer_buf, composer_win)
  end
  vim.api.nvim_create_autocmd({ "TextChanged", "TextChangedI" }, {
    group = group,
    buffer = composer_buf,
    callback = function() M.resize_composer(composer_buf, composer_win, true) end,
  })
  vim.api.nvim_create_autocmd("BufWinEnter", {
    group = group,
    buffer = composer_buf,
    callback = resize,
  })
  vim.api.nvim_create_autocmd({ "WinResized", "VimResized" }, {
    group = group,
    callback = resize,
  })
end

---@param composer_buf integer
---@param composer_win integer
---@param text_edit? boolean The caller is a composer text edit. Such an edit moves the geometry
---observer's windows only when it changes the composer height, so an edit that keeps the height
---skips the observer, which otherwise captures and restores every transcript view per keystroke.
function M.resize_composer(composer_buf, composer_win, text_edit)
  if not (vim.api.nvim_buf_is_valid(composer_buf) and vim.api.nvim_win_is_valid(composer_win)) then return end
  if vim.api.nvim_win_get_buf(composer_win) ~= composer_buf then return end
  local options = config.options.harness
  local measured = vim.api.nvim_win_text_height(composer_win, {
    start_row = 0, max_height = options.composer_max_height + 1,
  })
  local winbar_height = vim.wo[composer_win].winbar ~= "" and 1 or 0
  local height = math.max(options.composer_min_height,
    math.min(options.composer_max_height, measured.all + 1 + winbar_height))
  local view = vim.api.nvim_win_call(composer_win, vim.fn.winsaveview)
  local height_changed = vim.api.nvim_win_get_height(composer_win) ~= height
  local restore = height_changed
  if restore then vim.api.nvim_win_set_height(composer_win, height) end
  local text_rows = vim.api.nvim_win_get_height(composer_win) - winbar_height
  -- Insert-mode scrolling can precede window growth and leave the first row hidden.
  if measured.all <= text_rows and (view.topline > 1 or view.skipcol > 0 or view.topfill > 0) then
    view.topline, view.skipcol, view.topfill = 1, 0, 0
    restore = true
  end
  if restore then
    vim.api.nvim_win_call(composer_win, function() vim.fn.winrestview(view) end)
  end
  if composer_observer[composer_win] and (height_changed or not text_edit) then composer_observer[composer_win]() end
end

---Align the final display row without changing the focused window.
---With `visible_height`, the final row rests on the last row an overlay leaves uncovered instead
---of on the window's last row, where the overlay would hide it.
---@param transcript_buf integer
---@param transcript_win integer
---@param visible_height? integer Rows from the window's top edge that an overlay leaves uncovered.
function M.position_tail(transcript_buf, transcript_win, visible_height)
  vim.api.nvim_win_set_cursor(transcript_win, { vim.api.nvim_buf_line_count(transcript_buf), 0 })
  vim.api.nvim_win_call(transcript_win, function() vim.cmd("normal! $zb") end)
  if not visible_height then return end
  local topline = M.maximum_content_topline(transcript_buf, transcript_win, visible_height)
  vim.api.nvim_win_call(transcript_win, function()
    if vim.fn.line("w0") < topline then vim.fn.winrestview({ topline = topline }) end
  end)
end

---Find the topline that puts the final display row on the last text row of the window, or of the
---`visible_height` rows an overlay such as the session picker leaves uncovered.
---@param transcript_buf integer
---@param transcript_win integer
---@param visible_height? integer Rows from the window's top edge that an overlay leaves uncovered.
---@return integer
function M.maximum_content_topline(transcript_buf, transcript_win, visible_height)
  local line_count = vim.api.nvim_buf_line_count(transcript_buf)
  local window_height = vim.api.nvim_win_get_height(transcript_win)
  if visible_height then window_height = math.min(window_height, visible_height) end
  window_height = math.max(1, window_height - (vim.wo[transcript_win].winbar ~= "" and 1 or 0))
  local function tail_height(start_row)
    local ok, height = pcall(vim.api.nvim_win_text_height, transcript_win, {
      start_row = start_row,
      end_row = line_count - 1,
    })
    return ok and height.all or (line_count - start_row)
  end
  -- Measure backward from the tail in doubling spans, so the cost follows the window height and
  -- not the transcript length. Folded and concealed rows can have no height, so a span of
  -- `window_height` rows is only the first guess.
  local span = window_height
  local low = math.max(0, line_count - span)
  while low > 0 and tail_height(low) <= window_height do
    span = span * 2
    low = math.max(0, line_count - span)
  end
  if low == 0 and tail_height(0) <= window_height then return 1 end
  local high = line_count - 1
  local first_fitting_row = line_count - 1
  while low <= high do
    local middle = math.floor((low + high) / 2)
    if tail_height(middle) > window_height then
      low = middle + 1
    else
      first_fitting_row = middle
      high = middle - 1
    end
  end
  return first_fitting_row + 1
end

---Pull a transcript window back when it shows empty rows below the final row.
---The boundary depends on rendered heights, so it must be applied again whenever those change
---without a scroll, such as when Markdown finishes rendering rows that entered the window.
---@param transcript_buf integer
---@param transcript_win integer
---@param visible_height? integer Rows from the window's top edge that an overlay leaves uncovered.
function M.clamp_scroll(transcript_buf, transcript_win, visible_height)
  if not vim.api.nvim_win_is_valid(transcript_win)
    or vim.api.nvim_win_get_buf(transcript_win) ~= transcript_buf then return end
  visible_height = visible_height or vim.w[transcript_win].forge_harness_preview_height
  local maximum_topline = M.maximum_content_topline(transcript_buf, transcript_win, visible_height)
  vim.api.nvim_win_call(transcript_win, function()
    local view = vim.fn.winsaveview()
    if view.topline <= maximum_topline then return end
    view.topline = maximum_topline
    vim.fn.winrestview(view)
  end)
end

---@param transcript_buf integer
---@param transcript_win integer
---@param visible_height? fun(): integer? Reads the rows an overlay currently leaves uncovered, for a
---  window an overlay partly covers; the boundary then follows the overlay as it resizes.
function M.attach_scroll_boundary(transcript_buf, transcript_win, visible_height)
  local group = vim.api.nvim_create_augroup("ForgeHarnessScroll" .. transcript_buf, { clear = true })
  vim.api.nvim_create_autocmd("WinScrolled", {
    group = group,
    buffer = transcript_buf,
    callback = function()
      M.clamp_scroll(transcript_buf, transcript_win, visible_height and visible_height() or nil)
    end,
  })
end

return M
