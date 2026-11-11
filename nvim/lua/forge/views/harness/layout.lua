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

local function keep_composer_normal_on_entry(buf)
  vim.api.nvim_clear_autocmds({ group = composer_mode_group, buffer = buf })
  vim.api.nvim_create_autocmd({ "BufEnter", "WinEnter" }, {
    group = composer_mode_group,
    buffer = buf,
    callback = function() vim.cmd("stopinsert") end,
    desc = "Keep Harness input in Normal mode when focused",
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
  return buf
end

---@param transcript_buffer integer
---@param composer_buffer integer
---@param session_id string
function M.rename_session_buffers(transcript_buffer, composer_buffer, session_id)
  vim.api.nvim_buf_set_name(transcript_buffer, transcript_name(session_id))
  vim.api.nvim_buf_set_name(composer_buffer, config.options.harness.composer_name .. "://" .. session_id)
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
  M.configure_gutterless_window(win)
end

---@param timeline_key? string
---@return integer, integer, integer, integer, integer
function M.open(timeline_key)
  local options = config.options.harness
  vim.cmd("tabnew")
  local tabpage = vim.api.nvim_get_current_tabpage()
  local transcript_win = vim.api.nvim_get_current_win()
  local transcript_buf = M.create_transcript_buffer(timeline_key)
  vim.api.nvim_win_set_buf(transcript_win, transcript_buf)
  M.configure_transcript_window(transcript_win)

  vim.cmd("belowright " .. tostring(options.composer_min_height) .. "split")
  local composer_win = vim.api.nvim_get_current_win()
  local composer_name = timeline_key and (options.composer_name .. "://" .. timeline_key)
    or options.composer_name
  local composer_buf = named_buffer(composer_name)
  vim.api.nvim_win_set_buf(composer_win, composer_buf)
  vim.bo[composer_buf].buftype = "nofile"
  vim.bo[composer_buf].bufhidden = "hide"
  vim.bo[composer_buf].swapfile = false
  vim.bo[composer_buf].filetype = "ForgeHarnessInput"
  vim.bo[composer_buf].modifiable = true
  vim.wo[composer_win].wrap = true
  vim.wo[composer_win].linebreak = true
  vim.wo[composer_win].showbreak = ""
  vim.wo[composer_win].smoothscroll = true
  vim.wo[composer_win].winfixheight = true
  input_gutter.apply(composer_win)
  keep_composer_normal_on_entry(composer_buf)
  return transcript_buf, transcript_win, composer_buf, composer_win, tabpage
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
---@param transcript_buf integer
---@param transcript_win integer
function M.position_tail(transcript_buf, transcript_win)
  vim.api.nvim_win_set_cursor(transcript_win, { vim.api.nvim_buf_line_count(transcript_buf), 0 })
  vim.api.nvim_win_call(transcript_win, function() vim.cmd("normal! $zb") end)
end

---@param transcript_buf integer
---@param transcript_win integer
---@return integer
function M.maximum_content_topline(transcript_buf, transcript_win)
  local line_count = vim.api.nvim_buf_line_count(transcript_buf)
  local window_height = vim.api.nvim_win_get_height(transcript_win)
    - (vim.wo[transcript_win].winbar ~= "" and 1 or 0)
  window_height = math.max(1, window_height)
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
function M.clamp_scroll(transcript_buf, transcript_win)
  if not vim.api.nvim_win_is_valid(transcript_win)
    or vim.api.nvim_win_get_buf(transcript_win) ~= transcript_buf then return end
  local maximum_topline = M.maximum_content_topline(transcript_buf, transcript_win)
  vim.api.nvim_win_call(transcript_win, function()
    local view = vim.fn.winsaveview()
    if view.topline <= maximum_topline then return end
    view.topline = maximum_topline
    vim.fn.winrestview(view)
  end)
end

---@param transcript_buf integer
---@param transcript_win integer
function M.attach_scroll_boundary(transcript_buf, transcript_win)
  local group = vim.api.nvim_create_augroup("ForgeHarnessScroll" .. transcript_buf, { clear = true })
  vim.api.nvim_create_autocmd("WinScrolled", {
    group = group,
    buffer = transcript_buf,
    callback = function() M.clamp_scroll(transcript_buf, transcript_win) end,
  })
end

return M
