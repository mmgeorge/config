local SessionPreview = {}

local layout = require("forge.views.harness.layout")

local active = nil

-- The preview draws in a float over the transcript window instead of in that window. The window
-- keeps its transcript buffer, view, folds, and host view throughout, because swapping the buffer
-- closed the transcript's host view, which discards the user's expanded sections, and stripped
-- the presentation the view owns. The float sits below every picker.
local preview_zindex = 60

---A float relative to a window is placed from the window's first text row, below its winbar, and
---`nvim_win_get_height` counts only text rows, so the float covers the text area from row 0.
---@param preview table
---@return integer row, integer height Rows of the transcript window the float covers.
local function float_geometry(preview)
  local height = vim.api.nvim_win_get_height(preview.transcript_win)
  if preview.visible_height then height = math.min(height, preview.visible_height) end
  return 0, math.max(1, height)
end

---@param preview table
local function show(preview)
  if not vim.api.nvim_win_is_valid(preview.transcript_win) then return end
  local row, height = float_geometry(preview)
  local config = { relative = "win", win = preview.transcript_win, row = row, col = 0,
    width = vim.api.nvim_win_get_width(preview.transcript_win), height = height,
    focusable = false, mouse = false, zindex = preview_zindex, style = "minimal" }
  if preview.window and vim.api.nvim_win_is_valid(preview.window) then
    vim.api.nvim_win_set_config(preview.window, config)
  else
    preview.window = vim.api.nvim_open_win(preview.buffer, false, config)
    local window = preview.window
    for name, value in pairs({ wrap = true, linebreak = true, breakindent = layout.transcript_wrapping.indent,
      breakindentopt = layout.transcript_wrapping.options, foldenable = false, scrolloff = 0,
      smoothscroll = false, winhighlight = "NormalFloat:Normal" }) do
      vim.api.nvim_set_option_value(name, value, { scope = "local", win = window })
    end
    layout.attach_scroll_boundary(preview.buffer, window)
  end
  layout.position_tail(preview.buffer, preview.window)
  if preview.status_view and not preview.status_suspended then
    preview.status_suspended = true
    preview.status_view:suspend(true)
  end
end

---@param preview table
local function hide(preview)
  if preview.window and vim.api.nvim_win_is_valid(preview.window) then vim.api.nvim_win_close(preview.window, true) end
  preview.window = nil
  if preview.status_suspended then
    preview.status_suspended = false
    preview.status_view:suspend(false)
  end
end

---Prepare a preview over `transcript_win`; nothing is drawn until a snapshot is rendered.
---@param transcript_win integer
---@param status_view? HarnessStatusView Status split hidden while another session is previewed.
---@return table
function SessionPreview.open(transcript_win, status_view)
  SessionPreview.close()
  local buffer = vim.api.nvim_create_buf(false, true)
  vim.bo[buffer].buftype = "nofile"
  -- Hiding the float for the current session must keep the buffer for the next preview.
  vim.bo[buffer].bufhidden = "hide"
  vim.bo[buffer].swapfile = false
  vim.bo[buffer].filetype = "ForgeHarness"
  vim.bo[buffer].modifiable = false
  layout.configure_transcript_buffer(buffer)
  active = {
    status_view = status_view,
    transcript_win = transcript_win,
    buffer = buffer,
  }
  return active
end

---@param transcript_win? integer
---@return boolean
function SessionPreview.is_open(transcript_win)
  return active ~= nil and (transcript_win == nil or active.transcript_win == transcript_win)
end

---Show a saved session's latest rows. The session picker floats over the lower rows of the
---transcript window, so the preview covers only the rows it leaves uncovered and ends on the last
---of them; resting it on the window's own last row would hide the latest exchange behind the picker.
---@param snapshot table
---@param visible_height? integer Rows from the transcript window's top edge that the picker leaves
---  uncovered; absent when nothing covers the window.
function SessionPreview.render(snapshot, visible_height)
  if not active or not vim.api.nvim_buf_is_valid(active.buffer) then return end
  local lines = snapshot.lines or {}
  -- A session without exchanges would otherwise preview as a blank float over the transcript.
  if not vim.iter(lines):any(function(line) return vim.trim(line) ~= "" end) then
    lines = { "", "  This session has no exchanges yet." }
  end
  vim.bo[active.buffer].modifiable = true
  vim.api.nvim_buf_set_lines(active.buffer, 0, -1, false, lines)
  vim.bo[active.buffer].modifiable = false
  active.visible_height = visible_height
  show(active)
end

---Remove the preview while the highlighted entry needs none, such as the current session, whose
---own transcript is already on screen.
function SessionPreview.hide()
  if active then hide(active) end
end

function SessionPreview.close()
  local preview = active
  active = nil
  if not preview then return end
  hide(preview)
  if vim.api.nvim_buf_is_valid(preview.buffer) then vim.api.nvim_buf_delete(preview.buffer, { force = true }) end
end

return SessionPreview
