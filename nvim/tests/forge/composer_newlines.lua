vim.loader.enable(false)
vim.o.lines, vim.o.columns = 40, 120
local layout = require("forge.views.harness.layout")
local _, transcript, composer, window = layout.open("newline-regression")
vim.wo[window].winbar = "submit"
vim.wo[window].scrolloff = 0

local function view()
  return vim.api.nvim_win_call(window, vim.fn.winsaveview)
end

local success, failure = xpcall(function()
  local lines = { "test", "", "", "" }
  vim.api.nvim_buf_set_lines(composer, 0, -1, false, lines)
  vim.api.nvim_win_set_height(window, 3)
  vim.api.nvim_win_call(window, function()
    vim.fn.winrestview({ lnum = 4, col = 0, topline = 3 })
  end)
  assert(view().topline > 1, "fixture did not reproduce pre-resize scrolling")
  layout.resize_composer(composer, window)
  assert(view().topline == 1, "newlines hid the first line after composer growth")
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), { 4, 0 }), "growth moved the insertion point")
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(composer, 0, -1, false), lines), "growth changed draft text")

  local long_lines = {}
  for index = 1, 40 do long_lines[index] = "line " .. index end
  vim.api.nvim_buf_set_lines(composer, 0, -1, false, long_lines)
  layout.resize_composer(composer, window)
  vim.api.nvim_win_call(window, function()
    vim.fn.winrestview({ lnum = 35, col = 2, topline = 30 })
  end)
  local scrolled = view()
  vim.api.nvim_set_current_win(transcript)
  layout.resize_composer(composer, window)
  assert(vim.api.nvim_get_current_win() == transcript, "resize stole focus")
  assert(view().topline == scrolled.topline, "long draft lost its scrolled viewport")
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), { 35, 2 }), "long draft cursor moved")

  lines = { string.rep("wrapped text ", 200), "", "", "" }
  vim.api.nvim_buf_set_lines(composer, 0, -1, false, lines)
  vim.api.nvim_win_call(window, function()
    vim.fn.winrestview({ lnum = 4, col = 0, topline = 2 })
  end)
  layout.resize_composer(composer, window)
  assert(view().topline > 1, "wrapped draft was incorrectly treated as fitting the window")
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), { 4, 0 }), "wrapped draft cursor moved")
end, debug.traceback)

if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("composer newlines: short drafts remain visible, long and wrapped drafts retain their viewport")
vim.cmd("qa!")
