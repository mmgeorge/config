-- Unit tests for `forge.views.harness.session_preview` (session_preview.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks({
  post_case = function() require("forge.views.harness.session_preview").close() end,
}) })

---@param count integer
---@return string[]
local function numbered_lines(count)
  local lines = {}
  for index = 1, count do lines[index] = "preview line " .. index end
  return lines
end

T["render_shows_the_tail_above_the_picker"] = function()
  local preview = require("forge.views.harness.session_preview")
  local window = vim.api.nvim_get_current_win()
  local transcript = buffer.scratch({ "session transcript" })
  vim.api.nvim_win_set_buf(window, transcript)
  vim.wo[window].winbar = ""
  local window_height = vim.api.nvim_win_get_height(window)
  assert(window_height > 12, "the fixture window is too short to be partly covered")
  local state = preview.open(window)
  ---@return integer topline the first preview line the float displays
  local function topline() return vim.fn.line("w0", state.window) end

  -- The picker floats over the lower rows of the transcript window, so a preview longer than the
  -- eight rows it leaves uncovered ends on the eighth row.
  preview.render({ lines = numbered_lines(60) }, 8)
  eq(vim.api.nvim_win_get_buf(window), transcript, { fail_reason = "the preview replaced the transcript in its window" })
  eq(buffer.line(state.buffer, 60), "preview line 60")
  eq(vim.api.nvim_win_get_height(state.window), 8)
  eq(topline(), 53, { fail_reason = "the latest exchange must end on the last row above the picker" })

  -- The scroll boundary would otherwise pull the tail back down behind the picker.
  vim.api.nvim_exec_autocmds("WinScrolled", { buffer = state.buffer })
  eq(topline(), 53, { fail_reason = "the scroll boundary moved the tail behind the picker" })

  -- A preview that fits in the uncovered rows starts at its first line.
  preview.render({ lines = numbered_lines(5) }, 8)
  eq(topline(), 1)

  -- Without a picker over the window, the tail rests on the window's last row.
  preview.render({ lines = numbered_lines(60) })
  eq(topline(), 60 - window_height + 1)
end

T["hide_and_close_leave_the_transcript_window_alone"] = function()
  local preview = require("forge.views.harness.session_preview")
  local window = vim.api.nvim_get_current_win()
  local rows = numbered_lines(80)
  local transcript = buffer.scratch(rows)
  vim.api.nvim_win_set_buf(window, transcript)
  vim.wo[window].winbar = "transcript winbar"
  vim.api.nvim_win_call(window, function() vim.fn.winrestview({ topline = 20, lnum = 25, col = 3 }) end)
  local view = vim.api.nvim_win_call(window, vim.fn.winsaveview)
  local state = preview.open(window)
  preview.render({ lines = numbered_lines(3) }, 10)
  -- The float starts below the winbar, which stays visible.
  eq(vim.api.nvim_win_get_config(state.window).row, 1)
  eq(vim.api.nvim_win_get_height(state.window), 9)
  preview.hide()
  eq(state.window, nil)
  preview.render({ lines = numbered_lines(3) }, 10)
  assert(state.window and vim.api.nvim_win_is_valid(state.window), "a hidden preview did not show again")
  preview.close()
  eq(vim.api.nvim_win_get_buf(window), transcript)
  eq(vim.api.nvim_win_call(window, vim.fn.winsaveview), view)
  eq(vim.wo[window].winbar, "transcript winbar")
end

return T
