-- Unit tests for `forge.views.harness.layout` (views/harness/layout.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["scroll_clamp_measures_the_tail"] = function()
  local layout = require("forge.views.harness.layout")
  local buffer = vim.api.nvim_create_buf(false, true)
  local window = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(window, buffer)
  MiniTest.finally(function() pcall(vim.api.nvim_buf_delete, buffer, { force = true }) end)
  local rows = {}
  for index = 1, 4000 do rows[index] = "row " .. index end
  vim.api.nvim_buf_set_lines(buffer, 0, -1, true, rows)
  vim.wo[window].wrap, vim.wo[window].winbar = false, ""
  local height = vim.api.nvim_win_get_height(window)

  local text_height, measured_rows = vim.api.nvim_win_text_height, 0
  vim.api.nvim_win_text_height = function(target, options)
    measured_rows = measured_rows + options.end_row - options.start_row + 1
    return text_height(target, options)
  end
  MiniTest.finally(function() vim.api.nvim_win_text_height = text_height end)

  -- The last full window of rows starts `height - 1` rows above the final row.
  MiniTest.expect.equality(layout.maximum_content_topline(buffer, window), 4000 - height + 1)
  assert(measured_rows < 40 * height, "scroll clamp measured " .. measured_rows .. " rows of a 4000-row transcript")

  -- Rows hidden in a closed fold take one screen row together, so the tail reaches further back.
  vim.wo[window].foldmethod, vim.wo[window].foldenable = "manual", true
  vim.api.nvim_win_call(window, function() vim.cmd("3901,3990fold") end)
  MiniTest.expect.equality(layout.maximum_content_topline(buffer, window), 4000 - height + 1 - 89)

  -- A transcript shorter than the window never scrolls.
  vim.api.nvim_win_call(window, function() vim.cmd("normal! zE") end)
  vim.api.nvim_buf_set_lines(buffer, 0, -1, true, { "one", "two" })
  MiniTest.expect.equality(layout.maximum_content_topline(buffer, window), 1)
end

return T
