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

T["tail_and_scroll_clamp_honor_the_rows_an_overlay_leaves_uncovered"] = function()
  local layout = require("forge.views.harness.layout")
  local buffer = vim.api.nvim_create_buf(false, true)
  local window = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(window, buffer)
  MiniTest.finally(function() pcall(vim.api.nvim_buf_delete, buffer, { force = true }) end)
  local rows = {}
  for index = 1, 60 do rows[index] = "row " .. index end
  vim.api.nvim_buf_set_lines(buffer, 0, -1, true, rows)
  vim.wo[window].wrap, vim.wo[window].winbar = false, ""
  local height = vim.api.nvim_win_get_height(window)
  assert(height > 12, "the fixture window is too short to be partly covered")
  ---@return integer topline
  local function topline() return vim.fn.line("w0", window) end

  -- An overlay that leaves eight rows uncovered moves the boundary to the eighth row.
  MiniTest.expect.equality(layout.maximum_content_topline(buffer, window, 8), 53)
  layout.position_tail(buffer, window, 8)
  MiniTest.expect.equality(topline(), 53)
  layout.clamp_scroll(buffer, window, 8)
  MiniTest.expect.equality(topline(), 53)

  -- The winbar takes one of the uncovered rows.
  vim.wo[window].winbar = "transcript"
  MiniTest.expect.equality(layout.maximum_content_topline(buffer, window, 8), 54)
  vim.wo[window].winbar = ""

  -- More uncovered rows than the window has change nothing.
  MiniTest.expect.equality(layout.maximum_content_topline(buffer, window, height + 10), 60 - height + 1)

  -- Without an overlay the boundary is the window's last row again.
  layout.clamp_scroll(buffer, window)
  MiniTest.expect.equality(topline(), 60 - height + 1)
end

T["composer_edit_notifies_geometry_only_on_height_change"] = function()
  local layout = require("forge.views.harness.layout")
  local composer = vim.api.nvim_create_buf(false, true)
  vim.cmd("belowright split")
  local window = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(window, composer)
  MiniTest.finally(function() pcall(vim.api.nvim_buf_delete, composer, { force = true }) end)
  local notified = 0
  layout.observe_composer(window, function() notified = notified + 1 end)

  layout.resize_composer(composer, window)
  MiniTest.expect.equality(notified, 1)
  local height = vim.api.nvim_win_get_height(window)

  vim.api.nvim_buf_set_lines(composer, 0, -1, true, { "one row" })
  layout.resize_composer(composer, window, true)
  MiniTest.expect.equality({ notified, vim.api.nvim_win_get_height(window) }, { 1, height })

  vim.api.nvim_buf_set_lines(composer, 0, -1, true, { "1", "2", "3", "4", "5", "6" })
  layout.resize_composer(composer, window, true)
  assert(vim.api.nvim_win_get_height(window) > height, "six rows did not grow the composer")
  MiniTest.expect.equality(notified, 2)
end

-- Smooth scrolling lets a window start inside a wrapped line. Two owners that position a following
-- transcript, one by screen rows and one by whole lines, then alternate its first row.
T["transcript_never_scrolls_smoothly_and_composer_leaves_global_untouched"] = function()
  require("forge.infra.config").setup({})
  local layout = require("forge.views.harness.layout")
  local global = vim.go.smoothscroll
  MiniTest.finally(function() vim.go.smoothscroll = global end)
  vim.go.smoothscroll = false
  local _, first_transcript = layout.open("first", "tab")
  MiniTest.expect.equality(vim.go.smoothscroll, false)
  MiniTest.expect.equality(vim.wo[first_transcript].smoothscroll, false)
  vim.go.smoothscroll = true
  local _, second_transcript, _, second_composer = layout.open("second", "tab")
  MiniTest.expect.equality(vim.wo[second_transcript].smoothscroll, false)
  MiniTest.expect.equality(vim.wo[second_composer].smoothscroll, true)
  MiniTest.expect.equality(vim.go.smoothscroll, true)
end


-- Focus moved from an Insert-mode composer into the transcript, as a mouse click does, kept
-- Insert mode there, and Enter split a rendered transcript row until the replica rejected the
-- buffer. Entering the transcript leaves Insert mode, so it never takes typed edits.
T["entering_the_transcript_leaves_insert_mode"] = function()
  require("forge.infra.config").setup({})
  local layout = require("forge.views.harness.layout")
  local transcript_buf, transcript_win, composer_buf = layout.open("insert-entry")
  local stop_count = 0
  local original_cmd = vim.cmd
  vim.cmd = setmetatable({}, { __index = original_cmd, __call = function(_, command)
    if command == "stopinsert" then stop_count = stop_count + 1 end
    return original_cmd(command)
  end })
  MiniTest.finally(function() vim.cmd = original_cmd end)
  vim.keymap.set("i", "<F2>", function()
    stop_count = 0
    vim.api.nvim_set_current_win(transcript_win)
  end, { buffer = composer_buf })
  vim.api.nvim_feedkeys(vim.keycode("i<F2>"), "xt", false)
  vim.cmd = original_cmd
  MiniTest.expect.equality(
    { vim.api.nvim_get_current_buf() == transcript_buf, stop_count > 0, vim.bo[transcript_buf].modifiable },
    { true, true, false })
end

return T
