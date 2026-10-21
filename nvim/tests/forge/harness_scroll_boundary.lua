vim.opt.runtimepath:prepend("nvim")
local layout = require("forge.views.harness.layout")
local transcript = vim.api.nvim_create_buf(false, true)
local window = vim.api.nvim_get_current_win()
vim.api.nvim_win_set_buf(window, transcript)
layout.configure_transcript_window(window)
vim.wo[window].winbar = "Read - Codex - Main"
vim.cmd("belowright 3new")
vim.api.nvim_win_set_height(window, 8)
local height = vim.api.nvim_win_get_height(window) - 1
local width = vim.api.nvim_win_get_width(window)
local lines = { string.rep("x", width + 5) }
for row = 2, height do lines[row] = "row " .. row end
vim.api.nvim_buf_set_lines(transcript, 0, -1, false, lines)
local maximum = layout.maximum_content_topline(transcript, window)
local visible = vim.api.nvim_win_text_height(window, { start_row = maximum - 1, end_row = #lines - 1 })
assert(visible.all <= height, "scroll clamp clips the final answer below a wrapped first row")
assert(maximum == 2, "scroll clamp must choose the first complete row that fits the tail")
lines[1] = "short"
vim.api.nvim_buf_set_lines(transcript, 0, -1, false, lines)
assert(layout.maximum_content_topline(transcript, window) == 1, "exact-height transcript should not overscroll")
vim.api.nvim_buf_set_lines(transcript, 0, -1, false, { "short transcript" })
assert(layout.maximum_content_topline(transcript, window) == 1, "short transcript should remain at the top")

local tail_follow = require("forge.views.harness.tail_follow")
local clock_ms = 0
local follow = tail_follow.new(transcript, function() return clock_ms end)
local function set_bottom(target_window)
  vim.api.nvim_win_call(target_window, function() vim.cmd("normal! Gzb") end)
end
local function append_row(value)
  vim.api.nvim_buf_set_lines(transcript, -1, -1, false, { value })
end
local function move_view_without_cursor(target_window, offset)
  vim.api.nvim_win_call(target_window, function()
    local view = vim.fn.winsaveview()
    view.topline = math.min(vim.api.nvim_buf_line_count(transcript), view.topline + offset)
    vim.fn.winrestview(view)
  end)
end
vim.api.nvim_buf_set_lines(transcript, 0, -1, false, vim.fn["repeat"]({ "tail row" }, 40))
set_bottom(window)
follow.reset(window)
follow.observe(window)
follow.capture(window)
append_row("appended at tail")
follow.publish(window)
assert(vim.api.nvim_win_get_cursor(window)[1] == vim.api.nvim_buf_line_count(transcript), "bottom reader must follow an append")

vim.api.nvim_win_set_cursor(window, { 5, 0 })
vim.api.nvim_win_call(window, function()
  local view = vim.fn.winsaveview()
  view.topline = 1
  vim.fn.winrestview(view)
end)
follow.observe(window)
clock_ms = 100
follow.observe(window)
follow.capture(window)
append_row("append while reading history")
follow.publish(window)
assert(vim.api.nvim_win_get_cursor(window)[1] == 5, "history reader must remain at its cursor after append")

local history_cursor = vim.api.nvim_win_get_cursor(window)
local history_view = vim.api.nvim_win_call(window, vim.fn.winsaveview)
move_view_without_cursor(window, 1)
follow.observe(window)
local scrolled_view = vim.api.nvim_win_call(window, vim.fn.winsaveview)
assert(scrolled_view.topline ~= history_view.topline, "viewport-only navigation must change the observed view")
assert(vim.deep_equal(history_cursor, vim.api.nvim_win_get_cursor(window)), "viewport-only navigation must preserve the cursor")
follow.capture(window)
append_row("append after viewport-only scroll")
follow.publish(window)
assert(vim.api.nvim_win_get_cursor(window)[1] == 5, "viewport-only scrolling away from bottom must preserve the cursor")
assert(vim.api.nvim_win_call(window, vim.fn.winsaveview).topline == scrolled_view.topline, "viewport-only scrolling away from bottom must preserve the viewport across publication")

clock_ms = 200
vim.api.nvim_win_set_cursor(window, { 2, 0 })
vim.api.nvim_win_call(window, function() vim.cmd("normal! zt") end)
follow.observe(window)
clock_ms = 300
set_bottom(window)
follow.observe(window)
follow.capture(window)
local row_before_grace_append = vim.api.nvim_buf_line_count(transcript)
append_row("append inside grace period")
follow.publish(window)
assert(vim.api.nvim_win_get_cursor(window)[1] == row_before_grace_append, "returning to the tail must not follow before the 500ms grace expires")

clock_ms = 400
set_bottom(window)
follow.observe(window)
local row_at_return_to_bottom = vim.api.nvim_win_get_cursor(window)[1]
clock_ms = 899
follow.capture(window)
append_row("append one millisecond before grace expires")
follow.publish(window)
assert(vim.api.nvim_win_get_cursor(window)[1] == row_at_return_to_bottom, "tail following must remain paused during the 500ms grace after returning to bottom")
-- Restore the bottom baseline programmatically without renewing the navigation deadline.
set_bottom(window)
follow.reset(window)
-- The deadline is measured from the return-to-bottom observation at 400ms.
clock_ms = 900
follow.capture(window)
append_row("append after grace period")
follow.publish(window)
assert(vim.api.nvim_win_get_cursor(window)[1] == vim.api.nvim_buf_line_count(transcript), "tail following must resume after returning to bottom and waiting 500ms")

follow.suspend(window)
local expansion_cursor = vim.api.nvim_win_get_cursor(window)
local expansion_view = vim.api.nvim_win_call(window, vim.fn.winsaveview)
clock_ms = 2000
follow.capture(window)
append_row("delayed checkpoint expansion")
follow.publish(window)
assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), expansion_cursor),
  "delayed section expansion resumed following after the navigation grace period")
assert(vim.api.nvim_win_call(window, vim.fn.winsaveview).topline == expansion_view.topline,
  "section expansion changed the reading viewport")
clock_ms = 3000
follow.capture(window)
append_row("subsequent checkpoint publication")
follow.publish(window)
assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), expansion_cursor),
  "later publication resumed following without navigation")
set_bottom(window)
follow.observe(window)
clock_ms = 3500
follow.capture(window)
append_row("append after navigation resumes following")
follow.publish(window)
assert(vim.api.nvim_win_get_cursor(window)[1] == vim.api.nvim_buf_line_count(transcript),
  "navigation back to bottom did not resume following")
follow.suspend(window)
follow.follow(window)
follow.capture(window)
append_row("append after explicit following")
follow.publish(window)
assert(vim.api.nvim_win_get_cursor(window)[1] == vim.api.nvim_buf_line_count(transcript),
  "explicit following did not clear section browsing")

local second_window = vim.api.nvim_open_win(transcript, false, { split = "right" })
vim.api.nvim_win_set_cursor(window, { 1, 0 })
follow.observe(window)
set_bottom(second_window)
follow.observe(second_window)
follow.capture(window)
follow.capture(second_window)
append_row("per-window publication")
follow.publish(window)
follow.publish(second_window)
assert(vim.api.nvim_win_get_cursor(window)[1] == 1, "a tail-following sibling must not move a history window")
assert(vim.api.nvim_win_get_cursor(second_window)[1] == vim.api.nvim_buf_line_count(transcript), "each window must retain independent tail eligibility")

local wrapped_lines = { "short" }
for row = 2, vim.api.nvim_win_get_height(window) - 1 do wrapped_lines[row] = "row " .. row end
wrapped_lines[#wrapped_lines + 1] = string.rep("wrapped final row ", vim.api.nvim_win_get_width(window))
vim.api.nvim_buf_set_lines(transcript, 0, -1, false, wrapped_lines)
vim.wo[window].wrap = true
vim.api.nvim_win_set_cursor(window, { #wrapped_lines, 0 })
vim.api.nvim_win_call(window, function()
  local view = vim.fn.winsaveview()
  view.topline = #wrapped_lines
  vim.fn.winrestview(view)
end)
follow.reset(window)
follow.capture(window)
append_row("append below partially visible wrapped final row")
follow.publish(window)
assert(vim.api.nvim_win_get_cursor(window)[1] == #wrapped_lines, "a partially visible wrapped final row must not count as fully at bottom")

vim.api.nvim_win_close(second_window, true)
follow.reset(window)
print("harness scroll boundary passed")
vim.cmd("qa!")
