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
print("harness scroll boundary passed")
vim.cmd("qa!")
