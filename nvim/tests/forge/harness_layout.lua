vim.loader.enable(false)
local layout = require("forge.views.harness.layout")
local config = require("forge.infra.config")
local transcript_buffer, transcript_window, composer_buffer, composer_window = layout.open("layout-test")
layout.attach_auto_height(composer_buffer, composer_window)
layout.resize_composer(composer_buffer, composer_window)
local prompt_height = vim.api.nvim_win_get_height(composer_window)

vim.cmd("belowright 6new")
local lower_window = vim.api.nvim_get_current_win()
local timeline_height = vim.api.nvim_win_get_height(transcript_window)
vim.api.nvim_win_close(lower_window, true)
assert(vim.api.nvim_win_get_height(composer_window) == prompt_height,
  "closing a lower pane expanded the Harness prompt")
assert(vim.api.nvim_win_get_height(transcript_window) > timeline_height,
  "closing a lower pane did not give the timeline more height")
assert(vim.wo[composer_window].winfixheight and not vim.wo[transcript_window].winfixheight,
  "Harness must fix the prompt height and leave the timeline flexible")

vim.api.nvim_set_current_win(composer_window)
vim.cmd("wincmd =")
assert(vim.api.nvim_win_get_height(composer_window) == prompt_height,
  "equalizing windows expanded the Harness prompt")
local original_lines = vim.o.lines
vim.o.lines = original_lines + 10
vim.cmd("redraw!")
assert(vim.api.nvim_win_get_height(composer_window) == prompt_height,
  "terminal growth expanded the Harness prompt")
vim.o.lines = original_lines
vim.cmd("redraw!")
assert(vim.api.nvim_win_get_height(composer_window) == prompt_height,
  "terminal shrink changed the Harness prompt height")

vim.api.nvim_buf_set_lines(composer_buffer, 0, -1, false, { "first", "second", "third", "fourth" })
vim.api.nvim_exec_autocmds("TextChanged", { buffer = composer_buffer })
assert(vim.api.nvim_win_get_height(composer_window) > prompt_height
  and vim.api.nvim_win_get_height(composer_window) <= config.options.harness.composer_max_height,
  "fixed prompt height prevented bounded content-driven growth")
vim.b[composer_buffer].forge_queue_rows = config.options.harness.composer_max_height
layout.resize_composer(composer_buffer, composer_window)
assert(vim.api.nvim_win_get_height(composer_window) == config.options.harness.composer_max_height,
  "queued prompts did not respect the configured maximum height")
vim.b[composer_buffer].forge_queue_rows = 0
vim.api.nvim_buf_set_lines(composer_buffer, 0, -1, false, { "" })
vim.api.nvim_exec_autocmds("TextChanged", { buffer = composer_buffer })
assert(vim.api.nvim_win_get_height(composer_window) == prompt_height,
  "clearing the draft did not restore compact prompt height")

vim.api.nvim_set_current_win(composer_window)
local second_transcript, second_timeline, second_composer, second_prompt = layout.open("layout-test-second")
assert(not vim.wo[second_timeline].winfixheight and vim.wo[second_prompt].winfixheight,
  "opening Harness from its prompt inherited fixed height into the timeline")
assert(vim.api.nvim_buf_is_valid(transcript_buffer) and vim.api.nvim_buf_is_valid(second_transcript)
  and vim.api.nvim_buf_is_valid(second_composer))
print("harness_layout: passed")
