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
local draft_height = vim.api.nvim_win_get_height(composer_window)
layout.resize_composer(composer_buffer, composer_window)
assert(vim.api.nvim_win_get_height(composer_window) == draft_height,
  "legacy queue rows changed the draft-only composer height")
vim.b[composer_buffer].forge_queue_rows = nil
vim.api.nvim_buf_set_lines(composer_buffer, 0, -1, false, { "" })
vim.api.nvim_exec_autocmds("TextChanged", { buffer = composer_buffer })
assert(vim.api.nvim_win_get_height(composer_window) == prompt_height,
  "clearing the draft did not restore compact prompt height")

assert(vim.wo[composer_window].smoothscroll, "composer did not enable visual-row scrolling")
local function assert_wrapped_height()
  local rows = vim.api.nvim_win_text_height(composer_window, { start_row = 0 }).all
  local winbar = vim.wo[composer_window].winbar ~= "" and 1 or 0
  local expected = math.max(config.options.harness.composer_min_height,
    math.min(config.options.harness.composer_max_height, rows + 1 + winbar))
  assert(vim.api.nvim_win_get_height(composer_window) == expected,
    ("wrapped draft used height %d instead of %d for %d visual rows"):format(
      vim.api.nvim_win_get_height(composer_window), expected, rows))
end
for _, text in ipairs({ string.rep("wrapped prompt ", 25), string.rep("界🙂\t", 30) }) do
  vim.api.nvim_buf_set_lines(composer_buffer, 0, -1, false, { text })
  vim.api.nvim_exec_autocmds("TextChangedI", { buffer = composer_buffer })
  assert_wrapped_height()
  assert(vim.api.nvim_win_get_height(composer_window) > prompt_height,
    "a single wrapped line retained the minimum height")
  assert(vim.api.nvim_buf_line_count(composer_buffer) == 1
    and vim.api.nvim_buf_get_lines(composer_buffer, 0, -1, false)[1] == text,
    "sizing inserted physical newlines into the draft")
end
vim.api.nvim_buf_set_lines(composer_buffer, 0, -1, false, { string.rep("width-sensitive prompt ", 16) })
vim.cmd("vsplit")
local width_peer = vim.api.nvim_get_current_win()
vim.api.nvim_set_current_win(composer_window)
vim.api.nvim_win_set_width(composer_window, 30)
vim.api.nvim_exec_autocmds("VimResized", {})
assert_wrapped_height()
local narrow_height = vim.api.nvim_win_get_height(composer_window)
vim.api.nvim_win_set_width(composer_window, 60)
vim.api.nvim_exec_autocmds("WinResized", {})
assert_wrapped_height()
assert(vim.api.nvim_win_get_height(composer_window) < narrow_height,
  "widening the composer did not shrink the wrapped draft")
vim.api.nvim_win_close(width_peer, true)
vim.api.nvim_exec_autocmds("VimResized", {})
vim.wo[composer_window].winbar = "Draft"
layout.resize_composer(composer_buffer, composer_window)
assert_wrapped_height()
vim.wo[composer_window].winbar = ""

local long_line = string.rep("overflowing prompt ", 200)
vim.api.nvim_buf_set_lines(composer_buffer, 0, -1, false, { long_line })
layout.resize_composer(composer_buffer, composer_window)
assert(vim.api.nvim_win_get_height(composer_window) == config.options.harness.composer_max_height,
  "overflowing wrapped draft did not stop at the maximum pane height")
vim.api.nvim_win_call(composer_window, function()
  vim.fn.winrestview({ topline = 1, skipcol = vim.api.nvim_win_get_width(composer_window) * 2 })
end)
local scrolled_view = vim.api.nvim_win_call(composer_window, vim.fn.winsaveview)
assert(scrolled_view.skipcol > 0, "fixture did not scroll within the wrapped line")
layout.resize_composer(composer_buffer, composer_window)
assert(vim.api.nvim_win_call(composer_window, vim.fn.winsaveview).skipcol == scrolled_view.skipcol,
  "resizing reset scrolling within a wrapped physical line")
vim.api.nvim_buf_set_lines(composer_buffer, 0, -1, false, { "short again" })
layout.resize_composer(composer_buffer, composer_window)
local compact_view = vim.api.nvim_win_call(composer_window, vim.fn.winsaveview)
assert(vim.api.nvim_win_get_height(composer_window) == prompt_height
  and compact_view.topline == 1 and compact_view.skipcol == 0,
  "shortening a wrapped draft did not restore the compact viewport")

vim.api.nvim_set_current_win(composer_window)
local second_transcript, second_timeline, second_composer, second_prompt = layout.open("layout-test-second")
assert(not vim.wo[second_timeline].winfixheight and vim.wo[second_prompt].winfixheight,
  "opening Harness from its prompt inherited fixed height into the timeline")
assert(vim.api.nvim_buf_is_valid(transcript_buffer) and vim.api.nvim_buf_is_valid(second_transcript)
  and vim.api.nvim_buf_is_valid(second_composer))
print("harness_layout: passed")
