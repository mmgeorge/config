vim.loader.enable(false)
local data = vim.fn.stdpath("data")
vim.opt.runtimepath:append(data .. "/site")
vim.opt.runtimepath:append(data .. "/lazy/render-markdown.nvim")
vim.opt.runtimepath:append(data .. "/lazy/nvim-treesitter")
require("render-markdown").setup(require("plugins.markdown")[1].opts())
local client = require("forge.client")
local requests = {}
client.host_is_accepting = function() return true end
client.request = function(request_specification, callback)
  local params = request_specification.params or {}

  callback = dofile("nvim/tests/forge/support/presentation_status.lua")(params, callback)
  requests[#requests + 1] = { params = params, callback = callback }
end
package.loaded["forge.views.harness.terminals"] = { watch = function() return { close = function() end } end }
local markdown = require("forge.render.harness.markdown")
local render, rendered = markdown.render, 0
markdown.render = function(...)
  rendered = rendered + 1
  return render(...)
end
-- Count sequence visits only inside Markdown viewport selection; other refresh observers, such as
-- the response reveal, walk the sequence for their own purposes.
local viewport, markdown_visits = markdown.viewport, 0
markdown.viewport = function(document, ...)
  local before = document.sequence.visits
  local range_list = viewport(document, ...)
  markdown_visits = markdown_visits + document.sequence.visits - before
  return range_list
end
local transcript = vim.api.nvim_create_buf(false, true)
local composer = vim.api.nvim_create_buf(false, true)
local window = vim.api.nvim_get_current_win()
vim.api.nvim_win_set_buf(window, transcript)
local owner = require("forge.views.harness.presentation").open({ session_id = "markdown-viewport",
  transcript_buffer = transcript, composer_buffer = composer, transcript_window = window,
  is_alive = function() return true end, notice = function(message) error(message) end,
}, function(value, failure) assert(value, failure) end)
local block = {}
for index = 1, 3000 do
  local text = {}
  for row = 1, 10 do text[row] = "settled row " .. index .. ":" .. row end
  block[index] = { id = "history:" .. index, text = text,
    metadata = { target = {}, decoration = {}, editable_region = {}, fold = {}, markdown = index == 1 or index == 3000 } }
end
block[1].text[1], block[3000].text[1] = "# First message", "# Last message"
requests[1].callback({ transcript = { document = owner.document, revision = 0, block = block } })
assert(vim.wait(10000, function() return owner.ready end, 1), "snapshot did not settle")
vim.cmd("redraw")
assert(owner.markdown_ranges[1].id == "history:1" and #owner.markdown_ranges == 1)
for revision = 1, 100 do
  markdown_visits = 0
  local count = #requests
  owner.sync()
  assert(vim.wait(1000, function() return #requests > count end, 1), "refresh did not leave the frame queue")
  requests[#requests].callback({ patch = { {
    document = owner.document, base = revision - 1, next = revision,
    base_rows = 30000, next_rows = 30000, base_blocks = 3000, next_blocks = 3000,
    block_edit = {}, removed_block = {}, metadata_edit = {},
    text_edit = { { start_row = 29999, removed_rows = 1, text = { "output " .. revision } } },
  } } })
  assert(owner.transcript.revision == revision)
  assert(markdown_visits < 300, "Markdown traversed off-screen history")
end
-- Every buffer change re-renders the visible Markdown, because an edit outside a message can
-- extend Tree-sitter's included region at its boundary; only the newly visible range is checked.
local rendered_before_scroll = rendered
vim.api.nvim_win_set_cursor(window, { 29991, 0 })
vim.cmd("normal! zt")
vim.api.nvim_exec_autocmds("CursorMoved", { buffer = transcript })
assert(vim.wait(1000, function() return owner.markdown_ranges[1].id == "history:3000" end, 1))
assert(rendered > rendered_before_scroll, "scrolling did not render the newly visible message")
vim.cmd("split")
local second = vim.api.nvim_get_current_win()
owner.views[second] = require("forge.input").open(owner.transcript, second)
vim.api.nvim_win_set_cursor(second, { 1, 0 })
vim.cmd("normal! zt")
owner.resize()
assert(#owner.markdown_ranges == 2, "split windows lost one visible Markdown region")
assert(owner.markdown_ranges[1].id == "history:1" and owner.markdown_ranges[2].id == "history:3000")
owner.close()
print("markdown_viewport OK: 30,000 rows, 100 retained-message updates, scrolling and splits")
vim.cmd("qa!")
