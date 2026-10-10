vim.loader.enable(false)
require("render-markdown").setup(require("plugins.markdown")[1].opts())
local client = require("forge.client")
local requests = {}
client.host_accepting = function() return true end
client.request_for = function(_, _, params, callback)
  requests[#requests + 1] = { params = params, callback = callback }
end
package.loaded["forge.views.harness.terminals"] = { watch = function() return { close = function() end } end }
local markdown = require("forge.render.harness.markdown")
local render, rendered = markdown.render, 0
markdown.render = function(...)
  rendered = rendered + 1
  return render(...)
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
local initial = rendered
for revision = 1, 100 do
  owner.transcript.sequence.visits = 0
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
  assert(owner.transcript.sequence.visits < 300, "Markdown traversed off-screen history")
  assert(rendered == initial, "tool output invalidated an unchanged visible message")
end
vim.api.nvim_win_set_cursor(window, { 29991, 0 })
vim.cmd("normal! zt")
vim.api.nvim_exec_autocmds("CursorMoved", { buffer = transcript })
assert(vim.wait(1000, function() return owner.markdown_ranges[1].id == "history:3000" end, 1))
assert(rendered > initial, "scrolling did not render the newly visible message")
vim.cmd("split")
local second = vim.api.nvim_get_current_win()
owner.views[second] = require("forge.input").open(owner.transcript, second)
vim.api.nvim_win_set_cursor(second, { 1, 0 })
vim.cmd("normal! zt")
owner.resize()
assert(#owner.markdown_ranges == 2, "split windows lost one visible Markdown region")
assert(owner.markdown_ranges[1].id == "history:1" and owner.markdown_ranges[2].id == "history:3000")
vim.wo[second].foldmethod = "manual"
vim.cmd("2,29990fold")
vim.api.nvim_win_set_cursor(second, { 1, 0 })
vim.cmd("normal! zt")
vim.cmd("redraw")
owner.transcript.sequence.visits = 0
local ranges = markdown.viewport(owner.transcript, second)
assert(owner.transcript.sequence.visits < 100, "Markdown scanned a closed historical fold")
assert(#ranges == 2 and ranges[2].id == "history:3000")
owner.close()
print("markdown_viewport OK: 30,000 rows, 100 retained-message updates, scrolling and splits")
vim.cmd("qa!")
