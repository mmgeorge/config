vim.loader.enable(false)
local original_provider = vim.api.nvim_set_decoration_provider
local provider
vim.api.nvim_set_decoration_provider = function(_, callbacks) provider = callbacks end
local buffer = require("forge.buffer")
vim.api.nvim_set_decoration_provider = original_provider
local session = buffer.open("source-overlay")
local source = { "- First", "+ 日本語", "1. Ordered" }
local metadata = { target = {}, decoration = {}, editable_region = {}, source_overlay = {} }
for row = 0, 1 do
  metadata.source_overlay[#metadata.source_overlay + 1] = {
    range = { start = { row = row, column = 0 }, ["end"] = { row = row, column = 1 } },
    text = "●", capture = "String", priority = 120,
  }
end
metadata.source_highlight = { {
  range = { start = { row = 1, column = 2 }, ["end"] = { row = 1, column = #source[2] } },
  capture = "RenderMarkdownCodeInline", priority = 4096,
}, {
  range = { start = { row = 2, column = 0 }, ["end"] = { row = 3, column = 0 } },
  capture = "RenderMarkdownCode", priority = 4096,
} }
metadata.source_overlay[3] = {
  range = { start = { row = 2, column = #source[3] }, ["end"] = { row = 2, column = #source[3] } },
  text = " ", capture = "Title", priority = 4096,
}
metadata.source_overlay[4] = {
  range = { start = { row = 2, column = 0 }, ["end"] = { row = 2, column = 1 } },
  text = "json", capture = "RenderMarkdownCodeInfo:json", priority = 4097,
}
local snapshot = { document = session.document, revision = 0,
  block = { { id = "source", text = source, metadata = metadata } } }
assert(buffer.apply_snapshot(session, snapshot).kind == "Applied")
vim.api.nvim_set_current_buf(session.buffer)
vim.wo.conceallevel, vim.wo.concealcursor = 3, ""
local original_extmark = vim.api.nvim_buf_set_extmark
local emitted = {}
local succeeded, failure = xpcall(function()
  vim.api.nvim_buf_set_extmark = function(_, _, row, column, options)
    emitted[#emitted + 1] = { row = row, column = column, options = options }
  end
  local window = vim.api.nvim_get_current_win()
  provider.on_line(nil, window, session.buffer, 0)
  assert(#emitted == 0, "cursor row hid literal source")
  provider.on_line(nil, window, session.buffer, 1)
  assert(#emitted == 2 and emitted[2].row == 1 and emitted[2].column == 0)
  assert(emitted[1].options.hl_group == "RenderMarkdownCodeInline" and emitted[1].options.priority == 4096)
  assert(emitted[2].options.virt_text[1][1] == "●" and emitted[2].options.ephemeral)
  vim.wo.concealcursor = "n"
  provider.on_line(nil, window, session.buffer, 0)
  assert(#emitted == 3, "concealcursor policy was ignored")
  vim.wo.conceallevel = 0
  provider.on_line(nil, window, session.buffer, 1)
  assert(#emitted == 3, "disabled source formatting emitted decorations")
  vim.wo.conceallevel, vim.wo.concealcursor = 3, ""
  vim.cmd("normal! Vj")
  provider.on_line(nil, window, session.buffer, 0)
  provider.on_line(nil, window, session.buffer, 1)
  assert(#emitted == 3, "visual selection hid literal source")
  vim.cmd("normal! \27")
  provider.on_line(nil, window, session.buffer, 2)
  assert(#emitted == 6 and emitted[4].options.hl_eol == true, "code background did not reach the window edge")
  assert(emitted[5].column == #source[3], "end-of-line source padding was lost")
  assert(emitted[6].options.virt_text[1][1] == " " and emitted[6].options.virt_text[2][1] == "json",
    "code information did not resolve into icon and language chunks")
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(session.buffer, 0, -1, false), source))
end, debug.traceback)
vim.api.nvim_buf_set_extmark = original_extmark
assert(succeeded, failure)
local invalid = vim.deepcopy(snapshot)
invalid.revision = 1
invalid.block[1].metadata.source_overlay[2].range.start.column = 3
invalid.block[1].metadata.source_overlay[2].range["end"].column = 4
assert(buffer.apply_snapshot(session, invalid).kind == "Desynchronized", "overlay accepted split UTF-8")
assert(vim.deep_equal(vim.api.nvim_buf_get_lines(session.buffer, 0, -1, false), source))
buffer.close(session)
print("source_overlay: passed")
vim.cmd("qa!")
