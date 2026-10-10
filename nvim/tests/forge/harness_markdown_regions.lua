vim.loader.enable(false)

local markdown = require("forge.render.harness.markdown")
local entry = {
  { row_count = 2, metadata = {} },
  { row_count = 3, metadata = { markdown = true } },
  { row_count = 1, metadata = {} },
  { row_count = 2, metadata = { markdown = true, layout = { indent = 6, source_indent = 4 } } },
}
local session = {
  buffer = vim.api.nvim_create_buf(false, true),
  sequence = require("forge.block_sequence").new(),
}
for index, block in ipairs(entry) do
  session.sequence:splice(index - 1, 0, { { id = "block:" .. index, entry = block } })
end
vim.api.nvim_set_current_buf(session.buffer)
vim.api.nvim_buf_set_lines(session.buffer, 0, -1, false, { "header", "header", "body", "body", "body", "tool", "nested", "nested" })

assert(vim.deep_equal(markdown.viewport(session, vim.api.nvim_get_current_win()), {
  { first0 = 2, after0 = 5, indent = 2, source_indent = 0, id = "block:2" },
  { first0 = 6, after0 = 8, indent = 6, source_indent = 4, id = "block:4" },
}), "Markdown rendering must exclude surrounding timeline rows")

io.write("harness_markdown_regions OK\n")
vim.cmd("qa!")
