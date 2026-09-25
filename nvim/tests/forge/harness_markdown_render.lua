vim.loader.enable(false)

local markdown = require("forge.render.harness.markdown")
local buffer = vim.api.nvim_create_buf(false, true)
local window = vim.api.nvim_get_current_win()
vim.api.nvim_win_set_buf(window, buffer)
vim.bo[buffer].filetype = "ForgeHarness"
vim.b[buffer].forge_native_document = true
vim.api.nvim_buf_set_lines(buffer, 0, -1, false, {
  "▸ Prompt", "```rust", "let unfinished = true;", "Tool result", "## Second response", "",
  "\\[", "\\int_{\\Omega} L_i(x,\\omega_i)", "\\]", "Where $L_o$ meets $L_i$.", "- First item",
})

markdown.render(buffer, window, {
  { first0 = 1, after0 = 3 },
  { first0 = 4, after0 = 11 },
})

local parser = vim.treesitter.get_parser(buffer, "markdown")
local trees = parser:parse(true)
assert(#trees == 2, "separate responses require separate Markdown parse trees")
assert(trees[2]:root():sexpr():find("atx_heading"), "an unfinished fence must not hide the next response heading")
local ui = require("render-markdown.core.ui")
vim.cmd("redraw")
vim.wait(1000, function()
  return #vim.api.nvim_buf_get_extmarks(buffer, ui.ns, 0, -1, {}) > 0
end, 10)
local marks = vim.api.nvim_buf_get_extmarks(buffer, ui.ns, 0, -1, {})
assert(#marks > 0, "render-markdown.nvim must decorate response rows")
for _, mark in ipairs(marks) do
  local row = mark[2]
  assert(row >= 1 and row < 3 or row >= 4 and row < 11, "Markdown decoration escaped a response region")
end

local rendered_line_list = {}
for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(buffer, ui.ns, 0, -1, { details = true })) do
  local details = mark[4]
  for _, chunk in ipairs(details.virt_text or {}) do rendered_line_list[#rendered_line_list + 1] = chunk[1] end
  for _, virtual_line in ipairs(details.virt_lines or {}) do
    for _, chunk in ipairs(virtual_line) do rendered_line_list[#rendered_line_list + 1] = chunk[1] end
  end
end
local rendered = table.concat(rendered_line_list, "\n")
assert(rendered:find("⌠", 1, true), "Harness display math was not converted: " .. rendered)

io.write("harness_markdown_render OK\n")
vim.cmd("qa!")
