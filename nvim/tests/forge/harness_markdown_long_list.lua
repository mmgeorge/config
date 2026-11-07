-- A long Markdown list must number its items in time linear in the list length.
-- render-markdown counts each item's position by walking every preceding sibling, which made one
-- transcript render quadratic in the list length and blocked the editor while it re-rendered.
vim.loader.enable(false)
local data = vim.fn.stdpath("data")
vim.opt.runtimepath:append(data .. "/site")
vim.opt.runtimepath:append(data .. "/lazy/render-markdown.nvim")
vim.opt.runtimepath:append(data .. "/lazy/nvim-treesitter")
require("render-markdown").setup(require("plugins.markdown")[1].opts())

local item_count = 3000
local markdown = require("forge.render.harness.markdown")
local buffer = vim.api.nvim_create_buf(false, true)
local window = vim.api.nvim_get_current_win()
vim.api.nvim_win_set_buf(window, buffer)
vim.bo[buffer].filetype = "ForgeHarness"
vim.b[buffer].forge_native_document = true
local line_list = { "▸ Prompt" }
for item = 1, item_count do line_list[#line_list + 1] = ("  - item %d"):format(item) end
vim.api.nvim_buf_set_lines(buffer, 0, -1, false, line_list)
-- Show the end of the list, where every visible item has the longest preceding chain.
vim.api.nvim_win_set_cursor(window, { #line_list, 0 })
vim.cmd("normal! zb")

local parser = vim.treesitter.get_parser(buffer, "markdown")
parser:parse(true)
local node_methods = getmetatable(parser:trees()[1]:root()).__index
local prev_sibling = node_methods.prev_sibling
local sibling_step_count = 0
node_methods.prev_sibling = function(node)
  sibling_step_count = sibling_step_count + 1
  return prev_sibling(node)
end

local ui = require("render-markdown.core.ui")
markdown.render(buffer, window, { { first0 = 1, after0 = #line_list, source_indent = 2 } })
assert(vim.wait(5000, function()
  return #vim.api.nvim_buf_get_extmarks(buffer, ui.ns, 0, -1, {}) > 0
end, 10), "render-markdown never decorated the list")
node_methods.prev_sibling = prev_sibling

-- A linear walk visits each preceding item about once; the quadratic walk repeats the whole chain
-- for every visible item (tens of thousands of steps for this list).
assert(sibling_step_count <= 2 * item_count,
  ("list numbering walked %d siblings for %d items"):format(sibling_step_count, item_count))
local numbered = false
for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(buffer, ui.ns, 0, -1, { details = true })) do
  for _, chunk in ipairs(mark[4].virt_text or {}) do
    if chunk[1]:find("●", 1, true) or chunk[1]:find("○", 1, true) or chunk[1]:find("◆", 1, true)
      or chunk[1]:find("◇", 1, true) then numbered = true end
  end
end
assert(numbered, "list items lost their bullet decoration")

io.write("harness_markdown_long_list OK\n")
vim.cmd("qa!")
