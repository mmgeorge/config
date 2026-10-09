vim.loader.enable(false)
vim.cmd("runtime plugin/render-markdown.lua")

local source = {
  { text = "Objective:", metadata = {} },
  { text = "Build a **reusable library** with `GamePlugin`.", metadata = { markdown = true } },
  { text = "Decisions:", metadata = {} },
  { text = "- **Use fixed coins.** Keep collection deterministic.", metadata = { markdown = true } },
  { text = "- **Separate presentation.** Support headless tests.", metadata = { markdown = true } },
  { text = "Changes:", metadata = {} },
  { text = "/// **Literal declaration documentation**", metadata = {} },
  { text = "pub struct GamePlugin;", metadata = {} },
}
local lines, records = {}, {}
for index, row in ipairs(source) do
  lines[index] = row.text
  records[index] = { source_index = index, row = index - 1 }
end
local buffer = vim.api.nvim_create_buf(false, true)
local window = vim.api.nvim_get_current_win()
vim.api.nvim_win_set_buf(window, buffer)
vim.bo[buffer].filetype = "ForgePlan"
vim.b[buffer].forge_native_document = true
vim.api.nvim_buf_set_lines(buffer, 0, -1, false, lines)
local markdown = require("forge.render.harness.markdown")
local ranges = require("forge.views.plan_review.markdown").ranges(source, { source_record_list = records })
markdown.render(buffer, window, ranges)
local parser = vim.treesitter.get_parser(buffer, "markdown")
local trees = parser:parse(true)
assert(#trees == 2, "Each section body must have its own Markdown tree")
assert(trees[2]:root():sexpr():find("list_item"), "Decisions must parse as a Markdown list")
for _, tree in ipairs(trees) do
  local first, _, after = tree:root():range()
  assert(first > 0 and after <= 5, "Markdown parsing included a generated heading or declaration")
end
local namespace = require("render-markdown.core.ui").ns
vim.cmd("redraw")
assert(vim.wait(1000, function()
  return #vim.api.nvim_buf_get_extmarks(buffer, namespace, 0, -1, {}) > 0
end, 10), "Section bodies did not receive rendered Markdown decorations")
for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(buffer, namespace, 0, -1, {})) do
  assert(mark[2] == 1 or mark[2] == 3 or mark[2] == 4,
    "Markdown decoration escaped a section body")
end
local strong, concealed = false, false
parser:for_each_tree(function(tree, language)
  if language:lang() ~= "markdown_inline" then return end
  local query = assert(vim.treesitter.query.get("markdown_inline", "highlights"))
  for capture, _, metadata in query:iter_captures(tree:root(), buffer) do
    strong = strong or query.captures[capture] == "markup.strong"
    concealed = concealed or metadata.conceal == ""
  end
end)
assert(strong and concealed, "Bold prose must receive emphasis and concealed Markdown delimiters")
assert(vim.deep_equal(lines, vim.api.nvim_buf_get_lines(buffer, 0, -1, false)),
  "Markdown rendering changed review source text")
assert(vim.bo[buffer].filetype == "ForgePlan", "Markdown rendering changed the composite buffer filetype")
io.write("plan_markdown_render OK\n")
vim.cmd("qa!")
