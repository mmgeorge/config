vim.loader.enable(false)

local markdown = require("forge.render.harness.markdown")
local entry = {
  { row_count = 2, metadata = {} },
  { row_count = 3, metadata = { markdown = true } },
  { row_count = 1, metadata = {} },
  { row_count = 2, metadata = { markdown = true, layout = { indent = 6, source_indent = 4 } } },
}
local session = {
  sequence = {
    count = function() return #entry end,
    at = function(_, index) return { entry = entry[index + 1] } end,
  },
}

assert(vim.deep_equal(markdown.ranges(session), {
  { first0 = 2, after0 = 5, indent = 2, source_indent = 0 },
  { first0 = 6, after0 = 8, indent = 6, source_indent = 4 },
}), "Markdown rendering must exclude surrounding timeline rows")

io.write("harness_markdown_regions OK\n")
vim.cmd("qa!")
