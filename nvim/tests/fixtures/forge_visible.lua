local replica = require("forge.buffer")
local session = replica.open("visible-fixture")
vim.api.nvim_set_hl(0, "ForgeFixtureSyntax", { fg = "#ff5555", bold = true })
vim.api.nvim_set_hl(0, "ForgeFixtureGutter", { fg = "#55ffff" })
local snapshot = { document = session.document, revision = 0, block = {
  { id = "heading", text = { "Native source document" }, metadata = { target = {}, decoration = {}, editable_region = {}, fold = {
    { id = "source", start = { row = 0, column = 0 }, ["end"] = { block = "source", position = { row = 4, column = 0 } }, closed = false },
  } } },
  { id = "source", text = { "local value = 42", "界 é 👨‍👩‍👧‍👦", "unchanged source bytes", "tail" }, metadata = {
    target = {}, decoration = {}, editable_region = {},
    gutter = { { position = { row = 0, column = 0 }, priority = 100, chunk = { { text = "1 │ ", capture = "ForgeFixtureGutter" } } } },
    visible_decoration = { { range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = 5 } }, capture = "ForgeFixtureSyntax", priority = 150 } },
  } },
} }
assert(replica.apply_snapshot(session, snapshot).kind == "Applied")
vim.api.nvim_set_current_buf(session.buffer)
require("forge.input").open(session, 0)
vim.wo.number = false
vim.wo.relativenumber = false
vim.wo.signcolumn = "no"
vim.wo.foldcolumn = "0"
vim.wo.statuscolumn = ""
vim.o.laststatus = 0
vim.cmd.redraw()
vim.defer_fn(function()
  assert(vim.fn.screenstring(2, 5) == "l")
  assert(vim.fn.screenattr(2, 5) ~= vim.fn.screenattr(2, 11), "visible syntax decoration is missing")
  assert(vim.fn.screenstring(3, 1) == "界" and vim.fn.screenstring(3, 4) == "é")
  assert(vim.fn.screenstring(3, 6) == "👨‍👩‍👧‍👦" and vim.fn.screenstring(4, 1) == "u")
  assert(#session.marks.source == 1, "visible syntax became a persistent mark")
  local row = {}
  for line = 1, 5 do
    local cell = {}
    for column = 1, 32 do cell[column] = vim.fn.screenstring(line, column) end
    row[line] = cell
  end
  local source = vim.api.nvim_get_runtime_file("lua/forge/buffer.lua", false)[1]
  local root = vim.fs.dirname(vim.fs.dirname(vim.fs.dirname(source)))
  vim.fn.writefile({ vim.json.encode({ columns = vim.o.columns, lines = vim.o.lines, row = row,
    syntax = vim.fn.screenattr(2, 5), plain = vim.fn.screenattr(2, 11) }) },
    vim.fs.joinpath(root, "rust", "forge", "target", "native-cells-" .. vim.o.columns .. ".json"))
end, 300)
vim.defer_fn(function() vim.cmd("qa!") end, 110000)
