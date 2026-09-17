vim.opt.runtimepath:append("nvim")
local selector = require("treesitter_smart_select")
local original_parser = vim.treesitter.get_parser
local original_query = vim.treesitter.query.get
local notices = {}
vim.notify = function(message) notices[#notices + 1] = message end
vim.treesitter.get_parser = function() return nil end
vim.treesitter.query.get = function() error("queried a language without a parser") end
vim.bo.filetype = "forge"
local lines = { "Title", "", "@mgeorge-esri @other", "next line", "", "last paragraph" }
vim.api.nvim_buf_set_lines(0, 0, -1, false, lines)
local function selected()
  local first, last = vim.fn.getpos("v"), vim.api.nvim_win_get_cursor(0)
  local start_row, start_col, end_row, end_col = first[2] - 1, first[3] - 1, last[1] - 1, last[2]
  if start_row > end_row or start_row == end_row and start_col > end_col then
    start_row, start_col, end_row, end_col = end_row, end_col, start_row, start_col
  end
  return { start_row, start_col, end_row, end_col }
end
local function expect(range)
  assert(vim.deep_equal(selected(), range), vim.inspect(selected()) .. " expected " .. vim.inspect(range))
end
vim.api.nvim_win_set_cursor(0, { 3, 3 })
vim.cmd("normal! v")
local ranges = { { 2, 1, 2, 7 }, { 2, 0, 2, 12 }, { 2, 0, 2, 19 }, { 2, 0, 3, 8 }, { 0, 0, 5, 13 } }
for _, range in ipairs(ranges) do selector.select_parent() expect(range) end
selector.select_parent()
expect(ranges[#ranges])
selector.undo_select_parent()
expect(ranges[4])
selector.select_parent()
expect(ranges[5])
for index = 4, 1, -1 do selector.undo_select_parent() expect(ranges[index]) end
selector.undo_select_parent()
expect({ 2, 3, 2, 3 })
assert(#notices == 0, vim.inspect(notices))
assert(vim.deep_equal(vim.api.nvim_buf_get_lines(0, 0, -1, false), lines))
vim.cmd("normal! \27")
vim.bo.filetype = ""
vim.api.nvim_buf_set_lines(0, 0, -1, false, { "@élise-esri" })
vim.api.nvim_win_set_cursor(0, { 1, 3 })
vim.cmd("normal! v")
selector.select_parent()
expect({ 0, 1, 0, 6 })
selector.select_parent()
expect({ 0, 0, 0, 11 })
selector.undo_select_parent()
expect({ 0, 1, 0, 6 })
vim.treesitter.get_parser = original_parser
vim.treesitter.query.get = original_query
vim.cmd("normal! \27")
vim.api.nvim_buf_set_lines(0, 0, -1, false, { "local value = 42" })
vim.bo.filetype = "lua"
assert(vim.treesitter.get_parser(0, "lua", { error = false }), "Lua syntax fixture requires the bundled parser")
vim.api.nvim_win_set_cursor(0, { 1, 7 })
vim.cmd("normal! v")
selector.select_parent()
assert(vim.wait(1000, function() return vim.deep_equal(selected(), { 0, 6, 0, 10 }) end),
  "syntax selection did not survive asynchronous parsing: " .. vim.inspect(selected()))
local parse_callback
vim.treesitter.get_parser = function()
  return { parse = function(_, _, callback) parse_callback = callback end }
end
selector.select_parent()
assert(parse_callback)
vim.api.nvim_win_set_cursor(0, { 1, 2 })
local moved = selected()
parse_callback(nil, {})
vim.wait(20, function() return false end)
assert(vim.deep_equal(selected(), moved), "late parsing replaced a newer selection")
vim.treesitter.get_parser = original_parser
print("smart_select: parserless expansion, shrink, and reviewer token passed")
vim.cmd("qa!")
