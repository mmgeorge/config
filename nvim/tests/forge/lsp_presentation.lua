local source = require("forge.lsp.source")
local startup = require("forge.lsp.startup")
local original_client, original_wait = source.select_client, startup.wait
source.select_client = function() return {} end
startup.wait = function(_, _, receive) receive() return function() end end
local original_semantic = package.loaded["forge.lsp.semantic"]
local hover_count = 0
local path = vim.fn.tempname() .. ".rs"
vim.fn.writefile({ "#[derive(Default)]", "pub struct Sample;", "", "fn caller() {}" }, path)
package.loaded["forge.lsp.semantic"] = { request = function(_, arguments, _, receive)
  if arguments.operation == "hover" then
    hover_count = hover_count + 1
    receive({ rows = {}, hover = "Implements notable traits: Future, Iterator, Read, Write" })
  else receive({ rows = { { path = path, line = 1, column = 1 } } }) end
  return function() end
end }
package.loaded["forge.lsp.query"] = nil
local query = require("forge.lsp.query")
local declarations = {
  { name = "Sample", kind = "struct_item", line = 2, column = 12, signature = "pub struct Sample",
    parent = vim.NIL, depth = 1, start_line = 1, end_line = 2,
    range_start_line = 1, range_start_column = 1, range_end_line = 2, range_end_column = 19 },
  { name = "caller", kind = "function_item", line = 4, column = 4, signature = "fn caller()",
    parent = vim.NIL, depth = 1, start_line = 4, end_line = 4,
    range_start_line = 4, range_start_column = 1, range_end_line = 4, range_end_column = 15 },
}
local function execute(operation)
  local result, failure, done
  query.request(vim.fs.dirname(path), { operation = operation, path = path, line = 4, column = 4 }, function(paths, receive)
    local allowed = {}
    for _, target in ipairs(paths) do allowed[target] = true end
    receive(allowed)
  end, function(_, _, receive) receive({ declarations = declarations, complete = true }) end,
    function(value, error) result, failure, done = value, error, true end)
  assert(vim.wait(1500, function() return done end, 5), "presentation query timed out")
  assert(not failure, failure)
  return result
end
local success, failure = xpcall(function()
  local definition = execute("definition")
  assert(definition.rows[1].line == 2 and definition.rows[1].column == 12, "definition pointed at an attribute instead of the identifier")
  local context = execute("context")
  assert(context.rows[1].excerpt[1].text == "fn caller() {}")
  assert(not context.hover and hover_count == 0, "callable source was supplemented with redundant hover metadata")
end, debug.traceback)
source.select_client, startup.wait = original_client, original_wait
package.loaded["forge.lsp.semantic"] = original_semantic
vim.fn.delete(path)
assert(success, failure)
print("lsp_presentation: passed")
