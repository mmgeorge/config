local source = require("forge.lsp.source")
local startup = require("forge.lsp.startup")
startup.wait = function(_, _, receive) receive() return function() end end
local directory = vim.fn.tempname()
vim.fn.mkdir(directory, "p")
local path = directory .. "/source.rs"
vim.fn.writefile({ "pub struct Store;", "impl Store {", "  fn save() {}", "  fn flush() {}", "}" }, path)
local declarations = {
  { name = "Store", kind = "struct_item", signature = "pub struct Store", visibility = "public", parent = vim.NIL, depth = 1,
    line = 1, column = 12, start_line = 1, end_line = 1, range_start_line = 1, range_start_column = 1, range_end_line = 1, range_end_column = 18 },
  { name = "Store", kind = "impl_item", signature = "impl Store", visibility = "structural", parent = vim.NIL, depth = 1,
    line = 2, column = 6, start_line = 2, end_line = 5, range_start_line = 2, range_start_column = 1, range_end_line = 5, range_end_column = 2 },
  { name = "save", kind = "function_item", signature = "fn save()", visibility = "unknown", parent = 1, depth = 2,
    line = 3, column = 6, start_line = 3, end_line = 3, range_start_line = 3, range_start_column = 3, range_end_line = 3, range_end_column = 15 },
  { name = "flush", kind = "function_item", signature = "fn flush()", visibility = "unknown", parent = 1, depth = 2,
    line = 4, column = 6, start_line = 4, end_line = 4, range_start_line = 4, range_start_column = 3, range_end_line = 4, range_end_column = 16 },
}
local reply, semantic_failure, analysis_failure
package.loaded["forge.lsp.semantic"] = { request = function(_, arguments, _, receive)
  if arguments.operation == "hover" then receive({ rows = {} })
  else receive(reply or { rows = {} }, semantic_failure) end
  return function() end
end }
local query = require("forge.lsp.query")
local function execute(arguments)
  local result, failure, done
  query.request(directory, arguments, function(paths, receive)
    local allowed = {}
    for _, item in ipairs(paths) do allowed[item] = true end
    receive(allowed)
  end, function(_, _, receive)
    receive({ declarations = declarations, complete = true }, analysis_failure)
  end, function(value, error) result, failure, done = value, error, true end)
  assert(vim.wait(2000, function() return done end, 5), "failure fixture did not finish")
  return result, failure
end
local success, failure = xpcall(function()
  local overview = assert(execute({ operation = "overview", path = path }))
  assert(overview.omitted_visibility_count == 2 and #overview.omitted_visibility == 2)
  assert(overview.omitted_visibility[1].text == "Store::save" and overview.omitted_visibility[2].line == 4)
  local all = assert(execute({ operation = "overview", path = path, visibility = "all" }))
  assert(all.omitted_visibility_count == 0 and #all.rows == 4)
  local qualified = assert(execute({ operation = "symbols", path = path, query = "Store::save" }))
  assert(#qualified.rows == 1 and qualified.rows[1].line == 3, "qualified file search depended on workspace results")
  local filtered = assert(execute({ operation = "symbols", path = path, query = "save" }))
  assert(#filtered.rows == 1 and filtered.rows[1].line == 3)

  semantic_failure = "unsupported: fixture does not support call hierarchy."
  local context = assert(execute({ operation = "context", path = path, symbol = "Store::save", include = { "callers" } }))
  assert(#context.rows[1].excerpt == 1 and context.rows[1].excerpt[1].line == 3)
  assert(context.warnings[1]:find("callers unavailable", 1, true))
  local absent, error = execute({ operation = "calls", path = path, symbol = "Store::save", direction = "incoming" })
  assert(not absent and error == semantic_failure)

  semantic_failure = "budget_exhausted: native LSP request limit reached"
  local exhausted = assert(execute({ operation = "paths", from = { symbol = "Store::save" }, to = { symbol = "Store::flush" }, max_requests = 2 }))
  assert(#exhausted.rows == 0 and exhausted.warnings[1] == "LSP request limit reached. Search incomplete.")

  semantic_failure = nil
  reply = { rows = { { path = path, line = 3, column = 6, text = "save" } } }
  analysis_failure = "Parser worker unavailable"
  local definition = assert(execute({ operation = "definition", path = path, line = 3, column = 6 }))
  assert(definition.rows[1].line == 3 and definition.warnings[1]:find("Parser worker unavailable", 1, true))
  local references = assert(execute({ operation = "references", path = path, line = 3, column = 6 }))
  assert(references.rows[1].line == 3, "missing structure discarded semantic references")
  analysis_failure = nil

  local selected, missing = execute({ operation = "context", path = path, symbol = "absent" })
  assert(not selected and missing == "symbol_not_found: absent in source.rs", missing)
  selected, missing = execute({ operation = "context", path = path, line = 200, column = 1 })
  assert(not selected and missing:find("invalid_position:", 1, true))
  selected, missing = execute({ operation = "context", path = directory .. "/missing.rs", symbol = "absent" })
  assert(not selected and missing:find("path_not_found:", 1, true), missing)

  local large = directory .. "/large.rs"
  vim.fn.writefile({ string.rep(" ", 300000), "pub struct Large;" }, large)
  local large_result = assert(execute({ operation = "symbols", path = large, visibility = "all", limit = 1000000 }))
  assert(#large_result.rows == 4, "large source was rejected before analysis")

  local original_clients = vim.lsp.get_clients
  vim.lsp.get_clients = function() return { { name = "fixture", config = { filetypes = { "rust" }, root_dir = directory },
    supports_method = function() return false end, is_stopped = function() return false end } } end
  local client, unsupported = source.select_client(path, "rust", nil, "textDocument/prepareCallHierarchy", directory)
  assert(not client and unsupported == "unsupported: fixture does not support call hierarchy.")
  vim.lsp.get_clients = original_clients
end, debug.traceback)
vim.fn.delete(directory, "rf")
assert(success, failure)
print("lsp_failures: passed")
