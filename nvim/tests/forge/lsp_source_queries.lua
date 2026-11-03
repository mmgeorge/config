local startup = require("forge.lsp.startup")
local original_wait = startup.wait
local original_semantic = package.loaded["forge.lsp.semantic"]
local source = require("forge.lsp.source")
local original_select = source.select_client
local directory = vim.fn.tempname()
vim.fn.mkdir(directory .. "/src", "p")
local path = directory .. "/src/lib.rs"
vim.fn.writefile({ "pub struct Ready;" }, path)
local pending, startup_calls, semantic_calls = nil, 0, 0
startup.wait = function(_, _, receive)
  startup_calls = startup_calls + 1
  pending = receive
  return function() pending = nil end
end
package.loaded["forge.lsp.semantic"] = { request = function(_, _, _, receive)
  semantic_calls = semantic_calls + 1
  receive({ rows = {} })
  return function() end
end }
local query = require("forge.lsp.query")
local function execute(arguments)
  local done, result, failure = false, nil, nil
  local cancel = query.request(directory, arguments, function(paths, receive)
    local allowed = {}
    for _, item in ipairs(paths) do allowed[item] = true end
    receive(allowed)
  end, function(_, _, receive)
    receive({ complete = true, declarations = {
      { name = "Ready", signature = "pub struct Ready", visibility = "public", depth = 1,
        line = 1, column = 12, start_line = 1, end_line = 1, parent = vim.NIL,
        range_start_line = 1, range_start_column = 1, range_end_line = 1, range_end_column = 18 },
    } })
  end, function(value, error) done, result, failure = true, value, error end)
  return function() return done, result, failure end, cancel
end
local success, failure = xpcall(function()
  for _, arguments in ipairs({
    { operation = "overview", path = directory, depth = 2, limit = 50 },
    { operation = "symbols", path = directory, query = "Ready" },
    { operation = "symbols", path = path },
    { operation = "symbols" },
  }) do
    local inspect, cancel = execute(arguments)
    local done = vim.wait(1000, function() return inspect() end, 5)
    if not done then cancel() end
    assert(done, arguments.operation .. " waited for language-server startup instead of reading source")
    local _, result, error = inspect()
    assert(not error and result and #result.rows == 1, tostring(error))
    assert(result.rows[1].text == "pub struct Ready")
  end
  assert(startup_calls == 0 and semantic_calls == 0, "source listing contacted a language server")
  for _, target in ipairs({ { path = path, line = 1, column = 12 }, { path = path, symbol = "Ready" } }) do
    local inspect, cancel = execute(vim.tbl_extend("force", target, { operation = "context" }))
    local done = vim.wait(1000, function() return inspect() end, 5)
    if not done then cancel() end
    assert(done, "source context waited for language-server startup")
    local _, result, error = inspect()
    assert(not error and result.rows[1].excerpt[1].text == "pub struct Ready;", tostring(error))
  end
  assert(startup_calls == 0 and semantic_calls == 0, "source context contacted a language server")
  source.select_client = function() return { initialized = true, progress = { pending = { indexing = true } } } end
  local context = execute({ operation = "context", path = path, line = 1, column = 12 })
  assert(vim.wait(1000, function() return context() end, 5), "optional hover waited for indexing")
  assert(semantic_calls == 0, "optional hover used an indexing server")
  source.select_client = original_select
  local inspect = execute({ operation = "symbols", query = "Ready" })
  local done, result, error = inspect()
  assert(done and result and not error and semantic_calls == 1, tostring(error))
end, debug.traceback)
startup.wait = original_wait
source.select_client = original_select
package.loaded["forge.lsp.semantic"] = original_semantic
vim.fn.delete(directory, "rf")
assert(success, failure)
print("lsp_source_queries: passed")
