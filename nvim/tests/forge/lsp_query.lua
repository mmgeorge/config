local startup = require("forge.lsp.startup")
local original_wait = startup.wait
startup.wait = function(_, _, receive) receive() return function() end end
local query = require("forge.lsp.query")
local directory = vim.fn.tempname()
vim.fn.mkdir(directory, "p")
local path = directory .. "/source.lua"
vim.fn.writefile({ "😀 value", "function sample() end" }, path)
local buffer = vim.api.nvim_create_buf(false, false)
vim.api.nvim_buf_set_name(buffer, path)
vim.api.nvim_buf_set_lines(buffer, 0, -1, false, { "😀 value", "function sample() end" })
vim.bo[buffer].filetype = "lua"
local original_clients, original_attached = vim.lsp.get_clients, vim.lsp.buf_is_attached
local handler, calls, pending_callback = nil, {}, nil
local client = { id = 981, name = "fixture", config = { root_dir = directory, filetypes = { "lua" } }, offset_encoding = "utf-16" }
function client:is_stopped() return false end
function client:supports_method() return true end
function client:cancel_request() end
function client:request(method, params, callback)
  calls[#calls + 1] = { method = method, params = params }
  pending_callback = callback
  vim.schedule(function() if handler then handler(method, params, callback) end end)
  return true, #calls
end
vim.lsp.get_clients = function() return { client } end
vim.lsp.buf_is_attached = function() return true end
local range = { start = { line = 0, character = 3 }, ["end"] = { line = 0, character = 8 } }
local function execute(arguments, authorize)
  local done, result, failure = false
  local cancel = query.request(directory, arguments, authorize or function(paths, receive)
    local allowed = {}
    for _, target in ipairs(paths) do allowed[target] = true end
    receive(allowed)
  end, function(value, error) result, failure, done = value, error, true end)
  return function()
    assert(vim.wait(1500, function() return done end, 5), "LSP query did not finish")
    return result, failure
  end, cancel
end
local success, failure = xpcall(function()
  for _, operation in ipairs({ "definition", "type_definition", "implementation", "references" }) do
    handler = function(_, params, callback)
      assert(params.position.character == 3, "input did not use server UTF-16 coordinates")
      callback(nil, { { targetUri = vim.uri_from_fname(path), targetSelectionRange = range },
        { uri = vim.uri_from_fname(path), range = range } })
    end
    local await = execute({ operation = operation, path = path, line = 1, column = 3 })
    local result, error = await()
    assert(not error, error)
    assert(#result.rows == 1 and result.rows[1].column == 3, vim.inspect(result))
    assert(result.source:find("unsaved", 1, true))
  end
  handler = function(_, _, callback) callback(nil, {}) end
  local await = execute({ operation = "references", path = path, line = 1, column = 3 })
  assert(#assert(await()).rows == 0)
  handler = function(_, _, callback) callback(nil, { { name = "sample", kind = 12, selectionRange = range,
    children = { { name = "value", kind = 13, selectionRange = range } } } }) end
  await = execute({ operation = "document_symbols", path = path, query = "sample.value" })
  local result = assert(await())
  assert(#result.rows == 1 and result.rows[1].text:find("sample.value", 1, true))
  handler = function(_, _, callback) callback(nil, { contents = { kind = "markdown", value = "function sample()" } }) end
  await = execute({ operation = "hover", path = path, line = 1, column = 3 })
  assert(assert(await()).hover == "function sample()")
  handler = function(method, _, callback)
    if method == "textDocument/prepareCallHierarchy" then callback(nil, { { name = "sample", uri = vim.uri_from_fname(path), range = range, selectionRange = range } })
    else callback(nil, { { from = { name = "caller", uri = vim.uri_from_fname(path), range = range } } }) end
  end
  await = execute({ operation = "calls", path = path, line = 1, column = 3, direction = "incoming" })
  assert(assert(await()).rows[1].text == "caller")
  handler = function(_, _, callback) callback(nil, { items = { { range = range, severity = 1, message = "fixture error" } } }) end
  await = execute({ operation = "diagnostics", path = path })
  result = assert(await())
  assert(result.freshness == "current" and result.rows[1].text == "error: fixture error")
  handler = function(_, _, callback) callback(nil, { { uri = vim.uri_from_fname(path), range = range } }) end
  await = execute({ operation = "definition", path = path, line = 1, column = 3 }, function(_, receive) receive({}) end)
  result = assert(await())
  assert(#result.rows == 0 and #result.warnings == 1, "denied target was included")
  handler = nil
  local cancel
  await, cancel = execute({ operation = "definition", path = path, line = 1, column = 3 })
  cancel()
  local cancelled, error = await()
  assert(not cancelled and error:find("cancelled", 1, true))
  pending_callback(nil, { uri = vim.uri_from_fname(path), range = range })
  await = execute({ operation = "definition", path = path, line = 1, column = 3 })
  vim.api.nvim_buf_set_lines(buffer, 0, 1, false, { "changed" })
  local stale
  stale, error = await()
  assert(not stale and error:find("stale_source", 1, true))
  assert(vim.api.nvim_buf_is_valid(buffer), "user buffer was deleted")
end, debug.traceback)
vim.lsp.get_clients, vim.lsp.buf_is_attached = original_clients, original_attached
startup.wait = original_wait
vim.api.nvim_buf_delete(buffer, { force = true })
vim.fn.delete(directory, "rf")
if not success then error(failure) end
print("lsp_query: passed")
