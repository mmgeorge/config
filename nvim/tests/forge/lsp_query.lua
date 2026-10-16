local startup = require("forge.lsp.startup")
local original_wait = startup.wait
startup.wait = function(_, _, receive) receive() return function() end end
local query = require("forge.lsp.semantic")
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
local function execute(arguments, authorize, budget, workspace)
  local done, result, failure = false
  local cancel = query.request(workspace or directory, arguments, authorize or function(paths, receive)
    local allowed = {}
    for _, target in ipairs(paths) do allowed[target] = true end
    receive(allowed)
  end, function(value, error) result, failure, done = value, error, true end, budget)
  return function()
    assert(vim.wait(1500, function() return done end, 5), "LSP query did not finish")
    return result, failure
  end, cancel
end
local success, failure = xpcall(function()
  for _, operation in ipairs({ "definition", "type_definition", "implementation", "references" }) do
    handler = function(_, params, callback)
      assert(params.position.character == 3, "input did not use server UTF-16 coordinates")
      assert(params.textDocument.uri == vim.uri_from_bufnr(buffer), "request changed the attached document URI")
      callback(nil, { { targetUri = vim.uri_from_fname(path), targetSelectionRange = range },
        { uri = vim.uri_from_fname(path), range = range } })
    end
    local await = execute({ operation = operation, path = vim.fn.has("win32") == 1 and path:upper() or path, line = 1, column = 3 })
    local result, error = await()
    assert(not error, error)
    assert(#result.rows == 1 and result.rows[1].column == 3, vim.inspect(result))
    assert(result.source:find("unsaved", 1, true))
  end
  local retries = 0
  handler = function(_, _, callback)
    retries = retries + 1
    if retries == 1 then callback({ code = -32801, message = "content modified" })
    else callback(nil, { { uri = vim.uri_from_fname(path), range = range } }) end
  end
  local retry = execute({ operation = "definition", path = path, line = 1, column = 3 })
  assert(#assert(retry()).rows == 1 and retries == 2, "content-modified response was not retried")
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
  result = assert(await())
  assert(#result.rows == 2 and result.rows[1].text == "sample" and result.rows[1].is_root)
  assert(result.rows[2].text == "caller" and not result.rows[2].is_root and result.rows[2].offset == 0)
  handler = function(method, _, callback)
    if method == "textDocument/prepareCallHierarchy" then
      callback(nil, { { name = "sample", uri = vim.uri_from_fname(path), range = range, selectionRange = range } })
    else callback(nil, {}) end
  end
  await = execute({ operation = "calls", path = path, line = 1, column = 3, direction = "outgoing" })
  result = assert(await())
  assert(#result.rows == 1 and result.rows[1].is_root, "empty hierarchy lost its prepared root")
  handler = function(method, _, callback)
    if method == "textDocument/prepareCallHierarchy" then
      callback(nil, { { name = "sample", uri = vim.uri_from_fname(path), range = range, selectionRange = range } })
    else callback(nil, {
      { to = { name = "first", uri = vim.uri_from_fname(path), range = range } },
      { to = { name = "second", uri = vim.uri_from_fname(path), range = range } },
    }) end
  end
  await = execute({ operation = "calls", path = path, line = 1, column = 3, direction = "outgoing", limit = 1 })
  result = assert(await())
  assert(#result.rows == 2 and result.rows[1].is_root and result.rows[2].text == "first" and result.next_offset == 1)
  await = execute({ operation = "calls", path = path, line = 1, column = 3, direction = "outgoing", limit = 1, offset = 1 })
  result = assert(await())
  assert(#result.rows == 2 and result.rows[1].is_root and result.rows[2].text == "second" and not result.next_offset)
  local before_budget = #calls
  local budget = { remaining = 1 }
  await = execute({ operation = "calls", path = path, line = 1, column = 3, direction = "outgoing" }, nil, budget)
  local limited, budget_error = await()
  assert(not limited and budget_error:find("budget_exhausted", 1, true), budget_error)
  assert(#calls == before_budget + 1 and budget.remaining == 0, "native request budget was exceeded")
  handler = function(_, _, callback) callback(nil, { items = { { range = range, severity = 1, message = "fixture error" } } }) end
  await = execute({ operation = "diagnostics", path = path })
  result = assert(await())
  assert(result.freshness == "current" and result.rows[1].text == "error: fixture error")
  handler = function(_, _, callback) callback(nil, { { uri = vim.uri_from_fname(path), range = range } }) end
  await = execute({ operation = "definition", path = path, line = 1, column = 3 }, function(_, receive) receive({}) end)
  result = assert(await())
  assert(#result.rows == 0 and #result.warnings == 1, "denied target was included")
  await = execute({ operation = "references", path = path, line = 1, column = 3, scope = "workspace" }, nil, nil, directory .. "/inner")
  result = assert(await())
  assert(#result.rows == 0 and not result.warnings, "scope filtering was reported as a permission denial")
  handler = function(_, _, callback)
    callback(nil, {
      { name = "sample", kind = 12, location = { uri = vim.uri_from_fname(directory .. "/other.lua"), range = range } },
      { name = "unrelated", kind = 12, location = { uri = vim.uri_from_fname(path), range = range } },
      { name = "sample", kind = 12, location = { uri = vim.uri_from_fname(path), range = range } },
    })
  end
  await = execute({ operation = "workspace_symbols", path = path, query = "sample", name_filter = "sample", search_path = path, limit = 1 })
  result = assert(await())
  assert(#result.rows == 1 and result.rows[1].path == require("forge.lsp.source").canonical(path), "symbol search escaped its file")
  assert(result.rows[1].text == "Function sample" and not result.next_offset, "filtering happened after pagination")
  client.server_info = { name = "rust-analyzer" }
  for _, scope in ipairs({ "workspace", "all" }) do
    handler = function(_, arguments, callback)
      assert(arguments.query == (scope == "all" and "sample#*" or "sample#"), "Rust lookup excluded non-type symbols or dependencies")
      callback(nil, {})
    end
    await = execute({ operation = "workspace_symbols", path = path, query = "sample", scope = scope })
    assert(await())
  end
  client.server_info = nil
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
  assert(not stale and error:find("source_changed", 1, true))
  assert(vim.api.nvim_buf_is_valid(buffer), "user buffer was deleted")
end, debug.traceback)
vim.lsp.get_clients, vim.lsp.buf_is_attached = original_clients, original_attached
startup.wait = original_wait
vim.api.nvim_buf_delete(buffer, { force = true })
vim.fn.delete(directory, "rf")
if not success then error(failure) end
print("lsp_query: passed")
