-- Unit tests for `forge.lsp.semantic` (semantic.lua). Each scenario row runs in its own
-- headless Neovim: `.forge/checks/lua-tests.nu` passes the row's name as `arg[1]`.
local scenarios = {
  { name = "lsp_query", run = function()
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
  end },
  { name = "workspace", run = function()
local startup = require("forge.lsp.startup")
local semantic = require("forge.lsp.semantic")
local directory = vim.fn.tempname()
vim.fn.mkdir(directory .. "/rust", "p")
vim.fn.mkdir(directory .. "/typescript", "p")
local rust_path, typescript_path = directory .. "/rust/main.rs", directory .. "/typescript/main.ts"
vim.fn.writefile({ "fn sample() {}" }, rust_path)
vim.fn.writefile({ "function sample() {}" }, typescript_path)
local original = { wait = startup.wait, clients = vim.lsp.get_clients, timer = vim.uv.new_timer }
local clients, calls, answered, cancelled, pending = {}, {}, {}, {}, {}
local hold, empty, malformed = false, false, false
local held_name, captured_deadline, startup_calls
---Holds the semantic layer's ten-second workspace deadline callback while capture is enabled,
---so the fixture fires the expiry directly instead of letting a timer elapse.
---@type fun()?
local deadline_expiry
startup.wait = function(_, _, receive) startup_calls = (startup_calls or 0) + 1 receive() return function() end end
vim.uv.new_timer = function()
  local timer = assert(original.timer())
  return {
    start = function(_, delay, interval, callback)
      if captured_deadline and delay == 10000 then deadline_expiry = callback return 0 end
      return timer:start(delay, interval, callback)
    end,
    stop = function() return timer:stop() end,
    close = function() return timer:close() end,
  }
end
local function make_client(identity, name, root, path, supported, failed)
  local client = { id = identity, name = name, root_dir = root, config = { root_dir = root },
    initialized = true, offset_encoding = "utf-16",
    server_capabilities = { diagnosticProvider = { workspaceDiagnostics = supported } } }
  function client:is_stopped() return false end
  function client:supports_method() return supported end
  function client:cancel_request(request) cancelled[name .. ":" .. request] = true end
  function client:request(method, _, receive)
    calls[name] = (calls[name] or 0) + 1
    local function respond()
      answered[name] = (answered[name] or 0) + 1
      if failed then receive({ message = "fixture failure" }) return end
      local range = { start = { line = 0, character = 0 }, ["end"] = { line = 0, character = 1 } }
      if method == "workspace/diagnostic" then
        if malformed and name == "typescript" then receive(nil, {}) return end
        receive(nil, { items = { { uri = vim.uri_from_fname(path), kind = "full", resultId = name,
          items = empty and {} or { { range = range, severity = 1, message = name .. " failure" } } } } })
      else
        assert(method == "workspace/symbol")
        receive(nil, { { name = "sample", kind = 12, location = { uri = vim.uri_from_fname(path), range = range } } })
      end
    end
    if hold or held_name == name then pending[#pending + 1] = respond else vim.schedule(respond) end
    return true, calls[name]
  end
  return client
end
clients = {
  make_client(71, "rust", directory .. "/rust", rust_path, true),
  make_client(72, "typescript", directory .. "/typescript", typescript_path, true),
  make_client(73, "unsupported", directory, rust_path, false),
  make_client(74, "failed", directory, rust_path, true, true),
}
vim.lsp.get_clients = function() return clients end
local function query(operation)
  local done, result, failure
  local cancel = semantic.request(directory, { operation = operation, query = "sample", scope = "workspace" }, function(paths, receive)
    local allowed = {}
    for _, path in ipairs(paths) do allowed[path] = true end
    receive(allowed)
  end, function(value, error) done, result, failure = true, value, error end)
  return function()
    assert(vim.wait(2000, function() return done end, 5), "workspace query did not finish")
    return result, failure
  end, cancel
end
local success, failure = xpcall(function()
  local await = query("diagnostics")
  local result, error = await()
  assert(result and not error, error)
  assert(#result.rows == 2 and #result.coverage == 4 and result.freshness == "unknown", vim.inspect(result))
  assert(calls.rust == 1 and calls.typescript == 1 and not calls.unsupported and calls.failed == 1)
  local coverage = {}
  for _, entry in ipairs(result.coverage) do coverage[entry.name] = entry.state end
  assert(coverage.rust == "complete" and coverage.typescript == "complete")
  assert(coverage.unsupported == "unsupported" and coverage.failed == "failed")
  await = query("workspace_symbols")
  result, error = await()
  assert(result and not error and #result.rows == 2 and #result.coverage == 4, error)
  table.remove(clients, 4)
  table.remove(clients, 3)
  empty = true
  await = query("diagnostics")
  result = assert(await())
  assert(#result.rows == 0 and result.freshness == "current" and not result.warnings)
  for _, entry in ipairs(result.coverage) do assert(entry.state == "complete") end
  empty, malformed = false, true
  await = query("diagnostics")
  result = assert(await())
  assert(#result.rows == 1 and result.freshness == "unknown", "bad server discarded the good report")
  malformed, hold = false, true
  local cancel
  await, cancel = query("diagnostics")
  cancel()
  result, error = await()
  assert(not result and error:find("cancelled", 1, true))
  for _, respond in ipairs(pending) do respond() end
  assert(next(cancelled), "cancellation did not reach native requests")
  hold, empty, held_name, captured_deadline = false, true, "typescript", true
  pending, startup_calls = {}, 0
  local rust_answers = answered.rust or 0
  local done, reply, failure
  require("forge.lsp.query").request(directory, { operation = "diagnostics" }, function(_, receive) receive({}) end,
    function() error("empty diagnostics should not require structural analysis") end,
    function(value, error) done, reply, failure = true, value, error end)
  -- Expire only after the responsive server answers, so the deadline marks just the held one failed.
  assert(vim.wait(2000, function() return deadline_expiry ~= nil and (answered.rust or 0) > rust_answers end, 5),
    "workspace deadline was not armed or the responsive server did not answer")
  assert(not done, "workspace facade replied before its deadline")
  deadline_expiry()
  assert(vim.wait(2000, function() return done end, 5), "workspace facade lost its timeout reply")
  assert(reply and not failure and #reply.coverage == 2 and reply.freshness == "unknown", failure or vim.inspect(reply))
  assert(startup_calls == 1, "workspace operation repeated its startup wait")
  for _, entry in ipairs(reply.coverage) do
    assert(entry.state == (entry.name == "typescript" and "failed" or "complete"))
  end
  for _, respond in ipairs(pending) do respond() end
end, debug.traceback)
startup.wait, vim.lsp.get_clients = original.wait, original.clients
vim.uv.new_timer = original.timer
vim.fn.delete(directory, "rf")
assert(success, failure)
print("lsp_workspace: passed")
  end },
}

local selected = arg[1]
for _, scenario in ipairs(scenarios) do
  if scenario.name == selected then return scenario.run() end
end
error(("unknown scenario %q"):format(tostring(selected)))
