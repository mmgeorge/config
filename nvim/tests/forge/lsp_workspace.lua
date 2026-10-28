local startup = require("forge.lsp.startup")
local semantic = require("forge.lsp.semantic")
local directory = vim.fn.tempname()
vim.fn.mkdir(directory .. "/rust", "p")
vim.fn.mkdir(directory .. "/typescript", "p")
local rust_path, typescript_path = directory .. "/rust/main.rs", directory .. "/typescript/main.ts"
vim.fn.writefile({ "fn sample() {}" }, rust_path)
vim.fn.writefile({ "function sample() {}" }, typescript_path)
local original = { wait = startup.wait, clients = vim.lsp.get_clients, timer = vim.uv.new_timer }
local clients, calls, cancelled, pending = {}, {}, {}, {}
local hold, empty, malformed = false, false, false
local held_name, fast_deadline, startup_calls
startup.wait = function(_, _, receive) startup_calls = (startup_calls or 0) + 1 receive() return function() end end
vim.uv.new_timer = function()
  local timer = assert(original.timer())
  return {
    start = function(_, delay, interval, callback) return timer:start(fast_deadline and delay == 10000 and 100 or delay, interval, callback) end,
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
  hold, empty, held_name, fast_deadline = false, true, "typescript", true
  pending, startup_calls = {}, 0
  local done, reply, failure
  require("forge.lsp.query").request(directory, { operation = "diagnostics" }, function(_, receive) receive({}) end,
    function() error("empty diagnostics should not require structural analysis") end,
    function(value, error) done, reply, failure = true, value, error end)
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
