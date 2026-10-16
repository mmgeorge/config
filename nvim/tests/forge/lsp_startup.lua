local startup = require("forge.lsp.startup")
local query = require("forge.lsp.semantic")
local source = require("forge.lsp.source")
local directory = vim.fn.tempname()
vim.fn.mkdir(directory, "p")
local path = directory .. "/main.lua"
vim.fn.writefile({ "function ready() end" }, path)
local original = { configs = vim.lsp.get_configs, start = vim.lsp.start, client = vim.lsp.get_client_by_id,
  clients = vim.lsp.get_clients, attached = vim.lsp.buf_is_attached }
local calls, queried, client = 0, false, nil
local root_in_fast_event = false
local deferred_root
local hold_root = false
vim.lsp.get_configs = function(filter)
  assert(filter.enabled)
  return { { name = "delayed", filetypes = { "lua" }, root_dir = function(_, on_root)
    if hold_root then deferred_root = on_root return end
    vim.uv.fs_stat(path, function(failure)
      assert(not failure, failure)
      root_in_fast_event = vim.in_fast_event()
      on_root(directory)
    end)
  end } }
end
vim.lsp.start = function(config, options)
  assert(not vim.in_fast_event(), "LSP start ran in a fast event")
  assert(vim.api.nvim_buf_is_loaded(options.bufnr), "startup source was unloaded")
  assert(config.name == "delayed" and config.root_dir == directory)
  if client then return client.id end
  calls = calls + 1
  client = { id = 91, initialized = false, config = config, name = config.name }
  function client:is_stopped() return false end
  function client:supports_method() return true end
  function client:request(_, _, callback)
    assert(self.initialized, "query reached a server before initialization")
    queried = true
    vim.schedule(function() callback(nil, { contents = "ready" }) end)
    return true, 1
  end
  function client:cancel_request() end
  return client.id
end
vim.lsp.get_client_by_id = function(identity) return client and client.id == identity and client end
vim.lsp.get_clients = function() return client and client.initialized and { client } or {} end
vim.lsp.buf_is_attached = function() return true end
local success, failure = xpcall(function()
  startup.start(directory)
  assert(vim.wait(1000, function() return client ~= nil end, 5), "background startup did not run")
  assert(root_in_fast_event, "fixture did not exercise a libuv root callback")
  local done, result, error
  query.request(directory, { operation = "hover", path = path, line = 1, column = 1 }, function(_, receive) receive({}) end,
    function(value, message) done, result, error = true, value, message end)
  vim.wait(150, function() return done end, 5)
  assert(not done and not queried, "query resolved while the server was initializing")
  client.initialized = true
  client.progress = { pending = { indexing = "Indexing" } }
  vim.wait(150, function() return done end, 5)
  assert(not done and not queried, "query ran while the server reported indexing")
  client.progress.pending = {}
  assert(vim.wait(1000, function() return done end, 5), "query did not resume after initialization")
  assert(not error and result.hover == "ready", tostring(error))
  assert(calls == 1, "query created a duplicate server")
  local missing, missing_done
  startup.wait(directory, directory .. "/missing.lua", function(failure) missing, missing_done = failure, true end)
  assert(vim.wait(1000, function() return missing_done end, 5))
  assert(missing:find("path_not_found:", 1, true) and calls == 1)
  local recovered, recovery_error
  startup.wait(directory, path, function(failure) recovered, recovery_error = true, failure end)
  assert(vim.wait(1000, function() return recovered end, 5))
  assert(not recovery_error, "missing source poisoned the valid startup waiter")
  local cancelled = false
  client.initialized = false
  local cancel = startup.wait(directory, path, function() cancelled = true end)
  cancel()
  vim.wait(80, function() return cancelled end, 5)
  assert(not cancelled, "cancelled startup waiter called its consumer")
  startup.close()
  assert(client and calls == 1, "closing Harness stopped an editor-owned server")
  assert(not source.loaded(path), "startup leaked a hidden buffer")
  hold_root = true
  startup.start(directory)
  assert(vim.wait(1000, function() return deferred_root ~= nil end, 5), "deferred root was not requested")
  startup.close()
  deferred_root(directory)
  vim.wait(80, function() return calls > 1 end, 5)
  assert(calls == 1 and not source.loaded(path), "late root callback restarted a closed workspace")
end, debug.traceback)
startup.close()
vim.lsp.get_configs, vim.lsp.start, vim.lsp.get_client_by_id = original.configs, original.start, original.client
vim.lsp.get_clients, vim.lsp.buf_is_attached = original.clients, original.attached
vim.fn.delete(directory, "rf")
assert(success, failure)
print("lsp_startup: passed")
