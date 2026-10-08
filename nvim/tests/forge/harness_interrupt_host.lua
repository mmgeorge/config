vim.opt.runtimepath:prepend("nvim")
vim.loader.enable(false)
local root = vim.fn.getcwd()
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
assert(vim.fn.mkdir(workspace, "p") == 1 and vim.fn.mkdir(data, "p") == 1)
local executable = require("forge.builder").binary_path()
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind)
  return (kind == "data" or kind == "config") and data or original_stdpath(kind)
end
package.loaded["forge.builder"] = { ensure = function(callback)
  callback({ ok = true, path = executable })
  return function() end
end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local errors = {}
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
local success, failure = xpcall(function()
  vim.fn.chdir(workspace)
  require("forge").setup({ harness = { backend = "mock", backends = { mock = { command = { "blocking" } } } } })
  local opened
  client.start_harness(function(snapshot, request_error)
    assert(not request_error, request_error)
    opened = snapshot
  end)
  assert(vim.wait(10000, function() return opened ~= nil end, 10), "host initialization stalled")
  local finished, rejected, cancelled
  client.request("prompt.submit", { text = "Wait for interruption" }, function(_, request_error, detail)
    assert(request_error and detail.code == "turn_cancelled", vim.inspect({ request_error, detail }))
    finished = true
  end)
  client.request("prompt.submit", { text = "Must not reset cancellation" }, function(_, request_error)
    assert(request_error and request_error:find("execution request is already running", 1, true), request_error)
    rejected = true
  end)
  assert(vim.wait(5000, function() return rejected end, 10), "overlapping request was not rejected")
  client.request("turn.cancel", {}, function(result, request_error)
    assert(not request_error, request_error)
    cancelled = result.cancel_requested
  end)
  assert(vim.wait(10000, function() return finished and cancelled end, 10), "cancellation failed to settle")

  finished, cancelled = false, false
  client.request("prompt.submit", { text = "Cancel immediately after admission" }, function(_, request_error, detail)
    assert(request_error and detail.code == "turn_cancelled", vim.inspect({ request_error, detail }))
    finished = true
  end)
  client.request("turn.cancel", {}, function(result, request_error)
    assert(not request_error, request_error)
    cancelled = result.cancel_requested
  end)
  assert(vim.wait(10000, function() return finished and cancelled end, 10), "early cancellation was lost")
  finished = false
  local restarted, stopped
  client.request("goal.set", { objective = "Retain a durable stop after restart" }, function(_, request_error, detail)
    assert(request_error and detail.code == "turn_cancelled", vim.inspect({ request_error, detail }))
    finished = true
  end)
  client.request("turn.restart", {}, function(result, request_error)
    assert(not request_error, request_error)
    restarted = result.restart_requested
  end)
  assert(vim.wait(10000, function() return finished and restarted end, 10), "restart did not settle")
  client.request("turn.cancel", {}, function(result, request_error)
    assert(not request_error, request_error)
    stopped = result.cancel_requested
  end)
  assert(vim.wait(5000, function() return stopped end, 10), "settled restart could not be cancelled")
  local settled
  client.request("state.get", {}, function(result, request_error)
    assert(not request_error, request_error)
    settled = result
  end)
  assert(vim.wait(5000, function() return settled ~= nil end, 10))
  assert(settled.goal.state == "paused", "goal stop was not persisted")
  assert(#errors == 0, table.concat(errors, "\n"))
end, debug.traceback)
client.stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.fn.chdir(root)
vim.fn.stdpath = original_stdpath
vim.fn.delete(workspace, "rf")
vim.fn.delete(data, "rf")
assert(success and collected, failure or "host was not collected")
print("harness_interrupt_host: passed")
