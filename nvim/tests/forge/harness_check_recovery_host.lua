vim.loader.enable(false)
local root = vim.fn.getcwd()
vim.opt.runtimepath:prepend(root .. "/nvim")
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
local executable = vim.fn.fnamemodify(require("forge.builder").binary_path(), ":p")
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
local function request(session_id, method, params)
  local received, result, failure
  client.request_for(session_id, method, params or {}, function(value, problem)
    result, failure, received = value, problem, true
  end)
  assert(vim.wait(10000, function() return received end, 10), method .. " timed out")
  assert(not failure, method .. ": " .. tostring(failure))
  return result
end
local function open(session_id)
  local result, failure
  client.start_harness(function(value, problem) result, failure = value, problem end, { session_id = session_id })
  assert(vim.wait(10000, function() return result or failure end, 10), "reopen timed out")
  assert(not failure, failure)
  return result
end
local success, failure = xpcall(function()
  vim.fn.writefile({ vim.json.encode({ checks = {
  ["slow"] = { trigger = { auto = false }, action = { command = "print running; sleep 30sec", label = "Recovery check", category = "test" } }
} }) }, workspace .. "/.forge.json")
  vim.fn.chdir(workspace)
  require("forge").setup({ harness = { backend = "mock", backends = { mock = { command = { "mock" } } } } })
  local snapshot = open()
  local session_id = snapshot.session.id
  request(session_id, "session.configure", { shell = "nushell" })
  request(session_id, "prompt.submit", { text = "Before closing" })
  for _, pause_first in ipairs({ true, false }) do
    local run_id = request(session_id, "checks.run", { check = { "slow" } }).run_id
    assert(vim.wait(5000, function()
      local run = request(session_id, "checks.status", { run_id = run_id })
      return run.commands[1] and run.commands[1].state == "running"
    end, 10), "check did not start")
    if pause_first then
      request(session_id, "task.transition", { operation_id = "pause-before-close", action = "pause" })
    end
    if vim.fn.has("win32") == 1 then
      local killed = vim.system({ "taskkill", "/PID", tostring(client._client.process.pid), "/T", "/F" }, { text = true }):wait(5000)
      assert(killed.code == 0, killed.stderr)
    else
      client._client.process:kill(9)
    end
    assert(vim.wait(5000, function() return client._client.process == nil end, 10), "closed host retained ownership")
    snapshot = open(session_id)
    assert(not snapshot.checks_running, "reopened host retained active checks")
    if pause_first then
      local pause = request(session_id, "task.operation", { operation_id = "pause-before-close" })
      assert(pause.state == "completed" or pause.state == "cancelled", vim.inspect(pause))
      assert(not pause.error or pause.error == vim.NIL, "recovered pause retained an error")
    end
    assert(request(session_id, "checks.status", { run_id = run_id }).state == "interrupted",
      "recovery did not retain interrupted check history")
    request(session_id, "prompt.submit", { text = "Continue after reopening " .. tostring(pause_first) })
    local history = request(session_id, "exchange.list")
    assert(history[#history].state == "complete", "next prompt did not finish after recovery")
    assert(#request(session_id, "checks.list").history == (pause_first and 1 or 2),
      "reopening restarted checks without a request")
  end
end, debug.traceback)
client.stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.fn.chdir(root)
vim.fn.stdpath = original_stdpath
assert(success and collected, failure or "host was not collected")
print("harness_check_recovery_host: passed")
vim.cmd("qa!")
