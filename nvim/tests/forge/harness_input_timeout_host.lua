vim.loader.enable(false)
local root = vim.fn.getcwd()
vim.opt.runtimepath:prepend(root .. "/nvim")
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
assert(vim.fn.mkdir(workspace, "p") == 1 and vim.fn.mkdir(data, "p") == 1)
local executable = require("forge.builder").binary_path()
assert(vim.fn.executable(executable) == 1, "build the release Forge host first")
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind)
  return (kind == "data" or kind == "config") and data or original_stdpath(kind)
end
package.loaded["forge.builder"] = { ensure = function(callback)
  callback({ ok = true, path = executable })
  return function() end
end }
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local session = require("forge.session")
local errors = {}
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
local success, failure = xpcall(function()
  vim.fn.chdir(workspace)
  require("forge").setup({ harness = { provider = "mock", providers = { mock = { command = { "mock" } } } } })
  require("forge.views.harness").open()
  assert(vim.wait(10000, function()
    return (session.harness.session and session.harness.presentation and session.harness.presentation.ready) or #errors > 0
  end, 10))
  assert(#errors == 0, table.concat(errors, "\n"))
  local function request(method, params)
    local result, request_error, completed
    client.request({ method = method, params = params, session_id = require("forge.event_contract").method_policy(method).session_context ~= "none" and assert(require("forge.session").harness.session).id or nil }, function(value, error)
      result, request_error, completed = value, error, true
    end)
    assert(vim.wait(10000, function() return completed end, 10), method .. " did not complete")
    assert(not request_error, request_error)
    return result
  end
  local initial = request("session.state.get", {})
  assert(vim.deep_equal(initial.session.approval_timeout, { amount = 10, unit = "m" }))
  assert(vim.deep_equal(initial.session.tool_timeout, { amount = 10, unit = "m" }))
  local held = dofile(root .. "/nvim/tests/forge/support/mock_provider.lua").hold(session.harness.session.id, "chat")
  local prompt_finished, prompt_error
  client.request({ method = "session.prompt.submit", params = { text = "Verify timeout configuration during this turn" }, session_id = assert(require("forge.session").harness.session).id }, function(_, error)
    prompt_finished, prompt_error = true, error
  end)
  held:wait()
  assert(not prompt_finished, "mock turn must remain active during configuration")
  local updated = request("session.input.timeout.configure", {
    approval_timeout = { amount = 2, unit = "s" }, tool_timeout = { amount = 3, unit = "m" },
  })
  assert(updated.approval_timeout.amount == 2 and updated.tool_timeout.amount == 3)
  assert(not prompt_finished, "configuration waited for or interrupted the provider")
  held:release()
  assert(vim.wait(10000, function() return prompt_finished end, 10))
  assert(not prompt_error, prompt_error)
  local snapshot = request("session.state.get", {})
  assert(vim.deep_equal(snapshot.session.approval_timeout, updated.approval_timeout))
  assert(vim.deep_equal(snapshot.session.tool_timeout, updated.tool_timeout))
  local settings = request("settings.get", {})
  assert(vim.deep_equal(settings.desired.approval_timeout, updated.approval_timeout))
  assert(vim.deep_equal(settings.desired.tool_timeout, updated.tool_timeout))
  assert(#errors == 0, table.concat(errors, "\n"))
end, debug.traceback)
require("forge.views.picker").close(false)
client.host_stop()
local collected = vim.wait(6000, function() return client._client.process == nil end, 10)
vim.fn.chdir(root)
vim.fn.stdpath = original_stdpath
if not success or not collected then
  io.stderr:write(tostring(failure or "host was not collected"), "\n")
  vim.cmd("cquit 1")
end
print("harness_input_timeout_host: passed")
vim.cmd("qa!")
