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
client._host_set_launcher_for_test(vim.system)
local errors = {}
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
local session_id
local function request(method, params)
  local settled, value, failure
  client.request({ method = method, params = params or {}, session_id = session_id }, function(result, request_error)
    value, failure, settled = result, request_error, true
  end)
  assert(vim.wait(10000, function() return settled end, 10), method .. " timeout")
  assert(not failure, method .. ": " .. tostring(failure))
  return value
end
-- session.prompt.submit settles at admission, so a turn's outcome is read from its exchange record.
local function exchange_state(prompt)
  for _, exchange in ipairs(request("session.exchange.list") or {}) do
    if exchange.prompt == prompt then return exchange.state end
  end
end
local function await_cancelled(prompt, message)
  local observed
  assert(vim.wait(10000, function()
    observed = exchange_state(prompt)
    return observed == "cancelled"
  end, 10), message .. ": " .. tostring(observed))
end
local success, failure = xpcall(function()
  vim.fn.chdir(workspace)
  require("forge").setup({ harness = { provider = "mock", providers = { mock = { command = { "blocking" } } } } })
  local opened
  client.session_initialize(function(snapshot, request_error)
    assert(not request_error, request_error)
    opened = snapshot
  end)
  assert(vim.wait(10000, function() return opened ~= nil end, 10), "host initialization stalled")
  session_id = opened.session.id
  local mock = dofile(root .. "/nvim/tests/forge/support/mock_provider.lua")
  local held = mock.hold(session_id, "chat")
  request("session.prompt.submit", { text = "Wait for interruption" })
  assert(held:wait().prompt == "Wait for interruption")
  assert(request("session.turn.cancel").cancel_requested, "cancellation of a held turn was not requested")
  await_cancelled("Wait for interruption", "held turn did not settle as cancelled")

  -- Cancelling before the admitted input starts a turn suspends the queue and keeps the message.
  request("session.prompt.submit", { text = "Cancel immediately after admission" })
  assert(request("session.turn.cancel").cancel_requested, "early cancellation was not requested")
  local input
  assert(vim.wait(10000, function()
    input = request("session.input.get")
    return input.suspended == true
  end, 10), "early cancellation did not suspend the queue: " .. vim.inspect(input))
  local retained = vim.tbl_filter(function(message)
    return message.text == "Cancel immediately after admission" and message.delivery == "queued"
  end, input.messages or {})
  assert(#retained == 1, "early cancellation lost the admitted message: " .. vim.inspect(input))
  assert(exchange_state("Cancel immediately after admission") == nil, "early cancellation still started a turn")
  assert(#errors == 0, table.concat(errors, "\n"))
end, debug.traceback)
client.host_stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.fn.chdir(root)
vim.fn.stdpath = original_stdpath
vim.fn.delete(workspace, "rf")
vim.fn.delete(data, "rf")
assert(success and collected, failure or "host was not collected")
print("harness_interrupt_host: passed")
