vim.loader.enable(false)

local function assert_true(value, message)
  if not value then error(message or "assertion failed", 2) end
end

local function assert_equals(actual, expected, message)
  if actual ~= expected then
    error((message or "values differ") .. "\nexpected: " .. vim.inspect(expected)
      .. "\nactual: " .. vim.inspect(actual), 2)
  end
end

local protocol = require("forge.protocol")
local original_system = vim.system
local original_notify = vim.notify
local original_builder = package.loaded["forge.builder"]
local original_client = package.loaded["forge.client"]
local taskkill_call = nil
local kill_signal = nil
local wait_count = 0

local function run()
  vim.notify = function() end
  package.loaded["forge.builder"] = {
    ensure = function(callback)
      vim.schedule(function() callback({ ok = true, path = "C:\\Forge Fixture\\forge.exe" }) end)
      return function() end
    end,
  }
  package.loaded["forge.client"] = nil
  local client = require("forge.client")
  client._set_launcher_for_test(function(command, options)
    assert_true(vim.deep_equal(command, { "C:\\Forge Fixture\\forge.exe" }),
      "host executable must remain one argv element")
    local process = {}
    process.write = function(_, encoded)
      local request = vim.json.decode(encoded)
      if request.method == "initialize" then
        vim.schedule(function()
          options.stdout(nil, vim.json.encode({ id = request.id, result = { protocol_version = protocol.VERSION } }) .. "\n")
        end)
      end
    end
    process.kill = function(_, signal) kill_signal = signal end
    process.wait = function()
      wait_count = wait_count + 1
      return { code = 0, signal = 0 }
    end
    process.pid = 4242
    return process
  end)
  vim.system = function(command, options, callback)
    taskkill_call = { command = vim.deepcopy(command), options = vim.deepcopy(options) }
    vim.schedule(function() callback({ code = 0, stdout = "SUCCESS", stderr = "" }) end)
    return { kill = function() end }
  end

  local started, start_error = false, nil
  client.start(function(_, failure)
    started, start_error = true, failure
  end)
  assert_true(vim.wait(1000, function() return started end, 10), "client initialize did not complete")
  assert_equals(start_error, nil, "client initialize failed")
  assert_true(client._client.process ~= nil, "client did not retain its process")

  client.stop("Windows cleanup fixture")
  assert_true(vim.wait(5000, function() return client._client.process == nil end, 10), "taskkill did not collect the host")
  assert_equals(kill_signal, 15, "drain deadline must request graceful process termination first")
  assert_true(vim.deep_equal(taskkill_call.command, { "taskkill", "/PID", "4242", "/T", "/F" }),
    "taskkill argv changed")
  assert_true(taskkill_call.options.text == true, "taskkill must decode Windows diagnostic text")
  assert_true(taskkill_call.options.stdout == true and taskkill_call.options.stderr == true,
    "taskkill must capture both output streams")
  assert_equals(taskkill_call.options.timeout, 2000, "taskkill timeout changed")
  assert_true(wait_count >= 1, "taskkill completion did not collect its process")
  client._reset_for_test()
end

local ok, failure = xpcall(run, debug.traceback)
vim.system = original_system
vim.notify = original_notify
package.loaded["forge.client"] = original_client
package.loaded["forge.builder"] = original_builder
if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
end
print("windows_ui_parity: executable argv and taskkill lifecycle passed")
vim.cmd("qa!")
