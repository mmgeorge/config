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
local original_has = vim.fn.has
local original_notify = vim.notify
local original_builder = package.loaded["forge.builder"]
local original_client = package.loaded["forge.client"]
local signal_list, wait_count = {}, 0

local function run()
  vim.notify = function() end
  vim.fn.has = function(feature)
    if feature == "win32" then return 0 end
    return original_has(feature)
  end
  package.loaded["forge.builder"] = {
    ensure = function(callback)
      vim.schedule(function() callback({ ok = true, path = "/tmp/forge fixture/forge" }) end)
      return function() end
    end,
  }
  package.loaded["forge.client"] = nil
  local client = require("forge.client")
  client._set_launcher_for_test(function(command, options)
    assert_true(vim.deep_equal(command, { "/tmp/forge fixture/forge" }),
      "Unix host executable must remain one argv element")
    local process = {}
    process.write = function(_, encoded)
      local request = vim.json.decode(encoded)
      if request.method == "initialize" then
        vim.schedule(function()
          options.stdout(nil, vim.json.encode({ id = request.id, result = { protocol_version = protocol.VERSION } }) .. "\n")
        end)
      end
    end
    process.kill = function(_, signal) signal_list[#signal_list + 1] = signal end
    process.wait = function(_, timeout)
      wait_count = wait_count + 1
      assert_equals(timeout, 0, "forced Unix collection must not wait indefinitely")
      return { code = 137, signal = 9 }
    end
    return process
  end)

  local started, start_error = false, nil
  client.start(function(_, failure) started, start_error = true, failure end)
  assert_true(vim.wait(1000, function() return started end, 10), "client initialize did not complete")
  assert_equals(start_error, nil, "client initialize failed")
  client.stop("Unix cleanup fixture")
  assert_true(vim.wait(5000, function() return client._client.process == nil end, 10),
    "Unix forced shutdown did not collect the host")
  assert_true(vim.deep_equal(signal_list, { 15, 9 }), "Unix shutdown must send TERM then KILL")
  assert_equals(wait_count, 1, "Unix forced shutdown must collect exactly once")
  client._reset_for_test()
end

local ok, failure = xpcall(run, debug.traceback)
vim.fn.has = original_has
vim.notify = original_notify
package.loaded["forge.client"] = original_client
package.loaded["forge.builder"] = original_builder
if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
end
print("unix_client_shutdown: forced Unix process collection passed")
vim.cmd("qa!")
