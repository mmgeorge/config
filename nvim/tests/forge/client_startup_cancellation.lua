vim.loader.enable(false)
local client = require("forge.client")
local builder = require("forge.builder")
local original_ensure = builder.ensure
local initialized, failed = false, 0
builder.ensure = function(callback) callback({ ok = true, path = "startup-cancellation" }) end
client._host_reset_for_test()
client._host_set_launcher_for_test(function(_, options, on_exit)
  return {
    write = function(_, encoded)
      local request = vim.json.decode(encoded)
      if request.method == "host.initialize" then
        options.stdout(nil, vim.json.encode({ id = request.id, result = {
          protocol_version = require("forge.protocol").VERSION,
        } }) .. "\n")
      elseif request.method == "session.initialize" then
        initialized = true
      elseif request.method == "host.shutdown" then
        vim.schedule(function() on_exit({ code = 0 }) end)
      end
    end,
    kill = function() end,
  }
end)
local success, failure = xpcall(function()
  for _ = 1, 2 do
    client.session_initialize(function(result, message)
      assert(result == nil and message, "cancelled startup reported success")
      failed = failed + 1
    end, { services_only = true })
  end
  assert(vim.wait(1000, function() return initialized end, 5), "initialization was not sent")
  client.host_stop()
  assert(vim.wait(1000, function() return client._client.process == nil end, 5), "host did not stop")
  assert(failed == 2, "concurrent runtime startup lost a completion callback: " .. failed)
  assert(not client._client.runtime_starting and not client._client.harness_starting)
  assert(vim.tbl_isempty(client._client.runtime_initialize_callback))
  assert(vim.tbl_isempty(client._client.pending))
end, debug.traceback)
client._host_reset_for_test()
builder.ensure = original_ensure
if not success then io.stderr:write(tostring(failure), "\n") vim.cmd("cquit 1") end
print("client_startup_cancellation: passed")
vim.cmd("qa!")
