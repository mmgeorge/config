vim.loader.enable(false)
local client = require("forge.client")
local builder = require("forge.builder")
local original_ensure = builder.ensure
local launch_count, shutdown_count, restart_rejected, request_rejected = 0, 0, false, false
builder.ensure = function(callback) callback({ ok = true, path = "exit-admission-fixture" }) end
client._host_reset_for_test()
client._host_set_launcher_for_test(function(_, options, on_exit)
  launch_count = launch_count + 1
  return {
    write = function(_, encoded)
      local request = vim.json.decode(encoded)
      if request.method == "host.initialize" then
        options.stdout(nil, vim.json.encode({ id = request.id, result = {
          protocol_version = require("forge.protocol").VERSION,
        } }) .. "\n")
      elseif request.method == "host.shutdown" then
        shutdown_count = shutdown_count + 1
        vim.schedule(function() on_exit({ code = 0 }) end)
      end
    end,
    kill = function() error("graceful fixture must not need forced collection") end,
  }
end)

local success, failure = xpcall(function()
  local initialized = false
  client.host_start(function(_, problem) assert(not problem, problem) initialized = true end)
  assert(vim.wait(1000, function() return initialized end), "host did not initialize")
  client.host_stop(nil, function()
    -- Completion runs inside the exit wait, after normal draining has cleared.
    client.host_start(function(result, problem, detail)
      assert(result == nil and problem == "Forge host is exiting")
      restart_rejected = detail.code == "not_admitted"
    end)
    client.request({ method = "repository.revision.list", params = {} }, function(result, problem, detail)
      assert(result == nil and problem == "Forge host is exiting")
      request_rejected = detail.code == "not_admitted"
    end)
  end)
  assert(client._client.process ~= nil, "ordinary runtime stop became synchronous")
  vim.api.nvim_exec_autocmds("VimLeavePre", { group = "ForgeClient" })
  assert(client._client.process == nil and not client._client.draining, "exit returned before collection")
  assert(restart_rejected and request_rejected, "exit callback admitted replacement work")
  assert(launch_count == 1 and shutdown_count == 1, "already-draining exit relaunched or repeated shutdown")
end, debug.traceback)
client._host_reset_for_test()
builder.ensure = original_ensure
if not success then io.stderr:write(tostring(failure), "\n") vim.cmd("cquit 1") end
print("client_exit_admission: passed")
vim.cmd("qa!")
