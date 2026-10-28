vim.loader.enable(false)
local builder = require("forge.builder")
local client = require("forge.client")
local original_ensure = builder.ensure
local requests = {}
local snapshot = { session = { id = "active-startup", current_task_id = "restored-task" },
  startup_timing = { phases = { controller_ready_ms = 25 }, broker = { database_open_ms = 4 } } }
local result, failure, completed

local success, error_message = xpcall(function()
  require("forge").setup({ harness = { backend = "mock" }, harness_logging = false })
  builder.ensure = function(callback) callback({ ok = true, path = "active-startup-fixture" }) end
  client._reset_for_test()
  client._set_launcher_for_test(function(_, options, on_exit)
    local function emit(message)
      options.stdout(nil, vim.json.encode(message) .. "\n")
    end
    return {
      write = function(_, encoded)
        local request = vim.json.decode(encoded)
        requests[#requests + 1] = request.method
        if request.method == "initialize" then
          emit({ id = request.id, result = { protocol_version = require("forge.protocol").VERSION } })
        elseif request.method == "harness.initialize" then
          emit({ id = request.id, result = snapshot })
        elseif request.method == "trace.configure" then
          emit({ id = request.id, result = { enabled = false } })
        elseif request.method == "shutdown" then
          vim.schedule(function() on_exit({ code = 0 }) end)
        elseif request.method ~= "transport.consumed" then
          emit({ id = request.id, error = {
            code = "request_failed",
            message = "This operation requires a settled session. Use a task transition while execution is active: no permits available",
          } })
        end
      end,
      kill = function() end,
    }
  end)
  local phases = {}
  client.start_harness(function(value, request_error)
    result, failure, completed = value, request_error, true
  end, nil, { mark = function(phase, fields) phases[#phases + 1] = { phase = phase, fields = fields } end })
  assert(vim.wait(1000, function() return completed end, 5), "Harness startup did not finish")
  assert(not failure, failure)
  assert(result.session.current_task_id == "restored-task", "startup changed the active task")
  assert(client._client.harness_ready, "restored Harness session is not ready")
  assert(phases[1].phase == "host.requested" and phases[2].phase == "host.ready")
  assert(phases[3].phase == "harness.initialize.requested" and phases[4].phase == "harness.initialize.received")
  assert(phases[4].fields.rust.broker.database_open_ms == 4 and phases[5].phase == "trace.configure.received")
  assert(vim.tbl_contains(requests, "trace.configure"), "startup omitted tracing configuration")
  local allowed = { initialize = true, ["harness.initialize"] = true,
    ["trace.configure"] = true, ["transport.consumed"] = true }
  for _, method in ipairs(requests) do
    assert(allowed[method], "startup sent a session mutation: " .. method)
  end
end, debug.traceback)

client._reset_for_test()
builder.ensure = original_ensure
if not success then
  io.stderr:write(tostring(error_message), "\n")
  vim.cmd("cquit 1")
end
print("harness_active_startup: passed")
vim.cmd("qa!")
