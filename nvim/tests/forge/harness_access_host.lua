vim.opt.runtimepath:prepend("nvim")
vim.loader.enable(false)
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
assert(vim.fn.mkdir(workspace, "p") == 1 and vim.fn.mkdir(data, "p") == 1)
local executable = require("forge.builder").binary_path()
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return (kind == "data" or kind == "config") and data or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback) callback({ ok = true, path = executable }) return function() end end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local operation, starts, current_session = {}, 0, nil
client.subscribe(function(event, payload)
  if event == "task_operation" then operation[payload.id] = payload end
  if event == "backend_event" and payload.kind == "execution_state" then starts = starts + 1 end
  if event == "session_configured" then current_session = payload.session or payload end
end)
local function await(predicate, message)
  assert(vim.wait(10000, predicate, 10), message .. " " .. vim.inspect(operation))
end
local function request(session_id, method, params)
  local done, result, failure
  client.request_for(session_id, method, params, function(value, problem) done, result, failure = true, value, problem end)
  await(function() return done end, method)
  assert(not failure, failure)
  return result
end
local function open(session_id)
  local result, failure
  client.start_harness(function(value, problem) result, failure = value, problem end, { session_id = session_id })
  await(function() return result or failure end, "initialization")
  assert(not failure, failure)
  return result
end
local success, failure = xpcall(function()
  vim.fn.chdir(workspace)
  require("forge").setup({ harness = { backend = "mock", backends = { mock = { command = { "blocking" } } } } })
  local state = require("forge.session").harness
  require("forge").open_harness()
  await(function() return state.ready and state.session end, "open Harness")
  local session_id = state.session.id
  require("forge.views.harness.controller").task_transition({ action = "goal", text = "Continue until interrupted" })
  await(function() return starts == 1 end, "goal start")
  local access = { sandbox = false, write_access = "workspace", writable_directory = { workspace }, windows_sandbox = "elevated" }
  local applied
  require("forge.views.harness.controller").configure({ access = access }, false, function(value) applied = value end)
  await(function() return starts == 2 end, "configuration did not resume goal")
  assert(applied == true and state.session.access.sandbox == false, "configuration waited for goal completion")
  assert(current_session.access.sandbox == false and #current_session.access.writable_directory == 1)
  assert(current_session.execution_mode == "write")
  local task_id = current_session.current_task_id
  require("forge.views.harness.controller").set_mode("read")
  await(function() return starts == 3 end, "Read approval mode paused the task")
  assert(current_session.execution_mode == "read" and current_session.access.sandbox == false)
  assert(current_session.current_task_id == task_id)
  request(session_id, "task.transition", { operation_id = "pause", action = "pause" })
  await(function() return operation.pause and operation.pause.state == "completed" end, "pause")
  client._client.process:kill(9)
  await(function() return client._client.process == nil end, "shutdown")
  local recovered = open(session_id)
  assert(recovered.session.execution_mode == "read")
  assert(recovered.session.access.sandbox == false and #recovered.session.access.writable_directory == 1)
  assert(recovered.session.current_task_id == task_id)
end, debug.traceback)
client.stop()
if not success then error(failure) end
print("harness_access_host: passed")
vim.cmd("qa!")
