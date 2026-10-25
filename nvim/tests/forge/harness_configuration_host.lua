vim.loader.enable(false)
local root = vim.fn.getcwd()
vim.opt.runtimepath:prepend(root .. "/nvim")
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
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local task = require("forge.views.harness.task")
local errors, starts = {}, 0
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
client.subscribe(function(event, payload)
  if event == "backend_event" and payload.kind == "execution_state" then starts = starts + 1 end
end)
local function await(predicate, message)
  assert(vim.wait(10000, function() return #errors > 0 or predicate() end, 10),
    message .. ": " .. vim.inspect({ starts = starts, mode = state.session and state.session.execution_mode,
      task = task.current(state), operation = state.task_operation, error = state.configuration_error }))
  assert(#errors == 0, table.concat(errors, "\n"))
end
local success, failure = xpcall(function()
  vim.fn.chdir(workspace)
  require("forge").setup({ harness = { backend = "mock", backends = { mock = { command = { "blocking" } } } } })
  require("forge").open_harness()
  await(function() return state.ready and state.session end, "Harness did not open")

  local idle_applied
  controller.configure({ effort = "low" }, true, function(applied) idle_applied = applied end)
  await(function() return idle_applied ~= nil end, "idle configuration did not settle")
  assert(idle_applied and state.session.effort == "low" and starts == 0)

  controller.task_transition({ action = "goal", text = "Wait for configuration changes" })
  await(function() return starts == 1 and task.current(state) end, "goal did not start")
  local task_id = task.current(state).id
  state.busy, state.task_operation = false, nil
  local applied
  controller.configure({ effort = "high" }, true, function(value) applied = value end)
  await(function() return applied ~= nil and starts == 2 end, "configuration did not resume active work")
  assert(applied and state.session.effort == "high", "stale idle state rejected thinking selection")
  assert(task.current(state).id == task_id, "configuration replaced the running task")
  assert(not state.configuration_error, state.configuration_error)

  controller.set_mode("read")
  await(function() return starts == 3 and state.session.execution_mode == "read" end,
    "Read mode did not resume active work")
  controller.set_mode("write")
  await(function() return starts == 4 and state.session.execution_mode == "write" end,
    "Write mode did not resume active work")
  assert(task.current(state).id == task_id, "permission change replaced the running task")
  await(function()
    local transcript = table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
    return transcript:find("Permission changed to Write", 1, true) ~= nil
  end, "permission notice replaced the task lifecycle heading")

  controller.task_transition({ action = "pause" })
  await(function() return not state.busy and not state.task_operation and not state.state_sync_pending end,
    "pause did not settle")
  assert(task.current(state).status == "paused")
  local paused_applied
  controller.configure({ effort = "low" }, true, function(value) paused_applied = value end)
  await(function() return paused_applied ~= nil and not state.task_operation and not state.state_sync_pending end,
    "paused configuration did not settle")
  assert(paused_applied and state.session.effort == "low")
  assert(starts == 4 and task.current(state).status == "paused", "configuration restarted a paused task")
  local transcript = table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
  assert(not transcript:find("Settings applying", 1, true), transcript)
  assert(not transcript:find("Settings verifying", 1, true), transcript)
  assert(not transcript:find("Settings stopping", 1, true), transcript)
end, debug.traceback)
require("forge.views.harness.workspace").release(state)
if state.presentation then state.presentation.close() end
client.stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.fn.chdir(root)
vim.fn.stdpath = original_stdpath
if not success or not collected then
  io.stderr:write(tostring(failure or "host was not collected"), "\n")
  vim.cmd("cquit 1")
end
print("harness_configuration_host: passed")
vim.cmd("qa!")
