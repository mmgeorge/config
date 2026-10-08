vim.opt.runtimepath:prepend("nvim")
vim.loader.enable(false)
local root = vim.fn.getcwd()
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
assert(vim.fn.mkdir(workspace, "p") == 1 and vim.fn.mkdir(data, "p") == 1)
local executable = require("forge.builder").binary_path()
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return (kind == "data" or kind == "config") and data or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  callback({ ok = true, path = executable })
  return function() end
end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local errors, operation, starts = {}, {}, 0
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
client.subscribe(function(event, payload)
  if event == "task_operation" then operation[payload.id] = payload end
  if event == "backend_event" and payload.kind == "execution_state" then starts = starts + 1 end
end)
local function await(predicate, message)
  assert(vim.wait(10000, predicate, 10), message .. "\n" .. table.concat(errors, "\n") .. "\n" .. vim.inspect(operation))
end
local function request(session_id, method, params)
  local received, result, failure
  client.request_for(session_id, method, params, function(value, problem)
    result, failure, received = value, problem, true
  end)
  await(function() return received end, method .. " stalled")
  assert(not failure, failure)
  return result
end
local function open(session_id)
  local opened, failure
  client.start_harness(function(result, problem) opened, failure = result, problem end, { session_id = session_id })
  await(function() return opened or failure end, "host initialization stalled")
  assert(not failure, failure)
  return opened
end
local success, failure = xpcall(function()
  vim.fn.chdir(workspace)
  require("forge").setup({ harness = { backend = "mock", backends = { mock = { command = { "blocking" } } } } })
  local initial = open()
  local session_id = initial.session.id
  local acknowledgement = request(session_id, "task.transition", { operation_id = "plan-before-stop", action = "plan", text = "Plan until interrupted" })
  assert(acknowledgement.state == "accepted")
  await(function() return starts == 1 end, "planning did not start")
  assert(request(session_id, "health.get", {}).responsive)
  request(session_id, "task.transition", { operation_id = "pause-once", action = "pause" })
  await(function() return operation["pause-once"] and operation["pause-once"].state == "completed" end, "pause did not collect planning")
  local paused = request(session_id, "state.get", {})
  assert(paused.task[1].status == "paused" and #paused.exchange == 1)
  assert(paused.artifact[1].state == "generating", "interrupt incorrectly failed the draft")
  local resumed_prompt
  client.request_for(session_id, "prompt.submit", { text = "continue planning without questions" }, function(_, problem) resumed_prompt = problem or true end)
  await(function() return starts == 2 or resumed_prompt end, "message did not resume planning")
  assert(starts == 2, tostring(resumed_prompt))
  request(session_id, "task.transition", { operation_id = "pause-message", action = "pause" })
  await(function() return operation["pause-message"] and operation["pause-message"].state == "completed" end, "resumed planning did not stop")
  local resumed = request(session_id, "state.get", {})
  assert(resumed.task[1].id == paused.task[1].id)
  assert(resumed.artifact[1].state == "generating", "resumed draft lost editable state")
  assert(resumed.exchange[2].plan_id == paused.task[1].plan_id and resumed.exchange[2].prompt == "continue planning without questions",
    "message did not retain the interrupted planning context")
  request(session_id, "task.transition", { operation_id = "goal-before-kill", action = "goal", text = "Retain interrupted goal" })
  await(function() return starts == 3 end, "goal did not start")
  client.request_for(session_id, "task.transition", { operation_id = "pause-before-settings", action = "pause" }, function(_, problem) assert(not problem, problem) end)
  request(session_id, "task.transition", { operation_id = "settings-after-pause", action = "permission", mode = "full" })
  await(function() return operation["settings-after-pause"] and operation["settings-after-pause"].state == "completed" end, "settings did not collect the pause")
  local changed = request(session_id, "state.get", {})
  assert(changed.task[1].status == "paused" and changed.session.execution_mode == "full" and starts == 3,
    "settings after pause restarted execution")
  request(session_id, "task.transition", { operation_id = "resume-before-kill", action = "resume" })
  await(function() return starts == 4 end, "explicit resume did not start")
  client._client.process:kill(9)
  await(function() return client._client.process == nil end, "killed host retained ownership")
  local recovered = open(session_id)
  assert(recovered.runtime_epoch ~= initial.runtime_epoch)
  assert(#recovered.task == 2 and #recovered.exchange == 4)
  assert(recovered.task[1].status == "paused", vim.inspect(recovered.task))
  assert(recovered.task_operation.state == "outcome_unknown")
  assert(request(session_id, "task.operation", { operation_id = "resume-before-kill" }).state == "outcome_unknown")
  local repeated = request(session_id, "task.transition", { operation_id = "resume-before-kill", action = "resume" })
  assert(repeated.state == "outcome_unknown", "uncertain operation was replayed")
  vim.wait(100, function() return false end)
  local inspected = request(session_id, "state.get", {})
  assert(#inspected.exchange == 4 and starts == 4, "restart automatically dispatched work")
  local recovered_prompt
  client.request_for(session_id, "prompt.submit", { text = "continue the saved goal" }, function(_, problem) recovered_prompt = problem or true end)
  await(function() return starts == 5 or recovered_prompt end, "message did not resume recovered goal")
  assert(starts == 5, tostring(recovered_prompt))
  request(session_id, "task.transition", { operation_id = "pause-recovered-message", action = "pause" })
  await(function() return operation["pause-recovered-message"] and operation["pause-recovered-message"].state == "completed" end, "recovered goal did not stop")
  local final = request(session_id, "state.get", {})
  assert(final.task[1].id == recovered.task[1].id and final.exchange[5].goal_id == recovered.task[1].goal_id)
  assert(recovered.exchange[4].prompt == "" and recovered.exchange[4].lifecycle == "Goal resumed",
    "restart lost the control-resume lifecycle label")
  request(session_id, "task.transition", { operation_id = "resume-for-permission", action = "resume" })
  await(function() return starts == 6 end, "goal did not resume before permission change")
  request(session_id, "task.transition", { operation_id = "permission-while-running", action = "permission", mode = "yolo" })
  await(function() return starts == 7 end, "permission change did not resume goal")
  request(session_id, "task.transition", { operation_id = "pause-after-permission", action = "pause" })
  await(function() return operation["pause-after-permission"] and operation["pause-after-permission"].state == "completed" end, "changed goal did not stop")
  local changed_goal = request(session_id, "state.get", {})
  assert(changed_goal.task[1].id == recovered.task[1].id)
  assert(changed_goal.exchange[7].prompt == "" and changed_goal.exchange[7].lifecycle == "Permission changed to YOLO · Goal resumed",
    vim.inspect(changed_goal.exchange[7]))
  request(session_id, "task.transition", { operation_id = "resume-planning-control", action = "resume", task_id = paused.task[1].id })
  await(function() return starts == 8 end, "planning control resume did not start")
  request(session_id, "task.transition", { operation_id = "pause-planning-control", action = "pause" })
  await(function() return operation["pause-planning-control"] and operation["pause-planning-control"].state == "completed" end, "planning control did not stop")
  local planning = request(session_id, "state.get", {})
  assert(planning.exchange[8].prompt == "" and planning.exchange[8].lifecycle == "Planning resumed")
  assert(planning.exchange[8].plan_id == paused.task[1].plan_id)
end, debug.traceback)
client.stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.fn.chdir(root)
vim.fn.stdpath = original_stdpath
assert(success and collected, failure or "host was not collected")
print("harness_task_recovery_host: passed")
vim.cmd("qa!")
