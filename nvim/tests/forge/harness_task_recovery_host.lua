vim.opt.runtimepath:prepend("nvim")
vim.loader.enable(false)
local root = vim.fn.getcwd()
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
assert(vim.fn.mkdir(workspace, "p") == 1 and vim.fn.mkdir(data, "p") == 1)
local executable = vim.fn.fnamemodify(require("forge.builder").binary_path(), ":p")
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return (kind == "data" or kind == "config") and data or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  callback({ ok = true, path = executable })
  return function() end
end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local errors = {}
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
local function await(predicate, message)
  assert(vim.wait(15000, predicate, 10), message .. "\n" .. table.concat(errors, "\n"))
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
local function submit(session_id, id, text)
  local input = request(session_id, "input.get", {})
  local result = request(session_id, "input.apply", {
    control_revision = input.control_revision,
    id = id, action = { kind = "submit", message = {
      id = id, text = text, intent = "auto", attachments = {},
    } },
  })
  assert(result.error == nil or result.error == vim.NIL, vim.inspect(result.error))
  return result
end
local function transition(session_id, id, action)
  action.operation_id = id
  local receipt = request(session_id, "task.transition", action)
  assert(receipt.state == "completed", "state mutation did not complete at admission: " .. vim.inspect(receipt))
  return receipt
end
local function await_snapshot(session_id, predicate, message)
  local current, failure, pending
  await(function()
    if failure then error(failure) end
    if current and predicate(current) then return true end
    if not pending then
      pending = true
      client.request_for(session_id, "state.get", {}, function(value, problem)
        current, failure, pending = value, problem, false
      end)
    end
    return false
  end, message)
  return current
end
local function empty_task(snapshot)
  return snapshot.session.active_task == nil or snapshot.session.active_task == vim.NIL
end
local function stop_host()
  assert(client._client.process, "host process is missing")
  client._client.process:kill(9)
  await(function() return client._client.process == nil end, "killed host retained ownership")
end
local success, failure = xpcall(function()
  vim.fn.writefile({ vim.json.encode({ checks = vim.empty_dict() }) }, workspace .. "/.forge.json")
  vim.fn.chdir(workspace)
  require("forge").setup({ harness = { backend = "mock" } })
  local initial = open()
  local session_id = initial.session.id
  local mock = dofile(root .. "/nvim/tests/forge/support/mock_provider.lua")

  local held = mock.hold(session_id)
  submit(session_id, "original-input", "Inspect startup")
  held:wait()
  local original = request(session_id, "exchange.list", {})[1]
  assert(original and original.state == "running")
  transition(session_id, "goal-start", { action = "goal", text = "Finish startup work" })
  await_snapshot(session_id, function(snapshot)
    local task = snapshot.session.active_task
    return task and task ~= vim.NIL and task.kind == "goal" and task.entry
      and task.entry ~= vim.NIL and task.entry.delivery == "accepted"
  end, "goal entry was not acknowledged during the held turn")
  transition(session_id, "goal-pause", { action = "pause" })
  local paused = request(session_id, "state.get", {})
  assert(paused.session.active_task.goal.state == "paused")
  assert(request(session_id, "exchange.list", {})[1].state == "running", "pause cancelled the active provider turn")
  transition(session_id, "goal-clear", { action = "clear" })
  assert(empty_task(request(session_id, "state.get", {})))
  local cleared_exchange = request(session_id, "exchange.list", {})
  assert(#cleared_exchange == 1 and cleared_exchange[1].id == original.id
    and cleared_exchange[1].state == "running", "clear replaced or cancelled the active exchange")
  held:release()
  await_snapshot(session_id, function(snapshot) return empty_task(snapshot) and snapshot.status.kind == "idle" end,
    "cleared task continued after the original response")
  local completed = request(session_id, "exchange.list", {})
  assert(#completed == 1 and completed[1].id == original.id and completed[1].state == "complete")

  local interrupted_hold = mock.hold(session_id)
  submit(session_id, "before-restart-input", "Inspect the pending plan boundary")
  interrupted_hold:wait()
  local interrupted = request(session_id, "exchange.list", {})[2]
  assert(interrupted and interrupted.state == "running")
  transition(session_id, "pending-plan", { action = "plan", text = "Create the recovery design" })
  local pending = request(session_id, "state.get", {}).session.active_task
  assert(pending.kind == "plan" and pending.entry.delivery == "pending",
    "plan entry did not wait for the existing turn boundary")
  local entry_id, plan_id = pending.entry.id, pending.plan.artifact.id
  assert(#request(session_id, "exchange.list", {}) == 2, "pending plan created a competing exchange")
  stop_host()

  local restarted = open(session_id)
  assert(restarted.runtime_epoch ~= initial.runtime_epoch)
  for _, id in ipairs({ "goal-start", "goal-pause", "goal-clear", "pending-plan" }) do
    assert(request(session_id, "task.operation", { operation_id = id }).state == "completed",
      "restart changed a completed receipt: " .. id)
  end
  local recovered = await_snapshot(session_id, function(snapshot)
    local task = snapshot.session.active_task
    return task and task ~= vim.NIL and task.kind == "plan" and task.entry.delivery == "accepted"
      and task.plan.artifact.state == "awaiting_review"
  end, "pending plan entry was not replayed after restart")
  assert(recovered.session.active_task.entry.id == entry_id)
  assert(recovered.session.active_task.plan.artifact.id == plan_id)
  local history = request(session_id, "exchange.list", {})
  assert(#history == 3 and history[1].id == original.id and history[2].id == interrupted.id,
    "restart replaced prior exchanges or delivered the plan entry more than once: " .. vim.inspect(history))
  assert(history[3].origin.kind == "task_started" and history[3].origin.start.kind == "plan")
  assert(history[3].prompt:find("Create the recovery design", 1, true))
  local plan_exchange_id = history[3].id

  transition(session_id, "pending-plan", { action = "plan", text = "Create the recovery design" })
  transition(session_id, "goal-clear", { action = "clear" })
  local deduplicated = request(session_id, "state.get", {})
  assert(deduplicated.session.active_task.plan.artifact.id == plan_id,
    "replaying an old clear receipt cleared the replacement task")
  assert(deduplicated.session.active_task.entry.id == entry_id)
  assert(#request(session_id, "exchange.list", {}) == 3, "receipt replay submitted another provider exchange")
  stop_host()

  local settled = open(session_id)
  assert(settled.runtime_epoch ~= restarted.runtime_epoch)
  assert(settled.session.active_task.plan.artifact.id == plan_id)
  assert(settled.session.active_task.entry.id == entry_id and settled.session.active_task.entry.delivery == "accepted")
  local final = request(session_id, "exchange.list", {})
  assert(#final == 3 and final[3].id == plan_exchange_id, "accepted entry replayed on the next restart")
  assert(request(session_id, "task.operation", { operation_id = "pending-plan" }).state == "completed")
end, debug.traceback)
client.stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.fn.chdir(root)
vim.fn.stdpath = original_stdpath
assert(success and collected, failure or "host was not collected")
print("harness_task_recovery_host: passed")
vim.cmd("qa!")
