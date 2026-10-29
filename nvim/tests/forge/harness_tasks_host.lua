vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
vim.loader.enable(false)
local root = vim.fn.getcwd()
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
assert(vim.fn.mkdir(workspace, "p") == 1 and vim.fn.mkdir(data, "p") == 1)
vim.fn.writefile({vim.json.encode({checks=vim.empty_dict()})},workspace .. "/.forge.json")
local executable = vim.fn.fnamemodify(require("forge.builder").binary_path(), ":p")
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return (kind == "data" or kind == "config") and data or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  callback({ ok = true, path = executable })
  return function() end
end }
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local errors = {}
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local task = require("forge.views.harness.task")
local function latest_exchange(matches)
  local snapshot, failure
  client.request({ method = "session.exchange.list", params = {}, session_id = assert(require("forge.session").harness.session).id }, function(result, problem) snapshot, failure = result, problem end)
  assert(vim.wait(5000, function() return snapshot or failure end, 10), "snapshot stalled")
  assert(not failure, failure)
  for index = #snapshot, 1, -1 do
    if not matches or matches(snapshot[index]) then return snapshot[index] end
  end
end
local function wait_settled()
  assert(vim.wait(15000, function()
    local input_idle = not state.input or (#state.input.pending == 0 and #state.input.confirmed == 0)
    return #errors > 0 or (input_idle and not state.busy and not state.task_operation and not state.state_sync_pending)
  end, 10), "task did not settle: " .. vim.inspect(task.current(state)))
  assert(#errors == 0, table.concat(errors, "\n"))
end
local function raw_request(method, params)
  local received, result, failure
  client.request({ session_id = state.session.id, method = method, params = params }, function(value, problem)
    result, failure, received = value, problem, true
  end)
  assert(vim.wait(5000, function() return received end, 10), method .. " did not complete")
  assert(not failure, failure)
  return result
end
local success, failure = xpcall(function()
  vim.fn.chdir(workspace)
  require("forge").setup({ harness = { backend = "mock" } })
  require("forge").open_harness()
  assert(vim.wait(10000, function() return state.ready and state.session ~= nil end, 10), "Harness did not open")
  for _, selection in ipairs({ { "/fast", "fast" }, { "/ultrafast", "ultrafast" }, { "/ultrafast", "default" } }) do
    vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { selection[1] })
    controller.submit()
    assert(vim.wait(5000, function()
      return state.session.service_tier == selection[2] and not state.configuring and not state.configuration_debounce
    end, 10), "service-tier configuration did not settle: " .. selection[1])
    assert(not latest_exchange(), "service-tier command reached the model as a prompt")
  end
  local mock = dofile(root .. "/nvim/tests/forge/support/mock_provider.lua")
  local held = mock.hold(state.session.id)
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "Inspect the canonical command boundary" })
  controller.submit()
  held:wait()
  local original = latest_exchange()
  assert(original.state == "running")
  assert(raw_request("session.prompt.submit", { operation_id = "raw-host-goal", text = "/goal Verify canonical raw routing" }).state == "completed")
  assert(vim.wait(5000, function()
    local current = task.current(state)
    return current and current.kind == "goal" and current.entry and current.entry.delivery == "accepted"
  end, 10), "raw goal was not acknowledged by the held provider")
  assert(latest_exchange().id == original.id and latest_exchange().state == "running")
  assert(vim.wait(5000, function()
    return table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
      :find("◇ Goal started: Verify canonical raw routing", 1, true)
  end, 10), "raw goal did not use the canonical task-start presentation")
  local rendered = table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
  assert(not rendered:find("Steering: /goal", 1, true), "raw command became literal steering")
  assert(raw_request("session.goal.pause", { operation_id = "raw-host-pause" }).state == "completed")
  assert(vim.wait(5000, function() return task.status(task.current(state)) == "paused" end, 10))
  assert(raw_request("session.goal.resume", { operation_id = "raw-host-resume" }).state == "completed")
  assert(vim.wait(5000, function()
    local current = task.current(state)
    return current and task.status(current) == "running" and current.entry.delivery == "accepted"
  end, 10))
  assert(raw_request("session.goal.clear", { operation_id = "raw-host-clear" }).state == "completed")
  assert(vim.wait(5000, function() return task.current(state) == nil end, 10))
  assert(latest_exchange().id == original.id and latest_exchange().state == "running", "raw control cancelled the held provider")
  held:release()
  wait_settled()
  assert(latest_exchange().id == original.id and latest_exchange().state == "complete")
  controller.toggle_write()
  wait_settled()
  assert(state.session.access.write_access == "off")
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "/plan Demonstrate a replaceable task slot" })
  controller.submit()
  wait_settled()
  local plan_task = assert(task.current(state))
  assert(plan_task.kind == "plan" and task.status(plan_task) == "waiting", vim.inspect(plan_task))
  assert(state.session.approval_mode == "ask")
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "", "admitted task did not clear composer")
  for _, approval in ipairs({ "deny", "allow", "ask" }) do
    controller.task_transition({ action = "permission", change = { kind = "set_approval", mode = approval } })
    wait_settled()
    assert(task.current(state).revision == plan_task.revision and task.status(task.current(state)) == "waiting",
      "permission change dismissed review wait")
    assert(state.active_plan.id == plan_task.plan.artifact.id and state.session.approval_mode == approval)
    assert(state.session.access.write_access == "off", "review approval restored writes")
  end
  controller.task_transition({ action = "goal", text = "Exercise bounded goal continuation" })
  wait_settled()
  assert(task.current(state).kind == "goal")
  assert(state.session.approval_mode == "ask" and state.session.access.write_access == "off", "new goal widened access")
  controller.task_transition({ action = "attach_plan", plan_id = plan_task.plan.artifact.id })
  wait_settled()
  assert(task.current(state).kind == "plan" and task.status(task.current(state)) == "waiting")
  assert(task.current(state).revision ~= plan_task.revision, "reattaching reused a former task contract")
  assert(state.active_plan.id == plan_task.plan.artifact.id, "reattaching lost the saved artifact")
  controller.task_transition({ action = "clear" })
  wait_settled()
  assert(not task.current(state), "clear retained current task")
  task.open(state, nil, function() error("opening saved artifacts submitted a task") end)
  assert(vim.wait(5000, function() return require("forge.views.picker").is_open("harness-task") end, 10))
  local artifact_picker = require("forge.views.picker")._state_for_test()
  assert(#artifact_picker.spec.page_list[1].option_list > 0, "clearing a task removed its saved artifact")
  require("forge.views.picker").close()
  controller.task_transition({ action = "plan", text = "Execute a reviewed declaration design" })
  wait_settled()
  local executing_plan = task.current(state).plan.artifact.id
  controller.task_transition({ action = "execute", plan_id = "last" })
  wait_settled()
  assert(task.current(state).execution.record.plan_id == executing_plan and task.current(state).kind == "execute",
    "acceptance lost the saved artifact")
  assert(task.status(task.current(state)) == "blocked" and task.phase(task.current(state)) == "implement",
    "read-only mock execution did not stop at its no-progress boundary: " .. vim.inspect(task.current(state)))
  assert(state.session.approval_mode == "ask" and state.session.access.write_access == "off", "execution widened access")
  local tab_count, conversation = #vim.api.nvim_list_tabpages(), state.session.id
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "/task new" })
  controller.submit()
  assert(require("forge.views.picker").is_open("harness-task"), "new task did not open the task picker")
  assert(#vim.api.nvim_list_tabpages() == tab_count and state.session.id == conversation, "new task changed the conversation")
  assert(task.current(state).execution.record.plan_id == executing_plan, "opening new task picker detached execution")
  local new_picker = require("forge.views.picker")._state_for_test()
  new_picker.spec.on_confirm({ option = new_picker.spec.page_list[1].option_list[1] })
  require("forge.views.picker").close(true)
  vim.wait(20, function() return false end)
  assert(vim.api.nvim_get_current_buf() == state.composer_buf)
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "/plan ")
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "" })
  controller.task_transition({ action = "resume" })
  wait_settled()
  assert(task.current(state).execution.record.plan_id == executing_plan, "resuming replaced the accepted artifact")
  local continuation = latest_exchange(function(exchange)
    return exchange.origin and exchange.origin.kind == "task_continuation"
  end)
  assert(continuation and #continuation.prompt > 0, "automatic continuation omitted its actual instruction")
  assert(continuation.execution_id == nil and continuation.goal_id == nil and continuation.kind == nil,
    "transcript retained task routing authority")

end, debug.traceback)
client.host_stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.fn.chdir(root)
vim.fn.stdpath = original_stdpath
assert(success and collected, failure or "Harness host was not collected")
print("harness_tasks_host: passed")
vim.cmd("qa!")
