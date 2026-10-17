vim.opt.runtimepath:prepend("nvim")
local client = require("forge.client")
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local task = require("forge.views.harness.task")
local requests, failures = {}, {}
client.request_for = function(session_id, method, params, reply)
  requests[#requests + 1] = { session_id = session_id, method = method, params = params, reply = reply }
end
require("forge.infra.notifications").error = function(message) failures[#failures + 1] = message end
controller.render, controller.refresh_winbar, controller.attach_transcript = function() end, function() end, function() end
vim.defer_fn = function() end
local function reset()
  if state.working_timer then state.working_timer:stop() state.working_timer:close() end
  state.working_timer, state.working_started_ns = nil, nil
  state.session = { id = "permission-transition", execution_mode = "write", current_task_id = "task" }
  state.task = { { id = "task", kind = "goal", phase = "continue", status = "running" } }
  state.busy, state.no_checkpoint = true, false
  state.execution_notice, state.host_error, state.task_operation = nil, nil, nil
  state.sync_error, state.connection_error = nil, nil
  state.state_sync_pending, state.state_sync_again, state.state_sync_callback = nil, nil, nil
  state.queue, state.pending_steer, state.capability = {}, {}, {}
  state.approval, state.active_elicitation, state.selected_agent_run_id = {}, nil, nil
  state.goal, state.goal_execution, state.status = nil, nil, nil
  state.configuring, state.configuration_debounce, state.pending_config = nil, nil, nil
  requests, failures = {}, {}
end
local function last(method)
  local request = requests[#requests]
  assert(request and request.method == method, vim.inspect(request))
  return request
end
for _, acknowledged_first in ipairs({ true, false }) do
  reset()
  controller.set_mode("yolo")
  local change = last("task.transition")
  assert(change.params.action == "permission" and change.params.mode == "yolo" and not change.params.target)
  if acknowledged_first then change.reply({ state = "accepted" }) end
  task.receive(state, { id = change.params.operation_id, state = "running" })
  if not acknowledged_first then change.reply({ state = "accepted" }) end
  assert(state.task_operation.state == "running", "late acknowledgement regressed execution")
  controller.cancel_turn()
  local pause = last("task.transition")
  assert(pause.params.action == "pause")
  controller.cancel_turn()
  assert(last("task.transition") == pause, "duplicate stop intent")
  task.receive(state, { id = change.params.operation_id, state = "completed" })
  assert(state.busy and state.task_operation.id == pause.params.operation_id)
  task.receive(state, { id = pause.params.operation_id, state = "completed" })
  assert(not state.busy and not state.task_operation)
  assert(last("state.get") and #failures == 0)
end
reset()
controller.set_mode("write")
local previous = last("task.transition")
controller.set_mode("read")
local latest = last("task.transition")
assert(previous.params.operation_id ~= latest.params.operation_id)
task.receive(state, { id = previous.params.operation_id, state = "superseded" })
assert(state.busy and state.task_operation.id == latest.params.operation_id)
task.receive(state, { id = latest.params.operation_id, state = "failed", error = "cleanup did not settle" })
assert(not state.busy and state.execution_notice == "cleanup did not settle" and #failures == 1)
assert(last("state.get"))
reset()
state.session.execution_mode = "read"
state.approval = { { id = "write-request", required_mode = "write", item_list = {} } }
local approval = require("forge.views.harness.approval")
local original_open, approval_host = approval.open, nil
approval.open = function(_, host) approval_host = host end
controller.present_approval()
approval.open = original_open
local mode_completed
approval_host.switch_mode("write", function(changed) mode_completed = changed end)
local mode_operation = last("task.transition")
assert(mode_operation.params.action == "permission" and mode_operation.params.mode == "write")
assert(mode_completed == nil, "approval mode choice must wait for the task transition")
mode_operation.reply({ state = "accepted" })
task.receive(state, { id = mode_operation.params.operation_id, state = "completed" })
assert(mode_completed == true)
state.approval = {}
local request_count = #requests
approval_host.switch_mode("write", function(changed) assert(changed == false) end)
assert(#requests == request_count, "stale approval changed the task mode")
reset()
state.host_error, state.execution_notice, state.busy = "host stopped", "Reopen Harness", false
state.queue = { { text = "keep queued", task_id = "task" } }
controller.cancel_turn()
controller.drain()
assert(#requests == 0 and #state.queue == 1 and not state.busy)
print("harness_mode_restart: passed")
vim.cmd("qa!")
