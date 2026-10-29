vim.opt.runtimepath:prepend("nvim")
local client = require("forge.client")
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local task = require("forge.views.harness.task")
local requests, failures = {}, {}
client.request = function(request_specification, reply)
  local session_id = request_specification.session_id
  local method = request_specification.method
  local params = request_specification.params or {}

  requests[#requests + 1] = { session_id = session_id, method = method, params = params, reply = reply }
end
require("forge.infra.notifications").error = function(message) failures[#failures + 1] = message end
controller.render, controller.refresh_winbar, controller.attach_transcript = function() end, function() end, function() end
vim.defer_fn = function() end
local function reset()
  if state.working_timer then state.working_timer:stop() state.working_timer:close() end
  state.working_timer, state.working_started_ns = nil, nil
  state.session = { id = "permission-transition", approval_mode = "ask", active_task = { revision = "task", kind = "goal", goal = { objective = "Fixture goal", state = "active" } } }

  state.busy, state.no_checkpoint = true, false
  state.execution_notice, state.host_error, state.task_operation = nil, nil, nil
  state.cancel_requested = false
  state.task_operation_scope, state.task_operation_by_id = nil, nil
  state.sync_error, state.connection_error = nil, nil
  state.state_sync_pending, state.state_sync_again, state.state_sync_callback = nil, nil, nil
  state.queue, state.pending_steer, state.capability = {}, {}, {}
  state.approval, state.active_elicitation, state.selected_agent_run_id = {}, nil, nil
  state.status = nil
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
  controller.task_transition({ action = "permission", change = { kind = "enable_yolo" } })
  local change = last("session.command.apply")
  assert(change.params.action == "permission" and change.params.change.kind == "enable_yolo" and not change.params.target)
  if acknowledged_first then change.reply({ state = "accepted", revision = 1 }) end
  task.receive(state, { id = change.params.operation_id, state = "running", revision = 2 })
  if not acknowledged_first then change.reply({ state = "accepted", revision = 1 }) end
  assert(state.task_operation.state == "running", "late acknowledgement regressed execution")
  controller.cancel_turn()
  local pause = last("session.command.apply")
  assert(pause.params.action == "pause")
  controller.cancel_turn()
  assert(last("session.command.apply") == pause, "duplicate stop intent")
  task.receive(state, { id = change.params.operation_id, state = "completed" })
  assert(state.busy and state.task_operation.id == pause.params.operation_id)
  task.receive(state, { id = pause.params.operation_id, state = "completed" })
  assert(not state.busy and not state.task_operation)
  local cancellation = requests[#requests - 1]
  assert(cancellation.method == "session.turn.cancel" and cancellation.params.restore_prompt_if_no_output)
  cancellation.reply({ cancel_requested = true })
  assert(last("session.state.get") and #failures == 0)
end
reset()
controller.task_transition({ action = "permission", change = { kind = "set_approval", mode = "ask" } })
local previous = last("session.command.apply")
controller.task_transition({ action = "permission", change = { kind = "set_approval", mode = "ask" } })
local latest = last("session.command.apply")
assert(previous.params.operation_id ~= latest.params.operation_id)
task.receive(state, { id = previous.params.operation_id, state = "superseded" })
assert(state.busy and state.task_operation.id == latest.params.operation_id)
task.receive(state, { id = latest.params.operation_id, state = "failed", error = "cleanup did not settle" })
assert(not state.busy and state.execution_notice == "cleanup did not settle" and #failures == 1)
assert(last("session.state.get"))
for _, mode in ipairs({ "n", "i" }) do
  reset()
  local buffer = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_set_current_buf(buffer)
  require("forge.shared.keymaps").setup_view_keymaps(buffer, "harness", controller.composer_command_set())
  local mapping = vim.fn.maparg("<S-Tab>", mode, false, true)
  assert(mapping.callback, "Shift-Tab is missing in " .. mode)
  mapping.callback()
  assert(last("session.command.apply").params.change.kind == "toggle_write_enabled")
  assert(state.session.approval_mode == "ask", "Shift-Tab changed unconfirmed approval state")
  vim.api.nvim_buf_delete(buffer, { force = true })
end
reset()
state.host_error, state.execution_notice, state.busy = "host stopped", "Reopen Harness", false
state.queue = { { text = "keep queued", task_id = "task" } }
controller.cancel_turn()
controller.render()
assert(#requests == 0 and #state.queue == 1 and not state.busy)
print("harness_mode_restart: passed")
vim.cmd("qa!")
