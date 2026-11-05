vim.opt.runtimepath:prepend("nvim")
local client = require("forge.client")
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local snapshot = require("forge.views.harness.snapshot")
local activity = require("forge.views.harness.activity")
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
  state.session = { id = "permission-transition", approval_mode = "ask", active_task = { revision = "task", kind = "goal", phase = "work", status = "running", goal = { objective = "Fixture goal", state = "active" } } }

  state.runtime_epoch, state.snapshot_revision = "permission-owner", 1
  state.activity, state.no_checkpoint = { revision = 1, phase = "working" }, false
  state.execution_notice, state.host_error = nil, nil
  state.cancel_requested = false
  state.cleanup_operation_id, state.cleanup_epoch, state.cleanup_notice = nil, nil, nil
  state.sync_error, state.connection_error = nil, nil
  state.state_sync_pending, state.state_sync_again, state.state_sync_callback = nil, nil, nil
  state.queue, state.pending_steer, state.capability = {}, {}, {}
  state.approval, state.active_elicitation, state.selected_agent_run_id = {}, nil, nil
  state.status = nil
  state.configuring, state.configuration_debounce, state.pending_config = nil, nil, nil
  requests, failures = {}, {}
end
local function configured(revision, mode)
  local committed = vim.deepcopy(state.session)
  committed.approval_mode = mode
  return snapshot.apply(state, { session = committed, runtime_epoch = state.runtime_epoch,
    snapshot_revision = revision, activity = { revision = revision, phase = "working" } })
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
  assert(configured(2, "yolo"))
  if not acknowledged_first then change.reply({ state = "accepted", revision = 1 }) end
  assert(activity.busy(state) and state.session.approval_mode == "yolo",
    "configuration acknowledgement regressed the active exchange or committed settings")
  controller.cancel_turn()
  local cancellation = last("session.turn.cancel")
  assert(cancellation.params.restore_prompt_if_no_output)
  controller.cancel_turn()
  local latest_cancellation = last("session.turn.cancel")
  assert(latest_cancellation.params.operation_id ~= cancellation.params.operation_id,
    "cancellation intents reused an operation identity")
  cancellation.reply({ cancel_requested = true })
  assert(state.cleanup_operation_id == latest_cancellation.params.operation_id and activity.busy(state),
    "old cancellation reply cleared current cleanup or manufactured idle activity")
  latest_cancellation.reply({ cancel_requested = true })
  assert(not state.cleanup_operation_id and activity.busy(state), "cancellation reply manufactured idle activity")
  assert(activity.receive(state, { revision = 3, phase = "idle" }, state.runtime_epoch))
  assert(not activity.busy(state) and #requests == 3 and #failures == 0)
end
reset()
local input_state = require("forge.views.harness.input_state")
input_state.attach(state, function() end)
last("session.input.get").reply({ revision = 10, control_revision = 4, suspended = false, messages = {} })
requests = {}
controller.cancel_turn()
local first_cancel = last("session.turn.cancel")
assert(state.input.control_revision == 4 and not state.input.suspended,
  "cancellation predicted authoritative input state")
input_state.submit(state, "immediate follow-up", {}, "auto")
local first_submit = last("session.input.apply")
assert(first_submit.params.after_control == first_cancel.params.operation_id and first_submit.params.control_revision == 4,
  "immediate submit omitted its pending cancellation or predicted a control revision")
controller.cancel_turn()
local second_cancel = last("session.turn.cancel")
first_cancel.reply({ cancel_requested = true })
assert(state.input.pause_operation_id == second_cancel.params.operation_id
  and state.cleanup_operation_id == second_cancel.params.operation_id,
  "older cancellation reply cleared the newer cancellation correlation")
input_state.submit(state, "latest follow-up", {}, "auto")
local second_submit = last("session.input.apply")
assert(second_submit.params.after_control == second_cancel.params.operation_id and second_submit.params.control_revision == 4,
  "submit did not follow the latest pending cancellation")
assert(#requests == 4 and #failures == 0, "cancellation submission introduced polling or errors")
state.input = nil
reset()
controller.task_transition({ action = "permission", change = { kind = "set_approval", mode = "ask" } })
local previous = last("session.command.apply")
controller.task_transition({ action = "permission", change = { kind = "set_approval", mode = "ask" } })
local latest = last("session.command.apply")
assert(previous.params.operation_id ~= latest.params.operation_id)
assert(configured(3, "deny"))
previous.reply({ state = "completed" })
assert(not configured(2, "ask") and state.session.approval_mode == "deny" and activity.busy(state),
  "stale configuration replaced the committed settings or active exchange")
latest.reply(nil, "cleanup did not settle", { code = "request_failed" })
assert(activity.busy(state) and state.execution_notice == "cleanup did not settle" and #failures == 1)
assert(#requests == 2, "command replies initiated view-owned recovery")
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
state.host_error, state.execution_notice = "host stopped", "Reopen Harness"
state.queue = { { text = "keep queued", task_id = "task" } }
controller.cancel_turn()
controller.render()
assert(#requests == 0 and #state.queue == 1 and not activity.busy(state))
print("harness_mode_restart: passed")
vim.cmd("qa!")
