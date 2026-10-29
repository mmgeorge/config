vim.loader.enable(false)
local client = require("forge.client")
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local input = require("forge.views.harness.input_state")
local host = dofile("nvim/tests/forge/support/input.lua").new(state)
local controls, transitions = {}, {}
require("forge.views.harness.prompt_history").record = function() end
controller.render, controller.refresh_winbar = function() end, function() end
state.composer_buf = vim.api.nvim_create_buf(false, true)
state.session = { id = "steering-task" }
state.presentation = { capture_prompt = function()
  return { buffer = state.composer_buf, document = "composer", is_current = function() return true end }
end }
controller.task_transition = function(action, _, completed)
  transitions[#transitions + 1] = { action = action, completed = completed }
end
client.request = function(request, callback)
  assert(request.session_id == "steering-task", "submission changed its session owner")
  if host.request(request.method, request.params, callback) then return end
  controls[#controls + 1] = { request = request, callback = callback }
end
input.attach(state, function() end)
local function draft(text)
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { text })
end
local function text() return vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] end
local function submitted(message, intent, submit)
  local before = #host.requests
  draft(message)
  submit()
  assert(#host.requests == before + 1, "composer message was not admitted")
  local request = host.requests[#host.requests].parameters.action
  assert(request.kind == "submit" and request.message.text == message and request.message.intent == intent)
  host.flush()
end

for _, scenario in ipairs({
  { busy = true, native_steer = true },
  { busy = true, native_steer = false },
  { busy = false, configuring = true },
  { busy = true, task_operation = { action = "configure", state = "accepted" } },
  { busy = true, status = { kind = "waiting_for_agent", agent_count = 2 } },
}) do
  state.busy, state.configuring = scenario.busy, scenario.configuring
  state.task_operation, state.status = scenario.task_operation, scenario.status
  state.capability = { native_steer = scenario.native_steer }
  submitted("Follow up through server admission", "auto", controller.submit)
  submitted("Explicit follow-up", "auto", controller.steer_submit)
  submitted("Keep this queued", "queue", controller.queue_submit)
end
state.task_operation = nil
for _, command in ipairs({ { "/plan Convert to X", "plan", "Convert to X" },
  { "/goal Complete X", "goal", "Complete X" } }) do
  local before = #host.requests
  draft(command[1])
  controller.submit()
  local transition = transitions[#transitions].action
  assert(transition.action == command[2] and transition.text == command[3])
  assert(#host.requests == before, "task command became a provider steering message")
end

state.pending_steer = { { text = "Pending steering" } }
local retained = vim.deepcopy(state.queue)
local before = #host.requests
draft("Keep my unsent draft")
controller.cancel_turn()
local pause = transitions[#transitions]
assert(pause.action.action == "pause" and text() == "Keep my unsent draft")
assert(#host.requests == before and vim.deep_equal(state.queue, retained))
state.task_operation = { action = "pause", state = "accepted" }
local transition_count = #transitions
controller.cancel_turn()
assert(#transitions == transition_count, "duplicate cancellation replaced the pending pause")
state.task_operation = nil
pause.completed(true)
local cancellation = controls[#controls]
assert(cancellation.request.method == "session.turn.cancel"
  and cancellation.request.params.restore_prompt_if_no_output == true)
controller.cancel_turn()
assert(#transitions == transition_count, "duplicate cancellation was submitted during cleanup")
cancellation.callback({ cancel_requested = true })
assert(text() == "Keep my unsent draft" and vim.deep_equal(state.queue, retained))

local child_action
controller.agent_action = function(action, child_id) child_action = { action = action, id = child_id } end
state.selected_agent_run_id = "selected-child"
controller.cancel_turn()
assert(child_action.action == "interrupt" and child_action.id == "selected-child")
assert(#transitions == transition_count and #host.requests == before and text() == "Keep my unsent draft")
print("harness_submit_steering: passed")
