vim.loader.enable(false)
local client = require("forge.client")
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
require("forge.views.harness.prompt_history").record = function() end
require("forge.infra.notifications").warn = function() end
require("forge.infra.notifications").info = function() end
controller.refresh_winbar = function() end
controller.drain = function() end
state.composer_buf = vim.api.nvim_create_buf(false, true)
state.busy = true
state.session = { id = "steering-task" }
state.capability = { native_steer = true }
state.queue, state.pending_steer = {}, {}
local calls, reply = {}, nil
local transition
controller.task_transition = function(action) transition = action end
client.request = function(method, params, callback)
  calls[#calls + 1] = { method = method, params = params }
  reply = callback
end
local function draft(text) vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { text }) end
local function text() return vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] end
state.selected_agent_run_id = nil
state.active_wait = nil
draft("correct the running main turn")
controller.submit()
assert(#calls == 1 and calls[1].method == "turn.steer", "running main turn without a wait was queued")
assert(calls[1].params.target == nil and #state.queue == 0)
reply({})
draft("/plan convert to X")
controller.submit()
assert(#calls == 1 and transition.action == "plan" and transition.text == "convert to X", "active /plan did not enter the coordinator")
state.queue = {}
state.selected_agent_run_id = "child"
draft("/plan convert child to X")
controller.submit()
assert(#calls == 1 and transition.text == "convert child to X", "selected child intercepted task creation")
state.selected_agent_run_id = nil
state.queue = {}
draft("please convert to X")
controller.submit()
assert(#calls == 2 and calls[2].method == "turn.steer", "ordinary prompt did not steer")
reply({})
draft("explicit follow-up")
controller.queue_submit()
assert(#calls == 2 and state.queue[1].text == "explicit follow-up", "queue submission steered")
state.queue = {}
state.busy, state.configuring = false, true
draft("after configuration")
controller.submit()
assert(#calls == 2 and state.queue[1].text == "after configuration", "configuration-only state attempted steering")
state.busy, state.configuring = true, false
state.capability.native_steer = false
state.queue = {}
draft("unsupported backend")
controller.submit()
assert(#calls == 2 and state.queue[1].text == "unsupported backend", "unsupported backend attempted steering")
state.queue = {}
state.status = { kind = "waiting_for_agent", agent_count = 2 }
draft("resume while the children continue")
controller.submit()
assert(#calls == 3 and calls[3].method == "turn.steer", "Harness wait incorrectly required native steering")
assert(calls[3].params.text == "resume while the children continue" and #state.queue == 0)
reply({})
draft("explicit steer during Harness wait")
controller.steer_submit()
assert(#calls == 4 and calls[4].method == "turn.steer", "explicit steer rejected Harness wait")
reply({})
state.status = { kind = "working" }
draft("queue during unsupported active generation")
controller.submit()
assert(#calls == 4 and state.queue[1].text == "queue during unsupported active generation")
state.queue, state.pending_steer = {}, {}
state.capability.native_steer = true
transition = nil
draft("steer checks setup immediately")
controller.cancel_turn()
assert(#calls == 5 and calls[5].method == "turn.steer", "Ctrl-C did not send the composer immediately")
assert(calls[5].params.text == "steer checks setup immediately" and #state.queue == 0)
assert(text() == "" and transition == nil, "Ctrl-C paused instead of delivering the steering message")
reply({})
draft("")
controller.cancel_turn()
assert(transition.action == "pause" and transition.restore_prompt_if_no_output == true and #calls == 5,
  "empty Ctrl-C must retract an output-free submission instead of only pausing")
print("harness_submit_steering: passed")
