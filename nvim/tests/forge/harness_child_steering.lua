vim.loader.enable(false)
local client = require("forge.client")
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
require("forge.views.harness.prompt_history").record = function() end
require("forge.infra.notifications").warn = function() end
require("forge.infra.notifications").error = function() end
require("forge.infra.notifications").info = function() end
controller.refresh_winbar = function() end
controller.render = function() end
controller.drain = function() end
state.session = { id = "child-steering", model = "test" }
state.composer_buf = vim.api.nvim_create_buf(false, true)
dofile("nvim/tests/forge/support/activity.lua").set_busy(state, true)
state.capability = { native_steer = true }
state.queue, state.pending_steer = {}, {}
state.selected_agent_run_id = "child"
local child = { id = "child", state = "ready", provider_thread_id = "thread" }
local exchange = {
  agent_id = "child",
  state = "running",
  turn = { { state = "running", provider = { thread_id = "thread", turn_id = "turn" } } },
}
state.agent = { run = { child }, exchange = { exchange } }
-- The host resolves the child's active provider turn and decides whether a message steers it or
-- waits for its next input boundary, so the editor addresses only the selected child identity.
local calls, other_calls, reply = {}, {}, nil
client.request = function(request_specification, callback)
  local method = request_specification.method
  local params = request_specification.params or {}
  if method:match("^session%.agent%.") then
    calls[#calls + 1] = { method = method, params = params, session_id = request_specification.session_id }
    reply = callback
  else
    other_calls[#other_calls + 1] = method
  end
end
local function draft(text) vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { text }) end
local function text() return vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] end
local function main_agent_requested()
  return vim.iter(other_calls):any(function(method)
    return method == "session.prompt.submit" or method == "session.input.apply" or method == "session.turn.steer"
  end)
end
draft("review the boundary")
controller.steer_submit()
assert(#calls == 1 and calls[1].method == "session.agent.message.send")
assert(calls[1].session_id == "child-steering" and calls[1].params.target == "child")
assert(calls[1].params.message == "review the boundary")
assert(text() == "review the boundary", "draft cleared before steering acknowledgement")
reply(nil, "turn already completed")
assert(text() == "review the boundary" and #state.queue == 0, "failed child steering became a new request")
assert(not main_agent_requested(), "failed child steering fell back to the main agent")
controller.submit()
assert(#calls == 2 and calls[2].method == "session.agent.followup" and calls[2].params.target == "child")
assert(text() == "review the boundary", "draft cleared before follow-up acknowledgement")
reply({ status = "admitted" })
assert(text() == "" and #state.queue == 0)
draft("another steer")
controller.steer_submit()
assert(#calls == 3 and calls[3].params.message == "another steer")
draft("new draft while awaiting acknowledgement")
reply({ delivery = "steered" })
assert(text() == "new draft while awaiting acknowledgement", "acknowledgement erased newer input")
state.selected_agent_run_id = "missing"
controller.submit()
assert(#calls == 4 and calls[4].params.target == "missing", "missing child input was not addressed to that child")
assert(not main_agent_requested(), "missing child input fell back to the main agent")
print("harness_child_steering: passed")
vim.cmd("qa!")
