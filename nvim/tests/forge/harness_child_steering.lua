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
local calls, reply = {}, nil
client.request = function(method, params, callback)
  calls[#calls + 1] = { method = method, params = params }
  reply = callback
end
local function draft(text) vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { text }) end
local function text() return vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] end
draft("review the boundary")
controller.submit()
assert(#calls == 1 and calls[1].method == "turn.steer")
assert(calls[1].params.target.thread_id == "thread" and calls[1].params.target.turn_id == "turn")
assert(text() == "review the boundary", "draft cleared before steering acknowledgement")
reply(nil, "turn already completed")
assert(text() == "review the boundary" and #state.queue == 0, "failed child steering became a new request")
exchange.state, exchange.turn[1].state = "complete", { outcome = "completed" }
controller.submit()
assert(#calls == 1 and text() == "review the boundary", "completed child accepted input")
exchange.state = "running"
exchange.turn[1] = { state = "running", provider = { thread_id = "thread", turn_id = "next-turn" } }
controller.submit()
reply({})
assert(text() == "" and #state.queue == 0)
draft("another steer")
controller.submit()
draft("new draft while awaiting acknowledgement")
reply({})
assert(text() == "new draft while awaiting acknowledgement", "acknowledgement erased newer input")
state.selected_agent_run_id = "missing"
controller.submit()
assert(#calls == 3, "missing child input fell back to the main agent")
print("harness_child_steering: passed")
