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
local calls, reply = {}, nil
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
draft("explicit follow-up")
controller.queue_submit()
assert(#calls == 1 and state.queue[1] == "explicit follow-up", "queue submission steered")
state.queue = {}
state.busy, state.configuring = false, true
draft("after configuration")
controller.submit()
assert(#calls == 1 and state.queue[1] == "after configuration", "configuration-only state attempted steering")
state.busy, state.configuring = true, false
state.capability.native_steer = false
state.queue = {}
draft("unsupported backend")
controller.submit()
assert(#calls == 1 and state.queue[1] == "unsupported backend", "unsupported backend attempted steering")
print("harness_submit_steering: passed")
