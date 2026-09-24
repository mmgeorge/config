vim.opt.runtimepath:prepend("nvim")
local state = require("forge.session").harness
local client = require("forge.client")
local controller = require("forge.views.harness.controller")
local requests, prompt = {}, nil
client.host_generation = function() return 1 end
client.request_for = function(_, method, params, callback)
  assert(method == "harness.document" and params.operation == "recap")
  requests[#requests + 1] = callback
end
client.request = function(method, params) prompt = { method = method, params = params } end
controller.render = function() end
controller.refresh_winbar = function() end
state.session = { id = "recap-controller", model = "test" }
state.composer_buf = vim.api.nvim_create_buf(false, true)
state.capability, state.queue, state.busy = {}, {}, true
local function compose(text) vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { text }) end
compose("/recap")
controller.queue_submit()
assert(#requests == 1 and #state.queue == 0 and state.busy)
requests[1]({ text = "Recap text" })
compose("Next task")
controller.queue_submit()
assert(state.recap.text and #state.queue == 1, "queue admission erased the recap")
state.busy = false
controller.drain()
assert(prompt.method == "prompt.submit" and state.recap == nil, "prompt dispatch did not erase recap")
compose("/recap")
controller.submit()
require("forge.views.harness.recap").clear(state)
requests[2]({ text = "late" })
assert(state.recap == nil)
print("harness_recap_controller: passed")
vim.cmd("qa!")
