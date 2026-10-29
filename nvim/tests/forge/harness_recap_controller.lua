vim.opt.runtimepath:prepend("nvim")
local state = require("forge.session").harness
local client = require("forge.client")
local controller = require("forge.views.harness.controller")
local requests, prompt = {}, nil
local input = require("forge.views.harness.input_state")
local host = dofile("nvim/tests/forge/support/input.lua").new(state)
client.host_get_generation = function() return 1 end
client.request = function(request_specification, callback)
  local method = request_specification.method
  local params = request_specification.params or {}

  if host.request(method, params, callback) then return end
  assert(method == "session.document" and params.operation == "recap")
  requests[#requests + 1] = callback
end
client.request = function(request_specification)
  local method = request_specification.method
  local params = request_specification.params or {}
   prompt = { method = method, params = params } end
controller.render = function() end
controller.refresh_winbar = function() end
state.session = { id = "recap-controller", model = "test" }
state.composer_buf = vim.api.nvim_create_buf(false, true)
state.capability, state.queue, state.busy = {}, {}, true
input.attach(state, function() end)
state.busy = true
local function compose(text) vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { text }) end
compose("/recap")
controller.queue_submit()
assert(#requests == 1 and #state.queue == 0 and state.busy)
requests[1]({ text = "Recap text" })
compose("Next task")
controller.queue_submit()
assert(state.recap.text and #state.queue == 1, "queue admission erased the recap")
state.busy = false
host.flush()
controller.render()
assert((not prompt or prompt.method ~= "session.prompt.submit") and state.recap.text, "presentation independently dispatched the host queue")
compose("/recap")
controller.submit()
require("forge.views.harness.recap").clear(state)
requests[2]({ text = "late" })
assert(state.recap == nil)
print("harness_recap_controller: passed")
vim.cmd("qa!")
