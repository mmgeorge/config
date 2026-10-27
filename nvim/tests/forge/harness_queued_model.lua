vim.loader.enable(false)
local client = require("forge.client")
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local input = require("forge.views.harness.input_state")
local host = dofile("nvim/tests/forge/support/input.lua").new(state)
local warnings = {}
controller.render = function() end
controller.refresh_winbar = function() end
require("forge.views.harness.prompt_history").record = function() end
require("forge.infra.notifications").warn = function(message) warnings[#warnings + 1] = message end
require("forge.infra.notifications").error = function(message) error(message) end
client.request_for = function(_, method, parameters, callback)
  assert(host.request(method, parameters, callback), method)
end
state.session = { id = "queued-model", backend = "mock", model = "old" }
state.capability = { model_selection = true, effort_selection = true }
state.composer_buf = vim.api.nvim_create_buf(false, true)
state.presentation = { follow_tail = function() end }
state.queue = {}
input.attach(state, function() end)
local function compose(text)
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, vim.split(text, "\n", { plain = true }))
end
compose("/model chosen xhigh explain this\nand this")
controller.queue_submit()
assert(state.session.model == "old" and #state.queue == 1, "queueing changed active model settings")
local queued = state.queue[1]
assert(queued.config.model == "chosen" and queued.config.effort == "xhigh")
assert(queued.text == "explain this\nand this")
host.flush()
controller.edit_queued(-1)
compose("/model another impossible never send")
controller.submit()
assert(#warnings == 1 and state.queue_edit, "invalid effort committed the edit")
assert(queued.config.model == "chosen")
compose("/model another high revised prompt")
controller.submit()
assert(queued.config.model == "another" and queued.text == "revised prompt")
assert(state.session.model == "old", "optimistic edit changed provider configuration")
host.flush()
assert(queued.config.model == "another" and queued.config.effort == "high")
assert(queued.text == "revised prompt")
local configured
controller.task_transition = function(action)
  assert(action.action == "configure")
  configured = action.config
end
compose("/model settings-only high")
controller.queue_submit()
assert(#state.queue == 1 and configured.model == "settings-only", "model-only selection was queued as empty input")
assert(#warnings == 1)
print("harness_queued_model: passed")
vim.cmd("qa!")
