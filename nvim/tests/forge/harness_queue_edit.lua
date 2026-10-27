vim.loader.enable(false)
local client = require("forge.client")
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local input = require("forge.views.harness.input_state")
local host = dofile("nvim/tests/forge/support/input.lua").new(state)
local errors, warnings = {}, {}
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
require("forge.infra.notifications").warn = function(message) warnings[#warnings + 1] = message end
require("forge.views.harness.prompt_history").record = function() end
controller.render = function() end
controller.refresh_winbar = function() end
client.request_for = function(_, method, parameters, callback)
  assert(host.request(method, parameters, callback), method)
end
state.session = { id = "queue-edit", backend = "mock", current_task_id = "task" }
state.queue, state.pending_steer = {}, {}
state.composer_buf = vim.api.nvim_create_buf(false, true)
state.presentation = { follow_tail = function() end, capture_prompt = function()
  return { document = "draft", buffer = state.composer_buf, is_current = function() return true end }
end }
input.attach(state, function() end)
local function compose(text)
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { text })
end
local function text() return vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] end
host.seed({
  { text = "prior", task_id = "task" },
  { text = "second", task_id = "task", config = { model = "retained" } },
  { text = "third", task_id = "task" },
})
local prior, second, third = unpack(state.queue)
compose("original composer draft")
controller.edit_queued(-1)
assert(text() == "third" and state.queue_edit.entry == third, "editing waited for reservation acknowledgement")
compose("third edited")
controller.edit_queued(-1)
compose("second edited")
controller.edit_queued(1)
assert(text() == "third edited" and state.queue_edit.barrier == second)
host.flush()
host.deliver(prior.id)
assert(state.queue[1] == second and state.queue_edit.drafts[second] == "second edited")
controller.submit()
assert(text() == "original composer draft" and not state.queue_edit)
assert(state.queue[1] == second and second.text == "second edited" and third.text == "third edited")
assert(second.config.model == "retained")
host.flush()
assert(state.queue[1] == second and second.text == "second edited")

compose("next draft")
controller.edit_queued(-1)
compose("retained third")
controller.edit_queued(-1)
compose("")
host.flush()
controller.submit()
assert(#state.queue == 1 and state.queue[1] == third and text() == "next draft")
host.flush()
assert(third.text == "retained third")

controller.edit_queued(-1)
compose("unqueue this")
host.flush()
controller.queue_submit()
assert(text() == "unqueue this" and #state.queue == 0)
local before = #host.requests
host.flush()
assert(#host.requests == before, "unqueue submitted the text")
controller.steer_submit()
assert(text() == "" and state.queue[1].text == "unqueue this")
assert(host.requests[#host.requests].parameters.action.message.intent == "auto")
host.flush()
assert(#errors == 0 and #warnings == 0, table.concat(errors, "\n"))
print("harness_queue_edit: passed")
vim.cmd("qa!")
