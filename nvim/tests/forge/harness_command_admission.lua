vim.loader.enable(false)
local source = require("forge.views.harness.completion.command_source")
assert(not source.accepts_prompt("/sess"))
assert(not source.accepts_prompt("  /sess extra text"))
assert(not source.accepts_prompt("/"))
assert(source.accepts_prompt("/sessions anything /unrecognized"))
assert(source.accepts_prompt("/plan arbitrary prompt"))
assert(source.accepts_prompt("/plan"))
assert(source.accepts_prompt("Explain /sess"))
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
state.composer_buf = vim.api.nvim_create_buf(false, true)
state.queue = {}
for _, submit in ipairs({ controller.submit, controller.queue_submit }) do
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "/sess" })
  submit()
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "/sess")
  assert(#state.queue == 0)
end
local client = require("forge.client")
local notifications = require("forge.infra.notifications")
require("forge.views.harness.prompt_history").record = function() end
local requests, warnings = {}, {}
client.request_for = function(_, method, params, callback)
  requests[#requests + 1] = { method = method, params = params }
  callback({})
end
notifications.warn = function(message) warnings[#warnings + 1] = message end
controller.refresh_winbar = function() end
state.session = { id = "plan-mode", mode = "write", execution_mode = "write" }
state.transcript_win, state.composer_win = vim.api.nvim_get_current_win(), vim.api.nvim_get_current_win()
state.no_checkpoint = false
state.busy = false
for _, submit in ipairs({ controller.submit, controller.queue_submit }) do
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "  /plan  " })
  submit()
  assert(state.session.execution_mode == "write")
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "")
  assert(#state.queue == 0)
end
assert(#requests == 2 and requests[1].method == "plan.list" and requests[2].method == "plan.list")
state.busy = true
vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "/plan" })
controller.submit()
assert(#requests == 3 and #warnings == 0 and #state.queue == 0)
print("harness_command_admission: passed")
