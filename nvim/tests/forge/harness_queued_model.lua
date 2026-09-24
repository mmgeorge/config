vim.loader.enable(false)
local client = require("forge.client")
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local model_picker = require("forge.views.harness.model_picker")
local history = require("forge.views.harness.prompt_history")
local notifications = require("forge.infra.notifications")
local requests, rejected, selection = {}, {}, nil
client.request = function(method, params, callback)
  requests[#requests + 1] = { method = method, params = params, callback = callback }
end
history.record = function() end
notifications.info = function() end
notifications.error = function(message) rejected[#rejected + 1] = message end
controller.render = function() end
controller.refresh_winbar = function() end
model_picker.open = function(options) selection = options.on_confirm end
state.session = { id = "queue-model", backend = "mock", model = "old", resolved_model = "old" }
state.capability = { model_selection = true, effort_selection = true }
state.model_backend, state.model_list = "mock", { { id = "gpt-5.6-sol" } }
state.composer_buf = vim.api.nvim_create_buf(false, true)
local function compose(text)
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, vim.split(text, "\n", { plain = true }))
end
local function configured(request)
  request.callback({ id = "queue-model", backend = "mock", model = request.params.model,
    resolved_model = request.params.model, effort = request.params.effort })
end
local success, failure = xpcall(function()
  local opened_mcp = false
  local open_mcp_picker = controller.open_mcp_picker
  controller.open_mcp_picker = function() opened_mcp = true end
  compose("/mcp")
  controller.queue_submit()
  assert(opened_mcp and #requests == 0 and #state.queue == 0, "queued /mcp reached the provider")
  controller.open_mcp_picker = open_mcp_picker
  state.busy = true
  state.queue = { "earlier prompt" }
  compose("/model gpt-5.6-sol xhigh explain this\nand this")
  controller.queue_submit()
  assert(#requests == 0 and #state.queue == 2 and state.session.model == "old")
  state.busy = false
  controller.drain()
  assert(requests[1].method == "prompt.submit" and requests[1].params.text == "earlier prompt")
  state.busy = false
  controller.drain()
  assert(requests[2].method == "session.configure" and requests[2].params.model == "gpt-5.6-sol"
    and requests[2].params.effort == "xhigh" and requests[2].params.validate)
  controller.drain()
  assert(#requests == 2, "queue advanced before configuration acknowledgement")
  configured(requests[2])
  assert(requests[3].method == "prompt.submit" and requests[3].params.text == "explain this\nand this")
  assert(state.session.model == "gpt-5.6-sol" and #state.queue == 0)

  compose("/model")
  controller.queue_submit()
  assert(selection and #requests == 3, "queued /model did not open the picker")
  selection({ model = "picked", effort = "high", context_window = 64000 })
  assert(#requests == 3 and #state.queue == 1, "picker applied the model during the active turn")
  local rows = require("forge.render.harness.queue").build(state.queue, 100)
  assert(rows[2][1][1]:find("/model picked high", 1, true), "queue preview lost the selected model")
  state.busy = false
  controller.drain()
  assert(requests[4].params.model == "picked" and requests[4].params.context_window == 64000)
  configured(requests[4])
  assert(#requests == 4 and #state.queue == 0, "model-only entry submitted an empty prompt")

  compose("/model invalid xhigh never run")
  controller.queue_submit()
  requests[5].callback(nil, "unavailable model")
  assert(#rejected == 1 and #state.queue == 1 and #requests == 5 and not state.configuring)
  assert(state.session.model == "picked", "rejected configuration changed the active model")
  controller.edit_last_queued()
  assert(#state.queue == 0 and table.concat(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false), "\n")
    == "/model invalid xhigh never run", "failed command could not be edited")
  compose("/model next impossible prompt")
  controller.queue_submit()
  assert(#requests == 5 and #state.queue == 0 and #rejected == 2, "invalid effort reached the provider")
  compose("/model next high submitted prompt")
  controller.submit()
  assert(requests[6].method == "session.configure")
  configured(requests[6])
  assert(requests[7].method == "prompt.submit" and requests[7].params.text == "submitted prompt")
  controller.command_set().action_by_id.model.run({})
  assert(type(selection) == "function", "shared model binding passed command context as the picker callback")
end, debug.traceback)
if state.working_timer then state.working_timer:stop() state.working_timer:close() state.working_timer = nil end
vim.api.nvim_buf_delete(state.composer_buf, { force = true })
if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("harness_queued_model: passed")
vim.cmd("qa!")
