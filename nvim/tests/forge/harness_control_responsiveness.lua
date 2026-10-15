vim.loader.enable(false)
local controller = require("forge.views.harness.controller")
local state = require("forge.session").harness
local client = require("forge.client")
local history = require("forge.views.harness.prompt_history")
local picker = require("forge.views.picker")
local settings = require("forge.views.harness.settings")
local catalog = require("forge.views.harness.provider_catalog")
local notifications = require("forge.infra.notifications")
local original_submit, original_drain = controller.submit, controller.drain
local original_configure, original_record = controller.configure, history.record
local original_request, original_request_for = client.request, client.request_for
local original_transition = controller.task_transition
local original_error = notifications.error
local composer = vim.api.nvim_create_buf(false, true)
state.session = { id = "responsive", backend = "mock", model = "test", effort = "medium" }
state.composer_buf, state.composer_win = composer, vim.api.nvim_get_current_win()
state.busy, state.queue = true, {}
local routed, configured = {}, {}
local success, failure = xpcall(function()
  controller.submit = function()
    routed[#routed + 1] = table.concat(vim.api.nvim_buf_get_lines(composer, 0, -1, false), "\n")
  end
  controller.drain, history.record = function() end, function() end
  local controls = { "/config", "/effort", "/effort high", "/model", "/model test", "/model test high", "/read", "/write", "/yolo", "/mode",
    "/fast", "/ultrafast", "/skills", "/mcp", "/checks", "/rename", "/sessions",
    "/task refresh", "/agents on", "/agents edit", "/backend", "/questions" }
  for _, command in ipairs(controls) do
    vim.api.nvim_buf_set_lines(composer, 0, -1, false, { command })
    controller.queue_submit()
    assert(routed[#routed] == command, command .. " was queued as provider text")
    assert(#state.queue == 0, command .. " waited behind active work")
  end
  controller.submit, controller.drain = original_submit, original_drain
  client.request_for = function(_, method, _, callback)
    assert(method == "trace.status", method)
    callback({ enabled = false })
  end
  controller.configure = function(params, _, completed)
    configured[#configured + 1] = { params = params, completed = completed }
  end
  settings.open(state, { window_list = { state.composer_win }, control_win = state.composer_win })
  local instance = picker._state_for_test()
  local options = {}
  for _, option in ipairs(instance.spec.page_list[1].option_list) do options[option.id] = option end
  instance.spec.action_list[2].callback({ option = options["plan-revisions"] })
  instance.spec.action_list[2].callback({ option = options["default-write-permission"] })
  assert(#configured == 2, "configuration picker blocked a second edit before acknowledgement")
  assert(configured[1].params.plan_auto_approve_revisions == false)
  assert(configured[2].params.default_write_permission == "yolo")
  assert(picker._state_for_test().spec.page_list[1].subtitle:find("Changes pending", 1, true))
  configured[1].completed(false)
  assert(picker._state_for_test().spec.page_list[1].subtitle:find("Changes pending", 1, true),
    "an older completion cleared the latest pending edit")
  state.session.default_write_permission = "yolo"
  state.session.plan_auto_approve_revisions = false
  configured[2].completed(true)
  assert(not picker._state_for_test().spec.page_list[1].subtitle:find("Changes pending", 1, true))
  picker.close()
  local requested = 0
  client.request = function() requested = requested + 1 end
  state.model_backend, state.model_list = "mock", { { id = "test", reasoning = { "low", "medium", "high" } } }
  local delivered = false
  catalog.models(false, function(models) delivered = models[1].id == "test" end)
  assert(delivered and requested == 0, "warm model controls waited for provider discovery")
  state.capability = { model_selection = true }
  state.composer_win = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(state.composer_win, composer)
  state.model_list, state.model_backend = nil, nil
  local model_response
  client.request = function(method, _, callback)
    if method == "backend.model_pins" then callback({})
    elseif method == "backend.models" then model_response = callback
    else error(method) end
  end
  controller.select_model()
  assert(picker._state_for_test().spec.page_list[1].empty_text == "Loading models...",
    "cold model selection did not open immediately")
  picker.close()
  model_response({ { id = "test", reasoning = { "low", "high" } } })
  assert(not picker.is_open(), "late model discovery reopened a cancelled picker")
  catalog.clear()
  state.model_list, state.model_backend = nil, nil
  state.capability.effort_selection = true
  local reported
  notifications.error = function(message) reported = message end
  controller.select_effort()
  assert(picker.is_open(), "cold effort selection did not open immediately")
  model_response(nil, "catalog unavailable")
  assert(not picker.is_open() and reported == "catalog unavailable",
    "failed effort discovery left a permanent loading picker")
  controller.configure = original_configure
  local transition_complete, applied
  controller.task_transition = function(_, _, completed) transition_complete = completed end
  controller.configure({ effort = "high" }, true, function(result) applied = result end)
  transition_complete(true)
  assert(applied == true, "successful active configuration was reported as unsuccessful")
  state.busy, state.task_operation = false, nil
  local configuration_request = {}
  client.request = function(method, params, callback)
    assert(method == "session.configure", method)
    configuration_request[#configuration_request + 1] = { params = params, callback = callback }
  end
  local first_applied, second_applied
  controller.configure({ default_write_permission = "write" }, true, function(result) first_applied = result end)
  controller.configure({ plan_auto_approve_revisions = true }, true, function(result) second_applied = result end)
  assert(#configuration_request == 1 and second_applied == nil)
  configuration_request[1].callback(vim.tbl_extend("force", state.session, { default_write_permission = "write" }))
  assert(vim.wait(1000, function() return #configuration_request == 2 end, 10),
    "a completed idle edit did not drain the next configuration")
  configuration_request[2].callback(vim.tbl_extend("force", state.session, { plan_auto_approve_revisions = true }))
  assert(first_applied == true and second_applied == true and state.pending_config_completed == nil,
    "successive idle configuration edits lost their completion callbacks")
  local resource_picker = require("forge.views.harness.provider_picker")
  local mutations = {}
  client.request = function(method, params, callback)
    if method == "backend.skills" then
      callback({ { name = "first", enabled = true }, { name = "second", enabled = true } })
    elseif method == "backend.mcp" then
      callback({ { name = "first", enabled = true, status = "connected" },
        { name = "second", enabled = true, status = "connected" } })
    else
      mutations[#mutations + 1] = { params = params, callback = callback }
    end
  end
  for _, resource in ipairs({ "skills", "mcp" }) do
    mutations = {}
    catalog.clear()
    resource_picker["open_" .. resource]({ host = { window_list = { state.composer_win }, control_win = state.composer_win } })
    for _, index in ipairs({ 1, 1, 2 }) do
      local current = picker._state_for_test()
      current.spec.action_list[1].callback({ option = current.spec.page_list[1].option_list[index] })
    end
    assert(#mutations == 3, resource .. " blocked another edit while a selection was pending")
    local current = picker._state_for_test()
    assert(current.spec.page_list[1].subtitle == "Changes pending")
    assert(current.spec.page_list[1].option_list[1].value.enabled == true
      and current.spec.page_list[1].option_list[2].value.enabled == false,
      resource .. " failed to display the newest desired selections")
    mutations[1].callback({ enabled = false })
    assert(picker._state_for_test().spec.page_list[1].option_list[1].value.enabled == true,
      resource .. " allowed an older completion to replace a newer selection")
    picker.close()
  end
end, debug.traceback)
controller.submit, controller.drain = original_submit, original_drain
controller.configure, history.record = original_configure, original_record
client.request, client.request_for = original_request, original_request_for
controller.task_transition = original_transition
notifications.error = original_error
if picker.is_open() then picker.close() end
vim.api.nvim_buf_delete(composer, { force = true })
if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("harness_control_responsiveness: passed")
vim.cmd("qa!")
