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
local original_request = client.request
local original_transition = controller.task_transition
local original_error = notifications.error
local composer = vim.api.nvim_create_buf(false, true)
state.session = { id = "responsive", provider = "mock", model = "test", effort = "medium" }
state.composer_buf, state.composer_win = composer, vim.api.nvim_get_current_win()
state.busy, state.queue = true, {}
local routed, configured = {}, {}
local success, failure = xpcall(function()
  controller.submit = function()
    routed[#routed + 1] = table.concat(vim.api.nvim_buf_get_lines(composer, 0, -1, false), "\n")
  end
  controller.drain, history.record = function() end, function() end
  local controls = { "/config", "/effort", "/effort high", "/model", "/read", "/write", "/yolo", "/ask", "/deny", "/allow",
    "/fast", "/ultrafast", "/skills", "/mcp", "/checks", "/rename", "/sessions",
    "/task refresh", "/agents on", "/agents edit", "/provider", "/questions" }
  for _, command in ipairs(controls) do
    vim.api.nvim_buf_set_lines(composer, 0, -1, false, { command })
    controller.queue_submit()
    assert(routed[#routed] == command, command .. " was queued as provider text")
    assert(#state.queue == 0, command .. " waited behind active work")
  end
  controller.submit, controller.drain = original_submit, original_drain
  client.request = function(request_specification, callback)
  local method = request_specification.method

    if method == "host.trace.status" then callback({ enabled = false })
    elseif method == "background.settings" then
      callback({ revision = 0, settings = request_specification.params.defaults })
    elseif method == "session.provider.model.list" then callback({})
    else error(method) end
  end
  controller.configure = function(params, completed)
    configured[#configured + 1] = { params = params, completed = completed }
  end
  settings.open(state, { window_list = { state.composer_win }, control_win = state.composer_win })
  local instance = picker._state_for_test()
  local options = {}
  for _, page in ipairs(instance.spec.page_list) do
    for _, option in ipairs(page.option_list) do options[option.id] = option end
  end
  instance.spec.on_field_action({ field = options["automatic-checks"].fields[1] }, "toggle", 1)
  instance.spec.on_field_action({ field = options["shell"].fields[1] }, "change", 1)
  assert(#configured == 2, "configuration picker blocked a second edit before acknowledgement")
  assert(configured[1].params.automatic_checks == true)
  assert(configured[2].params.shell ~= nil)
  assert(not (picker._state_for_test().spec.page_list[1].subtitle or ""):find("Changes pending", 1, true))
  configured[1].completed(false)
  assert(not (picker._state_for_test().spec.page_list[1].subtitle or ""):find("Changes pending", 1, true),
    "configuration displayed pending status after an older completion")
  state.session.shell = configured[2].params.shell
  state.automatic_checks = true
  configured[2].completed(true)
  assert(not (picker._state_for_test().spec.page_list[1].subtitle or ""):find("Changes pending", 1, true))
  picker.close()
  local requested = 0
  client.request = function() requested = requested + 1 end
  state.model_provider, state.model_list = "mock", { { id = "test", reasoning = { "low", "medium", "high" } } }
  local delivered = false
  catalog.models(false, function(models) delivered = models[1].id == "test" end)
  assert(delivered and requested == 0, "warm model controls waited for provider discovery")
  state.capability = { model_selection = true }
  state.composer_win = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(state.composer_win, composer)
  state.model_list, state.model_provider = nil, nil
  local model_response
  client.request = function(request_specification, callback)
  local method = request_specification.method

    if method == "provider.model.pin.list" then callback({})
    elseif method == "session.provider.model.list" then model_response = callback
    else error(method) end
  end
  controller.select_model()
  assert(picker._state_for_test().spec.page_list[1].empty_text == "Loading models...",
    "cold model selection did not open immediately")
  picker.close()
  model_response({ { id = "test", reasoning = { "low", "high" } } })
  assert(not picker.is_open(), "late model discovery reopened a cancelled picker")
  catalog.clear()
  state.model_list, state.model_provider = nil, nil
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
  controller.configure({ effort = "high" }, function(result) applied = result end)
  transition_complete(true)
  assert(applied == true, "successful active configuration was reported as unsuccessful")
  state.busy, state.task_operation = false, nil
  local configuration_request = {}
  controller.task_transition = function(action, _, callback)
    assert(action.action == "configure")
    configuration_request[#configuration_request + 1] = { params = action.config, callback = callback }
  end
  local first_applied, second_applied
  controller.configure({ shell = "system" }, function(result) first_applied = result end)
  controller.configure({ plan_compact = true }, function(result) second_applied = result end)
  assert(#configuration_request == 2 and second_applied == nil)
  state.session.shell = "system"
  configuration_request[1].callback(true)
  assert(#configuration_request == 2, "Lua delayed an explicit configuration")
  state.session.plan_compact = true
  configuration_request[2].callback(true)
  assert(first_applied == true and second_applied == true,
    "successive idle configuration edits lost their completion callbacks")
  local resource_picker = require("forge.views.harness.provider_picker")
  local mutations = {}
  client.request = function(request_specification, callback)
  local method = request_specification.method
  local params = request_specification.params or {}

    if method == "session.provider.skill.list" then
      callback({ { name = "first", enabled = true }, { name = "second", enabled = true } })
    elseif method == "session.provider.mcp.list" then
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
client.request = original_request
controller.task_transition = original_transition
notifications.error = original_error
if picker.is_open() then picker.close() end
vim.api.nvim_buf_delete(composer, { force = true })
if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("harness_control_responsiveness: passed")
vim.cmd("qa!")
