local MiniTest = require("mini.test")
local eq = MiniTest.expect.equality
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
local keymap = dofile("nvim/lua/forge/tests/support/keymap.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
local notify = dofile("nvim/lua/forge/tests/support/notify.lua")
local mock_client = dofile("nvim/lua/forge/tests/support/mock_client.lua")
local host = dofile("nvim/lua/forge/tests/support/host.lua")
local git_repository = dofile("nvim/lua/forge/tests/support/git_repository.lua")
local check_tool_path = vim.fs.normalize(vim.fn.fnamemodify("nvim/lua/forge/tests/fixtures/check_tool.lua", ":p"))

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["background"] = function()
local root = temp.directory("background")
local workspace = root .. "/repository"
local repository = git_repository.create({ path = workspace })
repository.run({ "commit", "-q", "--allow-empty", "-m", "feat: initial" })
vim.fn.writefile({ "local value = true" }, workspace .. "/example.lua")
local client = host.start({ data = root, config = root, schedule_ensure = false }).client
require("forge").setup({ harness = { provider = "mock" }, background_provider = "mock", background_generation = { mock = { model = "mock-model", reasoning_effort = "low" } } })
vim.fn.chdir(workspace)
local state = require("forge.session").harness
local errors = notify.capture({ min_level = vim.log.levels.ERROR, notifications = true })
local function call(method, params)
  local result, failure, done
  client.request({ method = method, params = params }, function(value, error) result, failure, done = value, error, true end)
  wait_until(function() return done end, method .. " timed out", { timeout_ms = 4000 })
  assert(not failure, tostring(failure) .. "\n" .. tostring(client._client.stderr) .. "\n" .. vim.inspect(errors))
  return result
end
local generation, generation_failure
client.generation_config(function(value, failure) generation, generation_failure = value, failure end)
wait_until(function() return generation ~= nil or generation_failure ~= nil end, "generation configuration did not settle", { timeout_ms = 4000 })
assert(generation, generation_failure)
local preview = call("repository.generation", { operation = "preview", workspace = workspace, comparison = "head" })
assert(preview.prompt.user:find("Current Background task: commit_message", 1, true))
assert(preview.prompt.user:find("Conventional Commit", 1, true))
assert(not preview.prompt.system:find("Conventional Commit", 1, true))
assert(preview.prompt.input_bytes == #preview.prompt.user + #preview.prompt.system)
local params = { operation = "generate", workspace = workspace, comparison = "head", generation = generation }
local first = call("repository.generation", params)
assert(first.session_id and first.message and first.message ~= "")
local second = call("repository.generation", params)
assert(first.session_id == second.session_id, "HEAD-stable generation lost persistence")
assert(state.session == nil, "Background generation initialized interactive Harness")
local query = { workspace = workspace, data_root = generation.data_root }
local snapshot = call("background.session.document", query)
assert(snapshot.session.id == first.session_id)
query.auto_compaction = { enabled = false, threshold_tokens = 64000 }
query.expected_revision = snapshot.settings.revision
snapshot = call("background.session.document", query)
assert(snapshot.settings.desired.auto_compaction.threshold_tokens == 64000)
query.auto_compaction, query.expected_revision = nil, nil
require("forge").open_background_timeline()
wait_until(function()
  return vim.api.nvim_buf_get_name(0):match("ForgeBackground")
    and buffer.text(0):find("Mock response",1,true)
end, "Background timeline did not render", { timeout_ms = 4000 })
assert(vim.bo.modifiable == false, "Background timeline is editable")
assert(state.session == nil, "Debug timeline selected interactive Harness")
repository.run({ "switch", "-q", "-c", "feature" })
local branch = call("repository.generation", params)
assert(branch.session_id ~= first.session_id, "branch switch reused obsolete context")
errors:assert_none()
assert(host.stop(), "Host did not stop")
end
-- The Background provider is its own saved setting: a chat provider that cannot run Background
-- sessions must not leak into Background settings or decide which provider generates.
T["background_provider_independent"] = function()
local root = temp.directory("background-provider")
local workspace = root .. "/repository"
local repository = git_repository.create({ path = workspace })
repository.run({ "commit", "-q", "--allow-empty", "-m", "feat: initial" })
vim.fn.writefile({ "local value = true" }, workspace .. "/example.lua")
host.start({ data = root, config = root, schedule_ensure = false })
require("forge").setup({ harness = { provider = "antigravity" }, background_provider = "mock",
  background_generation = { mock = { model = "mock-model", reasoning_effort = "low" } } })
vim.fn.chdir(workspace)
local client = require("forge.client")
local snapshot, failure, done
require("forge.integrations.background_settings").request(nil, nil, function(value, error)
  snapshot, failure, done = value, error, true
end)
wait_until(function() return done end, "Background settings did not settle", { timeout_ms = 4000 })
assert(snapshot, "Background settings failed under an unsupported chat provider: " .. tostring(failure))
assert(snapshot.settings.provider == "mock", vim.inspect(snapshot.settings))
assert(snapshot.settings.models.antigravity == nil, "the chat provider leaked into Background settings")
local selection = require("forge.integrations.background_settings").selection(snapshot)
local generation = assert(client.generation_get_config(selection))
assert(generation.provider.kind == "mock", "generation followed the chat provider")
local result, generation_failure, generated
client.request({ method = "repository.generation", params = { operation = "generate", workspace = workspace,
  comparison = "head", generation = generation } }, function(value, error)
  result, generation_failure, generated = value, error, true
end)
wait_until(function() return generated end, "Background generation did not settle", { timeout_ms = 4000 })
assert(result and result.message and result.message ~= "", tostring(generation_failure))
assert(host.stop(), "Host did not stop")
end
T["access_preferences"] = function()
local workspace = temp.directory("workspace")
git_repository.run(workspace, { "init" })
vim.fn.writefile({ vim.json.encode({ checks = vim.empty_dict() }) }, workspace .. "/.forge.json")
local data = temp.directory("data")
local errors = notify.capture({ min_level = vim.log.levels.ERROR })
local client = host.start({ data = data, config = data }).client
local state = require("forge.session").harness
local function wait_context()
  return vim.inspect({
    approval_mode = state.session and state.session.approval_mode, task_operation = state.task_operation,
    approval_timeout = state.session and state.session.approval_timeout,
  })
end
local wait_options = { timeout_ms = 4000, errors = errors, context = wait_context }
local function request(method, params)
  local result, failure, done
  client.request({ method = method, params = params, session_id = require("forge.event_contract").method_policy(method).session_context ~= "none" and assert(require("forge.session").harness.session).id or nil }, function(value, error) result, failure, done = value, error, true end)
  wait_until(function() return done end, method .. " did not settle", wait_options)
  assert(not failure, failure)
  return result
end
local picker = require("forge.views.picker")
local settings = require("forge.views.harness.settings")
local function field(id)
  local instance = assert(picker._state_for_test())
  for page_index, page in ipairs(instance.spec.page_list) do
    for index, option in ipairs(page.option_list) do
      if option.id == id then
        return { instance = instance, page = page, page_index = page_index, index = index, option = option, value = option.fields[1] }
      end
    end
  end
  error("missing field " .. id)
end
local function change(id, key)
  local selected = field(id)
  selected.instance.state.page_index = selected.page_index
  selected.instance.state.selected_index_by_page[selected.page.id] = selected.index
  picker.update(selected.instance.spec)
  keymap.invoke(key, { buffer = selected.instance.buf })
end
local function open_settings()
  settings.open(state, { window_list = { state.transcript_win, state.composer_win }, control_win = state.composer_win })
  wait_until(function() return picker.is_open("harness-config") end, "configuration did not open", wait_options)
end
vim.api.nvim_set_current_dir(workspace)
require("forge").setup({ diff_logging = false, harness_logging = false, harness = { provider = "mock" } })
require("forge.views.harness").open()
wait_until(function() return state.presentation and state.presentation.ready end, "Harness did not open", wait_options)
assert(not state.capability.os_sandbox and not state.session.access.sandbox)
open_settings()
assert(field("sandbox").value.muted and not field("sandbox").value.disabled_reason)
change("sandbox", "<CR>")
wait_until(function() return state.access_preference.sandbox and field("sandbox").value.value end,
  "muted sandbox preference did not save", wait_options)
assert(not state.session.access.sandbox, "mock provider enabled OS isolation")
picker.close(false)
open_settings()
assert(field("sandbox").value.value and field("sandbox").value.muted, "reopening lost the selected preference")
assert(not pcall(field, "isolation-mode"), "Mock must not invent provider isolation modes")
change("readable-directories", "<CR>")
wait_until(function() return vim.bo.buftype == "acwrite" end, "directory editor did not open", wait_options)
local directory_buffer = vim.api.nvim_get_current_buf()
local directory_row = vim.api.nvim_win_get_cursor(0)[1] - 1
vim.api.nvim_buf_set_text(directory_buffer, directory_row, 0, directory_row, 0, { workspace })
vim.api.nvim_exec_autocmds("TextChanged", { buffer = directory_buffer })
vim.cmd("write")
wait_until(function() return #state.access_preference.readable_directory == 1 and not vim.bo[directory_buffer].modified end,
  "directory preference did not save", wait_options)
assert(state.access_preference.sandbox and not state.session.access.sandbox,
  "directory editing overwrote the saved sandbox preference")
keymap.invoke("q")
wait_until(function() return picker.is_open("harness-config") end, "directory editor did not return to configuration", wait_options)
for _, mode in ipairs({ "deny", "allow", "ask" }) do
  change("approval-mode", "<Right>")
  wait_until(function() return state.session.approval_mode == mode and not require("forge.views.harness.activity").busy(state) end,
    "tool approval mode did not apply: " .. mode, wait_options)
  assert(state.access_preference.sandbox and not state.session.access.sandbox,
    "approval mode overwrote saved sandbox preference or effective access")
end
for _ = 1, 3 do change("approval_timeout", "<Right>") end
-- Timeout edits are ordinary session configuration now; the confirmed session value is the
-- settled state, and the picker must show the same final selection.
wait_until(function() return state.session.approval_timeout.amount == 13 end,
  "rapid timeout edits did not retain both final values", wait_options)
wait_until(function() return field("approval_timeout").value.value == "13m" end,
  "configuration picker does not show the host-confirmed 13m",
  { timeout_ms = 4000, context = function() return "shown: " .. tostring(field("approval_timeout").value.value) end })
local selected = request("settings.get", {})
assert(selected.desired.approval_timeout.amount == 13)
assert(selected.desired.access.sandbox and not state.session.access.sandbox)
assert(request("session.task.get", {}) == nil, "configuration started a task")
picker.close(false)
require("forge.views.harness.workspace").release(state)
assert(state.presentation.close())
picker.close(false)
require("forge.views.harness.workspace").release(state)
assert(host.stop(), "Harness host was not collected")
end
T["check_shortcuts"] = function()
local check_tool = dofile(check_tool_path)
local workspace = temp.directory("workspace")
local client = host.start().client
local state = require("forge.session").harness
local errors = notify.capture({ min_level = vim.log.levels.ERROR, notifications = true })
local function request(method, params)
  local done, result, failure
  client.request({ method = method, params = params or {}, session_id = require("forge.event_contract").method_policy(method).session_context ~= "none" and assert(require("forge.session").harness.session).id or nil }, function(value, problem) result, failure, done = value, problem, true end)
  wait_until(function() return done end, method .. " timed out", { timeout_ms = 4000 })
  assert(not failure, method .. ": " .. tostring(failure))
  return result
end
local function rows()
  return buffer.text(state.transcript_buf)
end
-- Live check progress belongs to the status split; completed results stay in the transcript.
local function status_rows()
  local status = state.presentation and state.presentation.status_view
  return table.concat(status and status.content and status.content.lines or {}, "\n")
end
local function start_check()
  local run_id = check_tool.start(request, { check = { "slow" } }).run_id
  wait_until(function()
    return state.checks_running and state.active_check_run == run_id and status_rows():find("Shortcut check is running", 1, true)
  end, "check did not become active", { timeout_ms = 4000, context = function()
    return vim.inspect({ run_id = run_id, checks_running = state.checks_running,
      active_check_run = state.active_check_run, errors = errors, status = status_rows(), transcript = rows() })
  end })
  return run_id
end
local function settled()
  return not state.checks_running and not state.cleanup_operation_id and not require("forge.views.harness.activity").busy(state)
    and #(state.input and state.input.drafts or {}) == 0
    and #state.queue == 0 and #state.pending_steer == 0 and #(state.pending_submit or {}) == 0
end
local function compose(text)
  buffer.set(state.composer_buf, { text })
end
vim.fn.writefile({ vim.json.encode({ checks = {
["slow"] = { trigger = { auto = false }, action = { command = "print 'Shortcut check is running'; sleep 30sec", label = "Shortcut check", category = "test" } }
} }) }, workspace .. "/.forge.json")
vim.api.nvim_set_current_dir(workspace)
require("forge").setup({ diff_logging = false, harness_logging = false,
  harness = { provider = "mock", providers = { mock = { command = { "mock" }, selectable = true } } } })
require("forge.views.harness").open()
wait_until(function() return state.session ~= nil end, "startup timed out", { timeout_ms = 4000 })
request("session.configure", { shell = "nushell" })
local run_id = start_check()
compose("")
keymap.invoke("<C-c>", { window = state.composer_win })
wait_until(function() return not state.checks_running end, "Ctrl-C did not stop the check", { timeout_ms = 3000,
  context = function() return vim.inspect({ operation = state.cleanup_operation_id, notice = state.execution_notice }) end })
assert(request("session.check.status", {run_id = run_id}).state == "interrupted", "Ctrl-C did not interrupt check history")
wait_until(settled, "Ctrl-C did not settle its pause", { timeout_ms = 3000 })
for _, pause_first in ipairs({ false, true }) do
  run_id = start_check()
  if pause_first then
    compose("")
    keymap.invoke("<C-c>", { window = state.composer_win })
    -- The interruption owns cleanup_operation_id from Ctrl-C until its host settlement.
    assert(state.cleanup_operation_id, "Ctrl-C did not start a pause")
  end
  local text = pause_first and "Replacement during pause" or "Replacement during check"
  compose(text)
  keymap.invoke("<C-s>", { window = state.composer_win })
  assert(buffer.line(state.composer_buf, 1) == "",
    "Ctrl-S did not capture the replacement")
  wait_until(function() return settled() and rows():find(text, 1, true) end,
    "Ctrl-S did not complete its replacement", { timeout_ms = 4000, context = function()
      return vim.inspect({ operation = state.cleanup_operation_id,
        drafts = state.input.drafts, notice = state.execution_notice, busy = require("forge.views.harness.activity").busy(state),
        pause_first = pause_first, input = request("session.input.get"), queue = state.queue,
        steering = state.pending_steer, submitting = state.pending_submit, errors = errors,
        transcript = rows() })
    end })
  assert(request("session.check.status", {run_id = run_id}).state == "interrupted", "Ctrl-S did not interrupt check history")
  assert(buffer.line(state.composer_buf, 1) == "", "Ctrl-S left the submitted draft")
end
require("forge.views.harness.workspace").release(state)
if state.presentation then state.presentation.close() end
assert(host.stop(), "host did not stop")
errors:assert_none()
end
T["control_responsiveness"] = function()
local controller = require("forge.views.harness.controller")
local state = require("forge.session").harness
local client = require("forge.client")
local history = require("forge.views.harness.prompt_history")
local picker = require("forge.views.picker")
local settings = require("forge.views.harness.settings")
local catalog = require("forge.views.harness.provider_catalog")
local notifications = require("forge.infra.notifications")
local original_submit, original_drain = controller.submit, controller.drain
local original_configure = controller.configure
local composer = buffer.scratch()
state.session = { id = "responsive", provider = "mock", model = "test", effort = "medium" }
state.composer_buf, state.composer_win = composer, vim.api.nvim_get_current_win()
dofile("nvim/lua/forge/tests/support/activity.lua").set_busy(state, true)
state.queue = {}
local routed, configured = {}, {}
do
  controller.submit = function()
    routed[#routed + 1] = buffer.text(composer)
  end
  controller.drain, history.record = function() end, function() end
  local controls = { "/config", "/effort", "/effort high", "/model", "/read", "/write", "/yolo", "/ask", "/deny", "/allow",
    "/fast", "/ultrafast", "/skills", "/mcp", "/checks", "/rename", "/sessions",
    "/task refresh", "/agents on", "/agents edit", "/provider", "/questions" }
  for _, command in ipairs(controls) do
    buffer.set(composer, { command })
    controller.queue_submit()
    assert(routed[#routed] == command, command .. " was queued as provider text")
    assert(#state.queue == 0, command .. " waited behind active work")
  end
  controller.submit, controller.drain = original_submit, original_drain
  mock_client.respond({
    ["host.trace.status"] = { enabled = false },
    ["background.settings"] = function(spec, callback) callback({ revision = 0, settings = spec.params.defaults }) end,
    ["session.provider.model.list"] = {},
  })
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
  local discovery_requests = mock_client.record()
  state.model_provider, state.model_list = "mock", { { id = "test", reasoning = { "low", "medium", "high" } } }
  local delivered = false
  catalog.models(false, function(models) delivered = models[1].id == "test" end)
  assert(delivered and #discovery_requests == 0, "warm model controls waited for provider discovery")
  state.capability = { model_selection = true }
  state.composer_win = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(state.composer_win, composer)
  catalog.clear()
  state.model_list, state.model_provider = nil, nil
  local model_response
  mock_client.respond({
    ["provider.model.pin.list"] = {},
    ["session.provider.model.list"] = function(_, callback) model_response = callback end,
  })
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
  -- The session owner's FIFO orders configuration against active work, so Lua sends each
  -- change immediately and completes it from that change's own acknowledgement.
  local configuration_request = {}
  mock_client.respond({ ["session.configure"] = function(spec, callback)
    assert(spec.session_id == state.session.id)
    configuration_request[#configuration_request + 1] = { params = spec.params, callback = callback }
  end })
  local applied
  controller.configure({ effort = "high" }, function(result) applied = result end)
  assert(#configuration_request == 1 and configuration_request[1].params.effort == "high",
    "active configuration waited for work to stop before reaching the session owner")
  configuration_request[1].callback({})
  assert(applied == true, "successful active configuration was reported as unsuccessful")
  dofile("nvim/lua/forge/tests/support/activity.lua").set_busy(state, false)
  state.task_operation = nil
  configuration_request = {}
  local first_applied, second_applied
  controller.configure({ shell = "system" }, function(result) first_applied = result end)
  controller.configure({ plan_compact = true }, function(result) second_applied = result end)
  assert(#configuration_request == 2 and second_applied == nil)
  state.session.shell = "system"
  configuration_request[1].callback({})
  assert(#configuration_request == 2, "Lua delayed an explicit configuration")
  state.session.plan_compact = true
  configuration_request[2].callback({})
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
end
end
T["controller_native"] = function()
local activity_fixture = dofile("nvim/lua/forge/tests/support/activity.lua")
local client = require("forge.client")
local session = require("forge.session")
local controller = require("forge.views.harness.controller")
local original_plan_question = package.loaded["forge.views.harness.plan_question"]
local completion_visible = false
package.loaded["blink.cmp"] = { hide = function() completion_visible = false end }
client.host_is_accepting = function() return true end
local state = session.harness
local generation, submitted, receive, navigated, sync_count = 1, nil, nil, nil, 0
local initial = {}
local goal_request = {}
local status_thought_request = {}
local configuration_request = {}
local timeline_source = { "Native transcript" }
local staged_response, staged_request
local hold_staging, staging_reply, staging_count, fail_selection = false, nil, 0, false
local submitted_callback, cancellation_callback
local function snapshot(document, text, composer)
  return { document = document, revision = 0, block = { { id = "body", text = type(text) == "table" and text or { text },
    metadata = { target = {}, decoration = {}, editable_region = composer and { { id = "composer", revision = 0,
      range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = #text } } } } or {} } } } }
end
client.host_get_generation = function() return generation end
local subscribers = {}
receive = function(event, payload, owner)
  local current = {}
  for callback in pairs(subscribers) do current[#current + 1] = callback end
  for _, callback in ipairs(current) do callback(event, payload, owner) end
end
client.session_events_subscribe = function(callback)
  subscribers[callback] = true
  return function() subscribers[callback] = nil end
end
client.request = function(request_specification, callback)
  local session_id = request_specification.session_id
  local method = request_specification.method
  local params = request_specification.params or {}

  assert(session_id == "native-controller")
  if method == "session.input.get" then
    callback({ revision = state.input and state.input.revision or 0, messages = {}, suspended = false })
    return
  end
  if method == "session.input.apply" then submitted = params submitted_callback = callback return end
  if method == "session.turn.cancel" then cancellation_callback = callback return end
  if method == "session.configure" then
    local requests = type(params.status_thoughts) == "boolean" and status_thought_request or configuration_request
    requests[#requests + 1] = { params = params, callback = callback }
    return
  end
  if method == "session.command.apply" then
    goal_request[#goal_request + 1] = { method = method, params = params, callback = callback }
    return
  end
  if method == "session.health.get" then callback({ responsive = true }) return end
  if method == "session.state.get" then callback({ session = state.session }) return end
  if method == "session.prompt.history.record" then vim.schedule(function() callback({}) end) return end
  if method == "session.provider.model.list" then callback({}) return end
  assert(method == "session.document")
  if params.operation == "open" then
    initial[#initial + 1] = params
    vim.schedule(function() callback({ transcript = snapshot(params.document, "Native transcript"),
      status = snapshot(params.document .. ":status", "Idle") }) end)
  elseif params.operation == "navigate_prompt" then
    navigated = params.input
    vim.schedule(function() callback({ anchor = { block = "body", position = { row = 0, column = 0 } } }) end)
  elseif params.operation == "staging" then
    staging_count = staging_count + 1
    staged_request = vim.deepcopy(params)
    staged_response = snapshot(params.document, timeline_source)
    staged_response.revision = state.presentation.transcript.revision + 1
    local lines = {}
    if #params.steering > 0 then
      lines[#lines + 1] = "Steering active turn"
      for _, entry in ipairs(params.steering) do lines[#lines + 1] = type(entry) == "table" and entry.text or entry end
    end
    if #params.queue > 0 then
      lines[#lines + 1] = "Queued follow-up inputs"
      for _, entry in ipairs(params.queue) do lines[#lines + 1] = type(entry) == "table" and entry.text or entry end
    end
    if #lines > 0 then
      staged_response.block[#staged_response.block + 1] = { id = "harness:staging", text = lines,
        metadata = { target = {}, decoration = {}, editable_region = {} } }
    end
    if hold_staging then staging_reply = callback
    else vim.schedule(function() callback({ updated = true }) end) end
  elseif params.operation == "select_agent" and fail_selection then
    fail_selection = false
    vim.schedule(function() callback(nil, "selection unavailable", { code = "request_failed" }) end)
  elseif params.operation == "sync" then
    sync_count = sync_count + 1
    local result = staged_response and { snapshot = staged_response } or { patch = {} }
    result.status = snapshot(params.document .. ":status", "Idle")
    staged_response = nil
    vim.schedule(function() callback(result) end)
  else vim.schedule(function() callback({ patch = {} }) end) end
end
do
  state.transcript_buf, state.transcript_win, state.composer_buf, state.composer_win, state.timeline_tab = require("forge.views.harness.layout").open("native-controller")
  state.session = { id = "native-controller", provider = "mock", provider_label = "Mock CLI", model = "mock", resolved_model = "mock", approval_mode = "ask" }
  buffer.set(state.composer_buf, { "native draft" })
  state.initial_snapshot_pending = true
  controller.attach()
  controller.render()
  assert(not state.presentation, "startup opened the retained session before synchronization")
  local pending_title = vim.api.nvim_eval_statusline(vim.wo[state.transcript_win].winbar,
    { use_winbar = true, maxwidth = 200 }).str
  assert(not pending_title:find("rxw", 1, true) and not pending_title:find("mock", 1, true),
    "startup displayed stale permissions or model: " .. pending_title)
  controller.activate_snapshot({ session = state.session, access_abbreviation = "rxwx" })
  assert(not state.initial_snapshot_pending, "initial snapshot did not release startup presentation")
  wait_until(function() return state.presentation and state.presentation.ready end, "Harness presentation did not become ready")
  local previous_status = state.status
  local approval = { owner = "plan_review", plan_id = "review-plan", elicitation = {
    revision = 1, answer_revision = 1, question_set = { id = "approval-questions", questions = {
      { id = "mode", header = "Tool approval", question = "Which mode?", allow_freeform = false,
        options = { { label = "ask", description = "Ask for approval" }, { label = "deny", description = "Deny requests" } } },
    } },
    answer = { { question_id = "mode", response = { kind = "selected", option = "ask" }, source = "user" } },
  } }
  local opened, approval_callbacks
  package.loaded["forge.views.harness.plan_question"] = { open = function(elicitation, callbacks)
    assert(elicitation == approval.elicitation)
    opened = (opened or 0) + 1
    approval_callbacks = callbacks
  end }
  local live_request = client.request
  client.request = function(specification, callback)
    assert(specification.method == "session.document", "approval dialog dismissal sent a lifecycle RPC: " .. specification.method)
    return live_request(specification, callback)
  end
  state.active_elicitation = approval
  controller.present_plan_question(true)
  assert(state.plan_question_open and opened == 1)
  receive("document_changed", { session_id = "native-controller", revision = state.timeline_revision or 0,
    status = { kind = "awaiting_plan_review" } }, "native-controller")
  assert(state.plan_question_open and state.active_elicitation == approval, "answered review status discarded its explicit confirmation UI")
  approval_callbacks.closed()
  assert(not state.plan_question_open and state.active_elicitation == approval)
  assert(approval.elicitation.answer[1].response.option == "ask", "dialog dismissal discarded committed answers")
  approval.owner = "plan_acceptance"
  controller.present_plan_question(true)
  assert(state.plan_question_open and opened == 2, "dismissed approval questions could not reopen")
  approval_callbacks.closed()
  state.active_elicitation, state.presented_question_key = nil, nil
  state.status = previous_status
  client.request = live_request
  package.loaded["forge.views.harness.plan_question"] = original_plan_question
  state.access_abbreviation = nil
  local null_snapshot = vim.json.decode([[{
    "goal": null, "goal_execution": null, "active_plan": null,
    "active_elicitation": null, "active_wait": null,
    "approval": [], "artifact": [], "timeline": [], "prompt_history": null
  }]])
  null_snapshot.session = state.session
  controller.activate_snapshot(null_snapshot)
  assert(state.goal == nil and state.active_elicitation == nil, "startup retained nullable state")
  local original_state_request = client.request
  local synchronized = false
  client.request = function(request_specification, callback)
  local method = request_specification.method

    if method == "session.input.get" then
      callback({ revision = 0, messages = {}, suspended = false })
      return
    end
    assert(method == "session.state.get")
    callback(null_snapshot)
    synchronized = true
  end
  receive("plan_changes_requested", {}, "native-controller")
  assert(synchronized and not state.state_sync_pending, "reject snapshot synchronization did not settle")
  assert(state.goal == nil and state.active_elicitation == nil, "reject snapshot retained nullable state")
  local reads, completed = {}, 0
  state.runtime_epoch, state.timeline_revision, state.snapshot_revision = "ordering", 10, 20
  state.state_sync_callback = { function(result, failure)
    assert(result and not failure)
    completed = completed + 1
  end }
  client.request = function(request_specification, callback)
  local method = request_specification.method

    if method == "session.input.get" then
      callback({ revision = 0, messages = {}, suspended = false })
      return
    end
    assert(method == "session.state.get")
    reads[#reads + 1] = callback
  end
  receive("plan_changes_requested", {}, "native-controller")
  local stale = vim.deepcopy(null_snapshot)
  stale.runtime_epoch, stale.timeline_revision, stale.snapshot_revision = "ordering", 9, 19
  reads[1](stale)
  assert(completed == 0 and not state.sync_error, "an older snapshot failed pending callers")
  wait_until(function() return #reads == 2 end, "snapshot reconciliation did not issue a second state read")
  stale.timeline_revision, stale.snapshot_revision = 10, 20
  reads[2](stale)
  assert(completed == 1 and not state.sync_error, "snapshot reconciliation lost a waiting caller")
  state.runtime_epoch, state.timeline_revision, state.snapshot_revision = nil, 0, 0
  client.request = original_state_request
  assert(buffer.line(state.transcript_buf, 1) == "Native transcript")
  local approval_view = require("forge.views.harness.approval")
  local permission = { request_id = "navigation", execution_kind = "managed", targets = { {
    target_id = "command", title = "Run command", detail = "whoami.exe /groups", commands = {},
    choices = { { choice_id = "allow_once", label = "Allow once" } },
  } } }
  local pending_snapshot
  local snapshot_request = client.request
  local function approval_snapshot(pending)
    return { session = state.session, approval = pending, status = { kind = "working" } }
  end
  client.request = function(request_specification, callback)
  local session_id = request_specification.session_id
  local method = request_specification.method
  local params = request_specification.params or {}

    if method == "session.state.get" then pending_snapshot = callback return end
    return snapshot_request(request_specification, callback)
  end
  receive("state_invalidated", {}, "native-controller")
  local stale_snapshot = pending_snapshot
  receive("provider_event", { kind = "approval_requested", data = permission }, "native-controller")
  wait_until(approval_view.is_open, "approval did not open")
  stale_snapshot(approval_snapshot({}))
  assert(#state.approval == 1, "a snapshot captured before approval erased the pending request")
  wait_until(function() return pending_snapshot ~= stale_snapshot end,
    "a snapshot captured before approval erased the pending request")
  pending_snapshot(approval_snapshot({ permission }))
  keymap.invoke("q")
  assert(not approval_view.is_open() and #state.approval == 1, "q must only dismiss the approval window")
  vim.api.nvim_set_current_win(state.transcript_win)
  local timeline_window = vim.api.nvim_get_current_win()
  receive("state_invalidated", {}, "native-controller")
  pending_snapshot(approval_snapshot({ permission }))
  vim.wait(30, function() return false end, 5)
  assert(not approval_view.is_open() and vim.api.nvim_get_current_win() == timeline_window,
    "snapshot refresh reopened a dismissed approval and stole timeline focus")
  local notice = require("forge.views.harness.health").notice(state)
  assert(notice == nil, "dismissed approval replaced the native lifecycle status")
  controller.reopen_question()
  assert(approval_view.is_open(), "explicit reopen must restore the pending approval")
  keymap.invoke("q")
  receive("provider_event", { kind = "approval_cancelled", data = { request_id = permission.request_id } }, "native-controller")
  assert(#state.approval == 0 and not state.dismissed_approval_id)
  client.request = snapshot_request
  assert(vim.wo[state.transcript_win].breakindent, "native attachment discarded Harness continuation indentation")
  assert(vim.wo[state.composer_win].winbar == "", "composer still displays key hints")
  state.session.name = "Parser 100% coverage"
  controller.refresh_winbar()
  local title = vim.api.nvim_eval_statusline(vim.wo[state.transcript_win].winbar,
    { use_winbar = true, maxwidth = 200 }).str
  assert(title:find("rxw · sandbox · mock", 1, true), "winbar lost access, sandbox order, or model")
  state.questions = { revision = 1, questions = {
    { content = { question = "Which behavior should the function provide?" }, resolution = { kind = "pending" } },
  } }
  controller.refresh_winbar()
  assert(table.concat(state.presentation.status_view.content.lines, "\n"):find(
    "└─ Which behavior should the function provide?", 1, true), "controller omitted pending questions from status")
  assert(not vim.wo[state.transcript_win].winbar:find("question", 1, true), "question status remained in the winbar")
  state.questions = nil
  controller.refresh_winbar()
  assert(not table.concat(state.presentation.status_view.content.lines, "\n"):find("answer questions", 1, true))
  for _, sandbox in ipairs({ true, false }) do
    state.session.access = { sandbox = sandbox }
    for _, access in ipairs({ "r", "rx", "rw", "rxw", "rwx", "rxwx" }) do
      for _, approval in ipairs({ "ask", "deny", "allow" }) do
        for _, check_editing in ipairs({ false, true }) do
          state.check_editing = check_editing
          state.access_abbreviation, state.session.approval_mode = access, approval
          controller.refresh_winbar()
          local winbar = vim.wo[state.transcript_win].winbar
          local displayed = vim.api.nvim_eval_statusline(winbar, { use_winbar = true, maxwidth = 200 }).str
          local yolo = access == "rxwx" and approval == "allow" and not sandbox
          local policy_label = access .. (({ deny = "dx", allow = "ax" })[approval] or "")
            .. (check_editing and "ce" or "")
          assert(displayed:find(policy_label .. " · ", 1, true), displayed)
          assert(not displayed:find("check editing", 1, true), "check editing retained a separate field")
          assert(not displayed:find(" · " .. approval .. " · ", 1, true), "approval retained a separate field")
          assert((displayed:find("YOLO · ", 1, true) ~= nil) == yolo, displayed)
          assert(winbar:find("%#ForgeStatusLabel#" .. policy_label .. "%*", 1, true), "policy lost its gray status capture")
          assert((winbar:find("ForgeHarnessYoloIndicator", 1, true) ~= nil) == yolo)
          assert(not displayed:find("•", 1, true), "winbar mixed separator sizes")
          local _, dots = displayed:gsub("·", "")
          local _, gray_dots = winbar:gsub("%%#ForgeStatusHint# · %%%*", "")
          assert(dots > 0 and gray_dots == dots, "winbar separator inherited a value's highlight: " .. winbar)
          assert((displayed:find(" · sandbox · mock", 1, true) ~= nil) == sandbox,
            "sandbox must precede the model only when enabled")
        end
      end
    end
  end
  state.session.access = { sandbox = true }
  state.check_editing = false
  state.access_abbreviation, state.session.approval_mode = "rxw", "ask"
  local previous_effort = state.session.effort
  state.session.effort = "high"
  require("forge.infra.highlights").setup()
  assert(vim.api.nvim_get_hl(0, { name = "ForgeStatusLabel", link = false }).fg == 0xc0c0c0)
  assert(vim.deep_equal(vim.api.nvim_get_hl(0, { name = "ForgeHarnessUltrafast", link = false }),
    vim.api.nvim_get_hl(0, { name = "ForgeHarnessYoloIndicator", link = false })),
    "Ultrafast must use the same neon red as YOLO")
  for _, speed in ipairs({
    { tier = "default", text = "mock high", icon = nil },
    { tier = "fast", text = "󱐌 mock high", icon = "󱐌", group = "ForgeStatusLabel" },
    { tier = "ultrafast", text = "󱐋 mock high", icon = "󱐋", group = "ForgeHarnessUltrafast" },
  }) do
    state.session.service_tier = speed.tier
    controller.refresh_winbar()
    local winbar = vim.wo[state.transcript_win].winbar
    local displayed = vim.api.nvim_eval_statusline(winbar, { use_winbar = true, maxwidth = 200 }).str
    assert(displayed:find("sandbox · " .. speed.text, 1, true), displayed)
    assert(not displayed:find(" · Main", 1, true), "main agent label remains in the winbar")
    assert(not displayed:find("+", 1, true), "legacy speed marker remains")
    if speed.icon then
      assert(winbar:find("%#" .. speed.group .. "#" .. speed.icon .. "%*", 1, true), winbar)
      assert(winbar:find("%#ForgeStatusLabel# mock high%*", 1, true), "speed color leaked into model text")
    else
      assert(not displayed:find("󱐌", 1, true) and not displayed:find("󱐋", 1, true))
    end
  end
  state.session.effort, state.session.service_tier = previous_effort, nil
  state.configuration_error = "fixture rejected"
  controller.refresh_winbar()
  assert(vim.api.nvim_eval_statusline(vim.wo[state.transcript_win].winbar,
    { use_winbar = true, maxwidth = 200 }).str:find("Settings rejected: fixture rejected", 1, true),
    "configuration failure was hidden")
  state.configuration_error = nil
  controller.refresh_winbar()
  assert(not title:find("Mock CLI", 1, true), "winbar still shows the CLI provider")
  assert(not title:find("Parser 100% coverage", 1, true), "session name remains in top bar")
  assert(title:find(" · sandbox · ", 1, true), "sandbox indicator missing from top bar")
  state.session.access = { sandbox = false }
  controller.refresh_winbar()
  title = vim.api.nvim_eval_statusline(vim.wo[state.transcript_win].winbar,
    { use_winbar = true, maxwidth = 200 }).str
  assert(not title:find("sandbox", 1, true), "disabled sandbox remains in top bar")
  local tab_title = vim.api.nvim_eval_statusline(vim.o.tabline,
    { use_tabline = true, maxwidth = 200 }).str
  assert(tab_title:find("Parser 100% coverage", 1, true), "session name missing from tab")
  state.session.name = "123456789012345678901234567890 extra"
  controller.refresh_winbar()
  tab_title = vim.api.nvim_eval_statusline(vim.o.tabline,
    { use_tabline = true, maxwidth = 200 }).str
  assert(tab_title:find("123456789012345678901234567890", 1, true) and not tab_title:find("extra", 1, true),
    "tab must show only the first 30 session-name characters")
  activity_fixture.set_busy(state, true)
  local long_timeline = { "Native transcript" }
  for index = 1, vim.api.nvim_win_get_height(state.transcript_win) + 80 do
    long_timeline[#long_timeline + 1] = "Earlier response " .. index
  end
  local transcript = state.presentation.transcript
  timeline_source = long_timeline
  assert(require("forge.buffer").apply_snapshot(transcript, {
    document = transcript.document, revision = transcript.revision + 1,
    block = { { id = "body", text = long_timeline,
      metadata = { target = {}, decoration = {}, editable_region = {} } } },
  }).kind == "Applied")
  vim.api.nvim_set_current_win(state.composer_win)
  state.presentation.follow_tail()
  local staging_cursor = vim.api.nvim_win_get_cursor(state.transcript_win)
  state.queue = { { text = "queued follow-up" } }
  for index = 1, vim.api.nvim_win_get_height(state.transcript_win) + 5 do
    state.queue[#state.queue + 1] = { text = "queued extra " .. index }
  end
  state.pending_steer = { { text = "steering update" } }
  hold_staging = true
  controller.refresh_winbar()
  wait_until(function() return staging_reply ~= nil end, "staging request did not reach the host")
  local composer_height = vim.api.nvim_win_get_height(state.composer_win)
  local queue_namespace = vim.api.nvim_create_namespace("ForgeHarnessQueue")
  assert(#vim.api.nvim_buf_get_extmarks(state.composer_buf, queue_namespace, 0, -1, {}) == 0,
    "staging remained in the editable composer")
  buffer.set(state.composer_buf, { "typing draft" })
  state.queue[#state.queue].text = "latest queued update"
  controller.refresh_winbar()
  assert(staging_count == 1, "staging changes were not coalesced during an active request")
  fail_selection = true
  state.presentation.select_agent("missing-child")
  hold_staging = false
  staging_reply({ updated = true })
  assert(vim.api.nvim_win_get_height(state.composer_win) == composer_height,
    "refreshing staging resized the composer")
  wait_until(function()
    return vim.api.nvim_buf_get_lines(state.transcript_buf, -2, -1, false)[1] == state.queue[#state.queue].text
  end, "timeline staging is missing")
  assert(staged_request.queue[1] == "queued follow-up" and staged_request.steering[1] == "steering update",
    "staging request lost its messages")
  assert(staging_count == 2, "selection failure did not flush the latest staged messages")
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(state.transcript_win), staging_cursor),
    "passive queue refresh moved the transcript cursor")
  assert(vim.api.nvim_get_current_win() == state.composer_win,
    "passive queue refresh moved focus from the composer")
  vim.api.nvim_set_current_win(state.transcript_win)
  vim.api.nvim_win_set_cursor(state.transcript_win, { 1, 2 })
  vim.api.nvim_win_call(state.transcript_win, function() vim.cmd("normal! zt") end)
  local reading_view = vim.api.nvim_win_call(state.transcript_win, vim.fn.winsaveview)
  state.queue[#state.queue].text = "queue update while reading history"
  controller.refresh_winbar()
  wait_until(function()
    return vim.api.nvim_buf_get_lines(state.transcript_buf, -2, -1, false)[1] == state.queue[#state.queue].text
  end, "queue refresh did not publish while reading history")
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(state.transcript_win), { 1, 2 }),
    "staging refresh displaced intentional transcript browsing")
  assert(vim.deep_equal(vim.api.nvim_win_call(state.transcript_win, vim.fn.winsaveview), reading_view),
    "staging refresh scrolled a history reader")
  assert(vim.api.nvim_get_current_win() == state.transcript_win,
    "staging refresh moved focus from the history reader")
  state.queue, state.pending_steer = {}, {}
  buffer.set(state.composer_buf, { "native draft" })
  controller.refresh_winbar()
  wait_until(function()
    return vim.api.nvim_buf_line_count(state.transcript_buf) == #timeline_source
  end, "cleared messages remained staged")
  controller.configure({ status_thoughts = true })
  controller.configure({ status_thoughts = false })
  assert(#status_thought_request == 2 and #goal_request == 0,
    "status thought configuration entered the task-transition path")
  status_thought_request[2].callback({ status_thoughts = false })
  status_thought_request[1].callback({ status_thoughts = true })
  assert(state.status_thoughts == false and require("forge.views.harness.activity").busy(state),
    "a superseded status thought response changed the preference or interrupted work")
  state.session.model, state.session.resolved_model = "default", nil
  receive("provider_event", { kind = "runtime_resolved", data = {
    session_id = "native-controller", provider = "Mock CLI", model = vim.NIL,
  } }, "native-controller")
  assert(state.session.resolved_model == nil, "nullable native model became a truthy Lua sentinel")
  title = vim.api.nvim_eval_statusline(vim.wo[state.transcript_win].winbar, { use_winbar = true, maxwidth = 200 }).str
  assert(title:find("default", 1, true) and not title:find("unconfirmed", 1, true) and not title:find("vim.NIL", 1, true),
    "an unresolved session did not display its configured model")
  state.session.model, state.session.model_label = "saved-alias", "Saved Alias 5"
  controller.refresh_winbar()
  title = vim.api.nvim_eval_statusline(vim.wo[state.transcript_win].winbar, { use_winbar = true, maxwidth = 200 }).str
  assert(title:find("Saved Alias 5", 1, true) and not title:find("saved-alias", 1, true),
    "a session without a loaded catalog did not display its saved model label")
  state.session.model, state.session.model_label = "default", nil
  receive("provider_event", { kind = "runtime_resolved", data = {
    session_id = "native-controller", provider = "Mock CLI", model = "resolved-model",
  } }, "native-controller")
  title = vim.api.nvim_eval_statusline(vim.wo[state.transcript_win].winbar,
    { use_winbar = true, maxwidth = 200 }).str
  assert(title:find("resolved-model", 1, true) and not title:find("resolving model", 1, true),
    "running session did not publish the resolved model immediately")
  receive("provider_event", { kind = "runtime_resolved", data = {
    session_id = "other-session", model = "wrong-model",
  } }, "native-controller")
  assert(state.session.resolved_model == "resolved-model", "foreign runtime replaced the active model")
  state.session.model, state.session.resolved_model = "mock", nil
  state.recap = { text = "Old recap" }
  receive("provider_event", { kind = "turn_started" }, "native-controller")
  assert(state.recap == nil and require("forge.views.harness.activity").busy(state), "provider continuation retained recap or changed busy state")
  controller.refresh_winbar()
  assert(vim.wo[state.composer_win].winbar == "", "busy composer restored key hints")
  receive("session_changed", { snapshot = { runtime_epoch = state.runtime_epoch, snapshot_revision = (state.snapshot_revision or 0) + 1, activity = state.activity,
    access_abbreviation = state.access_abbreviation, capability = state.capability,
    session = { id = "native-controller", provider = "mock", model = "mock", approval_mode = "ask",
      active_task = { revision = "execution", kind = "execute", phase = "implement", status = "running", execution = {
        title = "Implement the design", record = { state = "active", phase = "implement" } } } },
  } }, "native-controller")
  assert(require("forge.views.harness.activity").busy(state) and state.session.active_task.kind == "execute" and state.session.approval_mode == "ask")
  title = vim.api.nvim_eval_statusline(vim.wo[state.transcript_win].winbar,
    { use_winbar = true, maxwidth = 200 }).str
  assert(title:find(" · exec", 1, true))
  assert(title:find("exec · implement", 1, true), "execution phase disappeared from the winbar")
  assert(not title:find("running", 1, true))
  state.access_abbreviation = "rxwx"
  for _, check_editing in ipairs({ false, true }) do
    state.check_editing = check_editing
    for _, enabled in ipairs({ true, false }) do
      state.session.active_task.auto_accept_plan = enabled
      controller.refresh_winbar()
      local displayed = vim.api.nvim_eval_statusline(vim.wo[state.transcript_win].winbar,
        { use_winbar = true, maxwidth = 200 }).str
      local policy_label = "rxwx" .. (check_editing and "ce" or "") .. (enabled and "pa" or "")
      assert(displayed:find(policy_label .. " · ", 1, true),
        "permission suffix did not follow authoritative task acceptance policy: " .. displayed)
      assert(not displayed:find("Auto-accept:", 1, true), "separate acceptance field remains in the winbar")
    end
  end
  state.access_abbreviation, state.check_editing = "rxw", false
  activity_fixture.set_busy(state, false)
  for _, status in ipairs({ "paused", "blocked" }) do
    state.session.active_task.execution.record.state = status
    state.session.active_task.status = status
    controller.refresh_winbar()
    title = vim.api.nvim_eval_statusline(vim.wo[state.transcript_win].winbar,
      { use_winbar = true, maxwidth = 200 }).str
    assert(title:find(" · exec", 1, true), "task disappeared from the winbar")
    assert(title:find("exec · implement", 1, true), "execution phase disappeared from the winbar")
    assert(title:find("exec · implement · " .. status, 1, true), "task suspension lost its phase")
  end
  state.session.active_task = nil
  controller.refresh_winbar()
  assert(not vim.wo[state.transcript_win].winbar:find("rxwpa", 1, true),
    "clearing the task retained its acceptance indicator")
  local previous_artifact = state.artifact
  state.artifact = { { id = "first" }, { id = "second" } }
  state.session.active_task = { kind = "plan", phase = "draft", status = "running", auto_accept_plan = true,
    plan = { paused = false, artifact = { title = "Plan fixture", state = "generating" } } }
  controller.refresh_winbar()
  local planning_status = vim.api.nvim_eval_statusline(vim.wo[state.transcript_win].winbar,
    { use_winbar = true, maxwidth = 200 }).str
  assert(planning_status:find("rxwpa · ", 1, true) and planning_status:find(" · plan", 1, true),
    "plan lost its task type or permission suffix")
  assert(planning_status:find("plan · draft", 1, true), "planning phase disappeared from the winbar")
  assert(planning_status:find("2 artefacts", 1, true), "artifact count disappeared from the winbar")
  local planning_winbar = vim.wo[state.transcript_win].winbar
  assert(planning_winbar:find("2 artefacts", 1, true) > planning_winbar:find("%=", 1, true),
    "artifact count remained in the left-aligned status")
  state.artifact, state.session.active_task = previous_artifact, nil
  controller.refresh_winbar()
  local original_window = state.transcript_win
  vim.api.nvim_set_current_win(original_window)
  vim.cmd("vsplit")
  local secondary = vim.api.nvim_get_current_win()
  state.presentation.refresh_views()
  wait_until(function() return state.presentation.views[secondary] ~= nil end, "secondary window did not become a Harness view")
  assert(vim.wo[secondary].breakindent and vim.wo[secondary].breakindentopt
    == require("forge.views.harness.layout").transcript_wrapping.options,
    "secondary Harness view did not retain continuation indentation")
  vim.api.nvim_win_set_cursor(original_window, { 1, 3 })
  vim.api.nvim_win_set_cursor(secondary, { 1, 5 })
  controller.jump_prompt(1)
  wait_until(function() return navigated ~= nil and vim.api.nvim_win_get_cursor(secondary)[2] == 0 end, "prompt navigation did not move the secondary view")
  assert(navigated.view == state.presentation.views[secondary].id)
  assert(vim.api.nvim_win_get_cursor(original_window)[2] == 3, "secondary navigation moved the initiating window")
  vim.api.nvim_win_close(original_window, true)
  wait_until(function() return state.presentation.views[original_window] == nil end, "closed window remained a Harness view")
  assert(not state.presentation.closed and state.presentation.views[secondary])
  local sync_before_submission = sync_count
  activity_fixture.set_busy(state, false)
  vim.api.nvim_set_current_win(state.composer_win)
  completion_visible = true
  controller.submit()
  assert(not completion_visible, "native submission retained completion for the consumed draft")
  assert(submitted and submitted.action.kind == "submit" and submitted.action.message.text == "native draft")
  assert(#state.queue == 0 and #state.pending_steer == 0 and #state.pending_submit == 1,
    "idle Ctrl-S entered a queue instead of submitting directly")
  assert(not require("forge.views.harness").provider_switch_available(),
    "direct submission lost the pending-input provider-switch guard")
  wait_until(function() return sync_count > sync_before_submission end,
    "optimistic submission did not refresh the native transcript", { timeout_ms = 2500 })
  assert(#staged_request.queue == 0 and #staged_request.steering == 0, "idle submission flashed in the visual queue")
  assert(buffer.line(state.composer_buf, 1) == "")
  submitted_callback({ operation_id = submitted.id, revision = state.input.revision + 1,
    suspended = false, upsert = {}, removed = {}, error = "simulated admission failure" })
  assert(#state.input.drafts == 1, "failed input draft was not retained")
  assert(buffer.line(state.composer_buf, 1) == "native draft")
  local input_state = require("forge.views.harness.input_state")
  local failed_id = state.input.drafts[1].id
  buffer.set(state.composer_buf, { "newer unsent draft" })
  keymap.invoke("<M-r>", { mode = "i" })
  assert(submitted.action.message.id == failed_id, "retry shortcut changed message identity")
  assert(buffer.line(state.composer_buf, 1) == "newer unsent draft",
    "retry shortcut overwrote newer typing")
  submitted_callback({ operation_id = submitted.id, revision = state.input.revision + 1,
    suspended = false, upsert = {}, removed = {}, error = "still rejected" })
  input_state.discard_rejected(state, state.input.drafts[1].id)
  activity_fixture.set_busy(state, true)
  buffer.set(state.composer_buf, { "explicit queued followup" })
  controller.queue_submit()
  local queued_message = vim.deepcopy(submitted.action.message)
  queued_message.revision = 1
  submitted_callback({ operation_id = submitted.id, revision = state.input.revision + 1,
    suspended = false, upsert = { queued_message }, removed = {} })
  buffer.set(state.composer_buf, { "steer the current turn" })
  controller.steer_submit()
  local steering_message = vim.deepcopy(submitted.action.message)
  steering_message.revision, steering_message.delivery, steering_message.route = 1, "delivering", "steering"
  submitted_callback({ operation_id = submitted.id, revision = state.input.revision + 1,
    suspended = false, upsert = { steering_message }, removed = {} })
  assert(steering_message.intent == "auto" and #state.queue == 1 and #state.pending_steer == 1,
    "Ctrl-S behind a queued message joined the explicit queue")
  wait_until(function()
    local text = buffer.text(state.transcript_buf)
    local steering = text:find("Steering active turn", 1, true)
    local queue = text:find("Queued follow-up inputs", 1, true)
    return steering and queue and steering < queue and text:find("steer the current turn", 1, true)
      and text:find("explicit queued followup", 1, true)
  end, "the separate steering section did not render above the explicit queue", { timeout_ms = 2500 })
  steering_message.delivery = "delivering"
  receive("input_changed", { revision = state.input.revision + 1, suspended = false,
    upsert = { steering_message }, removed = {} }, "native-controller")
  assert(#state.pending_steer == 1 and #state.queue == 1, "in-flight steering vanished before acceptance")
  steering_message.delivery = "accepted"
  receive("input_changed", { revision = state.input.revision + 1, suspended = false,
    upsert = { steering_message }, removed = {} }, "native-controller")
  assert(#state.pending_steer == 0 and #state.queue == 1, "steering acceptance changed the explicit queue")
  receive("input_changed", { revision = state.input.revision + 1, suspended = false,
    upsert = {}, removed = { steering_message.id, queued_message.id } }, "native-controller")
  buffer.set(state.composer_buf, { "" })
  vim.api.nvim_buf_set_text(state.composer_buf, 0, 0, 0, 0, { "draft after host collection" })
  generation = 2
  controller.render()
  wait_until(function() return state.presentation and state.presentation.ready and state.presentation.host_generation == 2 end, "presentation did not reopen on the new host generation")
  assert(initial[2].initial == nil, "new host received an unsent draft")
  assert(buffer.line(state.composer_buf, 1) == "draft after host collection")
  submitted = nil
  activity_fixture.set_busy(state, false)
  state.queue = { "queued while finalizing" }
  state.status = { kind = "finalizing", exchange_id = "exchange-1", error = "cleanup pending" }
  controller.render()
  assert(#state.queue == 1 and submitted == nil, "finalizing status drained queued input")
  state.status = { kind = "idle" }
  controller.render()
  assert(#state.queue == 1 and submitted == nil, "Lua independently dispatched host-owned input")
  state.queue = {}
  state.selected_agent_run_id = nil
  activity_fixture.set_busy(state, true)
  activity_fixture.set_busy(state, true)
  controller.cancel_turn()
  assert(cancellation_callback and require("forge.views.harness.activity").busy(state), "cancellation cleared busy state before settlement")
  cancellation_callback({})
  assert(require("forge.views.harness.activity").busy(state), "command reply manufactured idle activity")
  receive("activity_changed", { runtime_epoch = state.runtime_epoch,
    activity = { revision = (state.activity and state.activity.revision or 0) + 1, phase = "idle" } }, "native-controller")
  assert(not require("forge.views.harness.activity").busy(state), "owner idle event did not settle cancellation")
  local original_plain_request = client.request
  local child_request_for = client.request
  client.request = function(request_specification, callback)
  local session_id = request_specification.session_id
  local method = request_specification.method
  local params = request_specification.params or {}

    if method == "session.state.get" then return end
    return child_request_for(request_specification,callback)
  end
  local child_request = {}
  client.request = function(request_specification, callback)
  local method = request_specification.method
  local params = request_specification.params or {}

    if method == "session.prompt.history.record" or method == "session.state.get" then return end
    child_request[#child_request + 1] = { method = method, params = params }
    if method == "session.agent.interrupt" then callback({ status = "interrupting" }) end
  end
  activity_fixture.set_busy(state, true)
  state.capability.native_steer = true
  state.selected_agent_run_id = "child"
  state.agent = { run = { { id = "child", definition = "explorer", state = "ready" } }, exchange = {} }
  local child_exchange = { id = "child-exchange", agent_id = "child", state = "running",
    turn = { { state = { kind = "running" }, provider = { thread_id = "child-thread", turn_id = "child-turn" } } } }
  state.timeline = { { kind = "agent_lifecycle", run = state.agent.run[1], exchange = { child_exchange }, agent = {} } }
  state.agent.summary = { child = { state = "running", target = { thread_id = "child-thread", turn_id = "child-turn" } } }
  buffer.set(state.composer_buf, { "" })
  controller.cancel_turn()
  assert(child_request[1].method == "session.agent.interrupt" and child_request[1].params.target == "child")
  assert(not state.cancel_requested, "settled child cancellation retained the parent cancellation flag")
  buffer.set(state.composer_buf, { "child instruction" })
  controller.steer_submit()
  assert(child_request[2].method == "session.agent.message.send" and child_request[2].params.target == "child")
  state.cancel_requested = false
  child_exchange.state = "complete"
  state.agent.summary.child = { state = "complete" }
  buffer.set(state.composer_buf, { "" })
  controller.cancel_turn()
  assert(child_request[#child_request].method == "session.agent.interrupt" and child_request[#child_request].params.target == "child",
    "completed-child interruption must remain scoped to the child")
  state.agent.run[1].task_name = "parser-review"
  state.agent.run[1].created_at_ms = os.time() * 1000
  state.agent.summary.child = { state = "running", tool_count = 1, failed_count = 0 }
  controller.open_agent_picker()
  local picker = require("forge.views.picker")
  local picker_state = require("forge.views.picker.state")
  local picker_instance = picker._state_for_test()
  assert(picker_state.selected_option(picker_instance.state,picker_instance.spec).value.run.id == "child")
  local updated_agent = vim.deepcopy(state.agent)
  updated_agent.summary.child = { state = "running", tool_count = 7, failed_count = 1 }
  receive("provider_event", {kind="agent_updated",data=updated_agent},"native-controller")
  picker_instance = picker._state_for_test()
  local selected = picker_state.selected_option(picker_instance.state,picker_instance.spec)
  assert(selected.value.run.id == "child" and selected.detail:find("7 tools called (1 failed)",1,true),
    "live agent events must refresh metrics without changing selection or polling state")
  picker.close(true)
  local child_permission = vim.deepcopy(permission)
  child_permission.request_id = "child-approval"
  child_permission.provider = "Fixture Alpha"
  child_permission.execution_owner = {session_id="native-controller",agent_id="child"}
  state.selected_agent_run_id = "different-child"
  receive("provider_event", {kind="approval_requested",data=child_permission},"native-controller")
  wait_until(approval_view.is_open, "child approval did not open")
  picker_instance = picker._state_for_test()
  local approval_page = picker_state.page(picker_instance.state,picker_instance.spec)
  assert(approval_page.content_list[1].text == "Agent parser-review · Fixture Alpha",
    "approval must identify its requesting child")
  local interrupt_key = require("forge.shared.keymaps").view_keys_for("harness","cancel")[1]
  keymap.invoke(interrupt_key)
  assert(child_request[#child_request].params.target == "child",
    "approval interruption must ignore the selected timeline and target its owner")
  receive("provider_event", {kind="approval_cancelled",data={request_id=child_permission.request_id}},"native-controller")
  state.cancel_requested = false
  local root_permission = vim.deepcopy(permission)
  root_permission.request_id = "root-approval"
  root_permission.execution_owner = {session_id="native-controller",agent_id="native-controller:agent:primary"}
  receive("provider_event", {kind="approval_requested",data=root_permission},"native-controller")
  wait_until(approval_view.is_open, "root approval did not open")
  keymap.invoke(interrupt_key)
  assert(child_request[#child_request].method == "session.turn.cancel",
    "root approval interruption must remain root-owned while a child is selected")
  receive("provider_event", {kind="approval_cancelled",data={request_id=root_permission.request_id}},"native-controller")
  client.request = original_plain_request
  client.request = child_request_for
  state.subagent_tools = false
  for _, enabled in ipairs({true,false}) do
    local applied
    controller.configure({subagent_tools=enabled},function(result) applied = result end)
    local request = configuration_request[#configuration_request]
    assert(applied == nil and state.subagent_tools ~= enabled, "unacknowledged command changed committed tools")
    local revision = (state.snapshot_revision or 0) + 1
    local committed = { session=vim.deepcopy(state.session), runtime_epoch=state.runtime_epoch,
      snapshot_revision=revision, subagent_tools=enabled, capability=state.capability }
    receive("session_configured", {snapshot=committed},"native-controller")
    assert(state.subagent_tools == enabled and applied == nil, "configuration event did not update tools independently of its waiter")
    local stale = vim.deepcopy(committed)
    stale.snapshot_revision, stale.subagent_tools = revision - 1, not enabled
    receive("session_configured", {snapshot=stale},"native-controller")
    assert(state.subagent_tools == enabled, "stale configuration replaced committed tools")
    request.callback({})
    assert(applied == true, "configuration command did not notify its waiter")
  end
  require("forge.views.harness.workspace").release(state)
  state.presentation.close()
  state.presentation = nil
  state.composer_buf = buffer.scratch()
  state.composer_win = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(state.composer_win, state.composer_buf)
  activity_fixture.set_busy(state, true)
  state.active_wait, state.selected_agent_run_id = {}, "child"
  state.queue = {}
  buffer.set(state.composer_buf, { "explicit follow-up" })
  require("forge.shared.keymaps").setup_view_keymaps(state.composer_buf, "harness", controller.command_set())
  keymap.invoke("<C-q>", { mode = "i" })
  assert(state.queue[1].text == "explicit follow-up", "queue key steered an active wait or selected child")
  assert(buffer.line(state.composer_buf, 1) == "")
  assert(keymap.get("<C-s>", { mode = "i" }).desc == "Submit the composer")
  state.active_wait = nil
  state.selected_agent_run_id = nil
  activity_fixture.set_busy(state, true)
  state.queue = { "keep queued work" }
  for _, command in ipairs({ "pause", "clear" }) do
    buffer.set(state.composer_buf, { "/goal " .. command })
    controller.submit()
    assert(goal_request[#goal_request] and goal_request[#goal_request].params.action == command,
      "goal control was queued behind active work")
    assert(#state.queue == 1 and state.queue[1] == "keep queued work")
    assert(require("forge.views.harness.activity").busy(state), "goal control cleared the running turn state")
    goal_request[#goal_request].callback({ state = "accepted" })
    assert(require("forge.views.harness.activity").busy(state), "goal acknowledgement cleared the running turn state")
  end
  receive("goal_changed", { state = "cleared" }, "native-controller")
  local profiles = require("forge.views.harness.subagent_profiles")
  local routed = {}
  profiles.open = function(selected_state, _, profile) assert(selected_state == state) routed = { "edit", profile } end
  controller.spawn_agent = function(profile, text) routed = { "spawn", profile, text } end
  controller.open_spawn_picker = function(profile) routed = { "spawn_picker", profile } end
  controller.open_agent_picker = function() routed = { "agents" } end
  controller.select_agent = function(name) routed = { "select", name } end
  controller.configure = function(params)
    assert(vim.deep_equal(params, { subagent_tools = params.subagent_tools }))
    routed = { "tools", params.subagent_tools }
  end
  state.selected_agent_run_id = "child"
  for _, command in ipairs({
    { "/agents on", { "tools", true } },
    { "/agents on", { "tools", true } },
    { "/agents off", { "tools", false } },
    { "/agents off", { "tools", false } },
    { "/agents edit", { "edit" } },
    { "/agents edit reviewer", { "edit", "reviewer" } },
    { "/agents spawn reviewer Inspect parser", { "spawn", "reviewer", "Inspect parser" } },
    { "/agents spawn reviewer", { "spawn_picker", "reviewer" } },
    { "/agents spawn", { "spawn_picker" } },
    { "/agents", { "agents" } },
    { "/agents main", { "select", "main" } },
  }) do
    buffer.set(state.composer_buf, { command[1] })
    controller.submit()
    assert(vim.deep_equal(routed, command[2]), "incorrect route for " .. command[1])
    assert(buffer.line(state.composer_buf, 1) == "")
    assert(#state.queue == 1 and state.queue[1] == "keep queued work", "agent command was queued as a prompt")
  end
  for _, text in ipairs({ "/agent edit", "/agent edit reviewer" }) do
    routed = {}
    buffer.set(state.composer_buf, { text })
    controller.submit()
    assert(vim.tbl_isempty(routed), "singular agent command must not route")
    assert(buffer.line(state.composer_buf, 1) == text)
    assert(#state.queue == 1 and state.queue[1] == "keep queued work", "singular agent command was queued")
  end
end
end
T["gutter_selection"] = function()
local runtime = vim.fs.normalize(vim.fn.fnamemodify("nvim", ":p"))
local child = MiniTest.new_child_neovim()
child.start({ "-u", "NONE", "-i", "NONE", "--noplugin" })
MiniTest.finally(function() pcall(child.stop) end)
local run = child.lua
do
  child.api.nvim_ui_attach(72, 28, { rgb = true })
  run([[vim.api.nvim__inspect_cell(1, 0, 0)]])
  run([[
    local runtime = ...
    vim.opt.rtp:prepend(runtime)
    _G.keymap = dofile("nvim/lua/forge/tests/support/keymap.lua")
    _G.wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
    vim.o.laststatus, vim.o.showtabline, vim.o.showmode, vim.o.ruler = 0, 0, false, false
    vim.api.nvim_set_hl(0, "Normal", { fg = 0xffffff, bg = 0x000000 })
    vim.api.nvim_set_hl(0, "Visual", { bg = 0x123456 })
    vim.api.nvim_set_hl(0, "DiffAdd", { fg = 0x00ff00, bg = 0x002800 })
    vim.api.nvim_set_hl(0, "DiffDelete", { fg = 0xff0000, bg = 0x280000 })
    local config = require("forge.infra.config")
    config.setup({})
    local client = require("forge.client")
    client.host_is_accepting = function() return true end
    client.host_get_generation = function() return 1 end
    client.session_events_subscribe = function() return function() end end
    _G.open_response = nil
    client.request = function(request_specification, callback)
  local method = request_specification.method
  local params = request_specification.params or {}

  callback = dofile("nvim/lua/forge/tests/support/presentation_status.lua")(params, callback)
      if method == "session.document" and params.operation == "open" then
        open_response = { document = params.document, callback = callback }
      elseif method == "session.document" and params.operation == "background_terminals" then
        callback({ supported = false })
      elseif method == "session.health.get" then callback({ responsive = true })
      elseif method == "session.input.get" then
        callback({ revision = 0, messages = {}, suspended = false, execution_busy = false })
      else callback({ patch = {} }) end
    end
    _G.state = require("forge.session").harness
    state.session = { id = "gutter-selection", provider = "mock", model = "mock", approval_mode = "ask" }
    state.transcript_buf, state.transcript_win, state.composer_buf, state.composer_win, state.timeline_tab =
      require("forge.views.harness.layout").open("gutter-selection")
    _G.controller = require("forge.views.harness.controller")
    controller.attach()
    vim.api.nvim_set_current_win(state.transcript_win)
    _G.presentation = require("forge.views.harness.presentation").open({
      session_id = state.session.id, transcript_buffer = state.transcript_buf,
      transcript_window = state.transcript_win, composer_buffer = state.composer_buf,
      is_alive = function() return true end, notice = function(message) error(message) end,
    }, function(owner, message) assert(owner, message) end)
    state.presentation = presentation
    _G.source = {
      "Modified src/presentation.rs +14 -7", "@@ -6,2 +6,3 @@",
      "use bevy::prelude::*;", "use bevy::text::FontSize;",
      "use bevy::transform::TransformSystems;", "",
      "    a long source line that wraps across the narrow transcript window and continues onto another screen row",
    }
    local metadata = { target = {}, editable_region = {}, decoration = {}, gutter = {}, layout = { indent = 4 } }
    for row = 2, 6 do
      local capture = row == 3 and "DiffDelete" or row == 4 and "DiffAdd" or "LineNr"
      local sign = row == 3 and "-" or row == 4 and "+" or " "
      metadata.gutter[#metadata.gutter + 1] = { position = { row = row, column = 0 }, priority = 100,
        chunk = { { text = "  6", capture = capture }, { text = "  ", capture = capture },
          { text = "7 " .. sign .. " ", capture = capture } } }
    end
    open_response.callback({ transcript = { document = open_response.document, revision = 0,
      block = { { id = "diff", text = source, metadata = metadata } } } })
    assert(presentation.ready)
    _G.prior_copy_count = 0
    vim.keymap.set("x", "<Space>l", function() prior_copy_count = prior_copy_count + 1 end,
      { buffer = state.transcript_buf })
    vim.g.clipboard = { name = "Harness fixture", copy = {
      ["+"] = function(lines, kind) _G.clipboard = { lines = lines, kind = kind } end,
      ["*"] = function() end,
    }, paste = { ["+"] = function() return { {}, "V" } end, ["*"] = function() return { {}, "V" } end } }
    _G.move = function(row)
      vim.api.nvim_win_set_cursor(state.transcript_win, { row, 0 })
      vim.api.nvim_exec_autocmds("CursorMoved", { buffer = state.transcript_buf })
    end
    _G.escape = function()
      keymap.feed("<Esc>", "nx")
      vim.api.nvim_exec_autocmds("ModeChanged", {})
    end
    _G.start = function(row)
      move(row)
      keymap.invoke("W")
      assert(vim.api.nvim_get_mode().mode == "V")
    end
    vim.wo[state.transcript_win].winbar = ""
    move(3)
    assert(vim.fn.getcurpos()[4] == 0, "ordinary timeline cursor gained a gutter offset")
    start(4)
    move(6)
    assert(presentation.transcript.gutter_selection.first == 4 and presentation.transcript.gutter_selection.last == 6)
    move(3)
    assert(presentation.transcript.gutter_selection.first == 3 and presentation.transcript.gutter_selection.last == 4)
    escape()
    start(3)
    move(5)
  ]], { runtime })
  run([[
    assert(vim.api.nvim_get_mode().mode == "V", "selection mode changed between RPC calls: " .. vim.inspect(vim.api.nvim_get_mode()))
    vim.cmd("redraw!")
    local selection = presentation.transcript.gutter_selection
    for row = selection.first, selection.last do
      local position = vim.fn.screenpos(state.transcript_win, row, 1)
      local cursor_column = position.col + require("forge.gutter").bounds(presentation.transcript, row).width
      for column = position.col, cursor_column + #source[row] - 1 do
        if row ~= vim.fn.line(".") or column ~= cursor_column then
          assert(vim.api.nvim__inspect_cell(1, position.row - 1, column - 1)[2].background == 0x123456,
            "selected text or gutter cell lost Visual at " .. row .. ":" .. column)
        end
      end
    end
    escape()
    assert(presentation.transcript.gutter_selection == nil)
    keymap.invoke("<Space>l", { mode = "x" })
    assert(prior_copy_count == 1)
    start(3)
    move(6)
    local selected_column = {}
    for row = 3, 6 do
      selected_column[row] = vim.api.nvim_eval_statusline(vim.wo.statuscolumn, {
        winid = state.transcript_win, use_statuscol_lnum = row,
      }).str
    end
    keymap.invoke("<Space>l", { mode = "x" })
    assert(clipboard.kind == "V")
    for index = 1, 4 do
      local layout = require("forge.gutter").bounds(presentation.transcript, index + 2)
      local prefix = {}
      for _, insertion in ipairs(layout.insertion) do
        for _, chunk in ipairs(insertion.chunk) do prefix[#prefix + 1] = chunk.text end
      end
      local column = selected_column[index + 2]
      assert(clipboard.lines[index] == column .. table.concat(prefix) .. source[index + 2],
        "copied gutter differs from display: " .. vim.inspect({
          actual = clipboard.lines[index], expected = column .. table.concat(prefix) .. source[index + 2],
        }))
    end
    assert(presentation.transcript.gutter_selection == nil and vim.api.nvim_get_mode().mode == "n")
    assert(vim.deep_equal(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, true), source))
    start(6)
    vim.cmd("redraw!")
    local position = vim.fn.screenpos(state.transcript_win, 6, 1)
    assert(vim.api.nvim__inspect_cell(1, position.row - 1, position.col - 1)[2].background == 0x123456,
      "empty row gutter lost Visual")
    escape()
    start(7)
    vim.cmd("redraw!")
    position = vim.fn.screenpos(state.transcript_win, 7, 1)
    assert(vim.api.nvim__inspect_cell(1, position.row, position.col + 11)[2].background == 0x123456,
      "wrapped continuation lost Visual")
    escape()
    vim.cmd("vsplit")
    _G.other_window = vim.api.nvim_get_current_win()
    vim.api.nvim_set_current_win(state.transcript_win)
    start(4)
    vim.cmd("redraw!")
    local other = vim.fn.screenpos(other_window, 4, 1)
    assert(vim.api.nvim__inspect_cell(1, other.row - 1, other.col - 1)[2].background ~= 0x123456,
      "selection leaked into another split")
    escape()
    vim.api.nvim_win_close(other_window, true)
    start(4)
    vim.api.nvim_set_current_win(state.composer_win)
    assert(presentation.transcript.gutter_selection == nil, "buffer leave retained selection")
    assert(vim.fn.maparg("W", "n", false, true).buffer ~= 1, "composer binds gutter selection")
    escape()
    vim.api.nvim_set_current_win(state.transcript_win)
    keymap.invoke("?")
    local help = table.concat(vim.api.nvim_buf_get_lines(0, 0, -1, true), "\n")
    assert(help:find("W", 1, true) and help:find("including the diff gutter", 1, true), "help omitted W: " .. help)
    keymap.invoke("q")
    local config = require("forge.infra.config")
    config.setup({ keymaps = { harness = { visual_line_with_gutter = "gW" } } })
    vim.keymap.del("n", "W", { buffer = state.transcript_buf })
    controller.attach_transcript(state.transcript_buf)
    assert(keymap.get("gW").buffer == 1)
    config.setup({ keymaps = { harness = { visual_line_with_gutter = false } } })
    vim.keymap.del("n", "gW", { buffer = state.transcript_buf })
    controller.attach_transcript(state.transcript_buf)
    assert(vim.fn.maparg("gW", "n", false, true).buffer ~= 1 and vim.fn.maparg("W", "n", false, true).buffer ~= 1)
    config.setup({})
    controller.attach_transcript(state.transcript_buf)
    start(4)
    presentation.close({ preserve_buffer = true })
    assert(presentation.selection.closed and presentation.transcript.gutter_selection == nil)
    keymap.invoke("<Space>l", { mode = "x" })
    assert(prior_copy_count == 2, "close did not restore prior clipboard mapping")
    escape()
    local rejected = false
    local pending = require("forge.views.harness.presentation").open({
      session_id = state.session.id, transcript_buffer = state.transcript_buf,
      transcript_window = state.transcript_win, composer_buffer = state.composer_buf,
      is_alive = function() return true end, notice = function(message) error(message) end,
    }, function(owner, message) rejected = owner == nil and message == "fixture rejection" end)
    state.presentation = pending
    keymap.invoke("W")
    assert(vim.api.nvim_get_mode().mode == "n", "pending presentation admitted selection")
    open_response.callback(nil, "fixture rejection", { code = "invalid_state" })
    assert(rejected and pending.selection.closed, "rejected startup retained selection owner")
    assert(vim.fn.exists("#ForgeGutterSelection" .. state.transcript_buf) == 0,
      "rejected startup retained selection callbacks")
    wait_until(function() return pending.closed end, "rejected presentation did not close")
    local replica = require("forge.buffer")
    local input = require("forge.input")
    for _, indent in ipairs({ 2, 4, 6 }) do
      vim.cmd("tabnew")
      local prompt_window = vim.api.nvim_get_current_win()
      local prompt = replica.open("prompt-alignment-" .. indent)
      vim.api.nvim_set_current_win(prompt_window)
      vim.api.nvim_win_set_buf(prompt_window, prompt.buffer)
      assert(replica.apply_snapshot(prompt, { document = prompt.document, revision = 0, block = {
        { id = "prompt", text = { "● first prompt line", "second prompt line", "third prompt line" },
          metadata = { target = {}, decoration = {}, editable_region = {},
            layout = { indent = indent, marker = { text = "●", capture = "Normal" } },
            conceal = { { range = { start = { row = 0, column = 0 },
              ["end"] = { row = 0, column = #"● " } }, replacement = "", line = false, priority = 100 } },
          } },
        { id = "activity", text = { "▸ Thinking" },
          metadata = { target = {}, decoration = {}, editable_region = {},
            layout = { indent = indent, marker = { text = "▸", capture = "Normal" } },
            conceal = { { range = { start = { row = 0, column = 0 },
              ["end"] = { row = 0, column = #"▸ " } }, replacement = "", line = false, priority = 100 } },
          } },
        { id = "status", text = { "Idle" }, metadata = {
          target = {}, decoration = {}, editable_region = {}, status = { row = 0, animated = false },
        } },
      } }).kind == "Applied")
      vim.api.nvim_buf_set_extmark(prompt.buffer, vim.api.nvim_create_namespace("GutterSelectionStatusMarker"), 4, 0, {
        sign_text = "◇", sign_hl_group = "Normal",
      })
      local view = input.open(prompt, prompt_window, { margin = 0,
        columns = { signcolumn = "yes:1", statuscolumn = "%s" },
        fold_markers = true,
        conceal = { level = 3, cursor = "nvic" },
      })
      vim.cmd("normal! ggzt")
      vim.cmd("redraw!")
      local first = vim.fn.screenpos(prompt_window, 1, 1)
      local text_column = vim.api.nvim_win_get_position(prompt_window)[2] + indent
      assert(vim.api.nvim__inspect_cell(1, first.row - 1, text_column)[1] == "f",
        "prompt text does not follow its displayed marker at indent " .. indent)
      for row = 2, 3 do
        local continuation = vim.fn.screenpos(prompt_window, row, 1)
        local expected = row == 2 and "s" or "t"
        assert(vim.api.nvim__inspect_cell(1, continuation.row - 1, text_column)[1] == expected,
          "prompt continuation does not align with first-line text at indent " .. indent)
      end
      local selection = require("forge.document_commands").attach_selection(prompt, { normalize = false })
      vim.api.nvim_win_set_cursor(prompt_window, { 1, 0 })
      selection.start()
      vim.api.nvim_win_set_cursor(prompt_window, { 5, 0 })
      vim.api.nvim_exec_autocmds("CursorMoved", { buffer = prompt.buffer })
      vim.cmd("redraw!")
      for row = 1, 5 do
        local position = vim.fn.screenpos(prompt_window, row, 1)
        local left = vim.api.nvim_win_get_position(prompt_window)[2]
        for column = left, left + indent - 1 do
          assert(vim.api.nvim__inspect_cell(1, position.row - 1, column)[2].background == 0x123456,
            "timeline gutter lost Visual at indent " .. indent .. " row " .. row .. " column " .. column)
        end
        local right = left + vim.api.nvim_win_get_width(prompt_window) - 1
        assert(vim.api.nvim__inspect_cell(1, position.row - 1, right)[2].background == 0x123456,
          "whole-line selection stopped before the right edge")
      end
      keymap.invoke("<Space>l", { mode = "x" })
      local marker_indent = string.rep(" ", math.max(0, indent - 2))
      assert(clipboard.lines[1] == marker_indent .. "● first prompt line",
        "copied prompt lost its displayed marker: " .. vim.inspect(clipboard.lines))
      assert(clipboard.lines[2] == string.rep(" ", indent) .. "second prompt line",
        "copied continuation lost its indentation")
      assert(clipboard.lines[4] == marker_indent .. "▾ Thinking",
        "copied activity did not use its displayed expansion marker: " .. vim.inspect(clipboard.lines))
      assert(clipboard.lines[5] == "◇ Idle", "copy lost the displayed status sign: " .. vim.inspect(clipboard.lines))
      selection.close()
      input.close(view)
      replica.close(prompt)
    end
  ]])
end
end
T["input_timeout"] = function()
vim.o.columns = 130
require("forge").setup({ harness = { provider = "mock" } })
local picker = require("forge.views.picker")
local approval = require("forge.views.harness.approval")
local settings = require("forge.views.harness.settings")
local controller = require("forge.views.harness.controller")
local client = require("forge.client")
local duration = require("forge.views.harness.duration")
local host = { window_list = { vim.api.nvim_get_current_win() }, control_win = vim.api.nvim_get_current_win(), closed = function() end }
-- The picker measures elapsed countdown time with `vim.uv.hrtime` and redraws from a libuv
-- timer. The fixture advances that clock by hand and captures the tick instead of waiting
-- for a real second to pass.
local original_hrtime, original_new_timer = vim.uv.hrtime, vim.uv.new_timer
local elapsed_ns = 0
local countdown_tick ---@type fun()?
vim.uv.hrtime = function() return original_hrtime() + elapsed_ns end
vim.uv.new_timer = function()
  local timer = assert(original_new_timer())
  return {
    start = function(_, _, _, callback) countdown_tick = callback return 0 end,
    stop = function() return timer:stop() end,
    close = function() return timer:close() end,
    is_closing = function() return timer:is_closing() end,
  }
end
local seconds, microseconds = vim.uv.gettimeofday()
local deadline = seconds * 1000 + microseconds / 1000 + 62000
approval.open({ request_id = "timeout-request", execution_kind = "managed", deadline = { expires_at_ms = deadline }, targets = {
  { target_id = "item", title = "Read", detail = "Inspect source", commands = {}, choices = { { choice_id = "deny_once", label = "Deny once" } } },
} },vim.tbl_extend("force", host, { resolve = function() error("UI must not resolve the deadline") end }))
vim.uv.new_timer = original_new_timer
local instance = picker._state_for_test()
local before = vim.api.nvim_buf_get_changedtick(instance.buf)
local cursor = vim.api.nvim_win_get_cursor(instance.win)
local function title()
  local chunks = vim.api.nvim_win_get_config(instance.win).title
  local text = {}
  for _, chunk in ipairs(chunks) do text[#text + 1] = chunk[1] end
  return table.concat(text)
end
assert(title():find("01:02", 1, true), title())
assert(countdown_tick, "picker did not start its countdown timer")
elapsed_ns = 1000 * 1000000
countdown_tick()
-- The tick is schedule-wrapped, so the wait only drains the scheduled redraw.
wait_until(function() return title():find("01:01", 1, true) ~= nil end, "countdown did not redraw to 01:01", { context = title })
vim.uv.hrtime = original_hrtime
assert(vim.api.nvim_buf_get_changedtick(instance.buf) == before, "countdown rewrote the body")
assert(vim.deep_equal(vim.api.nvim_win_get_cursor(instance.win), cursor), "countdown moved cursor")
local timer = instance.timer
picker.close(false)
assert(timer:is_closing(), "picker timer leaked")

local configured = { id = "timeout-settings", provider = "mock", approval_timeout = { amount = 10, unit = "m" } }
local state = { session = configured }
local original_configure = controller.configure
local saved = 0
local deferred, pending_save
mock_client.respond({
  ["host.trace.status"] = { enabled = false },
  ["session.provider.model.list"] = {},
  ["background.settings"] = function(spec, callback) callback({ revision = 0, settings = spec.params.defaults }) end,
})
controller.configure = function(value, completed)
  saved = saved + 1
  local function complete(applied)
    if applied then for key, entry in pairs(value) do configured[key] = entry end end
    completed(applied)
  end
  if deferred then pending_save = complete else complete(true) end
end
local function press(target, key)
  keymap.invoke(key, { buffer = target })
end
local function edit(id)
  local current = picker._state_for_test()
  for page_index, page in ipairs(current.spec.page_list) do
    for index, option in ipairs(page.option_list) do
      if option.id == id then
        assert(#option.fields == 1, "timeout uses separate amount and unit fields")
        current.state.page_index = page_index
        current.state.selected_index_by_page[page.id] = index
        picker.update(current.spec)
        press(current.buf, "<CR>")
        return assert(current.field_editor)
      end
    end
  end
  error("missing timeout " .. id)
end
local function shown(editor)
  return buffer.line(editor.buffer, 1)
end
settings.open(state, host)
local editor = edit("approval_timeout")
assert(shown(editor) == "10m" and not vim.bo[editor.buffer].modifiable)
press(editor.buffer, "<Up>")
assert(shown(editor) == "11m")
press(editor.buffer, "<Down>")
assert(shown(editor) == "10m")
press(editor.buffer, "<Esc>")
assert(saved == 0 and configured.approval_timeout.amount == 10, "cancel saved the draft")
editor = edit("approval_timeout")
for minute = 9, 2, -1 do
  press(editor.buffer, "<Down>")
  assert(shown(editor) == minute .. "m")
end
for _, label in ipairs({ "60s", "45s", "30s", "15s", "15s" }) do
  press(editor.buffer, "<Down>")
  assert(shown(editor) == label, "incorrect lower-bound step: " .. shown(editor))
end
assert(saved == 0, "stepping saved before confirmation")
press(editor.buffer, "<CR>")
assert(saved == 1 and configured.approval_timeout.amount == 15 and configured.approval_timeout.unit == "s")
editor = edit("approval_timeout")
local choices = duration.choices()
assert(#choices == 63)
for index = 2, #choices do
  press(editor.buffer, "<Up>")
  assert(shown(editor) == choices[index], "incorrect increment at " .. choices[index])
end
press(editor.buffer, "<Up>")
assert(shown(editor) == "60m", "maximum wrapped or exceeded 60 minutes")
press(editor.buffer, "<CR>")
assert(saved == 2 and configured.approval_timeout.amount == 60 and configured.approval_timeout.unit == "m")
configured.approval_timeout = { amount = 10, unit = "m" }
settings.open(state, host)
editor = edit("approval_timeout")
press(editor.buffer, "<Down>")
press(editor.buffer, "<CR>")
assert(saved == 3 and configured.approval_timeout.amount == 9 and configured.approval_timeout.unit == "m")
deferred = true
editor = edit("approval_timeout")
press(editor.buffer, "<Down>")
assert(shown(editor) == "8m")
press(editor.buffer, "<CR>")
press(editor.buffer, "<Up>")
press(editor.buffer, "<Esc>")
assert(editor.pending and shown(editor) == "8m" and saved == 4, "pending choice changed or closed")
pending_save(false)
assert(not editor.pending and not vim.bo[editor.buffer].modifiable, "failed choice save enabled free text")
press(editor.buffer, "<Down>")
assert(shown(editor) == "7m")
press(editor.buffer, "<CR>")
pending_save(true)
assert(saved == 5 and configured.approval_timeout.amount == 7 and not picker._state_for_test().field_editor)
deferred = false
editor = edit("tool_timeout")
assert(shown(editor) == "10m", "tool and shell timeout did not default to ten minutes")
press(editor.buffer, "<Up>")
press(editor.buffer, "<CR>")
assert(saved == 6 and configured.tool_timeout.amount == 11 and configured.tool_timeout.unit == "m",
  "tool and shell deadline was not saved through the timeout editor")
for index, choice in ipairs(choices) do
  local value = duration.parse(choice)
  assert(duration.render(value) == choice)
  assert(duration.render(duration.change(value, 1)) == choices[math.min(#choices, index + 1)])
  assert(duration.render(duration.change(value, -1)) == choices[math.max(1, index - 1)])
end
assert(duration.render({ amount = 1, unit = "m" }) == "60s")
assert(duration.render(duration.change({ amount = 0, unit = "s" }, -1)) == "15s")
assert(duration.render(duration.change({ amount = 10, unit = "h" }, 1)) == "60m")
picker.close(false)
controller.configure = original_configure
local timeout_configuration = require("forge.views.harness.timeout_configuration")
local timeout_state = { session = { id = "ordered-timeouts", approval_timeout = { amount = 10, unit = "m" },
  status_thoughts = true } }
local completions, failures = {}, {}
local requests = mock_client.record()
local function configure_timeout(values)
  timeout_configuration.configure(timeout_state, values, function(applied) completions[#completions + 1] = applied end,
    function(message) failures[#failures + 1] = message end)
end
configure_timeout({ approval_timeout = { amount = 11, unit = "m" } })
configure_timeout({ approval_timeout = { amount = 12, unit = "m" } })
configure_timeout({ approval_timeout = { amount = 14, unit = "m" } })
configure_timeout({ approval_timeout = { amount = 13, unit = "m" } })
assert(#requests == 1, "rapid edits sent concurrent timeout writes")
requests[1].callback({ approval_timeout = { amount = 11, unit = "m" } })
assert(#requests == 2 and requests[2].spec.params.approval_timeout.amount == 13
  , "pending edits lost the newest value")
assert(timeout_state.session.approval_timeout.amount == 10, "older response replaced a pending edit")
requests[2].callback({ approval_timeout = { amount = 13, unit = "m" } })
assert(timeout_state.session.approval_timeout.amount == 13 and timeout_state.session.status_thoughts == true)
assert(#completions == 4 and not vim.tbl_contains(completions, false))
configure_timeout({ approval_timeout = { amount = 14, unit = "m" } })
configure_timeout({ approval_timeout = { amount = 15, unit = "m" } })
requests[3].callback(nil, "write failed")
assert(#failures == 1 and completions[5] == false and #requests == 4, "a failed write stranded pending edits")
requests[4].callback({ approval_timeout = { amount = 15, unit = "m" }, status_thoughts = false })
assert(timeout_state.session.approval_timeout.amount == 15 and timeout_state.session.status_thoughts == true,
  "timeout response overwrote another setting")
configure_timeout({ approval_timeout = { amount = 16, unit = "m" } })
configure_timeout({ approval_timeout = { amount = 17, unit = "m" } })
timeout_state.session = { id = "replacement-session", approval_timeout = { amount = 10, unit = "m" } }
requests[5].callback({ approval_timeout = { amount = 16, unit = "m" } })
assert(#requests == 5 and timeout_state.session.approval_timeout.amount == 10,
  "a stale timeout callback wrote to the replacement session")
assert(completions[7] == false and completions[8] == false)
end
T["model_pins"] = function()
local pins = require("forge.views.harness.model_pins")
local model_picker = require("forge.views.harness.model_picker")
local picker = require("forge.views.picker")
local picker_state = require("forge.views.picker.state")
local confirmed
local errors = notify.capture({ min_level = vim.log.levels.ERROR, notifications = true })
local request_list = mock_client.record()
local function selected()
  local instance = picker._state_for_test()
  return picker_state.selected_option(instance.state, instance.spec)
end
local source = {
  { id = "older", reasoning = { "low", "high" }, default_reasoning = "low" },
  { id = "newer", reasoning = { "low", "high" }, default_reasoning = "low" },
  { id = "third" },
}
local function open(current, is_current)
  return model_picker.open({ host = { win = vim.api.nvim_get_current_win() }, model_list = source,
    provider = "fixture_alpha", current_model = current, is_current = is_current,
    on_confirm = function(config) confirmed = config end })
end
local function first()
  return picker._state_for_test().spec.page_list[1].option_list[1]
end
do
  require("forge").setup({ harness = { provider = "mock" } })
  pins.refresh("fixture_alpha", function(_, pin_error) assert(not pin_error) end)
  request_list[#request_list].callback({})
  open("newer")
  assert(selected().id == "newer" and first().id == "older")
  keymap.invoke("<Right>")
  assert(selected().value.picker_reasoning == "high")
  keymap.invoke("p")
  local write = request_list[#request_list]
  assert(write.spec.method == "provider.model.pin.set" and write.spec.params.model == "newer" and write.spec.params.pinned)
  local count = #request_list
  keymap.invoke("p")
  assert(#request_list == count, "pin writes overlapped")
  assert(first().id == "older", "unacknowledged pin changed ordering")
  write.callback({ "newer" })
  assert(first().id == "newer" and first().label == "* newer")
  assert(selected().id == "newer" and selected().value.picker_reasoning == "high")
  assert(confirmed == nil, "pinning configured the model")
  keymap.invoke("p")
  write = request_list[#request_list]
  write.callback(nil, "database failure")
  assert(#errors == 1 and errors[1].message:find("database failure", 1, true))
  assert(first().id == "newer" and pins.get("fixture_alpha").newer)
  keymap.invoke("p")
  write = request_list[#request_list]
  write.callback({})
  assert(first().id == "older" and selected().id == "newer", "unpin lost the original order or selection")
  keymap.invoke("p")
  write = request_list[#request_list]
  picker.close()
  open("third")
  write.callback({ "newer" })
  assert(first().id == "older" and selected().id == "third", "late response updated a replacement picker")
  picker.close()
  local current = true
  open("third", function() return current end)
  keymap.invoke("p")
  write = request_list[#request_list]
  current = false
  write.callback({ "newer", "third" })
  assert(first().id == "newer", "provider-stale response changed the picker")
  assert(pins.get("fixture_alpha").third, "successful stale write was not retained")
  picker.close()

  pins.refresh("fixture_alpha", function(_, pin_error) assert(not pin_error) end)
  local read = request_list[#request_list]
  pins.set("fixture_alpha", "older", true, function(_, pin_error) assert(not pin_error) end)
  request_list[#request_list].callback({ "older" })
  read.callback({ "third" })
  assert(pins.get("fixture_alpha").older and not pins.get("fixture_alpha").third, "late read replaced a newer write")

  local state = require("forge.session").harness
  state.session = { id = "pin-completion", provider = "fixture_alpha" }
  state.model_provider, state.model_list = "fixture_alpha", source
  local completion = require("forge.views.harness.completion.command_source").new()
  local result
  completion:request_model_list(function(model_list) result = model_list end)
  assert(result[1].id == "older")
  pins.set("fixture_alpha", "third", true, function(_, pin_error) assert(not pin_error) end)
  request_list[#request_list].callback({ "third" })
  completion:request_model_list(function(model_list) result = model_list end)
  assert(result[1].id == "third" and state.model_list[1].id == "older", "completion changed the raw catalog")
  buffer.set(0, { "/model " })
  vim.o.virtualedit = "onemore"
  vim.api.nvim_win_set_cursor(0, { 1, 7 })
  completion:get_completions({}, function(value) result = value.items end)
  assert(result[1].label == "third" and result[1].sortText == "000001", "model completion lost pin ordering")
end
end
T["new"] = function()
local builder = require("forge.builder")
local client = require("forge.client")
local forge = require("forge")
local session = require("forge.session")

local test_root = temp.directory("new")
local crate_root = vim.fs.joinpath(test_root, "crate")
vim.fn.mkdir(vim.fs.joinpath(crate_root, "src"), "p")
vim.fn.writefile({ "[package]", 'name = "diff-review-harness"', 'version = "0.1.0"' },
  vim.fs.joinpath(crate_root, "Cargo.toml"))
vim.fn.writefile({ "fn main() {}" }, vim.fs.joinpath(crate_root, "src", "main.rs"))

local initialize_count = 0
local session_new_count = 0
local session_sequence = 0
local initialize_request

local function snapshot(name)
  session_sequence = session_sequence + 1
  return {
    session = {
      id = "session-" .. session_sequence,
      name = name or "",
      workspace = vim.fn.getcwd(),
      provider = "mock",
      model = "mock-model",
      provider_label = "Mock CLI",
      resolved_model = "mock-model",
      effort = "low",
      approval_mode = "ask",
      native_fork = false,
      native_compact = true,
    },
    interaction = {},
    timeline = {},
    capability = { native_fork = false, native_compact = true },
    no_checkpoint = false,
    artifact = {},
    approval = {},
    agent = { definition = {}, run = {}, turn = {} },
    prompt_history = {},
  }
end

local function emit(options, message)
  options.stdout(nil, vim.json.encode(message) .. "\n")
end

local function mock_launcher(_, options, _)
  local process = {}
  function process:write(payload)
    local request = vim.json.decode(vim.trim(payload))
    if request.method == "host.transport.consumed" then return end
    if request.method == "host.initialize" then
      emit(options, { id = request.id, result = { protocol_version = require("forge.protocol").VERSION } })
    elseif request.method == "provider.registry.list" then
      emit(options, { id = request.id, result = dofile("nvim/lua/forge/tests/support/provider_catalog.lua").registry() })
    elseif request.method == "text.initialize" then
      emit(options, { id = request.id, result = {} })
    elseif request.method == "session.input.get" then
      emit(options, { id = request.id, result = { revision = 0, messages = {} } })
    elseif request.method == "session.initialize" then
      initialize_count = initialize_count + 1
      initialize_request = request
      local initialized = snapshot(request.params.new_session_name)
      emit(options, { id = request.id, result = initialized })
    elseif request.method == "session.new" then
      session_new_count = session_new_count + 1
      if request.params.name == "fail" then
        emit(options, { id = request.id, error = { code = "request_failed", message = "mock new-session failure" } })
      else
        emit(options, { id = request.id, result = snapshot(request.params.name) })
      end
    elseif request.method == "host.trace.configure" then
      emit(options, { id = request.id, result = { enabled = request.params.enabled } })
    elseif request.method == "host.shutdown" then
      emit(options, { id = request.id, result = { shutdown = true } })
    else
      error("unexpected Harness request: " .. request.method)
    end
  end
  function process:kill() end
  return process
end

MiniTest.finally(function()
  pcall(client._host_reset_for_test)
  pcall(builder._host_reset_for_test)
end)
do
  forge.setup({ diff_logging = false, harness_logging = false, harness = { provider = "mock" } })
  builder._set_crate_dir_for_test(crate_root)
  builder._set_artifact_root_for_test(vim.fs.joinpath(test_root, "artifacts"))
  vim.fn.mkdir(vim.fs.dirname(builder.binary_path()), "p")
  vim.fn.writefile({ "mock" }, builder.binary_path())
  client._host_set_launcher_for_test(mock_launcher)

  local baseline_tab_count = vim.fn.tabpagenr("$")
  require("forge.views.harness").new_session()
  eq(vim.fn.tabpagenr("$"), baseline_tab_count + 1,
    { fail_reason = "cold ForgeHarnessNew should create exactly one timeline tab" })
  wait_until(function()
    return session.harness.session and session.harness.session.id == "session-2"
  end, "cold ForgeHarnessNew should open the session that session.new created")
  eq(initialize_count, 1, { fail_reason = "cold ForgeHarnessNew should initialize one broker" })
  eq(session_new_count, 1, { fail_reason = "cold ForgeHarnessNew should create its fresh session through session.new" })
  eq(initialize_request.params.new_session_name, nil,
    { fail_reason = "cold startup should restore the broker without naming a session; session.new owns creation" })

  local cold_tab = session.harness.timeline_tab
  require("forge.views.harness").new_session("warm start")
  eq(vim.fn.tabpagenr("$"), baseline_tab_count + 2,
    { fail_reason = "warm ForgeHarnessNew should create one additional timeline tab" })
  wait_until(function()
    return session.harness.session and session.harness.session.name == "warm start"
  end, "warm ForgeHarnessNew should complete through session.new")
  eq(initialize_count, 1, { fail_reason = "warm ForgeHarnessNew should reuse the running broker" })
  eq(session_new_count, 2, { fail_reason = "warm ForgeHarnessNew should create one broker session" })
  assert(vim.api.nvim_tabpage_is_valid(cold_tab), "warm ForgeHarnessNew should preserve the prior timeline tab")

  vim.cmd("tabclose!")
  vim.cmd("tabclose!")
  eq(vim.fn.tabpagenr("$"), baseline_tab_count, { fail_reason = "closing Harness timelines should restore the baseline" })
  require("forge.views.harness").new_session("after close")
  eq(vim.fn.tabpagenr("$"), baseline_tab_count + 1,
    { fail_reason = "ForgeHarnessNew after closing every timeline should create exactly one tab" })
  wait_until(function()
    return session.harness.session and session.harness.session.name == "after close"
  end, "ForgeHarnessNew should remain usable after every timeline closes")
  eq(session_new_count, 3, { fail_reason = "ForgeHarnessNew after tab closure should reuse the running broker" })

  local successful_tab = session.harness.timeline_tab
  require("forge.views.harness").new_session("fail")
  eq(vim.fn.tabpagenr("$"), baseline_tab_count + 2,
    { fail_reason = "a failed ForgeHarnessNew should retain exactly one provisional tab" })
  wait_until(function()
    local line_list = buffer.lines(session.harness.transcript_buf)
    return vim.tbl_contains(line_list, "  New session failed: mock new-session failure")
  end, "a failed ForgeHarnessNew should explain the failure in its provisional timeline")
  assert(vim.api.nvim_tabpage_is_valid(successful_tab),
    "a failed ForgeHarnessNew should preserve the previously active timeline")
end
end
T["prompt_lifecycle"] = function()
local state = require("forge.session").harness
local client = require("forge.client")
local controller = require("forge.views.harness.controller")
local input = require("forge.views.harness.input_state")
local submission = require("forge.views.harness.prompt_submission")
local host = dofile("nvim/lua/forge/tests/support/input.lua").new(state)
local controls = {}
local errors = notify.capture({ min_level = vim.log.levels.ERROR, notifications = true })
require("forge.views.harness.prompt_history").record = function() end
require("forge.views.harness.agent_picker").refresh = function() end
controller.render = function() end
controller.refresh_winbar = function() end
controller.attach_transcript = function() end
state.session = { id = "prompt-lifecycle", provider = "mock" }
state.capability = { native_steer = true }
state.composer_buf = buffer.scratch()
state.queue, state.pending_steer = {}, {}
local current = true
state.presentation = { capture_prompt = function()
  return { buffer = state.composer_buf, document = "original", is_current = function() return current end }
end }
client.request = function(request_specification, callback)
  local method = request_specification.method
  local parameters = request_specification.params or {}

  if host.request(method, parameters, callback) then return end
  if method == "session.state.get" then
    callback({ session = state.session, capability = state.capability,
      status = { kind = "idle" }, runtime_epoch = state.runtime_epoch })
    return
  end
  controls[#controls + 1] = { method = method, parameters = parameters, callback = callback }
end
input.attach(state, function() end)
local function compose(text)
  buffer.set(state.composer_buf, vim.split(text, "\n", { plain = true }))
end
local function composer_text() return buffer.text(state.composer_buf) end
---@param text string
---@return {id: string, text: string, delivery?: string}?
local function retained_message(text)
  for _, messages in ipairs({ state.queue, state.pending_steer }) do
    for _, message in ipairs(messages or {}) do
      if type(message) == "table" then
        ---@cast message {id: string, text: string, delivery?: string}
        if message.text == text then
          assert(type(message.id) == "string", "fixture host message requires its original input identity")
          return message
        end
      end
    end
  end
end
local function route_message(id, delivery, route)
  for _, message in ipairs(host.messages) do
    if message.id == id then
      message.revision = message.revision + 1
      message.delivery, message.route = delivery, route
      host.revision = host.revision + 1
      input.receive(state, { revision = host.revision, suspended = false,
        upsert = { vim.deepcopy(message) }, removed = {} })
      return
    end
  end
  error("host message is missing: " .. id)
end
local path_prompt = "/workspace/Screen shot.png\n[Image #1]\n\n"
compose(path_prompt)
controller.submit()
assert(composer_text() == "" and state.pending_submit[1].text == path_prompt and #state.queue == 0)
local first = host.requests[1]
compose("first follow-up")
controller.queue_submit()
compose("second follow-up")
controller.queue_submit()
assert(#state.queue == 2 and #state.pending_submit == 1 and #host.requests == 3,
  "queued drafts must submit independent host commands before earlier acknowledgements")
compose("newer draft")
current = false
host.flush()
assert(composer_text() == "newer draft", "retired capture overwrote the replacement composer")
assert(#state.queue == 3 and #state.pending_submit == 0, "host queued receipts must replace pending drafts")
host.complete(first)
assert(#state.queue == 3 and #state.pending_submit == 0 and #host.messages == 3,
  "duplicate receipt removed another message or duplicated host input")
current = true

controller.cancel_turn()
assert(#state.queue == 3, "requesting cancellation must preserve queued input")
local cancellation = controls[#controls]
assert(cancellation.method == "session.turn.cancel" and cancellation.parameters.restore_prompt_if_no_output)
cancellation.callback({ cancel_requested = true })
local count = #host.requests
controller.render()
assert(#host.requests == count and composer_text() == "newer draft", "pause drained queued input or overwrote typing")
controller.task_transition({ action = "resume" })
local resume = controls[#controls]
assert(resume.parameters.action == "resume")
resume.callback({ state = "completed" })

compose("active-turn addition")
dofile("nvim/lua/forge/tests/support/activity.lua").set_busy(state, false)
state.execution_busy = true
controller.submit()
---@type { id: string, text: string, delivery?: string }
local addition = assert(state.pending_submit[#state.pending_submit])
assert(addition.text == "active-turn addition" and #state.pending_steer == 0,
  "local busy state must not invent host steering admission")
assert(host.requests[#host.requests].parameters.action.message.intent == "auto",
  "stale local busy state bypassed host routing")
host.flush()
assert(assert(retained_message("active-turn addition")).delivery == "queued" and #state.pending_steer == 0,
  "a queued receipt must not imply native steering")
route_message(addition.id, "delivering", "steering")
assert(#state.pending_steer == 1 and state.pending_steer[1].id == addition.id and #state.pending_submit == 0,
  "host routing must move the original message into pending steering")
route_message(addition.id, "accepted", "steering")
assert(not retained_message("active-turn addition") and #state.pending_steer == 0,
  "host acceptance must remove only the accepted steering message")
assert(#state.queue == 3 and composer_text() == "", "steering acceptance changed unrelated queued input")

state.checks_running, state.active_check_run = true, "check"
compose("replace check")
local control_count = #controls
controller.submit()
assert(#controls == control_count, "Lua interrupted checks instead of submitting input")
assert(host.requests[#host.requests].parameters.action.message.text == "replace check"
  and host.requests[#host.requests].parameters.action.message.intent == "auto")
compose("draft after cancellation")
controller.cancel_turn()
assert(controls[#controls].method == "session.turn.cancel")
controls[#controls].callback({ cancel_requested = true })
assert(composer_text() == "draft after cancellation")
host.flush()
assert(retained_message("replace check"), "cancellation discarded pending input")
state.checks_running, state.active_check_run = false, nil

compose("retract before output")
submission.start(state, { text = "retract before output", context = state.presentation.capture_prompt() }, function() end)
host.flush()
local retracted = assert(retained_message("retract before output"))
host.revision = host.revision + 1
input.receive(state, { revision = host.revision, suspended = true, upsert = {}, removed = {}, retracted = retracted.id })
assert(composer_text() == "retract before output", "retraction did not restore the unchanged composer")
host.flush()
compose("retract while typing")
submission.start(state, { text = "retract while typing", context = state.presentation.capture_prompt() }, function() end)
host.flush()
retracted = assert(retained_message("retract while typing"))
compose("preserve this")
host.revision = host.revision + 1
input.receive(state, { revision = host.revision, suspended = true, upsert = {}, removed = {}, retracted = retracted.id })
assert(composer_text() == "preserve this" and assert(retained_message("retract while typing")).id == retracted.id)
errors:assert_none()
end
T["provider_checklist"] = function()
-- Exercise provider checklists and deferred native creation through a real host and mock provider.
local original_directory = vim.fn.getcwd()
local fixture_evidence = dofile("nvim/lua/forge/tests/support/evidence_directory.lua")("harness-provider-checklist")
local evidence = fixture_evidence.path
local workspace, data = evidence .. "/workspace", evidence .. "/data"
vim.fn.mkdir(data, "p")
git_repository.create({ path = workspace, files = { ["README.md"] = { "Checklist fixture" } }, message = "Fixture baseline" })
local client = host.start({ data = data, config = data }).client
local state = require("forge.session").harness
local last_snapshot
local diagnostics = notify.capture({ min_level = vim.log.levels.ERROR, notifications = true })

---@param method string
---@param params? table
---@return any
local state_snapshot = dofile("nvim/lua/forge/tests/support/state.lua")
local function request(method, params)
  -- Snapshot reads replay their publication page; see support/state.lua.
  if method == "session.state.get" then last_snapshot = state_snapshot.get() return last_snapshot end
  local completion = {}
  client.request({ method = method, params = params or {},
    session_id = require("forge.event_contract").method_policy(method).session_context ~= "none"
      and assert(state.session).id or nil }, function(value, failure)
    completion.value, completion.failure, completion.done = value, failure, true
  end)
  wait_until(function() return completion.done end, method .. " did not finish", { timeout_ms = 4000 })
  assert(not completion.failure, tostring(completion.failure))
  if method == "session.state.get" then last_snapshot = completion.value end
  return completion.value
end

---@param entries { content: string, status: "pending"|"in_progress"|"completed" }[]
---@return table
local function checklist_step(entries)
  local entry_list = {}
  for ordinal, entry in ipairs(entries) do
    entry_list[ordinal] = { provider_id = "entry-" .. ordinal, content = entry.content,
      priority = vim.NIL, status = entry.status, provider_ordinal = ordinal - 1 }
  end
  return { kind = "checklist", update = { scope_id = "fixture-plan", name = "Fixture plan",
    complete = false, replace_entries = true, entry_list = entry_list } }
end

---@return string[]
local function transcript()
  return buffer.lines(state.transcript_buf)
end

---@param pattern string
---@return integer
local function count(pattern)
  local total = 0
  for _, line in ipairs(transcript()) do
    if line:find(pattern, 1, true) then total = total + 1 end
  end
  return total
end

---@param steps table[]
---@param text string
local function prompt(steps, text)
  local reply = "Fixture reply: " .. text
  table.insert(steps, { kind = "message", content = "response", text = reply })
  request("session.provider.mock_response", { operation = "script",
    script = { steps = steps, terminal = "completed" } })
  request("session.prompt.submit", { text = text })
  wait_until(function()
    return count(reply) > 0 and request("session.state.get").status.kind == "idle"
  end, "scripted prompt did not settle: " .. text, { timeout_ms = 4000 })
end

-- Finished exchanges show sections as headings; open one through the transcript's real mapping.
---@param heading string
local function open_section(heading)
  local window = vim.fn.bufwinid(state.transcript_buf)
  assert(window ~= -1, "transcript window is not displayed")
  local row
  for index, line in ipairs(transcript()) do
    if line:find(heading, 1, true) then row = index end
  end
  assert(row, "section heading is missing: " .. heading)
  vim.api.nvim_set_current_win(window)
  vim.api.nvim_win_set_cursor(window, { row, 0 })
  keymap.invoke("<Tab>")
end

local success, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = true, harness = { provider = "mock" } })
  require("forge.views.harness").open()
  wait_until(function() return state.ready and state.session ~= nil end, "startup timeout", { timeout_ms = 4000 })

  -- Startup keeps the new chat local: no native conversation exists before the first input.
  local opened = request("session.state.get")
  assert(opened.session.provider_session_id == nil or opened.session.provider_session_id == vim.NIL,
    "startup allocated a native conversation: " .. vim.inspect(opened.session.provider_session_id))

  -- Each provider replacement updates the canonical checklist rather than appending stale rows.
  prompt({
    checklist_step({ { content = "Read the owner", status = "completed" }, { content = "Move the types", status = "in_progress" } }),
    checklist_step({ { content = "Ship it", status = "in_progress" }, { content = "Verify it", status = "in_progress" },
      { content = "Document it", status = "pending" } }),
    checklist_step({ { content = "Ship it", status = "completed" }, { content = "Verify it", status = "in_progress" },
      { content = "Document it", status = "pending" } }),
  }, "Report the checklist.")
  local first = request("session.state.get")
  assert(type(first.session.provider_session_id) == "string" and first.session.provider_session_id ~= "",
    "the first input did not open a native conversation")
  local heading = "Provider-reported checklist · 1/3 completed · Fixture plan"
  wait_until(function() return count(heading) > 0 end, "checklist heading did not render", { timeout_ms = 4000 })
  open_section(heading)
  wait_until(function() return count("[x] Ship it") > 0 end, "checklist rows did not open", { timeout_ms = 4000 })
  for _, row in ipairs({ "[x] Ship it", "[>] Verify it · in progress", "[ ] Document it" }) do
    assert(count(row) == 1, "missing checklist row " .. row .. "\n" .. table.concat(transcript(), "\n"))
  end
  assert(count("Read the owner") == 0 and count("[>] Ship it") == 0, "replacement kept a stale entry")

  -- A later exchange without a checklist neither repeats nor fabricates progress.
  local transcript_window = vim.fn.bufwinid(state.transcript_buf)
  assert(transcript_window ~= -1, "transcript window is not displayed")
  vim.api.nvim_win_set_cursor(transcript_window, { 1, 0 })
  prompt({}, "Answer without a checklist.")
  assert(count("Provider-reported checklist") == 1, "a checklist-free exchange fabricated a checklist section")
  assert(vim.api.nvim_win_get_cursor(transcript_window)[1] == 1, "transcript refresh moved the cursor")
  diagnostics:assert_none()
end, debug.traceback)
if not success then
  local diagnostic = { workspace = workspace, data = data, snapshot = last_snapshot, errors = diagnostics,
    host_stderr = client._client.stderr, failure = failure,
    transcript = state.transcript_buf and vim.api.nvim_buf_is_valid(state.transcript_buf) and transcript() or nil }
  vim.fn.writefile(vim.split(vim.inspect(diagnostic), "\n", { plain = true }), evidence .. "/failure.txt")
  failure = tostring(failure) .. "\nRetained diagnostics: " .. evidence .. "/failure.txt"
end
require("forge.views.harness.workspace").release(state)
if state.presentation then state.presentation.close() end
local collected = host.stop()
vim.api.nvim_set_current_dir(original_directory)
-- Remove passing evidence only after the host exits and releases its files.
local release_failure = fixture_evidence.release(success and collected)
assert(success and collected, failure or "host was not collected")
assert(not release_failure, release_failure)
end
T["provider_destination"] = function()
local session = require('forge.session')
local client = require('forge.client')
dofile("nvim/lua/forge/tests/support/provider_catalog.lua").install()
local picker = require('forge.views.picker')
local harness = require('forge.views.harness')
local controller = require('forge.views.harness.controller')
local session_picker = require('forge.views.harness.session_picker')
local preview = require('forge.views.harness.session_preview')
local state = session.harness
state.session = { id = 'source', provider = 'fixture_alpha', workspace = 'workspace' }
dofile("nvim/lua/forge/tests/support/activity.lua").set_busy(state, false)
state.queue, state.pending_steer = {}, {}
local spec, selected, list_failure, empty_list, previewed, notifications
local preview_request_list = {}
local deferred_preview_list = {}
local defer_preview = false
require('forge.infra.notifications').error = function(message) notifications = message end
require('forge.infra.notifications').warn = function(message) notifications = message end
preview.open = function() end
preview.close = function() end
preview.render = function(result) previewed = result end
picker.open = function(next_spec) spec = next_spec end
picker.update = function(next_spec) spec = next_spec end
picker.is_open = function() return false end
picker.close = function(notify)
  if notify ~= false and spec and spec.on_close then spec.on_close() end
end
harness.switch_provider = function(provider, destination) selected = { provider = provider, destination = destination } end
client.request = function(request_specification, callback)
  local method = request_specification.method
  local params = request_specification.params or {}

  if method == 'session.preview' then
    assert(request_specification.session_id == 'source',
      'preview must use the captured calling provider session, not the candidate')
    preview_request_list[#preview_request_list + 1] = vim.deepcopy(request_specification)
    if defer_preview then
      deferred_preview_list[#deferred_preview_list + 1] = function() callback({ preview = params.session_id }) end
    else
      callback({ preview = params.session_id })
    end
    return
  end
  assert(method == 'session.list' and params.scope == 'repo')
  if list_failure then callback(nil, 'list failed') return end
  callback(empty_list and {} or {
    { id = 'chosen', provider = 'fixture_beta', workspace = 'workspace', name = 'Chosen', created_at_ms = 1 },
    { id = 'fixture_alpha', provider = 'fixture_alpha', workspace = 'workspace', name = 'Fixture Alpha', created_at_ms = 2 },
    { id = 'other-workspace', provider = 'fixture_beta', workspace = 'elsewhere', name = 'Other', created_at_ms = 3 },
  })
end
local function choose_provider()
  controller.select_provider()
  for _, option in ipairs(spec.page_list[1].option_list) do
    if option.value == 'fixture_beta' then spec.on_confirm({ option = option }) return end
  end
  error('missing Fixture Beta provider')
end
local function choose_destination(kind)
  for _, option in ipairs(spec.page_list[1].option_list) do
    if option.value == kind then spec.on_confirm({ option = option }) return end
  end
  error('missing destination')
end
controller.select_provider()
for _, option in ipairs(spec.page_list[1].option_list) do
  if option.value == 'fixture_alpha' then spec.on_confirm({ option = option }) break end
end
assert(spec.page_list[1].title == 'Select Harness' and selected == nil)
choose_provider()
assert(spec.page_list[1].title == 'Select Chat' and selected == nil)
choose_destination('resume')
assert(#spec.page_list[1].option_list == 1 and #spec.action_list == 0)
local option = spec.page_list[1].option_list[1]
assert(option.id == 'chosen')
local source_session = state.session
state.session = { id = 'later-view', provider = 'fixture_alpha', workspace = 'workspace' }
spec.on_change({ option = option })
state.session = source_session
assert(previewed.preview == 'chosen')
assert(preview_request_list[#preview_request_list].params.session_id == 'chosen',
  'the cross-provider candidate belongs in preview params')
assert(selected == nil and state.session.id == 'source', 'preview must not switch sessions')
defer_preview = true
spec.on_change({ option = option })
spec.on_change({ option = nil })
previewed = nil
deferred_preview_list[#deferred_preview_list]()
assert(previewed == nil, 'clearing selection must invalidate an outstanding preview')
spec.on_change({ option = option })
local closed_preview = deferred_preview_list[#deferred_preview_list]
defer_preview = false
spec.on_confirm({ option = option })
closed_preview()
assert(previewed == nil, 'a closed destination picker must reject late preview callbacks')
assert(selected.provider == 'fixture_beta' and selected.destination.session_id == 'chosen')
assert(state.session.id == 'source', 'selection changed the source before startup')
selected = nil
choose_provider()
empty_list = true
choose_destination('resume')
assert(#spec.page_list[1].option_list == 0)
spec.on_close()
wait_until(function() return spec.page_list[1].title == 'Select Chat' end, 'closing an empty session list did not return to destinations')
assert(selected == nil and spec.page_list[1].title == 'Select Chat')
choose_destination('new')
assert(selected.destination.kind == 'new')
selected = nil
choose_provider()
list_failure = true
choose_destination('resume')
wait_until(function() return spec.page_list[1].title == 'Select Chat' end, 'a failed session list did not return to destinations')
assert(notifications == 'list failed' and selected == nil)
assert(session_picker._state_for_test() == nil)
list_failure, empty_list = false, false
state.session = nil
session_picker.open({})
local preview_request_count = #preview_request_list
previewed, notifications = nil, nil
spec.on_change({ option = spec.page_list[1].option_list[1] })
assert(#preview_request_list == preview_request_count,
  'an uninitialized caller must not route preview through the candidate')
assert(previewed == nil and notifications ==
  'Wait for the calling session to initialize before previewing saved sessions.')
session_picker.close()
state.session = source_session
state.pending_steer = { { prompt = 'pending' } }
assert(not harness.provider_switch_available())
end
T["queue_viewport"] = function()
vim.o.lines, vim.o.columns = 30, 80
local workspace, data = temp.directory("workspace"), temp.directory("data")
local client = host.start({ data = data, config = data }).client
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local failures = notify.capture({ min_level = vim.log.levels.ERROR, notifications = true })
local wait_options = { timeout_ms = 4000, errors = failures, context = function()
  return vim.inspect({
    ready = state.ready, session = state.session, initial_snapshot_pending = state.initial_snapshot_pending,
    presentation = state.presentation and { ready = state.presentation.ready, failure = state.presentation.failure,
      syncing = state.presentation.syncing, pending = state.presentation.pending },
    input = state.input, host = { ready = client._client.ready, runtime_ready = client._client.runtime_ready },
  })
end }
local function drafts_pending()
  for _, draft in ipairs(state.input.drafts) do
    if draft.draft_state == "pending" then return true end
  end
  return false
end
local function settled()
  wait_until(function()
    return state.input and not drafts_pending() and not state.input.refreshing
      and state.presentation and not state.presentation.syncing and not state.presentation.pending
      and not state.presentation.staging_inflight and not state.presentation.applying
      and vim.deep_equal(state.presentation.staging, state.presentation.applied_staging)
  end, "queue presentation did not settle", wait_options)
end
local function compose(text)
  buffer.set(state.composer_buf, vim.split(text, "\n", { plain = true }))
end
local function text()
  return buffer.text(state.composer_buf)
end
do
  vim.fn.chdir(workspace)
  require("forge").setup({ harness_logging = false, harness = { provider = "mock",
    providers = { mock = { command = { "mock" }, selectable = true } } } })
  require("forge.views.harness").open()
  wait_until(function() return state.ready and state.presentation and state.presentation.ready and state.input end, "startup", wait_options)
  controller.cancel_turn()
  wait_until(function() return not state.task_operation and not state.request_busy end, "pause", wait_options)
  local original = "first queued line\nfirst hidden line"
  local selected = "second queued line\n" .. string.rep("complete draft line 界\n", 12) .. "last draft line"
  compose(original)
  controller.queue_submit()
  compose(selected)
  controller.queue_submit()
  settled()
  local transcript = buffer.text(state.transcript_buf)
  local status = buffer.text(state.presentation.status_view.buffer)
  assert(status:find("first queued line", 1, true) and status:find("second queued line", 1, true))
  assert(not status:find("first hidden line", 1, true) and not status:find("last draft line", 1, true))
  assert(not transcript:find("first queued line", 1, true) and not transcript:find("second queued line", 1, true))
  assert(state.presentation.status_view.window ~= nil, "paused queue did not keep the split visible")
  controller.edit_queued(-1)
  assert(text() == selected, "editing waited for acknowledgement or lost the complete draft")
  settled()
  assert(vim.api.nvim_get_current_win() == state.composer_win)
  controller.edit_queued(-1)
  assert(text() == original)
  settled()
  controller.edit_queued(1)
  settled()
  assert(text() == selected)
  vim.api.nvim_set_current_win(state.transcript_win)
  vim.api.nvim_win_set_cursor(state.transcript_win, { 1, 2 })
  vim.api.nvim_win_call(state.transcript_win, function() vim.cmd("normal! zt") end)
  local view = vim.api.nvim_win_call(state.transcript_win, vim.fn.winsaveview)
  controller.refresh_winbar()
  settled()
  assert(vim.deep_equal(view, vim.api.nvim_win_call(state.transcript_win, vim.fn.winsaveview)),
    "passive queue refresh moved a history reader")
  controller.submit()
  -- Committing the edit is a host input mutation; the published queue releases its reservation.
  wait_until(function()
    for _, entry in ipairs(state.queue) do if entry.reservation then return false end end
    return true
  end, "edit commit never released its reservation", wait_options)
  settled()
  assert(not state.queue_edit and #state.queue == 2)
  controller.edit_queued(-1)
  controller.queue_submit()
  -- Unqueueing is a host input mutation rather than a draft, so wait for the published queue.
  wait_until(function() return #state.queue == 1 end, "unqueue never reached the host queue", wait_options)
  settled()
  assert(not state.queue_edit and #state.queue == 1 and text() == selected,
    "unqueue lost the complete message or removed another entry")
end
require("forge.views.harness.workspace").release(state)
if state.presentation then state.presentation.close() end
assert(host.stop(), "fixture host did not stop")
end
T["session_picker_tabs"] = function()
-- The session picker acts on Harness views in other tabs: deleting a session closes every view
-- that shows it, and opening a retained session in the current tab keeps that tab in place.
require("forge.infra.config").setup({})
require("forge.infra.highlights").setup()

local client = require("forge.client")
local controller = require("forge.views.harness.controller")
local navigation = require("forge.views.harness.session_navigation")
local picker = require("forge.views.picker")
local session = require("forge.session")

local workspace_path = vim.fn.getcwd()
local deleted_by_id = {}
local document_request_by_session = {}
local failures = notify.capture({ min_level = vim.log.levels.ERROR, notifications = true })
client.host_get_generation = function() return 1 end
client.host_is_accepting = function() return true end
client.session_events_subscribe = function() return function() end end

---@param session_id string
---@return table
local function session_record(session_id)
  return { id = session_id, name = session_id .. " name", provider = "mock", model = "mock",
    approval_mode = "ask", workspace = workspace_path, created_at_ms = 1 }
end

---Build a native document response for the production presentation adapter.
local function document_snapshot(document, text)
  return { document = document, revision = 0, block = { { id = "body", text = { text },
    metadata = { target = {}, decoration = {}, editable_region = {} } } } }
end

local listed_id_list = {}
client.request = function(request, callback)
  local params = request.params or {}
  local method = request.method
  local result, failure, detail
  if method == "session.input.get" then
    result = { revision = 0, messages = {}, suspended = true, execution_busy = false }
  elseif method == "session.health.get" then result = { responsive = true }
  elseif method == "session.provider.model.list" then result = {}
  elseif method == "session.list" then
    result = {}
    for _, session_id in ipairs(listed_id_list) do
      if not deleted_by_id[session_id] then result[#result + 1] = session_record(session_id) end
    end
  elseif method == "session.preview" then result = { lines = { "preview " .. params.session_id } }
  elseif method == "session.delete" then
    deleted_by_id[params.session_id] = true
    result = { deleted = params.session_id }
  elseif method == "session.resume" then result = { session = session_record(params.session_id) }
  elseif method == "session.state.get" then result = { session = session_record(request.session_id) }
  elseif method == "session.document" then
    if params.operation ~= "close" then
      document_request_by_session[request.session_id] = (document_request_by_session[request.session_id] or 0) + 1
    end
    if deleted_by_id[request.session_id] then
      -- The host reports a deleted session's documents with its generic request failure code.
      failure, detail = "session not found", { code = "request_failed" }
    elseif params.operation == "open" then
      result = { transcript = document_snapshot(params.document, request.session_id .. " transcript"),
        status = document_snapshot(params.document .. ":status", "Waiting") }
    elseif params.operation == "background_terminals" then result = { supported = false }
    elseif params.operation == "sync" then
      result = { patch = {}, status = document_snapshot(params.document .. ":status", "Waiting") }
    elseif params.operation == "staging" then result = { updated = true }
    elseif params.operation == "close" then result = { closed = true }
    else result = { patch = {} } end
  else error("Unexpected native request: " .. method) end
  vim.schedule(function() callback(result, failure, detail) end)
end

---@param state ForgeHarnessPresentationState
local function settled(state)
  wait_until(function() return state.presentation and state.presentation.ready end,
    "presentation did not become ready", { timeout_ms = 2000 })
end

---Construct a session in its own tab with a real presentation.
---@param session_id string
---@return ForgeHarnessPresentationState
local function create_session(session_id)
  local pending = navigation.begin_new(session_id)
  local state = pending.state
  state.ready = true
  navigation.complete_pending(pending, { session = session_record(session_id) })
  settled(state)
  listed_id_list[#listed_id_list + 1] = session_id
  return state
end

---Open /sessions from `state`, filter to `session_id`, and run the insert-mode `key` action.
---@param state ForgeHarnessPresentationState
---@param session_id string
---@param key string
local function picker_action(state, session_id, key)
  vim.api.nvim_set_current_win(state.composer_win)
  controller.open_session_picker()
  wait_until(function() return picker.is_open("sessions") end, "session picker did not open")
  keymap.invoke("/")
  local active_picker = picker._state_for_test()
  buffer.set(active_picker.search_buf, { session_id })
  vim.api.nvim_exec_autocmds("TextChangedI", { buffer = active_picker.search_buf })
  keymap.invoke(key, { mode = "i" })
end

do
  local registry = navigation._registry_for_test()

  -- Deleting a session shown in another tab closes that view instead of leaving it retrying.
  local alpha = create_session("alpha")
  local bravo = create_session("bravo")
  local bravo_tab, bravo_presentation = bravo.timeline_tab, bravo.presentation
  local bravo_buffer_list = { bravo.transcript_buf, bravo.composer_buf }
  picker_action(alpha, "bravo", "<C-j>")
  wait_until(function() return deleted_by_id.bravo end, "picker did not delete bravo")
  local requests_at_deletion = document_request_by_session.bravo
  wait_until(function() return not vim.api.nvim_tabpage_is_valid(bravo_tab) end,
    "the tab showing the deleted session stayed open")
  assert(bravo_presentation.closed, "the deleted session's presentation kept syncing")
  assert(registry.bravo == nil and session.harness_by_id.bravo == nil, "the deleted session stayed registered")
  for _, buffer in ipairs(bravo_buffer_list) do
    assert(not vim.api.nvim_buf_is_valid(buffer), "the deleted session retained its buffer")
  end
  assert(document_request_by_session.bravo == requests_at_deletion,
    "the deleted session's presentation issued document reads after deletion")
  assert(not vim.api.nvim_eval_statusline(vim.o.tabline, { use_tabline = true, maxwidth = 300 }).str:find("bravo", 1, true),
    "a tab label still names the deleted session")
  picker.close(true)

  -- Opening a session retained without a tab replaces the current tab in place.
  local charlie = create_session("charlie")
  local delta = create_session("delta")
  buffer.set(delta.composer_buf, { "delta draft" })
  assert(require("forge.shared.view_command_set").dispatch(controller.command_set(), "close", delta))
  assert(registry.delta == delta and vim.api.nvim_buf_is_valid(delta.composer_buf), "closing delta did not retain it")
  local charlie_tab = charlie.timeline_tab
  local charlie_tab_number = vim.api.nvim_tabpage_get_number(charlie_tab)
  local tab_count = #vim.api.nvim_list_tabpages()
  picker_action(charlie, "delta", "<CR>")
  wait_until(function() return delta.presentation and delta.presentation.ready end,
    "delta did not open")
  assert(#vim.api.nvim_list_tabpages() == tab_count, "opening in the current tab changed the tab count")
  assert(delta.timeline_tab == charlie_tab and vim.api.nvim_tabpage_is_valid(charlie_tab),
    "opening in the current tab moved the session to a new tab")
  assert(vim.api.nvim_get_current_tabpage() == charlie_tab
    and vim.api.nvim_tabpage_get_number(charlie_tab) == charlie_tab_number, "the current tab changed position")
  assert(vim.api.nvim_win_get_buf(delta.composer_win) == delta.composer_buf
    and buffer.line(delta.composer_buf, 1) == "delta draft",
    "the retained session lost its draft")
  assert(session.harness == delta, "the current tab did not select the chosen session")

  -- A session hidden by a plain `:tabclose` of its tab also opens in place of the current tab.
  local echo = create_session("echo")
  local foxtrot = create_session("foxtrot")
  vim.cmd("tabclose " .. vim.api.nvim_tabpage_get_number(echo.timeline_tab))
  local foxtrot_tab = foxtrot.timeline_tab
  tab_count = #vim.api.nvim_list_tabpages()
  picker_action(foxtrot, "echo", "<CR>")
  wait_until(function() return session.harness.session and session.harness.session.id == "echo" end,
    "echo did not open")
  assert(#vim.api.nvim_list_tabpages() == tab_count,
    ("opening a tab-closed session in the current tab changed the tab count from %d to %d")
      :format(tab_count, #vim.api.nvim_list_tabpages()))
  assert(vim.api.nvim_get_current_tabpage() == foxtrot_tab, "the tab-closed session opened outside the current tab")
  failures:assert_none()
end
end
T["subagent_profiles"] = function()
local client = require("forge.client")
local config = require("forge.infra.config")
local picker = require("forge.views.picker")
local form = require("forge.shared.form")
local original_open = form.open
local current_form, pending_save, pending_models
local delayed_save, delayed_models = false, false
local saves = 0
local model_failure
local default_provider, alternate_provider, empty_provider = "primary", "secondary", "unavailable"
local catalog_by_provider = {
  [default_provider] = { capability = { fast_mode = true, ultrafast_mode = true }, models = {
    { id = "fixture-model", reasoning = { "low", "high" }, default_reasoning = "low", is_default = true, service_tiers = { "fast", "ultrafast" } },
    { id = "simple-model", reasoning = {}, service_tiers = {} },
  } },
  [alternate_provider] = { capability = { fast_mode = false, ultrafast_mode = false }, models = {
    { id = "alternate-first", reasoning = { "medium" }, default_reasoning = "medium" },
    { id = "alternate-default", reasoning = {}, is_default = true },
  } },
  [empty_provider] = { capability = {}, models = {} },
}
local snapshot = { path = "global/forge/subagents.json", revision = "1", document = {
  default_profile = "worker", profiles = {
    worker = { enabled = true, description = "Implement", instructions = "Worker instructions", provider = "inherit",
      model = "inherit", reasoning_effort = "inherit", service_tier = "inherit", approval_mode = "inherit" },
    reviewer = { enabled = true, description = "Review", instructions = "Reviewer instructions", provider = "inherit",
      model = "inherit", reasoning_effort = "inherit", service_tier = "inherit", approval_mode = "inherit" },
  },
} }
snapshot.document.profiles.disabled = vim.tbl_extend("force", snapshot.document.profiles.reviewer, { enabled = false })
local state = { session = { id = "profiles", provider = default_provider, model = "fixture-model", effort = "high", service_tier = "fast" } }
local errors = notify.capture()
form.open = function(options) current_form = original_open(options) return current_form end
client.request = function(request_specification, callback)
  local session_id = request_specification.session_id
  local method = request_specification.method
  local params = request_specification.params or {}

  assert(session_id == "profiles")
  if method == "agent.profile.get" then callback(vim.deepcopy(snapshot))
  elseif method == "agent.profile.save" then
    assert(params.expected_revision == snapshot.revision, "editor lost its revision")
    local function complete(failure)
      if failure then callback(nil, failure) return end
      local default_profile = params.document.profiles[params.document.default_profile]
      if not default_profile or not default_profile.enabled then
        callback(nil, "Default profile must be enabled") return
      end
      saves = saves + 1
      snapshot.document = vim.deepcopy(params.document)
      snapshot.revision = tostring(saves + 1)
      callback(vim.deepcopy(snapshot))
    end
    if delayed_save then pending_save = complete else complete() end
  elseif method == "session.agent.model.list" then
    if delayed_models then pending_models = callback
    else callback(vim.deepcopy(catalog_by_provider[params.provider]), model_failure) end
  else error("unexpected profile request: " .. method) end
end

-- Inherited tuning follows configuration, even when the last actual native model differs.
local inherited = require("forge.views.harness.subagent_selection").new(state)
inherited.catalog[default_provider] = vim.deepcopy(catalog_by_provider[default_provider])
state.session.resolved_model = "simple-model"
local inherited_result
inherited:cycle({ provider = "inherit", model = "inherit", reasoning_effort = "default", service_tier = "inherit" }, "reasoning_effort", 1,
  function(value, failure) assert(not failure, failure) inherited_result = value end)
assert(inherited_result and inherited_result.reasoning_effort == "low", "observed model changed inherited configuration capabilities")
state.session.resolved_model = nil

local function select(id)
  local instance = assert(picker._state_for_test())
  for _, option in ipairs(instance.spec.page_list[1].option_list) do
    if option.id == id then instance.spec.on_confirm({ option = option }) return end
  end
  error("missing option " .. id)
end

local function action(id, selected)
  local instance = assert(picker._state_for_test())
  for _, entry in ipairs(instance.spec.action_list or {}) do
    if entry.id == id then entry.callback({ option = selected and { value = selected } }) return end
  end
  error("missing action " .. id)
end

local function focus(id)
  for _, field in ipairs(current_form.fields) do
    if field.id == id then
      local anchor = current_form.editable.native.anchor[id]
      local mark = vim.api.nvim_buf_get_extmark_by_id(current_form.buffer, current_form.namespace, field.mark, {})
      vim.api.nvim_win_set_cursor(current_form.window, { (anchor and anchor.start.row or mark[1]) + 1, 0 })
      vim.api.nvim_exec_autocmds("CursorMoved", { buffer = current_form.buffer })
      return field
    end
  end
  error("missing field " .. id)
end

local function edit(id, value)
  focus(id)
  local anchor = current_form.editable.native.anchor[id]
  assert(vim.bo[current_form.buffer].modifiable, "text field is locked")
  vim.api.nvim_buf_set_text(current_form.buffer, anchor.start.row, anchor.start.column,
    anchor.finish.row, anchor.finish.column, vim.split(value, "\n", { plain = true }))
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = current_form.buffer })
end

do
  require("forge").setup({ harness = { provider = default_provider } })
  config.options.harness.providers = {
    [default_provider] = { selectable = true },
    [alternate_provider] = { selectable = true },
    [empty_provider] = { selectable = true },
  }
  local window, origin_tab = vim.api.nvim_get_current_win(), vim.api.nvim_get_current_tabpage()
  local host = { window_list = { window }, control_win = window }
  require("forge.views.harness.subagent_profiles").open(state, host)
  local option_list = picker._state_for_test().spec.page_list[1].option_list
  assert(#option_list == 3, "profile picker must contain only profiles, including disabled profiles")
  assert(option_list[1].id == "disabled" and option_list[1].columns[2] == "←No→")
  assert(vim.trim(picker._state_for_test().spec.page_list[1].column_headers[2]) == "Enable")
  local baseline_position_list
  for _, width in ipairs({ 90, 100, 123 }) do
    local page = picker._state_for_test().spec.page_list[1]
    local frame = require("forge.views.picker.layout").build(page, 1, width, {})
    local heading, profile_row
    for _, line in ipairs(frame.lines) do
      if line:find("Profile", 1, true) and line:find("Provider", 1, true) then heading = line end
      if line:find("disabled", 1, true) then profile_row = line end
    end
    assert(heading and profile_row, "profile table did not render")
    assert(not profile_row:find("…", 1, true), "profile values must fit at width " .. width)
    assert(not profile_row:find("(", 1, true), "inherit label includes the resolved parent value")
    local search_start = 1
    local column_position_list = {}
    for _, label in ipairs({ "Provider", "Model", "Thinking", "Speed" }) do
      local heading_start = assert(heading:find(label, 1, true))
      local value_start, value_end = profile_row:find("inherit", search_start, true)
      assert(value_start and value_end, "inherit label was truncated at width " .. width)
      assert(vim.fn.strdisplaywidth(heading:sub(1, heading_start - 1))
        == vim.fn.strdisplaywidth(profile_row:sub(1, value_start - 1)), label .. " column is misaligned")
      column_position_list[#column_position_list + 1] = vim.fn.strdisplaywidth(heading:sub(1, heading_start - 1))
      search_start = value_end + 1
    end
    baseline_position_list = baseline_position_list or column_position_list
    assert(vim.deep_equal(column_position_list, baseline_position_list), "extra width must not spread the columns")
  end
  select("reviewer")
  assert(not picker.is_open("subagent-profiles"))
  assert(vim.api.nvim_get_current_tabpage() ~= origin_tab, "form did not open a full page")
  assert(#vim.api.nvim_tabpage_list_wins(0) == 1 and vim.bo.filetype == "ForgeForm")
  local form_lines = buffer.lines(current_form.buffer)
  assert(form_lines[1] == "reviewer", "form must start with the Name field")
  assert(#form_lines == #current_form.fields + 1, "form retained header or spacer rows")
  local label_style = vim.api.nvim_get_hl(0, { name = "ForgeFormLabel", link = false })
  assert(label_style.fg == 0xffffff and label_style.bold, "form labels must be white and bold")
  for index, field in ipairs(current_form.fields) do
    local mark = vim.api.nvim_buf_get_extmark_by_id(current_form.buffer, current_form.namespace, field.mark, { details = true })
    assert(mark[1] == index - 1, "form rows must be consecutive")
    assert(mark[3].virt_text[1][1] == field.label .. ": " and mark[3].virt_text[1][2] == "ForgeFormLabel")
    if field.choices then assert(mark[3].virt_text[2][2] == "Normal", "choice value inherited label styling") end
  end
  focus("model")
  assert(not vim.bo.modifiable, "choice value allowed native text edits")
  current_form:cycle(1)
  current_form:cycle(1)
  assert(current_form:values().model == "fixture-model")
  focus("reasoning_effort")
  for _ = 1, 3 do current_form:cycle(1) end
  assert(current_form:values().reasoning_effort == "high")
  edit("description", "Review carefully")
  edit("instructions", "Review instructions\nSecond line\n")
  focus("default")
  current_form:cycle(1)
  assert(saves == 0, "field edits saved separately")
  current_form:save()
  assert(saves == 1 and not vim.bo.modified)
  assert(snapshot.document.default_profile == "reviewer")
  assert(snapshot.document.profiles.reviewer.instructions == "Review instructions\nSecond line\n")
  focus("instructions")
  local field_text = current_form:values().instructions
  current_form:refresh()
  assert(current_form:values().instructions == field_text, "focus refresh rewrote native input")

  delayed_save = true
  edit("instructions", "Submitted instructions")
  current_form:save()
  edit("instructions", "Newer instructions")
  pending_save()
  assert(vim.bo.modified and current_form:values().instructions == "Newer instructions")
  assert(snapshot.document.profiles.reviewer.instructions == "Submitted instructions")
  current_form:save()
  pending_save("stale revision")
  assert(vim.bo.modified and errors[#errors].message:find("stale revision"))
  current_form:close()
  assert(current_form.active and errors[#errors].message:find("Unsaved changes"))
  current_form:reload()
  assert(not vim.bo.modified and current_form:values().instructions == "Submitted instructions")
  current_form:close()
  assert(vim.api.nvim_get_current_tabpage() == origin_tab)
  assert(picker.is_open("subagent-profiles"))

  delayed_save = false
  select("worker")
  assert(current_form:values().instructions == "Worker instructions", "another profile reused the draft")
  focus("provider")
  current_form:cycle(1)
  assert(current_form:values().provider == default_provider)
  current_form:reload()
  current_form:close()
  action("add")
  edit("name", "custom")
  edit("instructions", "Custom instructions")
  current_form:save()
  assert(snapshot.document.profiles.custom.provider == "inherit")
  edit("name", "renamed")
  current_form:save()
  assert(snapshot.document.profiles.renamed and not snapshot.document.profiles.custom)
  current_form:close()
  action("delete", "renamed")
  assert(not snapshot.document.profiles.renamed)

  local instance = picker._state_for_test()
  local function press_arrow(key, name)
    for index, option in ipairs(instance.spec.page_list[1].option_list) do
      if option.id == name then instance.state.selected_index_by_page["subagent-profiles"] = index end
    end
    keymap.invoke(key, { buffer = instance.buf })
  end
  local before_toggle = saves
  press_arrow("r", "disabled")
  assert(snapshot.document.profiles.disabled.enabled and saves == before_toggle + 1)
  assert(picker._state_for_test() == instance, "toggle reopened the picker")
  assert(instance.spec.page_list[1].option_list[1].columns[2] == "←Yes→")
  press_arrow("h", "disabled")
  assert(not snapshot.document.profiles.disabled.enabled and saves == before_toggle + 2)
  assert(instance.spec.page_list[1].option_list[1].columns[2] == "←No→")
  press_arrow("<Right>", "reviewer")
  assert(snapshot.document.profiles.reviewer.enabled and saves == before_toggle + 2)
  assert(errors[#errors].message:find("Default profile must be enabled"))

  delayed_save = true
  press_arrow("<Left>", "worker")
  local first_pending = pending_save
  press_arrow("<Right>", "worker")
  select("worker")
  action("open-config")
  action("reload")
  assert(picker.is_open("subagent-profiles") and pending_save == first_pending,
    "pending toggle allowed another operation")
  pending_save("stale revision")
  assert(snapshot.document.profiles.worker.enabled and errors[#errors].message:find("stale revision"))
  press_arrow("<Right>", "worker")
  pending_save()
  assert(not snapshot.document.profiles.worker.enabled)
  assert(instance.state.selected_index_by_page["subagent-profiles"] == 3,
    "toggle lost the selected profile")
  assert(instance.spec.page_list[1].option_list[3].columns[2] == "←No→")
  delayed_save = false

  for _, option in ipairs(instance.spec.page_list[1].option_list) do
    assert(option.value, "unexpected non-profile menu option")
  end

  picker.close()
  require("forge.views.harness.subagent_profiles").open(state, host)
  select("worker")
  focus("model")
  delayed_models = true
  current_form:cycle(1)
  current_form:close()
  pending_models(catalog_by_provider[default_provider])
  assert(picker.is_open("subagent-profiles"), "late catalog callback reopened the form")
  picker.close()

  delayed_models = false
  local worker = snapshot.document.profiles.worker
  worker.provider, worker.model, worker.reasoning_effort, worker.service_tier = default_provider, "fixture-model", "high", "ultrafast"
  require("forge.views.harness.subagent_profiles").open(state, host)
  instance = picker._state_for_test()
  action("next-field", "worker")
  local before_provider = saves
  press_arrow("h", "worker")
  worker = snapshot.document.profiles.worker
  assert(worker.provider == alternate_provider and worker.model == "alternate-first",
    "provider change must select the first advertised model, not the default model")
  assert(worker.reasoning_effort == "default" and worker.service_tier == "default",
    "provider change retained unsupported thinking or speed")
  assert(saves == before_provider + 1, "dependent changes were not saved atomically")
  action("next-field", "worker")
  press_arrow("h", "worker")
  assert(snapshot.document.profiles.worker.model == "alternate-default", "model column did not cycle")
  action("next-field", "worker")
  press_arrow("h", "worker")
  assert(snapshot.document.profiles.worker.reasoning_effort == "default", "non-reasoning model offered thinking")
  action("next-field", "worker")
  press_arrow("h", "worker")
  assert(snapshot.document.profiles.worker.service_tier == "default", "unsupported provider offered fast speed")

  select("worker")
  focus("provider")
  local before_empty = current_form:values()
  current_form:cycle(1)
  assert(vim.deep_equal(current_form:values(), before_empty) and errors[#errors].message:find("No models available"),
    "empty catalog changed the profile")
  model_failure = "catalog offline"
  current_form:cycle(1)
  assert(vim.deep_equal(current_form:values(), before_empty) and errors[#errors].message:find("catalog offline"))
  model_failure = nil
  current_form:cycle(-1)
  assert(current_form:values().provider == default_provider and current_form:values().model == "fixture-model")
  focus("reasoning_effort")
  current_form:cycle(1)
  current_form:cycle(1)
  assert(current_form:values().reasoning_effort == "high")
  focus("model")
  current_form:cycle(1)
  assert(current_form:values().model == "simple-model" and current_form:values().reasoning_effort == "default",
    "model change retained unsupported thinking")
  focus("reasoning_effort")
  current_form:cycle(1)
  assert(current_form:values().reasoning_effort == "default", "incompatible inherited thinking was offered")
  focus("service_tier")
  current_form:cycle(1)
  assert(current_form:values().service_tier == "default", "model without speed tiers offered fast")
  focus("provider")
  current_form:cycle(1)
  assert(current_form:values().service_tier == "default", "full-page editor retained unsupported speed")

  delayed_models = true
  current_form:cycle(1)
  local stale_catalog = pending_models
  edit("instructions", "Newer draft during catalog lookup")
  stale_catalog({ capability = {}, models = { { id = "empty-first", reasoning = {} } } })
  assert(current_form:values().provider == alternate_provider and current_form:values().instructions == "Newer draft during catalog lookup",
    "late catalog response overwrote newer form edits")
  current_form:reload()
  current_form:close()
  picker.close()

  require("forge.views.harness.subagent_profiles").open(state, host)
  instance = picker._state_for_test()
  action("next-field", "worker")
  local before_late = saves
  press_arrow("h", "worker")
  local pending_provider = pending_models
  press_arrow("h", "worker")
  assert(pending_models == pending_provider, "pending provider switch admitted another mutation")
  picker.close()
  pending_provider({ capability = {}, models = { { id = "empty-first", reasoning = {} } } })
  assert(saves == before_late and not picker.is_open("subagent-profiles"), "closed picker saved a late provider switch")
  delayed_models = false

  local config_text = vim.json.encode(snapshot.document)
  local config_path = temp.file("profile subagents.json", { config_text })
  snapshot.path = config_path
  require("forge.views.harness.subagent_profiles").open(state, host)
  local before_open = saves
  assert(keymap.get("oc").buffer == 1, "config hotkey must be local to the picker")
  keymap.feed("oc")
  assert(not picker.is_open("subagent-profiles"), "config hotkey left the picker open")
  assert(saves == before_open, "config hotkey mutated a profile")
  assert(vim.api.nvim_buf_get_name(0) == vim.fn.fnamemodify(config_path, ":p"), "config hotkey opened the wrong file")
  assert(vim.api.nvim_get_current_tabpage() ~= origin_tab, "config did not open in its own tab")
  assert(vim.bo.buftype == "" and vim.bo.modifiable and not vim.bo.readonly)
  assert(buffer.line(0, 1) == config_text, "config contents changed on open")
  vim.cmd("tabclose")
end
end

return T
