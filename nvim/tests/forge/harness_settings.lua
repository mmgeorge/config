vim.loader.enable(false)
vim.o.columns = 130
vim.o.lines = 50
require("forge").setup({ harness = { backend = "mock" } })
local client = require("forge.client")
local settings = require("forge.views.harness.settings")
local picker = require("forge.views.picker")
local path = vim.fn.tempname() .. ".jsonl"
vim.fn.writefile({ '{"timestamp_ms":1700000000123,"event":"request","session_id":"settings-session","payload":{"method":"turn/start","items":["a,b","c"]}}' }, path)
local enabled = false
local controller = require("forge.views.harness.controller")
local original_configure = controller.configure
local original_transition = controller.task_transition
local original_clue = package.loaded["mini.clue"]
local source_buffer = vim.api.nvim_get_current_buf()
local source_clue_disabled = vim.b[source_buffer].miniclue_disable
local clue_requests = 0
package.loaded["mini.clue"] = {
  ensure_buf_triggers = function(buffer)
    clue_requests = clue_requests + 1
    if vim.b[buffer].miniclue_disable then return end
    vim.keymap.set("n", "<Space>", function()
      error("mini.clue replaced the settings toggle")
    end, { buffer = buffer, nowait = true })
  end,
}
local state
local configured_session = {
  id = "settings-session", backend = "codex", model = "mock-model", effort = "medium",
  plan_executor = { enabled = false, model = "mock-model", effort = "medium" }, plan_compact = false,
}
local original_request = client.request
local commit_settings = require("forge.integrations.background_settings")
local original_commit_request = commit_settings.request
commit_settings.request = function(_, _, callback)
  callback({ revision = 0,
    settings = { auto_compaction = { enabled = true, threshold_tokens = 128000 }, models = { codex = { model = "mock-model", thinking = "low" },
      copilot = { model = "mock-model", thinking = "low" },
      claude = { model = "mock-model" }, antigravity = { model = "mock-model" }, mock = { model = "mock-model" } } } })
end
client.request = function(request_specification, callback)
  local session_id = request_specification.session_id
  local method = request_specification.method
  local params = request_specification.params or {}

  local context = require("forge.event_contract").method_policy(method).session_context
  assert(context == "required" and session_id == "settings-session"
    or context ~= "required" and (session_id == nil or session_id == "settings-session"))
  assert(method == "host.trace.status" or method == "host.trace.configure" or method == "session.trace.clear"
    or method == "session.configure" or method == "session.provider.model.list" or method == "session.check.editing.set")
  if method == "session.provider.model.list" then
    callback({
      { id = "mock-model", reasoning = { "low", "medium", "high" }, default_reasoning = "medium", service_tiers = { "fast", "ultrafast" } },
      { id = "other-model", reasoning = { "low", "high" }, default_reasoning = "low", service_tiers = { "fast", "ultrafast" } },
    })
    return
  end
  if method == "session.check.editing.set" then
    callback({ check_editing = params.enabled })
    return
  end
  if method == "session.configure" then
    if params.ask_model then
      state.ask_model = vim.deepcopy(params.ask_model)
      callback({ ask_model = state.ask_model })
      return
    end
    for _, key in ipairs({ "model", "effort", "service_tier", "auto_compaction", "approval_timeout", "answer_timeout" }) do
      if params[key] ~= nil then configured_session[key] = params[key] end
    end
    if params.shell then configured_session.shell = params.shell end
    if params.access then
      configured_session.access = vim.deepcopy(params.access)
      if configured_session.backend ~= "codex" then configured_session.access.sandbox = false end
    end
    if params.plan_executor_enabled ~= nil then configured_session.plan_executor.enabled = params.plan_executor_enabled end
    if params.plan_compact ~= nil then configured_session.plan_compact = params.plan_compact end
    if params.plan_executor_model then configured_session.plan_executor.model = params.plan_executor_model end
    if params.plan_executor_effort then configured_session.plan_executor.effort = params.plan_executor_effort end
    callback(vim.deepcopy(configured_session))
    if params.session_tools ~= nil then state.session_tools = params.session_tools end
    return
  end
  if method == "host.trace.configure" then enabled = params.enabled end
  if method == "session.trace.clear" then vim.fn.writefile({}, path) end
  callback({ enabled = enabled, path = path })
end
controller.configure = function(params, _, completed)
  client.request({ session_id = "settings-session", method = "session.configure", params = params }, function(result)
    state.session = result
    if params.status_thoughts ~= nil then state.status_thoughts = params.status_thoughts end
    if params.check_timing then state.check_timing = params.check_timing end
    if params.automatic_checks ~= nil then state.automatic_checks = params.automatic_checks end
    completed(true)
  end)
end
controller.task_transition = function(action, _, completed)
  assert(action.action == "permission" and action.change.kind == "set_approval")
  configured_session.approval_mode = action.change.mode
  state.session.approval_mode = action.change.mode
  completed(true)
end
local success, failure = xpcall(function()
  state = { session = configured_session, capability = { model_selection = true, effort_selection = true, fast_mode = true, ultrafast_mode = true, native_compact = true, os_sandbox = true }, busy = true }
  local host = { window_list = { vim.api.nvim_get_current_win() }, control_win = vim.api.nvim_get_current_win() }
  settings.open(state, host)
  local instance = picker._state_for_test()
  assert(clue_requests > 0, "settings did not exercise the mini.clue integration")
  assert(vim.b[instance.buf].miniclue_disable == true, "picker permits leader triggers to replace its bindings")
  assert(vim.b[source_buffer].miniclue_disable == source_clue_disabled, "picker disabled source-buffer clues")
  package.loaded["mini.clue"].ensure_buf_triggers(instance.buf)
  local text = table.concat(vim.api.nvim_buf_get_lines(instance.buf, 0, -1, false), "\n")
  assert(not text:find("CLI:", 1, true), text)
  assert(instance.spec.page_list[1].subtitle == nil, "settled configuration should have no subtitle")
  assert(not text:find("Plan Executor", 1, true) and not text:find("Plan Compact", 1, true), text)
  assert(not text:find("[←", 1, true), text)
  assert(not text:find("Description", 1, true), "the permanent description column remains")
  local function find(id)
    local current = picker._state_for_test()
    for page_index, page in ipairs(current.spec.page_list) do
      for index, option in ipairs(page.option_list) do
        for _, field in ipairs(option.fields or {}) do
          if field.id == id then return option, field, index, page_index, page.id end
        end
      end
    end
    error("Missing field " .. id)
  end
  local function select(id)
    local option, field, index, page_index, page_id = find(id)
    local current = picker._state_for_test()
    current.state.page_index = page_index
    current.state.selected_index_by_page[page_id] = index
    current.state.field_by_page[page_id] = { [option.id] = field.id }
    picker.update(current.spec)
    return field
  end
  local function press(binding)
    local current = picker._state_for_test()
    local mapping = vim.api.nvim_buf_call(current.buf, function() return vim.fn.maparg(binding, "n", false, true) end)
    assert(mapping.callback, "missing key " .. binding)
    mapping.callback()
  end
  local function key(id, binding)
    select(id)
    press(binding)
  end
  local function assert_configuration_geometry()
    local current = picker._state_for_test()
    local bounds = require("forge.views.picker.layout").host_bounds(host.window_list)
    local window = vim.api.nvim_win_get_config(current.win)
    assert(window.width + 2 == bounds.width, "configuration does not fill its host width")
    assert(window.height + 2 == math.floor(bounds.height * 0.8), "configuration height is not 80% of its host")
    assert(window.col == bounds.left, "configuration is not anchored to the left")
    assert(window.row == bounds.bottom - window.height - 2, "configuration is not anchored to the bottom")
    assert(#current.frame.lines == window.height, "short category did not fill the fixed window")
    local help = current.frame.content_range[#current.frame.content_range]
    assert(help.last == window.height and help.last - help.first + 1 == 2, "description moved away from the bottom")
  end
  assert_configuration_geometry()
  assert(instance.spec.page_navigation == "tabs" and #instance.spec.page_list == 5)
  local space_mapping = vim.api.nvim_buf_call(instance.buf, function()
    return vim.fn.maparg("<Space>", "n", false, true)
  end)
  assert(space_mapping.buffer == 1 and space_mapping.callback, "configuration lost the generic Space toggle binding")
  assert(instance.spec.hint_mode == "help")
  local footer = vim.api.nvim_win_get_config(instance.win).footer
  assert(type(footer) == "table" and vim.trim(footer[1][1]) == "? help",
    "configuration must show only the help shortcut")
  local row_count, row_id_set = 0, {}
  for page_index, page in ipairs(instance.spec.page_list) do
    assert(page.id == ({ "models", "workflow", "permissions", "tools", "system" })[page_index])
    for _, row in ipairs(page.option_list) do
      assert(not row_id_set[row.id], "setting row occurs in multiple categories")
      row_id_set[row.id] = true
      row_count = row_count + 1
      assert(page.toggle_on_confirm == true, "configuration does not use Enter for toggles")
      assert(page.field_description_height == 2, "configuration help must reserve exactly two lines")
      assert(row.fields[1].description, "configuration description is missing: " .. row.id)
      for index = 2, #row.fields do
        assert(row.fields[index].join_previous, "independent settings share a row")
      end
      assert(row.id ~= "permission-defaults" and row.label ~= "Defaults", "obsolete Defaults row remains")
    end
    key(page.option_list[1].fields[1].id, "<Up>")
    assert(picker._state_for_test().state.selected_index_by_page[page.id] == 1,
      "Up wrapped from the first setting in " .. page.id)
    key(page.option_list[#page.option_list].fields[1].id, "<Down>")
    assert(picker._state_for_test().state.selected_index_by_page[page.id] == #page.option_list,
      "Down wrapped from the last setting in " .. page.id)
    assert_configuration_geometry()
  end
  vim.o.columns, vim.o.lines = 82, 30
  picker.update(picker._state_for_test().spec)
  assert_configuration_geometry()
  vim.o.columns, vim.o.lines = 130, 50
  picker.update(picker._state_for_test().spec)
  assert_configuration_geometry()
  assert(row_count > 22, "compound settings have not been split into individual rows")
  for _, section in ipairs({ "Main model", "Background model" }) do
    local rows = vim.tbl_filter(function(row) return row.section == section end, instance.spec.page_list[1].option_list)
    assert(rows[#rows - 1].label == "Compact" and rows[#rows].label == "Threshold",
      "model section must end with Compact and Threshold")
  end
  for _, page in ipairs(instance.spec.page_list) do
    for _, row in ipairs(page.option_list) do
      assert(row.section ~= "Main compaction" and row.section ~= "Background compaction",
        "separate compaction section remains")
    end
  end
  assert(find("ask-model").section == "Ask model")
  local _, ask_field = find("ask-model")
  assert(ask_field.value == configured_session.model and ask_field.disabled_reason)
  local override_row, override_field = find("ask-override")
  assert(override_row.label == "Override" and override_field.value == false)
  assert(override_field.description:find("main model used for planning", 1, true))
  local ask_rows = vim.tbl_filter(function(row) return row.section == "Ask model" end, instance.spec.page_list[1].option_list)
  assert(ask_rows[1].id == "ask-override" and #ask_rows == 4)
  local _, thinking_field = find("ask-thinking")
  local _, speed_field = find("ask-speed")
  assert(thinking_field.disabled_reason and speed_field.disabled_reason)
  key("ask-override", "<CR>")
  assert(state.ask_model.model == configured_session.model and state.ask_model.effort == configured_session.effort)
  assert(state.ask_model.service_tier == "default")
  local _, enabled_model = find("ask-model")
  assert(not enabled_model.disabled_reason)
  select("session-tools")
  local before_help = picker.capture_state()
  key("session-tools", "?")
  local help_window = vim.api.nvim_get_current_win()
  assert(vim.bo.filetype == "ForgeHelp", "? did not open picker help")
  assert(vim.api.nvim_win_get_config(help_window).zindex > vim.api.nvim_win_get_config(picker._state_for_test().win).zindex,
    "help opened behind the picker")
  local help_text = table.concat(vim.api.nvim_buf_get_lines(0, 0, -1, false), "\n")
  assert(help_text:find("<Space>", 1, true) and help_text:find("<CR>", 1, true), "help omitted activation bindings: " .. help_text)
  assert(help_text:find("Toggle, edit, or open", 1, true))
  assert(help_text:find("Show the next picker page", 1, true))
  local close_help = vim.fn.maparg("q", "n", false, true)
  assert(close_help.callback, "help has no close mapping")
  close_help.callback()
  assert(not vim.api.nvim_win_is_valid(help_window))
  assert(vim.deep_equal(picker.capture_state(), before_help), "closing help changed the selected setting")
  key("ask-model", "<Right>")
  assert(state.ask_model.model == "other-model" and configured_session.model == "mock-model")
  settings.open(state, host)
  key("ask-thinking", "<Right>")
  assert(state.ask_model.effort == "high" and configured_session.effort == "medium")
  key("ask-speed", "<Right>")
  assert(state.ask_model.service_tier == "fast" and configured_session.model == "mock-model")
  key("ask-override", "<Space>")
  assert(state.ask_model.model == nil and state.ask_model.effort == nil and state.ask_model.service_tier == nil)
  local _, inherited_model = find("ask-model")
  assert(inherited_model.value == configured_session.model and inherited_model.disabled_reason)
  key("ask-model", "<Right>")
  assert(state.ask_model.model == nil, "disabled Ask model changed without Override")
  assert(find("session-tools").section == "Tools")
  key("session-tools", "<CR>")
  assert(state.session_tools == false)
  assert(find("session-tools").fields[1].value == false, "confirmed history setting kept the stale picker value")
  key("session-tools", "<Space>")
  assert(state.session_tools == true)
  assert(find("session-tools").fields[1].value == true)
  assert(find("check-timing").section == "Checks")
  local workflow_rows = picker._state_for_test().spec.page_list[2].option_list
  assert(workflow_rows[1].id == "automatic-checks", "Automatic must be the first check setting")
  for _, row in ipairs(workflow_rows) do
    assert(row.id ~= "plan-approval", "unused planning approval must not appear in configuration")
    assert(row.id ~= "default-approval", "new-session approval defaults must not appear in configuration")
    assert(row.id ~= "plan-revisions", "task acceptance must not appear in global configuration")
  end
  for _, row in ipairs(instance.spec.page_list[1].option_list) do
    assert(row.id ~= "permission-defaults" and row.label ~= "Defaults", "obsolete Defaults row remains")
  end
  key("check-timing", "<Right>")
  assert(state.check_timing == "between_work")
  key("check-timing", "<Left>")
  assert(state.check_timing == "after_queue")
  assert(find("automatic-checks").section == "Checks")
  key("automatic-checks", "<Right>")
  assert(state.automatic_checks ~= true, "arrows toggled automatic checks")
  key("automatic-checks", "<CR>")
  assert(state.automatic_checks == true and state.check_timing == "after_queue")
  assert(find("automatic-checks").fields[1].value == true)
  key("automatic-checks", "<CR>")
  assert(state.automatic_checks == false and state.check_timing == "after_queue")
  key("status-thoughts", "<Right>")
  assert(state.status_thoughts ~= true, "arrows must not toggle booleans")
  key("status-thoughts", "<CR>")
  assert(state.status_thoughts == true)
  key("status-thoughts", "<Tab>")
  assert(picker._state_for_test().spec.page_list[picker._state_for_test().state.page_index].id == "models",
    "Tab did not wrap from the last category to the first")
  press("<S-Tab>")
  assert(picker._state_for_test().spec.page_list[picker._state_for_test().state.page_index].id == "system",
    "Shift-Tab did not wrap from the first category to the last")
  local focused = require("forge.views.picker.state").selected_field(picker._state_for_test().state, picker._state_for_test().spec)
  assert(focused.id == "status-thoughts", "category cycling lost its selected row")
  local subagent_row = find("subagent-tools")
  local lsp_row = find("lsp-tools")
  assert(subagent_row ~= lsp_row and #subagent_row.fields == 1 and #lsp_row.fields == 1)
  assert(subagent_row.section == "Tools" and lsp_row.section == "Tools")
  assert(find("status-thoughts").section == "Display")
  local all_pages = picker._state_for_test().spec.page_list
  local rows = all_pages[#all_pages].option_list
  local last = rows[#rows]
  assert(last.id == "logging" and last.section == "Debug", "Debug must be the final category")
  key("subagent-tools", "<Tab>")
  assert(picker._state_for_test().spec.page_list[picker._state_for_test().state.page_index].id == "system",
    "Tab did not switch to the next category")
  press("<S-Tab>")
  assert(picker._state_for_test().spec.page_list[picker._state_for_test().state.page_index].id == "tools",
    "Shift-Tab did not switch to the previous category")
  focused = require("forge.views.picker.state").selected_field(picker._state_for_test().state, picker._state_for_test().spec)
  assert(focused.id == "subagent-tools", "returning to a category lost its selected setting")
  local timeout_row = find("approval_timeout")
  assert(#timeout_row.fields == 1 and timeout_row.fields[1].value == "10m", "timeout must be one combined value")
  assert(not pcall(find, "approval_timeout:unit") and not pcall(find, "answer_timeout:unit"), "timeout retained a unit field")
  key("logging", "<CR>")
  assert(enabled and require("forge.infra.perf").enabled("harness"))
  key("logging", "<CR>")
  assert(not enabled)
  local check_editing = find("check-editing")
  assert(check_editing.section == "Checks" and check_editing.label == "Allow editing")
  local access_rows = picker._state_for_test().spec.page_list[3].option_list
  local last_permission
  for _, row in ipairs(access_rows) do
    if row.section == "Checks" then last_permission = row end
  end
  assert(last_permission.id == "check-editing", "Allow editing must be in its own Checks subsection")
  key("check-editing", "<CR>")
  assert(state.check_editing == true and find("check-editing").fields[1].value == true)
  key("check-editing", "<Space>")
  assert(state.check_editing == false)
  local mode_row, mode_field, _, _, mode_page = find("approval-mode")
  assert(mode_page == "permissions" and mode_row.section == "Tools" and mode_field.value == "Ask")
  for _, choice in ipairs({ { "deny", "Deny" }, { "allow", "Approve" }, { "ask", "Ask" } }) do
    key("approval-mode", "<Right>")
    assert(configured_session.approval_mode == choice[1] and find("approval-mode").fields[1].value == choice[2])
  end
  local approval_row, _, _, _, approval_page = find("approval_timeout")
  assert(approval_row.section == "Tools" and approval_page == "permissions")
  assert(not pcall(find, "answer_timeout"), "optional questions must not have an answer timeout")
  local enabled_description = find("sandbox").fields[1].description
  assert(enabled_description:find("Network access is enabled", 1, true))
  key("sandbox", "<CR>")
  assert(configured_session.access.sandbox == false)
  local disabled_description = find("sandbox").fields[1].description
  assert(disabled_description ~= enabled_description and disabled_description:find("without OS sandbox", 1, true))
  assert(table.concat(picker._state_for_test().frame.lines, " "):find("without OS sandbox", 1, true),
    "sandbox option description did not reach the rendered help area")
  if vim.fn.has("win32") == 1 then
    local windows_row, windows_field = find("windows-sandbox")
    assert(windows_field.muted and not windows_field.disabled_reason and not windows_field.description:find("Inactive", 1, true))
    local _, spans = require("forge.views.picker.field").columns(windows_row, "windows-sandbox", true)
    assert(spans[2][1][2] == "ForgePickerHint", "disabled Windows mode is not gray when focused")
    local saved_mode = configured_session.access.windows_sandbox
    key("windows-sandbox", "<Right>")
    assert(configured_session.access.windows_sandbox ~= saved_mode, "muted Windows mode could not be changed")
    assert(find("windows-sandbox").fields[1].muted, "changing the saved mode enabled an inactive control")
    key("windows-sandbox", "<Left>")
    key("sandbox", "<CR>")
    assert(not find("windows-sandbox").fields[1].muted)
    key("windows-sandbox", "<Right>")
    local unelevated_description = find("windows-sandbox").fields[1].description
    assert(unelevated_description:find("Restricted token", 1, true) and unelevated_description:find("denied-read paths", 1, true))
    key("windows-sandbox", "<Right>")
    local mxc_description = find("windows-sandbox").fields[1].description
    assert(mxc_description:find("Microsoft Execution Containers", 1, true)
      and mxc_description:find("compatible Windows 11", 1, true)
      and mxc_description:find("Falls back to Elevated", 1, true))
    assert(table.concat(picker._state_for_test().frame.lines, " "):find("Microsoft Execution Containers", 1, true),
      "Windows mode changes did not refresh the rendered description")
    key("windows-sandbox", "<Right>")
    assert(find("windows-sandbox").fields[1].description:find("Setup needs admin approval", 1, true))
    key("sandbox", "<CR>")
  end
  key("write-access", "<Right>")
  assert(configured_session.access.write_access == "full" and configured_session.access.sandbox == false)
  key("compact-threshold", "<Right>")
  assert(configured_session.auto_compaction.threshold_tokens == 256000)
  key("compact-threshold", "<CR>")
  local threshold_picker = picker._state_for_test()
  local threshold_rows = threshold_picker.spec.page_list[1].option_list
  assert(vim.deep_equal(vim.tbl_map(function(row) return row.label end, threshold_rows), { "64k", "128k", "256k", "512k" }))
  press("<Down>")
  press("<CR>")
  assert(configured_session.auto_compaction.threshold_tokens == 512000)
  assert(require("forge.views.picker.state").selected_field(picker._state_for_test().state, picker._state_for_test().spec).id == "compact-threshold")
  key("approval_timeout", "<CR>")
  local editor = assert(picker._state_for_test().field_editor)
  local increase = vim.api.nvim_buf_call(editor.buffer, function() return vim.fn.maparg("<Up>", "n", false, true) end)
  increase.callback()
  local save = vim.api.nvim_buf_call(editor.buffer, function() return vim.fn.maparg("<CR>", "n", false, true) end)
  save.callback()
  assert(configured_session.approval_timeout.amount == 11 and configured_session.approval_timeout.unit == "m"
    and not picker._state_for_test().field_editor)
  key("main-model", "<Right>")
  assert(configured_session.model == "other-model" and configured_session.effort == "low")
  key("main-thinking", "<Right>")
  assert(configured_session.effort == "high")
  key("main-speed", "<Right>")
  assert(configured_session.service_tier == "fast")
  local executable = vim.fn.executable
  vim.fn.executable = function(name) return name == "nu" and 0 or executable(name) end
  key("shell", "<Right>")
  assert(configured_session.shell == "system")
  vim.fn.executable = function(name) return name == "nu" and 1 or executable(name) end
  key("shell", "<Right>")
  assert(configured_session.shell == "nushell")
  key("shell", "<Right>")
  assert(configured_session.shell == "system")
  vim.fn.executable = executable
  for _, page in ipairs(picker._state_for_test().spec.page_list) do
    for _, option in ipairs(page.option_list) do
      assert(option.id ~= "native-import", "session import must not appear in configuration")
    end
  end
  assert(find("read-access").section == "Access")
  assert(find("write-access").section == "Access")
  assert(find("readable-directories").section == "Access")
  assert(find("writable-directories").section == "Access")
  assert(find("sandbox").section == "Sandbox")
  if vim.fn.has("win32") == 1 then assert(find("windows-sandbox").section == "Sandbox") end
  key("provider", "<Left>")
  assert(picker.is_open("harness-config"), "Left opened an action")
  picker.close()
  configured_session.backend, configured_session.provider_label = "copilot", "Copilot CLI"
  state.capability.os_sandbox = false
  settings.open(state, { window_list = { vim.api.nvim_get_current_win() }, control_win = vim.api.nvim_get_current_win() })
  local _, sandbox = find("sandbox")
  assert(sandbox.muted and not sandbox.disabled_reason, "unsupported sandbox must remain editable and muted")
  assert(sandbox.description:find("only with Codex", 1, true))
  if vim.fn.has("win32") == 1 then
    local _, windows_mode = find("windows-sandbox")
    assert(windows_mode.muted and not windows_mode.disabled_reason
      and windows_mode.description:find("Codex on Windows only", 1, true))
  end
  local failure_message
  local original_warn = require("forge.infra.notifications").warn
  require("forge.infra.notifications").warn = function(message) failure_message = message end
  key("sandbox", "<CR>")
  require("forge.infra.notifications").warn = original_warn
  assert(not failure_message and configured_session.access.sandbox == false)
  assert(state.access_preference.sandbox and find("sandbox").fields[1].value == true,
    "unsupported provider lost the selected sandbox preference")
  picker.close()
  settings.open(state, { window_list = { vim.api.nvim_get_current_win() }, control_win = vim.api.nvim_get_current_win() })
  assert(find("sandbox").fields[1].value == true, "reopening displayed effective access instead of the preference")
  key("read-access", "<Right>")
  assert(configured_session.access.read_access == "workspace")
  key("write-access", "<Right>")
  assert(configured_session.access.write_access == "off")
  key("write-access", "<Right>")
  assert(configured_session.access.write_access == "workspace")
  picker.close()
  for _, backend in ipairs({ "copilot", "claude", "antigravity", "mock" }) do
    configured_session.backend = backend
    state.capability.os_sandbox = true
    settings.open(state, { window_list = { vim.api.nvim_get_current_win() }, control_win = vim.api.nvim_get_current_win() })
    for _, id in ipairs(vim.fn.has("win32") == 1 and { "sandbox", "windows-sandbox" } or { "sandbox" }) do
      local row, field = find(id)
      assert(field.muted and not field.disabled_reason, backend .. " did not keep " .. id .. " editable and muted")
      local _, spans = require("forge.views.picker.field").columns(row, id, true)
      assert(spans[2][1][2] == "ForgePickerHint", backend .. " did not gray out " .. id)
      local before = field.value
      key(id, id == "sandbox" and "<CR>" or "<Right>")
      assert(find(id).fields[1].value ~= before, backend .. " could not change muted " .. id)
      assert(not configured_session.access.sandbox, "unsupported provider enabled runtime isolation")
    end
    picker.close()
  end
  settings.log("settings-session", "on")
  assert(enabled)
  local before = #vim.api.nvim_list_tabpages()
  settings.log("settings-session", "open")
  assert(#vim.api.nvim_list_tabpages() == before + 1)
  assert(vim.bo.readonly and not vim.bo.modifiable)
  local log_buffer = vim.api.nvim_get_current_buf()
  local log_text = table.concat(vim.api.nvim_buf_get_lines(log_buffer, 0, -1, false), "\n")
  assert(log_text:find("request  #1", 1, true), log_text)
  assert(log_text:find('"method": "turn/start"', 1, true), log_text)
  assert(log_text:find('"a,b"', 1, true), log_text)
  vim.fn.delete(path)
  vim.fn.maparg("R", "n", false, true).callback()
  assert(vim.api.nvim_buf_get_lines(log_buffer, 0, -1, false)[1] == "No log events yet")
  vim.fn.writefile({ '{"timestamp_ms":1700000000123,"event":"request","payload":{"method":"turn/start"}}' }, path)
  vim.fn.writefile({ '{"timestamp_ms":1700000000123,"event":"response","payload":{"ok":true}}' }, path, "a")
  vim.fn.maparg("R", "n", false, true).callback()
  log_text = table.concat(vim.api.nvim_buf_get_lines(log_buffer, 0, -1, false), "\n")
  assert(log_text:find("response  #2", 1, true), log_text)
  settings.log("settings-session")
  assert(#vim.api.nvim_list_tabpages() == before + 1, "log open duplicated the tab")
  assert(vim.api.nvim_get_current_buf() == log_buffer)
  settings.log("settings-session", "clear")
  assert(enabled)
  assert(vim.api.nvim_buf_get_lines(log_buffer, 0, -1, false)[1] == "No log events yet")
  settings.log("settings-session", "off")
  assert(not enabled)
end, debug.traceback)
controller.configure = original_configure
controller.task_transition = original_transition
package.loaded["mini.clue"] = original_clue
client.request = original_request
commit_settings.request = original_commit_request
vim.fn.delete(path)
if not success then error(failure) end
print("harness_settings: passed")
vim.cmd("qa!")
