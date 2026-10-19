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
local state
local configured_session = {
  id = "settings-session", backend = "codex", model = "mock-model", effort = "medium",
  plan_executor = { enabled = false, model = "mock-model", effort = "medium" }, plan_compact = false,
}
local original_request = client.request_for
local commit_settings = require("forge.integrations.background_settings")
local original_commit_request = commit_settings.request
commit_settings.request = function(_, _, callback)
  callback({ revision = 0,
    settings = { auto_compaction = { enabled = true, threshold_tokens = 128000 }, models = { codex = { model = "mock-model", thinking = "low" },
      copilot = { model = "mock-model", thinking = "low" } } } })
end
client.request_for = function(session_id, method, params, callback)
  assert(session_id == "settings-session")
  assert(method == "trace.status" or method == "trace.configure" or method == "trace.session.clear"
    or method == "session.configure" or method == "backend.models" or method == "checks.editing")
  if method == "backend.models" then
    callback({
      { id = "mock-model", reasoning = { "low", "medium", "high" }, default_reasoning = "medium" },
      { id = "other-model", reasoning = { "low", "high" }, default_reasoning = "low" },
    })
    return
  end
  if method == "checks.editing" then callback({ check_editing = params.enabled }) return end
  if method == "session.configure" then
    for _, key in ipairs({ "model", "effort", "service_tier", "auto_compaction", "approval_timeout", "answer_timeout" }) do
      if params[key] ~= nil then configured_session[key] = params[key] end
    end
    if params.shell then configured_session.shell = params.shell end
    if params.access then configured_session.access = params.access end
    if params.default_write_permission then configured_session.default_write_permission = params.default_write_permission end
    if params.plan_permission then configured_session.plan_permission = params.plan_permission end
    if params.plan_executor_enabled ~= nil then configured_session.plan_executor.enabled = params.plan_executor_enabled end
    if params.plan_compact ~= nil then configured_session.plan_compact = params.plan_compact end
    if params.plan_auto_approve_revisions ~= nil then configured_session.plan_auto_approve_revisions = params.plan_auto_approve_revisions end
    if params.plan_executor_model then configured_session.plan_executor.model = params.plan_executor_model end
    if params.plan_executor_effort then configured_session.plan_executor.effort = params.plan_executor_effort end
    callback(vim.deepcopy(configured_session))
    return
  end
  if method == "trace.configure" then enabled = params.enabled end
  if method == "trace.session.clear" then vim.fn.writefile({}, path) end
  callback({ enabled = enabled, path = path })
end
controller.configure = function(params, _, completed)
  client.request_for("settings-session", "session.configure", params, function(result)
    state.session = result
    if params.status_thoughts ~= nil then state.status_thoughts = params.status_thoughts end
    if params.check_timing then state.check_timing = params.check_timing end
    completed(true)
  end)
end
local success, failure = xpcall(function()
  state = { session = configured_session, capability = { model_selection = true, effort_selection = true, fast_mode = true, ultrafast_mode = true, native_compact = true, os_sandbox = true }, busy = true }
  settings.open(state, { window_list = { vim.api.nvim_get_current_win() }, control_win = vim.api.nvim_get_current_win() })
  local instance = picker._state_for_test()
  local text = table.concat(vim.api.nvim_buf_get_lines(instance.buf, 0, -1, false), "\n")
  assert(not text:find("CLI:", 1, true), text)
  assert(instance.spec.page_list[1].subtitle == nil, "settled configuration should have no subtitle")
  assert(not text:find("Plan Executor", 1, true) and not text:find("Plan Compact", 1, true), text)
  assert(not text:find("[←", 1, true), text)
  assert(not text:find("Description", 1, true), "the permanent description column remains")
  local function find(id)
    local current = picker._state_for_test()
    for index, option in ipairs(current.spec.page_list[1].option_list) do
      for _, field in ipairs(option.fields or {}) do
        if field.id == id then return option, field, index end
      end
    end
    error("Missing field " .. id)
  end
  local function select(id)
    local option, field, index = find(id)
    local current = picker._state_for_test()
    current.state.selected_index_by_page.config = index
    current.state.field_by_page.config = { [option.id] = field.id }
    picker.update(current.spec)
    return field
  end
  local function key(id, binding)
    select(id)
    local current = picker._state_for_test()
    local mapping = vim.api.nvim_buf_call(current.buf, function() return vim.fn.maparg(binding, "n", false, true) end)
    assert(mapping.callback, "missing key " .. binding)
    mapping.callback()
  end
  assert(#instance.spec.page_list[1].option_list == 19, "settings are not grouped into 19 rows")
  assert(find("check-timing").section == "Checks")
  assert(find("plan-permission").section == "Planning")
  assert(find("plan-revisions").section == "Planning")
  assert(find("default-write-permission").section == "Execute")
  for _, row in ipairs(instance.spec.page_list[1].option_list) do
    assert(row.id ~= "permission-defaults" and row.label ~= "Defaults", "obsolete Defaults row remains")
  end
  key("check-timing", "<Right>")
  assert(state.check_timing == "between_work")
  key("check-timing", "<Left>")
  assert(state.check_timing == "after_queue")
  key("status-thoughts", "<Right>")
  assert(state.status_thoughts ~= true, "arrows must not toggle booleans")
  key("status-thoughts", "<Space>")
  assert(state.status_thoughts == true)
  key("status-thoughts", "<Tab>")
  local focused = require("forge.views.picker.state").selected_field(picker._state_for_test().state, picker._state_for_test().spec)
  assert(focused.id == "status-thoughts", "Tab left a single-field row")
  local subagent_row = find("subagent-tools")
  local lsp_row = find("lsp-tools")
  assert(subagent_row ~= lsp_row and #subagent_row.fields == 1 and #lsp_row.fields == 1)
  assert(subagent_row.section == "Tools" and lsp_row.section == "Tools")
  assert(find("status-thoughts").section == "Display")
  local last = picker._state_for_test().spec.page_list[1].option_list[19]
  assert(last.id == "logging" and last.section == "Debug", "Debug must be the final category")
  key("subagent-tools", "<Tab>")
  focused = require("forge.views.picker.state").selected_field(picker._state_for_test().state, picker._state_for_test().spec)
  assert(focused.id == "subagent-tools", "flat tool row requires horizontal navigation")
  key("logging", "<Space>")
  assert(enabled and require("forge.infra.perf").enabled("harness"))
  key("logging", "<Space>")
  assert(not enabled)
  key("check-editing", "<Space>")
  assert(state.check_editing == true)
  key("check-editing", "<Space>")
  assert(state.check_editing == false)
  key("default-write-permission", "<Right>")
  assert(configured_session.default_write_permission == "yolo")
  local _, execute_permission = find("default-write-permission")
  assert(execute_permission.value == "YOLO")
  key("default-write-permission", "<Left>")
  assert(configured_session.default_write_permission == "write")
  local _, execute_permission = find("default-write-permission")
  assert(execute_permission.value == "Write")
  key("plan-permission", "<Right>")
  assert(configured_session.plan_permission == "read")
  key("plan-permission", "<Left>")
  assert(configured_session.plan_permission == vim.NIL)
  key("plan-revisions", "<Space>")
  assert(configured_session.plan_auto_approve_revisions == false)
  key("plan-revisions", "<Space>")
  assert(configured_session.plan_auto_approve_revisions == true)
  key("sandbox", "<Space>")
  assert(configured_session.access.sandbox == false)
  key("write-access", "<Right>")
  assert(configured_session.access.write_access == "full" and configured_session.access.sandbox == false)
  key("compact-threshold", "<Right>")
  assert(configured_session.auto_compaction.threshold_tokens == 128001)
  key("approval_timeout:unit", "<Right>")
  assert(configured_session.approval_timeout.unit == "h")
  key("answer_timeout:amount", "<Left>")
  assert(configured_session.answer_timeout.amount == 9)
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
  find("native-import")
  find("readable-directories")
  find("writable-directories")
  key("provider", "<Left>")
  assert(picker.is_open("harness-config"), "Left opened an action")
  picker.close()
  configured_session.backend, configured_session.provider_label = "copilot", "Copilot CLI"
  state.capability.os_sandbox = false
  settings.open(state, { window_list = { vim.api.nvim_get_current_win() }, control_win = vim.api.nvim_get_current_win() })
  local _, sandbox = find("sandbox")
  assert(sandbox.disabled_reason:find("unavailable", 1, true), "unsupported sandbox is not explained")
  local failure_message
  local original_warn = require("forge.infra.notifications").warn
  require("forge.infra.notifications").warn = function(message) failure_message = message end
  key("sandbox", "<Space>")
  require("forge.infra.notifications").warn = original_warn
  assert(failure_message and configured_session.access.sandbox == false)
  key("read-access", "<Right>")
  assert(configured_session.access.read_access == "workspace")
  key("write-access", "<Right>")
  assert(configured_session.access.write_access == "workspace")
  picker.close()
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
client.request_for = original_request
commit_settings.request = original_commit_request
vim.fn.delete(path)
if not success then error(failure) end
print("harness_settings: passed")
vim.cmd("qa!")
