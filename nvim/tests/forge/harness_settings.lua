vim.loader.enable(false)
vim.o.columns = 130
require("forge").setup({ harness = { backend = "mock" } })
local client = require("forge.client")
local settings = require("forge.views.harness.settings")
local picker = require("forge.views.picker")
local path = vim.fn.tempname() .. ".jsonl"
vim.fn.writefile({ '{"timestamp_ms":1700000000123,"event":"request","session_id":"settings-session","payload":{"method":"turn/start","items":["a,b","c"]}}' }, path)
local enabled = false
local configured_session = {
  id = "settings-session", backend = "codex", model = "mock-model", effort = "medium",
  plan_executor = { enabled = false, model = "mock-model", effort = "medium" }, plan_compact = false,
}
local original_request = client.request_for
client.request_for = function(session_id, method, params, callback)
  assert(session_id == "settings-session")
  assert(method == "trace.status" or method == "trace.configure" or method == "trace.session.clear"
    or method == "session.configure" or method == "backend.models")
  if method == "backend.models" then
    callback({
      { id = "mock-model", reasoning = { "low", "medium", "high" }, default_reasoning = "medium" },
      { id = "other-model", reasoning = { "low", "high" }, default_reasoning = "low" },
    })
    return
  end
  if method == "session.configure" then
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
local success, failure = xpcall(function()
  local state = { session = configured_session, capability = { model_selection = true, native_compact = true }, busy = true }
  settings.open(state, { window_list = { vim.api.nvim_get_current_win() }, control_win = vim.api.nvim_get_current_win() })
  local instance = picker._state_for_test()
  local text = table.concat(vim.api.nvim_buf_get_lines(instance.buf, 0, -1, false), "\n")
  assert(text:find("CLI: Codex CLI", 1, true), text)
  assert(text:find("Logging", 1, true) and text:find("Off", 1, true), text)
  assert(not text:find("Plan Executor", 1, true) and not text:find("Plan Compact", 1, true), text)
  assert(text:find("Description", 1, true), text)
  assert(not text:find("[←", 1, true), text)
  assert(instance.spec.page_list[1].option_list[3].label == "Auto-approve plan revisions")
  local options = instance.spec.page_list[1].option_list
  assert(options[1].id == "default-write-permission" and options[2].id == "plan-permission")
  instance.spec.action_list[2].callback({ option = options[1] }, instance)
  assert(configured_session.default_write_permission == "full")
  instance.spec.action_list[1].callback({ option = options[1] }, instance)
  assert(configured_session.default_write_permission == "write")
  instance.spec.action_list[2].callback({ option = options[2] }, instance)
  assert(configured_session.plan_permission == "read")
  instance.spec.action_list[1].callback({ option = options[2] })
  assert(configured_session.plan_permission == vim.NIL)
  instance.spec.action_list[2].callback({ option = options[3] })
  assert(configured_session.plan_auto_approve_revisions == false)
  instance.spec.action_list[1].callback({ option = options[3] })
  assert(configured_session.plan_auto_approve_revisions == true)
  local logging = instance.spec.page_list[1].option_list[4]
  instance.spec.action_list[1].callback({ option = logging })
  assert(enabled and require("forge.infra.perf").enabled("harness"))
  instance.spec.action_list[1].callback({ option = logging })
  assert(not enabled and not require("forge.infra.perf").enabled("harness"))
  assert(#instance.spec.page_list[1].option_list == 5)
  local provider = instance.spec.page_list[1].option_list[5]
  assert(provider.id == "provider" and provider.columns[2] == "Codex CLI")
  instance.spec.action_list[1].callback({ option = provider })
  assert(not enabled and picker.is_open("harness-config"), "Left opened the provider picker")
  assert(picker.is_open("harness-config"))
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
client.request_for = original_request
vim.fn.delete(path)
if not success then error(failure) end
print("harness_settings: passed")
vim.cmd("qa!")
