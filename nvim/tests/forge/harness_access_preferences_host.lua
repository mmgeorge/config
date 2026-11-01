vim.loader.enable(false)
local root = vim.fs.normalize(vim.fn.getcwd())
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
assert(vim.fn.mkdir(workspace, "p") == 1 and vim.fn.mkdir(data, "p") == 1)
assert(vim.system({ "git", "init", workspace }, { text = true }):wait(10000).code == 0)
vim.fn.writefile({ vim.json.encode({ checks = vim.empty_dict() }) }, workspace .. "/.forge.json")
local executable = data .. "/forge" .. (vim.fn.has("win32") == 1 and ".exe" or "")
assert(vim.uv.fs_copyfile(vim.g.forge_test_executable or require("forge.builder").binary_path(), executable))
local original_stdpath, original_notify = vim.fn.stdpath, vim.notify
vim.fn.stdpath = function(kind) return (kind == "data" or kind == "config") and data or original_stdpath(kind) end
local errors = {}
vim.notify = function(message, level)
  if level == vim.log.levels.ERROR then errors[#errors + 1] = tostring(message) end
end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = executable }) end)
  return function() end
end }
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local function await(predicate, message)
  assert(vim.wait(10000, function() return #errors > 0 or predicate() end, 10), message .. " " .. vim.inspect({
    approval_mode = state.session and state.session.approval_mode, task_operation = state.task_operation,
    configuring = state.configuring, task_config = state.task_config,
  }))
  assert(#errors == 0, table.concat(errors, "\n"))
end
local function request(method, params)
  local result, failure, done
  client.request({ method = method, params = params, session_id = require("forge.event_contract").method_policy(method).session_context ~= "none" and assert(require("forge.session").harness.session).id or nil }, function(value, error) result, failure, done = value, error, true end)
  await(function() return done end, method .. " did not settle")
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
  local mapping = vim.api.nvim_buf_call(selected.instance.buf, function() return vim.fn.maparg(key, "n", false, true) end)
  mapping.callback()
end
local function open_settings()
  settings.open(state, { window_list = { state.transcript_win, state.composer_win }, control_win = state.composer_win })
  await(function() return picker.is_open("harness-config") end, "configuration did not open")
end
local success, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = false, harness = { backend = "mock" } })
  require("forge.views.harness").open()
  await(function() return state.presentation and state.presentation.ready end, "Harness did not open")
  assert(not state.capability.os_sandbox and not state.session.access.sandbox)
  open_settings()
  assert(field("sandbox").value.muted and not field("sandbox").value.disabled_reason)
  change("sandbox", "<CR>")
  await(function() return state.access_preference.sandbox and field("sandbox").value.value end, "muted sandbox preference did not save")
  assert(not state.session.access.sandbox, "mock provider enabled OS isolation")
  picker.close(false)
  open_settings()
  assert(field("sandbox").value.value and field("sandbox").value.muted, "reopening lost the selected preference")
  if vim.fn.has("win32") == 1 then
    assert(field("windows-sandbox").value.muted)
    change("windows-sandbox", "<Right>")
    await(function() return state.access_preference.windows_sandbox == "unelevated" end, "muted Windows mode did not save")
  end
  change("readable-directories", "<CR>")
  await(function() return vim.bo.buftype == "acwrite" end, "directory editor did not open")
  local directory_buffer = vim.api.nvim_get_current_buf()
  local directory_row = vim.api.nvim_win_get_cursor(0)[1] - 1
  vim.api.nvim_buf_set_text(directory_buffer, directory_row, 0, directory_row, 0, { workspace })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = directory_buffer })
  vim.cmd("write")
  await(function() return #state.access_preference.readable_directory == 1 and not vim.bo[directory_buffer].modified end,
    "directory preference did not save")
  assert(state.access_preference.sandbox and not state.session.access.sandbox,
    "directory editing overwrote the saved sandbox preference")
  vim.fn.maparg("q", "n", false, true).callback()
  await(function() return picker.is_open("harness-config") end, "directory editor did not return to configuration")
  for _, mode in ipairs({ "deny", "allow", "ask" }) do
    change("approval-mode", "<Right>")
    await(function() return state.session.approval_mode == mode and not state.busy end,
      "tool approval mode did not apply: " .. mode)
    assert(state.access_preference.sandbox and not state.session.access.sandbox,
      "approval mode overwrote saved sandbox preference or effective access")
  end
  for _ = 1, 3 do change("approval_timeout", "<Right>") end
  await(function()
    return not state.input_timeout_configuration.in_flight
      and state.session.approval_timeout.amount == 13
  end, "rapid timeout edits did not retain both final values")
  assert(field("approval_timeout").value.value == "13m")
  local selected = request("settings.get", {})
  assert(selected.desired.approval_timeout.amount == 13)
  assert(selected.desired.access.sandbox and not state.session.access.sandbox)
  assert(request("session.task.get", {}) == nil, "configuration started a task")
  picker.close(false)
  require("forge.views.harness.workspace").release(state)
  assert(state.presentation.close())
end, debug.traceback)
picker.close(false)
require("forge.views.harness.workspace").release(state)
client.host_stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(root)
vim.fn.stdpath, vim.notify = original_stdpath, original_notify
vim.fn.delete(workspace, "rf")
vim.fn.delete(data, "rf")
assert(success and collected, failure or "Harness host was not collected")
print("harness_access_preferences_host: passed")
vim.cmd("qa!")
