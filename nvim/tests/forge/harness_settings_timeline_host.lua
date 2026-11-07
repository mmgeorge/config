vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
vim.loader.enable(false)
-- Settings changes stay out of the conversation, and the session-history setting toggles live
-- through the configuration picker. The inspection tools themselves are covered by the
-- `tools::session` Rust tests.
local executable = vim.fn.fnamemodify(require("forge.builder").binary_path(), ":p")
local data = vim.fn.tempname()
local workspace = vim.fs.joinpath(data, "settings-timeline")
vim.fn.mkdir(workspace, "p")
local stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return (kind == "data" or kind == "config") and data or stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  callback({ ok = true, path = executable })
  return function() end
end }
require("forge").setup({ harness_logging = false, harness = { provider = "mock" },
  background_generation = { mock = { model = "mock-model", reasoning_effort = "low" } } })
vim.fn.chdir(workspace)
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local errors = {}
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
local function wait(predicate, message)
  assert(vim.wait(10000, function() return #errors > 0 or predicate() end, 10), message)
  assert(#errors == 0, table.concat(errors, "\n"))
end
require("forge").open_harness()
wait(function() return state.ready and state.session end, "Harness did not start")
local configured
require("forge.views.harness.controller").configure({ service_tier = "fast" }, function(applied)
  configured = applied
end)
wait(function() return configured ~= nil end, "Service tier validation did not finish")
assert(configured, "Service tier validation failed")
wait(function() return state.session.service_tier == "fast" end, "Service tier was not applied")
local snapshot
client.request({ method = "session.preview", params = { session_id = state.session.id }, session_id = state.session.id },
  function(result, failure)
    assert(not failure, failure)
    snapshot = result
  end)
wait(function() return snapshot ~= nil end, "State did not refresh")
assert(type(snapshot.lines) == "table", "Session preview omitted rendered lines")
assert(not table.concat(snapshot.lines, "\n"):find("Settings completed", 1, true),
  "Completed settings appeared in the session preview")
local text = table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
assert(not text:find("Settings completed", 1, true), "Completed settings appeared in the transcript")

local picker = require("forge.views.picker")
require("forge.views.harness.settings").open(state, {
  window_list = { state.transcript_win, state.composer_win }, control_win = state.composer_win,
})
wait(function() return picker.is_open("harness-config") end, "configuration picker did not open")
local function history_field()
  local instance = picker._state_for_test()
  for page_index, page in ipairs(instance.spec.page_list) do
    for index, option in ipairs(page.option_list) do
      for _, field in ipairs(option.fields or {}) do
        if field.id == "session-tools" then return instance, option, field, index, page_index, page.id end
      end
    end
  end
  error("Session history field is missing")
end
local function toggle_history()
  local instance, option, field, index, page_index, page_id = history_field()
  instance.state.page_index = page_index
  instance.state.selected_index_by_page[page_id] = index
  instance.state.field_by_page[page_id] = { [option.id] = field.id }
  picker.update(instance.spec)
  local mapping = vim.api.nvim_buf_call(instance.buf, function() return vim.fn.maparg(" ", "n", false, true) end)
  assert(mapping.callback, "Session history has no Space mapping")
  mapping.callback()
end
toggle_history()
wait(function() local _, _, field = history_field() return state.session_tools == false and field.value == false end,
  "persisted off state did not reach the live picker")
toggle_history()
wait(function() local _, _, field = history_field() return state.session_tools == true and field.value == true end,
  "persisted on state did not reach the live picker")
picker.close()
client.host_stop()
assert(vim.wait(5000, function() return not client._client.process end, 10), "Host did not stop")
print("harness_settings_timeline_host: settings stayed out of the conversation and session history toggled live")
vim.cmd("qa!")
