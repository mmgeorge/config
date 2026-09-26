vim.loader.enable(false)
require("forge").setup({ harness = { backend = "mock" } })
local client = require("forge.client")
local settings = require("forge.views.harness.settings")
local picker = require("forge.views.picker")
local path = vim.fn.tempname() .. ".jsonl"
vim.fn.writefile({ '{"event":"test"}' }, path)
local enabled = false
local original_request = client.request_for
client.request_for = function(session_id, method, params, callback)
  assert(session_id == "settings-session")
  assert(method == "trace.status" or method == "trace.configure")
  if method == "trace.configure" then enabled = params.enabled end
  callback({ enabled = enabled, path = path })
end
local success, failure = xpcall(function()
  local state = { session = { id = "settings-session", backend = "codex" }, busy = true }
  settings.open(state, { window_list = { vim.api.nvim_get_current_win() }, control_win = vim.api.nvim_get_current_win() })
  local instance = picker._state_for_test()
  local text = table.concat(vim.api.nvim_buf_get_lines(instance.buf, 0, -1, false), "\n")
  assert(text:find("CLI: Codex CLI", 1, true), text)
  assert(text:find("Logging", 1, true) and text:find("Off", 1, true), text)
  vim.fn.maparg("<Right>", "n", false, true).callback()
  assert(enabled and require("forge.infra.perf").enabled("harness"))
  vim.fn.maparg("<Left>", "n", false, true).callback()
  assert(not enabled and not require("forge.infra.perf").enabled("harness"))
  picker.close()
  settings.log("settings-session", "on")
  assert(enabled)
  local before = #vim.api.nvim_list_tabpages()
  settings.log("settings-session", "open")
  assert(#vim.api.nvim_list_tabpages() == before + 1)
  assert(vim.bo.readonly and not vim.bo.modifiable)
  assert(vim.api.nvim_get_current_line():find('"event"', 1, true))
  settings.log("settings-session")
  assert(#vim.api.nvim_list_tabpages() == before + 1, "log open duplicated the tab")
  settings.log("settings-session", "off")
  assert(not enabled)
end, debug.traceback)
client.request_for = original_request
vim.fn.delete(path)
if not success then error(failure) end
print("harness_settings: passed")
vim.cmd("qa!")
