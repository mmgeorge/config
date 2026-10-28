vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
vim.loader.enable(false)
if not vim.g.forge_status_split_grid then
  local child = vim.fn.jobstart({ vim.v.progpath, "--embed", "--headless", "-u", "NONE", "-i", "NONE" }, { rpc = true })
  assert(child > 0)
  local ok, failure = pcall(function()
    vim.rpcrequest(child, "nvim_ui_attach", 100, 30, { rgb = true })
    vim.rpcrequest(child, "nvim_exec_lua", [[vim.g.forge_status_split_grid = true; dofile('nvim/tests/forge/harness_status_split_host.lua')]], {})
  end)
  vim.fn.jobstop(child)
  assert(ok, failure)
  print("harness_status_split_host: passed at 100x30")
  return
end
local original_directory = vim.fn.getcwd()
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
local executable = data .. "/forge-host" .. (vim.fn.has("win32") == 1 and ".exe" or "")
assert(vim.uv.fs_copyfile(require("forge.builder").binary_path(), executable))
local original_stdpath, original_notify = vim.fn.stdpath, vim.notify
vim.fn.stdpath = function(kind) return kind == "data" and data or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback) vim.schedule(function() callback({ ok = true, path = executable }) end) end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local errors = {}
vim.notify = function(message, level) if level == vim.log.levels.ERROR then errors[#errors + 1] = tostring(message) end end
local function request(method, params)
  local done, result, failure
  client.request(method, params or {}, function(value, error_message) result, failure, done = value, error_message, true end)
  assert(vim.wait(10000, function() return done end, 10), method .. " timed out")
  assert(not failure, failure)
  return result
end
local ok, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = false,
    harness = { backend = "mock", backends = { mock = { command = { "visible-blocking" }, selectable = true } } } })
  require("forge.views.harness").open()
  assert(vim.wait(10000, function() return state.presentation and state.presentation.ready end, 10), table.concat(errors, "\n"))
  assert(state.presentation.status_view.window == nil)
  vim.api.nvim_set_current_win(state.composer_win)
  local prompt_failure
  client.request("prompt.submit", { text = "Keep status visible" }, function(_, error_message) prompt_failure = error_message end)
  assert(vim.wait(5000, function() return state.presentation.status_view.window ~= nil end, 10), table.concat(errors, "\n"))
  assert(vim.api.nvim_get_current_win() == state.composer_win)
  local view = state.presentation.status_view
  assert(vim.api.nvim_win_get_config(view.window).focusable == false)
  assert(not table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n"):find("Working ·", 1, true))
  vim.api.nvim_set_current_win(state.transcript_win)
  local cursor = vim.api.nvim_win_get_cursor(state.transcript_win)
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "first typed line", "second typed line", "third typed line" })
  require("forge.views.harness.layout").resize_composer(state.composer_buf, state.composer_win)
  assert(vim.deep_equal(cursor, vim.api.nvim_win_get_cursor(state.transcript_win)))
  assert(vim.api.nvim_get_current_win() == state.transcript_win)
  local preview = require("forge.views.harness.session_preview")
  preview.open(state.transcript_win, view)
  assert(view.window == nil)
  preview.close()
  assert(view.window ~= nil)
  request("turn.cancel")
  assert(vim.wait(5000, function() return view.snapshot and #view.snapshot.block == 0 end, 10), table.concat(errors, "\n"))
  assert(view.window == nil)
  assert(prompt_failure == nil or prompt_failure:find("cancelled", 1, true))
  assert(#errors == 0, table.concat(errors, "\n"))
end, debug.traceback)
require("forge.views.harness.workspace").release(state)
if state.presentation then state.presentation.close() end
client.stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.fn.stdpath, vim.notify = original_stdpath, original_notify
vim.api.nvim_set_current_dir(original_directory)
vim.fn.delete(workspace, "rf")
vim.fn.delete(data, "rf")
assert(ok and collected, failure or "status fixture host did not stop")
