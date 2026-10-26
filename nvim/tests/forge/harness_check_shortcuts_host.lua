vim.loader.enable(false)
vim.opt.runtimepath:prepend(vim.fs.normalize(vim.fn.getcwd()) .. "/nvim")
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
local binary = data .. "/forge-host" .. (vim.fn.has("win32") == 1 and ".exe" or "")
assert(vim.uv.fs_copyfile(require("forge.builder").binary_path(), binary))
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return kind == "data" and data or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = binary }) end)
  return function() end
end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local errors = {}
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = debug.traceback(message, 2) end
local function request(method, params)
  local done, result, failure
  client.request(method, params or {}, function(value, problem) result, failure, done = value, problem, true end)
  assert(vim.wait(10000, function() return done end, 10), method .. " timed out")
  assert(not failure, method .. ": " .. tostring(failure))
  return result
end
local function rows()
  return table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
end
local function press(key)
  vim.api.nvim_set_current_win(state.composer_win)
  local mapping = vim.fn.maparg(key, "n", false, true)
  assert(type(mapping.callback) == "function", "shortcut is not installed: " .. key)
  mapping.callback()
end
local function start_check()
  local run_id = request("checks.run", { check = { "slow" } }).run_id
  assert(vim.wait(5000, function()
    return state.checks_running and state.active_check_run == run_id and rows():find("Shortcut check is running", 1, true)
  end, 10), "check did not become active")
  return run_id
end
local function settled()
  return not state.checks_running and not state.task_operation and not state.busy
    and not state.prompt_submission and not state.check_interrupt_pending
end
local function compose(text)
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { text })
end
local succeeded, failure = xpcall(function()
  vim.fn.writefile({ vim.json.encode({ checks = {
  ["slow"] = { trigger = { auto = false }, action = { command = "print 'Shortcut check is running'; sleep 30sec", label = "Shortcut check", category = "test" } }
} }) }, workspace .. "/.forge.json")
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = false,
    harness = { backend = "mock", backends = { mock = { command = { "mock" }, selectable = true } } } })
  require("forge.views.harness").open()
  assert(vim.wait(10000, function() return state.session ~= nil end, 10), "startup timed out")
  request("session.configure", { shell = "nushell" })
  local run_id = start_check()
  compose("")
  press("<C-c>")
  assert(vim.wait(3000, function() return not state.checks_running end, 10),
    "Ctrl-C did not stop the check: " .. vim.inspect({ operation = state.task_operation, notice = state.execution_notice }))
  assert(request("checks.status", {run_id = run_id}).state == "interrupted", "Ctrl-C did not interrupt check history")
  assert(vim.wait(3000, settled, 10), "Ctrl-C did not settle its pause")
  for _, pause_first in ipairs({ false, true }) do
    run_id = start_check()
    if pause_first then
      compose("")
      press("<C-c>")
      assert(state.task_operation, "Ctrl-C did not start a pause")
    end
    local text = pause_first and "Replacement during pause" or "Replacement during check"
    compose(text)
    press("<C-s>")
    assert(state.check_interrupt_pending, "Ctrl-S did not capture the replacement")
    assert(vim.wait(5000, function() return settled() and rows():find(text, 1, true) end, 10),
      "Ctrl-S did not complete its replacement: " .. vim.inspect({ operation = state.task_operation,
        pending = state.check_interrupt_pending, notice = state.execution_notice, busy = state.busy }))
    assert(request("checks.status", {run_id = run_id}).state == "interrupted", "Ctrl-S did not interrupt check history")
    assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "", "Ctrl-S left the submitted draft")
  end
end, debug.traceback)
require("forge.views.harness.workspace").release(state)
if state.presentation then state.presentation.close() end
client.stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.fn.stdpath = original_stdpath
assert(succeeded and collected, failure or "host did not stop")
assert(#errors == 0, table.concat(errors, "\n"))
print("harness_check_shortcuts_host: passed")
vim.cmd("qa!")
