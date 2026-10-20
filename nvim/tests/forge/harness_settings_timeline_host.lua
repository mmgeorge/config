vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
vim.loader.enable(false)
local stage = "settings-timeline"
arg[2] = vim.fn.tempname()
local executable = vim.fn.fnamemodify(require("forge.builder").binary_path(), ":p")
local data = assert(arg[2])
local workspace = vim.fs.joinpath(data, stage)
vim.fn.mkdir(workspace, "p")
local stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return (kind == "data" or kind == "config") and data or stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  callback({ ok = true, path = executable })
  return function() end
end }
require("forge").setup({ harness = { backend = "mock" },
  background_generation = { mock = { model = "mock-model", reasoning_effort = "low" } } })
vim.fn.chdir(workspace)
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local errors = {}
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
local function wait(predicate, message)
  assert(vim.wait(15000, function() return #errors > 0 or predicate() end, 10), message)
  assert(#errors == 0, table.concat(errors, "\n"))
end
require("forge").open_harness()
wait(function() return state.ready and state.session end, "Harness did not start")
local configured
require("forge.views.harness.controller").configure({ service_tier = "fast" }, true, function(applied)
  configured = applied
end)
wait(function() return configured ~= nil end, "Service tier validation did not finish")
assert(configured, "Service tier validation failed")
wait(function() return state.session.service_tier == "fast" end, "Service tier was not applied")
local snapshot
client.request("session.preview", { session_id = state.session.id }, function(result, failure)
  assert(not failure, failure)
  snapshot = result
end)
wait(function() return snapshot ~= nil end, "State did not refresh")
assert(type(snapshot.lines) == "table", "Session preview omitted rendered lines")
assert(not table.concat(snapshot.lines, "\n"):find("Settings completed", 1, true),
  "Completed settings appeared in the session preview")
local text = table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
assert(not text:find("Settings completed", 1, true), "Completed settings appeared in the transcript")
client.stop()
assert(vim.wait(5000, function() return not client._client.process end, 10), "Host did not stop")
print("harness_settings_timeline_host: validated speed without a completion row")
vim.cmd("qa!")
