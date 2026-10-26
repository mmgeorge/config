vim.loader.enable(false)
local root = vim.fs.normalize(vim.fn.getcwd())
vim.opt.runtimepath:prepend(root .. "/nvim")
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
assert(vim.system({ "git", "init", "--quiet", workspace }, { text = true }):wait(10000).code == 0)
vim.fn.writefile({ "before" }, workspace .. "/source.txt")
vim.fn.writefile({ vim.json.encode({ checks = {
  ["verify"] = { trigger = { auto = true, paths = { "source.txt" } }, action = { command = "exit 7", label = "Verify", category = "test" } }
} }) }, workspace .. "/.forge.json")
local executable = require("forge.builder").binary_path()
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind)
  return (kind == "data" or kind == "config") and data or original_stdpath(kind)
end
package.loaded["forge.builder"] = { ensure = function(callback)
  callback({ ok = true, path = executable })
  return function() end
end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local errors = {}
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
local function await(predicate, message)
  assert(vim.wait(20000, function() return #errors > 0 or predicate() end, 10), message)
  assert(#errors == 0, table.concat(errors, "\n"))
end
local success, failure = xpcall(function()
  vim.fn.chdir(workspace)
  require("forge").setup({ harness = { backend = "mock", backends = {
    mock = { command = { "session-blocking" }, selectable = true },
  } } })
  require("forge").open_harness()
  await(function() return state.ready and state.session end, "Harness did not open")
  local configured
  client.request("session.configure", { shell = "nushell" }, function(_, problem)
    assert(not problem, tostring(problem))
    configured = true
  end)
  await(function() return configured end, "shell configuration did not settle")
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "Inspect the fixture" })
  controller.submit()
  local baseline = data .. "/forge/harness/" .. state.session.id .. "/checks/change-baseline.json"
  await(function() return vim.fn.filereadable(baseline) == 1 end, "baseline was not captured")
  vim.fn.writefile({ "after" }, workspace .. "/source.txt")
  controller.set_mode("write")
  await(function()
    return state.session.execution_mode == "write" and not state.busy
      and not state.task_operation and not state.state_sync_pending
  end, "resumed repair did not settle")
  local catalog
  client.request("checks.list", {}, function(value, problem)
    assert(not problem, tostring(problem))
    catalog = value
  end)
  await(function() return catalog ~= nil end, "check history did not load")
  assert(catalog.automatic.blocked, vim.inspect(catalog.automatic))
  assert(#catalog.history == 2, vim.inspect(catalog.history))
  assert(catalog.history[1].state == "failed" and catalog.history[2].state == "failed")
  local exchanges
  client.request("exchange.list", {}, function(value, problem)
    assert(not problem, tostring(problem))
    exchanges = value
  end)
  await(function() return exchanges ~= nil end, "exchange history did not load")
  assert(#exchanges == 1 and exchanges[1].prompt == "Inspect the fixture", vim.inspect(exchanges))
  for _, run in ipairs(catalog.history) do
    assert(run.owner.kind == "exchange" and run.owner.exchange_id == exchanges[1].id
      and run.owner.session_id == state.session.id, "checks lost their exchange owner")
  end
  local _, summary_row = state.presentation.transcript.sequence:position(exchanges[1].id .. ":summary")
  assert(summary_row ~= nil, "owning exchange summary is missing")
  vim.api.nvim_set_current_win(state.transcript_win)
  vim.api.nvim_win_set_cursor(state.transcript_win, { summary_row + 1, 0 })
  assert(state.presentation.toggle_heading(state.transcript_win), "exchange activity did not expand")
  await(function()
    local text = table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
    local _, count = text:gsub("Checks finished", "")
    return count == #catalog.history
  end, "owned checks did not render inside the expanded exchange")
  local transcript = table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
  assert(not transcript:find("Check repair started", 1, true), transcript)
  assert(transcript:find("Checks finished", 1, true), "attached check results are missing")
end, debug.traceback)
require("forge.views.harness.workspace").release(state)
if state.presentation then state.presentation.close() end
client.stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.fn.chdir(root)
vim.fn.stdpath = original_stdpath
if not success or not collected then
  io.stderr:write(tostring(failure or "host was not collected"), "\n")
  vim.cmd("cquit 1")
end
print("harness_check_repair_resume_host: passed")
vim.cmd("qa!")
