vim.loader.enable(false)
local root = vim.fs.normalize(vim.fn.getcwd())
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
assert(vim.system({ "git", "init", "--quiet", workspace }, { text = true }):wait(10000).code == 0)
vim.fn.writefile({ "before" }, workspace .. "/source.txt")
vim.fn.writefile({ vim.json.encode({ checks = { version = 2, definitions = {
  { id = "verify", name = "Verify", category = "test", area = "Fixture", enabled = true, auto = true,
    trigger = { kind = "files_changed", include = { "source.txt" } }, all = { command = "exit 7" } },
  { id = "optional", name = "Optional", category = "test", area = "Fixture", enabled = true, auto = false,
    trigger = { kind = "any_change" }, all = { command = "print optional" } },
} } }) }, workspace .. "/.forge.json")
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
local function request(method, params)
  local settled, value, failure
  client.request(method, params or {}, function(result, error) value, failure, settled = result, error, true end)
  assert(vim.wait(20000, function() return settled end, 10), method .. " timeout")
  assert(not failure, method .. ": " .. tostring(failure))
  return value
end
local succeeded, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ harness = { backend = "mock", backends = { mock = { command = { "session-blocking" }, selectable = true } } } })
  require("forge.views.harness").open()
  assert(vim.wait(10000, function() return state.session ~= nil end, 10), "startup timeout")
  request("session.configure", { shell = "nushell" })
  local done, completed, problem
  client.request("prompt.submit", { text = "Inspect the fixture" }, function(value, error)
    completed, problem, done = value, error, true
  end)
  local baseline = data .. "/forge/harness/" .. state.session.id .. "/checks/change-baseline.json"
  assert(vim.wait(10000, function() return vim.fn.filereadable(baseline) == 1 end, 10), "initial check baseline was not captured")
  assert(not done, "fixture provider completed before the source change")
  vim.fn.writefile({ "after" }, workspace .. "/source.txt")
  assert(vim.wait(20000, function() return done end, 10), "automatic checks did not settle")
  assert(not problem, tostring(problem))
  assert(completed.automatic_checks and completed.automatic_checks.blocked, vim.inspect(completed))
  local catalog = request("checks.list")
  local attempts, reused = 0, 0
  for _, run in ipairs(catalog.history) do
    for _, command in ipairs(run.commands) do
      if command.context.definition_id == "verify" then
        if type(command.started_at_ms) == "number" then attempts = attempts + 1 end
        if type(command.reused_from) == "table" then reused = reused + 1 end
      else
        assert(command.state == "skipped", "nonautomatic check ran without selection")
      end
    end
  end
  assert(attempts == 1 and reused == 1, vim.inspect({ attempts = attempts, reused = reused }))
  assert(catalog.automatic.pending and catalog.automatic.blocked)
  request("checks.override", { enabled = vim.empty_dict(), scopes = vim.empty_dict(), auto = { verify = false } })
  local selected = request("checks.run", { ids = { "verify", "optional" } }).run_id
  local run
  assert(vim.wait(10000, function()
    run = request("checks.status", { run_id = selected })
    return run.state ~= "running"
  end, 20), "explicit checks did not settle")
  assert(#run.commands == 2 and run.commands[1].state == "failed" and run.commands[2].state == "passed")
end, debug.traceback)
require("forge.views.harness.workspace").release(state)
if state.presentation then state.presentation.close() end
client.stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(root)
vim.fn.stdpath = original_stdpath
assert(succeeded and collected, failure or "automatic check host was not collected")
print("harness_automatic_checks: passed")
