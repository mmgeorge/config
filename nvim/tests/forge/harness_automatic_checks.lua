vim.loader.enable(false)
local root = vim.fs.normalize(vim.fn.getcwd())
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
assert(vim.system({ "git", "init", "--quiet", workspace }, { text = true }):wait(10000).code == 0)
vim.fn.writefile({ "before" }, workspace .. "/source.txt")
vim.fn.writefile({ vim.json.encode({ checks = {
  ["verify"] = { trigger = { auto = true, paths = { "source.txt" } }, action = { command = "exit 7", label = "Verify", category = "test" } },
  ["optional"] = { trigger = { auto = false }, action = { command = "print optional", label = "Optional", category = "test" } },
  ["waiting"] = { trigger = { auto = false }, action = { command = "sleep 30sec", label = "Waiting", category = "test" } },
  ["handoff"] = { trigger = { auto = false }, action = { command = "sleep 200ms; print complete", label = "Handoff", category = "test" } }
} }) }, workspace .. "/.forge.json")
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
  local selected = request("checks.run", { check = { "verify", "optional" } }).run_id
  local run
  assert(vim.wait(10000, function()
    run = request("checks.status", { run_id = selected })
    return run.state ~= "running"
  end, 20), "explicit checks did not settle")
  assert(run.commands[1].state == "failed" and run.commands[2].state == "passed")
  local waiting = request("checks.run", { check = { "waiting" } }).run_id
  assert(vim.wait(5000, function() return state.checks_running and state.active_check_run == waiting end, 10),
    "check activity did not reach the composer")
  local controller = require("forge.views.harness.controller")
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "queued after checking" })
  controller.queue_submit()
  assert(#state.queue == 1 and request("checks.status", { run_id = waiting }).state == "running",
    "queueing interrupted checks")
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "interrupt checking now" })
  controller.submit()
  assert(vim.wait(5000, function()
    return request("checks.status", { run_id = waiting }).state == "interrupted"
  end, 10), "direct submission did not interrupt checks")
  assert(vim.wait(5000, function()
    local snapshot = request("state.get")
    for _, prompt in ipairs(snapshot.prompt_history or {}) do
      if prompt.text == "interrupt checking now" then return true end
    end
    return false
  end, 10), "direct submission was not admitted after check interruption")
  assert(#state.queue == 1, "queued prompt overtook the direct prompt")
  assert(vim.wait(15000, function() return not state.busy and #state.queue == 0 end, 10),
    "existing queue did not finish: " .. tostring(state.execution_notice))
  for attempt = 1, 3 do
    local handoff = request("checks.run", { check = { "handoff" } }).run_id
    assert(vim.wait(5000, function() return state.checks_running end, 10), "handoff check did not start")
    local prompt = "queued after normal check completion " .. attempt
    vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { prompt })
    controller.queue_submit()
    assert(#state.queue == 1, "check completion queue lost its pending message")
    assert(vim.wait(15000, function()
      return not state.busy and not state.prompt_submission and #state.queue == 0
    end, 10), "normal check completion did not drain its queue: " .. tostring(state.execution_notice))
    assert(not state.execution_notice, tostring(state.execution_notice))
    assert(request("checks.status", { run_id = handoff }).state == "passed", "queued prompt interrupted normal checks")
    local occurrences = 0
    for _, entry in ipairs(request("state.get").prompt_history or {}) do
      if entry.text == prompt then occurrences = occurrences + 1 end
    end
    assert(occurrences == 1, "queued prompt was lost or submitted more than once")
  end
end, debug.traceback)
require("forge.views.harness.workspace").release(state)
if state.presentation then state.presentation.close() end
client.stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(root)
vim.fn.stdpath = original_stdpath
assert(succeeded and collected, failure or "automatic check host was not collected")
print("harness_automatic_checks: passed")
