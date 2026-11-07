vim.loader.enable(false)
local check_tool = dofile(vim.fn.getcwd() .. "/nvim/tests/forge/fixtures/check_tool.lua")
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
-- Launch the shared pinned executable lease rather than a freshly scanned per-run copy.
binary = dofile(vim.fs.dirname(debug.getinfo(1, "S").source:sub(2)) .. "/support/host_executable.lua")()
-- Resolve before the fixture changes into its workspace; the script path is relative to the launch directory.
local check_storage = dofile(vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p:h") .. "/support/check_storage.lua")
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return kind == "data" and data or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = binary }) end)
  return function() end
end }
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local state_snapshot = dofile(vim.fs.dirname(debug.getinfo(1, "S").source:sub(2)) .. "/support/state.lua")
local function request(method, params)
  -- Snapshot reads replay their publication page; see support/state.lua.
  if method == "session.state.get" then return state_snapshot.get() end
  local settled, value, failure
  client.request({ method = method, params = params or {}, session_id = require("forge.event_contract").method_policy(method).session_context ~= "none" and assert(require("forge.session").harness.session).id or nil }, function(result, error) value, failure, settled = result, error, true end)
  assert(vim.wait(20000, function() return settled end, 10), method .. " timeout")
  assert(not failure, method .. ": " .. tostring(failure))
  return value
end
local succeeded, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ harness = { provider = "mock", providers = { mock = { command = { "session-blocking" }, selectable = true } } } })
  require("forge.views.harness").open()
  assert(vim.wait(10000, function() return state.session ~= nil end, 10), "startup timeout")
  request("session.configure", { shell = "nushell", automatic_checks = true })
  local activity = require("forge.views.harness.activity")
  -- session.prompt.submit settles at admission, so turn and check completion are observed through
  -- published activity and the check catalog rather than the submit callback.
  local admitted, problem
  client.request({ method = "session.prompt.submit", params = { text = "Inspect the fixture" }, session_id = assert(require("forge.session").harness.session).id }, function(_, error)
    problem, admitted = error, true
  end)
  local baseline = check_storage.path(data, state.session.id, "change-baseline.json")
  assert(vim.wait(10000, function() return vim.fn.filereadable(baseline) == 1 end, 10), "initial check baseline was not captured")
  assert(vim.wait(5000, function() return admitted and activity.busy(state) end, 10),
    "fixture provider completed before the source change: " .. tostring(problem))
  assert(not problem, tostring(problem))
  vim.fn.writefile({ "after" }, workspace .. "/source.txt")
  local catalog
  assert(vim.wait(20000, function()
    if activity.busy(state) or state.checks_running then return false end
    catalog = request("session.check.list")
    return catalog.automatic.blocked ~= nil and catalog.automatic.blocked ~= vim.NIL
  end, 50), "automatic checks did not settle: " .. vim.inspect(catalog and catalog.automatic))
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
  request("session.check.override", { enabled = vim.empty_dict(), scopes = vim.empty_dict(), auto = { verify = false } })
  local selected = check_tool.start(request, { check = { "verify", "optional" } }).run_id
  local run
  assert(vim.wait(10000, function()
    run = request("session.check.status", { run_id = selected })
    return run.state ~= "running"
  end, 20), "explicit checks did not settle")
  -- Run records list every configured definition, with unselected ones skipped, so match by definition.
  local command_state = {}
  for _, command in ipairs(run.commands) do command_state[command.context.definition_id] = command.state end
  assert(command_state.verify == "failed" and command_state.optional == "passed", vim.inspect(vim.tbl_map(function(command)
    return { definition = command.context and command.context.definition_id, state = command.state }
  end, run.commands)))
  local waiting = check_tool.start(request, { check = { "waiting" } }).run_id
  assert(vim.wait(5000, function() return state.checks_running and state.active_check_run == waiting end, 10),
    "check activity did not reach the composer")
  local controller = require("forge.views.harness.controller")
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "queued after checking" })
  controller.queue_submit()
  assert(#state.queue == 1 and request("session.check.status", { run_id = waiting }).state == "running",
    "queueing interrupted checks")
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "interrupt checking now" })
  controller.submit()
  assert(vim.wait(5000, function()
    return request("session.check.status", { run_id = waiting }).state == "interrupted"
  end, 10), "direct submission did not interrupt checks: " .. vim.inspect({
    check = request("session.check.status", { run_id = waiting }).state, input = request("session.input.get"),
    drafts = state.input.drafts, notice = state.execution_notice }))
  -- prompt_history records composer recall at submission; admission is the exchange it starts.
  assert(vim.wait(10000, function()
    for _, exchange in ipairs(request("session.exchange.list", {}) or {}) do
      if exchange.prompt == "interrupt checking now" then return true end
    end
    return false
  end, 10), "direct submission was not admitted after check interruption")
  assert(#state.queue == 1, "queued prompt overtook the direct prompt")
  assert(vim.wait(15000, function() return not require("forge.views.harness.activity").busy(state) and #state.queue == 0 end, 10),
    "existing queue did not finish: " .. tostring(state.execution_notice))
  for attempt = 1, 3 do
    local handoff = check_tool.start(request, { check = { "handoff" } }).run_id
    assert(vim.wait(5000, function() return state.checks_running end, 10), "handoff check did not start")
    local prompt = "queued after normal check completion " .. attempt
    vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { prompt })
    controller.queue_submit()
    assert(#state.queue == 1, "check completion queue lost its pending message")
    assert(vim.wait(15000, function()
      return not require("forge.views.harness.activity").busy(state) and not state.prompt_submission and #state.queue == 0
    end, 10), "normal check completion did not drain its queue: " .. tostring(state.execution_notice))
    assert(not state.execution_notice, tostring(state.execution_notice))
    assert(request("session.check.status", { run_id = handoff }).state == "passed", "queued prompt interrupted normal checks")
    local occurrences = 0
    for _, entry in ipairs(request("session.state.get").prompt_history or {}) do
      if entry.text == prompt then occurrences = occurrences + 1 end
    end
    assert(occurrences == 1, "queued prompt was lost or submitted more than once")
  end
end, debug.traceback)
require("forge.views.harness.workspace").release(state)
if state.presentation then state.presentation.close() end
client.host_stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(root)
vim.fn.stdpath = original_stdpath
assert(succeeded and collected, failure or "automatic check host was not collected")
print("harness_automatic_checks: passed")
