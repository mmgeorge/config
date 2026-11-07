vim.loader.enable(false)
local check_tool = dofile(vim.fn.getcwd() .. "/nvim/tests/forge/fixtures/check_tool.lua")
local root = vim.fs.normalize(vim.fn.getcwd())
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
assert(vim.system({ "git", "init", "--quiet", workspace }, { text = true }):wait(10000).code == 0)
vim.fn.writefile({ "before" }, workspace .. "/source.txt")
vim.fn.writefile({ vim.json.encode({ checks = {
  ["verify"] = { trigger = { auto = true, paths = { "source.txt" } }, action = { command = "print queue-verified", label = "Queue verification", category = "test" } }
} }) }, workspace .. "/.forge.json")
local binary = data .. "/forge-host" .. (vim.fn.has("win32") == 1 and ".exe" or "")
-- Launch the shared pinned executable lease rather than a freshly scanned per-run copy.
binary = dofile(vim.fs.dirname(debug.getinfo(1, "S").source:sub(2)) .. "/support/host_executable.lua")()
-- Resolve before the fixture changes into its workspace; the script path is relative to the launch directory.
local check_storage = dofile(vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p:h") .. "/support/check_storage.lua")
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return (kind == "data" or kind == "config") and data or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = binary }) end)
  return function() end
end }
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local starts, failures = 0, {}
require("forge.infra.notifications").error = function(message) failures[#failures + 1] = message end
client.session_events_subscribe(function(event, payload)
  if event == "provider_event" and payload.kind == "turn_started" then starts = starts + 1 end
end)
local function await(predicate, message)
  assert(vim.wait(15000, function() return #failures > 0 or predicate() end, 10),
    message .. "\n" .. vim.inspect({ queue = state.queue, task = state.session.active_task,
      operation = state.task_operation, input = state.input and { revision = state.input.revision,
        pending = state.input.pending, suspended = state.input.suspended }, status = state.status,
      busy = require("forge.views.harness.activity").busy(state), state_sync_pending = state.state_sync_pending, checks_running = state.checks_running,
      checks_deferred = state.checks_deferred, automatic_checks = state.automatic_checks }))
  assert(#failures == 0, table.concat(failures, "\n"))
end
local state_snapshot = dofile(vim.fs.dirname(debug.getinfo(1, "S").source:sub(2)) .. "/support/state.lua")
local function request(method, params)
  -- Snapshot reads replay their publication page; see support/state.lua.
  if method == "session.state.get" then return state_snapshot.get() end
  local done, result, failure
  client.request({ method = method, params = params or {}, session_id = require("forge.event_contract").method_policy(method).session_context ~= "none" and assert(require("forge.session").harness.session).id or nil }, function(value, problem) result, failure, done = value, problem, true end)
  await(function() return done end, method .. " did not complete")
  assert(not failure, tostring(failure))
  return result
end
local function submit(text, queued)
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { text })
  if queued then controller.queue_submit() else controller.submit() end
end
-- Lua copies checks_deferred only from state snapshots, and no event republishes it when queue
-- completion clears the gate, so deferral is read from the host's authoritative snapshot.
local function host_deferred()
  return request("session.state.get").checks_deferred == true
end
local function completed()
  if require("forge.views.harness.activity").busy(state) or state.state_sync_pending
    or state.checks_running or #state.queue > 0 then return false end
  -- Read both gate fields from one snapshot: the host clears deferral as it admits the completion run.
  local snapshot = request("session.state.get")
  return snapshot.checks_deferred ~= true and snapshot.checks_running ~= true
end
local function executed()
  local count = 0
  for _, run in ipairs(request("session.check.list").history) do
    for _, command in ipairs(run.commands) do
      if command.state == "passed" and type(command.started_at_ms) == "number" then count = count + 1 end
    end
  end
  return count
end
-- The host clears deferral while it admits the completion run, before that run reports activity,
-- so a gate that must run checks is settled only once its execution is recorded.
local function completed_after(execution_count)
  return function() return completed() and executed() >= execution_count end
end
local success, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ harness = { provider = "mock", providers = {
    mock = { command = { "session-blocking" }, selectable = true },
  } } })
  require("forge.views.harness").open()
  await(function() return state.session ~= nil end, "startup timeout")
  await(completed, "startup configuration did not settle")
  request("session.configure", { shell = "nushell" })
  assert(request("session.state.get").check_timing == "after_queue")
  assert(request("session.state.get").automatic_checks == false, "automatic checks must default off")
  local check_policy = request("session.check.list")
  assert(check_policy.automatic_checks == false, "check catalog must expose default disablement")
  assert(check_policy.check_timing == "after_queue", "check catalog must expose default timing")
  request("session.configure", { automatic_checks = true })

  submit("First queued change")
  await(function() return starts == 1 end, "first item did not start")
  local live_setting
  controller.configure({ check_timing = "after_queue" }, function(value) live_setting = value end)
  await(function() return live_setting end, "active timing configuration failed")
  assert(starts == 1 and require("forge.views.harness.activity").busy(state), "changing timing restarted the active provider")
  local baseline = check_storage.path(data, state.session.id, "change-baseline.json")
  await(function() return vim.fn.filereadable(baseline) == 1 end, "change baseline missing")
  local owner = vim.json.decode(table.concat(vim.fn.readfile(baseline), "\n")).owner
  submit("Second queued item", true)
  vim.fn.writefile({ "first batch edit" }, workspace .. "/source.txt")
  await(function() return starts == 2 end, "queue did not advance")
  assert(executed() == 0, "checks ran between queued work under the default")
  await(completed_after(1), "queue did not finish its completion checks")
  assert(executed() == 1, "final gate lost the earlier queued edit")
  assert(vim.json.decode(table.concat(vim.fn.readfile(baseline), "\n")).owner == owner,
    "the last item replaced the first change baseline")
  if vim.g.forge_check_queue_first_batch then return end

  local applied
  controller.configure({ check_timing = "between_work" }, function(value) applied = value end)
  await(function() return applied end, "timing configuration failed")
  assert(request("settings.get").desired.check_timing == "between_work", "timing was not persisted")
  submit("First item with intermediate checks")
  await(function() return starts == 3 end, "intermediate batch did not start")
  await(function() return vim.json.decode(table.concat(vim.fn.readfile(baseline), "\n")).owner ~= owner end,
    "intermediate batch did not capture its baseline")
  owner = vim.json.decode(table.concat(vim.fn.readfile(baseline), "\n")).owner
  submit("Second item with intermediate checks", true)
  vim.fn.writefile({ "second batch edit" }, workspace .. "/source.txt")
  await(function() return starts == 4 end, "intermediate queue did not advance")
  assert(executed() == 2, "between-work checks did not finish before the next item: " .. vim.inspect(request("session.check.list").history))
  await(completed, "intermediate batch did not finish")

  applied = nil
  controller.configure({ check_timing = "after_queue" }, function(value) applied = value end)
  await(function() return applied end, "default timing could not be restored")
  owner = vim.json.decode(table.concat(vim.fn.readfile(baseline), "\n")).owner
  submit("Change before deleting queued work")
  await(function() return starts == 5 end, "deletion batch did not start")
  await(function() return vim.json.decode(table.concat(vim.fn.readfile(baseline), "\n")).owner ~= owner end,
    "deletion batch did not capture its baseline")
  submit("Remove this pending item", true)
  -- Edit the host-confirmed queued entry; until admission settles the composer only holds a local draft.
  await(function()
    for _, entry in ipairs(state.input.confirmed) do
      if entry.text == "Remove this pending item" and entry.delivery == "queued" then return true end
    end
    return false
  end, "queued item was not confirmed by the host")
  controller.edit_queued(-1)
  assert(state.queue_edit, "queued item did not enter editing")
  vim.fn.writefile({ "third batch edit" }, workspace .. "/source.txt")
  await(function() return not require("forge.views.harness.activity").busy(state) and host_deferred() end, "editing did not retain the deferred gate")
  assert(executed() == 2, "checks ran while queued work remained")
  submit("")
  await(completed_after(3), "removing the final queued item did not run checks")
  assert(starts == 5 and executed() == 3, "queue removal lost validation or started deleted work: "
    .. vim.inspect({ starts = starts, executed = executed(), history = vim.tbl_map(function(run)
      return { trigger = run.context and run.context.trigger, state = run.state, owner = run.owner,
        commands = vim.tbl_map(function(command) return { state = command.state, started_at_ms = command.started_at_ms, reused = command.reused_from ~= nil } end, run.commands), started_at_ms = run.started_at_ms }
    end, request("session.check.list").history) }))

  submit("Work with automatic checks disabled")
  await(function() return starts == 6 end, "disabled-check work did not start")
  applied = nil
  controller.configure({ automatic_checks = false }, function(value) applied = value end)
  await(function() return applied end, "automatic checks could not be disabled during work")
  assert(starts == 6 and require("forge.views.harness.activity").busy(state), "disabling checks restarted the provider")
  assert(request("session.state.get").automatic_checks == false and state.automatic_checks == false)
  assert(request("settings.get").desired.automatic_checks == false, "disabled setting was not persisted")
  assert(request("session.check.list").automatic_checks == false, "check catalog did not observe live disablement")
  vim.fn.writefile({ "edit with completion checking disabled" }, workspace .. "/source.txt")
  await(completed, "disabled-check work did not complete")
  assert(executed() == 3 and state.check_timing == "after_queue", "disabled gate ran or lost its timing")
  local explicit = check_tool.start(request, { check = { "verify" } })
  local checked = request("session.check.status", { run_id = explicit.run_id, wait_for_completion = true })
  assert(checked.state == "passed" and executed() == 4, "disabling automatic checks disabled explicit runs")
  applied = nil
  controller.configure({ automatic_checks = true }, function(value) applied = value end)
  await(function() return applied end, "automatic checks could not be enabled")
  assert(request("session.check.list").automatic_checks == true, "check catalog did not observe live enablement")
  owner = vim.json.decode(table.concat(vim.fn.readfile(baseline), "\n")).owner
  submit("Work after automatic checks are enabled")
  await(function() return starts == 8 end, "enabled-check work did not start")
  await(function() return vim.json.decode(table.concat(vim.fn.readfile(baseline), "\n")).owner ~= owner end,
    "enabled-check work did not capture its baseline")
  vim.fn.writefile({ "edit after completion checking enabled" }, workspace .. "/source.txt")
  await(completed_after(5), "enabled-check work did not complete")
  assert(executed() == 5, "re-enabling did not restore the completion gate: "
    .. vim.inspect({ automatic_checks = request("session.state.get").automatic_checks,
      history = vim.tbl_map(function(run)
        return { trigger = run.context.trigger, state = run.state, commands = vim.tbl_map(function(command)
          return { state = command.state, started_at_ms = command.started_at_ms }
        end, run.commands) }
      end, request("session.check.list").history) }))
end, debug.traceback)
require("forge.views.harness.workspace").release(state)
if state.presentation then state.presentation.close() end
client.host_stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(root)
vim.fn.stdpath = original_stdpath
assert(success and collected, failure or "queue fixture host was not collected")
print("harness_check_queue_host: passed")
