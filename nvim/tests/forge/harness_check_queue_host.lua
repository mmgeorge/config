vim.loader.enable(false)
local root = vim.fs.normalize(vim.fn.getcwd())
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
assert(vim.system({ "git", "init", "--quiet", workspace }, { text = true }):wait(10000).code == 0)
vim.fn.writefile({ "before" }, workspace .. "/source.txt")
vim.fn.writefile({ vim.json.encode({ checks = { version = 2, definitions = {
  { id = "verify", name = "Queue verification", category = "test", area = "Fixture", enabled = true,
    trigger = { kind = "files_changed", include = { "source.txt" } }, all = { command = "print queue-verified" } },
} } }) }, workspace .. "/.forge.json")
local binary = data .. "/forge-host" .. (vim.fn.has("win32") == 1 and ".exe" or "")
assert(vim.uv.fs_copyfile(require("forge.builder").binary_path(), binary))
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return (kind == "data" or kind == "config") and data or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = binary }) end)
  return function() end
end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local starts, failures = 0, {}
require("forge.infra.notifications").error = function(message) failures[#failures + 1] = message end
client.subscribe(function(event, payload)
  if event == "backend_event" and payload.kind == "execution_state" then starts = starts + 1 end
end)
local function await(predicate, message)
  assert(vim.wait(15000, function() return #failures > 0 or predicate() end, 10), message)
  assert(#failures == 0, table.concat(failures, "\n"))
end
local function request(method, params)
  local done, result, failure
  client.request(method, params or {}, function(value, problem) result, failure, done = value, problem, true end)
  await(function() return done end, method .. " did not complete")
  assert(not failure, tostring(failure))
  return result
end
local function submit(text, queued)
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { text })
  if queued then controller.queue_submit() else controller.submit() end
end
local function completed()
  return not state.busy and not state.state_sync_pending and not state.task_operation
    and not state.checks_running and not state.checks_deferred and #state.queue == 0
end
local function executed()
  local count = 0
  for _, run in ipairs(request("checks.list").history) do
    for _, command in ipairs(run.commands) do
      if command.state == "passed" and type(command.started_at_ms) == "number" then count = count + 1 end
    end
  end
  return count
end
local success, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ harness = { backend = "mock", backends = {
    mock = { command = { "session-blocking" }, selectable = true },
  } } })
  require("forge.views.harness").open()
  await(function() return state.session ~= nil end, "startup timeout")
  request("session.configure", { shell = "nushell" })
  assert(request("state.get").check_timing == "after_queue")

  submit("First queued change")
  await(function() return starts == 1 end, "first item did not start")
  local live_setting
  controller.configure({ check_timing = "after_queue" }, false, function(value) live_setting = value end)
  await(function() return live_setting end, "active timing configuration failed")
  assert(starts == 1 and state.busy, "changing timing restarted the active provider")
  local baseline = data .. "/forge/harness/" .. state.session.id .. "/checks/change-baseline.json"
  await(function() return vim.fn.filereadable(baseline) == 1 end, "change baseline missing")
  local owner = vim.json.decode(table.concat(vim.fn.readfile(baseline), "\n")).owner
  submit("Second queued item", true)
  vim.fn.writefile({ "first batch edit" }, workspace .. "/source.txt")
  await(function() return starts == 2 end, "queue did not advance")
  assert(executed() == 0, "checks ran between queued work under the default")
  await(completed, "queue did not finish its completion checks")
  assert(executed() == 1, "final gate lost the earlier queued edit")
  assert(vim.json.decode(table.concat(vim.fn.readfile(baseline), "\n")).owner == owner,
    "the last item replaced the first change baseline")

  local applied
  controller.configure({ check_timing = "between_work" }, false, function(value) applied = value end)
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
  assert(executed() == 2, "between-work checks did not finish before the next item: " .. vim.inspect(request("checks.list").history))
  await(completed, "intermediate batch did not finish")

  applied = nil
  controller.configure({ check_timing = "after_queue" }, false, function(value) applied = value end)
  await(function() return applied end, "default timing could not be restored")
  owner = vim.json.decode(table.concat(vim.fn.readfile(baseline), "\n")).owner
  submit("Change before deleting queued work")
  await(function() return starts == 5 end, "deletion batch did not start")
  await(function() return vim.json.decode(table.concat(vim.fn.readfile(baseline), "\n")).owner ~= owner end,
    "deletion batch did not capture its baseline")
  submit("Remove this pending item", true)
  controller.edit_queued(-1)
  vim.fn.writefile({ "third batch edit" }, workspace .. "/source.txt")
  await(function() return not state.busy and state.checks_deferred end, "editing did not retain the deferred gate")
  assert(executed() == 2, "checks ran while queued work remained")
  submit("")
  await(completed, "removing the final queued item did not run checks")
  assert(starts == 5 and executed() == 3, "queue removal lost validation or started deleted work: "
    .. vim.inspect({ starts = starts, history = request("checks.list").history }))
end, debug.traceback)
require("forge.views.harness.workspace").release(state)
if state.presentation then state.presentation.close() end
client.stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(root)
vim.fn.stdpath = original_stdpath
assert(success and collected, failure or "queue fixture host was not collected")
print("harness_check_queue_host: passed")
