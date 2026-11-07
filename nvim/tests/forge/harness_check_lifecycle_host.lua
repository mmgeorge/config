vim.loader.enable(false)
local check_tool = dofile(vim.fn.getcwd() .. "/nvim/tests/forge/fixtures/check_tool.lua")
vim.opt.runtimepath:prepend(vim.fs.normalize(vim.fn.getcwd()) .. "/nvim")
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
local binary = data .. "/forge-host" .. (vim.fn.has("win32") == 1 and ".exe" or "")
-- Launch the shared pinned executable lease rather than a freshly scanned per-run copy.
binary = dofile(vim.fs.dirname(debug.getinfo(1, "S").source:sub(2)) .. "/support/host_executable.lua")()
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
  local done, result, failure
  client.request({ method = method, params = params or {}, session_id = require("forge.event_contract").method_policy(method).session_context ~= "none" and assert(require("forge.session").harness.session).id or nil }, function(value, problem) result, failure, done = value, problem, true end)
  assert(vim.wait(10000, function() return done end, 10), method .. " timed out")
  assert(not failure, method .. ": " .. tostring(failure))
  return result
end
local function rows()
  return table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
end
-- Live check progress belongs to the status split; completed results stay in the transcript.
local function status_rows()
  local status = state.presentation and state.presentation.status_view
  return table.concat(status and status.content and status.content.lines or {}, "\n")
end
local function preview()
  if not vim.g.forge_check_preview then return end
  vim.g.forge_check_preview_continue = false
  vim.keymap.set({ "n", "i" }, "<F6>", function() vim.g.forge_check_preview_continue = true end)
  vim.cmd("redraw")
  assert(vim.wait(25000, function() return vim.g.forge_check_preview_continue end, 10), "preview timed out")
end
local succeeded, failure = xpcall(function()
  local checks = {
    slow = { trigger = {}, action = { label = "Streaming check", category = "test", command = "print 'Check output is streaming'; sleep 30sec" } },
    failure = { trigger = {}, action = { label = "One failing check", category = "test", command = "exit 7" } },
  }
  for index = 1, 31 do
    checks["quick-" .. index] = { trigger = {}, action = { label = "Quick check " .. index, category = "test", command = "print done" } }
  end
  vim.fn.writefile({ vim.json.encode({ checks = checks }) }, workspace .. "/.forge.json")
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = false,
    harness = { provider = "mock", providers = { mock = { command = { "mock" }, selectable = true } } } })
  require("forge.views.harness").open()
  assert(vim.wait(10000, function() return state.session ~= nil end, 10), "startup timed out")
  request("session.configure", { shell = "nushell" })
  local id = check_tool.start(request, { check = { "slow", "quick-1", "quick-2", "quick-3" } }).run_id
  assert(vim.wait(5000, function()
    return state.checks_running and state.active_check_run == id
      and status_rows():find("Check output is streaming", 1, true)
  end, 10), "running check did not reach the visible status: " .. vim.inspect({ run_id = id,
    checks_running = state.checks_running, active_check_run = state.active_check_run, status = status_rows(), transcript = rows() }))
  assert(status_rows():find("Checking", 1, true) and not status_rows():find("User requested", 1, true), status_rows())
  assert(status_rows():find("3/4 finished", 1, true) and not status_rows():find("/33 finished", 1, true), status_rows())
  preview()
  request("session.turn.cancel", {})
  assert(vim.wait(3000, function()
    return not state.checks_running and not status_rows():find("Checks running", 1, true)
  end, 10), "interrupted check retained live activity")
  local cancelled = request("session.check.status", { run_id = id, wait_for_completion = true, wait_ms = 3000 })
  assert(cancelled.state == "interrupted" and type(cancelled.completed_at_ms) == "number", vim.inspect(cancelled))
  -- check_run makes the run exchange-owned, so its result appears in the exchange's aggregate
  -- check heading, which reports the interrupted command in its outcome summary.
  assert(rows():find("Checks finished · 3/4 passed · 1 interrupted", 1, true), rows())
  preview()
  local failed = check_tool.start(request, { check = { "failure" } }).run_id
  assert(request("session.check.status", { run_id = failed, wait_for_completion = true, wait_ms = 3000 }).state == "failed")
  request("session.state.get")
  assert(vim.wait(3000, function() return rows():find("1 failed", 1, true) ~= nil end, 10), rows())
end, debug.traceback)
client.host_stop()
vim.wait(3000, function() return client._client.process == nil end, 10)
vim.fn.stdpath = original_stdpath
if not succeeded then error(failure) end
print("harness_check_lifecycle_host passed")
vim.cmd("qa!")
