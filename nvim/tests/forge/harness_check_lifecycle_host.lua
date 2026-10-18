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
local function preview()
  if not vim.g.forge_check_preview then return end
  vim.g.forge_check_preview_continue = false
  vim.keymap.set({ "n", "i" }, "<F6>", function() vim.g.forge_check_preview_continue = true end)
  vim.cmd("redraw")
  assert(vim.wait(25000, function() return vim.g.forge_check_preview_continue end, 10), "preview timed out")
end
local succeeded, failure = xpcall(function()
  local definitions = {
    { id = "slow", name = "Streaming check", category = "test", area = "Fixture", enabled = true,
      trigger = { kind = "manual" }, all = { command = "print 'Check output is streaming'; sleep 30sec" } },
    { id = "failure", name = "One failing check", category = "test", area = "Fixture", enabled = true,
      trigger = { kind = "manual" }, all = { command = "exit 7" } },
  }
  for index = 1, 31 do
    definitions[#definitions + 1] = { id = "quick-" .. index, name = "Quick check " .. index,
      category = "test", area = "Fixture", enabled = true,
      trigger = { kind = "manual" }, all = { command = "print done" } }
  end
  vim.fn.writefile({ vim.json.encode({ checks = { version = 2, definitions = definitions } }) }, workspace .. "/.forge.json")
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = false,
    harness = { backend = "mock", backends = { mock = { command = { "mock" }, selectable = true } } } })
  require("forge.views.harness").open()
  assert(vim.wait(10000, function() return state.session ~= nil end, 10), "startup timed out")
  request("session.configure", { shell = "nushell" })
  local id = request("checks.run", { ids = { "slow", "quick-1", "quick-2", "quick-3" } }).run_id
  assert(vim.wait(5000, function()
    return state.checks_running and state.active_check_run == id
      and rows():find("Check output is streaming", 1, true)
  end, 10), "running check did not reach the visible transcript")
  assert(rows():find("Checking", 1, true) and rows():find("User requested", 1, true), rows())
  assert(rows():find("0/4 finished", 1, true) and not rows():find("/33 finished", 1, true), rows())
  preview()
  request("checks.cancel", { run_id = id })
  assert(vim.wait(3000, function()
    return not state.checks_running and not rows():find("Checks running", 1, true)
  end, 10), "interrupted check retained live activity")
  local cancelled = request("checks.status", { run_id = id, wait_for_completion = true, wait_ms = 3000 })
  assert(cancelled.state == "interrupted" and type(cancelled.completed_at_ms) == "number", vim.inspect(cancelled))
  assert(rows():find("Checks interrupted", 1, true), rows())
  preview()
  local failed = request("checks.run", { ids = { "failure" } }).run_id
  assert(request("checks.status", { run_id = failed, wait_for_completion = true, wait_ms = 3000 }).state == "failed")
  request("state.get")
  assert(vim.wait(3000, function() return rows():find("1 failed", 1, true) ~= nil end, 10), rows())
end, debug.traceback)
client.stop()
vim.wait(3000, function() return client._client.process == nil end, 10)
vim.fn.stdpath = original_stdpath
if not succeeded then error(failure) end
print("harness_check_lifecycle_host passed")
vim.cmd("qa!")
