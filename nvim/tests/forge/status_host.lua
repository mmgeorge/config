vim.loader.enable(false)
local root = vim.fs.normalize(vim.fn.getcwd())
local fixture = vim.fn.tempname()
local data_directory = vim.fn.tempname()
assert(vim.fn.mkdir(fixture, "p") == 1)
local build = require("forge.builder").binary_path()
assert(vim.fn.executable(build) == 1, "build the Forge host first")
assert(vim.fn.mkdir(data_directory, "p") == 1)
local executable = build
local phase_file = vim.fn.tempname() .. ".status-host-phase.log"
local original_stdpath, original_notify = vim.fn.stdpath, vim.notify
vim.fn.stdpath = function(kind) return kind == "data" and data_directory or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  callback({ ok = true, path = executable })
  return function() end
end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local status = require("forge.status")
local state, notices
local phase = "setup"
notices = {}
vim.notify = function(message, level)
  if level == vim.log.levels.ERROR then notices[#notices + 1] = tostring(message) end
end
local function git(arguments)
  local command = { "git", "-C", fixture }
  vim.list_extend(command, arguments)
  local result = vim.system(command, { text = true, timeout = 10000 }):wait()
  assert(result.code == 0, result.stderr)
  return result.stdout
end
local function settled()
  return state.replica.status == "Applied" and not state.pending and not state.scheduled
    and #(state.replica.inventory.pending or {}) == 0
    and next(client._client.pending) == nil
end
local function pending_diagnostic()
  local pending = {}
  for id, request in pairs(client._client.pending) do pending[id] = request.method end
  return vim.inspect({
    phase = phase,
    replica = state and state.replica.status or nil,
    status_pending = state and state.pending or nil,
    status_scheduled = state and state.scheduled or nil,
    status_request_active = state and state.request_active or nil,
    queued_status_requests = state and #(state.request_queue or {}) or nil,
    client_pending = pending,
    client_stderr = client._client.stderr,
    notices = notices,
  })
end
---@param name string
local function record_phase(name)
  phase = name
  vim.fn.writefile({ string.format("%d %s", vim.uv.hrtime(), name) }, phase_file, "a")
end
---@return string
local function failure_diagnostic()
  return pending_diagnostic() .. "\nphase file: " .. phase_file
end
local function await_phase(name, timeout, predicate)
  record_phase(name)
  assert(vim.wait(timeout, predicate, 200), "status host phase timed out: " .. failure_diagnostic())
end
local ok, failure = xpcall(function()
  record_phase("setup repository")
  git({ "init", "--quiet" })
  git({ "config", "user.name", "Forge Fixture" })
  git({ "config", "user.email", "forge@example.test" })
  vim.fn.writefile({ "local value = 1", "return value" }, fixture .. "/tracked file.lua")
  git({ "add", "tracked file.lua" })
  git({ "commit", "--quiet", "-m", "Status fixture" })
  vim.fn.writefile({ "local value = 2", "return value" }, fixture .. "/tracked file.lua")
  vim.fn.chdir(fixture)
  record_phase("open native status")
  require("forge").setup()
  state = require("forge.views.commands").open()
  await_phase("initial document application", 15000, settled)
  assert(#notices == 0, table.concat(notices, "\n"))
  assert(vim.b[state.replica.buffer].forge_native_document and not client._client.harness_ready)
  assert(vim.wo.foldmethod == "expr", "public facade replaced native folds")
  local closed = false
  record_phase("close native status")
  status.close(state, function() closed = true end)
  await_phase("close native status", 6000, function() return closed end)
  state = nil
end, debug.traceback)
if state then status.close(state) end
client.stop()
record_phase("collect native host")
local collected = vim.wait(6000, function() return client._client.process == nil end, 200)
record_phase("cleanup")
vim.fn.chdir(root)
vim.fn.stdpath, vim.notify = original_stdpath, original_notify
vim.fn.delete(fixture, "rf")
vim.fn.delete(data_directory, "rf")
if not ok or not collected then
  vim.api.nvim_err_writeln((failure or "host was not collected") .. "\nphase file: " .. phase_file)
  vim.cmd("cquit 1")
end
vim.fn.delete(phase_file)
print("status_host OK: public native Status lifecycle collected")
vim.cmd("qa!")
