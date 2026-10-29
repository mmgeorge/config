vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
vim.loader.enable(false)
local origin = vim.fn.getcwd()
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
assert(vim.fn.mkdir(workspace, "p") == 1 and vim.fn.mkdir(data, "p") == 1)
local executable = require("forge.builder").binary_path()
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return (kind == "data" or kind == "config") and data or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback) callback({ ok = true, path = executable }) return function() end end }
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local current_session_id
local function await(predicate, message)
  assert(vim.wait(15000, predicate, 10), message)
end
local function write(name, content) vim.fn.writefile({ content }, workspace .. "/" .. name) end
local function read(name) return table.concat(vim.fn.readfile(workspace .. "/" .. name, "b"), "\n") end
local function git(arguments)
  local command = { "git", "-C", workspace }
  vim.list_extend(command, arguments)
  local result = vim.system(command, { text = true }):wait(10000)
  assert(result.code == 0, result.stderr)
end
local function request(method, params, permit_failure)
  local done, result, failure
  client.request({ method = method, params = params or {}, session_id = require("forge.event_contract").method_policy(method).session_context ~= "none" and current_session_id or nil }, function(value, problem) done, result, failure = true, value, problem end)
  await(function() return done end, method .. " did not settle")
  assert(permit_failure or not failure, failure)
  return result, failure
end
local function open(session_id)
  local snapshot, failure
  client.session_initialize(function(value, problem) snapshot, failure = value, problem end, { session_id = session_id })
  await(function() return snapshot or failure end, "host initialization")
  assert(not failure, failure)
  current_session_id = snapshot.session.id
  return snapshot
end
local function stop()
  client.host_stop()
  await(function() return client._client.process == nil end, "host collection")
end
local function exchange(edits)
  vim.fn.delete(workspace .. "/mock-provider-change.txt")
  local done, failure
  client.request({ method = "session.prompt.submit", params = { text = "Checkpoint integration fixture" }, session_id = current_session_id }, function(_, problem) done, failure = true, problem end)
  await(function() return vim.fn.filereadable(workspace .. "/mock-provider-change.txt") == 1 end, "provider did not start")
  for name, content in pairs(edits) do write(name, content) end
  request("session.turn.cancel")
  await(function() return done end, "cancelled exchange did not finalize")
  assert(failure and failure:find("cancel", 1, true), failure)
end
local success, failure = xpcall(function()
  git({ "init", "-q" })
  git({ "config", "user.name", "Checkpoint fixture" })
  git({ "config", "user.email", "checkpoint@example.invalid" })
  git({ "config", "core.autocrlf", "false" })
  for _, name in ipairs({ "a", "b", "c" }) do write(name, name) end
  git({ "add", "." })
  git({ "commit", "-qm", "fixture" })
  write("a", "initial staged a")
  git({ "add", "a" })
  write("b", "initial unstaged b")
  local initial_index = vim.fn.readblob(workspace .. "/.git/index")
  vim.fn.chdir(workspace)
  require("forge").setup({ harness = { backend = "mock", backends = { mock = { command = { "writing-blocking" } } } } })
  local session_id = open().session.id
  exchange({ a = "first a", b = "first b" })
  exchange({ a = "second a", c = "second c" })
  local history = request("session.exchange.list")
  assert(#history == 2 and history[1].checkpoint_before and history[2].checkpoint_after)
  stop()
  open(session_id)
  write("a", "later user a")
  local preview = request("session.exchange.rollback.prepare", { exchange_id = history[2].id })
  assert(preview.warning_count > 0, "later edit had no warning")
  assert(type(request("session.exchange.rollback.preview", { preview_id = preview.preview_id, offset = 0 })) == "table")
  write("a", "edit after confirmation")
  local _, stale = request("session.exchange.rollback.apply", { preview_id = preview.preview_id }, true)
  assert(stale and stale:find("preview is stale", 1, true), stale)
  assert(read("c") == "second c\n", "stale preview partially wrote files")
  preview = request("session.exchange.rollback.prepare", { exchange_id = history[2].id })
  request("session.exchange.rollback.apply", { preview_id = preview.preview_id })
  assert(read("a") == "first a\n" and read("b") == "first b\n" and read("c") == "c\n")
  assert(vim.deep_equal(initial_index, vim.fn.readblob(workspace .. "/.git/index")), "restore changed staging")
  stop()
  open(session_id)
  assert(request("session.exchange.recovery").pending == false, "completed restore was lost after restart")
  local recovery = request("session.exchange.recovery", { action = "undo" })
  request("session.exchange.rollback.apply", { preview_id = recovery.preview_id })
  assert(read("a") == "edit after confirmation\n" and read("c") == "second c\n", "Undo last restore lost recovery bytes")
  preview = request("session.exchange.rollback.prepare", { exchange_id = history[1].id })
  request("session.exchange.rollback.apply", { preview_id = preview.preview_id })
  assert(read("a") == "initial staged a\n", "checkpoint zero a: " .. vim.inspect(read("a")))
  assert(read("b") == "initial unstaged b\n", "checkpoint zero b: " .. vim.inspect(read("b")))
  assert(read("c") == "second c\n", "rollback overwrote a file outside the remaining exchange's changes")
  assert(vim.deep_equal(initial_index, vim.fn.readblob(workspace .. "/.git/index")), "checkpoint zero lost staging")
end, debug.traceback)
stop()
vim.fn.chdir(origin)
vim.fn.stdpath = original_stdpath
vim.fn.delete(workspace, "rf")
vim.fn.delete(data, "rf")
assert(success, failure)
print("harness_checkpoint_host: two exchanges, restart, stale confirmation, recovery and staging passed")
