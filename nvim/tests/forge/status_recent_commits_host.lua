vim.loader.enable(false)
local root = vim.fn.getcwd()
local workspace, data_directory = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data_directory, "p")
local executable = vim.g.forge_recent_executable or require("forge.builder").binary_path()
assert(vim.fn.executable(executable) == 1, "build the Forge host first")
local original_stdpath, original_notify = vim.fn.stdpath, vim.notify
vim.fn.stdpath = function(kind) return kind == "data" and data_directory or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  callback({ ok = true, path = executable })
  return function() end
end }
package.loaded["forge.views.status.status_context"] = { attach = function()
  return { refresh = function() end, close = function() end }
end, producer_handlers = function() return {} end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local status, history = require("forge.status"), require("forge.status_history")
local state, notices = nil, {}
local request_host = client.request_host
local demand_count = 0
client.request_host = function(method, params, callback, progress)
  if method == "status" and params.operation == "demand" then
    demand_count = demand_count + 1
    if demand_count > 64 then
      callback(nil, "Repeated demand: " .. vim.inspect(params))
      return
    end
  end
  return request_host(method, params, callback, progress)
end
vim.notify = function(message, level)
  if level == vim.log.levels.ERROR then notices[#notices + 1] = tostring(message) end
end
local function git(arguments)
  local command = { "git", "-C", workspace }
  vim.list_extend(command, arguments)
  local result = vim.system(command, { text = true, timeout = 10000 }):wait()
  assert(result.code == 0, result.stderr)
  return vim.trim(result.stdout)
end
local function settle()
  assert(vim.wait(10000, function()
    return #notices > 0 or state.ready and not state.pending and not state.scheduled
      and not state.request_active and next(client._client.pending) == nil
  end, 10), "native commit expansion timed out")
  assert(#notices == 0, table.concat(notices, "\n"))
end
local function row(id) return select(2, state.replica.sequence:position(id)) + 1 end
local function toggle(id)
  vim.api.nvim_win_set_cursor(0, { row(id) + (id == "status:context:recent-title" and 1 or 0), 0 })
  vim.fn.maparg("<Tab>", "n", false, true).callback()
  settle()
end
local function open_commit(oid)
  toggle("status:context:recent:" .. oid)
  return assert(state.replica.commit[oid], "commit was not loaded")
end
local latest
local ok, failure = xpcall(function()
  git({ "init", "--quiet" })
  git({ "config", "user.name", "Forge Fixture" })
  git({ "config", "user.email", "forge@example.test" })
  vim.fn.writefile({ "local value = 1", "return value" }, workspace .. "/first.lua")
  vim.fn.writefile({ "initial" }, workspace .. "/second.txt")
  git({ "add", "." })
  git({ "commit", "--quiet", "-m", "Initial files" })
  local initial = git({ "rev-parse", "HEAD" })
  vim.fn.writefile({ "local value = 2", "return value" }, workspace .. "/first.lua")
  git({ "mv", "second.txt", "renamed.txt" })
  git({ "add", "." })
  git({ "commit", "--quiet", "-m", "Update value and rename file" })
  latest = git({ "rev-parse", "HEAD" })
  git({ "commit", "--quiet", "--allow-empty", "-m", "Empty commit" })
  local empty = git({ "rev-parse", "HEAD" })
  vim.fn.writefile({ "local value = 3", "return value" }, workspace .. "/first.lua")
  state = status.open({ workspace = workspace })
  settle()
  assert(next(state.replica.commit) == nil)
  toggle("status:context:recent-title")
  local owner = open_commit(latest)
  assert(#owner.snapshot.file == 2, "commit inventory differs from Git")
  local selected
  for _, file in ipairs(owner.snapshot.file) do
    local key = history.file_key(owner, file.id)
    assert(state.replica.file[key].body == nil, "collapsed file loaded eagerly")
    if file.path == "first.lua" then selected = key end
  end
  assert(selected)
  toggle("file:" .. selected)
  local body = assert(state.replica.file[selected].body)
  local text = {}
  for index = 0, body.sequence:count() - 1 do vim.list_extend(text, body.sequence:at(index).entry.text) end
  local rendered = table.concat(text, "\n")
  assert(rendered:find("value = 1", 1, true) and rendered:find("value = 2", 1, true), rendered)
  assert(not rendered:find("value = 3", 1, true), "commit diff read the working tree")
  assert(next(body.fold.record), "shared diff hunk folds missing")
  vim.api.nvim_win_set_cursor(0, { row("file:" .. selected), 0 })
  status.navigate(state, true)
  settle()
  local cursor = vim.api.nvim_win_get_cursor(0)
  assert(cursor[1] > row("file:" .. selected), "native hunk navigation did not enter commit diff")
  status.refresh(state)
  settle()
  assert(state.replica.file[selected].body, "refresh lost historical diff")
  assert(vim.fn.foldclosed(row("file:" .. selected)) == -1, "refresh closed historical file")
  local root_owner = open_commit(initial)
  assert(#root_owner.snapshot.file == 2, "root commit omitted added files")
  local root_key = history.file_key(root_owner, root_owner.snapshot.file[1].id)
  toggle("file:" .. root_key)
  assert(state.replica.file[root_key].body and state.replica.file[selected].body)
  local empty_owner = open_commit(empty)
  assert(#empty_owner.snapshot.file == 0)
  assert(state.replica.block["status:context:recent:" .. empty .. ":placeholder"].text[1] == "No changed files")
  vim.fn.maparg("zR", "n", false, true).callback()
  settle()
  for _, model in pairs(state.replica.file) do assert(model.body, "open-all left a file unloaded") end
  assert(#notices == 0)
end, debug.traceback)
local function cleanup()
  if state then status.close(state) end
  vim.wait(3000, function() return not state or not state.request_active end, 10)
  client.stop()
  vim.wait(3000, function() return client._client.process == nil end, 10)
  vim.fn.stdpath, vim.notify = original_stdpath, original_notify
  client.request_host = request_host
  vim.fn.chdir(root)
  vim.fn.delete(workspace, "rf")
  vim.fn.delete(data_directory, "rf")
end
if ok and vim.g.forge_recent_manual then
  vim.cmd("normal! zM")
  toggle("status:context:recent-title")
  vim.api.nvim_win_set_cursor(0, { row("status:context:recent:" .. latest), 0 })
  vim.api.nvim_create_autocmd("VimLeavePre", { once = true, callback = cleanup })
  return
end
cleanup()
assert(ok, failure)
assert(#notices == 0, table.concat(notices, "\n"))
print("native recent commits: passed (lazy files, historical diff, root, empty, navigation, refresh, cleanup)")
