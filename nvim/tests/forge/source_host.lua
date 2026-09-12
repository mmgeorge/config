vim.loader.enable(false)

local root = vim.fs.normalize(vim.fn.getcwd())
local fixture = vim.fn.tempname()
local data_directory = vim.fn.tempname()
assert(vim.fn.mkdir(fixture, "p") == 1)
local executable_name = "forge" .. (vim.fn.has("win32") == 1 and ".exe" or "")
local build = require("forge.builder").binary_path()
assert(vim.fn.executable(build) == 1, "build the selected Forge host before this integration test")
assert(vim.fn.mkdir(data_directory, "p") == 1)
local executable = data_directory .. "/" .. executable_name
assert(vim.uv.fs_copyfile(build, executable))
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind)
  if kind == "data" then return data_directory end
  return original_stdpath(kind)
end
package.loaded["forge.builder"] = {
  ensure = function(callback)
    vim.schedule(function() callback({ ok = true, path = executable }) end)
    return function() end
  end,
}
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local source = require("forge.source_document")
local notices, state = {}, nil
local function git(arguments)
  local command = { "git", "-C", fixture }
  vim.list_extend(command, arguments)
  local result = vim.system(command, { text = true, timeout = 10000 }):wait()
  assert(result.code == 0, result.stderr)
end
local ok, failure = xpcall(function()
  git({ "init", "--quiet" })
  git({ "config", "user.name", "Forge Fixture" })
  git({ "config", "user.email", "forge@example.test" })
  local lines = {}
  for index = 1, 700 do lines[index] = ("local value_%d = %d"):format(index, index) end
  assert(vim.fn.writefile(lines, fixture .. "/source file.lua") == 0)
  git({ "add", "source file.lua" })
  git({ "commit", "--quiet", "-m", "Source fixture" })
  local origin = vim.api.nvim_get_current_buf()
  state = source.open({ workspace = fixture, path = "source file.lua", revision = "HEAD", line = 700,
    is_current = function() return vim.api.nvim_get_current_buf() == origin end,
    on_error = function(message) notices[#notices + 1] = tostring(message) end })
  assert(vim.wait(10000, function()
    return #notices > 0 or (state.replica.status == "Applied" and state.replica.row_count == 700 and not state.more)
  end, 10), "source delivery did not finish")
  assert(#notices == 0, table.concat(notices, "\n"))
  assert(vim.api.nvim_get_current_buf() == state.replica.buffer, "source did not bind its initiating window")
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, true), lines), "native source rows changed")
  assert(vim.api.nvim_win_get_cursor(0)[1] == 700, "requested source row was not reached across batches")
  assert(vim.bo[state.replica.buffer].readonly and not vim.bo[state.replica.buffer].modifiable)
  assert(vim.b[state.replica.buffer].forge_native_document, "native source ownership missing")
  assert(not client._client.harness_ready, "source open initialized Harness")
  local closed = false
  source.close(state, function() closed = true end)
  assert(vim.wait(3000, function() return closed end, 10), "native source close did not settle")
  state = nil
end, debug.traceback)
if state then source.close(state) end
client.stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.fn.stdpath = original_stdpath
vim.fn.delete(fixture, "rf")
vim.fn.delete(data_directory, "rf")
if not ok or not collected then
  vim.api.nvim_err_writeln(failure or "source host process was not collected")
  vim.cmd("cquit 1")
end
print("source_host OK: 700 exact rows, multiple native batches, no Harness session")
vim.cmd("qa!")
