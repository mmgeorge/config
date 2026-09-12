vim.loader.enable(false)
local root = vim.fs.normalize(vim.fn.getcwd())
local fixture = vim.fn.tempname()
local data_directory = vim.fn.tempname()
assert(vim.fn.mkdir(fixture, "p") == 1)
local executable_name = "forge" .. (vim.fn.has("win32") == 1 and ".exe" or "")
local build = require("forge.builder").binary_path()
assert(vim.fn.executable(build) == 1, "build the Forge host first")
assert(vim.fn.mkdir(data_directory, "p") == 1)
local executable = data_directory .. "/" .. executable_name
assert(vim.uv.fs_copyfile(build, executable))
local original_stdpath, original_notify = vim.fn.stdpath, vim.notify
vim.fn.stdpath = function(kind) return kind == "data" and data_directory or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = executable }) end)
  return function() end
end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local status = require("forge.status")
local state, notices
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
    and next(client._client.pending) == nil
end
local gutter = require("forge.gutter")
local clipboard
vim.g.clipboard = { name = "Forge fixture", copy = {
  ["+"] = function(lines, kind) clipboard = { lines = lines, kind = kind } end,
  ["*"] = function() end,
}, paste = { ["+"] = function() return { {}, "V" } end, ["*"] = function() return { {}, "V" } end } }
local function mapping(key, mode)
  local value = vim.fn.maparg(key, mode or "n", false, true)
  return value.buffer == 1 and value or nil
end
local function moved(row)
  vim.api.nvim_win_set_cursor(0, { row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = state.replica.buffer })
  return vim.fn.getcurpos()
end
local ok, failure = xpcall(function()
  git({ "init", "--quiet" })
  git({ "config", "user.name", "Forge Fixture" })
  git({ "config", "user.email", "forge@example.test" })
  vim.fn.writefile({ "function foo()", "", "  return 1", "end" }, fixture .. "/sample.lua")
  git({ "add", "sample.lua" })
  git({ "commit", "--quiet", "-m", "Empty row fixture" })
  vim.fn.writefile({ "function foo()", "  return 2", "end" }, fixture .. "/sample.lua")
  vim.fn.chdir(fixture)
  state = require("forge.views.commands").open()
  assert(vim.wait(10000, settled, 10), "native Status did not settle: " .. table.concat(notices, "\n"))
  local buffer = state.replica.buffer
  assert(vim.bo[buffer].filetype == "ForgeStatus")
  local file_row
  for row, line in ipairs(vim.api.nvim_buf_get_lines(buffer, 0, -1, true)) do
    if line:match("^Modified sample%.lua ") then file_row = row break end
  end
  assert(file_row, "native Status omitted file header: " .. table.concat(vim.api.nvim_buf_get_lines(buffer, 0, -1, true), "\n"))
  moved(file_row)
  if vim.fn.foldclosed(file_row) ~= -1 then mapping("<Tab>").callback() end
  assert(vim.wait(10000, settled, 10), "native diff demand did not settle")
  local empty_row, code_row
  for row, line in ipairs(vim.api.nvim_buf_get_lines(buffer, 0, -1, true)) do
    if gutter.bounds(state.replica, row) then
      if line == "" then empty_row = row end
      if line == "  return 2" then code_row = row end
    end
  end
  assert(empty_row and code_row, "native diff omitted empty or changed source row")
  local tick = vim.api.nvim_buf_get_changedtick(buffer)
  local empty_bounds = gutter.bounds(state.replica, empty_row)
  local position = moved(empty_row)
  assert(position[2] == empty_row and position[3] == 1 and position[4] == empty_bounds.width,
    "empty row cursor lost gutter offset: " .. vim.inspect(position))
  assert(vim.api.nvim_buf_get_lines(buffer, empty_row - 1, empty_row, true)[1] == "", "empty row gained padding")
  local code = vim.api.nvim_buf_get_lines(buffer, code_row - 1, code_row, true)[1]
  local bounds = gutter.bounds(state.replica, code_row)
  position = moved(code_row)
  assert(position[2] == code_row and position[3] == bounds.column + 1 and position[4] == bounds.width,
    "code row cursor lost gutter offset: " .. vim.inspect(position))
  assert(not mapping("<Space>l", "x"), "clipboard mapping installed before W")
  mapping("W").callback()
  assert(mapping("<Space>l", "x"), "W omitted temporary clipboard mapping")
  position = vim.fn.getcurpos()
  assert(vim.api.nvim_get_mode().mode == "V" and position[3] == 1 and position[4] == 0,
    "W did not select the actual gutter edge")
  assert(state.replica.gutter_selection.first == code_row and state.replica.gutter_selection.last == code_row,
    "gutter highlight range differs from native selection")
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buffer })
  position = vim.fn.getcurpos()
  assert(position[3] == 1 and position[4] == 0 and state.replica.gutter_selection,
    "cursor normalization disturbed active gutter selection")
  local prefix = {}
  for _, entry in ipairs(bounds.gutter) do
    for _, chunk in ipairs(entry.chunk) do prefix[#prefix + 1] = chunk.text end
  end
  mapping("<Space>l", "x").callback()
  assert(clipboard and clipboard.kind == "V"
    and vim.deep_equal(clipboard.lines, { table.concat(prefix) .. code, "" }),
    "clipboard differs from visible source and gutter: " .. vim.inspect(clipboard))
  assert(vim.api.nvim_get_mode().mode == "n" and not mapping("<Space>l", "x")
    and state.replica.gutter_selection == nil, "clipboard completion retained selection ownership")
  mapping("W").callback()
  assert(mapping("<Space>l", "x"), "second W omitted clipboard mapping")
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Esc>", true, false, true), "nx", false)
  vim.api.nvim_exec_autocmds("ModeChanged", {})
  assert(not mapping("<Space>l", "x") and state.replica.gutter_selection == nil,
    "Escape retained clipboard or gutter highlight ownership")
  position = vim.fn.getcurpos()
  assert(position[3] == bounds.column + 1 and position[4] == bounds.width,
    "normal cursor restriction did not resume")
  vim.fn.setpos(".", { 0, code_row, #code + 20, 20 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buffer })
  position = vim.fn.getcurpos()
  assert(position[2] == code_row and position[3] == #code and position[4] == 0,
    "EOL clamp retained virtual text: " .. vim.inspect(position))
  assert(vim.api.nvim_buf_get_changedtick(buffer) == tick, "cursor or selection changed source bytes")
  assert(#notices == 0, table.concat(notices, "\n"))
end, debug.traceback)
if state then status.close(state) end
client.stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.fn.chdir(root)
vim.fn.stdpath, vim.notify = original_stdpath, original_notify
vim.fn.delete(fixture, "rf")
vim.fn.delete(data_directory, "rf")
if not ok or not collected then vim.api.nvim_err_writeln(failure or "host was not collected") vim.cmd("cquit 1") end
print("empty_diff_cursor OK: native source, cursor, gutter selection, clipboard and collected close")
vim.cmd("qa!")
