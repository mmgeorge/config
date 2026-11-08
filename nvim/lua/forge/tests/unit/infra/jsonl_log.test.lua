-- Unit tests for `forge.infra.jsonl_log` (jsonl_log.lua). Each scenario row runs in its own
-- headless Neovim: `.forge/checks/lua-tests.nu` passes the row's name as `arg[1]`.
local scenarios = {
  { name = "jsonl_log", run = function()
vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local path = vim.fn.tempname()
local errors = {}
local write, open = vim.uv.fs_write, vim.uv.fs_open
local writer = require("forge.infra.jsonl_log").new(function() return path end,
  function(failure) errors[#errors + 1] = failure end, 32)
local function await(target)
  assert(vim.wait(1000, (target or writer).idle, 5), "log writer did not settle")
end
local ok, failure = xpcall(function()
  vim.uv.fs_write = function(descriptor, text, offset, callback)
    return write(descriptor, text:sub(1, 7), offset, callback)
  end
  writer.append({ event = "first", text = string.rep("λ🙂", 20) })
  assert(vim.fn.filereadable(path) == 0, "append performed synchronous disk I/O")
  await()
  assert(vim.json.decode(vim.fn.readfile(path)[1]).text == string.rep("λ🙂", 20))
  writer.append({ event = "second" })
  await()
  assert(vim.json.decode(vim.fn.readfile(path .. ".1")[1]).event == "first")
  assert(vim.json.decode(vim.fn.readfile(path)[1]).event == "second")
  writer.append({ event = "fills-second", text = string.rep("preserved", 20) })
  await()
  writer.append({ event = "third" })
  await()
  assert(vim.json.decode(vim.fn.readfile(path .. ".1")[1]).event == "second")
  assert(vim.json.decode(vim.fn.readfile(path)[1]).event == "third")
  vim.uv.fs_write = write
  vim.uv.fs_open = function(_, _, _, callback) vim.schedule(function() callback("EACCES: fixture") end) end
  writer.append({ event = "retained" })
  assert(vim.wait(1000, function() return #errors == 1 end, 5))
  vim.uv.fs_open = open
  writer.append({ event = "after-retry" })
  await()
  local rows = vim.fn.readfile(path)
  assert(vim.json.decode(rows[#rows - 1]).event == "retained")
  assert(vim.json.decode(rows[#rows]).event == "after-retry")
  local default_writer = require("forge.infra.jsonl_log").new(function() return path end,
    function(problem) error(problem) end)
  default_writer.append({ event = "large", text = string.rep("x", 2 * 1024 * 1024) })
  await(default_writer)
  default_writer.append({ event = "wrapped" })
  await(default_writer)
  assert(vim.json.decode(vim.fn.readfile(path)[1]).event == "wrapped")
  local preceding = vim.fn.readfile(path .. ".1")
  assert(vim.json.decode(preceding[#preceding]).event == "large", "default rotation discarded a complete record")
end, debug.traceback)
vim.uv.fs_write, vim.uv.fs_open = write, open
vim.fn.delete(path)
vim.fn.delete(path .. ".1")
assert(ok, failure)
print("jsonl_log: asynchronous ordering, partial writes, rotation, and retry passed")
  end },
}

local selected = arg[1]
for _, scenario in ipairs(scenarios) do
  if scenario.name == selected then return scenario.run() end
end
error(("unknown scenario %q"):format(tostring(selected)))
