-- Unit tests for `forge.infra.jsonl_log` (jsonl_log.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["jsonl_log"] = function()
  local path = temp.directory("log") .. "/events.jsonl"
  local errors = {}
  local write, open = vim.uv.fs_write, vim.uv.fs_open
  local writer = require("forge.infra.jsonl_log").new(function() return path end,
    function(failure) errors[#errors + 1] = failure end, 32)
  vim.uv.fs_write = function(descriptor, text, offset, callback)
    return write(descriptor, text:sub(1, 7), offset, callback)
  end
  writer.append({ event = "first", text = string.rep("λ🙂", 20) })
  eq(vim.fn.filereadable(path), 0, { fail_reason = "append performed synchronous disk I/O" })
  wait_until(writer.idle, "log writer did not settle")
  assert(vim.json.decode(vim.fn.readfile(path)[1]).text == string.rep("λ🙂", 20))
  writer.append({ event = "second" })
  wait_until(writer.idle, "log writer did not settle")
  eq(vim.json.decode(vim.fn.readfile(path .. ".1")[1]).event, "first")
  eq(vim.json.decode(vim.fn.readfile(path)[1]).event, "second")
  writer.append({ event = "fills-second", text = string.rep("preserved", 20) })
  wait_until(writer.idle, "log writer did not settle")
  writer.append({ event = "third" })
  wait_until(writer.idle, "log writer did not settle")
  eq(vim.json.decode(vim.fn.readfile(path .. ".1")[1]).event, "second")
  eq(vim.json.decode(vim.fn.readfile(path)[1]).event, "third")
  vim.uv.fs_write = write
  vim.uv.fs_open = function(_, _, _, callback) vim.schedule(function() callback("EACCES: fixture") end) end
  writer.append({ event = "retained" })
  wait_until(function() return #errors == 1 end, "log writer did not report the failed open")
  vim.uv.fs_open = open
  writer.append({ event = "after-retry" })
  wait_until(writer.idle, "log writer did not settle")
  local rows = vim.fn.readfile(path)
  eq(vim.json.decode(rows[#rows - 1]).event, "retained")
  eq(vim.json.decode(rows[#rows]).event, "after-retry")
  local default_writer = require("forge.infra.jsonl_log").new(function() return path end,
    function(problem) error(problem) end)
  default_writer.append({ event = "large", text = string.rep("x", 2 * 1024 * 1024) })
  wait_until(default_writer.idle, "default log writer did not settle")
  default_writer.append({ event = "wrapped" })
  wait_until(default_writer.idle, "default log writer did not settle")
  eq(vim.json.decode(vim.fn.readfile(path)[1]).event, "wrapped")
  local preceding = vim.fn.readfile(path .. ".1")
  eq(vim.json.decode(preceding[#preceding]).event, "large", { fail_reason = "default rotation discarded a complete record" })
end

return T
