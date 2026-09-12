vim.loader.enable(false)
local log = require("forge.startup_log")
local path = vim.fn.tempname()
log.path = function() return path end
local deferred = {}
local original_defer = vim.defer_fn
vim.defer_fn = function(callback) deferred[#deferred + 1] = callback end
local success, failure = xpcall(function()
  local finish = log.span("fixture", { profile = "dev" })
  deferred[1]()
  finish("test failure")
  deferred[2]()
  finish("duplicate")
  local rows = vim.fn.readfile(path)
  assert(#rows == 3, "completed span kept logging or finished twice")
  assert(vim.json.decode(rows[1]).event == "fixture.begin")
  assert(vim.json.decode(rows[2]).event == "fixture.waiting")
  assert(vim.json.decode(rows[3]).fields.error == "test failure")
  vim.fn.writefile({ string.rep("x", 2 * 1024 * 1024) }, path)
  log.write("bounded", { text = string.rep("y", 40000) })
  assert(vim.uv.fs_stat(path).size < 20000, "log size was not bounded")
  assert(vim.json.decode(vim.fn.readfile(path)[1]).event == "bounded")
  local original_write = vim.fn.writefile
  vim.fn.writefile = function() error("unwritable log") end
  local wrote = pcall(log.write, "failure")
  vim.fn.writefile = original_write
  assert(wrote, "logging failure escaped into startup")
end, debug.traceback)
vim.defer_fn = original_defer
vim.fn.delete(path)
if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("startup_log OK")
vim.cmd("qa!")
