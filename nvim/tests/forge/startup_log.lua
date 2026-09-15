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
  local original_provider = vim.api.nvim_set_decoration_provider
  local provider
  vim.api.nvim_set_decoration_provider = function(_, callbacks) provider = callbacks end
  local buffer = vim.api.nvim_get_current_buf()
  local started = vim.uv.hrtime()
  log.watch_redraw(buffer, "fixture", started, "status.action.redraw", { operation = 1 })
  log.watch_redraw(buffer, "fixture", started, "status.update.redraw", { operation = 1, phase = "accepted" })
  vim.api.nvim_set_decoration_provider = original_provider
  assert(provider.on_start())
  provider.on_win(nil, vim.api.nvim_get_current_win(), buffer)
  provider.on_end()
  assert(vim.wait(1000, function() return #vim.fn.readfile(path) == 3 end, 5))
  local events = {}
  for _, row in ipairs(vim.fn.readfile(path)) do
    local record = vim.json.decode(row)
    events[record.event] = record.fields
  end
  assert(events["status.action.redraw"].operation == 1)
  assert(events["status.update.redraw"].phase == "accepted")
  assert(events["status.action.redraw"].ready_to_redraw_us >= 0)
  assert(not provider.on_start(), "completed redraw watches survived")
end, debug.traceback)
vim.defer_fn = original_defer
vim.fn.delete(path)
if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("startup_log OK")
vim.cmd("qa!")
