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
  assert(vim.wait(1000, function()
    if vim.fn.filereadable(path) == 0 then return false end
    local rows = vim.fn.readfile(path)
    return #rows == 3 and rows[3]:sub(-1) == "}"
  end, 5), "asynchronous span records did not finish")
  local rows = vim.fn.readfile(path)
  assert(#rows == 3, "completed span kept logging or finished twice")
  assert(vim.json.decode(rows[1]).event == "fixture.begin")
  assert(vim.json.decode(rows[2]).event == "fixture.waiting")
  assert(vim.json.decode(rows[3]).fields.error == "test failure")
  vim.fn.writefile({ string.rep("x", 2 * 1024 * 1024) }, path)
  local complete_text = string.rep("y", 40000)
  log.write("rotated", { text = complete_text })
  assert(vim.wait(1000, function()
    if vim.fn.filereadable(path) == 0 then return false end
    local rows = vim.fn.readfile(path)
    local ok, record = pcall(vim.json.decode, rows[1] or "")
    return ok and record.event == "rotated"
  end, 5))
  assert(vim.json.decode(vim.fn.readfile(path)[1]).fields.text == complete_text, "large diagnostic record was truncated")
  assert(vim.fn.filereadable(path .. ".1") == 1, "rotation lost the preceding log")
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
vim.fn.delete(path .. ".1")
if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("startup_log OK")
vim.cmd("qa!")
