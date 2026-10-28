local directory = vim.fn.tempname()
local original = { stdpath = vim.fn.stdpath, provider = vim.api.nvim_set_decoration_provider, write = vim.uv.fs_write }
local redraw
vim.fn.stdpath = function(kind) return kind == "cache" and directory or original.stdpath(kind) end
vim.api.nvim_set_decoration_provider = function(_, provider) redraw = provider end
vim.uv.fs_write = function(descriptor, contents, offset, receive)
  return original.write(descriptor, contents:sub(1, 7), offset, receive)
end
local log = require("forge.harness_startup_log")
local buffer = vim.api.nvim_create_buf(false, true)
local success, failure = xpcall(function()
  local trace = log.begin({ workspace = "/fixture" })
  trace.mark("host.ready")
  trace.watch(buffer)
  assert(vim.fn.filereadable(log.path()) == 0, "startup logging wrote synchronously")
  assert(redraw.on_start())
  redraw.on_win(nil, nil, buffer)
  redraw.on_end()
  assert(trace.finished, "first visible redraw did not finish startup")
  trace.mark("must_not_be_logged")
  local cancelled = log.begin()
  cancelled.watch(buffer)
  cancelled.finish("cancelled")
  assert(not redraw.on_start(), "cancelled startup retained its redraw observer")
  assert(vim.wait(2000, function()
    local lines = vim.fn.filereadable(log.path()) == 1 and vim.fn.readfile(log.path()) or {}
    return #lines == 5 and lines[5]:sub(-1) == "}"
  end, 5), "asynchronous startup records did not finish writing")
  local records = vim.tbl_map(vim.json.decode, vim.fn.readfile(log.path()))
  assert(records[1].phase == "open" and records[2].phase == "host.ready" and records[3].phase == "first_redraw")
  assert(records[4].phase == "open" and records[5].phase == "cancelled")
  assert(records[1].startup_id ~= records[4].startup_id, "startups reused a correlation identity")
  assert(records[3].elapsed_ms >= records[2].elapsed_ms and records[3].since_previous_ms >= 0)
end, debug.traceback)
vim.uv.fs_write, vim.api.nvim_set_decoration_provider = original.write, original.provider
vim.fn.stdpath = original.stdpath
vim.api.nvim_buf_delete(buffer, { force = true })
vim.fn.delete(directory, "rf")
assert(success, failure)
print("harness_startup_log: passed")
