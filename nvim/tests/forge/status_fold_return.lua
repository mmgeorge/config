local fixture = dofile(vim.fn.getcwd() .. "/nvim/tests/forge/support/status_fixture.lua")
vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local status = require("forge.status")

local function metadata(target, fold)
  return { target = target or {}, decoration = {}, editable_region = {}, visible_decoration = {},
    fold = fold or {}, gutter = {} }
end

status._set_runner_for_test(function(_, params, callback)
  if params.operation == "open" then
    callback(fixture.snapshot(params.document, { path = "sample.txt" }))
  elseif params.operation == "demand" then
    callback(fixture.body(params.input.document, { "@@ +1 -1", "old", "new" }))
  elseif params.operation == "close" then
    callback({ closed = true })
  else
    callback(vim.NIL)
  end
end)

local state = status.open({ workspace = vim.fn.getcwd() })
assert(vim.wait(1000, function() return state.replica.status == "Applied" end, 5))
local window = vim.api.nvim_get_current_win()
vim.api.nvim_win_set_cursor(window, { 4, 0 })
vim.cmd("normal za")
status.demand(state)
assert(vim.wait(1000, function() return state.replica.file[1].body ~= nil end))
assert(vim.fn.foldclosed(4) == -1, "fixture fold did not open")

vim.cmd("enew")
vim.cmd("buffer " .. state.replica.buffer)
assert(vim.wait(1000, function() return state.view[window] ~= nil end, 5), "status view did not reattach")
assert(vim.fn.foldclosed(4) == -1, "open file fold was lost after returning to the status buffer")

status.close(state)
status._set_runner_for_test(nil)
print("status fold return passed")
vim.cmd("qa!")
