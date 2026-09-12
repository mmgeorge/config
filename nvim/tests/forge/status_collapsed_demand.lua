local fixture = dofile(vim.fn.getcwd() .. "/nvim/tests/forge/support/status_fixture.lua")
vim.loader.enable(false)
local status = require("forge.status")
local demand_count = 0
local state
local ok, failure = xpcall(function()
  status._set_runner_for_test(function(_, params, callback)
    if params.operation == "open" then
      callback(fixture.snapshot(params.document, { path = "source.rs" }))
    elseif params.operation == "demand" then
      demand_count = demand_count + 1
      callback(fixture.body(params.input.document, { "changed source" }))
    elseif params.operation == "close" then callback({ closed = true })
    elseif params.operation == "close_view" then callback(nil)
    else error("unexpected status operation " .. params.operation) end
  end)
  state = status.open({ workspace = vim.fn.getcwd() })
  assert(vim.wait(1000, function() return next(state.view) ~= nil and not state.scheduled end, 5))
  assert(vim.fn.foldclosed(4) == 4, "initial file did not collapse")
  status.demand(state)
  assert(vim.wait(1000, function() return not state.scheduled end, 5))
  assert(demand_count == 0, "collapsed file eagerly loaded its source")
  vim.api.nvim_win_set_cursor(0, { 4, 0 })
  local mapping = vim.fn.maparg("<Tab>", "n", false, true)
  assert(mapping.callback, "fold toggle was not installed")
  mapping.callback()
  assert(vim.wait(1000, function() return demand_count == 1 end, 5), "opening the file did not demand its source")
  assert(vim.fn.foldclosed(4) == -1, "file toggle failed to expand")
end, debug.traceback)
if state then status.close(state) end
status._set_runner_for_test(nil)
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") else vim.cmd("qa!") end
