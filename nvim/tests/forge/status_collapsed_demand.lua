local fixture = dofile(vim.fn.getcwd() .. "/nvim/tests/forge/support/status_fixture.lua")
vim.loader.enable(false)
local status = require("forge.status")
local demand_count = 0
local state
local notify = vim.notify
local notices = {}
vim.notify = function(message) notices[#notices + 1] = message end
local ok, failure = xpcall(function()
  status._set_runner_for_test(function(_, params, callback)
    if params.operation == "open" then
      callback(fixture.snapshot(params.document, { path = "source.rs" }))
    elseif params.operation == "demand" then
      demand_count = demand_count + 1
      local delivery = fixture.body(params.input.document, { "changed source" })
      delivery.syntax_diagnostic = "Syntax highlighting unavailable: fixture parse failure"
      callback(delivery)
    elseif params.operation == "close" then callback({ closed = true })
    elseif params.operation == "close_view" then callback(nil)
    else error("unexpected status operation " .. params.operation) end
  end)
  state = status.open({ workspace = vim.fn.getcwd() })
  assert(vim.wait(1000, function() return next(state.view) ~= nil and not state.scheduled end, 5))
  assert(require("forge.nodes").closed(state.replica,"file:1"), "initial file did not collapse")
  status.demand(state)
  assert(vim.wait(1000, function() return not state.scheduled end, 5))
  assert(demand_count == 0, "collapsed file eagerly loaded its source")
  vim.api.nvim_win_set_cursor(0, { 4, 0 })
  local mapping = vim.fn.maparg("<Tab>", "n", false, true)
  assert(mapping.callback, "fold toggle was not installed")
  mapping.callback()
  assert(vim.wait(1000, function() return demand_count == 1 and not state.pending end, 5), "opening the file did not demand its source")
  assert(not require("forge.nodes").closed(state.replica,"file:1"), "file toggle failed to expand")
  assert(#notices == 1 and notices[1] == "Syntax highlighting unavailable: fixture parse failure",
    "syntax failure did not notify while retaining the diff")
  assert(vim.api.nvim_buf_get_lines(state.replica.buffer, 4, 5, false)[1] == "changed source",
    "syntax failure discarded valid diff text")
end, debug.traceback)
if state then status.close(state) end
status._set_runner_for_test(nil)
vim.notify = notify
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") else vim.cmd("qa!") end
