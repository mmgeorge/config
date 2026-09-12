local root = vim.fn.getcwd()
vim.opt.runtimepath:prepend(root .. "/nvim")
package.path = root .. "/nvim/lua/?.lua;" .. root .. "/nvim/lua/?/init.lua;" .. package.path
local status = require("forge.status")
local notices, requests, pending, selected = {}, {}, nil, nil
vim.notify = function(message) notices[#notices + 1] = tostring(message) end
local function metadata(target, count)
  return { target = { { id = target, range = { start = { row = 0, column = 0 }, ["end"] = { row = count, column = 0 } } } }, decoration = {}, editable_region = {} }
end
status._set_runner_for_test(function(method, params, callback)
  requests[#requests + 1] = params.operation
  assert(method == "status", method)
  if params.operation == "open" then
    callback({ document = params.document, revision = 0, view = { kind = "status" },
      head = { state = "attached", reference = "main", object = "abc" }, context = vim.NIL,
      section = { { kind = "unstaged", file = { 1 } } },
      file = { { id = 1, generation = 1, section = "unstaged", change = "modified", path = "source.txt", untracked = false, stats = { state = "unknown" } } } })
  elseif params.operation == "demand" then
    assert(params.input.location.kind == "file" and params.input.location.id == 1)
    assert(params.input.block == nil and params.input.position == nil)
    pending = function()
      callback({ document = params.input.document, file = 1, generation = 1, more = false, state = { state = "ready" },
        snapshot = { document = "body:1:1", revision = 1, block = { { id = "body:3", text = { "old", "new" }, metadata = metadata("hunk:4", 2) } } } })
    end
  elseif params.operation == "input" then selected = params callback({ success = true })
  elseif params.operation == "refresh" or params.operation == "close_view" then callback(nil)
  elseif params.operation == "close" then callback({ closed = true })
  else error("unexpected status request: " .. params.operation) end
end)
local baseline = { number = vim.wo.number, wrap = vim.wo.wrap }
local state = status.open({ workspace = root })
assert(vim.wait(2000, function() return state.ready end), table.concat(notices, "\n"))
assert(state.replica.status == "Applied")
assert(not pending, "collapsed file triggered demand")
vim.api.nvim_win_set_cursor(0, { 4, 0 })
vim.cmd("normal! za")
status.demand(state)
assert(vim.wait(2000, function() return pending ~= nil end), table.concat(notices, "\n"))
pending()
assert(vim.wait(2000, function() return state.replica.row_count == 6 end), table.concat(notices, "\n"))
assert(vim.deep_equal(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, true), { "main abc", "", "Unstaged changes (1):", "Modified source.txt", "old", "new" }))
assert(vim.wait(2000, function() return not state.request_active end))
vim.api.nvim_win_set_cursor(0, { 5, 0 })
vim.cmd("normal! Vj")
status.action(state, "stage", true)
assert(vim.wait(2000, function() return selected ~= nil end))
assert(selected.input.location.kind == "body" and selected.selection.target[1].target == "hunk:4")
assert(#selected.selection.target == 1)
status.close(state)
assert(vim.wait(2000, function() return not state.request_active end))
assert(vim.wo.number == baseline.number and vim.wo.wrap == baseline.wrap)
assert(#notices == 0, table.concat(notices, "\n"))
status._set_runner_for_test(nil)
print("semantic status runtime: passed")
