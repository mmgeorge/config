local fixture = dofile(vim.fn.getcwd() .. "/nvim/tests/forge/support/status_fixture.lua")
vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local status = require("forge.status")
local demand_callback

local function metadata(target, fold)
  return { target = target or {}, decoration = {}, editable_region = {}, visible_decoration = {},
    fold = fold or {}, gutter = {} }
end

status._set_runner_for_test(function(_, params, callback)
  if params.operation == "open" then
    callback(fixture.snapshot(params.document, { path = "sample.txt" }))
  elseif params.operation == "demand" then
    demand_callback = callback
  elseif params.operation == "close" then
    callback({ closed = true })
  else
    callback(vim.NIL)
  end
end)

local state = status.open({ workspace = vim.fn.getcwd() })
assert(vim.wait(1000, function() return state.replica.status == "Applied" end, 5))
local first_window = vim.api.nvim_get_current_win()
vim.cmd("vsplit")
local second_window = vim.api.nvim_get_current_win()
for _, window in ipairs({ first_window, second_window }) do
  vim.api.nvim_set_current_win(window)
  vim.api.nvim_win_set_cursor(window, { 4, 0 })
  vim.cmd("normal zR")
end
assert(vim.wait(1000, function() return demand_callback ~= nil end, 5), "opening deferred folds did not demand their body")
for _, window in ipairs({ first_window, second_window }) do
  assert(vim.api.nvim_win_call(window, function() return vim.fn.foldclosed(4) end) == 4,
    "deferred placeholder became visible before its body arrived")
end

demand_callback(fixture.body(state.document, { "@@ +1 -1" }))
assert(vim.wait(1000, function() return not state.pending and not state.fold_open end, 5), "deferred fold delivery did not settle")
assert(vim.deep_equal(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, false),
  { "main abc", "", "Unstaged changes (1):", "Modified sample.txt", "@@ +1 -1" }))
for _, window in ipairs({ first_window, second_window }) do
  assert(vim.api.nvim_win_call(window, function() return vim.fn.foldclosed(4) end) == -1,
    "loaded fold did not open in every requesting window")
end

status.close(state)
status._set_runner_for_test(nil)
print("status deferred folds passed")
vim.cmd("qa!")
