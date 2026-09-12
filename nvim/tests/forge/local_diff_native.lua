local fixture = dofile(vim.fn.getcwd() .. "/nvim/tests/forge/support/status_fixture.lua")
local root = vim.fn.getcwd()
vim.opt.runtimepath:prepend(root .. "/nvim")
package.path = root .. "/nvim/lua/?.lua;" .. root .. "/nvim/lua/?/init.lua;" .. package.path
local status = require("forge.status")
local local_diff = require("forge.local_diff")
local opened, held_close, held_open = 0, nil, nil
status._set_runner_for_test(function(method, params, callback)
  assert(method == "status")
  if params.operation == "local" then
    opened = opened + 1
    local function answer()
      callback(fixture.snapshot(params.document, { revision = 1, path = params.filename, view = { kind = "local", path = params.filename } }))
    end
    if opened == 7 then held_open = answer else answer() end
  elseif params.operation == "close" and opened == 6 and not held_close then
    held_close = callback
  else callback({}) end
end)
local ok, failure = xpcall(function()
  local origin = vim.api.nvim_get_current_buf()
  local first
  for index = 1, 6 do
    local buffer = local_diff.open(root .. "/sample" .. index .. ".txt")
    first = first or buffer
    assert(vim.wait(1000, function() return local_diff.owner(buffer).state.replica.status == "Applied" end))
  end
  assert(vim.api.nvim_get_current_buf() == origin, "preview creation bound a window")
  assert(local_diff.open(root .. "/sample6.txt") == local_diff.open(root .. "/sample6.txt"), "preview reuse changed buffer")
  local last = local_diff.open(root .. "/sample7.txt")
  assert(opened == 6, "new admission started before old close collection")
  assert(not vim.api.nvim_buf_is_valid(first), "old hidden preview was not evicted")
  held_close({})
  assert(vim.wait(1000, function() return opened == 7 end))
  local_diff.close(root .. "/sample7.txt")
  held_open()
  vim.wait(20, function() return false end)
  assert(not vim.api.nvim_buf_is_valid(last), "late response recreated a closed preview")
  assert(vim.api.nvim_get_current_buf() == origin, "late response replaced the origin")
end, debug.traceback)
local_diff.close_all()
status._set_runner_for_test(nil)
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit") end
vim.cmd("qa!")
