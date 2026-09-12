local fixture = dofile(vim.fn.getcwd() .. "/nvim/tests/forge/support/status_fixture.lua")
vim.loader.enable(false)

local forge = require("forge")
local status = require("forge.status")

local function empty_snapshot(document)
  local snapshot = fixture.snapshot(document)
  snapshot.file, snapshot.section = {}, {}
  return snapshot
end

local ok, failure = xpcall(function()
  local opened = 0
  status._set_runner_for_test(function(method, params, callback)
    assert(method == "status", "public Status must use the native host route")
    if params.operation == "snapshot" then callback(empty_snapshot(params.document))
    elseif params.operation == "open" then
      opened = opened + 1
      callback(empty_snapshot(params.document))
    elseif params.operation == "demand" or params.operation == "close_view" then callback(vim.NIL)
    elseif params.operation == "refresh" then callback(vim.NIL)
    elseif params.operation == "close" then callback({ closed = true })
    else error("unexpected native Status operation: " .. params.operation) end
  end)
  forge.setup({ about_auto_generate = false })
  local state = forge.open()
  assert(vim.wait(3000, function() return state.replica.status == "Applied" end, 10), "native empty Status did not apply")
  assert(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, false)[1] == "main abc", "native empty Status text changed")
  assert(forge.open() == state and opened == 1, "public ForgeStatus did not reuse the active native document")
  status.close(state)
end, debug.traceback)

status._set_runner_for_test(nil)
if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
end
print("process_stdio: native empty Status public boundary passed")
vim.cmd("qa!")
