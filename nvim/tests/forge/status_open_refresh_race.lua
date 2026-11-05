local root = vim.fn.getcwd()
vim.opt.runtimepath:prepend(root .. "/nvim")
local fixture = dofile(root .. "/nvim/tests/forge/support/status_fixture.lua")
local status = require("forge.status")
local pending, opened, notices = nil, nil, {}
local original_notify = vim.notify
vim.notify = function(message, level)
  if level == vim.log.levels.ERROR then notices[#notices + 1] = tostring(message) end
end
status._set_runner_for_test(function(_, params, callback)
  if params.operation == "open" then callback(fixture.snapshot(params.document))
  elseif params.operation == "refresh" then pending = callback
  elseif params.operation == "snapshot" then callback(fixture.snapshot(params.document, { revision = 1 }))
  elseif params.operation == "open_target" then
    opened = vim.deepcopy(params.input)
    assert(opened.revision == 1, "queued open retained the pre-refresh revision")
    callback({ kind = "none" })
  elseif params.operation == "close" then callback({ closed = true })
  elseif params.operation == "close_view" then callback(nil)
  else error("unexpected operation: " .. params.operation) end
end)
local state = status.open({ workspace = root, bind = false })
assert(vim.wait(2000, function() return state.ready and not state.request_active end))
vim.api.nvim_clear_autocmds({ group = state.group })
vim.api.nvim_set_current_buf(state.replica.buffer)
vim.api.nvim_win_set_cursor(0, { 4, 0 })
-- Recovery requires a second request and an asynchronously applied snapshot.
state.refresh_recover = true
status.refresh(state)
status.action(state, "open", false)
assert(not opened, "open overtook its pending refresh")
assert(pending, "refresh was not requested")
pending(nil)
assert(vim.wait(2000, function() return opened ~= nil end), vim.inspect({notices=notices, active=state.refresh_active, epoch=state.refresh_epoch, recover=state.refresh_recover, queue=#(state.request_queue or {}), revision=state.replica.revision}))
assert(opened.location.kind == "file" and opened.location.id == 1, "refresh retargeted the clicked file")
assert(#notices == 0, vim.inspect(notices))
status.close(state)
status._set_runner_for_test(nil)
vim.notify = original_notify
print("status_open_refresh_race passed")
vim.cmd("qa!")
