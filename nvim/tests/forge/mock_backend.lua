local fixture = dofile(vim.fn.getcwd() .. "/nvim/tests/forge/support/status_fixture.lua")
vim.loader.enable(false)

local forge = require("forge")
local status = require("forge.status")

---@return table
local function snapshot(document)
  local snapshot = fixture.snapshot(document)
  snapshot.file, snapshot.section = {}, {}
  return snapshot
end

local original_notify = vim.notify
local ok, failure = xpcall(function()
  local request_list = {}
  status._set_runner_for_test(function(method, params, callback)
    assert(method == "status", "public Status must use the native route")
    request_list[#request_list + 1] = params.operation
    if params.operation == "open" or params.operation == "snapshot" then callback(snapshot(params.document))
    elseif params.operation == "refresh" or params.operation == "demand" or params.operation == "close_view" then callback(vim.NIL)
    elseif params.operation == "close" then callback({ closed = true })
    else error("unexpected native Status operation: " .. params.operation) end
  end)

  forge.setup({ about_auto_generate = false })
  vim.o.columns = 100
  vim.wo.number, vim.wo.relativenumber, vim.wo.signcolumn, vim.wo.foldcolumn = true, true, "yes", "2"
  local state = forge.open()
  assert(vim.wait(3000, function() return state.replica.status == "Applied" end, 10), "native Status did not apply its snapshot")
  local buffer = state.replica.buffer
  assert(vim.bo[buffer].filetype == "ForgeStatus", "public Status did not set ForgeStatus filetype")
  local winbar = vim.api.nvim_eval_statusline(vim.wo.winbar, {
    winid = vim.api.nvim_get_current_win(), use_winbar = true, maxwidth = vim.api.nvim_win_get_width(0),
  }).str
  assert(winbar:find("ForgeStatus", 1, true), "narrow native Status winbar omitted its title: " .. winbar)
  assert(not vim.wo.number and not vim.wo.relativenumber and vim.wo.signcolumn == "no" and vim.wo.foldcolumn == "0",
    "native Status did not own its window presentation")
  for _, key in ipairs({ "<Tab>", "q" }) do
    local mapping = vim.api.nvim_buf_call(buffer, function() return vim.fn.maparg(key, "n", false, true) end)
    assert(type(mapping.callback) == "function", "native Status did not install command mapping " .. key)
  end

  local ordinary = vim.api.nvim_create_buf(true, false)
  vim.api.nvim_win_set_buf(0, ordinary)
  assert(vim.wo.number and vim.wo.relativenumber and vim.wo.signcolumn == "yes" and vim.wo.foldcolumn == "2",
    "native Status presentation leaked into an ordinary buffer")
  vim.api.nvim_win_set_buf(0, buffer)
  assert(not vim.wo.number and not vim.wo.relativenumber and vim.wo.signcolumn == "no" and vim.wo.foldcolumn == "0",
    "native Status presentation did not restore on re-entry")

  assert(forge.open() == state, "public Status did not reuse the active native document")
  assert(vim.wait(1000, function()
    for _, operation in ipairs(request_list) do if operation == "refresh" then return true end end
    return false
  end, 10), "reopening public Status did not refresh the active document")
  status.close(state)
  assert(vim.wait(1000, function() return not state.active end, 10), "native Status did not close")
end, debug.traceback)

status._set_runner_for_test(nil)
vim.notify = original_notify
if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
end
print("mock_backend: native Status public presentation and command boundary passed")
vim.cmd("qa!")
