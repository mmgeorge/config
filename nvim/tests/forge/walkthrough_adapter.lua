vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local closed = false
local close_view = 0
require("forge.infra.config").setup({ walkthrough_inventory = false })
package.loaded["forge.client"] = { request_host = function(method, params, callback)
  assert(method == "walkthrough")
  if params.operation == "open" then
    callback({ inventory_state = params.inventory and "unavailable" or "disabled",
      inventory_diagnostic = params.inventory and "Sem fixture failure" or nil,
      snapshot = { document = params.document, revision = 1, block = {
    { id = "heading", text = { "Walkthrough heading" }, metadata = { target = {}, decoration = {}, editable_region = {}, fold = {
      { id = "fold", start = {row=0,column=0}, ["end"] = {block="body",position={row=1,column=0}}, closed=true } } } },
    { id = "body", text = { "Read-only detail" }, metadata = { target = {}, decoration = {}, editable_region = {} } } } } })
  elseif params.operation == "close_view" then close_view = close_view + 1 callback(vim.NIL)
  elseif params.operation == "close" then closed = true callback(true)
  else callback(vim.NIL) end
end }
local walkthrough = require("forge.walkthrough")
local notices = {}
local state
local success, failure = xpcall(function()
  state = walkthrough.open({ on_error = function(message) notices[#notices+1] = message end })
  assert(vim.wait(1000, function() return state.replica.status == "Applied" and not state.pending and #state.queue == 0 end))
  assert(#notices == 0, table.concat(notices,"\n"))
  assert(vim.api.nvim_get_current_buf() == state.replica.buffer)
  assert(vim.bo.readonly and vim.wo.foldmethod == "expr")
  vim.cmd("vsplit")
  vim.api.nvim_win_set_buf(0, state.replica.buffer)
  assert(vim.wait(1000, function()
    local count = 0
    for _ in pairs(state.view) do count = count + 1 end
    return count == 2
  end), "walkthrough did not attach both visible windows")
  walkthrough.close(state)
  assert(vim.wait(1000, function() return closed end))
  assert(next(state.view) == nil, "walkthrough close retained a visible view owner")
  assert(close_view == 0, "closing the walkthrough should collect the document without view requests")
  require("forge.infra.config").setup({ walkthrough_inventory = "sem" })
  notices = {}
  state = walkthrough.open({ on_error = function(message) notices[#notices+1] = message end })
  assert(vim.wait(1000, function() return state.replica.status == "Applied" and not state.pending and #state.queue == 0 end))
  assert(#notices == 1 and notices[1] == "Sem fixture failure", "Sem inventory failure was not surfaced")
  walkthrough.close(state)
  assert(vim.wait(1000, function() return closed end))
  state = nil
end, debug.traceback)
if state then walkthrough.close(state) end
if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("walkthrough_adapter OK")
vim.cmd("qa!")
