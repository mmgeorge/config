vim.loader.enable(false)
local client = require("forge.client")
local original = client.request_for
local request = {}
client.request_for = function(session, method, params, callback)
  assert(session == "tool-test" and method == "harness.document")
  request[#request + 1] = { params = params, callback = callback }
end
local function snapshot(document)
  return { document = document, revision = 0, block = { { id = "tool:0", text = { "tool output" }, metadata = { target = {}, decoration = {}, editable_region = {} } } } }
end
local owner
local success, failure = xpcall(function()
  local window = vim.api.nvim_get_current_win()
  local origin = vim.api.nvim_win_get_buf(window)
  owner = require("forge.views.harness.tool_output").open({ session_id = "tool-test", window = window,
    input = { sequence = 2 }, is_current = function() return true end, notice = error })
  assert(request[1].params.operation == "tool_open" and request[1].params.input.sequence == 2)
  request[1].callback({ snapshot = snapshot(owner.document), more = true })
  assert(vim.api.nvim_win_get_buf(window) == owner.replica.buffer)
  assert(request[2].params.operation == "tool_demand")
  owner.demand()
  assert(#request == 2, "concurrent tool demand")
  request[2].callback({ more = false })
  local exported
  owner.export(function(path) exported = path end)
  request[3].callback({ path = "retained-output.txt" })
  assert(exported == "retained-output.txt")
  owner.close()
  assert(request[4].params.operation == "close" and vim.api.nvim_win_get_buf(window) == origin)
  assert(not vim.api.nvim_buf_is_valid(owner.replica.buffer))
  local stale = require("forge.views.harness.tool_output").open({ session_id = "tool-test", window = window,
    input = {}, is_current = function() return false end, notice = error })
  request[5].callback({ snapshot = snapshot(stale.document), more = true })
  assert(stale.closed and request[6].params.operation == "close")
  assert(vim.api.nvim_win_get_buf(window) == origin)
  local cancelled = require("forge.views.harness.tool_output").open({ session_id = "tool-test", window = window,
    input = {}, is_current = function() return true end, notice = error })
  cancelled.close()
  assert(request[8].params.operation == "close")
  request[7].callback({ snapshot = snapshot(cancelled.document), more = true })
  assert(request[9].params.operation == "close", "late native open was not collected")
end, debug.traceback)
if owner and not owner.closed then owner.close() end
client.request_for = original
assert(success, failure)
print("harness_tool_output: passed")
