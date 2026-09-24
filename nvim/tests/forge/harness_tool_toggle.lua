vim.loader.enable(false)
local client = require("forge.client")
local session = require("forge.session")
local original_request, original_accepting = client.request_for, client.host_accepting
local requests = {}
client.host_accepting = function() return true end
client.request_for = function(_, _, params, callback)
  requests[#requests + 1] = { params = params, callback = callback }
end
local owner
local success, failure = xpcall(function()
  local transcript = vim.api.nvim_create_buf(false, true)
  local composer = vim.api.nvim_create_buf(false, true)
  local window = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(window, transcript)
  owner = require("forge.views.harness.presentation").open({ session_id = "toggle-test",
    transcript_buffer = transcript, composer_buffer = composer, transcript_window = window,
    is_alive = function() return true end, notice = error,
  }, function(value, err) assert(value and not err, err) end)
  local opened = requests[1].params
  requests[1].callback({ transcript = { document = opened.document, revision = 0, block = {
    { id = "call:tool", text = { "tool()", "one", "two", "three", "four", "…(2 hidden)" },
      metadata = { target = { { id = "call:tool", range = {
        start = { row = 0, column = 0 }, ["end"] = { row = 6, column = 0 },
      } } }, decoration = {}, editable_region = {} } },
  } }, composer = { document = opened.composer, revision = 0, block = {} } })
  session.harness.transcript_buf = transcript
  session.harness.transcript_win = window
  session.harness.presentation = owner
  for _, row in ipairs({ 2, 3, 4, 5, 6 }) do
    vim.api.nvim_win_set_cursor(window, { row, 0 })
    local previous = #requests
    require("forge.views.harness.controller").toggle_activity()
    assert(requests[previous + 1].params.operation == "toggle_tool", "Tab did not expand preview row " .. row)
    assert(requests[previous + 1].params.input.position.row == row - 1)
    requests[previous + 1].callback({ expanded = true })
    assert(requests[previous + 2].params.operation == "sync")
    requests[previous + 2].callback({ patch = {} })
  end
end, debug.traceback)
if owner then owner.close() end
session.harness.presentation = nil
client.request_for, client.host_accepting = original_request, original_accepting
assert(success, failure)
print("harness_tool_toggle: passed")
