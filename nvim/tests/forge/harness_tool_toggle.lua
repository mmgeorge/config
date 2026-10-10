vim.loader.enable(false)
local client = require("forge.client")
local session = require("forge.session")
local original_request, original_accepting = client.request_for, client.host_accepting
local requests = {}
client.host_accepting = function() return true end
client.request_for = function(_, _, params, callback)
  if params.operation == "background_terminals" then callback({ supported = false }) return end
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
      metadata = { node = { id = "call:tool", kind = "tool", lifecycle = "settled",
        generation = 1, content_revision = 1, loaded_rows = 5, loaded_bytes = 20,
        default_display = "heading", display = "full", expansion = true },
        target = { { id = "call:tool", range = {
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
    assert(requests[previous + 1].params.operation == "node", "Tab did not target the owning node from row " .. row)
    assert(requests[previous + 1].params.node == "call:tool")
    assert(requests[previous + 1].params.action == "set_expansion")
    assert(requests[previous + 1].params.expanded == false)
    requests[previous + 1].callback({ expanded = true })
    assert(vim.wait(1000, function() return requests[previous + 2] ~= nil end, 1))
    assert(requests[previous + 2].params.operation == "sync")
    requests[previous + 2].callback({ patch = {} })
  end
end, debug.traceback)
if owner then owner.close() end
session.harness.presentation = nil
client.request_for, client.host_accepting = original_request, original_accepting
assert(success, failure)
print("harness_tool_toggle: passed")
