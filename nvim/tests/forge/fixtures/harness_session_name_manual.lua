local client = require("forge.client")
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local layout = require("forge.views.harness.layout")
client.session_events_subscribe = function() return function() end end
state.transcript_buf, state.transcript_win, state.composer_buf, state.composer_win, state.timeline_tab =
  layout.open("session-name-manual")
state.session = { id = "session-name-manual", name = "Existing session", model = "selected-model", effort = "high" }
state.busy = true
local buffer = require("forge.buffer")
local transcript = buffer.open("session-name-status", {})
state.transcript_buf = transcript.buffer
vim.api.nvim_win_set_buf(state.transcript_win, transcript.buffer)
local status_block = { id = "status", text = { "Working · 2s" }, metadata = {
  status = { row = 0, animated = true, hint = "working" }, target = {}, decoration = {}, editable_region = {},
} }
assert(buffer.apply_snapshot(transcript, { document = transcript.document, revision = 0, block = {
  { id = "prompt", text = { "User: Fix background terminal counts" }, metadata = {} },
} }).kind == "Applied")
state.presentation = { transcript = transcript, close = function()
  state.presentation.status_view:close()
  return true
end }
client.request = function(request_specification, callback)
  local id = request_specification.session_id
  local method = request_specification.method
  local params = request_specification.params or {}

  assert(id == state.session.id)
  if method == "session.document" then
    assert(params.operation == "session_name" and params.model == "selected-model")
    vim.defer_fn(function() callback({ text = "Background terminal counts" }) end, 8000)
  elseif method == "session.rename" then
    callback({ name = params.name })
  elseif method == "session.prompt.history.record" then
    callback({})
  else
    error("unexpected request " .. method)
  end
end
controller.attach()
controller.refresh_winbar()
local status_view = require("forge.views.harness.status_view").new({ transcript = transcript,
  transcript_window = state.transcript_win, composer_window = state.composer_win,
  alive = function() return true end, notice = function(message) error(message) end, recover = function() end })
state.presentation.status_view = status_view
status_view:render({}, state.command_set)
status_view:apply({ document = transcript.document .. ":status", revision = 0, block = { status_block } }, function(ok) assert(ok) end)
