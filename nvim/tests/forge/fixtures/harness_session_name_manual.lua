local client = require("forge.client")
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local layout = require("forge.views.harness.layout")
client.subscribe = function() return function() end end
state.transcript_buf, state.transcript_win, state.composer_buf, state.composer_win, state.timeline_tab =
  layout.open("session-name-manual")
state.session = { id = "session-name-manual", name = "Existing session", model = "selected-model", effort = "high" }
state.busy = true
local buffer = require("forge.buffer")
local transcript = buffer.open("session-name-status", {})
state.transcript_buf = transcript.buffer
vim.api.nvim_win_set_buf(state.transcript_win, transcript.buffer)
assert(buffer.apply_snapshot(transcript, {
  document = transcript.document, revision = 0, block = { {
    id = "status", text = { "User: Fix background terminal counts", "", "Working (2s)" },
    metadata = { target = { { id = "status:working", range = {
      start = { row = 2, column = 0 }, ["end"] = { row = 3, column = 0 },
    } } }, decoration = {}, fold = {}, editable_region = {} },
  } },
}).kind == "Applied")
state.presentation = { transcript = transcript, close = function()
  require("forge.views.harness.status_hint").clear(transcript.buffer)
  return true
end }
client.request_for = function(id, method, params, callback)
  assert(id == state.session.id)
  if method == "harness.document" then
    assert(params.operation == "session_name" and params.model == "selected-model")
    vim.defer_fn(function() callback({ text = "Background terminal counts" }) end, 8000)
  elseif method == "session.rename" then
    callback({ name = params.name })
  elseif method == "history.record" then
    callback({})
  else
    error("unexpected request " .. method)
  end
end
controller.attach()
controller.refresh_winbar()
require("forge.views.harness.status_hint").render(transcript, state.command_set, 100)
