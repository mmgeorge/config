vim.opt.runtimepath:prepend("nvim")
local client = require("forge.client")
local controller = require("forge.views.harness.controller")
local notifications = require("forge.infra.notifications")
local state = require("forge.session").harness
local requests, errors, refreshes = {}, {}, 0
local generation = 1
client.host_generation = function() return generation end
client.request_for = function(id, method, params, callback)
  requests[#requests + 1] = { id = id, method = method, params = params, reply = callback }
end
notifications.error = function(message) errors[#errors + 1] = message end
controller.refresh_winbar = function() refreshes = refreshes + 1 end
state.session = { id = "session", name = "Existing name", model = "selected", effort = "high" }
state.busy = true
state.pending_config = { model = "pending-model" }
controller.rename_session("")
assert(requests[1].params.operation == "session_name" and requests[1].params.model == "pending-model")
assert(state.rename_status == "Generating session name…")
assert(state.session.name == "Existing name" and state.session.effort == "high" and state.busy)
requests[1].reply({ text = "Terminal status fixes" })
assert(state.rename_status == "Generating session name…")
assert(requests[2].method == "session.rename" and requests[2].params.expected_name == "Existing name")
requests[2].reply({ name = "Terminal status fixes", model = "stale model" })
assert(state.session.name == "Terminal status fixes" and state.session.model == "selected")
assert(refreshes == 2 and #errors == 0 and state.rename_status == nil)

controller.rename_session("")
local generated = requests[#requests]
controller.rename_session("Manual name")
local manual = requests[#requests]
manual.reply({ name = "Manual name" })
local count = #requests
generated.reply({ text = "Stale name" })
assert(#requests == count and state.session.name == "Manual name")

controller.rename_session("")
local older = requests[#requests]
controller.rename_session("")
older.reply({ text = "Older generated name" })
assert(requests[#requests].params.operation == "session_name")
assert(state.rename_status == "Generating session name…", "stale generation cleared newer progress")
requests[#requests].reply(nil, "generation failed")
assert(errors[1] == "generation failed" and state.session.name == "Manual name")
assert(state.rename_status == nil, "failed generation retained its status")
controller.rename_session("")
requests[#requests].reply({ text = "  " })
assert(#errors == 2 and state.session.name == "Manual name")

controller.rename_session("")
local before_restart = requests[#requests]
generation = 2
count = #requests
before_restart.reply({ text = "Late reply" })
assert(#requests == count and state.rename_status == nil)
controller.rename_session("")
local other_session = requests[#requests]
state.session = { id = "other", name = "Other session", model = "default", resolved_model = "private-alias" }
state.pending_config = nil
other_session.reply({ text = "Wrong session" })
assert(state.session.name == "Other session")
controller.rename_session("")
assert(requests[#requests].id == "other" and requests[#requests].params.model == "default")
state.composer_buf = vim.api.nvim_create_buf(false, true)
require("forge.views.harness.prompt_history").record = function() end
vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "/rename" })
controller.submit()
assert(requests[#requests].params.operation == "session_name", "bare /rename bypassed generation")
assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "")
vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "/rename Explicit title" })
controller.submit()
assert(requests[#requests].method == "session.rename" and requests[#requests].params.name == "Explicit title")
local tree = require("forge.render.harness.interaction_tree").build({
  { kind = "session_event", id = "rename", event = { kind = "renamed", name = "Old title" } },
  { kind = "session_event", id = "fork", event = { kind = "forked", source_session_id = "source" } },
})
assert(#tree.lines == 1 and tree.lines[1]:find("Forked from source", 1, true),
  "rename confirmation survived in cached Lua timeline rendering")
print("harness_session_name: passed")
vim.cmd("qa!")
