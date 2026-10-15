vim.loader.enable(false)
local client = require("forge.client")
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local picker = require("forge.views.picker")
local specification, opened, requests = nil, {}, {}
package.loaded["forge.views.plan_review"] = { open = function(plan) opened[#opened + 1] = plan end }
picker.open = function(value) specification = value end
client.host_generation = function() return 1 end
client.request_for = function(session_id, method, params, callback)
  assert(method == "plan.get")
  requests[#requests + 1] = { session_id = session_id, params = params, callback = callback }
end
state.session = { id = "browsing", current_task_id = "current-task" }
state.active_plan = { id = "current-plan" }
state.artifact = {
  { id = "failed-plan", title = "Failed attempt", state = "failed", updated_at_ms = 3 },
  { id = "current-plan", title = "Current review", state = "awaiting_review", updated_at_ms = 2 },
  { id = "older-plan", title = "Older review", state = "awaiting_review", updated_at_ms = 1 },
}
controller.open_artifact_picker()
assert(specification.page_list[1].option_list[1].value.id == "current-plan")
specification.on_confirm({ option = specification.page_list[1].option_list[3] })
requests[1].callback({ id = "older-plan" })
assert(opened[1].id == "older-plan")
assert(state.active_plan.id == "current-plan" and state.session.current_task_id == "current-task")
state.transcript_buf = vim.api.nvim_get_current_buf()
state.presentation = {
  activate = function(callback) callback({ kind = "plan", plan_id = "older-plan", revision = 1 }, {}) end,
  open_output = function() return false end,
}
controller.open_timeline_entry()
assert(requests[2].session_id == "browsing" and requests[2].params.revision == 1)
requests[2].callback({ id = "older-plan", historical_revision = 1 })
assert(#opened == 2 and state.active_plan.id == "current-plan")
controller.open_artifact_picker()
local stale = specification
state.session = { id = "other-session" }
stale.on_confirm({ option = stale.page_list[1].option_list[1] })
assert(#requests == 2, "stale picker sent a request into another session")
requests[2].callback({ id = "stale-plan" })
assert(#opened == 2, "stale response opened a review in another session")
print("harness_plan_browsing: task ownership and session boundaries preserved")
vim.cmd("qa!")
