vim.loader.enable(false)
local snapshot = require("forge.views.harness.snapshot")
local original_startup = package.loaded["forge.lsp.startup"]
package.loaded["forge.lsp.startup"] = { start = function() error("snapshot started LSP discovery") end }
local state = {
  session = { id = "snapshot-test", context_usage = { used = 12 } },
  active_elicitation = { elicitation = { question_set = { id = "old" } } },
  selected_agent_run_id = "missing",
}
local response = vim.json.decode([[{
  "session": {"id": "snapshot-test", "workspace": "/workspace", "context_usage": null, "name": null, "active_task": null},
  "goal": null, "goal_execution": null, "active_plan": null,
  "active_elicitation": null, "active_wait": null,
  "capability": null, "artifact": [], "approval": [],
  "agent": {"run": [], "definition": [], "exchange": []},
  "timeline": [], "timeline_revision": 3, "prompt_history": null
}]])
local before_invalid = vim.deepcopy(state)
local valid, failure = pcall(snapshot.apply, state, { id = "plan-command", outcome = { outcome = "task" } })
assert(not valid and tostring(failure):find("session identity", 1, true), "command receipt passed snapshot validation")
assert(vim.deep_equal(state, before_invalid), "invalid snapshot corrupted the current session")
response.access_preference = { sandbox = true, isolation_mode = "mxc" }
snapshot.apply(state, response)
assert(state.access_preference.sandbox and state.access_preference.isolation_mode == "mxc")
for _, field in ipairs({ "active_plan", "active_elicitation", "active_wait" }) do
  assert(state[field] == nil, field .. " retained a JSON null")
end
assert(state.session.name == nil and state.session.context_usage.used == 12)
assert(state.selected_agent_run_id == nil and state.timeline_revision == 3)
assert(type(state.capability) == "table" and #state.approval == 0)
assert(response.goal == vim.NIL and response.session.context_usage == vim.NIL,
  "snapshot projection mutated the transport response")
state.session.model, state.session.context_window = "old-model", "large"
response.session.model, response.session.context_window = "new-model", "small"
snapshot.apply(state, response)
assert(state.session.context_usage == nil, "new model inherited old model context observations")
state.session.context_usage = { used = 1, size = 1000000 }
response.runtime_epoch = "new-epoch"
snapshot.apply(state, response)
assert(state.session.context_usage == nil, "new runtime inherited old runtime context observations")
response.session.active_task = { revision = "goal", kind = "goal", phase = "work", status = "running", goal = { state = "paused", objective = "Test" } }
response.active_elicitation = { elicitation = { question_set = { id = "new" }, response = vim.NIL } }
snapshot.apply(state, response)
assert(state.session.active_task.goal.state == "paused")
assert(state.active_elicitation.elicitation.question_set.id == "new" and state.active_elicitation.elicitation.response == nil)
response.runtime_epoch, response.snapshot_revision = "first-runtime", 5
snapshot.apply(state, response)
local stale = vim.deepcopy(response)
stale.snapshot_revision, stale.session.name = 4, "stale"
assert(snapshot.apply(state, stale) == false and state.session.name ~= "stale")
state.activity = { revision = 9, phase = "working" }
response.runtime_epoch, response.snapshot_revision = "restarted-runtime", 1
response.activity = { revision = 0, phase = "idle" }
state.execution_notice = "Command failed"
snapshot.apply(state, response)
assert(state.execution_notice == "Command failed", "session projection cleared an independently owned command diagnostic")
state.question_drafts = { choice = { ask = "Explain the tradeoff" } }
state.question_picker_resume = { page_id = "choice" }
response.session.id = "replacement-session"
response.snapshot_revision = 1
assert(snapshot.apply(state, response))
assert(not require("forge.views.harness.activity").busy(state), "replacement session inherited activity")
assert(state.question_drafts == nil, "a different session inherited question drafts")
state.question_drafts = { choice = { ask = "A different question" } }
response.session.id = "snapshot-test"
assert(snapshot.apply(state, response))
assert(state.question_drafts.choice.ask == "Explain the tradeoff", "returning to a session lost its question draft")
assert(state.question_picker_resume.page_id == "choice")
-- Receiving a page cannot certify the owner's post-send durable acknowledgment.
state.publication_cursor = 4
response.owner_publication = { receipt_id = "slow-page", after = 4, through = 7, high_water = 9, more = true }
response.snapshot_revision = 2
assert(snapshot.apply(state, response))
assert(state.publication_cursor == 4, "received page advanced its cursor before owner confirmation")
assert(state.publication_received.through == 7)
-- An expired send is retried from the same confirmed cursor with fresh page authority.
response.owner_publication.receipt_id = "retried-page"
response.snapshot_revision = 3
assert(snapshot.apply(state, response) and state.publication_cursor == 4)
assert(not snapshot.confirm_publication(state, {
  receipt_id = "slow-page", runtime_epoch = state.runtime_epoch, after = 4, through = 7,
}), "late confirmation acknowledged a different received page")
assert(snapshot.confirm_publication(state, {
  receipt_id = "retried-page", runtime_epoch = state.runtime_epoch, after = 4, through = 7,
}))
assert(state.publication_cursor == 7 and state.publication_received == nil)
local older_page = vim.deepcopy(response)
older_page.snapshot_revision, older_page.owner_publication.through = 1, 5
assert(snapshot.apply(state, older_page) == false and state.publication_cursor == 7,
  "a stale snapshot moved the retained publication cursor")
response.snapshot_revision = 4
response.owner_publication = { receipt_id = "empty-page", after = 7, high_water = 7, more = false }
assert(snapshot.apply(state, response) and state.publication_cursor == 7,
  "an empty page lost its acknowledged cursor")
assert(snapshot.confirm_publication(state, {
  receipt_id = "empty-page", runtime_epoch = state.runtime_epoch, after = 7,
}))
response.session.id, response.snapshot_revision, response.owner_publication = "new-publication-session", 1, nil
assert(snapshot.apply(state, response) and state.publication_cursor == nil and state.publication_received == nil,
  "another session inherited publication authority")
-- Reconnect admits a publication page and its authoritative input baseline together.
local client = require("forge.client")
local input_state = require("forge.views.harness.input_state")
local original_request = client.request
local reconnect = { session = { id = "approval-reconnect" }, runtime_epoch = "approval-owner",
  snapshot_revision = 1, publication_cursor = 61 }
local submitted
client.request = function(request, reply)
  if request.method == "session.input.get" then
    reply({ revision = 62, control_revision = 7, suspended = false, messages = {} })
  else
    assert(request.method == "session.input.apply", "reconnect introduced an extra input read")
    submitted = request.params
  end
end
input_state.attach(reconnect, function() end)
local resynchronized = { session = reconnect.session, runtime_epoch = reconnect.runtime_epoch,
  snapshot_revision = 2, approval = { { id = "active-approval" } },
  input = { revision = 65, control_revision = 10, suspended = true, messages = {} },
  owner_publication = { receipt_id = "approval-page", after = 61, through = 65, more = false,
    events = { { sequence = 63, event = { session_id = "approval-reconnect", event = "input_changed",
      payload = { revision = 63, _publication = { sequence = 63 } } } } } } }
assert(snapshot.apply(reconnect, resynchronized))
assert(reconnect.input.revision == 65 and reconnect.input.control_revision == 10 and reconnect.input.suspended,
  "active-approval reconnect did not adopt the authoritative input baseline")
assert(reconnect.approval[1].id == "active-approval" and reconnect.publication_cursor == 61,
  "reconnect lost its approval or prematurely confirmed publication")
assert(snapshot.confirm_publication(reconnect, { receipt_id = "approval-page", runtime_epoch = reconnect.runtime_epoch,
  after = 61, through = 65 }))
input_state.submit(reconnect, "continue after cancellation", {}, "auto")
assert(submitted.control_revision == 10, "post-reconnect submit used a stale control revision")
resynchronized.snapshot_revision = 3
resynchronized.input = { revision = 64, control_revision = 9, suspended = false, messages = {} }
assert(snapshot.apply(reconnect, resynchronized))
assert(reconnect.input.revision == 65 and reconnect.input.control_revision == 10 and reconnect.input.suspended,
  "newer session snapshot regressed its independently ordered input projection")
local previous_input = reconnect.input
resynchronized.runtime_epoch, resynchronized.snapshot_revision = "replacement-owner", 1
resynchronized.input = { revision = 1, control_revision = 1, suspended = false, messages = {} }
assert(snapshot.apply(reconnect, resynchronized))
assert(reconnect.input == previous_input and reconnect.input.revision == 65,
  "replacement runtime input was applied to the previous runtime's input owner")
client.request = original_request
package.loaded["forge.lsp.startup"] = original_startup
print("harness_snapshot: passed")
