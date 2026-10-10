vim.loader.enable(false)
local snapshot = require("forge.views.harness.snapshot")
local state = {
  session = { id = "snapshot-test", context_usage = { used = 12 } },
  goal = { state = "active" },
  active_elicitation = { elicitation = { id = "old" } },
  selected_agent_run_id = "missing",
}
local response = vim.json.decode([[{
  "session": {"id": "snapshot-test", "context_usage": null, "name": null},
  "goal": null, "goal_execution": null, "active_plan": null,
  "active_elicitation": null, "active_wait": null,
  "capability": null, "artifact": [], "approval": [],
  "agent": {"run": [], "definition": [], "exchange": []},
  "timeline": [], "timeline_revision": 3, "prompt_history": null
}]])
snapshot.apply(state, response)
for _, field in ipairs({ "goal", "goal_execution", "active_plan", "active_elicitation", "active_wait" }) do
  assert(state[field] == nil, field .. " retained a JSON null")
end
assert(state.session.name == nil and state.session.context_usage.used == 12)
assert(state.selected_agent_run_id == nil and state.timeline_revision == 3)
assert(type(state.capability) == "table" and #state.approval == 0)
assert(response.goal == vim.NIL and response.session.context_usage == vim.NIL,
  "snapshot projection mutated the transport response")
response.goal = { state = "paused", completed_at_ms = vim.NIL }
response.active_elicitation = { elicitation = { id = "new", response = vim.NIL } }
snapshot.apply(state, response)
assert(state.goal.state == "paused" and state.goal.completed_at_ms == nil)
assert(state.active_elicitation.elicitation.id == "new" and state.active_elicitation.elicitation.response == nil)
response.runtime_epoch, response.snapshot_revision = "first-runtime", 5
snapshot.apply(state, response)
local stale = vim.deepcopy(response)
stale.snapshot_revision, stale.session.name = 4, "stale"
assert(snapshot.apply(state, stale) == false and state.session.name ~= "stale")
state.task_operation, state.busy = { id = "old-operation" }, true
response.runtime_epoch, response.snapshot_revision = "restarted-runtime", 1
response.task_operation = { state = "outcome_unknown", error = "Interrupted before confirmation" }
assert(snapshot.apply(state, response))
assert(not state.busy and not state.task_operation and state.execution_notice == "Interrupted before confirmation")
response.snapshot_revision = 2
response.task_operation = nil
response.status = { kind = "awaiting_plan_review", revision = 1 }
assert(snapshot.apply(state, response))
assert(state.execution_notice == nil, "settled operation failure survived the authoritative snapshot")
response.snapshot_revision = 3
response.task_operation = { id = "failed", state = "failed", error = "Provider failed" }
snapshot.apply(state, response)
state.execution_notice = "Connection lost"
response.snapshot_revision = 4
response.task_operation = nil
snapshot.apply(state, response)
assert(state.execution_notice == "Connection lost", "clearing an old operation hid a newer error")
print("harness_snapshot: passed")
