vim.opt.runtimepath:prepend("nvim")
local task = require("forge.views.harness.task")
local client = require("forge.client")
local notifications = require("forge.infra.notifications")
local requests, timers, failures = {}, {}, {}
client.request_for = function(session_id, method, params, reply)
  requests[#requests + 1] = { session_id = session_id, method = method, params = params, reply = reply }
end
vim.defer_fn = function(callback) timers[#timers + 1] = callback end
notifications.error = function(message) failures[#failures + 1] = message end
local state = { session = { id = "conversation" }, task = {} }
local settled, admitted, background_settled = 0, 0, 0
local function begin(action)
  task.transition(state, action, function() end, function(_, background)
    if background then background_settled = background_settled + 1 else settled = settled + 1 end
  end, function() admitted = admitted + 1 end)
end

begin({ action = "configure", config = { model = "model-a" } })
local changing = requests[#requests]
changing.reply({ state = "applying", revision = 3 })
begin({ action = "configure", config = { effort = "high" } })
local rejected = requests[#requests]
task.receive(state, { id = changing.params.operation_id, state = "resuming", revision = 5 })
rejected.reply(nil, "rejected", { code = "request_failed" })
assert(background_settled == 1, "rejected setting did not resolve its completion callback")
assert(state.task_operation.state == "resuming", "rollback restored obsolete task state")
changing.reply({ state = "accepted", revision = 2 })
assert(state.task_operation.state == "resuming", "stale receipt regressed task progress")
task.receive(state, { id = changing.params.operation_id, state = "completed", revision = 6 })
requests, timers, failures = {}, {}, {}
settled, admitted = 0, 0

begin({ action = "plan", text = "Create a plan" })
local first = requests[1]
assert(first.method == "task.transition" and first.params.operation_id)
first.reply({ id = first.params.operation_id, state = "accepted" })
assert(admitted == 1 and settled == 0)
begin({ action = "pause" })
local pause = requests[2]
task.receive(state, { id = first.params.operation_id, state = "completed" })
assert(settled == 0, "completion from superseded task must not settle the latest intent")
pause.reply({ id = pause.params.operation_id, state = "accepted" })
task.receive(state, { id = pause.params.operation_id, state = "completed" })
assert(settled == 1 and state.task_operation == nil)

begin({ action = "resume" })
local original = requests[#requests]
timers[#timers]()
local query = requests[#requests]
assert(query.method == "task.operation" and query.params.operation_id == original.params.operation_id,
  "uncertain acknowledgement must query the original operation instead of replaying")
query.reply({ state = "outcome_unknown", error = "Provider acknowledgement lost" })
assert(settled == 2 and state.execution_notice:find("Provider acknowledgement lost", 1, true))
assert(#failures == 0, "uncertain acknowledgement was displayed as a failed operation")

begin({ action = "plan", text = "Retain this pending task" })
local retained = requests[#requests]
retained.reply({ state = "accepted" })
begin({ action = "permission", mode = "invalid" })
requests[#requests].reply(nil, "setting rejected", { code = "request_failed" })
assert(state.task_operation.id == retained.params.operation_id and settled == 2,
  "rejected setting displaced the admitted task")
task.receive(state, { id = retained.params.operation_id, state = "completed" })
assert(settled == 3)

begin({ action = "resume" })
local polled = requests[#requests]
polled.reply({ state = "running" })
timers[#timers]()
requests[#requests].reply(nil, "Forge request admission is full", { code = "not_admitted" })
assert(state.task_operation.state == "running" and settled == 3,
  "a failed status read changed the admitted operation outcome")
assert(state.execution_notice:find("Retrying task status", 1, true))
assert(not require("forge.views.harness.health").notice(state),
  "task-control polling diagnostic replaced canonical execution status")
timers[#timers]()
requests[#requests].reply({ state = "running" })
assert(state.execution_notice == nil, "a successful status read retained the transient failure")
task.receive(state, { id = polled.params.operation_id, state = "completed" })
assert(settled == 4)

begin({ action = "resume" })
local cancelled = requests[#requests]
task.receive(state, { id = cancelled.params.operation_id, state = "cancelled" })
assert(settled == 5 and state.task_operation == nil and state.execution_notice == nil,
  "user cancellation did not settle cleanly")
local failure_count = #failures
task.receive(state, { id = cancelled.params.operation_id, state = "cancelled" })
assert(settled == 5 and #failures == failure_count, "duplicate cancellation caused an error")

begin({ action = "execute", plan_id = "plan" })
local old_conversation = requests[#requests]
state.session = { id = "another-conversation" }
old_conversation.reply({ state = "completed" })
assert(settled == 5, "old conversation callback must not settle current state")
state.session = { id = "import-conversation" }
task.transition(state, { action = "configure", selected = { "import:fixture" } }, function() end,
  function() settled = settled + 1 end, nil, "settings.import.apply")
local imported = requests[#requests]
assert(imported.method == "settings.import.apply", "import bypassed its durable mutation route")
imported.reply(nil, "acknowledgement unavailable", { code = "outcome_unknown" })
timers[#timers]()
local inspection = requests[#requests]
assert(inspection.method == "task.operation" and inspection.params.operation_id == imported.params.operation_id,
  "unknown import acknowledgement replayed the settings mutation")
inspection.reply({ state = "completed" })
assert(settled == 6 and state.task_operation == nil)
begin({ action = "pause" })
local queued_control = requests[#requests]
queued_control.reply(nil, "admission full", { code = "not_admitted" })
assert(state.task_operation and not require("forge.views.harness.health").notice(state))
timers[#timers]()
local retry = requests[#requests]
assert(retry.method == queued_control.method and retry.params.operation_id == queued_control.params.operation_id,
  "a local admission retry changed operation identity")
retry.reply({ state = "accepted" })
assert(not state.execution_notice, "admission success retained a waiting diagnostic")
task.receive(state, { id = retry.params.operation_id, state = "completed" })
assert(settled == 7)
begin({ action = "configure", config = { model = "model-b" } })
local earlier = requests[#requests]
begin({ action = "configure", config = { effort = "high" } })
local latest = requests[#requests]
earlier.reply(nil, "admission full", { code = "not_admitted" })
timers[#timers]()
assert(requests[#requests].params.operation_id == earlier.params.operation_id,
  "newer settings suppressed retry of an unadmitted earlier intent")
assert(state.task_operation.id == latest.params.operation_id,
  "background admission retry displaced the current operation")
print("harness_tasks: passed")
vim.cmd("qa!")
