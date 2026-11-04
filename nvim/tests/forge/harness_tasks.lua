vim.opt.runtimepath:prepend("nvim")
local task = require("forge.views.harness.task")
local client = require("forge.client")
local notifications = require("forge.infra.notifications")
local requests, timers, failures = {}, {}, {}
client.request = function(request_specification, reply)
  local session_id = request_specification.session_id
  local method = request_specification.method
  local params = request_specification.params or {}

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
task.receive(state, { id = changing.params.operation_id, state = "applying", revision = 3 })
begin({ action = "configure", config = { effort = "high" } })
local rejected = requests[#requests]
task.receive(state, { id = changing.params.operation_id, state = "resuming", revision = 5 })
rejected.reply(nil, "rejected", { code = "request_failed" })
assert(#failures == 1 and failures[1]:find("rejected", 1, true),
  "the task owner did not report the rejected operation")
assert(background_settled == 1, "rejected setting did not resolve its completion callback")
assert(state.task_operation.state == "resuming", "rollback restored obsolete task state")
changing.reply({ state = "accepted", revision = 2 })
assert(state.task_operation.state == "resuming", "stale receipt regressed task progress")
task.receive(state, { id = changing.params.operation_id, state = "completed", revision = 6 })
requests, timers, failures = {}, {}, {}
settled, admitted = 0, 0

begin({ action = "plan", text = "Create a plan" })
local first = requests[1]
assert(first.method == "session.command.apply" and first.params.operation_id)
task.receive(state, { id = first.params.operation_id, state = "active" })
assert(admitted == 1 and settled == 0)
begin({ action = "pause" })
local pause = requests[2]
task.receive(state, { id = first.params.operation_id, state = "completed" })
assert(settled == 0, "completion from superseded task must not settle the latest intent")
task.receive(state, { id = pause.params.operation_id, state = "active" })
pause.reply({ id = pause.params.operation_id, state = "completed" })
assert(settled == 1 and state.task_operation == nil)

begin({ action = "resume" })
local original = requests[#requests]
local pending_request_count = #requests
assert(#timers == 0, "task control created a status-poll timer while shared command recovery owns observation")
-- The shared client owns any status recovery and reports its final finite observation here.
original.reply({ state = "outcome_unknown", error = "Provider acknowledgement lost" })
assert(#requests == pending_request_count, "task control issued a duplicate status query or replay")
assert(settled == 2 and state.execution_notice:find("Provider acknowledgement lost", 1, true))
assert(#failures == 0, "uncertain acknowledgement was displayed as a failed operation")

begin({ action = "plan", text = "Retain this pending task" })
local retained = requests[#requests]
task.receive(state, { id = retained.params.operation_id, state = "active" })
begin({ action = "permission", mode = "invalid" })
requests[#requests].reply(nil, "setting rejected", { code = "request_failed" })
assert(state.task_operation.id == retained.params.operation_id and settled == 2,
  "rejected setting displaced the admitted task")
task.receive(state, { id = retained.params.operation_id, state = "completed" })
assert(settled == 3)

begin({ action = "resume" })
local observed = requests[#requests]
local observed_request_count = #requests
task.receive(state, { id = observed.params.operation_id, state = "running", revision = 2 })
assert(state.task_operation.state == "running" and settled == 3,
  "task progress settled the command before its finite completion")
assert(#timers == 0 and #requests == observed_request_count,
  "task progress created a second status observer")
assert(not require("forge.views.harness.health").notice(state),
  "task progress replaced canonical execution status")
observed.reply({ state = "completed", revision = 3 })
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
local import_request_count = #requests
assert(#timers == 0, "import added task-owned recovery polling")
-- Transport uncertainty is resolved by the shared client; the task layer receives only completion.
imported.reply({ state = "completed" })
assert(#requests == import_request_count, "import recovery replayed the settings mutation")
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
