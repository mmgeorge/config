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
local settled, admitted = 0, 0
local function begin(action)
  task.transition(state, action, function() end, function() settled = settled + 1 end, function() admitted = admitted + 1 end)
end

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
assert(#failures == 1)

begin({ action = "plan", text = "Retain this pending task" })
local retained = requests[#requests]
retained.reply({ state = "accepted" })
begin({ action = "permission", mode = "invalid" })
requests[#requests].reply(nil, "setting rejected", { code = "request_failed" })
assert(state.task_operation.id == retained.params.operation_id and settled == 2,
  "rejected setting displaced the admitted task")
task.receive(state, { id = retained.params.operation_id, state = "completed" })
assert(settled == 3)

begin({ action = "execute", plan_id = "plan" })
local old_conversation = requests[#requests]
state.session = { id = "another-conversation" }
old_conversation.reply({ state = "completed" })
assert(settled == 3, "old conversation callback must not settle current state")
print("harness_tasks: passed")
vim.cmd("qa!")
