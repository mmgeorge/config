vim.loader.enable(false)
local task = require("forge.views.harness.task")
local client = require("forge.client")
local requests, outcomes, failures = {}, {}, {}
client.request = function(spec, reply) requests[#requests+1] = { spec=spec, reply=reply } end
require("forge.infra.notifications").error = function(message) failures[#failures+1] = message end
vim.defer_fn = function() error("task view installed a retry timer") end
local state = { session = { id = "conversation", model = "original" }, activity = { revision=1, phase="idle" } }
local function begin(action)
  task.transition(state, action, function() end, function(failure) outcomes[#outcomes+1] = failure or "complete" end)
end
begin({ action="configure", config={model="first"} })
begin({ action="configure", config={model="second"} })
assert(requests[1].spec.params.operation_id ~= requests[2].spec.params.operation_id)
assert(not state.task_operation and not state.task_operation_by_id and state.session.model == "original",
  "view manufactured task progress or committed configuration")
requests[2].reply(nil, "rejected", {code="request_failed"})
requests[1].reply({state="completed"})
assert(#outcomes == 2 and outcomes[1] == "rejected" and outcomes[2] == "complete")
assert(#failures == 1 and state.session.model == "original" and state.activity.phase == "idle")
begin({action="plan", text="Create a plan"})
assert(requests[3].spec.method == "session.command.apply")
requests[3].reply({state="completed"})
assert(#outcomes == 3)
task.transition(state, {action="configure",selected={"import:fixture"}}, function() end,
  function(failure) assert(not failure) end, nil, "settings.import.apply")
assert(requests[4].spec.method == "settings.import.apply")
requests[4].reply({state="completed"})
assert(#requests == 4, "feature view issued recovery or snapshot requests")
print("harness_tasks: finite command results passed")
vim.cmd("qa!")
