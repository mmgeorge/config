vim.opt.runtimepath:prepend("nvim")
local subscribers, requests = {}, {}
package.loaded["forge.client"] = {
  session_events_subscribe = function(callback)
    subscribers[callback] = true
    return function() subscribers[callback] = nil end
  end,
  host_get_generation = function() return 1 end,
  request = function(spec, callback) requests[#requests + 1] = {spec=spec, callback=callback} end,
}
local session = require("forge.session")
session.harness.session = {id="events", provider="fixture"}
local catalog = require("forge.views.harness.provider_catalog")
local function emit(event, payload, session_id)
  for callback in pairs(subscribers) do callback(event, payload, session_id or "events") end
end
local completions = {}
local function completed(result, failure) completions[#completions + 1] = {result=result, failure=failure} end
catalog.set_mcp_enabled("server", true, completed)
emit("task_operation", {id="fast", state="completed"})
requests[1].callback({id="fast", state="accepted"})
assert(#completions == 1 and completions[1].result.id == "fast" and #requests == 1,
  "completion before receipt was lost")
assert(next(subscribers) == nil)
catalog.set_skill_enabled("skill", false, completed)
requests[2].callback({id="slow", state="accepted"})
assert(requests[3].spec.method == "session.command.operation.get")
requests[3].callback({id="slow", state="running"})
emit("task_operation", {id="other", state="completed"})
emit("task_operation", {id="slow", state="completed"}, "foreign")
assert(#completions == 1 and #requests == 3, "foreign completion or pending state triggered polling")
emit("task_operation", {id="slow", state="completed"})
assert(#completions == 2 and next(subscribers) == nil)
catalog.set_mcp_enabled("server", false, completed)
requests[4].callback({id="failed", state="accepted"})
emit("task_operation", {id="failed", state="failed", error="normalized failure"})
requests[5].callback({id="failed", state="completed"})
assert(#completions == 3 and completions[3].failure == "normalized failure", "late read completed twice")
catalog.set_skill_enabled("skill", true, completed)
requests[6].callback({id="lost", state="accepted"})
requests[7].callback({id="lost", state="running"})
emit("host_stopped", {message="host closed"})
assert(#completions == 4 and completions[4].failure == "host closed" and next(subscribers) == nil)
print("provider_catalog_events: passed")
vim.cmd("qa!")
