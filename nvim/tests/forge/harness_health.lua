vim.loader.enable(false)
local health = require("forge.views.harness.health")
local state = { session = { id = "session" }, status = { kind="idle" }, queue = {{text="Queued"}} }
assert(health.watch == nil)
for _, phase in ipairs({"idle","working","unavailable"}) do
  state.activity = { revision=1, phase=phase }
  assert(health.notice(state) == nil, "Lua manufactured lifecycle status from queued input")
end
state.status = {kind="working"}
state.approval = {{id="pending"}}
assert(health.notice(state) == nil, "permission UI replaced the owner status")
print("harness_health: owner status preserved")
vim.cmd("qa!")
