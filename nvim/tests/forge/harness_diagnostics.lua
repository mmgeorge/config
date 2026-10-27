vim.opt.runtimepath:prepend("nvim")
local health = require("forge.views.harness.health")
local diagnostic = require("forge.views.harness.diagnostic")
local state = { session = { id = "diagnostics" }, status = { kind = "awaiting_plan_review" } }
diagnostic.set(state, "execution_notice", "Message delivery failed", {
  owner = "message", category = "delivery", severity = "error",
})
assert(not health.notice(state), "message failure replaced canonical plan review")
assert(diagnostic.get(state, "execution_notice", "warning").failed)
diagnostic.set(state, "sync_error", "Snapshot delayed", { owner = "snapshot", severity = "info" })
assert(not health.notice(state).failed)
diagnostic.set(state, "sync_error", nil)
assert(state.execution_notice == "Message delivery failed", "unrelated recovery removed message failure")
assert(not health.notice({ connection_error = "Response delayed" }).failed)
assert(not health.notice({ execution_notice = "User stopped" }))
assert(health.notice({ host_error = "Storage corrupt" }).failed)
print("harness_diagnostics: passed")
vim.cmd("qa!")
