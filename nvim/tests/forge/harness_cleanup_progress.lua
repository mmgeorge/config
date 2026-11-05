vim.opt.runtimepath:prepend("nvim")
vim.loader.enable(false)
local progress = require("forge.views.harness.cleanup_progress")
local activity = require("forge.views.harness.activity")
local health = require("forge.views.harness.health")
local state = { runtime_epoch = "cleanup-owner", activity = { revision = 2, phase = "working" } }

progress.receive(state, { operation_id = "cancel-one", policy = "cancellation", state = "pending",
  message = "Cancellation still pending" })
assert(health.notice(state).text == "Cancellation still pending")
assert(activity.busy(state), "progress must not complete execution")
progress.timeout(state, "cancel-one")
assert(health.notice(state).text == "Exceeded cancellation timeout")
assert(activity.busy(state), "a response timeout must not release host cleanup ownership")
assert(not activity.receive(state, { revision = 1, phase = "idle" }, "cleanup-owner"))
assert(health.notice(state), "stale idle activity erased pending cleanup")

progress.receive(state, { operation_id = "cancel-two", policy = "cancellation", state = "pending",
  message = "Cancellation still pending" })
progress.settled(state, "cancel-one")
progress.timeout(state, "cancel-one")
assert(health.notice(state).text == "Cancellation still pending", "an old response replaced newer progress")
state.runtime_epoch = "replacement-owner"
progress.timeout(state, "cancel-two")
assert(health.notice(state).text == "Cancellation still pending", "retired-runtime response changed the notice")
assert(activity.receive(state, { revision = 3, phase = "idle" }, "replacement-owner"))
assert(not health.notice(state), "authoritative late settlement retained the cleanup notice")
assert(not activity.busy(state))
print("harness_cleanup_progress: passed")
vim.cmd("qa!")
