vim.loader.enable(false)
local activity = require("forge.views.harness.activity")
local state = { runtime_epoch = "owner-one" }
assert(activity.receive(state, { revision = 1, phase = "working" }, "owner-one"))
assert(activity.busy(state))
assert(activity.receive(state, { revision = 2, phase = "idle" }, "owner-one"))
assert(not activity.receive(state, { revision = 1, phase = "working" }, "owner-one"))
assert(not activity.receive(state, { revision = 5, phase = "working" }, "old-owner"))
state.request_busy, state.execution_busy, state.busy = true, true, true
assert(not activity.busy(state), "local request flags changed authoritative activity")
assert(activity.receive(state, { revision = 3, phase = "unavailable" }, "owner-one"))
assert(not activity.busy(state))
print("harness_activity: passed")
vim.cmd("qa!")
