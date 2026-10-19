vim.opt.runtimepath:prepend("nvim")
local client = require("forge.client")
local queue = require("forge.views.harness.check_queue")
local health = require("forge.views.harness.health")
local diagnostic = require("forge.views.harness.diagnostic")
local requests, timers = {}, {}
client.request_for = function(_, _, _, callback) requests[#requests + 1] = callback end
vim.defer_fn = function(callback) timers[#timers + 1] = callback end
local state = { ready = true, session = { id = "retry" }, runtime_epoch = "epoch", queue = { "follow up" } }
local changed
changed = function() queue.publish(state, changed) end
queue.publish(state, changed)
requests[1](nil, "admission full", { code = "not_admitted" })
assert(state.queue_notice and not state.execution_notice)
assert(not health.notice(state).failed)
timers[1]()
assert(#requests == 2, "queue did not retry its rejected publication")
diagnostic.set(state, "execution_notice", "Unrelated failure", { owner = "other", severity = "error" })
requests[2]({})
assert(not state.queue_notice and state.execution_notice == "Unrelated failure",
  "successful queue publication cleared an unrelated diagnostic")
assert(health.notice(state).failed)
diagnostic.set(state, "execution_notice", nil)
assert(not health.notice(state))
assert(not health.notice({ connection_error = "Response delayed" }).failed)
assert(not health.notice({ execution_notice = "User stopped" }).failed)
assert(health.notice({ host_error = "Storage corrupt" }).failed)
print("harness_diagnostics: passed")
vim.cmd("qa!")
