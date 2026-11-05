vim.opt.runtimepath:prepend("nvim")
local requests, subscribers, updates, notices = {}, {}, {}, {}
local capability = { background_terminals = true }
package.loaded["forge.client"] = {
  request = function(spec, callback)
    assert(spec.session_id == "session" and spec.params.operation == "background_terminals")
    requests[#requests + 1] = callback
  end,
  session_events_subscribe = function(callback)
    subscribers[callback] = true
    return function() subscribers[callback] = nil end
  end,
}
local function emit(event, payload, session_id)
  for subscriber in pairs(subscribers) do subscriber(event, payload, session_id or "session") end
end
local watch = require("forge.views.harness.terminals").watch({
  session_id = "session", alive = function() return true end,
  capability = function() return capability end,
  update = function(snapshot) updates[#updates + 1] = snapshot end,
  notice = function(message) notices[#notices + 1] = message end,
})
assert(#requests == 1, "attachment must query once")
watch.refresh()
assert(#requests == 1, "overlapping query admitted")
emit("provider_event", { kind = "terminals_changed", data = { terminal = {{ id = "live" }} } }, "other")
assert(#updates == 0, "foreign session changed inventory")
emit("provider_event", { kind = "terminals_changed", data = { terminal = {{ id = "live" }} } })
requests[1]({ terminal = {{ id = "stale" }} })
assert(#updates == 1 and updates[1].terminal[1].id == "live", "late snapshot overwrote stream")
emit("provider_event", { kind = "terminals_changed", data = { terminal = {} } })
assert(#updates[2].terminal == 0 and #requests == 1, "terminal exit must arrive without polling")
watch.refresh()
requests[2](nil, "offline")
assert(updates[3].unavailable and #notices == 1)
watch.refresh()
requests[3](nil, "offline")
assert(#notices == 1, "duplicate failure notification")
watch.refresh()
local count = #updates
watch.close()
requests[4]({ terminal = {{ id = "late" }} })
emit("provider_event", { kind = "terminals_changed", data = { terminal = {{ id = "late" }} } })
assert(#updates == count and next(subscribers) == nil, "closed observer retained updates")
capability = nil
local waiting = require("forge.views.harness.terminals").watch({
  session_id = "session", alive = function() return true end,
  capability = function() return capability end,
  update = function() end, notice = function() error("unexpected notice") end,
})
assert(#requests == 4, "unknown capability queried native state")
capability = { background_terminals = true }
emit("session_configured", {})
assert(#requests == 5, "configuration event did not initialize inventory")
requests[5]({ supported = true, terminal = {} })
waiting.close()
assert(next(subscribers) == nil)
print("harness_terminals: passed")
vim.cmd("qa!")
