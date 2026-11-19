local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local mock_client = dofile("nvim/lua/forge/tests/support/mock_client.lua")
local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
local eq = MiniTest.expect.equality
local T = MiniTest.new_set({ hooks = isolation.hooks() })

---Watch one session's inventory with recorded requests and a captured event subscriber.
---@return {observer: table, requests: RecordedRequest[], emit: fun(event: string, payload: table), counts: integer[]}
local function watch_inventory()
  local requests = mock_client.record()
  local subscriber
  stub(require("forge.client"), "session_events_subscribe", function(callback)
    subscriber = callback
    return function() subscriber = nil end
  end)
  local counts = {}
  local observer = require("forge.views.harness.terminals").watch({
    session_id = "session",
    alive = function() return true end,
    capability = function() return { background_terminals = true } end,
    update = function(snapshot) counts[#counts + 1] = #(snapshot.terminal or {}) end,
    notice = function() end,
  })
  return {
    observer = observer, requests = requests, counts = counts,
    emit = function(event, payload) subscriber(event, payload, "session") end,
  }
end

local one_terminal = { supported = true, terminal = { { id = "terminal", command = "sleep" } } }
local no_terminal = { supported = true, terminal = {} }

T["settled_turn_rereads_the_live_inventory"] = function()
  local fixture = watch_inventory()
  fixture.requests[1].callback(no_terminal)
  fixture.emit("provider_event", { kind = "terminals_changed", data = one_terminal })
  fixture.emit("activity_changed", { activity = { phase = "working" } })
  eq(#fixture.requests, 1)
  -- The provider never reported that the terminal ended, so only a read can correct the count.
  fixture.emit("activity_changed", { activity = { phase = "idle" } })
  eq(#fixture.requests, 2)
  eq(fixture.requests[2].spec.params, { operation = "background_terminals" })
  fixture.requests[2].callback(no_terminal)
  eq(fixture.counts, { 0, 1, 0 })
end

T["adopted_live_read_replaces_the_pushed_inventory"] = function()
  local fixture = watch_inventory()
  fixture.requests[1].callback(no_terminal)
  fixture.emit("provider_event", { kind = "terminals_changed", data = one_terminal })
  fixture.emit("activity_changed", { activity = { phase = "idle" } })
  -- Another reader's newer result wins over the read that was already in flight.
  fixture.observer.adopt(no_terminal)
  fixture.requests[2].callback(one_terminal)
  eq(fixture.counts, { 0, 1, 0 })
end

return T
