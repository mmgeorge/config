vim.loader.enable(false)
local clock, generation, tick = 0, 1
local original_now, original_defer = vim.uv.now, vim.defer_fn
local original_client = package.loaded["forge.client"]
local state = { session = { id = "session" }, busy = true,
  approval = { { id = "approval" } }, status = { kind = "working" } }
local refresh_count = 0
local responsive = true
local success, failure = xpcall(function()
  vim.uv.now = function() return clock end
  vim.defer_fn = function(callback) tick = callback end
  package.loaded["forge.client"] = {
    host_generation = function() return generation end,
    request_for = function(_, method, _, callback)
      assert(method == "health.get")
      if responsive then callback({}, nil) end
    end,
  }
  package.loaded["forge.views.harness.health"] = nil
  local health = require("forge.views.harness.health")
  health.watch(state, function() refresh_count = refresh_count + 1 end)
  local function advance(count)
    for _ = 1, count do clock = clock + 2000 tick() end
  end
  advance(30)
  assert(health.notice(state).text == "Waiting for your approval")
  assert(health.notice(state).waiting and health.notice(state).hint == "permission")
  state.approval_open = false
  advance(15)
  assert(health.notice(state).text == "Waiting for your approval", "closing the picker resolved the approval")
  state.approval = {}
  advance(150)
  assert(health.notice(state) == nil, "provider silence must not replace the native status")
  for _, kind in ipairs({ "awaiting_input", "awaiting_plan_review" }) do
    state.status.kind = kind
    advance(30)
    assert(health.notice(state) == nil)
  end
  state.status.kind = "working"
  state.active_elicitation = { id = "question" }
  advance(30)
  assert(health.notice(state) == nil)
  state.active_elicitation = nil
  advance(15)
  assert(health.notice(state) == nil)
  state.busy = false
  advance(1)
  assert(health.notice(state) == nil)
  responsive = false
  advance(6)
  assert(health.notice(state).text == "Connection unresponsive — task status unknown",
    "host heartbeat failure must remain visible")
  local previous_refreshes = refresh_count
  generation = 2
  state.busy = true
  advance(30)
  assert(refresh_count == previous_refreshes, "stale host watcher updated status")
end, debug.traceback)
vim.uv.now, vim.defer_fn = original_now, original_defer
package.loaded["forge.client"] = original_client
package.loaded["forge.views.harness.health"] = nil
assert(success, failure)
print("harness_health: passed")
vim.cmd("qa!")
