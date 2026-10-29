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
    host_get_generation = function() return generation end,
    request = function(request_specification, callback)
  local method = request_specification.method

      assert(method == "session.health.get")
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
  assert(health.notice(state) == nil, "Lua must not replace the native lifecycle with an approval-list inference")
  state.approval_open = false
  advance(15)
  assert(health.notice(state) == nil, "closing the picker must not manufacture a status")
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
  state.status.kind = "idle"
  state.session.active_task = { revision = "task", kind = "goal", goal = { state = "active" } }
  local queued = {text = "Next message", purpose = {kind = "user"}}
  state.queue = {queued}
  local handoff = health.notice(state)
  assert(handoff.text == "Working" and handoff.animated and handoff.hint == "working",
    "a ready queued message must keep Working visible during handoff")
  state.queue_paused = true
  assert(health.notice(state) == nil, "locally paused input must not imply a queued handoff")
  state.session.active_task = nil
  assert(health.notice(state) == nil, "taskless chat cancellation must not retain a Working spinner")
  state.queue_paused = false
  assert(health.notice(state).text == "Working", "resuming taskless queued work lost its handoff status")
  state.session.active_task = { revision = "task", kind = "goal", goal = { state = "active" } }
  state.queue_edit = {barrier = queued}
  assert(health.notice(state) == nil, "editing the next message must not imply a handoff")
  state.queue_edit = nil
  state.session.active_task.goal.state = "paused"
  assert(health.notice(state).text == "Working", "paused task state must not hide explicitly queued user input")
  state.session.active_task = { revision = "replacement-task", kind = "goal", goal = { state = "active" } }
  assert(health.notice(state).text == "Working", "task replacement hid queued user input")
  state.checks_running = true
  assert(health.notice(state) == nil, "native check progress must remain visible")
  state.checks_running = false
  state.queue = {}
  state.busy = true
  state.prompt_submission = {queue_entry = queued, accepted = true}
  assert(health.notice(state).text == "Working", "admission must not flash Idle after removing the queue entry")
  state.status.kind = "working"
  assert(health.notice(state) == nil, "native execution status must replace the handoff display")
  state.prompt_submission, state.busy, state.status.kind = nil, false, "idle"
  assert(health.notice(state) == nil, "the empty queue must return to native Idle after settlement")
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
