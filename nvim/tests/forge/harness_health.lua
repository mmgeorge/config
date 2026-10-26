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
  state.session.current_task_id = "task"
  state.task = {{id = "task", status = "running"}}
  local queued = {text = "Next message", task_id = "task"}
  state.queue = {queued}
  local handoff = health.notice(state)
  assert(handoff.text == "Working" and handoff.animated and handoff.hint == "working",
    "a ready queued message must keep Working visible during handoff")
  state.queue_edit = {barrier = queued}
  assert(health.notice(state) == nil, "editing the next message must not imply a handoff")
  state.queue_edit = nil
  state.task[1].status = "paused"
  assert(health.notice(state) == nil, "a paused task must not imply queued execution")
  state.task[1].status = "running"
  queued.task_id = "another-task"
  assert(health.notice(state) == nil, "queued work for another task must not imply execution")
  queued.task_id = "task"
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
