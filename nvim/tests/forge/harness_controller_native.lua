vim.loader.enable(false)
local client = require("forge.client")
local session = require("forge.session")
local controller = require("forge.views.harness.controller")
local original_request, original_subscribe, original_generation = client.request_for, client.subscribe, client.host_generation
local original_accepting = client.host_accepting
local original_completion = package.loaded["blink.cmp"]
local completion_visible = false
package.loaded["blink.cmp"] = { hide = function() completion_visible = false end }
client.host_accepting = function() return true end
local state = session.harness
local generation, submitted, receive, delayed_edit, navigated, sync_count = 1, nil, nil, nil, nil, 0
local initial = {}
local goal_request = {}
local submitted_callback
local function snapshot(document, text, composer)
  return { document = document, revision = 0, block = { { id = "body", text = { text },
    metadata = { target = {}, decoration = {}, editable_region = composer and { { id = "composer", revision = 0,
      range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = #text } } } } or {} } } } }
end
client.host_generation = function() return generation end
client.subscribe = function(callback) receive = callback return function() end end
client.request_for = function(session_id, method, params, callback)
  assert(session_id == "native-controller")
  if method == "prompt.submit" then submitted = params submitted_callback = callback return end
  if method == "goal.pause" or method == "goal.clear" then
    goal_request[#goal_request + 1] = { method = method, callback = callback }
    return
  end
  if method == "history.record" then vim.schedule(function() callback({}) end) return end
  assert(method == "harness.document")
  if params.operation == "open" then
    initial[#initial + 1] = params.initial[1]
    vim.schedule(function() callback({ transcript = snapshot(params.document, "Native transcript"), composer = snapshot(params.composer, params.initial[1], true) }) end)
  elseif params.operation == "edit_composer" then delayed_edit = callback
  elseif params.operation == "navigate_prompt" then
    navigated = params.input
    vim.schedule(function() callback({ anchor = { block = "body", position = { row = 0, column = 0 } } }) end)
  elseif params.operation == "sync" then
    sync_count = sync_count + 1
    vim.schedule(function() callback({ patch = {} }) end)
  else vim.schedule(function() callback({ patch = {} }) end) end
end
local success, failure = xpcall(function()
  state.transcript_buf, state.transcript_win, state.composer_buf, state.composer_win, state.timeline_tab = require("forge.views.harness.layout").open("native-controller")
  state.session = { id = "native-controller", backend = "mock", model = "mock", execution_mode = "read" }
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "native draft" })
  controller.attach()
  controller.render()
  assert(vim.wait(1000, function() return state.presentation and state.presentation.ready end))
  assert(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)[1] == "Native transcript")
  assert(vim.wo[state.transcript_win].breakindent, "native attachment discarded Harness continuation indentation")
  assert(vim.wo[state.composer_win].winbar:find("send", 1, true), "composer has no send hint")
  state.busy = true
  controller.refresh_winbar()
  assert(vim.wo[state.composer_win].winbar:find("queue", 1, true), "busy composer has no queue hint")
  state.goal = { state = "paused", created_at_ms = 1000 }
  receive("backend_event", { kind = "execution_state", data = {
    session = { id = "native-controller", backend = "mock", model = "mock", execution_mode = "write" },
    goal = { state = "active", created_at_ms = 1000 },
    goal_execution = { state = "active", created_at_ms = 1000, scheduler = { task = { { state = "active" } } } },
  } }, "native-controller")
  assert(state.busy and state.goal.state == "active" and state.session.execution_mode == "write")
  assert(vim.wo[state.transcript_win].winbar:find("Plan active", 1, true))
  receive("backend_event", { kind = "execution_state", data = {
    session = state.session, goal = vim.NIL, goal_execution = vim.NIL,
  } }, "native-controller")
  assert(state.goal == nil and state.goal_execution == nil, "absent execution state retained stale goal labels")
  state.busy = false
  state.session.execution_mode = "read"
  for _, owner in ipairs({ "goal", "goal_execution" }) do
    state[owner] = { state = "paused", created_at_ms = 1000 }
    controller.refresh_winbar()
    local label = owner == "goal" and "Goal paused" or "Plan paused"
    assert(vim.wo[state.transcript_win].winbar:find(label, 1, true), "paused work disappeared from the winbar")
    state[owner] = nil
    controller.refresh_winbar()
    assert(not vim.wo[state.transcript_win].winbar:find(label, 1, true), "stale pause label survived goal removal")
  end
  for status, label in pairs({ blocked = "blocked", stalled = "stalled",
    usage_limited = "usage limited", budget_limited = "budget limited" }) do
    for _, planning in ipairs({ false, true }) do
      state.goal = { state = status, created_at_ms = 1000 }
      state.goal_execution = planning and { state = "paused", created_at_ms = 1000 } or nil
      controller.refresh_winbar()
      assert(vim.wo[state.transcript_win].winbar:find((planning and "Plan " or "Goal ") .. label, 1, true),
        "native stop reason disappeared from the winbar")
    end
  end
  state.goal, state.goal_execution = nil, nil
  controller.refresh_winbar()
  local original_window = state.transcript_win
  vim.api.nvim_set_current_win(original_window)
  vim.cmd("vsplit")
  local secondary = vim.api.nvim_get_current_win()
  state.presentation.refresh_views()
  assert(vim.wait(1000, function() return state.presentation.views[secondary] ~= nil end))
  assert(vim.wo[secondary].breakindent and vim.wo[secondary].breakindentopt == "shift:0",
    "secondary Harness view did not retain continuation indentation")
  vim.api.nvim_win_set_cursor(original_window, { 1, 3 })
  vim.api.nvim_win_set_cursor(secondary, { 1, 5 })
  controller.jump_prompt(1)
  assert(vim.wait(1000, function() return navigated ~= nil and vim.api.nvim_win_get_cursor(secondary)[2] == 0 end))
  assert(navigated.view == state.presentation.views[secondary].id)
  assert(vim.api.nvim_win_get_cursor(original_window)[2] == 3, "secondary navigation moved the initiating window")
  vim.api.nvim_win_close(original_window, true)
  assert(vim.wait(1000, function() return state.presentation.views[original_window] == nil end))
  assert(not state.presentation.closed and state.presentation.views[secondary])
  local sync_before_submission = sync_count
  vim.api.nvim_set_current_win(state.composer_win)
  completion_visible = true
  controller.submit()
  assert(not completion_visible, "native submission retained completion for the consumed draft")
  assert(submitted and submitted.composer and not submitted.text)
  assert(vim.wait(2500, function() return sync_count > sync_before_submission end, 20),
    "busy Harness did not refresh the native transcript status")
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "native draft", "controller cleared before native admission")
  local metadata = snapshot(state.presentation.composer_id, "", true).block[1].metadata
  metadata.editable_region[1].revision = 1
  receive("backend_event", { kind = "composer_patch", data = { document = state.presentation.composer_id, base = 0, next = 1,
    base_rows = 1, next_rows = 1, base_blocks = 1, next_blocks = 1, block_edit = {}, removed_block = {},
    text_edit = { { start_row = 0, removed_rows = 1, text = { "" } } },
    metadata_edit = { { block = "body", row_count = 1, metadata = metadata } },
  } }, "native-controller")
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "")
  local original_state_request = client.request
  local recovered_state = false
  client.request = function(method)
    assert(method == "state.get")
    recovered_state = true
  end
  submitted_callback(nil, "simulated provider failure")
  assert(not state.busy and recovered_state, "provider failure did not reconcile execution state")
  state.state_sync_pending = false
  client.request = original_state_request
  vim.api.nvim_buf_set_text(state.composer_buf, 0, 0, 0, 0, { "draft after host collection" })
  require("forge.editable").flush(state.presentation.composer.editable)
  assert(delayed_edit)
  generation = 2
  controller.render()
  assert(vim.wait(1000, function() return state.presentation and state.presentation.ready and state.presentation.host_generation == 2 end))
  delayed_edit({ accepted = true })
  assert(initial[2] == "draft after host collection")
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "draft after host collection")
  submitted = nil
  state.busy = false
  state.queue = { "queued while finalizing" }
  state.status = { kind = "finalizing", exchange_id = "exchange-1", error = "cleanup pending" }
  controller.drain()
  assert(#state.queue == 1 and submitted == nil, "finalizing status drained queued input")
  state.status = { kind = "idle" }
  controller.drain()
  assert(#state.queue == 0 and submitted ~= nil, "idle status did not resume queued input")
  local original_plain_request = client.request
  local child_request = {}
  client.request = function(method, params, callback)
    if method == "history.record" then return end
    child_request[#child_request + 1] = { method = method, params = params }
    if method == "turn.cancel" then callback({ cancel_requested = true }) end
  end
  state.busy = true
  state.capability.native_steer = true
  state.selected_agent_run_id = "child"
  state.agent = { run = { { id = "child", definition = "explorer", state = "ready" } }, exchange = {} }
  local child_exchange = { id = "child-exchange", agent_id = "child", state = "running",
    turn = { { state = { kind = "running" }, provider = { thread_id = "child-thread", turn_id = "child-turn" } } } }
  state.timeline = { { kind = "agent_lifecycle", run = state.agent.run[1], exchange = { child_exchange }, agent = {} } }
  controller.cancel_turn()
  assert(child_request[1].method == "turn.cancel" and child_request[1].params.target.thread_id == "child-thread")
  assert(not state.cancel_requested, "settled child cancellation retained the parent cancellation flag")
  controller.steer_submit()
  assert(child_request[2].method == "turn.steer" and child_request[2].params.target.turn_id == "child-turn")
  state.cancel_requested = false
  child_exchange.state = "complete"
  controller.cancel_turn()
  assert(#child_request == 2, "completed-child cancellation fell through to the parent")
  client.request = original_plain_request
  state.presentation.close()
  state.presentation = nil
  state.composer_buf = vim.api.nvim_create_buf(false, true)
  state.composer_win = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(state.composer_win, state.composer_buf)
  state.selected_agent_run_id = nil
  state.busy = true
  state.queue = { "keep queued work" }
  for _, command in ipairs({ "pause", "clear" }) do
    vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "/goal " .. command })
    controller.submit()
    assert(goal_request[#goal_request] and goal_request[#goal_request].method == "goal." .. command,
      "goal control was queued behind active work")
    assert(#state.queue == 1 and state.queue[1] == "keep queued work")
    assert(state.busy, "goal control cleared the running turn state")
    goal_request[#goal_request].callback({ state = command == "pause" and "paused" or "cleared" })
    if command == "pause" then
      assert(state.goal and state.goal.state == "paused")
    else
      assert(state.goal == nil, "cleared goal survived the control acknowledgement")
    end
    assert(state.busy, "goal acknowledgement cleared the running turn state")
  end
  state.goal = { state = "active" }
  receive("goal_changed", { state = "cleared" }, "native-controller")
  assert(state.goal == nil, "cleared goal survived its lifecycle event")
end, debug.traceback)
if state.working_timer then state.working_timer:stop() state.working_timer:close() state.working_timer = nil end
client.request_for, client.subscribe, client.host_generation = original_request, original_subscribe, original_generation
client.host_accepting = original_accepting
package.loaded["blink.cmp"] = original_completion
assert(success, failure)
print("harness_controller_native: passed")
