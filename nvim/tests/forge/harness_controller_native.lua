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
local generation, submitted, receive, navigated, sync_count = 1, nil, nil, nil, 0
local initial = {}
local goal_request = {}
local status_thought_request = {}
local timeline_source = { "Native transcript" }
local staged_response, staged_request
local hold_staging, staging_reply, staging_count, fail_selection = false, nil, 0, false
local submitted_callback
local function snapshot(document, text, composer)
  return { document = document, revision = 0, block = { { id = "body", text = type(text) == "table" and text or { text },
    metadata = { target = {}, decoration = {}, editable_region = composer and { { id = "composer", revision = 0,
      range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = #text } } } } or {} } } } }
end
client.host_generation = function() return generation end
client.subscribe = function(callback) receive = callback return function() end end
client.request_for = function(session_id, method, params, callback)
  assert(session_id == "native-controller")
  if method == "prompt.submit" then submitted = params submitted_callback = callback return end
  if method == "session.configure" then
    assert(type(params.status_thoughts) == "boolean")
    status_thought_request[#status_thought_request + 1] = { params = params, callback = callback }
    return
  end
  if method == "task.transition" then
    goal_request[#goal_request + 1] = { method = method, params = params, callback = callback }
    return
  end
  if method == "health.get" then callback({ responsive = true }) return end
  if method == "state.get" then callback({ session = state.session }) return end
  if method == "history.record" then vim.schedule(function() callback({}) end) return end
  if method == "backend.models" then callback({}) return end
  assert(method == "harness.document")
  if params.operation == "open" then
    initial[#initial + 1] = params
    vim.schedule(function() callback({ transcript = snapshot(params.document, "Native transcript") }) end)
  elseif params.operation == "navigate_prompt" then
    navigated = params.input
    vim.schedule(function() callback({ anchor = { block = "body", position = { row = 0, column = 0 } } }) end)
  elseif params.operation == "staging" then
    staging_count = staging_count + 1
    staged_request = vim.deepcopy(params)
    staged_response = snapshot(params.document, timeline_source)
    staged_response.revision = state.presentation.transcript.revision + 1
    local lines = {}
    if #params.steering > 0 then
      lines[#lines + 1] = "Steering active turn"
      vim.list_extend(lines, params.steering)
    end
    if #params.queue > 0 then
      lines[#lines + 1] = "Queued follow-up inputs"
      vim.list_extend(lines, params.queue)
    end
    if #lines > 0 then
      staged_response.block[#staged_response.block + 1] = { id = "harness:staging", text = lines,
        metadata = { target = {}, decoration = {}, editable_region = {} } }
    end
    if hold_staging then staging_reply = callback
    else vim.schedule(function() callback({ updated = true }) end) end
  elseif params.operation == "select_agent" and fail_selection then
    fail_selection = false
    vim.schedule(function() callback(nil, "selection unavailable") end)
  elseif params.operation == "sync" then
    sync_count = sync_count + 1
    local result = staged_response and { snapshot = staged_response } or { patch = {} }
    staged_response = nil
    vim.schedule(function() callback(result) end)
  else vim.schedule(function() callback({ patch = {} }) end) end
end
local success, failure = xpcall(function()
  state.transcript_buf, state.transcript_win, state.composer_buf, state.composer_win, state.timeline_tab = require("forge.views.harness.layout").open("native-controller")
  state.session = { id = "native-controller", backend = "mock", provider_label = "Mock CLI", model = "mock", execution_mode = "read" }
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "native draft" })
  controller.attach()
  controller.render()
  assert(vim.wait(1000, function() return state.presentation and state.presentation.ready end))
  local null_snapshot = vim.json.decode([[{
    "goal": null, "goal_execution": null, "active_plan": null,
    "active_elicitation": null, "active_wait": null,
    "approval": [], "artifact": [], "timeline": [], "prompt_history": null
  }]])
  null_snapshot.session = state.session
  controller.activate_snapshot(null_snapshot)
  assert(state.goal == nil and state.active_elicitation == nil, "startup retained nullable state")
  local original_state_request = client.request_for
  local synchronized = false
  client.request_for = function(_, method, _, callback)
    assert(method == "state.get")
    callback(null_snapshot)
    synchronized = true
  end
  receive("plan_changes_requested", {}, "native-controller")
  assert(synchronized and not state.state_sync_pending, "reject snapshot synchronization did not settle")
  assert(state.goal == nil and state.active_elicitation == nil, "reject snapshot retained nullable state")
  client.request_for = original_state_request
  assert(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)[1] == "Native transcript")
  local approval_view = require("forge.views.harness.approval")
  local permission = { id = "navigation", item_list = { {
    id = "command", title = "Run command", detail = "whoami.exe /groups", command_list = {},
    choice_list = { { id = "allow_once", label = "Allow once" }, { id = "cancel", label = "Reject all" } },
  } } }
  local pending_snapshot
  local snapshot_request = client.request_for
  local function approval_snapshot(pending)
    return { session = state.session, approval = pending, status = { kind = "working" } }
  end
  client.request_for = function(session_id, method, params, callback)
    if method == "state.get" then pending_snapshot = callback return end
    return snapshot_request(session_id, method, params, callback)
  end
  receive("state_invalidated", {}, "native-controller")
  local stale_snapshot = pending_snapshot
  receive("backend_event", { kind = "approval_requested", data = permission }, "native-controller")
  assert(vim.wait(1000, approval_view.is_open), "approval did not open")
  stale_snapshot(approval_snapshot({}))
  assert(#state.approval == 1 and pending_snapshot ~= stale_snapshot,
    "a snapshot captured before approval erased the pending request")
  pending_snapshot(approval_snapshot({ permission }))
  vim.fn.maparg("q", "n", false, true).callback()
  assert(not approval_view.is_open() and #state.approval == 1, "q must only dismiss the approval window")
  vim.api.nvim_set_current_win(state.transcript_win)
  local timeline_window = vim.api.nvim_get_current_win()
  receive("state_invalidated", {}, "native-controller")
  pending_snapshot(approval_snapshot({ permission }))
  vim.wait(30, function() return false end)
  assert(not approval_view.is_open() and vim.api.nvim_get_current_win() == timeline_window,
    "snapshot refresh reopened a dismissed approval and stole timeline focus")
  local notice = require("forge.views.harness.health").notice(state)
  assert(notice.text == "Waiting for your approval" and notice.waiting and notice.hint == "permission")
  controller.reopen_question()
  assert(approval_view.is_open(), "explicit reopen must restore the pending approval")
  vim.fn.maparg("q", "n", false, true).callback()
  receive("backend_event", { kind = "approval_cancelled", data = { id = permission.id } }, "native-controller")
  assert(#state.approval == 0 and not state.dismissed_approval_id)
  client.request_for = snapshot_request
  assert(vim.wo[state.transcript_win].breakindent, "native attachment discarded Harness continuation indentation")
  assert(vim.wo[state.composer_win].winbar == "", "composer still displays key hints")
  state.session.name = "Parser 100% coverage"
  controller.refresh_winbar()
  local title = vim.api.nvim_eval_statusline(vim.wo[state.transcript_win].winbar,
    { use_winbar = true, maxwidth = 200 }).str
  assert(title:find("Read • mock", 1, true), "winbar lost execution mode or model")
  assert(not title:find("Mock CLI", 1, true), "winbar still shows the CLI provider")
  assert(not title:find("Parser 100% coverage", 1, true), "session name remains in top bar")
  assert(title:find(" • sandbox • ", 1, true), "sandbox indicator missing from top bar")
  state.session.access = { sandbox = false }
  controller.refresh_winbar()
  title = vim.api.nvim_eval_statusline(vim.wo[state.transcript_win].winbar,
    { use_winbar = true, maxwidth = 200 }).str
  assert(not title:find("sandbox", 1, true), "disabled sandbox remains in top bar")
  local tab_title = vim.api.nvim_eval_statusline(vim.o.tabline,
    { use_tabline = true, maxwidth = 200 }).str
  assert(tab_title:find("Parser 100% coverage", 1, true), "session name missing from tab")
  state.session.name = "123456789012345678901234567890 extra"
  controller.refresh_winbar()
  tab_title = vim.api.nvim_eval_statusline(vim.o.tabline,
    { use_tabline = true, maxwidth = 200 }).str
  assert(tab_title:find("123456789012345678901234567890", 1, true) and not tab_title:find("extra", 1, true),
    "tab must show only the first 30 session-name characters")
  state.busy = true
  local long_timeline = { "Native transcript" }
  for index = 1, vim.api.nvim_win_get_height(state.transcript_win) + 80 do
    long_timeline[#long_timeline + 1] = "Earlier response " .. index
  end
  local transcript = state.presentation.transcript
  timeline_source = long_timeline
  assert(require("forge.buffer").apply_snapshot(transcript, {
    document = transcript.document, revision = transcript.revision + 1,
    block = { { id = "body", text = long_timeline,
      metadata = { target = {}, decoration = {}, editable_region = {} } } },
  }).kind == "Applied")
  vim.api.nvim_set_current_win(state.composer_win)
  state.presentation.follow_tail()
  state.queue = { { text = "queued follow-up" } }
  for index = 1, vim.api.nvim_win_get_height(state.transcript_win) + 5 do
    state.queue[#state.queue + 1] = { text = "queued extra " .. index }
  end
  state.pending_steer = { { text = "steering update" } }
  hold_staging = true
  controller.refresh_winbar()
  assert(vim.wait(1000, function() return staging_reply ~= nil end))
  local composer_height = vim.api.nvim_win_get_height(state.composer_win)
  local queue_namespace = vim.api.nvim_create_namespace("ForgeHarnessQueue")
  assert(#vim.api.nvim_buf_get_extmarks(state.composer_buf, queue_namespace, 0, -1, {}) == 0,
    "staging remained in the editable composer")
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "typing draft" })
  state.queue[#state.queue].text = "latest queued update"
  controller.refresh_winbar()
  assert(staging_count == 1, "staging changes were not coalesced during an active request")
  fail_selection = true
  state.presentation.select_agent("missing-child")
  hold_staging = false
  staging_reply({ updated = true })
  assert(vim.api.nvim_win_get_height(state.composer_win) == composer_height,
    "refreshing staging resized the composer")
  assert(vim.wait(1000, function()
    return vim.api.nvim_buf_get_lines(state.transcript_buf, -2, -1, false)[1] == state.queue[#state.queue].text
  end), "timeline staging is missing")
  assert(staged_request.queue[1] == "queued follow-up" and staged_request.steering[1] == "steering update",
    "staging request lost its messages")
  assert(staging_count == 2, "selection failure did not flush the latest staged messages")
  local timeline_view = vim.api.nvim_win_call(state.transcript_win, vim.fn.winsaveview)
  local visible_height = vim.api.nvim_win_get_height(state.transcript_win)
    - (vim.wo[state.transcript_win].winbar ~= "" and 1 or 0)
  assert(vim.api.nvim_win_text_height(state.transcript_win, { start_row = timeline_view.topline - 1 }).all <= visible_height,
    "queue-only refresh left staging below the viewport")
  vim.api.nvim_set_current_win(state.transcript_win)
  vim.api.nvim_win_set_cursor(state.transcript_win, { 1, 0 })
  controller.refresh_winbar()
  assert(vim.api.nvim_win_get_cursor(state.transcript_win)[1] == 1,
    "staging refresh displaced intentional transcript browsing")
  state.queue, state.pending_steer = {}, {}
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "native draft" })
  controller.refresh_winbar()
  assert(vim.wait(1000, function()
    return vim.api.nvim_buf_line_count(state.transcript_buf) == #timeline_source
  end), "cleared messages remained staged")
  controller.configure({ status_thoughts = true })
  controller.configure({ status_thoughts = false })
  assert(#status_thought_request == 2 and #goal_request == 0,
    "status thought configuration entered the task-transition path")
  status_thought_request[2].callback({ status_thoughts = false })
  status_thought_request[1].callback({ status_thoughts = true })
  assert(state.status_thoughts == false and state.busy,
    "a superseded status thought response changed the preference or interrupted work")
  state.session.model, state.session.resolved_model = "default", nil
  receive("backend_event", { kind = "runtime_resolved", data = {
    session_id = "native-controller", provider = "Mock CLI", model = "resolved-model",
  } }, "native-controller")
  title = vim.api.nvim_eval_statusline(vim.wo[state.transcript_win].winbar,
    { use_winbar = true, maxwidth = 200 }).str
  assert(title:find("resolved-model", 1, true) and not title:find("resolving model", 1, true),
    "running session did not publish the resolved model immediately")
  receive("backend_event", { kind = "runtime_resolved", data = {
    session_id = "other-session", model = "wrong-model",
  } }, "native-controller")
  assert(state.session.resolved_model == "resolved-model", "foreign runtime replaced the active model")
  state.session.model, state.session.resolved_model = "mock", nil
  state.recap = { text = "Old recap" }
  receive("backend_event", { kind = "turn_started" }, "native-controller")
  assert(state.recap == nil and state.busy, "provider continuation retained recap or changed busy state")
  controller.refresh_winbar()
  assert(vim.wo[state.composer_win].winbar == "", "busy composer restored key hints")
  state.goal = { state = "paused", created_at_ms = 1000 }
  receive("backend_event", { kind = "execution_state", data = {
    session = { id = "native-controller", backend = "mock", model = "mock", execution_mode = "write", current_task_id = "execution" },
    task = { { id = "execution", kind = "execute", phase = "implement", status = "running" } },
    goal = { state = "active", created_at_ms = 1000 },
    goal_execution = { state = "active", created_at_ms = 1000, scheduler = { task = { { state = "active" } } } },
  } }, "native-controller")
  assert(state.busy and state.goal.state == "active" and state.session.execution_mode == "write")
  assert(vim.wo[state.transcript_win].winbar:find("execute · implement", 1, true))
  assert(not vim.wo[state.transcript_win].winbar:find("running", 1, true))
  receive("backend_event", { kind = "execution_state", data = {
    session = state.session, goal = vim.NIL, goal_execution = vim.NIL,
  } }, "native-controller")
  assert(state.goal == nil and state.goal_execution == nil, "absent execution state retained stale goal labels")
  state.busy = false
  state.session.execution_mode = "read"
  for _, kind in ipairs({ "plan", "execute", "goal" }) do
    for _, status in ipairs({ "paused", "blocked", "failed", "waiting" }) do
      state.task = { { id = "execution", kind = kind, phase = "review", status = status } }
      controller.refresh_winbar()
      assert(vim.wo[state.transcript_win].winbar:find(kind .. " · review · " .. status, 1, true),
        "durable task lifecycle disappeared from the winbar")
    end
  end
  state.goal, state.goal_execution, state.task = nil, nil, {}
  state.session.current_task_id = nil
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
  assert(submitted and submitted.submission and submitted.text == "native draft")
  assert(vim.wait(2500, function() return sync_count > sync_before_submission end, 20),
    "busy Harness did not refresh the native transcript status")
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "native draft", "controller cleared before native admission")
  receive("backend_event", { kind = "prompt_submission", data = { document = state.presentation.document,
    token = submitted.submission.token, state = "accepted" } }, "native-controller")
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "")
  local original_state_request = client.request_for
  local recovered_state = false
  client.request_for = function(_, method)
    assert(method == "state.get")
    recovered_state = true
  end
  submitted_callback(nil, "simulated provider failure")
  assert(not state.busy and recovered_state, "provider failure did not reconcile execution state")
  state.state_sync_pending = false
  client.request_for = original_state_request
  vim.api.nvim_buf_set_text(state.composer_buf, 0, 0, 0, 0, { "draft after host collection" })
  generation = 2
  controller.render()
  assert(vim.wait(1000, function() return state.presentation and state.presentation.ready and state.presentation.host_generation == 2 end))
  assert(initial[2].initial == nil, "new host received an unsent draft")
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "draft after host collection")
  submitted = nil
  state.busy = false
  state.queue = { "queued while finalizing" }
  state.status = { kind = "finalizing", exchange_id = "exchange-1", error = "cleanup pending" }
  controller.drain()
  assert(#state.queue == 1 and submitted == nil, "finalizing status drained queued input")
  state.status = { kind = "idle" }
  assert(state.execution_notice, "provider failure lost its persistent notice")
  state.execution_notice = nil
  controller.drain()
  assert(#state.queue == 0 and submitted ~= nil, "idle status did not resume queued input")
  local original_plain_request = client.request
  local child_request_for = client.request_for
  client.request_for = function(session_id, method, params, callback)
    if method == "state.get" then return end
    return child_request_for(session_id,method,params,callback)
  end
  local child_request = {}
  client.request = function(method, params, callback)
    if method == "history.record" or method == "state.get" then return end
    child_request[#child_request + 1] = { method = method, params = params }
    if method == "agent.interrupt" then callback({ status = "interrupting" }) end
  end
  state.busy = true
  state.capability.native_steer = true
  state.selected_agent_run_id = "child"
  state.agent = { run = { { id = "child", definition = "explorer", state = "ready" } }, exchange = {} }
  local child_exchange = { id = "child-exchange", agent_id = "child", state = "running",
    turn = { { state = { kind = "running" }, provider = { thread_id = "child-thread", turn_id = "child-turn" } } } }
  state.timeline = { { kind = "agent_lifecycle", run = state.agent.run[1], exchange = { child_exchange }, agent = {} } }
  state.agent.summary = { child = { state = "running", target = { thread_id = "child-thread", turn_id = "child-turn" } } }
  controller.cancel_turn()
  assert(child_request[1].method == "agent.interrupt" and child_request[1].params.target == "child")
  assert(not state.cancel_requested, "settled child cancellation retained the parent cancellation flag")
  controller.steer_submit()
  assert(child_request[2].method == "agent.message" and child_request[2].params.target == "child")
  state.cancel_requested = false
  child_exchange.state = "complete"
  state.agent.summary.child = { state = "complete" }
  controller.cancel_turn()
  assert(child_request[#child_request].method == "agent.interrupt" and child_request[#child_request].params.target == "child",
    "completed-child interruption must remain scoped to the child")
  state.agent.run[1].task_name = "parser-review"
  state.agent.run[1].created_at_ms = os.time() * 1000
  state.agent.summary.child = { state = "running", tool_count = 1, failed_count = 0 }
  controller.open_agent_picker()
  local picker = require("forge.views.picker")
  local picker_state = require("forge.views.picker.state")
  local picker_instance = picker._state_for_test()
  assert(picker_state.selected_option(picker_instance.state,picker_instance.spec).value.run.id == "child")
  local updated_agent = vim.deepcopy(state.agent)
  updated_agent.summary.child = { state = "running", tool_count = 7, failed_count = 1 }
  receive("backend_event", {kind="agent_updated",data=updated_agent},"native-controller")
  picker_instance = picker._state_for_test()
  local selected = picker_state.selected_option(picker_instance.state,picker_instance.spec)
  assert(selected.value.run.id == "child" and selected.detail:find("7 tools called (1 failed)",1,true),
    "live agent events must refresh metrics without changing selection or polling state")
  picker.close(true)
  local child_permission = vim.deepcopy(permission)
  child_permission.id = "child-approval"
  child_permission.provider = "Codex"
  child_permission.execution_owner = {session_id="native-controller",agent_id="child"}
  state.selected_agent_run_id = "different-child"
  receive("backend_event", {kind="approval_requested",data=child_permission},"native-controller")
  assert(vim.wait(1000,approval_view.is_open))
  picker_instance = picker._state_for_test()
  local approval_page = picker_state.page(picker_instance.state,picker_instance.spec)
  assert(approval_page.content_list[1].text == "Agent parser-review · Codex",
    "approval must identify its requesting child")
  local interrupt_key = require("forge.shared.keymaps").view_keys_for("harness","cancel")[1]
  vim.fn.maparg(interrupt_key,"n",false,true).callback()
  assert(child_request[#child_request].params.target == "child",
    "approval interruption must ignore the selected timeline and target its owner")
  receive("backend_event", {kind="approval_cancelled",data={id=child_permission.id}},"native-controller")
  local original_transition = controller.task_transition
  local root_interruption
  controller.task_transition = function(action) root_interruption=action end
  local root_permission = vim.deepcopy(permission)
  root_permission.id = "root-approval"
  root_permission.execution_owner = {session_id="native-controller",agent_id="native-controller:agent:primary"}
  receive("backend_event", {kind="approval_requested",data=root_permission},"native-controller")
  assert(vim.wait(1000,approval_view.is_open))
  vim.fn.maparg(interrupt_key,"n",false,true).callback()
  assert(root_interruption and root_interruption.action == "pause",
    "root approval interruption must remain root-owned while a child is selected")
  controller.task_transition = original_transition
  receive("backend_event", {kind="approval_cancelled",data={id=root_permission.id}},"native-controller")
  client.request = original_plain_request
  client.request_for = child_request_for
  state.busy, state.task_operation, state.task_config = true, nil, nil
  state.subagent_tools = false
  for _, enabled in ipairs({true,false}) do
    local applied = nil
    controller.configure({subagent_tools=enabled},false,function(result)
      assert(state.subagent_tools == enabled,"configuration callback observed stale subagent tools")
      applied = result
    end)
    local operation_id = state.task_operation.id
    receive("session_configured", {session=state.session,operation_id="stale-configuration"},"native-controller")
    assert(applied == nil and state.subagent_tools ~= enabled,"unrelated operation applied pending tools")
    receive("session_configured", {session=state.session,operation_id=operation_id},"native-controller")
    assert(applied == true and state.subagent_tools == enabled,"active configuration did not adopt tools")
    state.task_operation = nil
  end
  require("forge.views.harness.workspace").release(state)
  state.presentation.close()
  state.presentation = nil
  state.composer_buf = vim.api.nvim_create_buf(false, true)
  state.composer_win = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(state.composer_win, state.composer_buf)
  state.busy, state.active_wait, state.selected_agent_run_id = true, {}, "child"
  state.queue = {}
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "explicit follow-up" })
  require("forge.shared.keymaps").setup_view_keymaps(state.composer_buf, "harness", controller.command_set())
  vim.fn.maparg("<C-q>", "i", false, true).callback()
  assert(state.queue[1].text == "explicit follow-up", "queue key steered an active wait or selected child")
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "")
  assert(vim.fn.maparg("<C-s>", "i", false, true).desc == "Submit the composer")
  state.active_wait = nil
  state.selected_agent_run_id = nil
  state.busy = true
  state.queue = { "keep queued work" }
  for _, command in ipairs({ "pause", "clear" }) do
    vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "/goal " .. command })
    controller.submit()
    assert(goal_request[#goal_request] and goal_request[#goal_request].params.action == command,
      "goal control was queued behind active work")
    assert(#state.queue == 1 and state.queue[1] == "keep queued work")
    assert(state.busy, "goal control cleared the running turn state")
    goal_request[#goal_request].callback({ state = "accepted" })
    assert(state.busy, "goal acknowledgement cleared the running turn state")
  end
  state.goal = { state = "active" }
  receive("goal_changed", { state = "cleared" }, "native-controller")
  assert(state.goal == nil, "cleared goal survived its lifecycle event")
  local profiles = require("forge.views.harness.subagent_profiles")
  local original_profile_open, original_spawn, original_spawn_picker = profiles.open, controller.spawn_agent, controller.open_spawn_picker
  local original_agents, original_select = controller.open_agent_picker, controller.select_agent
  local original_configure = controller.configure
  local routed = {}
  profiles.open = function(selected_state, _, profile) assert(selected_state == state) routed = { "edit", profile } end
  controller.spawn_agent = function(profile, text) routed = { "spawn", profile, text } end
  controller.open_spawn_picker = function(profile) routed = { "spawn_picker", profile } end
  controller.open_agent_picker = function() routed = { "agents" } end
  controller.select_agent = function(name) routed = { "select", name } end
  controller.configure = function(params, validate)
    assert(vim.deep_equal(params, { subagent_tools = params.subagent_tools }))
    routed = { "tools", params.subagent_tools, validate }
  end
  state.selected_agent_run_id = "child"
  for _, command in ipairs({
    { "/agents on", { "tools", true, true } },
    { "/agents on", { "tools", true, true } },
    { "/agents off", { "tools", false, true } },
    { "/agents off", { "tools", false, true } },
    { "/agents edit", { "edit" } },
    { "/agents edit reviewer", { "edit", "reviewer" } },
    { "/agents spawn reviewer Inspect parser", { "spawn", "reviewer", "Inspect parser" } },
    { "/agents spawn reviewer", { "spawn_picker", "reviewer" } },
    { "/agents spawn", { "spawn_picker" } },
    { "/agents", { "agents" } },
    { "/agents main", { "select", "main" } },
  }) do
    vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { command[1] })
    controller.submit()
    assert(vim.deep_equal(routed, command[2]), "incorrect route for " .. command[1])
    assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "")
    assert(#state.queue == 1 and state.queue[1] == "keep queued work", "agent command was queued as a prompt")
  end
  for _, text in ipairs({ "/agent edit", "/agent edit reviewer" }) do
    routed = {}
    vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { text })
    controller.submit()
    assert(vim.tbl_isempty(routed), "singular agent command must not route")
    assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == text)
    assert(#state.queue == 1 and state.queue[1] == "keep queued work", "singular agent command was queued")
  end
  profiles.open, controller.spawn_agent, controller.open_spawn_picker = original_profile_open, original_spawn, original_spawn_picker
  controller.open_agent_picker, controller.select_agent = original_agents, original_select
  controller.configure = original_configure
end, debug.traceback)
if state.working_timer then state.working_timer:stop() state.working_timer:close() state.working_timer = nil end
client.request_for, client.subscribe, client.host_generation = original_request, original_subscribe, original_generation
client.host_accepting = original_accepting
package.loaded["blink.cmp"] = original_completion
if not success then
  print(failure)
  vim.cmd("cquit 1")
end
print("harness_controller_native: passed")
