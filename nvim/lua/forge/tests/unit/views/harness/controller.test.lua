-- Unit tests for `forge.views.harness.controller` (controller.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
local keymap = dofile("nvim/lua/forge/tests/support/keymap.lua")
local mock_client = dofile("nvim/lua/forge/tests/support/mock_client.lua")
local notify = dofile("nvim/lua/forge/tests/support/notify.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["harness_child_steering"] = function()
  local client = require("forge.client")
  local state = require("forge.session").harness
  local controller = require("forge.views.harness.controller")
  require("forge.views.harness.prompt_history").record = function() end
  require("forge.infra.notifications").warn = function() end
  require("forge.infra.notifications").error = function() end
  require("forge.infra.notifications").info = function() end
  controller.refresh_winbar = function() end
  controller.render = function() end
  controller.drain = function() end
  state.session = { id = "child-steering", model = "test" }
  state.composer_buf = buffer.scratch()
  dofile("nvim/lua/forge/tests/support/activity.lua").set_busy(state, true)
  state.capability = { native_steer = true }
  state.queue, state.pending_steer = {}, {}
  state.selected_agent_run_id = "child"
  local child = { id = "child", state = "ready", provider_thread_id = "thread" }
  local exchange = {
    agent_id = "child",
    state = "running",
    turn = { { state = "running", provider = { thread_id = "thread", turn_id = "turn" } } },
  }
  state.agent = { run = { child }, exchange = { exchange } }
  -- The host resolves the child's active provider turn and decides whether a message steers it or
  -- waits for its next input boundary, so the editor addresses only the selected child identity.
  local calls, other_calls, reply = {}, {}, nil
  client.request = function(request_specification, callback)
    local method = request_specification.method
    local params = request_specification.params or {}
    if method:match("^session%.agent%.") then
      calls[#calls + 1] = { method = method, params = params, session_id = request_specification.session_id }
      reply = callback
    else
      other_calls[#other_calls + 1] = method
    end
  end
  local function main_agent_requested()
    return vim.iter(other_calls):any(function(method)
      return method == "session.prompt.submit" or method == "session.input.apply" or method == "session.turn.steer"
    end)
  end
  buffer.set(state.composer_buf, { "review the boundary" })
  controller.steer_submit()
  assert(#calls == 1 and calls[1].method == "session.agent.message.send")
  assert(calls[1].session_id == "child-steering" and calls[1].params.target == "child")
  eq(calls[1].params.message, "review the boundary")
  -- The composer empties at submission, so a second submit before the reply sends nothing.
  eq(buffer.line(state.composer_buf, 1), "")
  controller.steer_submit()
  controller.submit()
  eq(#calls, 1)
  reply(nil, "turn already completed")
  assert(buffer.line(state.composer_buf, 1) == "review the boundary" and #state.queue == 0, "failed child steering became a new request")
  assert(not main_agent_requested(), "failed child steering fell back to the main agent")
  controller.submit()
  assert(#calls == 2 and calls[2].method == "session.agent.followup" and calls[2].params.target == "child")
  eq(buffer.line(state.composer_buf, 1), "")
  reply({ status = "admitted" })
  assert(buffer.line(state.composer_buf, 1) == "" and #state.queue == 0)
  buffer.set(state.composer_buf, { "rejected while typing" })
  controller.steer_submit()
  buffer.set(state.composer_buf, { "newer text" })
  reply(nil, "turn already completed")
  eq(buffer.line(state.composer_buf, 1), "newer text")
  buffer.set(state.composer_buf, { "another steer" })
  controller.steer_submit()
  assert(#calls == 4 and calls[4].params.message == "another steer")
  buffer.set(state.composer_buf, { "new draft while awaiting acknowledgement" })
  reply({ delivery = "steered" })
  assert(buffer.line(state.composer_buf, 1) == "new draft while awaiting acknowledgement", "acknowledgement erased newer input")
  state.selected_agent_run_id = "missing"
  controller.submit()
  assert(#calls == 5 and calls[5].params.target == "missing", "missing child input was not addressed to that child")
  assert(not main_agent_requested(), "missing child input fell back to the main agent")
end

T["new_session_commands_choose_tab_mode"] = function()
  local state = require("forge.session").harness
  local controller = require("forge.views.harness.controller")
  local calls = {}
  state.session = { id = "source", model = "test" }
  state.ready = true
  state.composer_buf = buffer.scratch()
  dofile("nvim/lua/forge/tests/support/activity.lua").set_busy(state, false)
  require("forge.views.harness").new_session = function(_, options)
    calls[#calls + 1] = options.open_mode
  end
  for _, command in ipairs({ "/clear", "/new" }) do
    buffer.set(state.composer_buf, { command })
    controller.submit()
    eq(buffer.line(state.composer_buf, 1), "")
  end
  eq(calls, { "current", "tab" })
end

T["config_opens_without_harness_windows"] = function()
  local session = require("forge.session")
  local state = session.harness
  local controller = require("forge.views.harness.controller")
  local defaults, openings = {}, {}
  local window = vim.api.nvim_get_current_win()
  local tab_count, window_count = #vim.api.nvim_list_tabpages(), #vim.api.nvim_list_wins()
  require("forge.client").session_initialize = function() error("configuration created a session") end
  require("forge.views.harness.global_settings").open = function(host)
    defaults[#defaults + 1] = host
  end
  require("forge.views.harness.settings").open = function(owner, host)
    openings[#openings + 1] = { owner = owner, host = host }
  end
  controller.open_config()
  eq(defaults[1].control_win, window)
  eq(#openings, 0)
  state.session = { id = "active-elsewhere" }
  controller.open_config()
  eq(#defaults, 2)
  state.composer_buf = vim.api.nvim_get_current_buf()
  state.composer_win = window
  controller.open_config()
  eq(#openings, 1)
  eq(openings[1].owner, state)
  eq(openings[1].host.window_list, { window })
  eq(openings[1].host.control_win, window)
  eq(#vim.api.nvim_list_tabpages(), tab_count)
  eq(#vim.api.nvim_list_wins(), window_count)
end

T["composer_admission"] = function()
  local client = require("forge.client")
  local state = require("forge.session").harness
  local controller = require("forge.views.harness.controller")
  local attachment = require("forge.views.harness.attachment")
  local input = require("forge.views.harness.input_state")
  local host = dofile("nvim/lua/forge/tests/support/input.lua").new(state)
  local warnings, requests, transitions = {}, {}, {}
  controller.refresh_winbar = function() end
  require("forge.infra.notifications").warn = function(message) warnings[#warnings + 1] = message end
  require("forge.infra.notifications").error = function(message) error(message) end
  client.request = function(request, callback)
    requests[#requests + 1] = request
    assert(request.session_id == "initialized-owner", "request escaped its session owner")
    if request.method == "session.prompt.history.record" then callback(nil) return end
    assert(host.request(request.method, request.params or {}, callback), request.method)
  end
  controller.task_transition = function(action)
    transitions[#transitions + 1] = action
  end
  state.composer_buf = buffer.scratch()
  attachment.attach(state.composer_buf)
  state.ready = false
  state.execution_notice = "retained startup notice"
  state.prompt_history = { { text = "older prompt", attachments = {} } }
  state.prompt_history_index = 1
  state.prompt_history_draft = { text = "retained navigation draft", attachments = {} }
  local selected_attachment = { id = "fixture-attachment", label = "[file]", name = "fixture.txt",
    media_type = "text/plain", start = 7, ["end"] = 13 }

  ---@param text string
  local function compose(text)
    buffer.set(state.composer_buf, { text })
    attachment.restore(state.composer_buf, text == "Review [file]" and { selected_attachment } or {})
  end

  -- Every public submission path must reject before even capturing or clearing the draft.
  for _, owner in ipairs({ false, {}, { id = "" } }) do
    state.session = owner or nil
    for _, submit in ipairs({ controller.submit, controller.queue_submit, controller.steer_submit }) do
      for _, text in ipairs({ "/allow", "Review [file]" }) do
        compose(text)
        local draft = attachment.capture(state.composer_buf)
        local history = vim.deepcopy(state.prompt_history)
        local navigation = vim.deepcopy(state.prompt_history_draft)
        for _ = 1, 2 do submit() end
        assert(vim.deep_equal(attachment.capture(state.composer_buf), draft), "early admission changed the draft")
        assert(vim.deep_equal(state.prompt_history, history), "early admission recorded history")
        assert(state.prompt_history_index == 1 and vim.deep_equal(state.prompt_history_draft, navigation),
          "early admission reset history navigation")
        assert(state.execution_notice == "retained startup notice", "early admission cleared startup state")
        assert(state.input == nil and #requests == 0 and #transitions == 0, "early admission sent session work")
      end
    end
  end
  eq(#warnings, 36)
  for _, warning in ipairs(warnings) do assert(warning == "Harness is still initializing") end

  -- A recorded startup failure replaces the "still initializing" warning with its cause.
  local startup_errors = {}
  local raise = require("forge.infra.notifications").error
  require("forge.infra.notifications").error = function(message) startup_errors[#startup_errors + 1] = message end
  state.session, state.start_failure = nil, "child continuation policy points to an unknown task"
  compose("/allow")
  controller.submit()
  assert(#warnings == 36 and #startup_errors == 1, "startup failure fell back to the initializing warning")
  assert(startup_errors[1]:find("Harness failed to start: child continuation policy", 1, true), startup_errors[1])
  state.start_failure = nil
  require("forge.infra.notifications").error = raise

  -- Ownership, rather than the legacy ready marker, releases the same draft for one explicit submission.
  state.session = { id = "initialized-owner", provider = "mock" }
  state.presentation = {
    capture_prompt = function()
      return { document = "composer", buffer = state.composer_buf, is_current = function() return true end }
    end,
  }
  input.attach(state, function() end)
  assert(#host.requests == 0, "initialization automatically submitted a retained draft")
  compose("/allow")
  controller.submit()
  assert(#transitions == 1 and transitions[1].change.mode == "allow")
  eq(state.prompt_history[1].text, "/allow")

  for _, submit in ipairs({ controller.submit, controller.queue_submit, controller.steer_submit }) do
    compose("Review [file]")
    local history_count, input_count = #state.prompt_history, #host.requests
    submit()
    assert(#state.prompt_history == history_count + 1, "resubmission did not record exactly one history entry")
    assert(#host.requests == input_count + 1, "resubmission did not send exactly one input")
    local message = host.requests[#host.requests].parameters.action.message
    assert(message.text == "Review [file]" and vim.deep_equal(message.attachments, { selected_attachment }))
    assert(attachment.capture(state.composer_buf).text == "", "accepted input did not clear composer")
    host.flush()
    submit()
    assert(#host.requests == input_count + 1 and #state.prompt_history == history_count + 1,
      "empty composer duplicated the accepted input")
  end
  assert(not state.ready, "fixture must exercise valid ownership with the legacy ready marker false")
end

T["continuation_sync"] = function()
  local client = require("forge.client")
  local session = require("forge.session")
  local controller = require("forge.views.harness.controller")
  local question = require("forge.views.harness.plan_question")
  local state = session.harness
  local activity_fixture = dofile("nvim/lua/forge/tests/support/activity.lua")
  local receive, host, continuation, state_reply, submitted
  local goal_requests, snapshot_requests = 0, 0
  local question_requests, question_presentations = nil, 0
  local queued = {}
  local input_revision = 0
  controller.render = function() end
  controller.refresh_winbar = function() end
  question.open = function(elicitation, value)
    question_presentations = question_presentations + 1
    host = value
    host.elicitation = elicitation
  end
  client.session_events_subscribe = function(callback) receive = callback return function() end end
  client.request = function(request_specification, callback)
    local method = request_specification.method
    local params = request_specification.params or {}

    if method == "session.question.continue" then continuation = callback
    elseif method == "session.state.get" then
      snapshot_requests = snapshot_requests + 1
      state_reply = callback
    elseif method == "session.command.apply" and params.action == "continue" then goal_requests = goal_requests + 1
    elseif method == "session.prompt.submit" then submitted = params.text
    elseif method == "session.prompt.history.record" then callback({})
    elseif method == "session.agent.start" then continuation = callback
    elseif method == "session.input.get" then
      callback({ revision = input_revision, messages = queued, suspended = false, execution_busy = false })
    elseif method == "session.input.apply" then
      assert(params.action.kind == "submit", "unexpected input operation")
      question_requests = function()
        input_revision = input_revision + 1
        callback({ operation_id = params.id, revision = input_revision, suspended = false,
          execution_busy = false, upsert = {}, removed = {} })
      end
    else error("unexpected request: " .. method) end
  end

  local function queue(text)
    queued = { { id = text, text = text, intent = "queue", delivery = "queued", revision = 1 } }
    input_revision = input_revision + 1
    require("forge.views.harness.input_state").refresh(state)
  end

  local function snapshot(goal, elicitation)
    return { session = state.session, goal = goal, active_elicitation = elicitation }
  end

  local function present(owner)
    state.active_elicitation = {
      owner = owner,
      elicitation = {
        question_set = { id = owner, questions = {
          { id = "choice", question = "Continue?", options = {} },
        } },
        answer = {}, current_index = 1,
      },
    }
    controller.present_plan_question(true)
    assert(host, "question host did not open")
    host.continue()
  end

  state.transcript_buf, state.transcript_win, state.composer_buf, state.composer_win =
    require("forge.views.harness.layout").open("continuation-sync")
  MiniTest.finally(function() pcall(require("forge.views.harness.workspace").release, state) end)
  state.session = { id = "continuation-sync", provider = "mock", model = "mock", approval_mode = "ask" }
  controller.attach()
  vim.api.nvim_set_current_win(state.composer_win)
  local focused_window = vim.api.nvim_get_current_win()
  -- Execution activity comes only from owner publications, never from a pending RPC.
  local owner_activity = vim.deepcopy(state.activity)
  present("plan_acceptance")
  local goal = { state = "active", native = false }
  receive("goal_continue_requested", goal, state.session.id)
  receive("plan_accepted", {}, state.session.id)
  vim.wait(20, function() return false end, 5)
  assert(goal_requests == 0, "goal advanced before the acceptance response")
  assert(vim.deep_equal(state.activity, owner_activity), "continuation request fabricated execution activity")
  continuation({})
  assert(not require("forge.views.harness.activity").busy(state))
  local before_stale_reply = snapshot_requests
  receive("plan_accepted", {}, state.session.id)
  state_reply(snapshot(goal, state.active_elicitation))
  wait_until(function() return snapshot_requests > before_stale_reply end,
    "coalesced invalidation did not request a fresh snapshot")
  assert(goal_requests == 0, "goal advanced using the consumed acceptance snapshot")
  state_reply(snapshot(goal))
  if state.state_sync_pending then state_reply(snapshot(goal)) end
  assert(goal_requests == 0 and not require("forge.views.harness.activity").busy(state), "snapshot dispatched a duplicate host-owned continuation")
  assert(vim.api.nvim_get_current_win() == focused_window, "continuation changed keyboard focus")

  activity_fixture.set_busy(state, false)
  state.goal = nil
  queue("queued after clarification")
  present("interaction")
  receive("question_answered", {}, state.session.id)
  continuation({})
  receive("question_answered", {}, state.session.id)
  local before_question_reply = snapshot_requests
  state_reply(snapshot(nil, state.active_elicitation))
  wait_until(function() return snapshot_requests > before_question_reply end,
    "answered clarification did not request a fresh snapshot")
  assert(submitted == nil, "queue drained using an outdated clarification snapshot")
  state_reply(snapshot())
  if state.state_sync_pending then state_reply(snapshot()) end
  assert(submitted == nil and state.queue[1].text == "queued after clarification",
    "clarification completion duplicated or lost host-owned input")
  assert(goal_requests == 0, "snapshot completion duplicated goal continuation")
  activity_fixture.set_busy(state, false)
  submitted = nil
  state.capability.agent = { catalog = true }
  local followed = false
  state.presentation = { follow_tail = function() followed = true end }
  controller.spawn_agent("explorer", "Inspect tests")
  assert(followed, "spawn did not resume tail following")
  state.presentation = nil
  queue("queued after child")
  state.cancel_requested = true
  continuation({})
  assert(submitted == nil, "spawn drained input before its fresh snapshot")
  state_reply(snapshot())
  if state.state_sync_pending then state_reply(snapshot()) end
  assert(submitted == nil and state.queue[1].text == "queued after child",
    "spawn completion duplicated or lost host-owned input")

  queued = {}
  input_revision = input_revision + 1
  require("forge.views.harness.input_state").refresh(state)
  activity_fixture.set_busy(state, false)
  state.queue, state.goal = {}, nil
  for _, owner in ipairs({ "plan", "interaction" }) do
    for _, outcome in ipairs({ "retained", "replaced", "withdrawn" }) do
      state.plan_question_open = false
      state.active_elicitation = {
        owner = owner, plan_id = owner == "plan" and "plan" or nil,
        exchange_id = owner == "interaction" and "exchange" or nil,
        elicitation = { revision = 1, current_index = 1,
          answer = { { question_id = "first", response = { kind = "selected", option = "Rust" } } },
          question_set = { id = "set", questions = {
            { id = "first", question = "Language?", options = {} },
            { id = "second", question = "Scope?", options = {} },
          } },
        },
      }
      controller.present_plan_question(true)
      local count = question_presentations
      host.ask({ question_id = "second", text = "What do you mean?" })
      controller.present_plan_question(true)
      assert(question_presentations == count and #state.pending_submit == 1, "picker reopened during explanation")
      local pending = vim.deepcopy(state.active_elicitation)
      if outcome == "replaced" then pending.elicitation.revision = 2 end
      if outcome == "withdrawn" then pending = nil end
      question_requests({})
      controller.present_plan_question(true)
      assert(question_presentations == count, "picker reopened before authoritative synchronization")
      state_reply(snapshot(nil, pending))
      vim.wait(30, function() return false end, 5)
      if pending then
        assert(question_presentations == count + 1, "pending question did not reopen after explanation")
        assert(vim.deep_equal(host.elicitation, pending.elicitation), "reopening lost answers or replacement")
        host.closed()
        assert(not state.plan_question_open)
        controller.present_plan_question(false)
        assert(question_presentations == count + 1, "dismissed question reopened without user action")
        controller.reopen_question()
        assert(question_presentations == count + 2, "reopen command did not restore pending question")
      else
        assert(question_presentations == count, "withdrawn question reopened")
      end
    end
  end
end

T["effort_selection"] = function()
  local state = require("forge.session").harness
  local controller = require("forge.views.harness.controller")
  local task = require("forge.views.harness.task")
  local keymaps = require("forge.shared.keymaps")
  local transition = {}
  local header = ""
  local header_segments = {}
  mock_client.respond({
    ["session.state.get"] = function(_, callback)
      callback({ session = state.session, capability = state.capability })
    end,
    ["session.configure"] = function(request_specification, callback)
      transition[#transition + 1] = { params = request_specification.params or {}, callback = callback }
    end,
  })
  local errors = notify.capture({ min_level = vim.log.levels.ERROR, notifications = true })
  require("forge.shared.keymaps").apply_view_winbar = function(_, _, _, _, segments)
    header_segments = segments
    header = table.concat(vim.tbl_map(function(segment) return segment.text end, segments))
  end
  controller.render, controller.attach_transcript = function() controller.refresh_winbar() end, function() end
  state.command_set = controller.command_set()
  state.transcript_win = vim.api.nvim_get_current_win()
  state.session = { id = "effort-test", provider = "fixture_alpha", model = "test", resolved_model = "test", effort = "medium" }
  state.capability = { effort_selection = true, fast_mode = true, ultrafast_mode = true }
  for _, tier in ipairs({ "default", "fast", "ultrafast" }) do
    state.session.service_tier = tier
    controller.refresh_winbar()
    local marker = ({ fast = "󱐌", ultrafast = "󱐋" })[tier]
    assert(header:find((marker and marker .. " " or "") .. "test medium", 1, true), header)
    for _, segment in ipairs(header_segments) do
      if segment.text == marker then
        eq(segment.group, (tier == "ultrafast" and "ForgeHarnessUltrafast" or "ForgeStatusLabel"))
      end
    end
    assert(not header:find("+", 1, true), "legacy speed marker remains")
    eq((header:find("󱐌", 1, true) ~= nil), (tier == "fast"))
    eq((header:find("󱐋", 1, true) ~= nil), (tier == "ultrafast"))
    assert(not header:find("unverified", 1, true) and not header:find("unsupported", 1, true), header)
  end
  -- Settings changes go straight to the host through `session.configure`; the header shows only
  -- the host-applied settings, so it changes when the session publishes the new values.
  dofile("nvim/lua/forge/tests/support/activity.lua").set_busy(state, false)
  state.session.model, state.session.resolved_model, state.session.effort = "test", "test", "medium"
  state.session.service_tier = nil
  state.model_provider, state.model_list = "fixture_alpha", { { id = "test", reasoning = { "minimal", "low", "medium", "high" }, service_tiers = { "fast", "ultrafast" } } }
  local function acknowledge(index)
    local request = transition[index]
    state.session = vim.tbl_extend("force", state.session, request.params)
    request.callback({})
  end
  local composer = buffer.scratch()
  keymaps.setup_view_keymaps(composer, "harness", state.command_set)
  local function press_effort(command_id, mode)
    local key = keymaps.view_keys_for("harness", command_id)[1]
    local mapping = keymap.get(key, { buffer = composer, mode = mode })
    assert(mapping.buffer == 1 and type(mapping.callback) == "function",
      command_id .. " must be mapped in " .. mode .. " mode")
    keymap.invoke(key, { buffer = composer, mode = mode })
  end
  for _, mode in ipairs({ "i", "n" }) do
    press_effort("effort_up", mode)
    assert(#transition == 1 and transition[1].params.effort == "high", vim.inspect(transition))
    acknowledge(1)
    press_effort("effort_down", mode)
    assert(#transition == 2 and transition[2].params.effort == "medium", vim.inspect(transition))
    acknowledge(2)
    transition = {}
  end
  press_effort("effort_down", "n")
  assert(#transition == 1 and transition[1].params.effort == "low")
  assert(header:find("test medium", 1, true), "header showed an unapplied effort: " .. header)
  acknowledge(1)
  controller.refresh_winbar()
  assert(header:find("test low", 1, true) and not header:find("*", 1, true), header)
  transition = {}

  controller.toggle_service_tier("fast")
  assert(#transition == 1 and transition[1].params.service_tier == "fast", vim.inspect(transition))
  acknowledge(1)
  controller.refresh_winbar()
  assert(header:find("󱐌 test low", 1, true), header)
  controller.toggle_service_tier("fast")
  assert(#transition == 2 and transition[2].params.service_tier == "default", "toggling the active tier must restore default")
  acknowledge(2)
  transition = {}

  local count = #transition
  state.capability.ultrafast_mode = false
  controller.toggle_service_tier("ultrafast")
  assert(state.configuration_error:find("does not support ultrafast", 1, true))
  assert(#transition == count, "an unsupported tier reached the host")
  errors:assert_none()
  local applied
  controller.configure({ effort = "unavailable" }, function(value) applied = value end)
  transition[#transition].callback(nil, "reasoning effort unavailable")
  assert(applied == false and state.session.effort == "low", "failed configuration replaced the applied setting")
  assert(state.configuration_error, "failed configuration was not reported")
  state.capability.ultrafast_mode = true
  state.model_list = { { id = "test", service_tiers = { "fast" } } }
  state.session.service_tier = "default"
  local before_invalid = #transition
  controller.configure_service_tier("ultrafast")
  assert(#transition == before_invalid and state.session.service_tier == "default")
  assert(errors[#errors].message:find("does not support ultrafast", 1, true))
  state.session.service_tier = "ultrafast"
  controller.refresh_winbar()
  assert(header:find("󱐋 test low", 1, true) and not header:find("unsupported ultrafast", 1, true), header)
  vim.api.nvim_buf_delete(composer, { force = true })
end

-- A task command empties the composer on submission rather than on the host's reply, which can
-- arrive only after a provider turn starts, and a rejection restores the draft unless newer
-- composer work replaced it.
T["task_command_clears_composer_before_reply"] = function()
  local state = require("forge.session").harness
  local controller = require("forge.views.harness.controller")
  require("forge.views.harness.prompt_history").record = function() end
  controller.render, controller.refresh_winbar = function() end, function() end
  state.session = { id = "task-composer", model = "test" }
  state.host_error, state.execution_notice = nil, nil
  state.queue, state.pending_steer, state.capability = {}, {}, {}
  state.composer_buf = buffer.scratch()
  local failures = notify.capture({ min_level = vim.log.levels.ERROR, notifications = true })
  local requests = mock_client.record()

  buffer.set(state.composer_buf, { "/goal Finish the port" })
  controller.submit()
  local accepted = requests[#requests]
  eq(accepted.spec.method, "session.command.apply")
  eq(buffer.text(state.composer_buf), "")
  accepted.callback({ state = "accepted" })
  eq(buffer.text(state.composer_buf), "")

  buffer.set(state.composer_buf, { "/plan Port the renderer" })
  controller.submit()
  local rejected = requests[#requests]
  eq(buffer.text(state.composer_buf), "")
  rejected.callback(nil, "task is already active", { code = "conflict" })
  eq(buffer.text(state.composer_buf), "/plan Port the renderer")
  eq(#failures, 1)

  controller.submit()
  local superseded = requests[#requests]
  buffer.set(state.composer_buf, { "newer draft" })
  superseded.callback(nil, "task is already active", { code = "conflict" })
  eq(buffer.text(state.composer_buf), "newer draft")
end

T["mode_restart"] = function()
  local state = require("forge.session").harness
  local controller = require("forge.views.harness.controller")
  local snapshot = require("forge.views.harness.snapshot")
  local activity = require("forge.views.harness.activity")
  local activity_fixture = dofile("nvim/lua/forge/tests/support/activity.lua")
  local requests, failures
  controller.render, controller.refresh_winbar, controller.attach_transcript = function() end, function() end, function() end
  vim.defer_fn = function() end
  local function reset()
    if state.working_timer then state.working_timer:stop() state.working_timer:close() end
    state.working_timer, state.working_started_ns = nil, nil
    state.session = { id = "permission-transition", approval_mode = "ask", active_task = { revision = "task", kind = "goal", phase = "work", status = "running", goal = { objective = "Fixture goal", state = "active" } } }

    state.runtime_epoch, state.snapshot_revision = "permission-owner", 1
    state.activity, state.no_checkpoint = nil, false
    activity_fixture.set_busy(state, true)
    state.execution_notice, state.host_error = nil, nil
    state.cancel_requested = false
    state.cleanup_operation_id, state.cleanup_epoch, state.cleanup_notice = nil, nil, nil
    state.sync_error, state.connection_error = nil, nil
    state.state_sync_pending, state.state_sync_again, state.state_sync_callback = nil, nil, nil
    state.queue, state.pending_steer, state.capability = {}, {}, {}
    state.approval, state.active_elicitation, state.selected_agent_run_id = {}, nil, nil
    state.status = nil
    state.configuring, state.configuration_debounce, state.pending_config = nil, nil, nil
    requests = mock_client.record()
    failures = notify.capture({ min_level = vim.log.levels.ERROR, notifications = true })
  end
  local function configured(revision, mode)
    local committed = vim.deepcopy(state.session)
    committed.approval_mode = mode
    return snapshot.apply(state, { session = committed, runtime_epoch = state.runtime_epoch,
      snapshot_revision = revision, activity = { revision = revision, phase = "working" } })
  end
  local function last(method)
    local request = requests[#requests]
    assert(request and request.spec.method == method, vim.inspect(request and request.spec))
    return request
  end
  for _, acknowledged_first in ipairs({ true, false }) do
    reset()
    controller.task_transition({ action = "permission", change = { kind = "enable_yolo" } })
    local change = last("session.command.apply")
    assert(change.spec.params.action == "permission" and change.spec.params.change.kind == "enable_yolo" and not change.spec.params.target)
    if acknowledged_first then change.callback({ state = "accepted", revision = 1 }) end
    assert(configured(2, "yolo"))
    if not acknowledged_first then change.callback({ state = "accepted", revision = 1 }) end
    assert(activity.busy(state) and state.session.approval_mode == "yolo",
      "configuration acknowledgement regressed the active exchange or committed settings")
    controller.cancel_turn()
    local cancellation = last("session.turn.cancel")
    assert(cancellation.spec.params.restore_prompt_if_no_output)
    controller.cancel_turn()
    local latest_cancellation = last("session.turn.cancel")
    assert(latest_cancellation.spec.params.operation_id ~= cancellation.spec.params.operation_id,
      "cancellation intents reused an operation identity")
    cancellation.callback({ cancel_requested = true })
    assert(state.cleanup_operation_id == latest_cancellation.spec.params.operation_id and activity.busy(state),
      "old cancellation reply cleared current cleanup or manufactured idle activity")
    latest_cancellation.callback({ cancel_requested = true })
    assert(not state.cleanup_operation_id and activity.busy(state), "cancellation reply manufactured idle activity")
    assert(activity.receive(state, { revision = 3, phase = "idle" }, state.runtime_epoch))
    assert(not activity.busy(state) and #requests == 3 and #failures == 0)
  end
  reset()
  local input_state = require("forge.views.harness.input_state")
  input_state.attach(state, function() end)
  last("session.input.get").callback({ revision = 10, control_revision = 4, suspended = false, messages = {} })
  requests = mock_client.record()
  controller.cancel_turn()
  local first_cancel = last("session.turn.cancel")
  assert(state.input.control_revision == 4 and not state.input.suspended,
    "cancellation predicted authoritative input state")
  input_state.submit(state, "immediate follow-up", {}, "auto")
  local first_submit = last("session.input.apply")
  assert(first_submit.spec.params.after_control == first_cancel.spec.params.operation_id and first_submit.spec.params.control_revision == 4,
    "immediate submit omitted its pending cancellation or predicted a control revision")
  controller.cancel_turn()
  local second_cancel = last("session.turn.cancel")
  first_cancel.callback({ cancel_requested = true })
  assert(state.input.pause_operation_id == second_cancel.spec.params.operation_id
    and state.cleanup_operation_id == second_cancel.spec.params.operation_id,
    "older cancellation reply cleared the newer cancellation correlation")
  input_state.submit(state, "latest follow-up", {}, "auto")
  local second_submit = last("session.input.apply")
  assert(second_submit.spec.params.after_control == second_cancel.spec.params.operation_id and second_submit.spec.params.control_revision == 4,
    "submit did not follow the latest pending cancellation")
  assert(#requests == 4 and #failures == 0, "cancellation submission introduced polling or errors")
  state.input = nil
  reset()
  controller.task_transition({ action = "permission", change = { kind = "set_approval", mode = "ask" } })
  local previous = last("session.command.apply")
  controller.task_transition({ action = "permission", change = { kind = "set_approval", mode = "ask" } })
  local latest = last("session.command.apply")
  assert(previous.spec.params.operation_id ~= latest.spec.params.operation_id)
  assert(configured(3, "deny"))
  previous.callback({ state = "completed" })
  assert(not configured(2, "ask") and state.session.approval_mode == "deny" and activity.busy(state),
    "stale configuration replaced the committed settings or active exchange")
  latest.callback(nil, "cleanup did not settle", { code = "request_failed" })
  assert(activity.busy(state) and state.execution_notice == "cleanup did not settle" and #failures == 1)
  assert(#requests == 2, "command replies initiated view-owned recovery")
  for _, mode in ipairs({ "n", "i" }) do
    reset()
    local composer = buffer.scratch()
    vim.api.nvim_set_current_buf(composer)
    require("forge.shared.keymaps").setup_view_keymaps(composer, "harness", controller.composer_command_set())
    keymap.invoke("<S-Tab>", { mode = mode })
    eq(last("session.command.apply").spec.params.change.kind, "toggle_write_enabled")
    assert(state.session.approval_mode == "ask", "Shift-Tab changed unconfirmed approval state")
    vim.api.nvim_buf_delete(composer, { force = true })
  end
  reset()
  state.host_error, state.execution_notice = "host stopped", "Reopen Harness"
  state.queue = { { text = "keep queued", task_id = "task" } }
  controller.cancel_turn()
  controller.render()
  assert(#requests == 0 and #state.queue == 1 and not activity.busy(state))
end

T["queued_model"] = function()
  local client = require("forge.client")
  local state = require("forge.session").harness
  local controller = require("forge.views.harness.controller")
  local input = require("forge.views.harness.input_state")
  local host = dofile("nvim/lua/forge/tests/support/input.lua").new(state)
  local warnings = {}
  controller.render = function() end
  controller.refresh_winbar = function() end
  require("forge.views.harness.prompt_history").record = function() end
  require("forge.infra.notifications").warn = function(message) warnings[#warnings + 1] = message end
  require("forge.infra.notifications").error = function(message) error(message) end
  client.request = function(request_specification, callback)
    local method = request_specification.method
    local parameters = request_specification.params or {}

    assert(host.request(method, parameters, callback), method)
  end
  state.session = { id = "queued-model", provider = "mock", model = "old" }
  state.capability = { model_selection = true, effort_selection = true }
  state.model_list = {
    { id = "chosen", reasoning = { "xhigh" } },
    { id = "another", reasoning = { "high" } },
    { id = "settings-only", reasoning = { "high" } },
    { id = "immediate", reasoning = { "high" } },
  }
  state.composer_buf = buffer.scratch()
  state.presentation = { follow_tail = function() end }
  state.queue = {}
  input.attach(state, function() end)
  buffer.set(state.composer_buf, "/model chosen xhigh explain this\nand this")
  controller.queue_submit()
  assert(state.session.model == "old" and #state.queue == 1, "queueing changed active model settings")
  local queued = state.queue[1]
  assert(queued.config.model == "chosen" and queued.config.effort == "xhigh")
  eq(queued.text, "explain this\nand this")
  host.flush()
  controller.edit_queued(-1)
  buffer.set(state.composer_buf, "/model another impossible never send")
  controller.submit()
  assert(#warnings == 1 and state.queue_edit, "invalid effort committed the edit")
  eq(queued.config.model, "chosen")
  buffer.set(state.composer_buf, "/model another high revised prompt")
  controller.submit()
  assert(queued.config.model == "chosen" and queued.text == "explain this\nand this")
  assert(state.session.model == "old", "pending input edit changed provider configuration")
  host.flush()
  assert(queued.config.model == "another" and queued.config.effort == "high")
  eq(queued.text, "revised prompt")
  local configured
  controller.configure = function(config)
    configured = config
  end
  dofile("nvim/lua/forge/tests/support/activity.lua").set_busy(state, true)
  state.execution_busy = true
  buffer.set(state.composer_buf, "/model settings-only high")
  controller.queue_submit()
  assert(#state.queue == 2 and configured == nil, "queueing a model-only selection configured the active turn")
  local settings = state.queue[2]
  assert(settings.text == "" and settings.config.model == "settings-only" and settings.config.effort == "high")
  assert(settings.intent == "queue" and state.session.model == "old" and state.task_config == nil)
  assert(buffer.line(state.composer_buf, 1) == "", "queued command stayed in the composer")
  host.flush()
  assert(state.queue[2] == settings and configured == nil, "queue acknowledgement configured the active turn")
  local editor = require("forge.views.harness.queue_editor")
  assert(editor.staging(state)[2].text == "/model settings-only high", "queued configuration was invisible")
  controller.edit_queued(-1)
  assert(buffer.line(state.composer_buf, 1) == "/model settings-only high",
    "editing lost the queued model command")
  controller.submit()
  host.flush()
  assert(state.queue[2] == settings and settings.config.model == "settings-only" and settings.text == "",
    "saving a configuration-only entry removed it")
  assert(configured == nil and state.session.model == "old", "editing configured the active turn")
  buffer.set(state.composer_buf, "/model immediate high")
  controller.submit()
  assert(#state.queue == 2 and configured.model == "immediate", "steering a model change did not configure the active turn")
  eq(#warnings, 1)
end

T["input_render_burst"] = function()
  local client = require("forge.client")
  local state = require("forge.session").harness
  local controller = require("forge.views.harness.controller")
  local requests = mock_client.record()
  local receive, renders = nil, 0
  client.session_events_subscribe = function(callback) receive = callback return function() end end
  controller.render = function() renders = renders + 1 end
  controller.attach_transcript = function() end
  require("forge.views.harness.workspace").attach = function() end
  require("forge.views.harness.prompt_history").attach = function() end
  state.session = { id = "render-burst", provider = "mock" }
  state.composer_buf, state.transcript_buf = buffer.scratch(), buffer.scratch()
  controller.attach()
  requests[1].callback({ revision = 0, messages = {}, suspended = false })
  for revision = 1, 100 do
    receive("input_changed", { revision = revision, upsert = {
      { id = "queued", text = tostring(revision), revision = revision, delivery = "queued" },
    }, removed = {} }, state.session.id)
  end
  -- Queue correctness cannot wait for the scheduled presentation work.
  eq(state.queue[1].text, "100")
  eq(renders, 0)
  wait_until(function() return renders > 0 end, "input burst was never rendered")
  eq(renders, 1)
end

T["unavailable_queue_edit_submit"] = function()
  local state = require("forge.session").harness
  local controller = require("forge.views.harness.controller")
  local input = require("forge.views.harness.input_state")
  local requests = mock_client.record()
  local warnings = {}
  controller.refresh_winbar = function() end
  require("forge.views.harness.prompt_history").record = function() end
  require("forge.infra.notifications").warn = function(message) warnings[#warnings + 1] = message end
  state.session = { id = "missing-edit", provider = "mock" }
  state.composer_buf = buffer.scratch()
  state.presentation = {
    follow_tail = function() end,
    capture_prompt = function()
      return { buffer = state.composer_buf, document = "composer", is_current = function() return true end }
    end,
  }
  state.queue = {}
  input.attach(state, function() end)
  requests[1].callback({ revision = 1, messages = {
    { id = "queued", revision = 1, text = "original", delivery = "queued", attachments = {} },
  }, suspended = false })
  controller.edit_queued(-1)
  input.receive(state, { revision = 2, removed = { "queued" }, upsert = {} })
  buffer.set(state.composer_buf, "new prompt")
  controller.submit()
  eq(state.queue_edit, nil)
  eq(buffer.text(state.composer_buf), "new prompt")
  eq(#warnings, 1)
  eq(requests[#requests].spec.params.action.kind, "release")
  controller.submit()
  eq(requests[#requests].spec.params.action.kind, "submit")
  eq(requests[#requests].spec.params.action.message.text, "new prompt")
  eq(buffer.text(state.composer_buf), "")
end

T["session_name"] = function()
  local client = require("forge.client")
  local controller = require("forge.views.harness.controller")
  local state = require("forge.session").harness
  local refreshes = 0
  local generation = 1
  client.host_get_generation = function() return generation end
  local requests = mock_client.record()
  local errors = notify.capture({ min_level = vim.log.levels.ERROR, notifications = true })
  controller.refresh_winbar = function() refreshes = refreshes + 1 end
  state.session = { id = "session", name = "Existing name", model = "selected", effort = "high" }
  dofile("nvim/lua/forge/tests/support/activity.lua").set_busy(state, true)
  -- The host owns configuration and Lua keeps no unconfirmed selection, so name generation uses
  -- the session's applied model.
  controller.rename_session("")
  assert(requests[1].spec.params.operation == "session_name" and requests[1].spec.params.model == "selected",
    vim.inspect(requests[1]))
  eq(state.rename_status, "Generating session name…")
  assert(state.session.name == "Existing name" and state.session.effort == "high" and require("forge.views.harness.activity").busy(state))
  requests[1].callback({ text = "Terminal status fixes" })
  eq(state.rename_status, "Generating session name…")
  assert(requests[2].spec.method == "session.rename" and requests[2].spec.params.expected_name == "Existing name")
  requests[2].callback({ name = "Terminal status fixes", model = "stale model" })
  assert(state.session.name == "Terminal status fixes" and state.session.model == "selected")
  assert(refreshes == 2 and #errors == 0 and state.rename_status == nil)

  controller.rename_session("")
  local generated = requests[#requests]
  controller.rename_session("Manual name")
  local manual = requests[#requests]
  manual.callback({ name = "Manual name" })
  local count = #requests
  generated.callback({ text = "Stale name" })
  assert(#requests == count and state.session.name == "Manual name")

  controller.rename_session("")
  local older = requests[#requests]
  controller.rename_session("")
  older.callback({ text = "Older generated name" })
  eq(requests[#requests].spec.params.operation, "session_name")
  assert(state.rename_status == "Generating session name…", "stale generation cleared newer progress")
  requests[#requests].callback(nil, "generation failed")
  assert(errors[1].message == "generation failed" and state.session.name == "Manual name")
  assert(state.rename_status == nil, "failed generation retained its status")
  controller.rename_session("")
  requests[#requests].callback({ text = "  " })
  assert(#errors == 2 and state.session.name == "Manual name")

  controller.rename_session("")
  local before_restart = requests[#requests]
  generation = 2
  count = #requests
  before_restart.callback({ text = "Late reply" })
  assert(#requests == count and state.rename_status == nil)
  controller.rename_session("")
  local other_session = requests[#requests]
  state.session = { id = "other", name = "Other session", model = "default", resolved_model = "private-alias" }
  other_session.callback({ text = "Wrong session" })
  eq(state.session.name, "Other session")
  controller.rename_session("Must fail visibly")
  local storage_failure = "SQLite contention prevented this operation (configured lock wait: 5 seconds)"
  requests[#requests].callback(nil, storage_failure)
  assert(errors[#errors].message == storage_failure, "storage error did not reach the Neovim notification")
  assert(state.session.name == "Other session", "failed storage write changed the displayed name")
  controller.rename_session("")
  assert(requests[#requests].spec.session_id == "other" and requests[#requests].spec.params.model == "default")
  state.composer_buf = buffer.scratch()
  require("forge.views.harness.prompt_history").record = function() end
  buffer.set(state.composer_buf, { "/rename" })
  controller.submit()
  assert(requests[#requests].spec.params.operation == "session_name", "bare /rename bypassed generation")
  eq(buffer.line(state.composer_buf, 1), "")
  buffer.set(state.composer_buf, { "/rename Explicit title" })
  controller.submit()
  assert(requests[#requests].spec.method == "session.rename" and requests[#requests].spec.params.name == "Explicit title")
  local tree = require("forge.render.harness.interaction_tree").build({
    { kind = "session_event", id = "rename", event = { kind = "renamed", name = "Old title" } },
    { kind = "session_event", id = "fork", event = { kind = "forked", source_session_id = "source" } },
  })
  assert(#tree.lines == 1 and tree.lines[1]:find("Forked from source", 1, true),
    "rename confirmation survived in cached Lua timeline rendering")
end

T["submit_steering"] = function()
  local client = require("forge.client")
  local state = require("forge.session").harness
  local controller = require("forge.views.harness.controller")
  local input = require("forge.views.harness.input_state")
  local host = dofile("nvim/lua/forge/tests/support/input.lua").new(state)
  local controls, transitions = {}, {}
  require("forge.views.harness.prompt_history").record = function() end
  controller.render, controller.refresh_winbar = function() end, function() end
  state.composer_buf = buffer.scratch()
  state.session = { id = "steering-task" }
  state.presentation = { capture_prompt = function()
    return { buffer = state.composer_buf, document = "composer", is_current = function() return true end }
  end }
  controller.task_transition = function(action, _, completed)
    transitions[#transitions + 1] = { action = action, completed = completed }
  end
  client.request = function(request, callback)
    assert(request.session_id == "steering-task", "submission changed its session owner")
    if host.request(request.method, request.params, callback) then return end
    controls[#controls + 1] = { request = request, callback = callback }
  end
  input.attach(state, function() end)
  local function submitted(message, intent, submit)
    local before = #host.requests
    buffer.set(state.composer_buf, { message })
    submit()
    assert(#host.requests == before + 1, "composer message was not admitted")
    local request = host.requests[#host.requests].parameters.action
    assert(request.kind == "submit" and request.message.text == message and request.message.intent == intent)
    host.flush()
  end

  for _, scenario in ipairs({
    { busy = true, native_steer = true },
    { busy = true, native_steer = false },
    { busy = false, configuring = true },
    { busy = true, task_operation = { action = "configure", state = "accepted" } },
    { busy = true, status = { kind = "waiting_for_agent", agent_count = 2 } },
  }) do
    dofile("nvim/lua/forge/tests/support/activity.lua").set_busy(state, scenario.busy)
    state.configuring = scenario.configuring
    state.task_operation, state.status = scenario.task_operation, scenario.status
    state.capability = { native_steer = scenario.native_steer }
    submitted("Follow up through server admission", "auto", controller.submit)
    submitted("Explicit follow-up", "auto", controller.steer_submit)
    submitted("Keep this queued", "queue", controller.queue_submit)
  end
  local clarification_target = { owner = "plan", id = "selected-plan", set_id = "questions", revision = 3, question_id = "q2" }
  input.submit(state, "Explain the selected question", {}, "auto", nil, nil, nil, clarification_target)
  local scoped_submission = host.requests[#host.requests].parameters
  assert(vim.deep_equal(scoped_submission.target, clarification_target), "clarification target was dropped")
  assert(scoped_submission.action.message.purpose.kind == "user", "UI manufactured an internal input purpose")
  host.flush()

  state.task_operation = nil
  for _, command in ipairs({ { "/plan Convert to X", "plan", "Convert to X" },
    { "/goal Complete X", "goal", "Complete X" } }) do
    local before = #host.requests
    buffer.set(state.composer_buf, { command[1] })
    controller.submit()
    local transition = transitions[#transitions].action
    assert(transition.action == command[2] and transition.text == command[3])
    assert(#host.requests == before, "task command became a provider steering message")
  end

  state.pending_steer = { { text = "Pending steering" } }
  local retained = vim.deepcopy(state.queue)
  local before = #host.requests
  buffer.set(state.composer_buf, { "Keep my unsent draft" })
  controller.cancel_turn()
  local transition_count = #transitions
  local cancellation = controls[#controls]
  assert(cancellation.request.method == "session.turn.cancel"
    and cancellation.request.params.restore_prompt_if_no_output == true)
  assert(buffer.line(state.composer_buf, 1) == "Keep my unsent draft" and #host.requests == before
    and vim.deep_equal(state.queue, retained))
  -- The session owner joins a cancellation issued during cleanup with the one already collecting,
  -- so Lua forwards it under its own identity instead of predicting host execution state.
  local control_count = #controls
  controller.cancel_turn()
  local repeated = controls[#controls]
  assert(#transitions == transition_count and #controls == control_count + 1
    and repeated.request.method == "session.turn.cancel",
    "cancellation during cleanup was not forwarded to the session owner")
  assert(repeated.request.params.operation_id ~= cancellation.request.params.operation_id,
    "repeated cancellation reused the first cancellation identity")
  assert(buffer.line(state.composer_buf, 1) == "Keep my unsent draft" and #host.requests == before and vim.deep_equal(state.queue, retained))
  cancellation.callback({ cancel_requested = true })
  repeated.callback({ cancel_requested = true })
  assert(buffer.line(state.composer_buf, 1) == "Keep my unsent draft" and vim.deep_equal(state.queue, retained))

  local child_action
  controller.agent_action = function(action, child_id) child_action = { action = action, id = child_id } end
  state.selected_agent_run_id = "selected-child"
  controller.cancel_turn()
  assert(child_action.action == "interrupt" and child_action.id == "selected-child")
  assert(#transitions == transition_count and #host.requests == before and buffer.line(state.composer_buf, 1) == "Keep my unsent draft")
end

T["undo_requires_native_conversation_rewind"] = function()
  local client = require("forge.client")
  local state = require("forge.session").harness
  local controller = require("forge.views.harness.controller")
  local warnings = {}
  require("forge.infra.notifications").warn = function(message) warnings[#warnings + 1] = message end
  require("forge.infra.notifications").error = function(message) error(message) end
  state.session = { id = "undo-capability", model = "test" }
  local methods = {}
  client.request = function(request_specification, callback)
    methods[#methods + 1] = request_specification.method
    if request_specification.method == "session.exchange.recovery" then callback(vim.NIL, nil) end
  end
  -- A provider without native rewind gets a notice and no rollback request.
  state.capability = { native_conversation_rewind = false }
  controller.open_undo_picker()
  eq(methods, { "session.exchange.recovery" })
  eq(#warnings, 1)
  assert(warnings[1]:find("unsupported", 1, true), warnings[1])
  -- A rewinding provider proceeds to the interaction list.
  methods, warnings = {}, {}
  state.capability = { native_conversation_rewind = true }
  controller.open_undo_picker()
  eq(methods, { "session.exchange.recovery", "session.exchange.list" })
  eq(warnings, {})
end

---Stub the collaborators a state read reaches after it applies a snapshot.
local function quiet_state_read(controller)
  controller.render, controller.refresh_winbar, controller.attach_transcript =
    function() end, function() end, function() end
  require("forge.views.harness.agent_picker").refresh = function() end
end

T["state_sync_retry"] = function()
  local state = require("forge.session").harness
  local controller = require("forge.views.harness.controller")
  local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
  quiet_state_read(controller)
  notify.capture({ min_level = vim.log.levels.ERROR, notifications = true })
  local requests = mock_client.record()
  local timers = {}
  stub(vim, "defer_fn", function(callback, delay) timers[#timers + 1] = { callback = callback, delay = delay } end)
  state.session = { id = "retried" }
  local function reads()
    return vim.tbl_filter(function(request) return request.spec.method == "session.state.get" end, requests)
  end
  controller.synchronize_state()
  for attempt, delay in ipairs({ 1000, 2000, 4000 }) do
    reads()[attempt].callback(nil, "storage busy", { code = "storage" })
    assert(state.sync_error, "a failed read was not shown")
    eq(#timers, attempt)
    eq(timers[attempt].delay, delay)
    timers[attempt].callback()
    eq(#reads(), attempt + 1)
  end
  reads()[4].callback(nil, "storage busy", { code = "storage" })
  eq(#timers, 3)
  -- A later read that succeeds clears the failure and starts a fresh sequence for the next one.
  controller.synchronize_state()
  reads()[5].callback({ session = { id = "retried" } })
  assert(not state.sync_error and state.state_sync_retry == nil)
  controller.synchronize_state()
  reads()[6].callback(nil, "storage busy", { code = "storage" })
  eq(timers[4].delay, 1000)
  -- A retry superseded by a successful read issues nothing.
  controller.synchronize_state()
  reads()[7].callback({ session = { id = "retried" } })
  timers[4].callback()
  eq(#reads(), 7)
end

T["state_sync_keyed_by_session"] = function()
  local state = require("forge.session").harness
  local controller = require("forge.views.harness.controller")
  quiet_state_read(controller)
  local requests = mock_client.record()
  state.session = { id = "first" }
  local settled = {}
  controller.synchronize_state(function(result, failure) settled[#settled + 1] = { result = result, failure = failure } end)
  local first = requests[#requests]
  -- An in-place switch issues a read for the new session instead of waiting on the old one.
  state.session = { id = "second" }
  controller.synchronize_state()
  local second = requests[#requests]
  assert(second ~= first and second.spec.session_id == "second", "the new session's read was suppressed")
  first.callback({ session = { id = "first" } })
  eq(state.session.id, "second")
  second.callback({ session = { id = "second" }, timeline_revision = 4 })
  eq(state.timeline_revision, 4)
  eq(#settled, 1)
  assert(settled[1].result and not settled[1].failure)
  -- A switch with no later read still reconciles the session the view now shows.
  controller.synchronize_state()
  local stale = requests[#requests]
  state.session = { id = "third" }
  stale.callback({ session = { id = "second" } })
  wait_until(function() return requests[#requests].spec.session_id == "third" end,
    "the shown session was never read after an in-place switch")
end

T["document_change_does_not_invalidate_reads"] = function()
  local client = require("forge.client")
  local state = require("forge.session").harness
  local controller = require("forge.views.harness.controller")
  local requests = mock_client.record()
  local receive
  client.session_events_subscribe = function(callback) receive = callback return function() end end
  quiet_state_read(controller)
  require("forge.views.harness.workspace").attach = function() end
  require("forge.views.harness.prompt_history").attach = function() end
  state.session = { id = "streaming", provider = "mock" }
  state.runtime_epoch, state.status = "current-epoch", { kind = "working" }
  state.composer_buf, state.transcript_buf = buffer.scratch(), buffer.scratch()
  controller.attach()
  controller.synchronize_state()
  local read = requests[#requests]
  eq(read.spec.method, "session.state.get")
  receive("document_changed", { session_id = "streaming", revision = 3 }, "streaming")
  eq(state.timeline_revision, 3)
  local count = #requests
  read.callback({ session = state.session, runtime_epoch = "current-epoch", timeline_revision = 3 })
  vim.wait(20, function() return #requests > count end, 5)
  eq(#requests, count)
  -- A patch from an earlier epoch cannot raise the floor for the current one.
  receive("document_changed", { session_id = "streaming", revision = 900, runtime_epoch = "previous-epoch" }, "streaming")
  eq(state.timeline_revision, 3)
end

T["approval_waits_for_a_window"] = function()
  local state = require("forge.session").harness
  local controller = require("forge.views.harness.controller")
  local approval = require("forge.views.harness.approval")
  controller.refresh_winbar = function() end
  local opened = 0
  approval.open = function() opened = opened + 1 end
  state.session = { id = "closed-view" }
  state.approval = { { request_id = "late", execution_kind = "managed", targets = {} } }
  state.transcript_win, state.composer_win = nil, nil
  controller.present_approval()
  assert(opened == 0 and not state.approval_open and not state.presented_approval_id,
    "an approval without a window blocked its later presentation")
  state.transcript_win = vim.api.nvim_get_current_win()
  approval.open = function() error("picker host failed") end
  local failures = notify.capture({ min_level = vim.log.levels.ERROR, notifications = true })
  controller.present_approval()
  assert(#failures == 1 and not state.approval_open, "a failed presentation kept the approval marked open")
  approval.open = function() opened = opened + 1 end
  controller.present_approval()
  assert(opened == 1 and state.approval_open and state.presented_approval_id == "late")
end

T["approval_returns_after_replacement"] = function()
  local state = require("forge.session").harness
  local controller = require("forge.views.harness.controller")
  local approval = require("forge.views.harness.approval")
  local picker = require("forge.views.picker")
  controller.refresh_winbar = function() end
  local window = vim.api.nvim_get_current_win()
  state.session = { id = "displaced" }
  state.transcript_win = window
  state.approval = { { request_id = "displaced", execution_kind = "managed", targets = { {
    target_id = "command", title = "Run command", detail = "whoami", commands = {},
    choices = { { choice_id = "allow_once", label = "Allow once" }, { choice_id = "deny_once", label = "Deny once" } },
  } } } }
  local requests = mock_client.record()
  controller.present_approval()
  assert(approval.is_open())
  -- No decision starts selected, so an Enter already in flight confirms nothing.
  local instance = picker._state_for_test()
  eq(require("forge.views.picker.state").selected_option(instance.state, instance.spec), nil)
  keymap.invoke("<CR>")
  assert(approval.is_open() and #requests == 0, "an unchosen approval was submitted")
  picker.open({ owner = "late-list", host = { window_list = { window }, control_win = window },
    page_list = { { option_list = { { label = "Late option" } } } } })
  assert(not state.approval_open and state.dismissed_approval_id == nil,
    "replacement by another picker was recorded as a user dismissal")
  picker.close(false)
  wait_until(approval.is_open, "the displaced approval was not presented again")
  keymap.invoke("q")
  assert(not approval.is_open() and state.dismissed_approval_id == "displaced", "q did not dismiss the approval")
end

T["rollback_respects_newer_composer_text"] = function()
  local client = require("forge.client")
  local state = require("forge.session").harness
  local controller = require("forge.views.harness.controller")
  local picker = require("forge.views.picker")
  controller.render, controller.refresh_winbar = function() end, function() end
  local warnings = {}
  require("forge.infra.notifications").warn = function(message) warnings[#warnings + 1] = message end
  local interaction = { id = "exchange", ordinal = 1, state = "complete", disposition = "current", prompt = "Original prompt" }
  client.request = function(request_specification, callback)
    if request_specification.method == "session.exchange.recovery" then callback(vim.NIL)
    elseif request_specification.method == "session.exchange.list" then callback({ interaction }) end
  end
  local specs = {}
  picker.open = function(spec) specs[#specs + 1] = spec end
  local completions = {}
  controller.task_transition = function(_, _, completed) completions[#completions + 1] = completed end
  state.session = { id = "rollback" }
  state.capability = { native_conversation_rewind = true }
  dofile("nvim/lua/forge/tests/support/activity.lua").set_busy(state, false)
  vim.cmd("split")
  state.composer_win = vim.api.nvim_get_current_win()
  state.composer_buf = buffer.scratch()
  vim.api.nvim_win_set_buf(state.composer_win, state.composer_buf)
  vim.cmd("wincmd p")
  local other_window = vim.api.nvim_get_current_win()
  local function roll_back()
    controller.open_undo_picker()
    specs[#specs].on_confirm({ option = { value = interaction } })
    specs[#specs].on_confirm({ option = { value = "conversation" } })
    return completions[#completions]
  end
  local completed = roll_back()
  buffer.set(state.composer_buf, { "newer typing" })
  completed(true)
  vim.wait(20, function() return false end, 5)
  eq(buffer.line(state.composer_buf, 1), "newer typing")
  eq(#warnings, 1)
  eq(vim.api.nvim_get_current_win(), other_window)
  buffer.set(state.composer_buf, { "" })
  completed = roll_back()
  completed(true)
  vim.wait(20, function() return false end, 5)
  eq(buffer.line(state.composer_buf, 1), "Original prompt")
  eq(vim.api.nvim_get_current_win(), other_window)
end

T["select_agent_reverts_on_failure"] = function()
  local state = require("forge.session").harness
  local controller = require("forge.views.harness.controller")
  controller.render = function() end
  state.agent = { run = { { id = "child", definition = "explorer" } }, exchange = {} }
  local settle
  state.presentation = { select_agent = function(_, callback) settle = callback end }
  controller.select_agent("child")
  eq(state.selected_agent_run_id, "child")
  settle("timeline unavailable")
  eq(state.selected_agent_run_id, nil)
  controller.select_agent("child")
  settle(nil)
  eq(state.selected_agent_run_id, "child")
end

T["preview_close_renders_its_view"] = function()
  local state = require("forge.session").harness
  local controller = require("forge.views.harness.controller")
  local preview = require("forge.views.harness.session_preview")
  local rendered = 0
  controller.render = function() rendered = rendered + 1 end
  state.transcript_buf = buffer.scratch()
  state.transcript_win = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(state.transcript_win, state.transcript_buf)
  preview.open(state.transcript_win)
  preview.close()
  eq(rendered, 1)
  eq(vim.api.nvim_win_get_buf(state.transcript_win), state.transcript_buf)
end

T["with_state_restores_after_failure"] = function()
  local session = require("forge.session")
  local controller = require("forge.views.harness.controller")
  local selected, background = session.harness, session.new_harness_state()
  MiniTest.expect.error(function()
    controller.with_state(background, function()
      assert(session.harness == background)
      error("render failed")
    end)
  end, "render failed")
  assert(session.harness == selected, "a failed background render left its state active")
end

T["clarification_settles_on_failed_read"] = function()
  local state = require("forge.session").harness
  local controller = require("forge.views.harness.controller")
  local input = require("forge.views.harness.input_state")
  local question = require("forge.views.harness.plan_question")
  quiet_state_read(controller)
  require("forge.views.harness.prompt_history").record = function() end
  dofile("nvim/lua/forge/tests/support/stub.lua")(vim, "defer_fn", function() end)
  notify.capture({ min_level = vim.log.levels.ERROR, notifications = true })
  local requests = mock_client.record()
  local host
  question.open = function(_, value) host = value end
  state.session = { id = "clarification" }
  state.transcript_win = vim.api.nvim_get_current_win()
  dofile("nvim/lua/forge/tests/support/activity.lua").set_busy(state, false)
  input.attach(state, function() end)
  requests[#requests].callback({ revision = 0, suspended = false, messages = {} })
  state.active_elicitation = { owner = "plan", plan_id = "plan", elicitation = { revision = 1, current_index = 0, answer = {},
    question_set = { id = "set", questions = { { id = "scope", question = "Scope?", options = {} } } } } }
  controller.present_plan_question(true)
  host.ask({ question_id = "scope", text = "What does scope cover?" })
  assert(state.question_clarification, "the clarification did not suppress the question")
  local submit = requests[#requests]
  eq(submit.spec.method, "session.input.apply")
  submit.callback({ operation_id = submit.spec.params.id, revision = 1, suspended = false, upsert = {}, removed = {} })
  local read = requests[#requests]
  eq(read.spec.method, "session.state.get")
  read.callback(nil, "host stopped")
  eq(state.question_clarification, nil)
end

-- `/btw <question>` forks the current session as an aside and sends the question to the fork, not
-- to the session the user typed it in.
T["btw_forks_an_aside_and_sends_its_question"] = function()
  local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
  local state = require("forge.session").harness
  local controller = require("forge.views.harness.controller")
  local navigation = require("forge.views.harness.session_navigation")
  stub(require("forge.views.harness.prompt_history"), "record", function() end)
  local warnings = notify.capture({ min_level = vim.log.levels.WARN, notifications = true })
  state.session = { id = "origin", name = "main" }
  state.capability = { native_fork = true }
  state.composer_buf = buffer.scratch()
  state.composer_win = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(state.composer_win, state.composer_buf)
  -- A view binds its input to a session when the fork result activates it; a closed tab never does.
  local aside_state, tab_open = nil, true
  stub(navigation, "begin_fork", function()
    aside_state = { input = {} }
    return { state = aside_state }
  end)
  stub(navigation, "complete_fork", function(pending, result)
    if tab_open then pending.state.input.session_id = result.session.id end
  end)
  local submitted = {}
  stub(require("forge.views.harness.input_state"), "submit", function(target_state, text, attachments, intent)
    submitted[#submitted + 1] = { state = target_state, text = text, attachments = attachments, intent = intent }
  end)
  local requests = mock_client.record()

  buffer.set(state.composer_buf, { "/btw why did the build fail?" })
  controller.submit()
  eq(#requests, 1)
  eq(requests[1].spec.method, "session.fork")
  eq(requests[1].spec.session_id, "origin")
  eq(requests[1].spec.params, { session_id = "origin", aside = true })
  eq(buffer.line(state.composer_buf, 1), "")
  eq(submitted, {})
  local opened_state = aside_state
  requests[1].callback({ session = { id = "aside" } })
  eq(submitted, { { state = opened_state, text = "why did the build fail?", attachments = {}, intent = "auto" } })
  eq(warnings, {})

  -- The fork answers after its tab closed: no view can queue the question, so the user is told.
  tab_open = false
  buffer.set(state.composer_buf, { "/btw and the tests?" })
  controller.submit()
  requests[2].callback({ session = { id = "aside-closed" } })
  eq(#submitted, 1)
  eq(#warnings, 1)
  assert(warnings[1].message:find("and the tests?", 1, true), warnings[1].message)

  -- A bare command asks nothing, so it forks nothing and returns to the composer for the question.
  buffer.set(state.composer_buf, { "/btw" })
  controller.submit()
  eq(#requests, 2)
  eq(buffer.line(state.composer_buf, 1), "/btw ")
  eq(#warnings, 2)

  -- An ordinary fork is not an aside and sends nothing on the user's behalf.
  tab_open = true
  buffer.set(state.composer_buf, { "/fork" })
  controller.submit()
  eq(requests[3].spec.params, { session_id = "origin" })
  requests[3].callback({ session = { id = "plain-fork" } })
  eq(#submitted, 1)
end

return T
