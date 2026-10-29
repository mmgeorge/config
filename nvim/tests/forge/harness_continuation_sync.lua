vim.loader.enable(false)
local client = require("forge.client")
local session = require("forge.session")
local controller = require("forge.views.harness.controller")
local question = require("forge.views.harness.plan_question")
local state = session.harness
local request_for, subscribe = client.request, client.session_events_subscribe
local render, refresh_winbar, open = controller.render, controller.refresh_winbar, question.open
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
  elseif method == "session.question.ask" then question_requests = callback
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

local success, failure = xpcall(function()
  state.transcript_buf, state.transcript_win, state.composer_buf, state.composer_win =
    require("forge.views.harness.layout").open("continuation-sync")
  state.session = { id = "continuation-sync", backend = "mock", model = "mock", approval_mode = "ask" }
  controller.attach()
  vim.api.nvim_set_current_win(state.composer_win)
  local focused_window = vim.api.nvim_get_current_win()
  present("plan_acceptance")
  local goal = { state = "active", native = false }
  receive("goal_continue_requested", goal, state.session.id)
  receive("plan_accepted", {}, state.session.id)
  vim.wait(20, function() return false end)
  assert(goal_requests == 0 and state.busy, "goal advanced before the acceptance response")
  continuation({})
  assert(not state.busy)
  local before_stale_reply = snapshot_requests
  receive("plan_accepted", {}, state.session.id)
  state_reply(snapshot(goal, state.active_elicitation))
  assert(vim.wait(1000, function() return snapshot_requests > before_stale_reply end),
    "coalesced invalidation did not request a fresh snapshot")
  assert(goal_requests == 0, "goal advanced using the consumed acceptance snapshot")
  state_reply(snapshot(goal))
  if state.state_sync_pending then state_reply(snapshot(goal)) end
  assert(goal_requests == 0 and not state.busy, "snapshot dispatched a duplicate host-owned continuation")
  assert(vim.api.nvim_get_current_win() == focused_window, "continuation changed keyboard focus")

  state.busy, state.goal = false, nil
  queue("queued after clarification")
  present("interaction")
  receive("question_answered", {}, state.session.id)
  continuation({})
  receive("question_answered", {}, state.session.id)
  local before_question_reply = snapshot_requests
  state_reply(snapshot(nil, state.active_elicitation))
  assert(vim.wait(1000, function() return snapshot_requests > before_question_reply end))
  assert(submitted == nil, "queue drained using an outdated clarification snapshot")
  state_reply(snapshot())
  if state.state_sync_pending then state_reply(snapshot()) end
  assert(submitted == nil and state.queue[1].text == "queued after clarification",
    "clarification completion duplicated or lost host-owned input")
  assert(goal_requests == 0, "snapshot completion duplicated goal continuation")
  state.busy, submitted = false, nil
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
  state.busy, state.queue, state.goal = false, {}, nil
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
      assert(question_presentations == count and #state.input.pending == 1, "picker reopened during explanation")
      local pending = vim.deepcopy(state.active_elicitation)
      if outcome == "replaced" then pending.elicitation.revision = 2 end
      if outcome == "withdrawn" then pending = nil end
      question_requests({})
      controller.present_plan_question(true)
      assert(question_presentations == count, "picker reopened before authoritative synchronization")
      state_reply(snapshot(nil, pending))
      vim.wait(30, function() return false end)
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
end, debug.traceback)

if state.working_timer then state.working_timer:stop() state.working_timer:close() end
if state.unsubscribe then state.unsubscribe() end
require("forge.views.harness.workspace").release(state)
client.request, client.session_events_subscribe = request_for, subscribe
controller.render, controller.refresh_winbar, question.open = render, refresh_winbar, open
assert(success, failure)
print("harness_continuation_sync: passed")
