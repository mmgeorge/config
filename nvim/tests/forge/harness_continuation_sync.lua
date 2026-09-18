vim.loader.enable(false)
local client = require("forge.client")
local session = require("forge.session")
local controller = require("forge.views.harness.controller")
local question = require("forge.views.harness.plan_question")
local state = session.harness
local request_for, subscribe = client.request_for, client.subscribe
local render, refresh_winbar, open = controller.render, controller.refresh_winbar, question.open
local receive, host, continuation, state_reply, submitted
local goal_requests, snapshot_requests = 0, 0
controller.render = function() end
controller.refresh_winbar = function() end
question.open = function(_, value) host = value end
client.subscribe = function(callback) receive = callback return function() end end
client.request_for = function(_, method, params, callback)
  if method == "question.continue" then continuation = callback
  elseif method == "state.get" then
    snapshot_requests = snapshot_requests + 1
    state_reply = callback
  elseif method == "goal.continue" then goal_requests = goal_requests + 1
  elseif method == "prompt.submit" then submitted = params.text
  elseif method == "history.record" then callback({})
  elseif method == "agent.start" then continuation = callback
  else error("unexpected request: " .. method) end
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
  state.session = { id = "continuation-sync", backend = "mock", model = "mock", execution_mode = "read" }
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
  state_reply(snapshot(goal, state.active_elicitation))
  assert(snapshot_requests == 2, "coalesced invalidation did not request a fresh snapshot")
  assert(goal_requests == 0, "goal advanced using the consumed acceptance snapshot")
  state_reply(snapshot(goal))
  assert(goal_requests == 1 and state.busy, "goal did not advance after acceptance synchronization")
  assert(vim.api.nvim_get_current_win() == focused_window, "continuation changed keyboard focus")

  state.busy, state.goal = false, nil
  state.queue = { "queued after clarification" }
  present("interaction")
  receive("question_answered", {}, state.session.id)
  continuation({})
  state_reply(snapshot(nil, state.active_elicitation))
  assert(submitted == nil, "queue drained using an outdated clarification snapshot")
  state_reply(snapshot())
  assert(submitted == "queued after clarification", "clarification completion stranded queued input")
  assert(goal_requests == 1, "snapshot completion duplicated goal continuation")
  state.busy, submitted = false, nil
  state.capability.agent = { catalog = true }
  local followed = false
  state.presentation = { follow_tail = function() followed = true end }
  controller.spawn_agent("explorer", "Inspect tests")
  assert(followed and state.busy, "spawn did not resume tail following")
  state.presentation = nil
  state.queue = { "queued after child" }
  state.cancel_requested = true
  continuation({})
  assert(not state.cancel_requested, "settled parent request retained cancellation state")
  assert(submitted == nil, "spawn drained input before its fresh snapshot")
  state_reply(snapshot())
  assert(submitted == "queued after child", "spawn stranded queued input")
end, debug.traceback)

if state.working_timer then state.working_timer:stop() state.working_timer:close() end
if state.unsubscribe then state.unsubscribe() end
client.request_for, client.subscribe = request_for, subscribe
controller.render, controller.refresh_winbar, question.open = render, refresh_winbar, open
assert(success, failure)
print("harness_continuation_sync: passed")
