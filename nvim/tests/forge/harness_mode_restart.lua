vim.opt.runtimepath:prepend("nvim")

local client = require("forge.client")
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local notifications = require("forge.infra.notifications")
local question = require("forge.views.harness.plan_question")

---@class ModeRestartRequest
---@field method string
---@field params table
---@field reply fun(result?: table, failure?: string, detail?: table)
---@type ModeRestartRequest[]
local requests = {}
---@type string[]
local failures = {}
local question_action

client.request = function(method, params, callback)
  requests[#requests + 1] = { method = method, params = params, reply = callback }
end
notifications.error = function(message) failures[#failures + 1] = message end
controller.render = function() end
controller.refresh_winbar = function() end
question.open = function(_, action) question_action = action end

local function reset()
  if state.working_timer then state.working_timer:stop() state.working_timer:close() end
  state.working_timer, state.working_started_ns = nil, nil
  state.session = { id = "mode-restart", mode = "write", execution_mode = "write" }
  state.busy, state.no_checkpoint = false, false
  state.execution_notice, state.host_error = nil, nil
  state.mode_restart, state.pending_mode = nil, nil
  state.mode_restart_requested = false
  state.cancel_requested = false
  state.state_sync_pending, state.state_sync_again, state.state_sync_callback = nil, nil, nil
  state.queue, state.pending_steer, state.capability = {}, {}, {}
  state.configuring, state.configuration_debounce, state.pending_config = nil, nil, nil
  state.goal, state.active_elicitation, state.selected_agent_run_id = nil, nil, nil
  state.plan_question_open, state.presented_question_key, state.status = false, nil, nil
  requests, failures = {}, {}
end

---@param method string
---@return ModeRestartRequest
local function last(method)
  local request = requests[#requests]
  assert(request and request.method == method, "expected " .. method .. ", got " .. vim.inspect(request))
  return request
end

---@param kind string
---@return ModeRestartRequest
local function start(kind)
  if kind == "goal" then
    state.goal = { state = "active" }
    controller.drain()
    return last("goal.continue")
  elseif kind == "compact" then
    state.capability.native_compact = true
    controller.compact()
    return last("session.compact")
  elseif kind == "agent" then
    state.capability.agent = { catalog = true }
    controller.spawn_agent("worker", "task")
    return last("agent.start")
  elseif kind == "prompt" then
    state.queue = { "continue task" }
    controller.drain()
    return last("prompt.submit")
  end
  state.active_elicitation = { owner = "plan_acceptance", elicitation = { id = "approval", questions = {} } }
  controller.present_plan_question(true)
  if kind == "clarification" then question_action.ask({ text = "clarify" })
  else question_action.continue() end
  return last(kind == "clarification" and "question.ask" or "question.continue")
end

local cancelled = { code = "turn_cancelled" }
for _, kind in ipairs({ "plan", "goal", "prompt", "clarification", "compact", "agent" }) do
  reset()
  start(kind).reply(nil, "Turn cancelled by user", cancelled)
  assert(not state.busy and state.execution_notice == "Paused" and #failures == 0,
    "ordinary cancellation was reported as a failure")
  for _, acknowledgement_first in ipairs({ true, false }) do
    reset()
    local execution = start(kind)
    state.selected_agent_run_id = "child"
    state.agent = { run = { { id = "child" } } }
    controller.set_mode("yolo")
    local restart = last("turn.restart")
    assert(not restart.params.target, "mode changes must cancel the whole session")
    if acknowledgement_first then
      restart.reply({ restart_requested = true })
      assert(last("turn.restart") == restart, "mode changed before execution settled")
      execution.reply(nil, "interrupted", cancelled)
    else
      execution.reply(nil, "interrupted", cancelled)
      assert(last("turn.restart") == restart, "mode changed before cleanup acknowledgement")
      restart.reply({ restart_requested = true })
    end
    local change = last("session.execution_mode")
    assert(change.params.mode == "yolo")
    change.reply({ id = "mode-restart", execution_mode = "yolo" })
    local resumed = last("exchange.resume")
    assert(state.busy and not state.mode_restart_requested and not state.pending_mode)
    assert(#failures == 0, "expected cancellation was reported as failure")
    controller.set_mode("read")
    last("turn.restart").reply({ restart_requested = true })
    resumed.reply(nil, "interrupted", cancelled)
    assert(last("session.execution_mode").params.mode == "read", "resumed work could not be interrupted")
    last("session.execution_mode").reply({ id = "mode-restart", execution_mode = "read" })
    last("exchange.resume").reply({})
    last("state.get")
    assert(not state.busy and #failures == 0)
  end
end

for _, acknowledgement_first in ipairs({ true, false }) do
  reset()
  local execution = start("goal")
  controller.set_mode("yolo")
  local restart = last("turn.restart")
  controller.cancel_turn()
  controller.cancel_turn()
  assert(last("turn.restart") == restart, "stop duplicated in-flight cleanup")
  if acknowledgement_first then
    restart.reply({ restart_requested = true })
    execution.reply(nil, "interrupted", cancelled)
  else
    execution.reply(nil, "interrupted", cancelled)
    restart.reply({ restart_requested = true })
  end
  last("turn.cancel").reply({ cancel_requested = true })
  assert(not state.busy and state.execution_notice == "Paused" and #failures == 0)
  assert(not vim.iter(requests):any(function(request) return request.method == "exchange.resume" end))
end

reset()
local applying_execution = start("goal")
controller.set_mode("yolo")
last("turn.restart").reply({ restart_requested = true })
applying_execution.reply(nil, "interrupted", cancelled)
local applying_mode = last("session.execution_mode")
controller.cancel_turn()
applying_mode.reply({ id = "mode-restart", execution_mode = "yolo" })
last("turn.cancel").reply({ cancel_requested = true })
assert(not state.busy and state.execution_notice == "Paused")
assert(not vim.iter(requests):any(function(request) return request.method == "exchange.resume" end))

reset()
local execution = start("plan")
controller.set_mode("yolo")
local restart = last("turn.restart")
controller.set_mode("full")
assert(last("turn.restart") == restart, "duplicate cleanup request")
execution.reply({})
restart.reply({ restart_requested = true })
assert(last("session.execution_mode").params.mode == "full", "latest mode was discarded")
controller.set_mode("read")
last("session.execution_mode").reply({ id = "mode-restart", execution_mode = "full" })
assert(last("session.execution_mode").params.mode == "read", "mode changed during application was discarded")
last("session.execution_mode").reply(nil, "mode rejected")
assert(not state.busy and not state.mode_restart_requested and #failures == 1)
assert(not vim.iter(requests):any(function(request) return request.method == "exchange.resume" end))

for _, completion_first in ipairs({ true, false }) do
  reset()
  execution = start("goal")
  controller.set_mode("yolo")
  restart = last("turn.restart")
  if completion_first then execution.reply(nil, "interrupted", cancelled) end
  restart.reply(nil, "cleanup failed")
  assert(not state.mode_restart_requested and not state.pending_mode)
  assert(state.busy == not completion_first, "cleanup failure lost active request ownership")
  assert(#failures == 1 and failures[1]:find("cleanup failed", 1, true))
  assert(not vim.iter(requests):any(function(request) return request.method == "session.execution_mode" end))
end

reset()
execution = start("goal")
controller.set_mode("yolo")
restart = last("turn.restart")
execution.reply(nil, "provider failed", { code = "backend_error" })
restart.reply({ restart_requested = true })
assert(not state.busy and #failures == 1 and failures[1] == "provider failed")
assert(not vim.iter(requests):any(function(request) return request.method == "exchange.resume" end))

reset()
controller.set_mode("read")
last("session.execution_mode").reply({ id = "mode-restart", execution_mode = "read" })
assert(not state.busy and #requests == 1, "idle switch started work")

reset()
local subscribed
state.status = { kind = "finalizing" }
controller.cancel_turn()
local finalization = last("turn.cancel")
controller.cancel_turn()
assert(last("turn.cancel") == finalization, "duplicate finalization retry")
finalization.reply(nil, "locked checkpoint source")
assert(not state.cancel_requested and not state.busy)
assert(state.execution_notice:find("locked checkpoint source", 1, true))
assert(#failures == 1)
controller.cancel_turn()
last("turn.cancel").reply({ cancel_requested = true })
assert(not state.cancel_requested and state.execution_notice == "Paused")
assert(not state.busy, "finalization retry resumed execution")

reset()
client.subscribe = function(callback) subscribed = callback return function() end end
client.host_accepting = function() return true end
controller.attach_transcript = function() end
require("forge.shared.keymaps").setup_view_keymaps = function() end
require("forge.views.harness.workspace").attach = function() end
require("forge.views.harness.prompt_history").attach = function() end
controller.attach()
start("goal")
controller.set_mode("yolo")
state.queue = { "keep this queued" }
subscribed("host_stopped", { message = "Forge host stopped (exit 1)" })
assert(not state.busy and not state.mode_restart_requested and not state.pending_mode)
assert(state.host_error and state.execution_notice:find("Reopen Harness", 1, true))
local request_count = #requests
for _ = 1, 3 do controller.cancel_turn() controller.drain() end
assert(#requests == request_count and #state.queue == 1, "host failure restarted work or discarded queue")
assert(#failures == 0, "idle cancellation generated repeated failure notifications")

print("harness_mode_restart: passed")
vim.cmd("qa!")
