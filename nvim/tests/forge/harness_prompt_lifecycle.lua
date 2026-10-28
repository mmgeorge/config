vim.loader.enable(false)
local state = require("forge.session").harness
local client = require("forge.client")
local controller = require("forge.views.harness.controller")
local input = require("forge.views.harness.input_state")
local submission = require("forge.views.harness.prompt_submission")
local host = dofile("nvim/tests/forge/support/input.lua").new(state)
local controls, errors = {}, {}
require("forge.views.harness.prompt_history").record = function() end
require("forge.views.harness.agent_picker").refresh = function() end
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
controller.render = function() end
controller.refresh_winbar = function() end
controller.attach_transcript = function() end
state.session = { id = "prompt-lifecycle", backend = "mock" }
state.capability = { native_steer = true }
state.composer_buf = vim.api.nvim_create_buf(false, true)
state.queue, state.pending_steer, state.task = {}, {}, {}
local current = true
state.presentation = { capture_prompt = function()
  return { buffer = state.composer_buf, document = "original", is_current = function() return current end }
end }
client.request_for = function(_, method, parameters, callback)
  if host.request(method, parameters, callback) then return end
  if method == "state.get" then
    callback({ session = state.session, capability = state.capability, task = state.task,
      status = { kind = "idle" }, runtime_epoch = state.runtime_epoch })
    return
  end
  controls[#controls + 1] = { method = method, parameters = parameters, callback = callback }
end
input.attach(state, function() end)
local function compose(text)
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, vim.split(text, "\n", { plain = true }))
end
local function composer_text() return table.concat(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false), "\n") end
local path_prompt = "/workspace/Screen shot.png\n[Image #1]\n\n"
compose(path_prompt)
controller.submit()
assert(composer_text() == "" and state.pending_submit[1].text == path_prompt and #state.queue == 0)
local first = host.requests[1]
compose("first follow-up")
controller.queue_submit()
compose("second follow-up")
controller.queue_submit()
assert(#state.queue == 2 and #state.pending_submit == 1 and #host.requests == 1, "queued input waited for acknowledgement")
compose("newer draft")
current = false
host.flush()
assert(composer_text() == "newer draft", "retired capture overwrote the replacement composer")
host.complete(first)
assert(#state.queue == 2 and #state.pending_submit == 1, "duplicate receipt removed another message")
current = true

controller.cancel_turn()
local pause = controls[#controls]
assert(pause.method == "task.transition" and pause.parameters.action == "pause")
assert(pause.parameters.text == nil and state.queue_paused and #state.queue == 2)
pause.callback({ state = "completed" })
local count = #host.requests
controller.render()
assert(#host.requests == count and composer_text() == "newer draft", "pause drained queued input or overwrote typing")
controller.task_transition({ action = "resume" })
local resume = controls[#controls]
assert(resume.parameters.action == "resume")
resume.callback({ state = "completed" })
assert(not state.queue_paused)

compose("active-turn addition")
state.busy, state.execution_busy = false, true
controller.submit()
assert(state.pending_steer[#state.pending_steer].text == "active-turn addition")
assert(host.requests[#host.requests].parameters.action.message.intent == "auto",
  "stale local busy state bypassed host routing")
host.flush()

state.checks_running, state.active_check_run = true, "check"
compose("replace check")
local control_count = #controls
controller.submit()
assert(#controls == control_count, "Lua interrupted checks instead of submitting input")
assert(host.requests[#host.requests].parameters.action.message.text == "replace check"
  and host.requests[#host.requests].parameters.action.message.intent == "auto")
compose("draft after cancellation")
controller.cancel_turn()
local replacement_pause = controls[#controls]
assert(replacement_pause.method == "task.transition")
replacement_pause.callback({ state = "completed" })
assert(state.queue_paused and composer_text() == "draft after cancellation")
host.flush()
assert(state.pending_steer[#state.pending_steer].text == "replace check")
state.checks_running, state.active_check_run = false, nil

compose("retract before output")
submission.start(state, { text = "retract before output", context = state.presentation.capture_prompt() }, function() end)
host.flush()
local retracted = state.pending_steer[#state.pending_steer]
host.revision = host.revision + 1
input.receive(state, { revision = host.revision, suspended = true, upsert = {}, removed = {}, retracted = retracted.id })
assert(composer_text() == "retract before output", "retraction did not restore the unchanged composer")
host.flush()
compose("retract while typing")
submission.start(state, { text = "retract while typing", context = state.presentation.capture_prompt() }, function() end)
host.flush()
retracted = state.pending_steer[#state.pending_steer]
compose("preserve this")
host.revision = host.revision + 1
input.receive(state, { revision = host.revision, suspended = true, upsert = {}, removed = {}, retracted = retracted.id })
assert(composer_text() == "preserve this" and state.pending_steer[#state.pending_steer].id == retracted.id)
assert(#errors == 0, table.concat(errors, "\n"))
if state.working_timer then state.working_timer:stop() state.working_timer:close() end
print("harness_prompt_lifecycle: passed")
vim.cmd("qa!")
