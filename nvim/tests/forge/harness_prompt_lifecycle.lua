vim.loader.enable(false)
local session = require("forge.session")
local state = session.harness
local client = require("forge.client")
local controller = require("forge.views.harness.controller")
local submission = require("forge.views.harness.prompt_submission")
local requests = {}
client.request_for = function(session_id, method, params, callback)
  assert(session_id == "prompt-lifecycle")
  requests[#requests + 1] = { method = method, params = params, callback = callback }
end
require("forge.views.harness.prompt_history").record = function() end
require("forge.views.harness.agent_picker").refresh = function() end
controller.render = function() end
controller.refresh_winbar = function() end
controller.attach_transcript = function() end
state.session = { id = "prompt-lifecycle" }
state.capability = { native_steer = true }
state.composer_buf = vim.api.nvim_create_buf(false, true)
state.queue, state.pending_steer = {}, {}
local current = true
state.presentation = { capture_prompt = function()
  return { buffer = state.composer_buf, document = "original", is_current = function() return current end }
end }
local function compose(text)
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, vim.split(text, "\n", { plain = true }))
end
local function complete_snapshot()
  local request = requests[#requests]
  assert(request.method == "state.get")
  request.callback({ session = state.session, capability = state.capability, status = { kind = "idle" } })
end

local path_prompt = "/workspace/Screen shot.png\n[Image #1]\n\n"
compose(path_prompt)
controller.submit()
local first = requests[1]
assert(first.method == "prompt.submit" and state.busy and state.prompt_submission)
assert(first.params.text == path_prompt, "absolute-path submission changed the complete captured draft")
compose("first follow-up")
controller.queue_submit()
compose("second follow-up")
controller.queue_submit()
assert(#state.queue == 2 and #requests == 1)

current = false
state.presentation = nil
compose("draft in the replacement composer")
assert(not submission.receive(state, { kind = "prompt_submission", data = {
  document = "original", token = first.params.submission.token, state = "accepted",
} }), "retired presentation changed its draft")
first.callback({})
assert(not state.busy and not state.prompt_submission, "retired presentation retained request ownership")
complete_snapshot()
local second = requests[#requests]
assert(second.method == "prompt.submit" and second.params.text == "first follow-up" and #state.queue == 1)
assert(state.busy and state.prompt_submission, "queued request did not acquire its own lifetime")
first.callback({})
assert(state.busy and state.prompt_submission and #state.queue == 1, "duplicate completion released a newer request")
second.callback({})
complete_snapshot()
local third = requests[#requests]
assert(third.method == "prompt.submit" and third.params.text == "second follow-up" and #state.queue == 0)
third.callback({})
complete_snapshot()
assert(not state.busy and not state.prompt_submission, "queue never returned to idle")
assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "draft in the replacement composer")

current = true
state.presentation = { capture_prompt = function()
  return { buffer = state.composer_buf, document = "original", is_current = function() return current end }
end }
state.checks_running, state.active_check_run = true, "manual-check"
compose("queued during checks")
local before_queue = #requests
controller.queue_submit()
assert(#requests == before_queue and #state.queue == 1, "queueing interrupted a check or started a prompt")
compose("interrupt manual check")
controller.submit()
local cancel = requests[#requests]
assert(cancel.method == "checks.cancel" and cancel.params.run_id == "manual-check")
assert(cancel.params.interrupt_execution == true, "Replacement prompt must collect its provider execution")
compose("newer unsent draft")
cancel.callback({ cancellation_requested = true })
local replacement = requests[#requests]
assert(replacement.method == "prompt.submit" and replacement.params.text == "interrupt manual check")
assert(#state.queue == 1, "queued work overtook the direct prompt")
submission.receive(state, { kind = "prompt_submission", data = {
  document = "original", token = replacement.params.submission.token, state = "accepted",
} })
assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "newer unsent draft",
  "accepting the captured prompt erased a newer draft")
state.checks_running, state.active_check_run = false, nil
replacement.callback({})
state.queue = {}
complete_snapshot()

for _, cancellation_first in ipairs({ true, false }) do
  compose("original provider prompt")
  controller.submit()
  local original = requests[#requests]
  state.checks_running, state.active_check_run = true, "automatic-check"
  compose("replace automatic check")
  controller.submit()
  local cancellation = requests[#requests]
  local count = #requests
  if cancellation_first then cancellation.callback({}) else original.callback(nil, "cancelled", { code = "turn_cancelled" }) end
  assert(#requests == count and state.check_interrupt_pending, "replacement started before both owners released")
  if cancellation_first then original.callback(nil, "cancelled", { code = "turn_cancelled" }) else cancellation.callback({}) end
  local next_prompt = requests[#requests]
  assert(next_prompt.method == "prompt.submit" and next_prompt.params.text == "replace automatic check")
  assert(not state.execution_notice and not state.check_interrupt_pending, "expected cancellation surfaced as a paused failure")
  state.checks_running, state.active_check_run = false, nil
  next_prompt.callback({})
  complete_snapshot()
end

for _, operation_state in ipairs({ "submitting", "accepted", "stopping", "running", "applying", "verifying", "resuming", "outcome_unknown" }) do
  state.task_operation = { action = "pause", state = operation_state }
  state.checks_running, state.active_check_run = true, "paused-check"
  compose("replace paused check")
  controller.submit()
  local cancellation = requests[#requests]
  assert(cancellation.method == "checks.cancel", "pending pause discarded the replacement prompt")
  local count = #requests
  cancellation.callback({})
  state.checks_running, state.active_check_run = false, nil
  controller.drain()
  assert(#requests == count and state.check_interrupt_pending, "replacement raced the pause operation")
  state.task_operation = nil
  controller.drain()
  local next_prompt = requests[#requests]
  assert(next_prompt.method == "prompt.submit" and next_prompt.params.text == "replace paused check")
  next_prompt.callback({})
  complete_snapshot()
end

local failures = { { message = "cancelled", detail = { code = "turn_cancelled" } },
  { message = "host stopped", detail = { code = "outcome_unknown" } } }
for _, failure in ipairs(failures) do
  local completion_count = 0
  submission.start(state, { text = "request surviving its view" }, function(_, message, detail)
    completion_count = completion_count + 1
    assert(message == failure.message and detail == failure.detail)
  end)
  local request = requests[#requests]
  request.callback(nil, failure.message, failure.detail)
  request.callback(nil, failure.message, failure.detail)
  assert(completion_count == 1 and not state.prompt_submission)
end
client.request_for = function() error("transport admission failed") end
local admission_failure
submission.start(state, { text = "request with failed admission" }, function(_, failure)
  admission_failure = failure
end)
assert(admission_failure:find("transport admission failed", 1, true) and not state.prompt_submission,
  "transport admission failure stranded request ownership")
if state.working_timer then state.working_timer:stop() state.working_timer:close() end
print("harness_prompt_lifecycle: passed")
vim.cmd("qa!")
