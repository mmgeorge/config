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
  request.callback({ session = state.session, capability = state.capability, status = { kind = "idle" }, task = state.task })
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
assert(second.method == "prompt.submit" and second.params.text == "first follow-up" and #state.queue == 2)
assert(second.params.queued and second.params.submission.token, "queued prompt has no admission identity")
assert(state.busy and state.prompt_submission, "queued request did not acquire its own lifetime")
state.busy = false
local pending_count = #requests
controller.drain()
assert(#requests == pending_count, "idle presentation submitted an in-flight queued prompt twice")
state.busy = true
assert(submission.receive(state, { kind = "prompt_submission", data = {
  token = second.params.submission.token, state = "accepted",
} }))
assert(#state.queue == 1, "acceptance did not remove the matching queue entry")
first.callback({})
assert(state.busy and state.prompt_submission and #state.queue == 1, "duplicate completion released a newer request")
second.callback({})
complete_snapshot()
local third = requests[#requests]
assert(third.method == "prompt.submit" and third.params.text == "second follow-up" and #state.queue == 1)
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
local notices = {}
require("forge.infra.notifications").error = function(message) notices[#notices + 1] = message end
local retained = { text = "retained queued prompt", attachments = { { id = "queued-image", label = "[Image #1]" } },
  config = { model = "captured-model" } }
local following = { text = "following queued prompt" }
state.queue = { retained, following }
controller.drain()
local rejected = requests[#requests]
assert(state.queue[1] == retained and state.prompt_submission.queue_entry == retained)
local edit, edit_failure = require("forge.views.harness.queue_editor").move(state, "", -2)
assert(not edit and edit_failure:find("being admitted", 1, true), "in-flight queue entry was editable")
rejected.callback(nil, "a Harness execution request is already running: no permits available", { code = "request_failed" })
complete_snapshot()
assert(state.queue[1] == retained and state.queue[2] == following and #state.queue == 2,
  "rejected admission lost or reordered queued messages")
assert(retained.config.model == "captured-model" and retained.attachments[1].id == "queued-image")
assert(state.execution_notice and not state.prompt_submission and not state.busy)
state.execution_notice = nil
controller.drain()
local retried = requests[#requests]
assert(retried.params.text == retained.text and vim.deep_equal(retried.params.attachments, retained.attachments))
assert(not submission.receive(state, { kind = "prompt_submission", data = {
  token = rejected.params.submission.token, state = "accepted",
} }), "stale acceptance removed the retried entry")
submission.receive(state, { kind = "prompt_submission", data = { token = retried.params.submission.token, state = "accepted" } })
submission.receive(state, { kind = "prompt_submission", data = { token = retried.params.submission.token, state = "accepted" } })
assert(#state.queue == 1 and state.queue[1] == following, "duplicate acceptance removed a later message")
submission.receive(state, { kind = "prompt_submission", data = { token = retried.params.submission.token, state = "retracted" } })
retried.callback(nil, "retracted", { code = "turn_retracted" })
complete_snapshot()
assert(#state.queue == 2 and state.queue[1] == retained and state.queue[2] == following,
  "retraction did not restore exactly one original entry")
state.execution_notice = nil
controller.drain()
local unknown = requests[#requests]
unknown.callback(nil, "connection closed", { code = "outcome_unknown" })
complete_snapshot()
assert(state.queue[1] == retained and state.execution_notice, "unknown admission lost queued input")
state.execution_notice = nil
controller.drain()
local accepted = requests[#requests]
submission.receive(state, { kind = "prompt_submission", data = { token = accepted.params.submission.token, state = "accepted" } })
accepted.callback(nil, "provider failed after acceptance", { code = "request_failed" })
complete_snapshot()
assert(#state.queue == 1 and state.queue[1] == following, "accepted work was queued for duplicate execution")
state.execution_notice = nil
state.queue = {}
for _, has_task in ipairs({ false, true }) do
  state.session.current_task_id = has_task and "cancel-task" or nil
  state.task = has_task and { { id = "cancel-task", kind = "chat", status = "running" } } or {}
  compose("active prompt to cancel")
  controller.submit()
  local active_prompt = requests[#requests]
  compose("steering awaiting acknowledgement")
  controller.steer_submit()
  local steering = requests[#requests]
  assert(steering.method == "turn.steer")
  local cancel_queue = {
    { text = "first retained follow-up", task_id = state.session.current_task_id },
    { text = "second retained follow-up", task_id = state.session.current_task_id },
  }
  state.queue = cancel_queue
  compose("unsent draft while cancelling")
  controller.cancel_turn()
  local pause = requests[#requests]
  assert(pause.method == "task.transition" and pause.params.action == "pause"
      and pause.params.text == nil and pause.params.attachments == nil,
    "Ctrl-C submitted composer content instead of a pause-only operation")
  steering.callback(nil, "steering missed the cancelled turn")
  active_prompt.callback(nil, "cancelled", { code = "turn_cancelled" })
  if has_task then state.task[1].status = "paused" end
  pause.callback({ state = "completed" })
  complete_snapshot()
  local settled_count = #requests
  vim.wait(20, function() return false end, 1)
  controller.drain()
  assert(#requests == settled_count and not state.busy and state.queue_suspended,
    "late steering or cancellation completion automatically submitted pending input")
  assert(state.queue == cancel_queue and #state.queue == 3
      and state.queue[1].text == "first retained follow-up"
      and state.queue[2].text == "second retained follow-up"
      and state.queue[3].text == "steering awaiting acknowledgement",
    "cancellation lost or reordered queued input and unaccepted steering")
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "unsent draft while cancelling",
    "cancellation erased the unsent draft")
  compose("explicitly start another prompt")
  controller.submit()
  local explicit_prompt = requests[#requests]
  assert(explicit_prompt.method == "prompt.submit" and explicit_prompt.params.text == "explicitly start another prompt"
      and not state.queue_paused, "an explicit prompt did not release the local queue pause")
  state.queue = {}
  explicit_prompt.callback({})
  complete_snapshot()
end
state.session.current_task_id, state.task = nil, {}
state.queue = {}
state.checks_running, state.active_check_run = true, "cancel-replacement"
compose("replacement pending check cancellation")
controller.submit()
local check_cancel = requests[#requests]
assert(check_cancel.method == "checks.cancel")
compose("newer unsent draft")
controller.cancel_turn()
local replacement_pause = requests[#requests]
assert(replacement_pause.method == "task.transition" and state.check_interrupt_pending == nil
    and state.queue[1].text == "replacement pending check cancellation",
  "Ctrl-C did not retain the pending check replacement as queued input")
check_cancel.callback({})
state.checks_running, state.active_check_run = false, nil
replacement_pause.callback({ state = "completed" })
complete_snapshot()
local paused_count = #requests
controller.drain()
assert(#requests == paused_count and state.queue_paused and #state.queue == 1
    and vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "newer unsent draft",
  "late check cancellation delivered the replacement or changed the newer draft")
state.queue = {}
controller.task_transition({ action = "resume" })
local resumed = requests[#requests]
assert(resumed.method == "task.transition" and resumed.params.action == "resume" and not state.queue_paused,
  "explicit resume did not release the local queue pause")
resumed.callback({ state = "completed" })
complete_snapshot()
state.session.current_task_id = "already-running"
state.task = {{ id = "already-running", kind = "chat", status = "running" }}
state.queue_paused = true
state.queue = {{ text = "resume locally paused queue", task_id = "already-running" }}
controller.task_transition({ action = "resume" })
local resumed_prompt = requests[#requests]
assert(resumed_prompt.method == "prompt.submit" and resumed_prompt.params.text == "resume locally paused queue",
  "explicit resume stranded the local queue when the host task was already running")
state.queue = {}
resumed_prompt.callback({})
complete_snapshot()
client.request_for = function() error("transport admission failed") end
local admission_failure
state.queue = { retained }
submission.start(state, { text = retained.text, queue_entry = retained }, function(_, failure)
  admission_failure = failure
end)
assert(admission_failure:find("transport admission failed", 1, true) and not state.prompt_submission,
  "transport admission failure stranded request ownership")
assert(state.queue[1] == retained, "synchronous transport rejection lost queued input")
if state.working_timer then state.working_timer:stop() state.working_timer:close() end
print("harness_prompt_lifecycle: passed")
vim.cmd("qa!")
