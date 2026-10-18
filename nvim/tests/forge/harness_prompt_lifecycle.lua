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
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { text })
end
local function complete_snapshot()
  local request = requests[#requests]
  assert(request.method == "state.get")
  request.callback({ session = state.session, capability = state.capability, status = { kind = "idle" } })
end

compose("first prompt")
controller.submit()
local first = requests[1]
assert(first.method == "prompt.submit" and state.busy and state.prompt_submission)
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
