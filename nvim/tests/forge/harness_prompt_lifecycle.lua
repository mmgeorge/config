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
state.session = { id = "prompt-lifecycle", provider = "mock" }
state.capability = { native_steer = true }
state.composer_buf = vim.api.nvim_create_buf(false, true)
state.queue, state.pending_steer = {}, {}
local current = true
state.presentation = { capture_prompt = function()
  return { buffer = state.composer_buf, document = "original", is_current = function() return current end }
end }
client.request = function(request_specification, callback)
  local method = request_specification.method
  local parameters = request_specification.params or {}

  if host.request(method, parameters, callback) then return end
  if method == "session.state.get" then
    callback({ session = state.session, capability = state.capability,
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
---@param text string
---@return {id: string, text: string, delivery?: string}?
local function retained_message(text)
  for _, messages in ipairs({ state.queue, state.pending_steer }) do
    for _, message in ipairs(messages or {}) do
      if type(message) == "table" then
        ---@cast message {id: string, text: string, delivery?: string}
        if message.text == text then
          assert(type(message.id) == "string", "fixture host message requires its original input identity")
          return message
        end
      end
    end
  end
end
local function route_message(id, delivery, route)
  for _, message in ipairs(host.messages) do
    if message.id == id then
      message.revision = message.revision + 1
      message.delivery, message.route = delivery, route
      host.revision = host.revision + 1
      input.receive(state, { revision = host.revision, suspended = false,
        upsert = { vim.deepcopy(message) }, removed = {} })
      return
    end
  end
  error("host message is missing: " .. id)
end
local path_prompt = "/workspace/Screen shot.png\n[Image #1]\n\n"
compose(path_prompt)
controller.submit()
assert(composer_text() == "" and state.pending_submit[1].text == path_prompt and #state.queue == 0)
local first = host.requests[1]
compose("first follow-up")
controller.queue_submit()
compose("second follow-up")
controller.queue_submit()
assert(#state.queue == 2 and #state.pending_submit == 1 and #host.requests == 3,
  "queued drafts must submit independent host commands before earlier acknowledgements")
compose("newer draft")
current = false
host.flush()
assert(composer_text() == "newer draft", "retired capture overwrote the replacement composer")
assert(#state.queue == 3 and #state.pending_submit == 0, "host queued receipts must replace pending drafts")
host.complete(first)
assert(#state.queue == 3 and #state.pending_submit == 0 and #host.messages == 3,
  "duplicate receipt removed another message or duplicated host input")
current = true

controller.cancel_turn()
assert(#state.queue == 3, "requesting cancellation must preserve queued input")
local cancellation = controls[#controls]
assert(cancellation.method == "session.turn.cancel" and cancellation.parameters.restore_prompt_if_no_output)
cancellation.callback({ cancel_requested = true })
local count = #host.requests
controller.render()
assert(#host.requests == count and composer_text() == "newer draft", "pause drained queued input or overwrote typing")
controller.task_transition({ action = "resume" })
local resume = controls[#controls]
assert(resume.parameters.action == "resume")
resume.callback({ state = "completed" })

compose("active-turn addition")
dofile("nvim/tests/forge/support/activity.lua").set_busy(state, false)
state.execution_busy = true
controller.submit()
---@type { id: string, text: string, delivery?: string }
local addition = assert(state.pending_submit[#state.pending_submit])
assert(addition.text == "active-turn addition" and #state.pending_steer == 0,
  "local busy state must not invent host steering admission")
assert(host.requests[#host.requests].parameters.action.message.intent == "auto",
  "stale local busy state bypassed host routing")
host.flush()
assert(assert(retained_message("active-turn addition")).delivery == "queued" and #state.pending_steer == 0,
  "a queued receipt must not imply native steering")
route_message(addition.id, "delivering", "steering")
assert(#state.pending_steer == 1 and state.pending_steer[1].id == addition.id and #state.pending_submit == 0,
  "host routing must move the original message into pending steering")
route_message(addition.id, "accepted", "steering")
assert(not retained_message("active-turn addition") and #state.pending_steer == 0,
  "host acceptance must remove only the accepted steering message")
assert(#state.queue == 3 and composer_text() == "", "steering acceptance changed unrelated queued input")

state.checks_running, state.active_check_run = true, "check"
compose("replace check")
local control_count = #controls
controller.submit()
assert(#controls == control_count, "Lua interrupted checks instead of submitting input")
assert(host.requests[#host.requests].parameters.action.message.text == "replace check"
  and host.requests[#host.requests].parameters.action.message.intent == "auto")
compose("draft after cancellation")
controller.cancel_turn()
assert(controls[#controls].method == "session.turn.cancel")
controls[#controls].callback({ cancel_requested = true })
assert(composer_text() == "draft after cancellation")
host.flush()
assert(retained_message("replace check"), "cancellation discarded pending input")
state.checks_running, state.active_check_run = false, nil

compose("retract before output")
submission.start(state, { text = "retract before output", context = state.presentation.capture_prompt() }, function() end)
host.flush()
local retracted = assert(retained_message("retract before output"))
host.revision = host.revision + 1
input.receive(state, { revision = host.revision, suspended = true, upsert = {}, removed = {}, retracted = retracted.id })
assert(composer_text() == "retract before output", "retraction did not restore the unchanged composer")
host.flush()
compose("retract while typing")
submission.start(state, { text = "retract while typing", context = state.presentation.capture_prompt() }, function() end)
host.flush()
retracted = assert(retained_message("retract while typing"))
compose("preserve this")
host.revision = host.revision + 1
input.receive(state, { revision = host.revision, suspended = true, upsert = {}, removed = {}, retracted = retracted.id })
assert(composer_text() == "preserve this" and assert(retained_message("retract while typing")).id == retracted.id)
assert(#errors == 0, table.concat(errors, "\n"))
if state.working_timer then state.working_timer:stop() state.working_timer:close() end
print("harness_prompt_lifecycle: passed")
vim.cmd("qa!")
