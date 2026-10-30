vim.loader.enable(false)
local client = require("forge.client")
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local attachment = require("forge.views.harness.attachment")
local input = require("forge.views.harness.input_state")
local host = dofile("nvim/tests/forge/support/input.lua").new(state)
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
state.composer_buf = vim.api.nvim_create_buf(false, true)
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
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { text })
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
assert(#warnings == 36)
for _, warning in ipairs(warnings) do assert(warning == "Harness is still initializing") end

-- Ownership, rather than the legacy ready marker, releases the same draft for one explicit submission.
state.session = { id = "initialized-owner", backend = "mock" }
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
assert(state.prompt_history[1].text == "/allow")

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
print("harness_composer_admission: passed")
vim.cmd("qa!")
