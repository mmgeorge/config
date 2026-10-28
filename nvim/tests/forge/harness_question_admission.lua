vim.loader.enable(false)
local client = require("forge.client")
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local question = require("forge.views.harness.plan_question")
local input = require("forge.views.harness.input_state")
local transport = dofile("nvim/tests/forge/support/input.lua").new(state)
local picker, errors = nil, {}
controller.render = function() end
controller.refresh_winbar = function() end
controller.attach_transcript = function() end
require("forge.views.harness.agent_picker").refresh = function() end
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
require("forge.views.harness.prompt_history").record = function() end
question.open = function(_, callbacks) picker = callbacks end
state.session = { id = "question-input", backend = "mock", model = "mock", approval_mode = "ask" }
state.task, state.queue = {}, {}
state.active_elicitation = { owner = "interaction", exchange_id = "exchange",
  elicitation = { question_set = { id = "set", questions = {
    { id = "choice", question = "Choose?", options = {} },
  } }, answer = {}, current_index = 0 },
}
client.request_for = function(_, method, parameters, callback)
  if transport.request(method, parameters, callback) then return end
  assert(method == "state.get", method)
  callback({ session = state.session, active_elicitation = state.active_elicitation, status = { kind = "awaiting_input" } })
end
input.attach(state, function() end)
controller.present_plan_question(true)
picker.ask({ question_id = "choice", text = "First clarification" })
assert(not state.busy and #state.queue == 0 and state.pending_submit[1].text == "First clarification")
controller.present_plan_question(true)
picker.ask({ question_id = "choice", text = "Second clarification" })
assert(#state.pending_submit == 1 and #state.pending_steer == 1 and #transport.requests == 1)
transport.complete(transport.requests[1])
assert(state.question_draft.text == "Second clarification", "older acknowledgement cleared the current draft")
transport.flush()
assert(not state.question_draft and not state.execution_notice)
assert(#state.queue == 0 and #state.pending_submit == 1 and #state.pending_steer == 1 and #errors == 0)
assert(state.pending_submit[1].intent == "auto" and state.pending_steer[1].intent == "auto")
print("harness_question_admission: passed")
vim.cmd("qa!")
