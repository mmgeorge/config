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
local picker_open_count = 0
question.open = function(_, callbacks) picker, picker_open_count = callbacks, picker_open_count + 1 end
state.session = { id = "question-input", provider = "mock", model = "mock", approval_mode = "ask" }
state.queue = {}
state.active_elicitation = { owner = "interaction", exchange_id = "exchange",
  elicitation = { question_set = { id = "set", questions = {
    { id = "choice", question = "Choose?", options = {} },
  } }, answer = {}, current_index = 0 },
}
client.request = function(request_specification, callback)
  local method = request_specification.method
  local parameters = request_specification.params or {}

  if transport.request(method, parameters, callback) then return end
  assert(method == "session.state.get", method)
  callback({ session = state.session, active_elicitation = state.active_elicitation, status = { kind = "awaiting_input" } })
end
input.attach(state, function() end)
controller.present_plan_question(true)
picker.ask({ question_id = "choice", text = "First clarification" })
assert(not require("forge.views.harness.activity").busy(state) and #state.queue == 0 and state.pending_submit[1].text == "First clarification")
-- An in-flight clarification keeps the question picker closed; the retained callbacks still
-- model a second clarification racing the first acknowledgement.
controller.present_plan_question(true)
assert(picker_open_count == 1, "question picker reopened while a clarification was unacknowledged")
picker.ask({ question_id = "choice", text = "Second clarification" })
-- Each clarification is its own host input submission; both stay editor drafts until admitted.
assert(#state.pending_submit == 2 and #state.pending_steer == 0 and #transport.requests == 2)
assert(state.pending_submit[2].text == "Second clarification")
transport.complete(transport.requests[1])
assert(state.question_draft.text == "Second clarification", "older acknowledgement cleared the current draft")
transport.flush()
assert(not state.question_draft and not state.execution_notice)
assert(#state.queue == 2 and #state.pending_submit == 0 and #state.pending_steer == 0 and #errors == 0)
assert(state.queue[1].intent == "auto" and state.queue[2].intent == "auto")
print("harness_question_admission: passed")
vim.cmd("qa!")
