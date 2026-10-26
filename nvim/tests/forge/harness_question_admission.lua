vim.loader.enable(false)
local client = require("forge.client")
local session = require("forge.session")
local controller = require("forge.views.harness.controller")
local question = require("forge.views.harness.plan_question")
local state = session.harness
local host, snapshot_reply
local request_list, notice_list = {}, {}
controller.render = function() end
controller.refresh_winbar = function() end
question.open = function(_, value) host = value end
vim.notify = function(message) notice_list[#notice_list + 1] = message end
client.subscribe = function() return function() end end
client.request_for = function(_, method, _, callback)
  if method == "question.ask" then request_list[#request_list + 1] = callback
  elseif method == "state.get" then snapshot_reply = callback
  elseif method == "history.record" then callback({})
  else error("unexpected request: " .. method) end
end
state.transcript_buf, state.transcript_win, state.composer_buf, state.composer_win =
  require("forge.views.harness.layout").open("admission")
state.session = { id = "admission", backend = "mock", model = "mock", execution_mode = "read" }
state.active_elicitation = { owner = "interaction", exchange_id = "exchange",
  elicitation = { question_set = { id = "set", questions = {
    { id = "choice", question = "Choose?", options = {} },
  } }, answer = {}, current_index = 0 },
}
state.queue = {}
controller.attach()
controller.present_plan_question(true)
host.ask({ question_id = "choice", text = "First clarification" })
assert(state.busy)
state.busy = false
controller.present_plan_question(true)
host.ask({ question_id = "choice", text = "Current clarification" })
local current_owner = state.execution_request_owner
request_list[1](nil, "already running", { code = "not_admitted" })
assert(state.busy and state.execution_request_owner == current_owner,
  "stale rejection cleared the current execution owner")
assert(not state.execution_notice, "stale rejection replaced the current diagnostic")
request_list[2](nil, "already running", { code = "not_admitted" })
assert(not state.busy)
assert(state.execution_notice:find("Not submitted", 1, true))
assert(state.active_elicitation.elicitation.question_set.id == "set")
assert(state.question_draft.text == "Current clarification")
assert(#notice_list == 0, "normal admission contention produced an error popup")
snapshot_reply({ session = state.session, active_elicitation = state.active_elicitation })
controller.present_plan_question(true)
assert(host.clarification_draft.text == "Current clarification", "retry lost the rejected draft")
if state.working_timer then state.working_timer:stop() state.working_timer:close() end
if state.unsubscribe then state.unsubscribe() end
print("harness_question_admission: passed")
vim.cmd("qa!")
