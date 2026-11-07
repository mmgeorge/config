vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
vim.loader.enable(false)
-- Resolve support paths before the fixture changes into its workspace.
local state_snapshot = dofile(vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p:h") .. "/support/state.lua")
local data, workspace = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(data, "p")
vim.fn.mkdir(workspace, "p")
vim.fn.writefile({ '{"checks":{}}' }, workspace .. "/.forge.json")
local executable = vim.fn.fnamemodify(require("forge.builder").binary_path(), ":p")
local stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return (kind == "data" or kind == "config") and data or stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback) callback({ ok = true, path = executable }) return function() end end }
require("forge").setup({ harness = { provider = "mock" } })
vim.fn.chdir(workspace)
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local errors = {}
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
local function wait(predicate, message)
  assert(vim.wait(15000, function() return #errors > 0 or predicate() end, 10), message)
  assert(#errors == 0, table.concat(errors, "\n"))
end
local function request(method, params)
  local complete, value, failure
  local function send()
    client.request({ method = method, params = params or {}, session_id = state.session.id }, function(result, problem)
      value, failure, complete = result, problem, true
    end)
  end
  send()
  wait(function()
    if complete and failure == "Harness cleanup is still running" then
      complete = false
      send()
    end
    return complete
  end, method .. " stalled")
  assert(not failure, tostring(failure))
  return value
end
local function delivery_settled()
  return not require("forge.views.harness.activity").busy(state) and vim.iter((state.input or {}).confirmed or {}):all(function(message)
    return message.delivery == "accepted"
  end)
end
-- Script one real provider tool call, then submit a prompt that the scripted turn consumes.
local function call_tool(name, arguments)
  request("session.provider.mock_response", { operation = "script", script = {
    steps = { { kind = "tool_call", name = name, arguments = arguments } }, terminal = "completed",
  } })
  request("session.prompt.submit", { text = "Call " .. name .. " for the fixture." })
end
local function ask(id, requirement)
  wait(delivery_settled, "Previous question delivery did not settle")
  -- question_ask derives the requirement from `wait`: a waiting question is required.
  call_tool("question_ask", { wait = requirement == "required", questions = {
    { header = id, question = "Which layout?", options = { { label = "Tabs", description = "Compact", recommended = true } } },
  } })
end
require("forge").open_harness()
wait(function() return state.ready and state.session end, "Harness did not start")
ask("first", "optional")
wait(function() return state.questions and #state.questions.questions == 1 end, "First question did not publish")
ask("second", "optional")
wait(function() return #state.questions.questions == 2 end, "Second question replaced the first")
local first = state.questions.questions[1]
request("session.question.answer", { command = { operation_id = "answer-first", question_id = first.id, revision = 1,
  action = { kind = "answer", answer = { kind = "selected", option_id = "1:0" } } } })
wait(function() return state.questions.questions[1].resolution.kind == "answered" end, "Answer did not persist")
local second = state.questions.questions[2]
local input = request("session.input.get")
request("session.input.apply", { id = "clarify-second", control_revision = input.control_revision,
  after_control = input.control_operation_id,
  target = { owner = "question", question_id = second.id, revision = second.content.revision },
  action = { kind = "submit", message = { id = "clarify-second", revision = 0,
    text = "Does this affect existing sessions?", purpose = { kind = "user" }, intent = "auto", attachments = {} } } })
wait(function() return #state.questions.questions[2].discussion == 1 end, "Clarification disappeared")
assert(state.questions.questions[2].resolution.kind == "pending", "Clarification answered the question")
wait(delivery_settled, "Clarification delivery did not settle")
call_tool("question_reply", { question_id = second.id,
  revision = 1, message_id = "clarify-second", text = "Only new sessions are affected." })
wait(function() return #state.questions.questions[2].discussion == 2 end, "Explanation did not attach")
ask("required", "required")
wait(function() return #state.questions.questions == 3 end, "Required question did not publish")
local required = state.questions.questions[3]
assert(required.requirement == "required" and required.resolution.kind == "pending")
require("forge.views.harness.controller").reopen_question()
local picker = require("forge.views.picker")
local view = picker._state_for_test()
assert(vim.iter(view.spec.page_list):any(function(page) return page.title == "required · Required" end))
picker.close(false)
request("session.question.answer", { command = { operation_id = "answer-required", question_id = required.id, revision = 1,
  action = { kind = "answer", answer = { kind = "other", text = "Use tabs" } } } })
wait(function() return state.questions.questions[3].resolution.kind == "answered" end, "Required answer did not persist")
local snapshot = state_snapshot.get()
assert(#snapshot.questions.questions == 3 and snapshot.questions.questions[2].resolution.kind == "pending")
require("forge.views.harness.controller").reopen_question()
view = picker._state_for_test()
assert(#view.spec.page_list == 1 and view.spec.page_list[1].id == second.id)
local confirm = vim.api.nvim_buf_call(view.buf, function() return vim.fn.maparg("<CR>", "n", false, true) end)
assert(confirm.callback, "final question has no confirmation mapping")
confirm.callback()
wait(function() return state.questions.questions[2].resolution.kind == "answered"
  and not picker.is_open("questions") end, "final saved answer left the question picker open")
assert(not vim.api.nvim_win_is_valid(view.win), "final answer retained the picker window")
assert(not vim.api.nvim_buf_is_valid(view.buf), "final answer retained the picker buffer")
require("forge.views.harness.workspace").release(state)
client.host_stop()
assert(vim.wait(5000, function() return not client._client.process end, 10), "Host did not stop")
print("harness_questions_host: persistent questions, independent answers, clarification and required picker passed")
