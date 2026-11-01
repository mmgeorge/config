vim.opt.runtimepath:prepend("nvim")
vim.loader.enable(false)
vim.o.columns = 120
vim.o.lines = 45
require("forge").setup({ harness = { backend = "mock" } })
local questions = require("forge.views.harness.questions")
local picker = require("forge.views.picker")
local client = require("forge.client")
local original_request = client.request
local sent = {}
client.request = function(request, callback) sent[#sent + 1] = { request = request, callback = callback } end
local function question(id, requirement)
  return { id = id, requirement = requirement, content = { revision = 1, header = id, question = "Which layout?", options = {
    { id = "tabs", label = "Tabs", description = "Compact" }, { id = "sidebar", label = "Sidebar", description = "Wide" },
  } }, resolution = { kind = "pending" }, discussion = {} }
end
local state = { session = { id = "question-session" }, questions = { revision = 1, questions = { question("first", "required"), question("second", "optional") } } }
local host = { window_list = { vim.api.nvim_get_current_win() }, control_win = vim.api.nvim_get_current_win() }
local function press(key)
  local instance = picker._state_for_test()
  local mapping = vim.api.nvim_buf_call(instance.buf, function() return vim.fn.maparg(key, "n", false, true) end)
  assert(mapping.callback, "missing picker key: " .. key)
  mapping.callback()
end
questions.open(state, host)
local instance = picker._state_for_test()
assert(instance.spec.page_list[1].title == "first · Required")
assert(instance.spec.page_list[1].content_list[1].text:find("Required", 1, true))
assert(instance.spec.page_list[1].content_list[2].text:find("Work is paused", 1, true))
assert(questions.count(state) == 2)
assert(#instance.spec.page_list[1].action_list == 0, "required question can be dismissed")
press("<Tab>")
assert(picker._state_for_test().state.page_index == 2, "Tab did not select the next question")
press("<Tab>")
assert(picker._state_for_test().state.page_index == 1, "Tab did not cycle question tabs")
press("<S-Tab>")
assert(picker._state_for_test().state.page_index == 2, "Shift-Tab did not select the previous question")
press("<S-Tab>")
press("<CR>")
assert(#sent == 1 and sent[1].request.params.command.action.answer.option_id == "tabs")
assert(questions.count(state) == 2, "uncommitted answer changed pending count")
state.questions.questions[1].resolution = { kind = "answered" }
sent[1].callback({ revision = 2, questions = state.questions.questions })
instance = picker._state_for_test()
assert(#instance.spec.page_list == 1 and instance.spec.page_list[1].id == "second")
assert(instance.spec.page_list[1].title == "second", "optional question marked Required")
assert(questions.count(state) == 1)
-- Arrival preserves the selected question and an open clarification editor.
instance.state.selected_index_by_page.second = 4
picker.update(instance.spec)
press("<CR>")
instance = picker._state_for_test()
assert(instance.input_buf and vim.api.nvim_buf_is_valid(instance.input_buf))
vim.api.nvim_buf_set_lines(instance.input_buf, 0, -1, false, { "Does this change existing sessions?" })
local focused = vim.api.nvim_get_current_win()
local next_snapshot = vim.deepcopy(state.questions)
next_snapshot.revision = 3
next_snapshot.questions[#next_snapshot.questions + 1] = question("third", "optional")
questions.receive(state, next_snapshot)
instance = picker._state_for_test()
assert(instance.spec.page_list[instance.state.page_index].id == "second")
assert(vim.api.nvim_get_current_win() == instance.input_win, "incoming question stole input focus")
assert(vim.api.nvim_buf_get_lines(instance.input_buf, 0, -1, false)[1] == "Does this change existing sessions?", "incoming question discarded draft")
picker.close()
assert(state.question_picker_resume.draft_by_page.second == "Does this change existing sessions?")
questions.open(state, host)
instance = picker._state_for_test()
assert(instance.state.draft_by_page.second == "Does this change existing sessions?")
-- Older publication cannot undo newer state.
questions.receive(state, { revision = 1, questions = {} })
assert(questions.count(state) == 2)
picker.close(false)

-- Each saved answer advances the picker; only the final committed answer closes it.
state.questions = { revision = 4, questions = { question("first", "required"), question("second", "optional") } }
state.question_picker_resume = nil
questions.open(state, host)
press("<CR>")
local first_answer = sent[#sent]
local answered = vim.deepcopy(state.questions)
answered.revision = 5
answered.questions[1].resolution = { kind = "answered" }
first_answer.callback(answered)
assert(picker.is_open("questions"), "saving the first answer closed a pending question")
assert(picker._state_for_test().spec.page_list[1].id == "second")
press("<CR>")
local last_answer = sent[#sent]
instance = picker._state_for_test()
local picker_window, picker_buffer = instance.win, instance.buf
assert(picker.is_open("questions"), "submitting the last answer closed before it was saved")
local notifications = require("forge.infra.notifications")
local original_error = notifications.error
notifications.error = function() end
last_answer.callback(nil, "Answer could not be saved")
notifications.error = original_error
assert(picker.is_open("questions") and questions.count(state) == 1,
  "failed final answer closed the picker or resolved the question")
assert(state.question_failure == "Answer could not be saved")
press("<CR>")
last_answer = sent[#sent]
answered = vim.deepcopy(state.questions)
answered.revision = 6
answered.questions[2].resolution = { kind = "answered" }
last_answer.callback(answered)
assert(not picker.is_open("questions"), "final saved answer left an empty picker open")
assert(not vim.api.nvim_win_is_valid(picker_window), "completed question window survived")
assert(not vim.api.nvim_buf_is_valid(picker_buffer), "completed question buffer survived")
assert(vim.api.nvim_get_current_win() == host.control_win, "completion did not restore host focus")
questions.open(state, host)
assert(not picker.is_open("questions"), "reopening completed answers created an empty picker")

-- Host publications also close the picker, but explicit history remains available.
state.questions = { revision = 7, questions = { question("withdrawn", "optional") } }
questions.open(state, host)
questions.receive(state, { revision = 8, questions = {} })
assert(not picker.is_open("questions"), "withdrawing the final pending question left the picker open")
state.questions = answered
state.question_show_history = true
questions.open(state, host)
questions.receive(state, { revision = 9, questions = answered.questions })
assert(picker.is_open("questions"), "history closed despite being explicitly requested")
press("H")
assert(not picker.is_open("questions"), "leaving completed history displayed the empty page")
client.request = original_request
print("harness questions tests passed")
