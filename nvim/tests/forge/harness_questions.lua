vim.opt.runtimepath:prepend("nvim")
vim.loader.enable(false)
vim.o.columns = 120
vim.o.lines = 45
require("forge").setup({ harness = { provider = "mock" } })
local questions = require("forge.views.harness.questions")
local picker = require("forge.views.picker")
local client = require("forge.client")
local original_request = client.request
local input_state = require("forge.views.harness.input_state")
local original_submit = input_state.submit
local submitted = {}
input_state.submit = function(target_state, text, attachments, intent, configuration, callback, retracted, target)
  submitted[#submitted + 1] = { state = target_state, text = text, attachments = attachments,
    intent = intent, target = target, callback = callback }
end
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
assert(instance.spec.page_list[2].title == "second · Optional")
assert(instance.spec.page_list[2].content_list[1].text == "second · Optional")
assert(questions.count(state) == 2)
assert(#instance.spec.page_list[1].action_list == 0, "required question can be dismissed")
assert(not vim.inspect(vim.api.nvim_win_get_config(instance.win).footer):find("d ignore", 1, true),
  "required question showed an ignore shortcut")
press("<Tab>")
assert(picker._state_for_test().state.page_index == 2, "Tab did not select the next question")
assert(vim.inspect(vim.api.nvim_win_get_config(instance.win).footer):find("d ignore", 1, true),
  "optional question omitted its ignore shortcut")
press(require("forge.shared.keymaps").view_keys_for("picker", "help", instance.spec.keymaps)[1])
assert(table.concat(vim.api.nvim_buf_get_lines(0, 0, -1, false), "\n"):find("Ignore optional question", 1, true),
  "optional question omitted ignore from help")
vim.fn.maparg("q", "n", false, true).callback()
press("<Tab>")
assert(picker._state_for_test().state.page_index == 1, "Tab did not cycle question tabs")
assert(not vim.inspect(vim.api.nvim_win_get_config(picker._state_for_test().win).footer):find("d ignore", 1, true),
  "switching to required retained the optional ignore shortcut")
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
assert(instance.spec.page_list[1].title == "second · Optional", "optional question lost its label")
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

-- Cancelling work leaves retained optional questions answerable while idle.
state.busy = false
state.status = { kind = "idle" }
state.questions = { revision = 7, questions = { question("cancelled-work", "optional") } }
questions.open(state, host)
assert(picker.is_open("questions"), "idle cancellation hid an unanswered optional question")
press("<CR>")
local idle_answer = sent[#sent]
assert(idle_answer.request.params.command.question_id == "cancelled-work")
local idle_answered = vim.deepcopy(state.questions)
idle_answered.revision = 8
idle_answered.questions[1].resolution = { kind = "answered" }
idle_answer.callback(idle_answered)
assert(not picker.is_open("questions") and questions.count(state) == 0,
  "idle optional answer did not commit and close the picker")

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
state.questions = { revision = 10, questions = { question("ignored", "optional") } }
questions.open(state, host)
press("d")
local ignored = sent[#sent]
assert(ignored.request.params.command.action.kind == "dismiss", "ignore did not submit dismissal")
local dismissed = vim.deepcopy(state.questions)
dismissed.revision = 11
dismissed.questions[1].resolution = { kind = "dismissed" }
ignored.callback(dismissed)
assert(not picker.is_open("questions"), "ignoring the final optional question left its picker open")

-- Repeated session snapshots leave a live Other editor untouched, even in Insert mode.
state.questions = { revision = 12, questions = { question("editing", "optional") } }
questions.open(state, host)
press("o")
press("<CR>")
instance = picker._state_for_test()
local editing_buffer, editing_window = instance.input_buf, instance.input_win
assert(editing_window and vim.api.nvim_win_is_valid(editing_window), "Other did not open the answer editor")
local input_leaves = 0
vim.api.nvim_create_autocmd("InsertLeave", { buffer = editing_buffer, callback = function()
  input_leaves = input_leaves + 1
end })
vim.wait(20, function() return false end, 5)
local verified_insert_refresh = false
vim.keymap.set("i", "<F12>", function()
  assert(vim.api.nvim_get_mode().mode:sub(1, 1) == "i", "regression must run while inserting text")
  local cursor = vim.api.nvim_win_get_cursor(editing_window)
  local changedtick = vim.api.nvim_buf_get_changedtick(editing_buffer)
  local previous_leaves = input_leaves
  local original_update = picker.update
  local updates = 0
  picker.update = function(spec)
    updates = updates + 1
    return original_update(spec)
  end
  local ok, failure = pcall(function()
    for _ = 1, 20 do questions.receive(state, vim.deepcopy(state.questions)) end
    assert(updates == 0, "unchanged session snapshots refreshed the question picker")
    local metadata_only = vim.deepcopy(state.questions)
    metadata_only.revision = 13
    questions.receive(state, metadata_only)
    assert(state.questions.revision == 13 and updates == 0, "revision-only update refreshed the question picker")
    local incoming = vim.deepcopy(state.questions)
    incoming.revision = 14
    incoming.questions[#incoming.questions + 1] = question("arrived", "optional")
    questions.receive(state, incoming)
    assert(updates == 1, "a new question did not update the picker exactly once")
    local refreshed = picker._state_for_test()
    assert(refreshed.input_buf == editing_buffer and refreshed.input_win == editing_window,
      "a new question recreated the answer editor")
    assert(vim.api.nvim_get_current_win() == editing_window and vim.api.nvim_get_mode().mode:sub(1, 1) == "i",
      "question refresh interrupted Insert mode")
    assert(vim.deep_equal(vim.api.nvim_win_get_cursor(editing_window), cursor), "question refresh moved the input cursor")
    assert(vim.api.nvim_buf_get_changedtick(editing_buffer) == changedtick, "question refresh rewrote the draft")
    assert(input_leaves == previous_leaves, "question refresh exited Insert mode")
  end)
  picker.update = original_update
  assert(ok, failure)
  verified_insert_refresh = true
end, { buffer = editing_buffer })
vim.api.nvim_feedkeys(vim.keycode("iabc<F12>def<Esc>"), "xt", false)
assert(verified_insert_refresh, "Insert-mode refresh regression did not execute")
assert(vim.api.nvim_buf_get_lines(editing_buffer, 0, -1, false)[1] == "abcdef",
  "typing across a question refresh lost or redirected characters: " .. vim.inspect(vim.api.nvim_buf_get_lines(editing_buffer, 0, -1, false)))
local submit_other = vim.api.nvim_buf_call(editing_buffer, function()
  return vim.fn.maparg("<C-s>", "n", false, true).callback
end)
assert(submit_other, "retained answer editor lost its submit binding")
submit_other()
local other_answer = sent[#sent]
assert(other_answer.request.params.command.question_id == "editing")
assert(other_answer.request.params.command.action.answer.kind == "other")
assert(other_answer.request.params.command.action.answer.text == "abcdef")
local saved_other = vim.deepcopy(state.questions)
saved_other.revision = 15
saved_other.questions[1].resolution = { kind = "answered" }
other_answer.callback(saved_other)
assert(not vim.api.nvim_win_is_valid(editing_window), "resolved question retained its obsolete editor")
assert(picker._state_for_test().spec.page_list[1].id == "arrived")
picker.close(false)
-- Choosing a structured answer must not resubmit the retained clarification draft.
state.questions = { revision = 16, questions = { question("clarify-then-answer", "required") } }
state.question_picker_resume = nil
questions.open(state, host)
press("a")
press("<CR>")
instance = picker._state_for_test()
local clarification = "Show one example before I choose."
vim.api.nvim_buf_set_lines(instance.input_buf, 0, -1, false, { clarification })
local function press_question_input(key)
  local current = picker._state_for_test()
  local mapping = vim.api.nvim_buf_call(current.input_buf, function()
    return vim.fn.maparg(key, "n", false, true)
  end)
  assert(mapping.callback, "missing question input key: " .. key)
  mapping.callback()
end
press_question_input("<C-s>")
local clarification_request = submitted[#submitted]
assert(clarification_request.text == clarification and clarification_request.intent == "auto")
assert(clarification_request.target.owner == "question"
  and clarification_request.target.question_id == "clarify-then-answer"
  and clarification_request.target.revision == 1)
local explanation = vim.deepcopy(state.questions)
explanation.revision = 17
explanation.questions[1].discussion = { { kind = "reply", text = "Tabs are compact; a sidebar is wide." } }
clarification_request.callback(nil)
questions.receive(state, explanation)
instance = picker._state_for_test()
assert(instance.input_kind == "ask" and instance.input_buf,
  "clarification response discarded the retained Ask editor")
assert(vim.api.nvim_buf_get_lines(instance.input_buf, 0, -1, false)[1] == clarification)
local before_answer = #sent
press_question_input("<C-f>")
press(require("forge.infra.config").options.picker.choice_keys[1])
press("<CR>")
local selected_answer = sent[#sent]
local selected_action = selected_answer.request.params.command.action
assert(#sent == before_answer + 1 and selected_action.kind == "answer",
  "confirming a selected answer resubmitted the Ask draft")
assert(selected_action.answer.kind == "selected" and selected_action.answer.option_id == "tabs")
assert(selected_action.answer.feedback == nil, "Ask text leaked into answer feedback")
assert(state.question_drafts["clarify-then-answer"].ask == clarification,
  "selecting an answer lost the independent clarification draft")
local resolved = vim.deepcopy(state.questions)
resolved.revision = 18
resolved.questions[1].resolution = { kind = "answered" }
selected_answer.callback(resolved)
assert(not picker.is_open("questions"), "selected answer did not resolve the question")

-- Explicit answer feedback still follows the selected option from the options pane.
state.questions = { revision = 19, questions = { question("answer-with-feedback", "required") } }
state.question_picker_resume = nil
questions.open(state, host)
press("<C-f>")
instance = picker._state_for_test()
assert(instance.input_kind == "feedback", "answer feedback used a clarification editor")
vim.api.nvim_buf_set_lines(instance.input_buf, 0, -1, false, { "Keep the compact layout." })
press_question_input("<C-f>")
press("<CR>")
local feedback_answer = sent[#sent].request.params.command.action
assert(feedback_answer.kind == "answer" and feedback_answer.answer.option_id == "tabs")
assert(feedback_answer.answer.feedback == "Keep the compact layout.",
  "confirming a structured answer discarded its explicit feedback")
picker.close(false)
client.request = original_request
input_state.submit = original_submit
print("harness questions tests passed")
