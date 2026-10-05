vim.loader.enable(false)
local client = require("forge.client")
local pending = {}
client.host_generation = function() return 1 end
client.host_accepting = function() return true end
client.request_for = function(_, method, params, callback)
  pending[#pending + 1] = { method = method, params = vim.deepcopy(params), callback = callback }
end
local buffer = vim.api.nvim_create_buf(false, true)
vim.api.nvim_buf_set_name(buffer, vim.fn.tempname() .. ".md")
vim.api.nvim_win_set_buf(0, buffer)
local notices = {}
local owner = require("forge.views.plan_review.document").attach({ session_id = "session", buffer = buffer,
  window = vim.api.nvim_get_current_win(), plan = { id = "plan", review_digest = "canonical" },
  notice = function(message) notices[#notices + 1] = message end }, function(_, failure) assert(not failure, failure) end)
local opened = { saved_source_digest = "saved", annotation = {}, source_row = {
  { id = "source", target = "source", text = "pub struct State;", source_line = 1,
    block = "plan:source", position = { row = 0, column = 0 }, metadata = {} },
}, snapshot = { document = owner.document, revision = 0, block = {
  { id = "plan:source", text = { "pub struct State;" }, metadata = { target = {}, editable_region = {}, decoration = {} } },
} } }
pending[1].callback(opened)
local comments = require("forge.draft_comments")
local function text() return table.concat(vim.api.nvim_buf_get_lines(buffer, 0, -1, false), "\n") end
local function replace_body(body)
  local captured = comments.capture(buffer)[1]
  assert(comments.focus(buffer, captured.id))
  local row = vim.api.nvim_win_get_cursor(0)[1] - 1
  vim.api.nvim_buf_set_lines(buffer, row, row + 1, false, { body })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = buffer })
end
local function leave_and_return(expected)
  vim.api.nvim_win_set_cursor(0, { 1, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buffer })
  assert(text():find(expected, 1, true), "leaving question lost " .. expected)
  for row, line in ipairs(vim.api.nvim_buf_get_lines(buffer, 0, -1, false)) do
    if line:find("Why own this state?", 1, true) then
      vim.api.nvim_win_set_cursor(0, { row, 0 })
      vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buffer })
      break
    end
  end
  assert(text():find(expected, 1, true), "returning to question lost " .. expected)
end
owner.action("question", function() end)
replace_body("Why own this state?")
assert(text():find("Plan question", 1, true), "question used the ordinary comment heading")
vim.cmd("write")
assert(pending[2].method == "plan.questions.answer", "saving did not ask the LLM")
assert(pending[2].params.draft_annotation[1].kind == "question", "question kind was lost in the request")
assert(text():find("Answering…", 1, true), "request omitted persistent progress")
leave_and_return("Answering…")
local function response(question, answer)
  return { annotation = { { id = "1", kind = "question", source = { start_line = 1, end_line = 1, body = question },
    reply = { question_body = question, body = answer, duration_ms = 3210 } } } }
end
pending[2].callback(response("Why own this state?", "It gives one component ownership. Callers use its public interface."))
assert(owner.ready and not owner.closed, "answer closed the review")
assert(not vim.bo[buffer].modified, "answer made a saved question dirty")
assert(text():find("Thought for 3 seconds", 1, true) and text():find("It gives one component ownership.", 1, true), "answer did not attach with its recorded duration")
assert(comments.capture(buffer)[1].source.body == "Why own this state?", "reply entered the saved question body")
leave_and_return("It gives one component ownership.")
vim.cmd("write")
assert(pending[3].params.operation == "plan_save_annotations", "saving an answered question asked it again")
pending[3].callback({ saved = true })
replace_body("Why this interface?")
vim.cmd("write")
assert(pending[4].method == "plan.questions.answer", "editing an answered question did not ask again")
replace_body("Newer unsaved question?")
pending[4].callback(response("Why this interface?", "This is an older answer."))
assert(vim.bo[buffer].modified and comments.capture(buffer)[1].source.body == "Newer unsaved question?", "late answer overwrote newer typing")
assert(not text():find("This is an older answer.", 1, true), "late answer appeared beneath a different question")
vim.cmd("write")
pending[5].callback(nil, "Provider unavailable")
assert(#notices == 1 and notices[1] == "Provider unavailable", "provider failure was not surfaced")
assert(vim.bo[buffer].modified and not owner.saving, "provider failure lost the draft or blocked retry")
vim.cmd("write")
assert(pending[6].method == "plan.questions.answer", "retry did not resubmit the unanswered question")
pending[6].callback(response("Newer unsaved question?", "The interface establishes the ownership boundary."))
assert(not vim.bo[buffer].modified and text():find("ownership boundary.", 1, true), "retry did not retain the answer")
owner.action("toggle_public", function(_, failure) assert(not failure, failure) end)
local refreshed = vim.deepcopy(opened)
refreshed.patch = {}
refreshed.annotation = response("Newer unsaved question?", "unused").annotation
refreshed.annotation[1].reply = nil
pending[7].callback(refreshed)
assert(text():find("ownership boundary.", 1, true), "source refresh discarded the retained answer")
for row, line in ipairs(vim.api.nvim_buf_get_lines(buffer, 0, -1, false)) do
  if line:find("ownership boundary.", 1, true) then
    vim.api.nvim_win_set_cursor(0, { row, 0 })
    vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buffer })
    owner.sync_editability()
    assert(not vim.bo[buffer].modifiable, "view adapter made the focused answer editable")
    break
  end
end
owner.action("question", function() end)
local captured = comments.capture(buffer)
assert(#captured == 2 and captured[2].parent_id == "1", "A on the answer did not create a linked follow-up")
local row = vim.api.nvim_win_get_cursor(0)[1] - 1
vim.api.nvim_buf_set_lines(buffer, row, row + 1, false, { "Which getter should I use?" })
vim.api.nvim_exec_autocmds("TextChanged", { buffer = buffer })
vim.cmd("write")
assert(pending[8].method == "plan.questions.answer" and pending[8].params.draft_annotation[2].parent_id == "1",
  "question request omitted the follow-up identity")
local threaded = response("Newer unsaved question?", "The interface establishes the ownership boundary.")
threaded.annotation[2] = { id = "2", kind = "question", parent_id = "1",
  source = { start_line = 1, end_line = 1, body = "Which getter should I use?" },
  reply = { question_body = "Which getter should I use?", body = "Use the state getter.", duration_ms = 1000 } }
pending[8].callback(threaded)
assert(not vim.bo[buffer].modified and text():find("Use the state getter.", 1, true), "follow-up answer was not retained")
assert(text():find("Thought for 1 second", 1, true), "one-second duration was not rendered correctly")
for row, line in ipairs(vim.api.nvim_buf_get_lines(buffer, 0, -1, false)) do
  if line:find("Use the state getter.", 1, true) then
    vim.api.nvim_win_set_cursor(0, { row, 0 })
    vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buffer })
    break
  end
end
owner.action("comment", function() end)
assert(comments.capture(buffer)[3].parent_id == "2", "C on a follow-up answer lost the conversation")
owner.close()
vim.api.nvim_buf_delete(buffer, { force = true })
print("plan_review_questions: save, answer, edited text, late response, failure, and retry passed")
