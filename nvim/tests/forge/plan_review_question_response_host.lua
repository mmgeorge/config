vim.loader.enable(false)
local root = vim.fs.normalize(vim.fn.getcwd())
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
assert(vim.fn.mkdir(workspace, "p") == 1 and vim.fn.mkdir(data, "p") == 1)
vim.fn.writefile({ vim.json.encode({ checks = vim.empty_dict() }) }, workspace .. "/.forge.json")
local initialized = vim.system({ "git", "init", workspace }, { text = true }):wait(30000)
assert(initialized.code == 0, initialized.stderr)
local executable = data .. "/forge" .. (vim.fn.has("win32") == 1 and ".exe" or "")
assert(vim.uv.fs_copyfile(vim.g.forge_test_executable or require("forge.builder").binary_path(), executable))
local original_stdpath, original_notify = vim.fn.stdpath, vim.notify
vim.fn.stdpath = function(kind) return kind == "data" and data or original_stdpath(kind) end
local errors = {}
vim.notify = function(message, level)
  if level == vim.log.levels.ERROR then errors[#errors + 1] = tostring(message) end
end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = executable }) end)
  return function() end
end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local harness = require("forge.views.harness")
local controller = require("forge.views.harness.controller")
local comments = require("forge.draft_comments")
local mock = dofile(root .. "/nvim/tests/forge/support/mock_provider.lua")
local review_view = require("forge.views.plan_review.native_controller")
local answer = "This declaration defines the proposed interface"

local function await(predicate, message)
  assert(vim.wait(10000, function() return #errors > 0 or predicate() end, 10), message)
  assert(#errors == 0, table.concat(errors, "\n"))
end

local function text(scene)
  return table.concat(vim.api.nvim_buf_get_lines(scene.review.buf, 0, -1, false), "\n")
end

local function press(scene, keys)
  vim.api.nvim_set_current_win(scene.review.win)
  vim.api.nvim_feedkeys(vim.keycode(keys), "xt", false)
end

local function row_containing(scene, literal, first_row)
  for row, line in ipairs(vim.api.nvim_buf_get_lines(scene.review.buf, 0, -1, false)) do
    if row >= (first_row or 1) and line:find(literal, 1, true) then return row end
  end
  error("Missing plan text: " .. literal .. "\n" .. text(scene))
end

local function focus_source(scene, literal)
  vim.api.nvim_set_current_win(scene.review.win)
  vim.api.nvim_win_set_cursor(scene.review.win, { row_containing(scene, literal or "requested_change"), 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = scene.review.buf })
end

local function add_message(scene, key, body)
  local count = #comments.capture(scene.review.buf)
  assert(vim.fn.maparg(key, "n", false, true).buffer == 1, "missing PlanReview mapping for " .. key)
  press(scene, key)
  await(function() return #comments.capture(scene.review.buf) == count + 1 end, key .. " did not create a message")
  vim.cmd("stopinsert")
  press(scene, "<Esc>")
  press(scene, "i" .. body .. "<Esc>")
  await(function() return comments.capture(scene.review.buf)[count + 1].source.body == body end,
    "typing did not populate the new message")
  return comments.capture(scene.review.buf)[count + 1]
end

local function begin_answer(scene, body)
  scene.hold = mock.hold(scene.review.session_id, "plan_question", true)
  press(scene, ":write<CR>")
  local pending = scene.hold:wait()
  assert(pending.mode == "plan_question" and pending.prompt:find(body, 1, true),
    "response hold captured a different question")
  assert(pending.session_id ~= scene.review.session_id, "review question entered the main timeline")
  scene.answer_session_id = pending.session_id
  assert(scene.review.owner.saving, "question did not remain pending before release")
  assert(text(scene):find("Answering…", 1, true), "pending question omitted its progress row")
end

local function finish_answer(scene)
  scene.hold:release()
  scene.hold = nil
  await(function() return not scene.review.owner.saving end, "question response did not settle")
end

local function assert_answer(scene, question)
  local retained
  for _, annotation in ipairs(comments.capture(scene.review.buf)) do
    if tostring(annotation.id) == tostring(question.id) then retained = annotation break end
  end
  assert(retained, "saved question disappeared")
  assert(retained.source.start_line == question.source.start_line
      and retained.source.end_line == question.source.end_line,
    ("question moved from source lines %d-%d to %d-%d"):format(question.source.start_line,
      question.source.end_line, retained.source.start_line, retained.source.end_line))
  local question_row = row_containing(scene, question.source.body)
  local answer_row = row_containing(scene, answer, question_row + 1)
  assert(answer_row > question_row, "answer did not appear below its question")
  assert(not text(scene):find("Answering…", 1, true), "completed question retained its progress row")
  local reply = scene.review.owner.reply_by_id[tostring(question.id)]
  assert(reply and reply.question_body == question.source.body, "answer attached to a different question")
  assert(not vim.bo[scene.review.buf].modified, "completed answer left the saved question dirty")
  assert(vim.deep_equal(vim.fn.readfile(scene.plan.working_path, "b"), scene.canonical),
    "answer changed the canonical plan")
end

local function ask(scene, body)
  local question = add_message(scene, "A", body)
  begin_answer(scene, body)
  finish_answer(scene)
  assert_answer(scene, question)
  return question
end

local function leave_question(scene, id)
  press(scene, "gg0")
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = scene.review.buf })
  await(function()
    for _, range in ipairs(scene.review.owner.comment_state.range_list) do
      if tostring(range.annotation.id) == tostring(id) and range.compact then return true end
    end
    return false
  end, "leaving the question did not collapse its thread")
  return vim.api.nvim_win_get_cursor(scene.review.win)
end

local function focus_answer(scene)
  local row = row_containing(scene, answer)
  vim.api.nvim_win_set_cursor(scene.review.win, { row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = scene.review.buf })
  assert(not vim.bo[scene.review.buf].modifiable, "answer body became editable")
end

local function reopen(scene)
  press(scene, "q")
  await(function() return state.plan_review == nil end, "saved PlanReview did not close")
  review_view.open(scene.plan)
  await(function() return state.plan_review and state.plan_review.owner.ready end, "saved PlanReview did not reopen")
  scene.review = state.plan_review
end

local cases = {
  { name = "answer while question remains focused", run = function(scene)
    ask(scene, "Why does requested_change own this behavior?")
  end },
  { name = "answer arrives after cursor leaves question", run = function(scene)
    local question = add_message(scene, "A", "Why does requested_change own this behavior?")
    begin_answer(scene, question.source.body)
    local cursor = leave_question(scene, question.id)
    finish_answer(scene)
    assert_answer(scene, question)
    assert(vim.deep_equal(vim.api.nvim_win_get_cursor(scene.review.win), cursor), "answer moved the cursor back to its question")
    assert(comments.focus(scene.review.buf, question.id))
    assert_answer(scene, question)
  end },
  { name = "leave and return before answer arrives", run = function(scene)
    local question = add_message(scene, "A", "Why does requested_change own this behavior?")
    begin_answer(scene, question.source.body)
    leave_question(scene, question.id)
    assert(comments.focus(scene.review.buf, question.id))
    finish_answer(scene)
    assert_answer(scene, question)
  end },
  { name = "A on answer creates a linked follow-up", run = function(scene)
    local parent = ask(scene, "Why does requested_change own this behavior?")
    focus_answer(scene)
    local followup = add_message(scene, "A", "Which caller uses this interface?")
    assert(tostring(followup.parent_id) == tostring(parent.id), "A on the answer lost its parent")
    begin_answer(scene, followup.source.body)
    leave_question(scene, followup.id)
    finish_answer(scene)
    assert_answer(scene, parent)
    assert_answer(scene, followup)
    assert(row_containing(scene, answer) < row_containing(scene, followup.source.body),
      "parent answer moved beneath its follow-up")
    assert(comments.focus(scene.review.buf, followup.id))
    assert_answer(scene, followup)
    reopen(scene)
    assert_answer(scene, parent)
    assert_answer(scene, followup)
  end },
  { name = "C on answer saves a linked revision comment", run = function(scene)
    local parent = ask(scene, "Why does requested_change own this behavior?")
    focus_answer(scene)
    local comment = add_message(scene, "C", "Clarify the caller contract.")
    assert(tostring(comment.parent_id) == tostring(parent.id), "C on the answer lost its parent")
    assert(comment.kind ~= "question", "C created another question")
    press(scene, ":write<CR>")
    await(function() return not scene.review.owner.saving end, "revision comment did not save")
    reopen(scene)
    assert(text(scene):find("Clarify the caller contract.", 1, true), "reopening lost the revision comment")
    assert(text(scene):find(answer, 1, true), "reopening the comment lost the answer")
  end },
  { name = "editing pending question rejects its old answer", run = function(scene)
    local question = add_message(scene, "A", "Why does requested_change own this behavior?")
    begin_answer(scene, question.source.body)
    assert(comments.focus(scene.review.buf, question.id))
    press(scene, "0ccWhich caller owns the revised behavior?<Esc>")
    await(function() return comments.capture(scene.review.buf)[1].source.body == "Which caller owns the revised behavior?" end,
      "question edit was not captured")
    finish_answer(scene)
    assert(vim.bo[scene.review.buf].modified, "old answer cleared the newer draft")
    assert(not text(scene):find(answer, 1, true), "old answer appeared beneath changed question text")
    question = comments.capture(scene.review.buf)[1]
    begin_answer(scene, question.source.body)
    finish_answer(scene)
    assert_answer(scene, question)
  end },
  { name = "close and reopen preserves question and answer", run = function(scene)
    local question = ask(scene, "Why does requested_change own this behavior?")
    leave_question(scene, question.id)
    reopen(scene)
    assert_answer(scene, question)
    assert(comments.focus(scene.review.buf, question.id))
    assert_answer(scene, question)
  end },
  { name = "prose question stays on its requirement after reopening", run = function(scene)
    focus_source(scene, "Preserve the registry ownership boundary.")
    local question = ask(scene, "Why must the registry retain this ownership boundary?")
    leave_question(scene, question.id)
    reopen(scene)
    assert_answer(scene, question)
    assert(row_containing(scene, question.source.body)
        > row_containing(scene, "Preserve the registry ownership boundary."),
      "question moved before the requirement it describes")
    reopen(scene)
    assert_answer(scene, question)
  end },
  { name = "prose comment preserves both selected range endpoints", run = function(scene)
    focus_source(scene, "Requirements")
    press(scene, "V<Down>")
    local comment = add_message(scene, "C", "Keep this requirement attached to its heading.")
    assert(comment.source.end_line > comment.source.start_line, "comment did not capture the selected range")
    press(scene, ":write<CR>")
    await(function() return not scene.review.owner.saving end, "range comment did not save")
    reopen(scene)
    local retained = comments.capture(scene.review.buf)[1]
    assert(vim.deep_equal(retained.source, comment.source), "reopening changed the comment's source range")
    assert(row_containing(scene, comment.source.body)
        > row_containing(scene, "Preserve the registry ownership boundary."),
      "range comment moved before the end of its selected source")
  end },
  { name = "close during pending answer retains review then reopens", run = function(scene)
    local question = add_message(scene, "A", "Why does requested_change own this behavior?")
    begin_answer(scene, question.source.body)
    press(scene, "q")
    assert(state.plan_review == scene.review, "closing dropped a pending question")
    assert(#errors == 1 and errors[1]:find("explicit operation in progress", 1, true), "pending close omitted its explanation")
    errors = {}
    leave_question(scene, question.id)
    finish_answer(scene)
    reopen(scene)
    assert_answer(scene, question)
  end },
}

vim.api.nvim_set_current_dir(workspace)
require("forge").setup({ diff_logging = false, harness_logging = false, harness = { backend = "mock" } })
local failures = {}
for _, case in ipairs(cases) do
  errors = {}
  local scene = {}
  local success, failure = xpcall(function()
    harness.new_session(case.name)
    await(function()
      state = require("forge.session").harness
      return state.session and state.session.name == case.name and state.presentation and state.presentation.ready
    end,
      "isolated Harness session did not open")
    vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "/plan build the feature" })
    controller.submit()
    await(function() return not state.busy and state.active_plan end, "mock plan did not arrive")
    scene.plan = state.active_plan
    scene.canonical = vim.fn.readfile(scene.plan.working_path, "b")
    review_view.open(scene.plan)
    await(function() return state.plan_review and state.plan_review.owner.ready end, "PlanReview did not open")
    scene.review = state.plan_review
    focus_source(scene)
    case.run(scene)
  end, debug.traceback)
  if not success then
    local diagnostic = case.name .. ": " .. tostring(failure)
    if scene.review and vim.api.nvim_buf_is_valid(scene.review.buf) then diagnostic = diagnostic .. "\n" .. text(scene) end
    failures[#failures + 1] = diagnostic
    print("FAIL: " .. case.name)
  else
    print("PASS: " .. case.name)
  end
  if scene.hold then pcall(function() scene.hold:cancel() end) end
  if scene.review and vim.api.nvim_buf_is_valid(scene.review.buf) then
    vim.wait(10000, function() return not scene.review.owner.saving end, 10)
    vim.bo[scene.review.buf].modified = false
    pcall(function() scene.review.command_set.action_by_id.close.run({}) end)
  end
  pcall(function() require("forge.views.harness.workspace").release(state) end)
  pcall(function() if state.presentation then state.presentation.close() end end)
end
client.stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(root)
vim.fn.stdpath, vim.notify = original_stdpath, original_notify
vim.fn.delete(workspace, "rf")
vim.fn.delete(data, "rf")
assert(collected, "Plan question test host was not collected")
assert(#failures == 0, table.concat(failures, "\n\n"))
print("plan_review_question_response_host: " .. #cases .. " scenarios passed")
