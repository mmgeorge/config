local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
local keymap = dofile("nvim/lua/forge/tests/support/keymap.lua")
local git_repository = dofile("nvim/lua/forge/tests/support/git_repository.lua")
local host_executable = dofile("nvim/lua/forge/tests/support/host_executable.lua")
local root = vim.fs.normalize(vim.fn.getcwd())
-- The unscripted mock answers a plan question by echoing its prompt. Response scripts bind to the
-- root session, while each review question runs in its own fork, so the echo marks the answer.
local answer = "Mock response: Answer this Plan Review question"
local mock_requirement = "Preserve the requested behavior at its owning boundary."
-- Each wait spans a host round trip, such as a fork's answer or a review reopening.
local host_timeout_ms = 4000

local workspace, data, executable, original_stdpath, original_notify
local client, state, harness, controller, comments, mock, review_view
local errors = {}
local scene = {}

local function row_containing(scene, literal, first_row)
  for row, line in ipairs(buffer.lines(scene.review.buf)) do
    if row >= (first_row or 1) and line:find(literal, 1, true) then return row end
  end
  error("Missing plan text: " .. literal .. "\n" .. buffer.text(scene.review.buf))
end

local function focus_source(scene, literal)
  vim.api.nvim_set_current_win(scene.review.win)
  vim.api.nvim_win_set_cursor(scene.review.win, { row_containing(scene, literal or "requested_change"), 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = scene.review.buf })
end

local function add_message(scene, key, body)
  local count = #comments.capture(scene.review.buf)
  assert(keymap.get(key, { buffer = scene.review.buf }).buffer == 1, "missing PlanReview mapping for " .. key)
  vim.api.nvim_set_current_win(scene.review.win)
  keymap.feed(key)
  wait_until(function() return #comments.capture(scene.review.buf) == count + 1 end, key .. " did not create a message",
    { errors = errors, timeout_ms = host_timeout_ms })
  vim.cmd("stopinsert")
  vim.api.nvim_set_current_win(scene.review.win)
  keymap.feed("<Esc>")
  vim.api.nvim_set_current_win(scene.review.win)
  keymap.feed("i" .. body .. "<Esc>")
  wait_until(function() return comments.capture(scene.review.buf)[count + 1].source.body == body end,
    "typing did not populate the new message", { errors = errors, timeout_ms = host_timeout_ms })
  return comments.capture(scene.review.buf)[count + 1]
end

local function begin_answer(scene, body)
  scene.hold = mock.hold(scene.review.session_id, "plan_question", true)
  vim.api.nvim_set_current_win(scene.review.win)
  keymap.feed(":write<CR>")
  local pending = scene.hold:wait()
  assert(pending.mode == "planning" and pending.prompt:find(body, 1, true),
    "response hold captured a different question")
  assert(pending.session_id ~= scene.review.session_id, "review question entered the main timeline")
  scene.answer_session_id = pending.session_id
  assert(scene.review.owner.saving, "question did not remain pending before release")
  assert(buffer.text(scene.review.buf):find("Generating response", 1, true), "pending question omitted its progress row")
  local model = scene.expected_model or state.session.resolved_model or state.session.model
  -- PlanReview presents an unresolved default model as a resolving state.
  if model == "default" then model = "Resolving model" end
  local effort = scene.expected_effort or (state.session.effort ~= "" and state.session.effort or "Default")
  assert(buffer.text(scene.review.buf):find("Generating response · " .. model .. " · Thinking: " .. effort, 1, true),
    "pending question omitted its captured model and thinking")
end

local function finish_answer(scene)
  scene.hold:release()
  scene.hold = nil
  wait_until(function() return not scene.review.owner.saving end, "question response did not settle",
    { errors = errors, timeout_ms = host_timeout_ms })
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
  assert(not buffer.text(scene.review.buf):find("Generating response", 1, true), "completed question retained its progress row")
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
  vim.api.nvim_set_current_win(scene.review.win)
  keymap.feed("gg0")
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = scene.review.buf })
  wait_until(function()
    for _, range in ipairs(scene.review.owner.comment_state.range_list) do
      if tostring(range.annotation.id) == tostring(id) and range.compact then return true end
    end
    return false
  end, "leaving the question did not collapse its thread", { errors = errors, timeout_ms = host_timeout_ms })
  return vim.api.nvim_win_get_cursor(scene.review.win)
end

local function focus_answer(scene)
  local row = row_containing(scene, answer)
  vim.api.nvim_win_set_cursor(scene.review.win, { row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = scene.review.buf })
  assert(not vim.bo[scene.review.buf].modifiable, "answer body became editable")
end

local function reopen(scene)
  vim.api.nvim_set_current_win(scene.review.win)
  keymap.feed("q")
  wait_until(function() return state.plan_review == nil end, "saved PlanReview did not close",
    { errors = errors, timeout_ms = host_timeout_ms })
  review_view.open(scene.plan)
  wait_until(function() return state.plan_review and state.plan_review.owner.ready end, "saved PlanReview did not reopen",
    { errors = errors, timeout_ms = host_timeout_ms })
  scene.review = state.plan_review
end

local hooks = isolation.hooks({
  pre_case = function()
    errors = {}
    scene = {}
    local case_name = MiniTest.current.case.desc[2]
    harness.new_session(case_name)
    wait_until(function()
      state = require("forge.session").harness
      return state.session and state.session.name == case_name and state.presentation and state.presentation.ready
    end, "isolated Harness session did not open", { errors = errors, timeout_ms = host_timeout_ms })
    buffer.set(state.composer_buf, { "/plan build the feature" })
    controller.submit()
    wait_until(function()
      return not require("forge.views.harness.activity").busy(state) and state.active_plan and type(state.active_plan.review_digest) == "string"
    end, "submitted mock plan did not arrive", { errors = errors, timeout_ms = host_timeout_ms })
    scene.plan = state.active_plan
    scene.canonical = vim.fn.readfile(scene.plan.working_path, "b")
    review_view.open(scene.plan)
    wait_until(function() return state.plan_review and state.plan_review.owner.ready end, "PlanReview did not open",
      { errors = errors, timeout_ms = host_timeout_ms })
    scene.review = state.plan_review
    focus_source(scene)
  end,
  post_case = function()
    if #MiniTest.current.case.exec.fails > 0 then
      if #errors > 0 then MiniTest.add_note(table.concat(errors, "\n")) end
      if scene.review and vim.api.nvim_buf_is_valid(scene.review.buf) then MiniTest.add_note(buffer.text(scene.review.buf)) end
    end
    if scene.hold then pcall(function() scene.hold:cancel() end) end
    if scene.review and vim.api.nvim_buf_is_valid(scene.review.buf) then
      vim.wait(host_timeout_ms, function() return not scene.review.owner.saving end, 5)
      vim.bo[scene.review.buf].modified = false
      pcall(function() scene.review.command_set.action_by_id.close.run({}) end)
    end
    pcall(function() require("forge.views.harness.workspace").release(state) end)
    pcall(function() if state.presentation then state.presentation.close() end end)
  end,
})

hooks.pre_once = function()
  workspace, data = vim.fn.tempname(), vim.fn.tempname()
  assert(vim.fn.mkdir(workspace, "p") == 1 and vim.fn.mkdir(data, "p") == 1)
  vim.fn.writefile({ vim.json.encode({ checks = vim.empty_dict() }) }, workspace .. "/.forge.json")
  git_repository.create({ path = workspace })
  executable = host_executable()
  original_stdpath, original_notify = vim.fn.stdpath, vim.notify
  vim.fn.stdpath = function(kind) return kind == "data" and data or original_stdpath(kind) end
  vim.notify = function(message, level)
    if level == vim.log.levels.ERROR then errors[#errors + 1] = tostring(message) end
  end
  package.loaded["forge.builder"] = { ensure = function(callback)
    vim.schedule(function() callback({ ok = true, path = executable }) end)
    return function() end
  end }
  client = require("forge.client")
  client._host_set_launcher_for_test(vim.system)
  state = require("forge.session").harness
  harness = require("forge.views.harness")
  controller = require("forge.views.harness.controller")
  comments = require("forge.draft_comments")
  mock = dofile(root .. "/nvim/lua/forge/tests/support/mock_provider.lua")
  review_view = require("forge.views.plan_review.native_controller")
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = false, harness = { provider = "mock" } })
end

hooks.post_once = function()
  local collected = false
  if client then
    client.host_stop()
    collected = vim.wait(5000, function() return client._client.process == nil end, 5)
  end
  vim.api.nvim_set_current_dir(root)
  if original_stdpath then vim.fn.stdpath, vim.notify = original_stdpath, original_notify end
  if workspace then vim.fn.delete(workspace, "rf") end
  if data then vim.fn.delete(data, "rf") end
  assert(collected, "Plan question test host was not collected")
end

local T = MiniTest.new_set({ hooks = hooks })

T["later question completes while the first fork is pending"] = function()
  local first = add_message(scene, "A", "Why does the first caller own this behavior?")
  begin_answer(scene, first.source.body)
  local first_hold = scene.hold
  local first_session = scene.answer_session_id
  focus_source(scene)
  local second = add_message(scene, "A", "Which second caller uses this interface?")
  begin_answer(scene, second.source.body)
  assert(scene.answer_session_id ~= first_session, "questions shared one fork")
  local second_hold = scene.hold
  scene.hold = first_hold
  second_hold:release()
  wait_until(function() return scene.review.owner.reply_by_id[tostring(second.id)] ~= nil end,
    "second answer waited for the first fork", { errors = errors, timeout_ms = host_timeout_ms })
  assert(scene.review.owner.reply_by_id[tostring(first.id)] == nil, "first fork did not remain pending")
  assert(scene.review.owner.saving, "first pending answer lost its lifecycle owner")
  finish_answer(scene)
  assert_answer(scene, first)
  assert_answer(scene, second)
end

T["one save starts a separate fork for each question"] = function()
  local first = add_message(scene, "A", "Why does the first declaration exist?")
  focus_source(scene)
  local second = add_message(scene, "A", "Why does the second declaration exist?")
  local first_hold = mock.hold(scene.review.session_id, "plan_question", true)
  local second_hold = mock.hold(scene.review.session_id, "plan_question", true)
  scene.hold = first_hold
  vim.api.nvim_set_current_win(scene.review.win)
  keymap.feed(":write<CR>")
  local first_pending = first_hold:wait()
  local second_pending = second_hold:wait()
  assert(first_pending.session_id ~= second_pending.session_id, "saved questions shared one fork")
  second_hold:release()
  wait_until(function() return next(scene.review.owner.reply_by_id) ~= nil end,
    "completed fork did not publish its answer independently", { errors = errors, timeout_ms = host_timeout_ms })
  finish_answer(scene)
  assert_answer(scene, first)
  assert_answer(scene, second)
end

T["answer while question remains focused"] = function()
  ask(scene, "Why does requested_change own this behavior?")
end

T["answer arrives after cursor leaves question"] = function()
  local question = add_message(scene, "A", "Why does requested_change own this behavior?")
  begin_answer(scene, question.source.body)
  local cursor = leave_question(scene, question.id)
  finish_answer(scene)
  assert_answer(scene, question)
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(scene.review.win), cursor), "answer moved the cursor back to its question")
  assert(comments.focus(scene.review.buf, question.id))
  assert_answer(scene, question)
end

T["leave and return before answer arrives"] = function()
  local question = add_message(scene, "A", "Why does requested_change own this behavior?")
  begin_answer(scene, question.source.body)
  leave_question(scene, question.id)
  assert(comments.focus(scene.review.buf, question.id))
  finish_answer(scene)
  assert_answer(scene, question)
end

T["A on answer creates a linked follow-up"] = function()
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
end

T["C on answer saves a linked revision comment"] = function()
  local parent = ask(scene, "Why does requested_change own this behavior?")
  focus_answer(scene)
  local comment = add_message(scene, "C", "Clarify the caller contract.")
  assert(tostring(comment.parent_id) == tostring(parent.id), "C on the answer lost its parent")
  assert(comment.kind ~= "question", "C created another question")
  vim.api.nvim_set_current_win(scene.review.win)
  keymap.feed(":write<CR>")
  wait_until(function() return not scene.review.owner.saving end, "revision comment did not save",
    { errors = errors, timeout_ms = host_timeout_ms })
  reopen(scene)
  assert(buffer.text(scene.review.buf):find("Clarify the caller contract.", 1, true), "reopening lost the revision comment")
  assert(buffer.text(scene.review.buf):find(answer, 1, true), "reopening the comment lost the answer")
end

T["editing pending question rejects its old answer"] = function()
  local question = add_message(scene, "A", "Why does requested_change own this behavior?")
  begin_answer(scene, question.source.body)
  assert(comments.focus(scene.review.buf, question.id))
  vim.api.nvim_set_current_win(scene.review.win)
  keymap.feed("0ccWhich caller owns the revised behavior?<Esc>")
  wait_until(function() return comments.capture(scene.review.buf)[1].source.body == "Which caller owns the revised behavior?" end,
    "question edit was not captured", { errors = errors, timeout_ms = host_timeout_ms })
  finish_answer(scene)
  assert(vim.bo[scene.review.buf].modified, "old answer cleared the newer draft")
  assert(not buffer.text(scene.review.buf):find(answer, 1, true), "old answer appeared beneath changed question text")
  question = comments.capture(scene.review.buf)[1]
  begin_answer(scene, question.source.body)
  finish_answer(scene)
  assert_answer(scene, question)
end

T["close and reopen preserves question and answer"] = function()
  local question = ask(scene, "Why does requested_change own this behavior?")
  leave_question(scene, question.id)
  reopen(scene)
  assert_answer(scene, question)
  assert(comments.focus(scene.review.buf, question.id))
  assert_answer(scene, question)
end

T["prose question stays on its requirement after reopening"] = function()
  -- The unscripted mock planner records this requirement in the overview.
  focus_source(scene, mock_requirement)
  local question = ask(scene, "Why must the registry retain this ownership boundary?")
  leave_question(scene, question.id)
  reopen(scene)
  assert_answer(scene, question)
  assert(row_containing(scene, question.source.body)
      > row_containing(scene, mock_requirement),
    "question moved before the requirement it describes")
  reopen(scene)
  assert_answer(scene, question)
end

T["prose comment preserves both selected range endpoints"] = function()
  focus_source(scene, "Requirements")
  -- A blank row separates each section heading from its body, so extend the selection
  -- through the requirement itself.
  local heading_row = vim.api.nvim_win_get_cursor(scene.review.win)[1]
  vim.api.nvim_set_current_win(scene.review.win)
  keymap.feed("V" .. string.rep("<Down>", row_containing(scene, mock_requirement, heading_row) - heading_row))
  local comment = add_message(scene, "C", "Keep this requirement attached to its heading.")
  assert(comment.source.end_line > comment.source.start_line, "comment did not capture the selected range")
  vim.api.nvim_set_current_win(scene.review.win)
  keymap.feed(":write<CR>")
  wait_until(function() return not scene.review.owner.saving end, "range comment did not save",
    { errors = errors, timeout_ms = host_timeout_ms })
  reopen(scene)
  local retained = comments.capture(scene.review.buf)[1]
  assert(vim.deep_equal(retained.source, comment.source), "reopening changed the comment's source range")
  assert(row_containing(scene, comment.source.body)
      > row_containing(scene, mock_requirement),
    "range comment moved before the end of its selected source")
end

T["close during pending answer retains review then reopens"] = function()
  local question = add_message(scene, "A", "Why does requested_change own this behavior?")
  begin_answer(scene, question.source.body)
  vim.api.nvim_set_current_win(scene.review.win)
  keymap.feed("q")
  assert(state.plan_review == scene.review, "closing dropped a pending question")
  assert(#errors == 1 and errors[1]:find("explicit operation in progress", 1, true), "pending close omitted its explanation")
  errors = {}
  leave_question(scene, question.id)
  finish_answer(scene)
  reopen(scene)
  assert_answer(scene, question)
end

T["answer completes while another tab owns the cursor"] = function()
  local question = add_message(scene, "A", "Why does requested_change own this behavior?")
  begin_answer(scene, question.source.body)
  vim.api.nvim_set_current_tabpage(scene.review.return_tab)
  local window = vim.api.nvim_get_current_win()
  local cursor = vim.api.nvim_win_get_cursor(window)
  finish_answer(scene)
  assert(vim.api.nvim_get_current_win() == window, "answer stole focus from another tab")
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), cursor), "answer moved another tab's cursor")
  assert_answer(scene, question)
  review_view.open(scene.plan)
  wait_until(function() return vim.api.nvim_get_current_tabpage() == scene.review.tab end, "review did not regain focus",
    { errors = errors, timeout_ms = host_timeout_ms })
  assert_answer(scene, question)
end

T["unsaved comment survives closing and reopening the plan"] = function()
  local comment = add_message(scene, "C", "Retain this unsaved review comment.")
  assert(vim.bo[scene.review.buf].modified, "new comment was not dirty")
  vim.api.nvim_set_current_win(scene.review.win)
  keymap.feed("q")
  wait_until(function() return vim.api.nvim_get_current_tabpage() == scene.review.return_tab end,
    "dirty PlanReview did not return to Harness", { errors = errors, timeout_ms = host_timeout_ms })
  review_view.open(scene.plan)
  wait_until(function() return state.plan_review and state.plan_review.owner.ready end,
    "dirty PlanReview did not reopen", { errors = errors, timeout_ms = host_timeout_ms })
  scene.review = state.plan_review
  assert(buffer.text(scene.review.buf):find(comment.source.body, 1, true), "reopening discarded the unsaved comment")
  assert(vim.bo[scene.review.buf].modified, "reopening cleared the unsaved marker")
  vim.api.nvim_set_current_win(scene.review.win)
  keymap.feed(":write<CR>")
  wait_until(function() return not scene.review.owner.saving end, "reopened comment did not save",
    { errors = errors, timeout_ms = host_timeout_ms })
  reopen(scene)
  assert(buffer.text(scene.review.buf):find(comment.source.body, 1, true), "saving discarded the recovered comment")
  assert(not vim.bo[scene.review.buf].modified, "saved recovered comment remained dirty")
end

T["cancelled answer clears progress and can be retried"] = function()
  local question = add_message(scene, "A", "Why does requested_change own this behavior?")
  begin_answer(scene, question.source.body)
  leave_question(scene, question.id)
  scene.hold:cancel()
  scene.hold = nil
  wait_until(function() return not scene.review.owner.saving and #errors > 0 end,
    "cancelled answer did not finish and report its failure", host_timeout_ms)
  assert(#errors == 1, "cancelled answer did not report one failure")
  errors = {}
  assert(not buffer.text(scene.review.buf):find("Generating response", 1, true), "cancelled answer retained its progress row")
  assert(scene.review.owner.reply_by_id[tostring(question.id)] == nil, "cancelled request published an answer")
  begin_answer(scene, question.source.body)
  finish_answer(scene)
  assert_answer(scene, question)
end

T["wiped review buffer settles its answer and can reopen"] = function()
  local question = add_message(scene, "A", "Why does requested_change own this behavior?")
  begin_answer(scene, question.source.body)
  vim.api.nvim_buf_delete(scene.review.buf, { force = true })
  scene.hold:release()
  scene.hold = nil
  wait_until(function() return not scene.review.owner.saving end, "closed review retained a completed answer operation",
    { errors = errors, timeout_ms = host_timeout_ms })
  review_view.open(scene.plan)
  wait_until(function() return state.plan_review and state.plan_review.owner.ready
    and vim.api.nvim_buf_is_valid(state.plan_review.buf) end, "wiped PlanReview did not reopen",
    { errors = errors, timeout_ms = host_timeout_ms })
  scene.review = state.plan_review
  assert_answer(scene, question)
end

T["wiped review settles a failed answer and allows retry"] = function()
  local question = add_message(scene, "A", "Why does requested_change own this behavior?")
  begin_answer(scene, question.source.body)
  vim.api.nvim_buf_delete(scene.review.buf, { force = true })
  scene.hold:cancel()
  scene.hold = nil
  wait_until(function() return not scene.review.owner.saving and #errors > 0 end,
    "closed review retained a failed answer operation", host_timeout_ms)
  assert(#errors == 1, "failed closed review did not report one failure")
  errors = {}
  review_view.open(scene.plan)
  wait_until(function() return state.plan_review and state.plan_review.owner.ready
    and vim.api.nvim_buf_is_valid(state.plan_review.buf) end, "failed wiped review did not reopen",
    { errors = errors, timeout_ms = host_timeout_ms })
  scene.review = state.plan_review
  begin_answer(scene, question.source.body)
  finish_answer(scene)
  assert_answer(scene, question)
end

T["pending question shows its Ask model and thinking after focus changes"] = function()
  local completed, configured, failure
  client.request({ session_id = scene.review.session_id, method = "session.configure",
    params = { ask_model = { model = "mock-model", effort = "high" } } }, function(result, diagnostic)
      completed, configured, failure = true, result, diagnostic
    end)
  wait_until(function() return completed end, "Ask settings did not settle",
    { errors = errors, timeout_ms = host_timeout_ms })
  assert(not failure, failure)
  state.ask_model = configured.ask_model
  scene.expected_model, scene.expected_effort = "mock-model", "high"
  local planning_effort = state.session.effort
  local question = add_message(scene, "A", "Why does the Ask model answer this question?")
  begin_answer(scene, question.source.body)
  leave_question(scene, question.id)
  assert(buffer.text(scene.review.buf):find("Generating response · mock-model · Thinking: high", 1, true),
    "collapsed question lost its captured Ask settings")
  assert(comments.focus(scene.review.buf, question.id))
  assert(buffer.text(scene.review.buf):find("Generating response · mock-model · Thinking: high", 1, true),
    "focused question lost its captured Ask settings")
  finish_answer(scene)
  assert_answer(scene, question)
  assert(state.session.effort == planning_effort, "Ask model changed the planning effort")
end

return T
