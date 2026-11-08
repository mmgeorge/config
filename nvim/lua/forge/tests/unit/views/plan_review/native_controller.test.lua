-- Unit tests for `forge.views.plan_review.native_controller` (native_controller.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local buffer_support = dofile("nvim/lua/forge/tests/support/buffer.lua")
local keymap = dofile("nvim/lua/forge/tests/support/keymap.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
local notify = dofile("nvim/lua/forge/tests/support/notify.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({
  hooks = isolation.hooks({
    post_case = function()
      local session = package.loaded["forge.session"]
      local review = session and session.harness.plan_review
      if review and review.owner then review.owner.close() end
    end,
  }),
})

T["controller"] = function()
  local encode_delivery = dofile("nvim/lua/forge/tests/support/plan_delivery.lua")
  local client = require("forge.client")
  local generation = 1
  client.host_get_generation = function() return generation end
  client.host_is_accepting = function() return true end
  local state = require("forge.session").harness
  state.session = { id = "plan-session" }
  state.transcript_win = vim.api.nvim_get_current_win()
  local activated, execution
  package.loaded["forge.views.harness.controller"] = { refresh_winbar = function() end, render = function() end,
    activate_snapshot = function() error("Plan command receipt is not a session snapshot") end,
    synchronize_state = function() activated = true end, present_plan_question = function() end,
    task_transition = function(action) execution = action end }
  local path = temp.file("plan.md", { "# Physical plan" })
  local pending
  local public_only = true
  client.request = function(request_specification, callback)
    local session_id = request_specification.session_id
    local method = request_specification.method
    local params = request_specification.params or {}

    callback = encode_delivery(callback)
    eq(session_id, "plan-session")
    if method == "session.plan.accept" or method == "session.plan.changes.request" then
      pending = { method = method, params = params, callback = callback } return
    end
    eq(method, "session.document")
    if params.operation == "plan_open" then
      local opened = { path = path, version = 1, public_only = public_only, saved_source_digest = "saved", snapshot = { document = params.document, revision = 0,
        block = {
        { id = "overview", text = { "Plan overview", "" }, metadata = { decoration = {}, editable_region = {}, target = {} } },
        { id = "file", text = { "file src/plan.rs", "child entity" }, metadata = { decoration = {}, editable_region = {}, target = {}, fold = {
          { id = "plan:file-tree:1", start = { row = 0, column = 0 }, ["end"] = { block = "file", position = { row = 2, column = 0 } }, closed = true },
        } } },
        { id = "stage", text = { "1. Establish shared foundations" }, metadata = { decoration = {}, editable_region = {}, target = {}, fold = {
          { id = "plan:section:foundation", start = { row = 0, column = 0 }, ["end"] = { block = "wrapped_task", position = { row = 4, column = 0 } }, closed = false },
        } } },
        { id = "task", text = { "Native projected plan", "task detail" }, metadata = { decoration = {}, editable_region = {}, target = {
          { id = "task", range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = 21 } } },
        }, fold = {
          { id = "plan:design:1", start = { row = 0, column = 0 }, ["end"] = { block = "task", position = { row = 2, column = 0 } }, closed = true },
        } } },
        { id = "wrapped_task", text = {
          "2. Own square velocity through a focused ECS component. The component keeps movement data with",
          "   the entity that uses it and exposes narrow operations for input changes and boundary reflection.",
          "   Its dedicated module prevents motion state from becoming application-wide state.",
          "   file src/motion.rs",
        }, metadata = { decoration = {}, editable_region = {}, target = {}, fold = {
          { id = "plan:design:2", start = { row = 2, column = 0 },
            heading_start = { block = "wrapped_task", position = { row = 0, column = 0 } },
            ["end"] = { block = "wrapped_task", position = { row = 4, column = 0 } }, closed = true },
        } } } } } }
      opened.source_row, opened.annotation = {}, {}
      for _, block in ipairs(opened.snapshot.block) do
        for index, text in ipairs(block.text) do
          opened.source_row[#opened.source_row + 1] = { id = block.id .. ":" .. index,
            text = text, source_line = #opened.source_row + 1, block = block.id,
            position = { row = index - 1, column = 0 }, metadata = block.metadata }
        end
      end
      callback(opened)
    elseif params.operation == "plan_action" and params.input.action == "jump_entity" then
      callback({ jump = { block = "overview", position = { row = 0, column = 5 } } })
    elseif params.operation == "plan_action" and params.input.action == "toggle_visibility" then
      public_only = not public_only
      callback({ public_only = public_only })
    else callback({}) end
  end
  require("forge.views.plan_review.native_controller").open({ id = "plan", state = "awaiting_review", working_path = path, review_digest = "canonical" })
  local review = state.plan_review
  assert(review and review.owner.ready)
  eq(vim.wo[review.win].virtualedit, "", { fail_reason = "PlanReview allows selecting past real text" })
  vim.cmd("normal! $")
  assert(vim.api.nvim_win_get_cursor(review.win)[2] == #"Plan overview" - 1,
    "end-of-line did not stop at the final text character")
  vim.cmd("normal! 999l")
  assert(vim.api.nvim_win_get_cursor(review.win)[2] == #"Plan overview" - 1,
    "cursor moved into virtual space")
  assert(vim.api.nvim_win_get_cursor(review.win)[1] == 1,
    "closing initial task folds scrolled PlanReview past its overview")
  local nodes = require("forge.nodes")
  local function closed(id) return nodes.closed(review.owner.replica, id) end
  local function row(id, offset)
    local _, first = review.owner.replica.sequence:position(id)
    return review.owner.replica.physical_row(first + (offset or 0))
  end
  local function answer_decision(comment, key)
    local decision_win = vim.api.nvim_get_current_win()
    local decision_buf = vim.api.nvim_win_get_buf(decision_win)
    buffer_support.set(decision_buf, comment)
    local mapping = keymap.get(key)
    if mapping.buffer == 1 and mapping.callback then mapping.callback() return end
    error("missing decision mapping " .. key)
  end
  assert(closed("plan:file-tree:1"), "file tree did not start closed")
  assert(not closed("plan:section:foundation"), "stage hid task headings")
  assert(closed("plan:design:1"), "task did not start closed")
  vim.api.nvim_win_set_cursor(review.win, { row("task") + 1, 0 })
  review.command_set.action_by_id.toggle.run({})
  assert(not closed("plan:design:1"), "task did not open")
  review.command_set.action_by_id.toggle.run({})
  assert(closed("plan:design:1"), "task did not close")
  for offset = 0, 2 do
    assert(closed("plan:design:2"))
    local heading_row = row("wrapped_task", offset) + 1
    vim.api.nvim_win_set_cursor(review.win, { heading_row, 0 })
    review.command_set.action_by_id.toggle.run({})
    assert(not closed("plan:design:2"), "wrapped task did not expand")
    assert(vim.api.nvim_win_get_cursor(review.win)[1] == heading_row)
    review.command_set.action_by_id.toggle.run({})
    assert(closed("plan:design:2"), "wrapped task did not collapse")
    assert(vim.api.nvim_win_get_cursor(review.win)[1] == heading_row)
  end
  vim.api.nvim_win_set_cursor(review.win, { row("stage") + 1, 0 })
  review.command_set.action_by_id.toggle.run({})
  assert(closed("plan:section:foundation"))
  review.command_set.action_by_id.toggle.run({})
  assert(not closed("plan:section:foundation") and closed("plan:design:1"))
  vim.cmd("vsplit")
  vim.api.nvim_win_set_buf(0, review.buf)
  review.owner.refresh_views()
  assert(not vim.wo.foldenable and closed("plan:design:1"))
  local task_row = row("task")
  vim.api.nvim_set_current_win(review.win)
  for _, command in ipairs({ "toggle", "open", "jump_entity", "toggle_visibility", "comment", "delete", "accept", "request_changes", "close", "help" }) do
    assert(review.command_set.action_by_id[command], "missing PlanReview command " .. command)
  end
  eq(keymap.get("q").buffer, 1, { fail_reason = "PlanReview did not bind q" })
  local visibility = keymap.get("ov")
  assert(visibility.buffer == 1 and visibility.desc == "Toggle All/Public diff visibility", "PlanReview did not bind shared ov")
  assert(not review.command_set.action_by_id.toggle_presentation, "Semantic-only PlanReview exposed a presentation toggle")
  assert(vim.fn.maparg("ox", "n", false, true).buffer ~= 1, "Semantic-only PlanReview bound ox")
  eq(review.public_only, true)
  keymap.feed("ov")
  wait_until(function() return review.public_only == false end, "ov did not select All")
  keymap.feed("ov")
  wait_until(function() return review.public_only == true end, "ov did not restore Public")
  eq(keymap.get("J").buffer, 1, { fail_reason = "PlanReview did not bind J to comment deletion" })
  vim.api.nvim_win_set_cursor(review.win, { task_row + 1, 3 })
  vim.cmd("clearjumps")
  local origin = vim.api.nvim_win_get_cursor(review.win)
  keymap.invoke(".")
  eq(vim.api.nvim_win_get_cursor(review.win), { 1, 5 }, { fail_reason = "plan jump did not reach declaration" })
  vim.keymap.set("n", ",", "<C-o>", { buffer = review.buf })
  keymap.feed(",")
  eq(vim.api.nvim_win_get_cursor(review.win), origin, { fail_reason = "comma did not return to jump origin" })
  local hidden_tab = review.tab
  vim.cmd("tabclose")
  require("forge.views.plan_review.native_controller").open({ id = "plan", state = "awaiting_review", working_path = path, review_digest = "canonical" })
  assert(review.tab ~= hidden_tab and review.tab == vim.api.nvim_get_current_tabpage())
  assert(review.win == vim.api.nvim_get_current_win(), "hidden review did not transfer its window ownership")
  local detached = review.owner
  assert(detached.close())
  vim.api.nvim_exec_autocmds("BufEnter", { buffer = review.buf })
  assert(not detached.attached(), "closed review remained reusable")
  vim.cmd("tabclose")
  require("forge.views.plan_review.native_controller").open({ id = "plan", state = "awaiting_review", working_path = path, review_digest = "canonical" })
  review = state.plan_review
  assert(detached.closed and review.owner ~= detached and review.owner.attached(),
    "reopening detached review did not establish a fresh native attachment")
  review.command_set.action_by_id.accept.run({})
  answer_decision("", "<Esc>")
  assert(not pending and not require("forge.views.harness.activity").busy(state), "cancelling the decision sent an RPC")

  review.command_set.action_by_id.accept.run({})
  local captured_digest = review.plan.review_digest
  review.plan.review_digest = "changed-while-popup-open"
  answer_decision("stale comment", "<CR>")
  assert(not pending and not require("forge.views.harness.activity").busy(state), "stale review digest sent an RPC")
  review.plan.review_digest = captured_digest

  review.command_set.action_by_id.accept.run({})
  local original_get_changedtick = vim.api.nvim_buf_get_changedtick
  vim.api.nvim_buf_get_changedtick = function(buffer)
    local changedtick = original_get_changedtick(buffer)
    return buffer == review.buf and changedtick + 1 or changedtick
  end
  local decision_ok, decision_failure = pcall(answer_decision, "stale tick comment", "<CR>")
  vim.api.nvim_buf_get_changedtick = original_get_changedtick
  assert(decision_ok, decision_failure)
  assert(not pending and not require("forge.views.harness.activity").busy(state), "stale changedtick sent an RPC")

  review.command_set.action_by_id.accept.run({})
  assert(not pending, "approval dispatched before the decision popup was answered")
  answer_decision("Approved after review", "<CR>")
  assert(pending and pending.params.comment == "Approved after review"
    and pending.params.review.document == review.owner.document and pending.params.digest == nil)
  assert(state.plan_review == review and not review.owner.closed, "approval closed before its capture completed")
  pending.callback({ command_result = { outcome = { outcome = "task", result = { disposition = "awaiting_input" } } } })
  assert(activated and state.plan_review == nil and not require("forge.views.harness.activity").busy(state))
  eq(execution, nil, { fail_reason = "approval issued a second execution command" })
  assert(vim.api.nvim_get_current_win() == state.transcript_win, "approval did not return to Harness")
  assert(vim.api.nvim_buf_is_valid(review.buf), "closing PlanReview deleted its physical buffer")
  eq(vim.fn.readfile(path), { "# Physical plan" })
  local native = require("forge.views.plan_review.native_controller")
  execution = nil
  native.open(review.plan)
  review = state.plan_review
  review.command_set.action_by_id.accept.run({})
  answer_decision("", "<CR>")
  pending.callback({ session = { id = "plan-session" }, active_plan = { state = "accepted", review = vim.NIL },
    goal_execution = { state = "paused" } })
  assert(execution == nil and state.plan_review == nil,
    "revision approval overrode the provider pause with an execute request")
  native.open(review.plan)
  review = state.plan_review
  review.command_set.action_by_id.request_changes.run({})
  answer_decision("Please revise", "<C-s>")
  assert(pending.method == "session.plan.changes.request" and pending.params.comment == "Please revise")
  assert(state.plan_review == review and not review.owner.closed, "revision closed before its capture completed")
  pending.callback(nil, "revision rejected")
  assert(state.plan_review and state.plan_review.owner.attached(), "failed submission did not restore review")
  keymap.invoke("q")
  eq(state.plan_review, nil, { fail_reason = "q did not close review" })
  native.open(review.plan)
  review = state.plan_review
  review.owner.annotation_saving, review.owner.saving = true, true
  local dispatched_before_queue = pending
  review.command_set.action_by_id.accept.run({})
  answer_decision("", "<CR>")
  -- The session owner publishes activity, so a queued submission proves itself by
  -- staying queued behind the save instead of by a Lua-side busy flag.
  assert(#review.owner.pending_operation == 1 and pending == dispatched_before_queue,
    "approval dispatched while an annotation save was still in flight")
  generation = 2
  native.open(review.plan)
  local recovered = state.plan_review
  assert(recovered ~= review and recovered.owner.ready)
  assert(pending ~= dispatched_before_queue and pending.method == "session.plan.accept",
    "recovery did not dispatch the queued approval")
  assert(pending.params.review.document == recovered.owner.document)
  pending.callback({ session = { id = "plan-session" } })
  assert(recovered.owner.closed and not state.plan_review and not require("forge.views.harness.activity").busy(state),
    "recovered submission completion retained the replacement review")
end

T["submission_close"] = function()
  local encode_delivery = dofile("nvim/lua/forge/tests/support/plan_delivery.lua")
  local client = require("forge.client")
  local harness = require("forge.session").harness
  local pending = {}
  local notice = notify.capture()
  client.host_get_generation = function() return 1 end
  client.host_is_accepting = function() return true end
  client.request = function(request_specification, callback)
    local method = request_specification.method
    local params = request_specification.params or {}

    callback = encode_delivery(callback)
    pending[#pending + 1] = { method = method, params = params, callback = callback }
  end
  package.loaded["forge.views.harness.controller"] = {
    refresh_winbar = function() end,
    activate_snapshot = function() error("Plan command receipt is not a session snapshot") end,
    synchronize_state = function() end,
    render = function() end,
    present_plan_question = function() end,
    task_transition = function() error("approval issued a second execution command") end,
  }
  local function submit_decision(comment)
    local decision_buf = vim.api.nvim_get_current_buf()
    buffer_support.set(decision_buf, { comment })
    local decision_mapping = keymap.get("<CR>", { buffer = decision_buf }).callback
    assert(decision_mapping, "decision popup did not bind Enter")
    decision_mapping()
  end
  harness.session = { id = "session" }
  harness.transcript_win = vim.api.nvim_get_current_win()
  local controller = require("forge.views.plan_review.native_controller")
  local comments = require("forge.draft_comments")

  local function open_review()
    local plan = { id = "plan", state = "awaiting_review", review_digest = "canonical", working_path = vim.fn.tempname() .. ".md" }
    controller.open(plan)
    local review = harness.plan_review
    local request = pending[#pending]
    eq(request.params.operation, "plan_open")
    request.callback({ saved_source_digest = "saved", annotation = {}, public_only = true,
      source_row = { { id = "source:1", target = "source:1", text = "pub struct Sample;", source_line = 1,
        block = "plan:source", position = { row = 0, column = 0 }, metadata = {} } },
      snapshot = { document = review.owner.document, revision = 0, block = {
        { id = "plan:source", text = { "pub struct Sample;" },
          metadata = { target = {}, editable_region = {}, decoration = {} } },
      } } })
    comments.add(review.buf, { id = "single", source_line = 1, body = "Preserve this feedback" }, false)
    vim.cmd("stopinsert")
    assert(vim.bo[review.buf].modified)
    return review
  end

  for _, action in ipairs({ "request_changes", "accept", "abort_plan" }) do
    local review = open_review()
    review.command_set.action_by_id[action].run({})
    if action ~= "abort_plan" then submit_decision("") end
    local request = pending[#pending]
    assert(request.method == (action == "accept" and "session.plan.accept" or action == "abort_plan" and "session.plan.cancel" or "session.plan.changes.request"))
    eq(request.params.draft_annotation[1].source.body, "Preserve this feedback")
    eq(#vim.fn.win_findbuf(review.buf), 0, { fail_reason = action .. " kept the review visible while awaiting its response" })
    assert(vim.api.nvim_get_current_win() == harness.transcript_win)
    assert(not review.owner.closed and review.owner.submission_pending, "hiding cancelled the pending submission")
    request.callback({}, nil)
    assert(review.owner.closed and not harness.plan_review, action .. " did not release its completed owner")
  end

  local review = open_review()
  review.command_set.action_by_id.request_changes.run({})
  submit_decision("")
  local request = pending[#pending]
  request.callback(nil, "Submission failed")
  eq(#vim.fn.win_findbuf(review.buf), 1, { fail_reason = "failed submission did not restore the review" })
  assert(vim.bo[review.buf].modified and comments.capture(review.buf)[1].source.body == "Preserve this feedback",
    "failed submission lost the unsaved comment")
  assert(not require("forge.views.harness.activity").busy(harness) and notice[#notice].message:find("Submission failed", 1, true))
end

-- The plan the question cases review: a requirement in prose and one declaration.
local question_plan_lines = {
  "Requirements",
  "",
  "Preserve the requested behavior at its owning boundary.",
  "",
  "pub fn requested_change();",
}
local requirement = "Preserve the requested behavior at its owning boundary."

local function row_containing(scene, literal, first_row)
  return assert(buffer_support.find_row(scene.review.buf, literal, { first_row = first_row }),
    "missing review text: " .. literal .. "\n" .. buffer_support.text(scene.review.buf))
end

local function focus_source(scene, literal)
  vim.api.nvim_set_current_win(scene.review.win)
  vim.api.nvim_win_set_cursor(scene.review.win, { row_containing(scene, literal or "requested_change"), 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = scene.review.buf })
end

local function question_review()
  local plan_review_host = dofile("nvim/lua/forge/tests/support/plan_review_host.lua")
  local plan_host = plan_review_host.start({ lines = question_plan_lines })
  local scene = { host = plan_host, review = plan_host.open(), notices = notify.capture() }
  scene.comments = require("forge.draft_comments")
  focus_source(scene)
  return scene
end

-- Creates a question with `A` or a comment with `C` at the cursor, or over the visual selection,
-- and replaces its body.
local function add_message(scene, key, body)
  local count = #scene.comments.capture(scene.review.buf)
  vim.api.nvim_set_current_win(scene.review.win)
  local visual = vim.api.nvim_get_mode().mode:match("^[vV\22]") ~= nil
  keymap.invoke(key, { buffer = scene.review.buf, mode = visual and "x" or "n" })
  vim.cmd("stopinsert")
  if vim.api.nvim_get_mode().mode:match("^[vV\22]") then keymap.feed("<Esc>", "nx") end
  local message = scene.comments.capture(scene.review.buf)[count + 1]
  assert(message, key .. " did not create a message")
  assert(scene.comments.focus(scene.review.buf, message.id))
  local row = vim.api.nvim_win_get_cursor(scene.review.win)[1] - 1
  vim.api.nvim_buf_set_lines(scene.review.buf, row, row + 1, false, { body })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = scene.review.buf })
  message = scene.comments.capture(scene.review.buf)[count + 1]
  eq(message.source.body, body, { fail_reason = "editing did not populate the new message" })
  return message
end

local function save(scene)
  vim.api.nvim_set_current_win(scene.review.win)
  vim.cmd("write")
end

local function leave_question(scene, id)
  vim.api.nvim_set_current_win(scene.review.win)
  vim.api.nvim_win_set_cursor(scene.review.win, { 1, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = scene.review.buf })
  local compact = vim.iter(scene.review.owner.comment_state.range_list):any(function(range)
    return tostring(range.annotation.id) == tostring(id) and range.compact
  end)
  assert(compact, "leaving the question did not collapse its thread")
  return vim.api.nvim_win_get_cursor(scene.review.win)
end

local function reopen(scene)
  vim.api.nvim_set_current_win(scene.review.win)
  keymap.feed("q")
  local state = require("forge.session").harness
  eq(state.plan_review, nil, { fail_reason = "saved PlanReview did not close" })
  scene.review = scene.host.open()
end

-- `options.others_pending` marks a review in which another question still awaits its answer, so
-- only this question's own progress row must be gone.
local function assert_answer(scene, question, body, options)
  local question_row = row_containing(scene, question.source.body)
  local answer_row = row_containing(scene, body, question_row + 1)
  assert(answer_row > question_row, "answer did not appear below its question")
  if options and options.others_pending then
    local progress_row = buffer_support.find_row(scene.review.buf, "Generating response", { first_row = question_row + 1 })
    assert(not progress_row or progress_row > answer_row, "completed question retained its progress row")
  else
    assert(not buffer_support.text(scene.review.buf):find("Generating response", 1, true), "completed question retained its progress row")
  end
  local reply = scene.review.owner.reply_by_id[tostring(question.id)]
  assert(reply and reply.question_body == question.source.body, "answer attached to a different question")
  assert(not vim.bo[scene.review.buf].modified, "completed answer left the saved question dirty")
end

local function ask(scene, body, answer)
  local question = add_message(scene, "A", body)
  save(scene)
  scene.host.answer(#scene.host.answers, answer)
  assert_answer(scene, question, answer)
  return question
end

T["answer arrives after cursor leaves question"] = function()
  local scene = question_review()
  local question = add_message(scene, "A", "Why does requested_change own this behavior?")
  save(scene)
  local cursor = leave_question(scene, question.id)
  scene.host.answer(1, "The caller owns it.")
  assert_answer(scene, question, "The caller owns it.")
  eq(vim.api.nvim_win_get_cursor(scene.review.win), cursor, { fail_reason = "answer moved the cursor back to its question" })
  assert(scene.comments.focus(scene.review.buf, question.id))
  assert_answer(scene, question, "The caller owns it.")
end

T["close and reopen preserves question and answer"] = function()
  local scene = question_review()
  local question = ask(scene, "Why does requested_change own this behavior?", "The caller owns it.")
  leave_question(scene, question.id)
  reopen(scene)
  assert_answer(scene, question, "The caller owns it.")
  assert(scene.comments.focus(scene.review.buf, question.id))
  assert_answer(scene, question, "The caller owns it.")
end

T["prose question stays on its requirement after reopening"] = function()
  local scene = question_review()
  focus_source(scene, requirement)
  local question = ask(scene, "Why must the registry retain this ownership boundary?", "The registry owns it.")
  leave_question(scene, question.id)
  reopen(scene)
  assert_answer(scene, question, "The registry owns it.")
  assert(row_containing(scene, question.source.body) > row_containing(scene, requirement),
    "question moved before the requirement it describes")
  reopen(scene)
  assert_answer(scene, question, "The registry owns it.")
end

T["prose comment preserves both selected range endpoints"] = function()
  local scene = question_review()
  focus_source(scene, "Requirements")
  local heading_row = vim.api.nvim_win_get_cursor(scene.review.win)[1]
  keymap.feed("V" .. string.rep("<Down>", row_containing(scene, requirement, heading_row) - heading_row))
  local comment = add_message(scene, "C", "Keep this requirement attached to its heading.")
  assert(comment.source.end_line > comment.source.start_line, "comment did not capture the selected range")
  save(scene)
  assert(not scene.review.owner.saving, "range comment did not save")
  reopen(scene)
  eq(scene.comments.capture(scene.review.buf)[1].source, comment.source,
    { fail_reason = "reopening changed the comment's source range" })
  assert(row_containing(scene, comment.source.body) > row_containing(scene, requirement),
    "range comment moved before the end of its selected source")
end

T["answer completes while another tab owns the cursor"] = function()
  local scene = question_review()
  local question = add_message(scene, "A", "Why does requested_change own this behavior?")
  save(scene)
  vim.api.nvim_set_current_tabpage(scene.review.return_tab)
  local window = vim.api.nvim_get_current_win()
  local cursor = vim.api.nvim_win_get_cursor(window)
  scene.host.answer(1, "The caller owns it.")
  eq(vim.api.nvim_get_current_win(), window, { fail_reason = "answer stole focus from another tab" })
  eq(vim.api.nvim_win_get_cursor(window), cursor, { fail_reason = "answer moved another tab's cursor" })
  assert_answer(scene, question, "The caller owns it.")
  scene.host.open()
  eq(vim.api.nvim_get_current_tabpage(), scene.review.tab, { fail_reason = "review did not regain focus" })
  assert_answer(scene, question, "The caller owns it.")
end

T["unsaved comment survives closing and reopening the plan"] = function()
  local scene = question_review()
  local comment = add_message(scene, "C", "Retain this unsaved review comment.")
  assert(vim.bo[scene.review.buf].modified, "new comment was not dirty")
  vim.api.nvim_set_current_win(scene.review.win)
  keymap.feed("q")
  eq(vim.api.nvim_get_current_tabpage(), scene.review.return_tab, { fail_reason = "dirty PlanReview did not return to Harness" })
  scene.review = scene.host.open()
  assert(buffer_support.text(scene.review.buf):find(comment.source.body, 1, true), "reopening discarded the unsaved comment")
  assert(vim.bo[scene.review.buf].modified, "reopening cleared the unsaved marker")
  save(scene)
  assert(not scene.review.owner.saving, "reopened comment did not save")
  reopen(scene)
  assert(buffer_support.text(scene.review.buf):find(comment.source.body, 1, true), "saving discarded the recovered comment")
  assert(not vim.bo[scene.review.buf].modified, "saved recovered comment remained dirty")
end

T["wiped review buffer settles its answer and can reopen"] = function()
  local scene = question_review()
  local question = add_message(scene, "A", "Why does requested_change own this behavior?")
  save(scene)
  local owner = scene.review.owner
  vim.api.nvim_buf_delete(scene.review.buf, { force = true })
  scene.host.answer(1, "The caller owns it.")
  assert(not owner.saving, "closed review retained a completed answer operation")
  scene.review = scene.host.open()
  assert(vim.api.nvim_buf_is_valid(scene.review.buf), "wiped PlanReview did not reopen")
  assert_answer(scene, question, "The caller owns it.")
end

T["wiped review settles a failed answer and allows retry"] = function()
  local scene = question_review()
  local question = add_message(scene, "A", "Why does requested_change own this behavior?")
  save(scene)
  local owner = scene.review.owner
  vim.api.nvim_buf_delete(scene.review.buf, { force = true })
  scene.host.fail(1, "Provider unavailable")
  assert(not owner.saving, "closed review retained a failed answer operation")
  eq(#scene.notices, 1, { fail_reason = "failed closed review did not report one failure" })
  scene.notices:clear()
  scene.review = scene.host.open()
  assert(vim.api.nvim_buf_is_valid(scene.review.buf), "failed wiped review did not reopen")
  save(scene)
  scene.host.answer(#scene.host.answers, "The caller owns it.")
  assert_answer(scene, question, "The caller owns it.")
end

local function pending_progress(scene, question)
  local question_row = row_containing(scene, question.source.body)
  row_containing(scene, "Generating response · mock-model · Thinking: high", question_row + 1)
  eq(scene.review.owner.reply_by_id[tostring(question.id)], nil, { fail_reason = "pending question already has an answer" })
end

T["one save sends one answer request per question"] = function()
  local scene = question_review()
  local first = add_message(scene, "A", "Why does the first declaration exist?")
  focus_source(scene)
  local second = add_message(scene, "A", "Why does the second declaration exist?")
  save(scene)
  eq(scene.host.saves, 1, { fail_reason = "two questions took more than one save" })
  eq(vim.tbl_map(function(request) return request.question_id end, scene.host.answers),
    { tostring(first.id), tostring(second.id) }, { fail_reason = "each question did not get its own answer request" })
  pending_progress(scene, first)
  pending_progress(scene, second)
  scene.host.answer(2, "The second caller owns it.")
  assert_answer(scene, second, "The second caller owns it.", { others_pending = true })
  pending_progress(scene, first)
  assert(scene.review.owner.saving, "the pending first answer lost its lifecycle owner")
  scene.host.answer(1, "The first caller owns it.")
  assert_answer(scene, first, "The first caller owns it.")
  assert_answer(scene, second, "The second caller owns it.")
  assert(not scene.review.owner.saving, "answered questions left the review saving")
end

T["later question completes while the first answer is pending"] = function()
  local scene = question_review()
  local first = add_message(scene, "A", "Why does the first caller own this behavior?")
  save(scene)
  focus_source(scene)
  local second = add_message(scene, "A", "Which second caller uses this interface?")
  save(scene)
  eq(scene.host.answers[2].question_id, tostring(second.id), { fail_reason = "second save did not ask the second question" })
  scene.host.answer(2, "The second caller uses it.")
  assert_answer(scene, second, "The second caller uses it.", { others_pending = true })
  pending_progress(scene, first)
  assert(scene.review.owner.saving, "the pending first answer lost its lifecycle owner")
  scene.host.answer(1, "The first caller owns it.")
  assert_answer(scene, first, "The first caller owns it.")
  assert_answer(scene, second, "The second caller uses it.")
end

T["failed answer clears only its own question"] = function()
  local scene = question_review()
  local first = add_message(scene, "A", "Why does the first declaration exist?")
  focus_source(scene)
  local second = add_message(scene, "A", "Why does the second declaration exist?")
  save(scene)
  scene.host.fail(1, "Provider unavailable")
  eq(#scene.notices, 1, { fail_reason = "the failed answer did not report one failure" })
  local first_row = row_containing(scene, first.source.body)
  local second_row = row_containing(scene, second.source.body)
  local progress_row = buffer_support.find_row(scene.review.buf, "Generating response", { first_row = first_row + 1 })
  assert(progress_row and progress_row > second_row, "the failure cleared the other question's pending answer")
  scene.host.answer(2, "The second caller owns it.")
  assert_answer(scene, second, "The second caller owns it.")
  eq(scene.review.owner.reply_by_id[tostring(first.id)], nil, { fail_reason = "the failed question gained an answer" })
end

T["close during pending answer retains review then reopens"] = function()
  local scene = question_review()
  local question = add_message(scene, "A", "Why does requested_change own this behavior?")
  save(scene)
  pending_progress(scene, question)
  local review = scene.review
  vim.api.nvim_set_current_win(review.win)
  keymap.feed("q")
  eq(require("forge.session").harness.plan_review, review, { fail_reason = "closing dropped a pending question" })
  local messages = scene.notices:messages()
  assert(#messages == 1 and messages[1]:find("explicit operation in progress", 1, true),
    "pending close omitted its explanation: " .. vim.inspect(messages))
  leave_question(scene, question.id)
  scene.host.answer(1, "The caller owns it.")
  reopen(scene)
  assert_answer(scene, question, "The caller owns it.")
end

return T
