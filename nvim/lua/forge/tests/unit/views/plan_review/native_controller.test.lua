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

return T
