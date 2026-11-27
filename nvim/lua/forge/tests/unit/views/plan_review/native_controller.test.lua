-- Unit tests for `forge.views.plan_review.native_controller` (native_controller.lua).
local finish_input = dofile("nvim/lua/forge/tests/support/plan_comment_input.lua")
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local buffer_support = dofile("nvim/lua/forge/tests/support/buffer.lua")
local keymap = dofile("nvim/lua/forge/tests/support/keymap.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
local notify = dofile("nvim/lua/forge/tests/support/notify.lua")
local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
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

T["walkthrough keys reveal deferred files and show the active step"] = function()
  local encode_delivery = dofile("nvim/lua/forge/tests/support/plan_delivery.lua")
  local client = require("forge.client")
  client.host_get_generation = function() return 1 end
  client.host_is_accepting = function() return true end
  local state = require("forge.session").harness
  state.session = { id = "walkthrough-session" }
  state.transcript_win = vim.api.nvim_get_current_win()
  local path = temp.file("walkthrough.md", { "# Review" })
  local file_a, file_b = "plan:design:file:a", "plan:design:file:b"
  local hunk = file_b .. ":hunk:0"
  local declaration = hunk .. ":row:0"
  local function step(index) return "plan:walkthrough:" .. index end
  local position = { row = 0, column = 0 }
  local function block(id, rows, endpoint)
    local metadata = { decoration = {}, editable_region = {}, target = {} }
    if endpoint then
      metadata.fold = {
        { id = id, start = position, ["end"] = { block = endpoint, position = { row = 1, column = 0 } }, closed = true },
      }
    end
    return { id = id, text = rows, metadata = metadata }
  end
  local function delivery(document, reviewed)
    local tail_a = reviewed and step(3) or file_a .. ":hunk:deferred"
    local tail_b = reviewed and declaration or file_b .. ":hunk:deferred"
    local blocks = { block("overview", { "Plan overview" }), block(file_a, { "First file" }, tail_a) }
    if reviewed then blocks[#blocks + 1] = block(step(1), { "╭─ 1/3 Walkthrough ─╮", "│ Open the review. │" }) end
    blocks[#blocks + 1] = block(tail_a, { reviewed and "╭─ 3/3 Finish ─╮" or "" })
    blocks[#blocks + 1] = block(file_b, { "Second file" }, tail_b)
    if reviewed then
      blocks[#blocks + 1] = block(hunk, { "@@" }, declaration)
      blocks[#blocks + 1] = block(step(2), { "╭─ 2/3 Register ─╮" })
      blocks[#blocks + 1] = block(declaration, { "fn register()" })
    else
      blocks[#blocks + 1] = block(tail_b, { "" })
    end
    local rows = {}
    for _, entry in ipairs(blocks) do
      for index = 1, #entry.text do
        rows[#rows + 1] = { block = entry.id, position = { row = index - 1, column = 0 }, source_line = 0 }
      end
    end
    return { snapshot = { document = document, revision = reviewed and 1 or 0, block = blocks },
      source_row = rows, annotation = {} }
  end
  local pending
  client.request = function(request_specification, callback)
    local params = request_specification.params
    callback = encode_delivery(callback)
    if params.operation == "plan_open" then
      local opened = delivery(params.document, false)
      opened.path, opened.version, opened.saved_source_digest = path, 1, "walkthrough-digest"
      opened.walkthrough = { step = 0, total = 3 }
      callback(opened)
    elseif params.operation == "plan_action" then
      pending = { input = params.input, callback = callback }
    else
      callback({})
    end
  end
  require("forge.views.plan_review.native_controller").open({
    id = "walkthrough", state = "awaiting_review", working_path = path, review_digest = "walkthrough-digest",
  })
  local review = assert(state.plan_review)
  local replica = review.owner.replica
  local nodes = require("forge.nodes")
  local function winbar(number)
    assert(vim.wo[review.win].winbar:find(("Walkthrough %d/3"):format(number), 1, true))
  end
  winbar(0)
  assert(nodes.closed(replica, file_a) and nodes.closed(replica, file_b))
  keymap.invoke("]w")
  eq(pending.input.action, "walkthrough_next")
  local loaded = delivery(pending.input.document, true)
  loaded.jump, loaded.walkthrough = { block = step(1), position = position }, { step = 1, total = 3 }
  pending.callback(loaded)
  eq(vim.api.nvim_get_current_line(), "╭─ 1/3 Walkthrough ─╮")
  winbar(1)
  assert(not nodes.closed(replica, file_a) and nodes.closed(replica, file_b))

  local _, third = replica.sequence:position(step(3))
  vim.api.nvim_win_set_cursor(review.win, { replica.physical_row(third) + 1, 3 })
  keymap.invoke("[w")
  eq(pending.input.action, "walkthrough_previous")
  eq(pending.input.block, step(3))
  pending.callback({ jump = { block = step(2), position = position },
    reveal = { block = declaration, position = position }, walkthrough = { step = 2, total = 3 } })
  eq(vim.api.nvim_get_current_line(), "╭─ 2/3 Register ─╮")
  winbar(2)
  assert(not nodes.closed(replica, file_a) and not nodes.closed(replica, file_b))
  assert(not nodes.closed(replica, hunk))
end

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
  local public_only = false
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
          { id = "plan:design:file:abc", start = { row = 0, column = 0 }, ["end"] = { block = "file", position = { row = 2, column = 0 } }, closed = true },
        } } },
        { id = "tests", text = { "tests src/plan.rs", "planned test" }, metadata = { decoration = {}, editable_region = {}, target = {}, fold = {
          { id = "plan:tests:file:abc", start = { row = 0, column = 0 }, ["end"] = { block = "tests", position = { row = 2, column = 0 } }, closed = true },
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
      callback({ jump = { block = "file", position = { row = 1, column = 5 } } })
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
  assert(closed("plan:design:file:abc") and closed("plan:tests:file:abc"), "files did not start closed")
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
  eq(review.public_only, false)
  keymap.feed("ov")
  wait_until(function() return review.public_only == true end, "ov did not select Public")
  keymap.feed("ov")
  wait_until(function() return review.public_only == false end, "ov did not restore All")
  eq(keymap.get("J").buffer, 1, { fail_reason = "PlanReview did not bind J to comment deletion" })
  vim.api.nvim_win_set_cursor(review.win, { task_row + 1, 3 })
  vim.cmd("clearjumps")
  local origin = vim.api.nvim_win_get_cursor(review.win)
  keymap.invoke(".")
  eq(vim.api.nvim_get_current_line(), "child entity", { fail_reason = "plan jump did not reveal the folded file" })
  eq(vim.api.nvim_win_get_cursor(review.win)[2], 5)
  vim.keymap.set("n", ",", "<C-o>", { buffer = review.buf })
  keymap.feed(",")
  eq(vim.api.nvim_win_get_cursor(review.win), { row("task") + 1, origin[2] }, { fail_reason = "comma did not return to jump origin" })
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
  review = assert(state.plan_review)
  assert(detached.closed and review.owner ~= detached and review.owner.attached(),
    "reopening detached review did not establish a fresh native attachment")
  eq({ closed("plan:design:file:abc"), closed("plan:tests:file:abc") }, { false, true })
  vim.api.nvim_win_set_cursor(review.win, { row("tests") + 1, 0 })
  keymap.invoke("<Tab>", { buffer = review.buf })
  eq(vim.api.nvim_buf_get_lines(review.buf, row("tests") + 1, row("tests") + 2, false), { "planned test" })
  keymap.invoke("q", { buffer = review.buf })
  require("forge.views.plan_review.native_controller").open(review.plan)
  review = assert(state.plan_review)
  eq(closed("plan:tests:file:abc"), false)
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
    synchronize_state = function(settled) if settled then settled({}, nil) end end,
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
  -- A paused plan whose acceptance raised questions: the questions come first, so the decision
  -- reply sends no one to /task resume.
  harness.session = { id = "session", active_task = { kind = "plan", status = "paused",
    plan = { artifact = { review = { elicitation = { question_set = { questions = { { id = "mode" } } } } } } } } }
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
    -- The host may still refuse the decision, so the review stays where the reviewer is until it
    -- answers.
    eq(vim.fn.win_findbuf(review.buf), { review.win }, { fail_reason = action .. " hid the review before the host answered" })
    eq(vim.api.nvim_get_current_win(), review.win)
    assert(not review.owner.closed and review.owner.submission_pending, action .. " did not wait for its response")
    request.callback({}, nil)
    eq(#vim.fn.win_findbuf(review.buf), 0, { fail_reason = action .. " kept the review visible after the host accepted" })
    assert(vim.api.nvim_get_current_win() == harness.transcript_win)
    assert(review.owner.closed and not harness.plan_review, action .. " did not release its completed owner")
    eq(vim.tbl_filter(function(entry) return entry.message:find("/task resume", 1, true) ~= nil end, notice), {})
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
  finish_input({ body })
  local message = scene.comments.capture(scene.review.buf)[count + 1]
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

local function expand_thread(review, id)
  for _, range in ipairs(review.owner.comment_state.range_list) do
    if range.folded and tostring(range.annotation.id) == tostring(id) then
      vim.api.nvim_win_set_cursor(review.win, { range.first_row + 1, 0 })
      keymap.invoke("<Tab>", { window = review.win })
      return
    end
  end
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
  expand_thread(scene.review, question.id)
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

T["Tab folds only the selected comment chain and counts its messages"] = function()
  local host = dofile("nvim/lua/forge/tests/support/plan_review_host.lua").start({ lines = { "Source" } })
  local function message(id, body, kind, parent, answer)
    return { id = id, kind = kind, parent_id = parent, source = { start_line = 1, end_line = 1, body = body },
      reply = answer and { question_body = body, body = answer, duration_ms = 1000 } or nil }
  end
  host.annotation = {
    message("1", "First question", "question", nil, "First answer"),
    message("2", "Separate comment"),
    message("3", "Follow-up comment", nil, "1"),
    message("4", "Nested question", "question", "3", "Nested answer"),
  }
  local review = host.open()
  local function toggle() keymap.invoke("<Tab>", { buffer = review.buf }) end
  local function summary()
    local row = vim.api.nvim_win_get_cursor(review.win)[1] - 1
    for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(review.buf, review.owner.comment_state.namespace,
      { row, 0 }, { row, -1 }, { details = true })) do
      if mark[4].virt_text then return vim.trim(mark[4].virt_text[1][1]) end
    end
  end
  eq(buffer_support.lines(review.buf), { "Source", "", "" })
  expand_thread(review, "1")
  expand_thread(review, "2")
  for _, body in ipairs({ "First question", "First answer", "Follow-up comment", "Nested question", "Nested answer" }) do
    vim.api.nvim_win_set_cursor(review.win, { assert(buffer_support.find_row(review.buf, body)), 0 })
    toggle()
    eq(summary(), "▸ 2 questions · 1 comment")
    eq(buffer_support.lines(review.buf), { "Source", "", "Separate comment" })
    toggle()
    eq(vim.api.nvim_get_current_line(), "First question")
  end
  vim.api.nvim_win_set_cursor(review.win, { assert(buffer_support.find_row(review.buf, "Separate comment")), 0 })
  toggle()
  eq(summary(), "▸ 0 questions · 1 comment")
  assert(buffer_support.find_row(review.buf, "Nested answer"))
  eq({ vim.bo[review.buf].modified, host.saves }, { false, 0 })
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
  expand_thread(scene.review, comment.id)
  assert(row_containing(scene, comment.source.body) > row_containing(scene, requirement),
    "range comment moved before the end of its selected source")
end

-- `startinsert` does nothing in Visual mode, so a comment opened over a selection left the editor
-- in V-LINE and the typed comment ran as commands.
T["comment over a visual selection leaves Visual mode for the editor"] = function()
  local scene = question_review()
  focus_source(scene, "Requirements")
  local heading_row = vim.api.nvim_win_get_cursor(scene.review.win)[1]
  keymap.feed("V" .. string.rep("<Down>", row_containing(scene, requirement, heading_row) - heading_row))
  eq(vim.api.nvim_get_mode().mode, "V")
  local selected_lines = vim.fn.getregion(vim.fn.getpos("v"), vim.fn.getpos("."), { type = "V" })
  local inserted = 0
  local original_cmd = vim.cmd
  vim.cmd = setmetatable({}, { __index = original_cmd, __call = function(_, command)
    if command == "startinsert" then inserted = inserted + 1 end
    return original_cmd(command)
  end })
  MiniTest.finally(function() vim.cmd = original_cmd end)
  keymap.invoke("C", { buffer = scene.review.buf, mode = "x" })
  vim.cmd = original_cmd
  eq({ vim.api.nvim_get_mode().mode, inserted }, { "n", 1 })
  local quote = vim.tbl_map(function(line) return line == "" and ">" or "> " .. line end, selected_lines)
  vim.list_extend(quote, { "", "" })
  eq(vim.api.nvim_buf_get_lines(0, 0, -1, true), quote)
  finish_input({ "Selected range" })
  local comment = scene.comments.capture(scene.review.buf)[1]
  assert(comment and comment.source.end_line > comment.source.start_line, "the comment lost its selected range")
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
  eq(scene.review.owner.comment_state.range_list[1].folded, true)
  expand_thread(scene.review, comment.id)
  assert(buffer_support.text(scene.review.buf):find(comment.source.body, 1, true), "reopening discarded the unsaved comment")
  assert(vim.bo[scene.review.buf].modified, "reopening cleared the unsaved marker")
  save(scene)
  assert(not scene.review.owner.saving, "reopened comment did not save")
  reopen(scene)
  expand_thread(scene.review, comment.id)
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
  -- The notice is one complete sentence that names what keeps the review open; a bare phrase
  -- ending at the notifier's edge read as a message cut off there.
  eq(messages, { "Plan review stays open until its running answers and edits finish." },
    { fail_reason = "pending close did not explain what keeps the review open" })
  leave_question(scene, question.id)
  scene.host.answer(1, "The caller owns it.")
  reopen(scene)
  assert_answer(scene, question, "The caller owns it.")
end

-- Presses made while a visibility toggle awaits its reply collapse to their parity, and every
-- reply's patch keeps the editor on the host's document revision.
T["rapid visibility toggles converge on the parity of the presses"] = function()
  local plan_review_host = dofile("nvim/lua/forge/tests/support/plan_review_host.lua")
  local plan_host = plan_review_host.start({ lines = { "pub struct Sample;", "pub fn sample();" } })
  local review = plan_host.open()
  local notices = notify.capture()
  local completed = 0
  local function press(count)
    vim.api.nvim_set_current_win(review.win)
    for _ = 1, count do review.command_set.action_by_id.toggle_visibility.run({}) end
    while plan_host.actions[completed + 1] do
      completed = completed + 1
      plan_host.complete_action(completed)
    end
  end
  press(10)
  eq(notices:messages(), {})
  eq({ host = plan_host.public_only, review = review.public_only }, { host = false, review = false })
  eq(review.owner.replica.revision, plan_host.revision)
  press(11)
  eq(notices:messages(), {})
  eq({ host = plan_host.public_only, review = review.public_only }, { host = true, review = true })
  eq(review.owner.replica.revision, plan_host.revision)
  press(1)
  eq({ host = plan_host.public_only, review = review.public_only }, { host = false, review = false })
  keymap.invoke("ot", { buffer = review.buf })
  keymap.invoke("oi", { buffer = review.buf })
  keymap.invoke("ot", { buffer = review.buf })
  press(0)
  eq({ review.public_only, review.owner.show_tests, review.owner.show_imports }, { false, false, true })
  eq(review.owner.replica.revision, plan_host.revision)
end

-- The revision a model submits after the review opened, with one line inserted above the
-- declaration so every later source line moves.
local revised_plan_lines = vim.list_extend({ "Overview" }, vim.deepcopy(question_plan_lines))

T["a new revision reloads the open review and carries unsaved drafts by their text"] = function()
  local scene = question_review()
  local comment = add_message(scene, "C", "Keep this draft across the revision.")
  assert(vim.bo[scene.review.buf].modified, "new comment was not dirty")
  local tab_count, review_window = #vim.api.nvim_list_tabpages(), scene.review.win
  local plan = scene.host.revise(revised_plan_lines)
  require("forge.views.plan_review.native_controller").observe(plan)
  local review = assert(require("forge.session").harness.plan_review, "review closed on the new revision")
  eq(review.plan.review_digest, plan.review_digest, { fail_reason = "open review stayed on the old revision" })
  eq({ tabs = #vim.api.nvim_list_tabpages(), window = review.win, focused = vim.api.nvim_get_current_win() },
    { tabs = tab_count, window = review_window, focused = review_window })
  eq(review.owner.saved_source_digest, "saved:2")
  local carried = scene.comments.capture(review.buf)
  eq(#carried, 1, { fail_reason = "the unsaved draft was not carried onto the new revision" })
  eq({ body = carried[1].source.body, line = carried[1].source.start_line },
    { body = comment.source.body, line = 6 })
  eq(scene.notices:messages(), {})
end

-- A new model revision replaces the document under the review's own window: the reviewer keeps the
-- tab and window they were in, whether that is the review or the Harness tab.
T["a new model revision reloads the review in place without moving focus"] = function()
  local scene = question_review()
  local tab_count = #vim.api.nvim_list_tabpages()
  local review_window = scene.review.win
  vim.api.nvim_set_current_tabpage(scene.review.return_tab)
  local focused = vim.api.nvim_get_current_win()
  local plan = scene.host.revise(revised_plan_lines)
  require("forge.views.plan_review.native_controller").observe(plan)
  local review = assert(require("forge.session").harness.plan_review, "review closed on the new revision")
  eq({ digest = review.plan.review_digest, window = review.win, tabs = #vim.api.nvim_list_tabpages(),
    focused = vim.api.nvim_get_current_win() },
    { digest = plan.review_digest, window = review_window, tabs = tab_count, focused = focused })
  eq(vim.api.nvim_win_get_buf(review_window), review.buf)
  assert(review.owner.attached(), "reloaded review did not attach")
  row_containing({ review = review }, "Overview")
  eq(scene.notices:messages(), {})
end

-- Review edits commit a new document version within the same model revision, and the reply of
-- the edit itself updates the review, so the session's plan record never reloads it, even while
-- the edit is still in flight.
T["a check toggle raises no notice and does not reload the review"] = function()
  local scene = question_review()
  local review = scene.review
  local owner = review.owner
  local controller = require("forge.views.plan_review.native_controller")
  stub(require("forge.views.plan_review.checks"), "open", function(_, apply) apply({ enabled = { lint = true } }) end)
  vim.api.nvim_set_current_win(review.win)
  review.command_set.action_by_id.checks.run({})
  local request = assert(scene.host.requests[1], "check toggle sent no request")
  eq(request.method, "session.plan.check.update")
  controller.observe(scene.host.edit())
  scene.host.complete_request(1)
  controller.observe(scene.host.plan)
  eq({ review = require("forge.session").harness.plan_review, owner = review.owner, closed = owner.closed == true },
    { review = review, owner = owner, closed = false })
  eq(review.plan.review_digest, scene.host.plan.review_digest)
  eq(vim.api.nvim_get_current_win(), review.win)
  eq(scene.notices:messages(), {})
end

T["a rename shows its preview before applying and raises no notice"] = function()
  local plan_review_host = dofile("nvim/lua/forge/tests/support/plan_review_host.lua")
  local plan_host = plan_review_host.start({ lines = { "pub struct Sample;", "pub fn sample() -> Sample;" },
    rename = { symbol = "sample", name = "Sample", existing_names = { "Sample" }, preview = {
      { block = "plan:1", column = 11, length = 6 }, { block = "plan:2", column = 19, length = 6 } } } })
  local review = plan_host.open()
  local notices = notify.capture()
  local owner = review.owner
  local controller = require("forge.views.plan_review.native_controller")
  local input, answer
  stub(require("forge.infra.popup_window"), "incremental_input", function(options, callback) input, answer = options, callback end)
  vim.api.nvim_set_current_win(review.win)
  vim.api.nvim_win_set_cursor(review.win, { 1, 11 })
  review.command_set.action_by_id.rename_entity.run({})
  assert(input, "rename did not ask for the new name")
  input.on_change("Portion")
  local namespace = vim.api.nvim_create_namespace("ForgePlanRename")
  local function previewed()
    return vim.tbl_map(function(mark) return mark[4].virt_text and mark[4].virt_text[1][1] end,
      vim.api.nvim_buf_get_extmarks(review.buf, namespace, 0, -1, { details = true }))
  end
  -- An earlier edit's plan record arrives while the reviewer is still typing the new name.
  controller.observe(plan_host.edit())
  eq({ preview = previewed(), requests = #plan_host.requests }, { preview = { "Portion", "Portion" }, requests = 0 })
  -- Entering the name keeps the preview on screen and asks before anything changes, so a name
  -- typed and entered in one motion still shows every span it rewrites.
  local confirmation
  stub(require("forge.infra.confirm"), "open", function(lines, on_yes, on_no, options)
    confirmation = { lines = lines, yes = on_yes, no = on_no, title = options and options.title }
  end)
  answer("Portion")
  eq({ preview = previewed(), requests = #plan_host.requests, confirming = confirmation ~= nil },
    { preview = { "Portion", "Portion" }, requests = 0, confirming = true })
  eq(confirmation.lines[1], "Rename Sample to Portion in 2 places?")
  confirmation.yes()
  local request = assert(plan_host.requests[1], "rename sent no request")
  eq({ method = request.method, name = request.params.name }, { method = "session.plan.entity.rename", name = "Portion" })
  plan_host.complete_request(1)
  controller.observe(plan_host.plan)
  eq({ review = require("forge.session").harness.plan_review, owner = review.owner, closed = owner.closed == true },
    { review = review, owner = owner, closed = false })
  eq(notices:messages(), {})
end

-- The host lists the names that already exist in the renamed declaration's scope with its
-- preview, those the workspace file declares as well as those the plan adds, so a colliding name
-- is refused when it is entered, before any confirmation asks the reviewer to decide.
T["a rename to a name that already exists in its scope is refused before confirmation"] = function()
  local plan_review_host = dofile("nvim/lua/forge/tests/support/plan_review_host.lua")
  local plan_host = plan_review_host.start({ lines = { "pub fn share();", "pub fn sample();" },
    rename = { symbol = "sample", name = "sample", existing_names = { "sample", "share" }, preview = {
      { block = "plan:2", column = 7, length = 6 } } } })
  local review = plan_host.open()
  local notices = notify.capture()
  local input, answer
  stub(require("forge.infra.popup_window"), "incremental_input", function(options, callback) input, answer = options, callback end)
  local confirmations = 0
  stub(require("forge.infra.confirm"), "open", function() confirmations = confirmations + 1 end)
  vim.api.nvim_set_current_win(review.win)
  vim.api.nvim_win_set_cursor(review.win, { 2, 7 })
  review.command_set.action_by_id.rename_entity.run({})
  assert(input, "rename did not ask for the new name")
  input.on_change("share")
  answer("share")
  local namespace = vim.api.nvim_create_namespace("ForgePlanRename")
  eq({ confirmations = confirmations, requests = #plan_host.requests,
    preview = #vim.api.nvim_buf_get_extmarks(review.buf, namespace, 0, -1, {}), notices = notices:messages() },
    { confirmations = 0, requests = 0, preview = 0, notices = { "`share` already exists in this scope" } })
end

-- The name input closes from its own Insert-mode submit key, and the confirmation it opens must
-- not record that ending Insert mode as the mode to return to: whichever way the reviewer answers,
-- the review is left in Normal mode.
T["closing the rename confirmation leaves the review in Normal mode"] = function()
  local plan_review_host = dofile("nvim/lua/forge/tests/support/plan_review_host.lua")
  local plan_host = plan_review_host.start({ lines = { "pub struct Sample;", "pub fn sample() -> Sample;" },
    rename = { symbol = "sample", name = "Sample", existing_names = { "Sample" }, preview = {
      { block = "plan:1", column = 11, length = 6 } } } })
  local review = plan_host.open()
  vim.keymap.set("n", "<F2>", function() review.command_set.action_by_id.rename_entity.run({}) end, { buffer = review.buf })
  for _, answer_key in ipairs({ "n", "y" }) do
    vim.api.nvim_set_current_win(review.win)
    vim.api.nvim_win_set_cursor(review.win, { 1, 11 })
    -- A mode change a closing popup requests takes effect at the next key, so the case reads the
    -- mode from a key typed after the answer, as the reviewer's next key would meet it.
    vim.api.nvim_feedkeys(vim.keycode("<F2>Portion<CR>" .. answer_key .. "<Cmd>let g:rename_closed_mode = mode(1)<CR><Esc>"), "xt", false)
    eq({ answer = answer_key, window = vim.api.nvim_get_current_win(), mode = vim.g.rename_closed_mode },
      { answer = answer_key, window = review.win, mode = "n" })
  end
  eq(#plan_host.requests, 1, { fail_reason = "the confirmed rename sent no request" })
end

-- A review the reviewer hid, or closed, stays out of the way; one notice says the revision is
-- ready and names the reopening key.
T["a new model revision of a hidden review is announced once"] = function()
  local scene = question_review()
  add_message(scene, "C", "Retain this across the hidden revision.")
  vim.api.nvim_set_current_win(scene.review.win)
  keymap.feed("q")
  local focused = vim.api.nvim_get_current_win()
  local tab_count = #vim.api.nvim_list_tabpages()
  local controller = require("forge.views.plan_review.native_controller")
  local plan = scene.host.revise(revised_plan_lines)
  controller.observe(plan)
  controller.observe(plan)
  eq(scene.notices:messages(), { "Plan revision 2 is ready · op" })
  eq({ focused = vim.api.nvim_get_current_win(), tabs = #vim.api.nvim_list_tabpages(),
    shown = #vim.fn.win_findbuf(scene.review.buf) }, { focused = focused, tabs = tab_count, shown = 0 })
  local review = scene.host.open()
  eq(review.plan.review_digest, plan.review_digest)
end

-- Review items no prompt delivered stay pending on the new revision. The host keeps the edits
-- and the comments it could not place on the record, and the editor names them once, so none is
-- dropped silently.
T["a new model revision names the review items that stay pending"] = function()
  local scene = question_review()
  vim.api.nvim_set_current_win(scene.review.win)
  keymap.feed("q")
  local controller = require("forge.views.plan_review.native_controller")
  local plan = vim.deepcopy(scene.host.revise(revised_plan_lines))
  plan.manual_changes = { { kind = "rename", from = "moneyAllocate", to = "moneyPortion" } }
  plan.unplaced_review_comments = { "Say whether zero parts are rejected." }
  controller.observe(plan)
  controller.observe(plan)
  eq(scene.notices:messages(), {
    "Plan revision 2 is ready · op",
    "Plan revision 2 did not receive these review items, which stay pending and go with your next"
      .. " message or decision:\n- Renamed moneyAllocate to moneyPortion\n- Comment: Say whether zero parts are rejected.",
  })
end

T["a new model revision of a closed review is announced once"] = function()
  local scene = question_review()
  vim.api.nvim_set_current_win(scene.review.win)
  keymap.feed("q")
  eq(require("forge.session").harness.plan_review, nil, { fail_reason = "saved PlanReview did not close" })
  local controller = require("forge.views.plan_review.native_controller")
  controller.observe(scene.host.edit())
  local plan = scene.host.revise(revised_plan_lines)
  controller.observe(plan)
  controller.observe(plan)
  eq(scene.notices:messages(), { "Plan revision 2 is ready · op" })
  eq(require("forge.session").harness.plan_review, nil)
end

-- A task that accepts plans automatically accepts the new revision without the reviewer, so a
-- notice inviting them to open it would describe a review that is no longer pending.
T["a new model revision of a task that accepts plans automatically is not announced"] = function()
  local scene = question_review()
  vim.api.nvim_set_current_win(scene.review.win)
  keymap.feed("q")
  local harness = require("forge.session").harness
  harness.session.active_task = { id = "task", kind = "plan", phase = "review", status = "waiting",
    auto_accept_plan = true, plan = { paused = false, artifact = { title = "Allocate", state = "awaiting_review" } } }
  local controller = require("forge.views.plan_review.native_controller")
  controller.observe(scene.host.revise(revised_plan_lines))
  eq(scene.notices:messages(), {})
end

-- A refused decision leaves the reviewer in the review with the host's whole reason; the reason
-- wraps to the notification width instead of running off it.
T["a refused decision keeps the review open and reports the host's reason"] = function()
  local scene = question_review()
  local review = scene.review
  local reason = "Plan acceptance was refused because the planning directory changed after the revision was submitted;"
    .. " revise the plan with /plan retry, then review the new revision before accepting it again."
  review.command_set.action_by_id.accept.run({})
  local decision_buf = vim.api.nvim_get_current_buf()
  eq(vim.bo[decision_buf].filetype, "ForgePlanDecision")
  keymap.invoke("<CR>", { buffer = decision_buf, mode = "i" })
  eq({ window = vim.api.nvim_get_current_win(), shown = vim.fn.win_findbuf(review.buf) },
    { window = review.win, shown = { review.win } })
  scene.host.complete_request(1, reason)
  eq({ review = require("forge.session").harness.plan_review, window = vim.api.nvim_get_current_win(),
    shown = vim.fn.win_findbuf(review.buf), tabs = #vim.api.nvim_list_tabpages() },
    { review = review, window = review.win, shown = { review.win }, tabs = 2 })
  eq(#scene.notices, 1)
  local notice = scene.notices[1]
  eq({ level = notice.level, text = (notice.message:gsub("\n", " ")) }, { level = vim.log.levels.ERROR, text = reason })
  local width = 0
  for _, line in ipairs(vim.split(notice.message, "\n")) do width = math.max(width, vim.fn.strdisplaywidth(line)) end
  assert(width < #reason, "the refusal reason was not wrapped")
end

-- Submitting from the decision popup returns focus to the review, never to the transcript, and
-- only the host's acceptance moves it to the composer.
T["submitting a decision keeps focus in the review until the host accepts"] = function()
  local scene = question_review()
  local review = scene.review
  local harness = require("forge.session").harness
  vim.api.nvim_set_current_tabpage(review.return_tab)
  vim.cmd("split")
  harness.composer_win = vim.api.nvim_get_current_win()
  vim.api.nvim_set_current_tabpage(review.tab)
  vim.api.nvim_set_current_win(review.win)
  review.command_set.action_by_id.request_changes.run({})
  local decision_buf = vim.api.nvim_get_current_buf()
  local order = {}
  vim.api.nvim_create_autocmd("WinEnter", { callback = function()
    local window = vim.api.nvim_get_current_win()
    order[#order + 1] = window == review.win and "review" or window == harness.transcript_win and "transcript"
      or window == harness.composer_win and "composer" or "other"
  end })
  keymap.invoke("<CR>", { buffer = decision_buf, mode = "i" })
  eq({ order = order, mode = vim.api.nvim_get_mode().mode }, { order = { "review" }, mode = "n" })
  scene.host.complete_request(1)
  eq(order[#order], "composer")
  assert(not vim.tbl_contains(order, "transcript"), "focus passed through the transcript: " .. vim.inspect(order))
  eq(scene.notices:messages(), {})
end

-- Closing a review returns to the Harness transcript, whose sign column takes two cells, and `op`
-- from there opens the review in a new tab that copies the transcript's window options. The review
-- presents its text at the same column however it was opened.
T["reopening a review from the transcript keeps the text column"] = function()
  local plan_review_host = dofile("nvim/lua/forge/tests/support/plan_review_host.lua")
  local plan_host = plan_review_host.start({ lines = question_plan_lines })
  local transcript = vim.api.nvim_get_current_win()
  vim.wo[transcript].signcolumn, vim.wo[transcript].statuscolumn = "yes:1", "%s"
  vim.cmd("split")
  vim.wo.signcolumn, vim.wo.foldcolumn, vim.wo.statuscolumn = "no", "0", ""
  local scene = { host = plan_host, review = plan_host.open(), comments = require("forge.draft_comments") }
  local function text_offset() return vim.fn.getwininfo(scene.review.win)[1].textoff end
  local opened = text_offset()
  local offsets = {}
  for round = 1, 2 do
    -- The second round holds an unsaved draft, so closing keeps the owner and `op` shows it again.
    if round == 2 then focus_source(scene) add_message(scene, "C", "Keep the column.") end
    vim.api.nvim_set_current_win(scene.review.win)
    keymap.feed("q")
    eq(vim.api.nvim_get_current_win(), transcript)
    scene.review = plan_host.open()
    offsets[round] = text_offset()
  end
  eq(offsets, { opened, opened })
end

-- A saved comment keeps the review's owner, so `q` hides the review and `op` shows the same owner in
-- a new tab. That tab copies the Harness window `op` ran in, and showing the review buffer again
-- restores the columns of the last window that showed it; after a reopen from the transcript, a
-- reopen from the composer drew the transcript's two-cell sign column in front of the review.
T["reopening a review with a saved comment keeps the text column"] = function()
  local plan_review_host = dofile("nvim/lua/forge/tests/support/plan_review_host.lua")
  local plan_host = plan_review_host.start({ lines = question_plan_lines })
  local transcript = vim.api.nvim_get_current_win()
  vim.wo[transcript].signcolumn, vim.wo[transcript].statuscolumn = "yes:1", "%s"
  vim.cmd("split")
  vim.wo.signcolumn, vim.wo.foldcolumn, vim.wo.statuscolumn = "no", "0", ""
  -- The composer opens below a transcript window that has no columns yet, as `layout.open` builds it.
  vim.cmd("belowright split")
  local composer = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(composer, buffer_support.scratch())
  require("forge.shared.input_gutter").apply(composer)
  vim.cmd("wincmd p")
  local scene = { host = plan_host, review = plan_host.open(), comments = require("forge.draft_comments") }
  local function text_offset() return vim.fn.getwininfo(scene.review.win)[1].textoff end
  local opened = text_offset()
  focus_source(scene)
  add_message(scene, "C", "Keep the column after saving.")
  save(scene)
  for _, origin in ipairs({ { "transcript", transcript }, { "composer", composer } }) do
    vim.api.nvim_set_current_win(scene.review.win)
    keymap.feed("q")
    vim.api.nvim_set_current_win(origin[2])
    scene.review = plan_host.open()
    eq(text_offset(), opened, { fail_reason = "reopening from the " .. origin[1] .. " moved the text" })
  end
end

-- `oY` while the task is paused records the acceptance and starts nothing. The reviewer learns that
-- once there is nothing left to answer: at once when the acceptance raised no questions, and not
-- while its questions still wait (the answer that completes them reports it instead).
T["an acceptance recorded while paused names /task resume once nothing is left to answer"] = function()
  local recorded = "Plan acceptance recorded. The task is paused; run /task resume to execute."
  for _, case in ipairs({
    { name = "no questions", status = "paused", expected = { recorded } },
    { name = "questions wait", status = "paused", elicitation = { owner = "plan_acceptance" }, expected = {} },
    { name = "running", status = "waiting", expected = {} },
  }) do
    local scene = question_review()
    local harness = require("forge.session").harness
    harness.session.active_task = { kind = "plan", status = case.status }
    local controller = package.loaded["forge.views.harness.controller"]
    controller.synchronize_state = function(settled)
      harness.active_elicitation = case.elicitation
      if settled then settled({}, nil) end
    end
    scene.review.command_set.action_by_id.accept.run({})
    keymap.invoke("<CR>", { buffer = vim.api.nvim_get_current_buf(), mode = "i" })
    local request = assert(scene.host.requests[1], "acceptance sent no request")
    request.callback({ id = "accept", state = "completed", outcome = { outcome = "task", result = {
      disposition = "awaiting_input", source = { id = "plan" }, task = { id = "task", revision = 2 },
      question_set = { id = "set", revision = 1, answer_revision = 0 } } } })
    local info = vim.tbl_map(function(entry) return entry.message end,
      vim.tbl_filter(function(entry) return entry.level == vim.log.levels.INFO end, scene.notices))
    eq({ case = case.name, notices = info }, { case = case.name, notices = case.expected })
    harness.active_elicitation = nil
  end
end

T["drafts whose text left the new revision are reported with their text"] = function()
  local scene = question_review()
  add_message(scene, "C", "Explain the removed declaration.")
  local plan = scene.host.revise({ "Requirements", "", "Preserve the requested behavior at its owning boundary." })
  require("forge.views.plan_review.native_controller").observe(plan)
  local review = assert(require("forge.session").harness.plan_review)
  eq(review.plan.review_digest, plan.review_digest)
  eq(#scene.comments.capture(review.buf), 0)
  local messages = table.concat(scene.notices:messages(), "\n")
  assert(messages:find("Explain the removed declaration.", 1, true), "dropped draft was not reported: " .. messages)
end

T["op reopens a hidden review with unsaved drafts on a new revision"] = function()
  local scene = question_review()
  local comment = add_message(scene, "C", "Retain this across the hidden revision.")
  vim.api.nvim_set_current_win(scene.review.win)
  keymap.feed("q")
  eq(vim.api.nvim_get_current_tabpage(), scene.review.return_tab, { fail_reason = "dirty PlanReview did not return to Harness" })
  scene.host.revise(revised_plan_lines)
  local review = scene.host.open()
  eq(review.plan.review_digest, "digest:2", { fail_reason = "op did not open the new revision" })
  eq(scene.notices:messages(), {})
  local carried = scene.comments.capture(review.buf)
  eq({ count = #carried, body = carried[1] and carried[1].source.body, line = carried[1] and carried[1].source.start_line },
    { count = 1, body = comment.source.body, line = 6 })
  assert(vim.bo[review.buf].modified, "carried draft lost its unsaved marker")
end

return T
