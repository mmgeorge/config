vim.loader.enable(false)
local client = require("forge.client")
local original_request, original_accepting = client.request_for, client.host_accepting
local original_generation = client.host_generation
local generation = 1
client.host_generation = function() return generation end
client.host_accepting = function() return true end
local state = require("forge.session").harness
state.session = { id = "plan-session" }
state.transcript_win = vim.api.nvim_get_current_win()
local original_controller = package.loaded["forge.views.harness.controller"]
local activated, execution
package.loaded["forge.views.harness.controller"] = { refresh_winbar = function() end, render = function() end,
  activate_snapshot = function(value) activated = value end, present_plan_question = function() end,
  task_transition = function(action) execution = action end }
local path = vim.fn.tempname() .. ".md"
vim.fn.writefile({ "# Physical plan" }, path)
local pending
client.request_for = function(session_id, method, params, callback)
  assert(session_id == "plan-session")
  if method == "plan.acceptance.begin" or method == "plan.request_changes" then
    pending = { method = method, params = params, callback = callback } return
  end
  assert(method == "harness.document")
  if params.operation == "plan_open" then
    local opened = { path = path, version = 1, saved_source_digest = "saved", snapshot = { document = params.document, revision = 0,
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
  else callback({}) end
end
local success, failure = xpcall(function()
  require("forge.views.plan_review.native_controller").open({ id = "plan", working_path = path, review_digest = "canonical" })
  local review = state.plan_review
  assert(review and review.owner.ready)
  assert(vim.wo[review.win].virtualedit == "", "PlanReview allows selecting past real text")
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
  for _, command in ipairs({ "toggle", "open", "jump_entity", "toggle_public", "comment", "delete", "accept", "request_changes", "close", "help" }) do
    assert(review.command_set.action_by_id[command], "missing PlanReview command " .. command)
  end
  assert(vim.fn.maparg("q", "n", false, true).buffer == 1, "PlanReview did not bind q")
  assert(vim.fn.maparg("J", "n", false, true).buffer == 1, "PlanReview did not bind J to comment deletion")
  vim.api.nvim_win_set_cursor(review.win, { task_row + 1, 3 })
  vim.cmd("clearjumps")
  local origin = vim.api.nvim_win_get_cursor(review.win)
  vim.fn.maparg(".", "n", false, true).callback()
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(review.win), { 1, 5 }), "plan jump did not reach declaration")
  vim.keymap.set("n", ",", "<C-o>", { buffer = review.buf })
  vim.api.nvim_feedkeys(",", "xt", false)
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(review.win), origin), "comma did not return to jump origin")
  local hidden_tab = review.tab
  vim.cmd("tabclose")
  require("forge.views.plan_review.native_controller").open({ id = "plan", working_path = path, review_digest = "canonical" })
  assert(review.tab ~= hidden_tab and review.tab == vim.api.nvim_get_current_tabpage())
  assert(review.win == vim.api.nvim_get_current_win(), "hidden review did not transfer its window ownership")
  local detached = review.owner
  assert(detached.close())
  vim.api.nvim_exec_autocmds("BufEnter", { buffer = review.buf })
  assert(not detached.attached(), "closed review remained reusable")
  vim.cmd("tabclose")
  require("forge.views.plan_review.native_controller").open({ id = "plan", working_path = path, review_digest = "canonical" })
  review = state.plan_review
  assert(detached.closed and review.owner ~= detached and review.owner.attached(),
    "reopening detached review did not establish a fresh native attachment")
  review.command_set.action_by_id.accept.run({})
  assert(pending and pending.params.review.document == review.owner.document and pending.params.digest == nil)
  assert(state.plan_review == review and not review.owner.closed, "approval closed before its capture completed")
  pending.callback({ session = { id = "plan-session" }, active_plan = { acceptance = {} } })
  assert(activated and state.plan_review == nil and not state.busy)
  assert(execution.action == "execute" and execution.plan_id == "plan" and execution.digest == "canonical")
  assert(vim.api.nvim_get_current_win() == state.transcript_win, "approval did not return to Harness")
  assert(vim.api.nvim_buf_is_valid(review.buf), "closing PlanReview deleted its physical buffer")
  assert(vim.deep_equal(vim.fn.readfile(path), { "# Physical plan" }))
  local native = require("forge.views.plan_review.native_controller")
  execution = nil
  native.open(review.plan)
  review = state.plan_review
  review.command_set.action_by_id.accept.run({})
  pending.callback({ session = { id = "plan-session" }, active_plan = { state = "accepted", acceptance = vim.NIL },
    goal_execution = { state = "paused" } })
  assert(execution == nil and state.plan_review == nil,
    "revision approval overrode the backend pause with an execute request")
  native.open(review.plan)
  review = state.plan_review
  local popup = require("forge.infra.popup_window")
  local original_input = popup.input
  popup.input = function(_, callback) callback("Please revise") end
  review.command_set.action_by_id.request_changes.run({})
  popup.input = original_input
  assert(pending.method == "plan.request_changes" and pending.params.comment == "Please revise")
  assert(state.plan_review == review and not review.owner.closed, "revision closed before its capture completed")
  pending.callback(nil, "revision rejected")
  assert(state.plan_review and state.plan_review.owner.attached(), "failed submission did not restore review")
  vim.fn.maparg("q", "n", false, true).callback()
  assert(state.plan_review == nil, "q did not close review")
  native.open(review.plan)
  review = state.plan_review
  review.owner.saving = true
  review.command_set.action_by_id.accept.run({})
  assert(review.owner.pending_operation and state.busy)
  generation = 2
  native.open(review.plan)
  local recovered = state.plan_review
  assert(recovered ~= review and recovered.owner.ready)
  assert(pending.params.review.document == recovered.owner.document)
  pending.callback({ session = { id = "plan-session" } })
  assert(recovered.owner.closed and not state.plan_review and not state.busy,
    "recovered submission completion retained the replacement review")
end, debug.traceback)
if state.plan_review and state.plan_review.owner then state.plan_review.owner.close() end
client.request_for, client.host_accepting = original_request, original_accepting
client.host_generation = original_generation
package.loaded["forge.views.harness.controller"] = original_controller
vim.fn.delete(path)
assert(success, failure)
print("plan_review_controller: passed")
