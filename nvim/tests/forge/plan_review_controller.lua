vim.loader.enable(false)
local client = require("forge.client")
local original_request, original_accepting = client.request_for, client.host_accepting
client.host_accepting = function() return true end
local state = require("forge.session").harness
state.session = { id = "plan-session" }
state.transcript_win = vim.api.nvim_get_current_win()
local original_controller = package.loaded["forge.views.harness.controller"]
local activated
package.loaded["forge.views.harness.controller"] = { refresh_winbar = function() end, render = function() end,
  activate_snapshot = function(value) activated = value end, present_plan_question = function() end }
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
    callback({ path = path, version = 1, saved_source_digest = "saved", snapshot = { document = params.document, revision = 0,
      block = {
      { id = "overview", text = { "Plan overview", "" }, metadata = { decoration = {}, editable_region = {}, target = {} } },
      { id = "file", text = { "file src/plan.rs", "child entity" }, metadata = { decoration = {}, editable_region = {}, target = {}, fold = {
        { id = "plan:file-tree:1", start = { row = 0, column = 0 }, ["end"] = { block = "file", position = { row = 2, column = 0 } }, closed = true },
      } } },
      { id = "task", text = { "Native projected plan", "task detail" }, metadata = { decoration = {}, editable_region = {}, target = {
        { id = "task", range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = 21 } } },
      }, fold = {
        { id = "plan:task:1", start = { row = 0, column = 0 }, ["end"] = { block = "task", position = { row = 2, column = 0 } }, closed = false },
      } } } } } })
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
  local _, file_row = review.owner.replica.sequence:position("file")
  assert(vim.fn.foldclosed(file_row + 1) == file_row + 1,
    "native PlanReview did not preserve Rust default-closed file-tree folds")
  local _, task_row = review.owner.replica.sequence:position("task")
  assert(vim.fn.foldclosed(task_row + 1) == task_row + 1, "native PlanReview did not close task folds by default")
  vim.api.nvim_win_set_cursor(review.win, { task_row + 1, 0 })
  review.command_set.action_by_id.toggle.run({})
  assert(vim.fn.foldclosed(task_row + 1) == -1, "native PlanReview did not open the selected task fold")
  assert(review.task_folded_by_id["plan:task:1"] == false,
    "native PlanReview did not retain the opened task fold")
  review.command_set.action_by_id.toggle.run({})
  assert(vim.fn.foldclosed(task_row + 1) == task_row + 1, "native PlanReview did not close the selected task fold")
  vim.cmd("vsplit")
  vim.api.nvim_win_set_buf(0, review.buf)
  review.owner.refresh_views()
  assert(vim.fn.foldclosed(task_row + 1) == task_row + 1,
    "native PlanReview did not restore task folding when a second view attached")
  vim.api.nvim_set_current_win(review.win)
  for _, command in ipairs({ "toggle", "open", "jump_entity", "entity_info", "rename_entity", "schema", "comment", "delete", "accept", "request_changes", "close", "help" }) do
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
  require("forge.editable").detach(detached.replica.editable)
  vim.api.nvim_exec_autocmds("BufEnter", { buffer = review.buf })
  assert(not detached.attached(), "detached review remained reusable")
  vim.cmd("tabclose")
  require("forge.views.plan_review.native_controller").open({ id = "plan", working_path = path, review_digest = "canonical" })
  review = state.plan_review
  assert(detached.closed and review.owner ~= detached and review.owner.attached(),
    "reopening detached review did not establish a fresh native attachment")
  review.command_set.action_by_id.accept.run({})
  assert(pending and pending.params.review.document == review.owner.document and pending.params.digest == nil)
  assert(state.plan_review == nil and review.owner.closed, "approval dispatch did not close review")
  assert(vim.api.nvim_get_current_win() == state.transcript_win, "approval did not return to Harness")
  pending.callback({ session = { id = "plan-session" } })
  assert(activated and state.plan_review == nil and not state.busy)
  assert(vim.api.nvim_buf_is_valid(review.buf), "closing PlanReview deleted its physical buffer")
  assert(vim.deep_equal(vim.fn.readfile(path), { "# Physical plan" }))
  local native = require("forge.views.plan_review.native_controller")
  native.open(review.plan)
  review = state.plan_review
  local popup = require("forge.infra.popup_window")
  local original_input = popup.input
  popup.input = function(_, callback) callback("Please revise") end
  review.command_set.action_by_id.request_changes.run({})
  popup.input = original_input
  assert(pending.method == "plan.request_changes" and pending.params.comment == "Please revise")
  assert(state.plan_review == nil and review.owner.closed, "rejection waited for generation before closing")
  pending.callback(nil, "revision rejected")
  assert(state.plan_review and state.plan_review.owner.attached(), "failed submission did not restore review")
  vim.fn.maparg("q", "n", false, true).callback()
  assert(state.plan_review == nil, "q did not close review")
end, debug.traceback)
if state.plan_review then state.plan_review.owner.close() end
client.request_for, client.host_accepting = original_request, original_accepting
package.loaded["forge.views.harness.controller"] = original_controller
vim.fn.delete(path)
assert(success, failure)
print("plan_review_controller: passed")
