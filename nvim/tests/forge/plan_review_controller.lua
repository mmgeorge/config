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
  if method == "plan.acceptance.begin" then pending = { params = params, callback = callback } return end
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
  else callback({}) end
end
local success, failure = xpcall(function()
  require("forge.views.plan_review.native_controller").open({ id = "plan", working_path = path, review_digest = "canonical" })
  local review = state.plan_review
  assert(review and review.owner.ready)
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
  for _, command in ipairs({ "toggle", "open", "jump_entity", "entity_info", "rename_entity", "schema", "comment", "accept", "request_changes", "close", "help" }) do
    assert(review.command_set.action_by_id[command], "missing PlanReview command " .. command)
  end
  local hidden_tab = review.tab
  vim.cmd("tabclose")
  require("forge.views.plan_review.native_controller").open({ id = "plan", working_path = path, review_digest = "canonical" })
  assert(review.tab ~= hidden_tab and review.tab == vim.api.nvim_get_current_tabpage())
  assert(review.win == vim.api.nvim_get_current_win(), "hidden review did not transfer its window ownership")
  review.command_set.action_by_id.accept.run({})
  assert(pending and pending.params.review.document == review.owner.document and pending.params.digest == nil)
  assert(state.plan_review == review, "review closed before native approval admission")
  pending.callback({ session = { id = "plan-session" } })
  assert(activated and state.plan_review == nil and not state.busy)
  assert(vim.api.nvim_buf_is_valid(review.buf), "closing PlanReview deleted its physical buffer")
  assert(vim.deep_equal(vim.fn.readfile(path), { "# Physical plan" }))
end, debug.traceback)
if state.plan_review then state.plan_review.owner.close() end
client.request_for, client.host_accepting = original_request, original_accepting
package.loaded["forge.views.harness.controller"] = original_controller
vim.fn.delete(path)
assert(success, failure)
print("plan_review_controller: passed")
