vim.loader.enable(false)
local root = vim.fs.normalize(vim.fn.getcwd())
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
assert(vim.fn.mkdir(workspace, "p") == 1 and vim.fn.mkdir(data, "p") == 1)
local executable = data .. "/forge" .. (vim.fn.has("win32") == 1 and ".exe" or "")
assert(vim.uv.fs_copyfile(require("forge.builder").binary_path(), executable))
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
package.loaded["forge.views.plan_review"] = require("forge.views.plan_review.native_controller")
local controller = require("forge.views.harness.controller")
local function await(predicate, message)
  assert(vim.wait(10000, function() return #errors > 0 or predicate() end, 10), message)
  assert(#errors == 0, table.concat(errors, "\n"))
end
local success, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = false, harness = { backend = "mock" } })
  require("forge.views.harness").open()
  await(function() return state.presentation and state.presentation.ready end, "Harness did not open")
  vim.api.nvim_buf_set_text(state.composer_buf, 0, 0, 0, 0, { "/plan build the feature" })
  controller.submit()
  await(function() return not state.busy and state.active_plan end, "mock plan did not arrive")
  local plan = state.active_plan
  local canonical = vim.fn.readfile(plan.working_path, "b")
  require("forge.views.plan_review").open(plan)
  await(function() return state.plan_review and state.plan_review.owner and state.plan_review.owner.ready end, "native PlanReview did not open")
  local review = state.plan_review
  assert(vim.bo[review.buf].buftype == "acwrite")
  assert(vim.fs.normalize(vim.api.nvim_buf_get_name(review.buf)) == vim.fs.normalize(plan.working_path))
  assert(vim.deep_equal(vim.fn.readfile(plan.working_path, "b"), canonical), "projection changed physical Markdown")
  local task_fold, task_fold_count = nil, 0
  for id, record in pairs(review.owner.replica.fold.record or {}) do
    if id:match("^plan:task:") then
      local _, block_row = review.owner.replica.sequence:position(record.owner)
      local row = block_row + record.fold.start.row + 1
      task_fold_count = task_fold_count + 1
      assert(vim.fn.foldclosed(row) >= 0, "native PlanReview left a task fold open by default")
      if vim.fn.foldclosed(row) == row then task_fold = { id = id, row = row } end
    end
  end
  assert(task_fold_count > 0, "native PlanReview projection has no task fold")
  assert(task_fold, "native PlanReview has no outermost task fold")
  assert(vim.fn.foldclosed(task_fold.row) == task_fold.row, "native PlanReview did not close task folds by default")
  vim.api.nvim_win_set_cursor(review.win, { task_fold.row, 0 })
  review.command_set.action_by_id.toggle.run({})
  assert(vim.fn.foldclosed(task_fold.row) == -1, "native PlanReview did not open the selected task fold")
  review.command_set.action_by_id.toggle.run({})
  assert(vim.fn.foldclosed(task_fold.row) == task_fold.row, "native PlanReview did not close the selected task fold")
  review.command_set.action_by_id.toggle.run({})
  assert(vim.fn.foldclosed(task_fold.row) == -1, "native PlanReview did not reopen the selected task fold")
  local source_row
  for row = 0, vim.api.nvim_buf_line_count(review.buf) - 1 do
    local location = require("forge.buffer").locate(review.owner.replica, row, 0)
    if location and location.target then source_row = row break end
  end
  assert(source_row, "native plan has no canonical source target")
  vim.api.nvim_win_set_cursor(review.win, { source_row + 1, 0 })
  local annotation
  review.owner.action("comment", function(result, action_error)
    assert(not action_error, action_error)
    annotation = result
  end)
  await(function() return annotation end, "native annotation was not created")
  local _, annotation_row = review.owner.replica.sequence:position(annotation.block)
  assert(annotation_row)
  local literal = "literal ** saved note"
  vim.api.nvim_buf_set_text(review.buf, annotation_row + annotation.row, 0, annotation_row + annotation.row, 0, { literal, "second line" })
  vim.cmd("write")
  await(function() return not vim.bo[review.buf].modified end, "annotation save was not acknowledged")
  assert(vim.deep_equal(vim.fn.readfile(plan.working_path, "b"), canonical), "annotation save overwrote canonical Markdown")
  review.command_set.action_by_id.close.run({})
  assert(state.plan_review == nil and vim.api.nvim_buf_is_valid(review.buf))
  require("forge.views.plan_review").open(plan)
  await(function() return state.plan_review and state.plan_review.owner.ready end, "native PlanReview did not reopen")
  assert(vim.deep_equal(vim.fn.readfile(plan.working_path, "b"), canonical))
  assert(table.concat(vim.api.nvim_buf_get_lines(state.plan_review.buf, 0, -1, false), "\n"):find(literal, 1, true), "saved annotation was lost on reopen")
  state.plan_review.command_set.action_by_id.accept.run({})
  await(function() return state.plan_review == nil and not state.busy end, "native plan acceptance did not settle")
  assert(vim.deep_equal(vim.fn.readfile(plan.working_path, "b"), canonical), "approval changed canonical Markdown")
  assert(state.presentation.close())
end, debug.traceback)
client.stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(root)
vim.fn.stdpath, vim.notify = original_stdpath, original_notify
vim.fn.delete(workspace, "rf")
vim.fn.delete(data, "rf")
assert(success and collected, failure or "PlanReview host was not collected")
print("plan_review_host: passed")
