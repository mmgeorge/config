vim.loader.enable(false)
local root = vim.fs.normalize(vim.fn.getcwd())
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
assert(vim.fn.mkdir(workspace, "p") == 1 and vim.fn.mkdir(data, "p") == 1)
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
  assert(vim.wo[review.win].virtualedit == "", "PlanReview enabled virtual cursor movement")
  local owner_count = 0
  for _, node in pairs(review.owner.replica.sequence.node or {}) do
    for _, overlay in ipairs(node.entry.metadata.source_overlay or {}) do
      if overlay.capture == "ForgeRightAlignedOwner" then
        owner_count = owner_count + 1
        local _, start = review.owner.replica.sequence:position(node.id)
        local row = start + overlay.range.start.row
        local text = vim.api.nvim_buf_get_lines(review.buf, row, row + 1, false)[1]
        assert(overlay.range.start.column == #text and overlay.range["end"].column == #text,
          "path label retained selectable source text")
        assert(not text:find("%s$"), "path alignment left trailing padding")
      end
    end
  end
  assert(owner_count > 0, "plan fixture has no virtual owner labels")
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
  local source_row, selection_end
  for row = 0, vim.api.nvim_buf_line_count(review.buf) - 1 do
    local location = require("forge.buffer").locate(review.owner.replica, row, 0)
    if location and location.target then
      if not source_row then source_row = row
      else selection_end = row break end
    end
  end
  assert(source_row, "native plan has no canonical source target")
  vim.api.nvim_win_set_cursor(review.win, { source_row + 1, 0 })
  assert(selection_end, "native plan has no second source target")
  vim.cmd("normal! V")
  vim.api.nvim_win_set_cursor(review.win, { selection_end + 1, 0 })
  local annotation
  review.owner.action("comment", function(result, action_error)
    assert(not action_error, action_error)
    annotation = result
  end)
  await(function() return annotation end, "native annotation was not created")
  local _, annotation_row = review.owner.replica.sequence:position(annotation.block)
  assert(annotation_row)
  local permitted = vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)
  assert(not vim.bo[review.buf].modifiable, "plan source rows must be read-only")
  assert(not pcall(vim.cmd, "normal! ggx"), "normal edit changed a protected source row")
  vim.bo[review.buf].modifiable = true
  local permitted_marks = vim.api.nvim_buf_get_extmarks(review.buf, review.owner.replica.namespace, 0, -1, { details = true })
  vim.api.nvim_buf_set_text(review.buf, 0, 0, 0, 0, { "forbidden edit" })
  assert(vim.wait(1000, function() return not review.owner.replica.editable.native.rejecting end, 10), "read-only edit was not restored")
  assert(#errors == 1 and errors[1] == "edit crosses a read-only boundary", table.concat(errors, "\n"))
  errors = {}
  for _, keys in ipairs({ "ggx", "ggdd", "ggu", "ggIforbidden\027" }) do
    vim.bo[review.buf].modifiable = true
    pcall(vim.cmd, "normal! " .. keys)
    assert(vim.wait(1000, function() return not review.owner.replica.editable.native.rejecting end, 10))
    for _, message in ipairs(errors) do assert(message == "edit crosses a read-only boundary", message) end
    errors = {}
    assert(review.owner.replica.changedtick == vim.api.nvim_buf_get_changedtick(review.buf), "stale counter after " .. keys)
    assert(vim.deep_equal(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false), permitted), "changed text after " .. keys)
  end
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false), permitted), "read-only edit changed plan text")
  assert(review.owner.replica.changedtick == vim.api.nvim_buf_get_changedtick(review.buf), "restored edit invalidated replica admission")
  assert(vim.deep_equal(vim.api.nvim_buf_get_extmarks(review.buf, review.owner.replica.namespace, 0, -1, { details = true }), permitted_marks),
    "rejected edit changed plan decoration anchors")
  assert(vim.api.nvim_buf_get_lines(review.buf, annotation_row, annotation_row + 1, false)[1]:find("lines ", 1, true), "visual selection lost its source range")
  vim.api.nvim_win_set_cursor(review.win, { annotation_row + annotation.row + 1, 0 })
  review.owner.sync_editability()
  assert(vim.bo[review.buf].modifiable, "comment body did not become editable")
  local literal = "literal ** saved note"
  vim.api.nvim_buf_set_text(review.buf, annotation_row + annotation.row, 0, annotation_row + annotation.row, 0, { literal, "second line" })
  vim.cmd("write")
  await(function() return not vim.bo[review.buf].modified end, "annotation save was not acknowledged")
  assert(vim.api.nvim_buf_get_lines(review.buf, annotation_row, annotation_row + 1, false)[1]:find("Plan comment", 1, true))
  vim.api.nvim_win_set_cursor(review.win, { 1, 0 })
  review.owner.sync_focus()
  await(function() return not review.owner.focus_pending and not review.owner.focused_annotation end, "comment did not compact on blur")
  local _, compact_row = review.owner.replica.sequence:position(annotation.block)
  assert(vim.api.nvim_buf_get_lines(review.buf, compact_row, compact_row + 1, false)[1]:find("╭─", 1, true))
  vim.api.nvim_win_set_cursor(review.win, { compact_row + 1, 0 })
  review.owner.sync_focus()
  await(function() return review.owner.focused_annotation == annotation.region and not review.owner.focus_pending end, "comment did not expand on focus")
  local _, expanded_row = review.owner.replica.sequence:position(annotation.block)
  assert(vim.api.nvim_win_get_cursor(review.win)[1] == expanded_row + 2, "focus did not enter comment body")
  vim.api.nvim_buf_set_text(review.buf, expanded_row + 1, 0, expanded_row + 1, 0, { "Revisited: " })
  vim.cmd("write")
  await(function() return not vim.bo[review.buf].modified end, "revisited annotation counter was rejected")
  vim.api.nvim_win_set_cursor(review.win, { source_row + 1, 0 })
  review.owner.sync_focus()
  await(function() return not review.owner.focus_pending and not review.owner.focused_annotation end, "second blur did not finish")
  review.owner.action("comment", function(_, action_error) assert(not action_error, action_error) end)
  vim.api.nvim_win_set_cursor(review.win, { vim.api.nvim_buf_line_count(review.buf), 0 })
  await(function() return not review.owner.add_pending and not review.owner.focus_pending and not review.owner.focused_annotation end,
    "abandoned empty annotation was not removed after a late add response")
  local count = 0
  for id in pairs(review.owner.replica.block) do if id:match("^plan:annotation:") then count = count + 1 end end
  assert(count == 1, "empty annotation survived blur or saved annotation disappeared")
  assert(vim.deep_equal(vim.fn.readfile(plan.working_path, "b"), canonical), "annotation save overwrote canonical Markdown")
  review.command_set.action_by_id.close.run({})
  assert(state.plan_review == nil and vim.api.nvim_buf_is_valid(review.buf))
  require("forge.views.plan_review").open(plan)
  await(function() return state.plan_review and state.plan_review.owner.ready end, "native PlanReview did not reopen")
  assert(vim.deep_equal(vim.fn.readfile(plan.working_path, "b"), canonical))
  assert(table.concat(vim.api.nvim_buf_get_lines(state.plan_review.buf, 0, -1, false), "\n"):find(literal, 1, true), "saved annotation was lost on reopen")
  state.plan_review.command_set.action_by_id.close.run({})
  local historical = vim.deepcopy(plan)
  historical.historical_revision = plan.model_revision
  historical.working_path = vim.fs.joinpath(vim.fs.dirname(plan.working_path), "revisions",
    ("submitted-%04d.md"):format(plan.model_revision))
  require("forge.views.plan_review").open(historical)
  await(function() return state.plan_review and state.plan_review.owner.ready end, "historical PlanReview did not open")
  local historical_review = state.plan_review
  assert(table.concat(vim.api.nvim_buf_get_lines(historical_review.buf, 0, -1, false), "\n"):find(literal, 1, true),
    "historical revision lost its saved annotation")
  local _, historical_row = historical_review.owner.replica.sequence:position("plan:annotation:" .. annotation.region)
  vim.api.nvim_win_set_cursor(historical_review.win, { historical_row + 1, 0 })
  historical_review.owner.sync_focus()
  historical_review.owner.sync_editability()
  assert(not vim.bo[historical_review.buf].modifiable, "historical annotation became editable")
  historical_review.command_set.action_by_id.delete.run({})
  historical_review.command_set.action_by_id.accept.run({})
  assert(state.plan_review == historical_review and not state.busy, "historical review accepted or deleted state")
  historical_review.command_set.action_by_id.close.run({})
  require("forge.views.plan_review").open(plan)
  await(function() return state.plan_review and state.plan_review.owner.ready end, "current review did not reopen after history")
  local deleted_block = "plan:annotation:" .. annotation.region
  local _, deleted_row = state.plan_review.owner.replica.sequence:position(deleted_block)
  vim.api.nvim_win_set_cursor(state.plan_review.win, { deleted_row + 1, 0 })
  vim.fn.maparg("J", "n", false, true).callback()
  await(function() return state.plan_review.owner.replica.block[deleted_block] == nil end,
    "J did not delete the selected plan comment")
  assert(not table.concat(vim.api.nvim_buf_get_lines(state.plan_review.buf, 0, -1, false), "\n"):find(literal, 1, true),
    "deleted annotation remained in the projection")
  assert(vim.deep_equal(vim.fn.readfile(plan.working_path, "b"), canonical), "comment deletion changed canonical Markdown")
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
