vim.loader.enable(false)
local root = vim.fs.normalize(vim.fn.getcwd())
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
assert(vim.fn.mkdir(workspace, "p") == 1 and vim.fn.mkdir(data, "p") == 1)
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
  local comments = require("forge.draft_comments")
  local rendered = table.concat(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false), "\n")
  assert(rendered:find("src/change.rs", 1, true) and rendered:find("requested_change", 1, true))
  vim.api.nvim_win_set_cursor(review.win, { 2, 0 })
  review.owner.action("comment", function(result, failure) assert(result and not failure, failure) end)
  vim.cmd("stopinsert")
  local body_row = vim.api.nvim_win_get_cursor(review.win)[1] - 1
  local literal = "Sp literal ** note"
  vim.api.nvim_buf_set_lines(review.buf, body_row, body_row + 1, false, { literal, "second line", "" })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = review.buf })
  assert(comments.capture(review.buf)[1].source.body == literal .. "\nsecond line\n")
  vim.cmd("write")
  await(function() return not review.owner.saving end, "explicit annotation save did not settle")
  assert(not vim.bo[review.buf].modified)
  assert(vim.deep_equal(vim.fn.readfile(plan.working_path, "b"), canonical))
  vim.api.nvim_win_set_cursor(review.win, { 1, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = review.buf })
  local compact = table.concat(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false), "\n")
  assert(compact:find("╭─ Plan comment", 1, true))
  review.command_set.action_by_id.close.run({})
  assert(not state.plan_review and vim.api.nvim_buf_is_valid(review.buf))
  require("forge.views.plan_review").open(plan)
  await(function() return state.plan_review and state.plan_review.owner.ready end, "saved review did not reopen")
  review = state.plan_review
  assert(comments.capture(review.buf)[1].source.body == literal .. "\nsecond line\n")
  local original_request = client.request_for
  local submitted_capture
  client.request_for = function(session_id, method, params, callback)
    if method == "plan.acceptance.begin" then submitted_capture = vim.deepcopy(params) end
    return original_request(session_id, method, params, callback)
  end
  review.owner.saving = true
  review.command_set.action_by_id.accept.run({})
  assert(review.owner.pending_operation and not submitted_capture)
  local original_document = review.owner.document
  local original_generation = client.host_generation()
  client.stop()
  assert(vim.wait(5000, function() return client._client.process == nil end, 10), "old plan host was not collected")
  local restarted
  client.start_harness(function(snapshot, failure)
    assert(not failure, failure)
    controller.activate_snapshot(snapshot)
    restarted = true
  end)
  await(function() return restarted and state.presentation and state.presentation.ready end,
    "replacement plan host did not initialize")
  assert(client.host_generation() ~= original_generation)
  require("forge.views.plan_review").open(plan)
  await(function() return submitted_capture ~= nil end, "queued acceptance did not resume after document recovery")
  assert(submitted_capture.review.document ~= original_document)
  assert(submitted_capture.draft_annotation[1].source.body == literal .. "\nsecond line\n")
  await(function() return not state.plan_review and not state.busy end, "plan acceptance did not settle")
  client.request_for = original_request
  assert(vim.deep_equal(vim.fn.readfile(plan.working_path, "b"), canonical))
  require("forge.views.harness.workspace").release(state)
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
