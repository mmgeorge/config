vim.loader.enable(false)
local root = vim.fs.normalize(vim.fn.getcwd())
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
assert(vim.fn.mkdir(workspace, "p") == 1 and vim.fn.mkdir(data, "p") == 1)
vim.fn.writefile({vim.json.encode({checks=vim.empty_dict()})},workspace .. "/.forge.json")
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
client._host_set_launcher_for_test(vim.system)
local state = require("forge.session").harness
package.loaded["forge.views.plan_review"] = require("forge.views.plan_review.native_controller")
local controller = require("forge.views.harness.controller")
local function await(predicate, message)
  assert(vim.wait(10000, function() return #errors > 0 or predicate() end, 10), message)
  assert(#errors == 0, table.concat(errors, "\n"))
end
local success, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = true,
    harness_log_path = data .. "/ui-perf.log", harness = { backend = "mock" } })
  require("forge.views.harness").open()
  await(function() return state.presentation and state.presentation.ready end, "Harness did not open")
  vim.api.nvim_buf_set_text(state.composer_buf, 0, 0, 0, 0, { "/plan build the feature" })
  controller.submit()
  await(function() return not state.busy and state.active_plan
    and state.active_plan.state == "awaiting_review" and state.active_plan.model_revision > 0 end,
    "mock plan did not reach review")
  local plan = state.active_plan
  local canonical = vim.fn.readfile(plan.working_path, "b")
  local submitted_revision = plan.model_revision
  local revision_directory = vim.fs.dirname(plan.working_path) .. "/revisions"
  local submitted_path = revision_directory .. ("/submitted-%04d.json"):format(submitted_revision)
  local submitted_content = vim.fn.readfile(submitted_path, "b")
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
  assert(review.public_only and review.owner.public_only, "PlanReview did not start Public")
  local visibility = vim.fn.maparg("ov", "n", false, true)
  assert(visibility.buffer == 1 and visibility.callback, "PlanReview did not bind ov")
  assert(vim.fn.maparg("ox", "n", false, true).buffer ~= 1,
    "Semantic-only PlanReview exposed a presentation toggle")
  visibility.callback()
  await(function() return review.public_only == false and review.owner.public_only == false end,
    "ov did not publish All visibility")
  visibility.callback()
  await(function() return review.public_only == true and review.owner.public_only == true end,
    "ov did not restore Public visibility")
  assert(vim.deep_equal(vim.fn.readfile(plan.working_path, "b"), canonical),
    "diff controls changed the saved plan")
  local owner, window, tab = review.owner, review.win, review.tab
  local trace_status
  client.request({ session_id = review.session_id, method = "host.trace.configure", params = { enabled = true } }, function(result, failure)
    assert(not failure, failure)
    trace_status = result
  end)
  await(function() return trace_status ~= nil end, "plan edit trace did not enable")
  for index, line in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
    local column = line:find("requested_change", 1, true)
    if column then vim.api.nvim_win_set_cursor(window, { index, column - 1 }) break end
  end
  local popup = require("forge.infra.popup_window")
  local previous_input = popup.incremental_input
  popup.incremental_input = function(_, callback) callback("reviewed_change") end
  review.command_set.action_by_id.rename_entity.run({})
  await(function() return review.plan.review_digest ~= plan.review_digest end, "native rename did not update the review")
  popup.incremental_input = previous_input
  assert(state.plan_review == review and review.owner == owner and review.win == window and review.tab == tab,
    "native rename reopened the plan")
  plan = review.plan
  assert(plan.model_revision == submitted_revision, "manual rename created a submitted revision")
  assert(vim.fn.filereadable(revision_directory .. ("/submitted-%04d.json"):format(submitted_revision + 1)) == 0,
    "manual rename wrote an immutable snapshot")
  assert(vim.deep_equal(vim.fn.readfile(submitted_path, "b"), submitted_content), "manual rename changed its submitted baseline")
  assert(plan.manual_changes[1] == "Renamed requested_change to reviewed_change")
  canonical = vim.fn.readfile(plan.working_path, "b")
  assert(table.concat(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false), "\n"):find("reviewed_change", 1, true))
  local trace_path
  client.request({ session_id = review.session_id, method = "host.trace.status", params = {} }, function(result, failure)
    assert(not failure, failure)
    trace_path = result.path
  end)
  await(function() return trace_path ~= nil end, "plan edit trace path did not arrive")
  local phases, operation_id = {}, nil
  for _, line in ipairs(vim.fn.readfile(trace_path)) do
    local record = vim.json.decode(line)
    if record.event == "plan.edit" and record.payload.method == "session.plan.entity.rename" then
      local payload = record.payload
      operation_id = operation_id or payload.operation_id
      assert(payload.operation_id == operation_id, "edit trace lost request correlation")
      if payload.status == "completed" then
        assert(type(payload.duration_ms) == "number" and payload.duration_ms >= 0)
        phases[payload.phase] = true
      end
    end
  end
  for _, phase in ipairs({ "service.total", "service.broker_lock_wait", "rename.reference_index",
    "rename.semantic_edit", "persist.working_json", "broker.timeline_reconcile",
    "service.recapture_review", "service.syntax", "review.project", "service.send_response" }) do
    assert(phases[phase], "missing plan edit phase: " .. phase)
  end
  for _, phase in ipairs({ "persist.submitted_json", "persist.submitted_markdown", "persist.submitted_navigation" }) do
    assert(not phases[phase], "draft rename performed a submitted snapshot write: " .. phase)
  end
  local perf = require("forge.infra.perf")
  perf.flush("harness")
  await(function()
    if vim.fn.filereadable(data .. "/ui-perf.log") ~= 1 then return false end
    for _, line in ipairs(vim.fn.readfile(data .. "/ui-perf.log")) do
      local record = vim.json.decode(line)
      if record.event == "ui.plan.edit" and record.phase == "complete" and record.operation_id == operation_id then return true end
    end
    return false
  end, "plan edit trace did not correlate the UI completion")
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
  local source_session = state.session.id
  vim.api.nvim_win_set_cursor(review.win, { 1, 0 })
  review.owner.action("question", function(result, failure) assert(result and not failure, failure) end)
  vim.cmd("stopinsert")
  local question_row = vim.api.nvim_win_get_cursor(review.win)[1] - 1
  vim.api.nvim_buf_set_lines(review.buf, question_row, question_row + 1, false, { "Why does this change preserve ownership?" })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = review.buf })
  vim.cmd("write")
  await(function() return not review.owner.saving and next(review.owner.reply_by_id) ~= nil end, "review question did not receive its forked answer")
  assert(state.session.id == source_session, "review answer replaced the source session")
  local original_request = client.request
  local submitted_capture
  client.request = function(request_specification, callback)
  local session_id = request_specification.session_id
  local method = request_specification.method
  local params = request_specification.params or {}

    if method == "session.plan.acceptance.begin" then submitted_capture = vim.deepcopy(params) end
    return original_request(request_specification, callback)
  end
  review.owner.annotation_saving, review.owner.saving = true, true
  review.command_set.action_by_id.accept.run({})
  local decision_buf = vim.api.nvim_get_current_buf()
  local decision_mapping
  for _, mapping in ipairs(vim.api.nvim_buf_get_keymap(decision_buf, "n")) do
    if mapping.lhs == "<CR>" then decision_mapping = mapping.callback end
  end
  assert(decision_mapping, "approval decision popup did not bind Enter")
  decision_mapping()
  assert(review.owner.pending_operation[1] and not submitted_capture)
  local original_document = review.owner.document
  local original_generation = client.host_get_generation()
  client.host_stop()
  assert(vim.wait(5000, function() return client._client.process == nil end, 10), "old plan host was not collected")
  local restarted
  client.session_initialize(function(snapshot, failure)
    assert(not failure, failure)
    controller.activate_snapshot(snapshot)
    restarted = true
  end)
  await(function() return restarted and state.presentation and state.presentation.ready end,
    "replacement plan host did not initialize")
  assert(client.host_get_generation() ~= original_generation)
  require("forge.views.plan_review").open(plan)
  await(function() return submitted_capture ~= nil end, "queued acceptance did not resume after document recovery")
  assert(submitted_capture.review.document ~= original_document)
  local retained_comment = false
  for _, annotation in ipairs(submitted_capture.draft_annotation) do
    retained_comment = retained_comment or annotation.source.body == literal .. "\nsecond line\n"
  end
  assert(retained_comment, "acceptance lost the saved comment")
  await(function() return not state.plan_review and not state.busy end, "plan acceptance did not settle")
  await(function() return state.active_plan and state.active_plan.model_revision == submitted_revision + 1 and not state.busy end,
    "final acceptance did not publish the reviewed draft")
  assert(vim.fn.filereadable(revision_directory .. ("/submitted-%04d.json"):format(submitted_revision + 1)) == 1,
    "final acceptance did not freeze the edited draft")
  assert(vim.fn.filereadable(revision_directory .. ("/submitted-%04d.json"):format(submitted_revision + 2)) == 0,
    "final acceptance created more than one review revision")
  assert(vim.deep_equal(vim.fn.readfile(submitted_path, "b"), submitted_content), "acceptance overwrote the original revision")
  client.request = original_request
  assert(vim.deep_equal(vim.fn.readfile(plan.working_path, "b"), canonical))
  require("forge.views.harness.workspace").release(state)
  assert(state.presentation.close())
end, debug.traceback)
require("forge.views.harness.workspace").release(state)
client.host_stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(root)
vim.fn.stdpath, vim.notify = original_stdpath, original_notify
vim.fn.delete(workspace, "rf")
vim.fn.delete(data, "rf")
assert(success and collected, failure or "PlanReview host was not collected")
print("plan_review_host: passed")
