vim.loader.enable(false)
local root = vim.fs.normalize(vim.fn.getcwd())
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
assert(vim.fn.mkdir(workspace, "p") == 1 and vim.fn.mkdir(data, "p") == 1)
assert(vim.system({ "git", "init", workspace }, { text = true }):wait(10000).code == 0)
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
local controller = require("forge.views.harness.controller")
local function await(predicate, message)
  assert(vim.wait(10000, function() return #errors > 0 or predicate() end, 10), message)
  assert(#errors == 0, table.concat(errors, "\n"))
end
local function request(method, params)
  local result, failure, done
  client.request(method, params, function(value, error) result, failure, done = value, error, true end)
  await(function() return done end, method .. " did not settle")
  assert(not failure, failure)
  return result
end
local success, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = false, harness = { backend = "mock" } })
  require("forge.views.harness").open()
  await(function() return state.presentation and state.presentation.ready end, "Harness did not open")
  assert(state.session.plan_auto_approve_revisions == true)
  local configured = request("session.configure", { plan_auto_approve_revisions = false })
  assert(configured.plan_auto_approve_revisions == false)
  vim.api.nvim_buf_set_text(state.composer_buf, 0, 0, 0, 0, { "/plan build the feature" })
  controller.submit()
  await(function() return not state.busy and state.active_plan end, "Plan did not arrive")
  local plan = state.active_plan
  request("plan.accept", { plan_id = plan.id, digest = plan.review_digest, execution_mode = "write" })
  await(function() return not state.busy and state.goal and state.goal.state == "stalled" end, "Read-only mock execution did not stop at its continuation guard")
  local snapshot = request("state.get", {})
  assert(snapshot.goal_execution.phase == "implement")
  assert(snapshot.goal_execution.state == "stalled")
  assert(snapshot.goal_execution.original_revision == 1 and snapshot.goal_execution.revision == 1)
  local text = table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
  assert(text:find("Execution started", 1, true), text)
  assert(text:find("Execution stopped at continuation limit", 1, true), text)
  local reviewed = request("plan.activate", { plan_id = plan.id, revision = 1 })
  require("forge.views.plan_review.native_controller").open(reviewed)
  await(function() return state.plan_review and state.plan_review.owner and state.plan_review.owner.ready end, "Execution report did not open")
  local review = state.plan_review
  local validation_row
  for position, line in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
    if line == "Verification:" then validation_row = position break end
  end
  assert(validation_row, "Verification requirements are missing")
  local validation_text = table.concat(vim.api.nvim_buf_get_lines(review.buf, validation_row - 1, validation_row + 6, false), "\n")
  assert(validation_text:find("  Automated:", 1, true) and validation_text:find("  Manual:", 1, true), validation_text)
  vim.api.nvim_win_set_cursor(review.win, { validation_row, 0 })
  vim.api.nvim_win_call(review.win, function()
    vim.cmd("normal! zc")
    assert(vim.fn.foldclosed(validation_row) == validation_row)
    assert(vim.fn.search("^Proposed declaration changes:", "bnw") < validation_row)
    local execution_row = vim.fn.search("Execution:", "nw")
    assert(execution_row > 0 and vim.fn.foldclosedend(validation_row) < execution_row,
      "verification fold includes the execution report: " .. table.concat(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false), "\n"))
    vim.cmd("normal! zo")
    assert(vim.fn.foldclosed(validation_row) == -1)
  end)
  local row
  for position, line in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
    if line:find("Execution:", 1, true) then row = position break end
  end
  assert(row, "Execution report heading is missing")
  vim.api.nvim_win_set_cursor(review.win, { row, 0 })
  local expanded
  review.owner.action("toggle_declaration", function(result, error) assert(not error, error) expanded = result end)
  await(function() return expanded end, "Execution report did not expand")
  text = table.concat(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false), "\n")
  assert(text:find("Original approved revision: 1", 1, true), text)
  assert(text:find("Missing: src/change.rs", 1, true), text)
  assert(text:find("Verification assessments", 1, true), text)
  require("forge.views.plan_review.native_controller").open(reviewed)
  await(function() return state.plan_review and state.plan_review ~= review and state.plan_review.owner.ready end, "Execution report did not refresh on reopening")
  state.plan_review.command_set.action_by_id.close.run({})
  require("forge.views.harness.workspace").release(state)
  assert(state.presentation.close())
end, debug.traceback)
require("forge.views.harness.workspace").release(state)
client.stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(root)
vim.fn.stdpath, vim.notify = original_stdpath, original_notify
vim.fn.delete(workspace, "rf")
vim.fn.delete(data, "rf")
assert(success and collected, failure or "Harness host was not collected")
print("semantic_execution_host: passed")
