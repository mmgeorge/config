vim.loader.enable(false)
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
assert(vim.fn.mkdir(workspace .. "/src", "p") == 1)
assert(vim.fn.mkdir(data, "p") == 1)
local compact = "pub struct Registry { pub first: u64, pub second: u64 }"
vim.fn.writefile({ compact }, workspace .. "/src/change.rs")
local initialized = vim.system({ "git", "init", workspace }, { text = true }):wait(10000)
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
local function await(predicate, message)
  assert(vim.wait(10000, function() return #errors > 0 or predicate() end, 10), message)
  assert(#errors == 0, table.concat(errors, "\n"))
end
local function bytes(path) return table.concat(vim.fn.readfile(path, "b"), "\n") end
local function text(buffer) return table.concat(vim.api.nvim_buf_get_lines(buffer, 0, -1, false), "\n") end
local success, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = false, harness = { backend = "mock" } })
  require("forge.views.harness").open()
  await(function() return state.presentation and state.presentation.ready end, "Harness did not open")
  vim.api.nvim_buf_set_text(state.composer_buf, 0, 0, 0, 0, { "/plan revise the registry interface" })
  require("forge.views.harness.controller").submit()
  await(function() return not state.busy and state.active_plan end, "mock design did not arrive")
  local plan = state.active_plan
  local artifact_path = plan.working_path:gsub("%.md$", ".json")
  local artifact = bytes(artifact_path)
  local document = vim.json.decode(artifact)
  assert(document.design.baseline["src/change.rs"].text == compact .. "\n", "baseline stored display formatting")
  assert(document.design.proposed["src/change.rs"] == "pub fn reviewed_change();\n")
  local projection = bytes(plan.working_path)
  require("forge.views.plan_review").open(plan)
  await(function() return state.plan_review and state.plan_review.owner.ready end, "declaration review did not open")
  local review = state.plan_review
  assert(text(review.buf):find("  pub second: u64,", 1, true), "compact member did not receive display indentation")
  assert(bytes(artifact_path) == artifact and bytes(plan.working_path) == projection, "opening rewrote the plan")
  local file_row, hunk_row
  for row, line in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
    if line:find("Modified src/change.rs", 1, true) then file_row = row end
    if line:find("@@", 1, true) then hunk_row = row end
  end
  assert(file_row and hunk_row, "diff headers are missing")
  local function closed(row)
    return vim.api.nvim_win_call(review.win, function() return vim.fn.foldclosed(row) end)
  end
  local function toggle(row)
    vim.api.nvim_win_set_cursor(review.win, { row, 0 })
    review.command_set.action_by_id.toggle.run({})
  end
  toggle(file_row)
  assert(closed(file_row) == file_row and closed(hunk_row) == file_row,
    "closing a file left its first hunk visible")
  toggle(file_row)
  assert(closed(file_row) == -1 and closed(hunk_row) == -1, "file did not reopen")
  toggle(hunk_row)
  assert(closed(file_row) == -1 and closed(hunk_row) == hunk_row, "hunk did not close independently")
  toggle(hunk_row)
  vim.api.nvim_win_set_cursor(review.win, { file_row, 0 })
  vim.api.nvim_win_call(review.win, function() vim.cmd("normal! zc") end)
  assert(closed(hunk_row) == file_row, "native file folding left its first hunk visible")
  vim.api.nvim_win_call(review.win, function() vim.cmd("normal! zo") end)
  local selected
  for row = 0, vim.api.nvim_buf_line_count(review.buf) - 1 do
    if vim.api.nvim_buf_get_lines(review.buf, row, row + 1, false)[1]:find("pub second:", 1, true) then selected = row break end
  end
  assert(selected, "second member is missing")
  vim.api.nvim_win_set_cursor(review.win, { selected + 1, 0 })
  local opened
  review.owner.action("open", function(value, error_message) assert(not error_message, error_message) opened = value end)
  await(function() return opened end, "declaration snapshot did not open")
  local declaration_rows = {}
  for _, block in ipairs(opened.declarations.block) do
    for _, row in ipairs(block.text) do declaration_rows[#declaration_rows + 1] = row end
  end
  assert(table.concat(declaration_rows, "\n"):find("  pub second: u64,", 1, true), "snapshot used saved spacing")
  local comment
  review.owner.action("comment", function(value, error_message) assert(not error_message, error_message) comment = value end)
  await(function() return comment end, "comment did not attach")
  local _, comment_row = review.owner.replica.sequence:position(comment.block)
  vim.api.nvim_win_set_cursor(review.win, { comment_row + comment.row + 1, 0 })
  review.owner.sync_editability()
  vim.api.nvim_buf_set_text(review.buf, comment_row + comment.row, 0, comment_row + comment.row, 0, { "Review the second member" })
  vim.cmd("write")
  await(function() return not vim.bo[review.buf].modified end, "comment did not save")
  toggle(file_row)
  assert(closed(hunk_row) == file_row, "saving a comment changed the file fold boundary")
  toggle(file_row)
  local annotation_path = vim.fn.glob(vim.fs.dirname(artifact_path) .. "/review-annotations-*.json", false, true)[1]
  assert(annotation_path, "comment storage is missing")
  local annotation = vim.json.decode(bytes(annotation_path)).annotation[1]
  assert(annotation.anchor.start.side == "baseline" and annotation.anchor.start.line == 1, "comment retained a display line")
  assert(annotation.anchor.start.column > 20, "compact fields lost their distinct saved positions")
  review.command_set.action_by_id.close.run({})
  await(function() return state.plan_review == nil end, "review did not close")
  require("forge.views.plan_review").open(plan)
  await(function() return state.plan_review and state.plan_review.owner.ready end, "review did not reopen")
  assert(text(state.plan_review.buf):find("Review the second member", 1, true), "reopening lost the comment")
  assert(bytes(artifact_path) == artifact and bytes(plan.working_path) == projection, "comment reopening rewrote the design")
  state.plan_review.command_set.action_by_id.accept.run({})
  await(function() return state.plan_review == nil and not state.busy end, "approval did not settle")
  assert(bytes(workspace .. "/src/change.rs") == compact .. "\n", "design approval changed project source")
  assert(bytes(artifact_path) == artifact, "approval reformatted the saved design")
  require("forge.views.harness.controller").command_set().action_by_id.close.run({})
  assert(state.presentation == nil, "Harness did not release its presentation")
end, debug.traceback)
require("forge.views.harness.workspace").release(state)
client.stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.fn.stdpath, vim.notify = original_stdpath, original_notify
assert(success and collected, failure or "declaration review host was not collected")
print("plan_declaration_presentation_host: passed (fixtures: " .. data .. ")")
