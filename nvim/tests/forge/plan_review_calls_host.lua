vim.loader.enable(false)
local root = vim.fn.getcwd()
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
assert(vim.fn.mkdir(workspace .. "/src", "p") == 1 and vim.fn.mkdir(data, "p") == 1)
local source = { "pub fn leaf() {}", "pub fn run() { leaf(); leaf(); }" }
vim.fn.writefile(source, workspace .. "/src/change.rs")
vim.fn.writefile({ "[package]", 'name = "call_navigation"', 'version = "0.1.0"', 'edition = "2021"', "[lib]", 'path = "src/change.rs"' }, workspace .. "/Cargo.toml")
vim.fn.writefile({ "invalid Rust {" }, workspace .. "/unrelated.rs")
local initialized = vim.system({ "git", "init", "--quiet", workspace }, { text = true }):wait(10000)
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
local picker = require("forge.views.picker")
local function await(predicate, message)
  local settled = vim.wait(30000, function() return #errors > 0 or predicate() end, 10)
  if not settled and state.transcript_buf and vim.api.nvim_buf_is_valid(state.transcript_buf) then
    message = message .. "\n" .. table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
  end
  assert(settled, message)
  assert(#errors == 0, table.concat(errors, "\n"))
end
local success, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = false, harness = { backend = "mock" } })
  require("forge.views.harness").open()
  await(function() return state.presentation and state.presentation.ready end, "Harness did not open")
  vim.api.nvim_buf_set_text(state.composer_buf, 0, 0, 0, 0, { "/plan change the dispatch interface" })
  require("forge.views.harness.controller").submit()
  await(function() return not state.busy and state.active_plan end, "plan did not arrive")
  local artifact = state.active_plan.working_path:gsub("%.md$", ".json")
  local canonical = vim.fn.readfile(artifact, "b")
  local revision_path = vim.fs.dirname(artifact) .. "/revisions/submitted-0001.json"
  local revision = vim.fn.readfile(revision_path, "b")
  local document = vim.json.decode(table.concat(canonical, "\n"))
  assert(not document.design.baseline["unrelated.rs"], "capture scanned an unrelated source")
  local captured
  for _, callable in ipairs(document.design.baseline_calls["src/change.rs"]) do
    if callable.owner == "run" then captured = callable.call end
  end
  assert(captured and #captured == 2 and captured[1].name == "leaf" and captured[2].name == "leaf")
  require("forge.views.plan_review").open(state.active_plan)
  await(function() return state.plan_review and state.plan_review.owner.ready end, "PlanReview did not open")
  local review = state.plan_review
  local leaf_row, leaf_column, call_row, call_count = nil, nil, nil, 0
  for index, row in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
    if row:find("pub fn leaf", 1, true) then leaf_row, leaf_column = index, row:find("leaf", 1, true) - 1 end
    if row:match("^%s+leaf$") then call_row, call_count = index, call_count + 1 end
  end
  assert(leaf_row, "baseline signature is absent")
  assert(call_count <= 1, "review did not deduplicate the saved occurrences")
  assert(call_count == 0 and table.concat(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false), "\n"):find("Calls...", 1, true), "Calls body did not start collapsed")
  local run_row
  for index, row in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
    if row:find("pub fn run", 1, true) then run_row = index end
  end
  vim.api.nvim_set_current_win(review.win)
  vim.api.nvim_win_set_cursor(review.win, { assert(run_row), 0 })
  review.command_set.action_by_id.toggle.run({})
  await(function()
    for _, row in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
      if row:match("^%s+leaf$") then return true end
    end
    return false
  end, "Tab on the function signature did not expand its Calls body")
  review.command_set.action_by_id.toggle.run({})
  await(function()
    for _, row in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
      if row:match("^%s+leaf$") then return false end
    end
    return true
  end, "Tab did not collapse the Calls body")
  if call_row then assert(vim.fn.foldclosed(call_row) ~= -1, "Calls body did not start folded") end
  vim.api.nvim_set_current_win(review.win)
  vim.api.nvim_win_set_cursor(review.win, { leaf_row, leaf_column })
  vim.cmd("silent! normal! zv")
  review.command_set.action_by_id.references.run({})
  await(function() return picker.is_open() end, "reference picker did not open")
  local active = picker._state_for_test()
  assert(active.spec.page_list[1].search, "reference picker is not searchable")
  assert(#active.spec.page_list[1].option_list == 1, "duplicate calls produced duplicate caller results")
  assert(active.spec.page_list[1].option_list[1].value.owner == "run")
  local select = vim.api.nvim_buf_call(active.buf, function() return vim.fn.maparg("<CR>", "n", false, true) end)
  assert(type(select.callback) == "function", "picker confirmation is unavailable")
  select.callback()
  await(function() return not picker.is_open() and vim.api.nvim_win_get_cursor(review.win)[1] ~= leaf_row end, "reference selection did not jump")
  assert(vim.api.nvim_win_get_buf(review.win) == review.buf, "reference selection left the plan buffer")
  local cursor = vim.api.nvim_win_get_cursor(review.win)
  local target = vim.api.nvim_buf_get_lines(review.buf, cursor[1] - 1, cursor[1], false)[1]
  assert(target:match("^%s+leaf$"), "reference jump missed the Calls entry: " .. target)
  assert(vim.fn.foldclosed(cursor[1]) == -1, "reference destination remains folded")
  review.command_set.action_by_id.jump_entity.run({})
  await(function() return vim.api.nvim_win_get_cursor(review.win)[1] == leaf_row end, "Calls definition jump did not select the plan definition")
  vim.api.nvim_win_set_cursor(review.win, cursor)
  review.command_set.action_by_id.references.run({})
  await(function() return picker.is_open() end, "call reference picker did not reopen")
  active = picker._state_for_test()
  vim.api.nvim_win_set_cursor(review.win, { leaf_row, leaf_column })
  select = vim.api.nvim_buf_call(active.buf, function() return vim.fn.maparg("<CR>", "n", false, true) end)
  select.callback()
  vim.wait(100, function() return false end, 10)
  assert(vim.api.nvim_win_get_cursor(review.win)[1] == leaf_row, "stale picker selection changed the review cursor")
  assert(vim.deep_equal(vim.fn.readfile(artifact, "b"), canonical), "reference navigation modified the plan")
  assert(vim.deep_equal(vim.fn.readfile(workspace .. "/src/change.rs"), source), "planning modified implementation source")
  local function select_new_definition()
    vim.api.nvim_set_current_win(review.win)
    for index, row in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
      if row:find("pub fn reviewed_change", 1, true) then
        vim.api.nvim_win_set_cursor(review.win, { index, row:find("reviewed_change", 1, true) - 1 }) return
      end
    end
    error("new plan function is absent")
  end
  local function rename_input()
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
      if vim.bo[buf].filetype == "ForgeRenameInput" then return buf end
    end
  end
  select_new_definition()
  local binding = vim.api.nvim_buf_call(review.buf, function() return vim.fn.maparg("<Space>f", "n", false, true) end)
  assert(type(binding.callback) == "function", "plan rename binding is absent")
  binding.callback()
  await(function() return rename_input() ~= nil end, "rename popup did not open")
  local input = rename_input()
  vim.api.nvim_buf_set_lines(input, 0, -1, false, { "dispatch" })
  vim.api.nvim_exec_autocmds("TextChangedI", { buffer = input })
  local namespace = vim.api.nvim_create_namespace("ForgePlanRename")
  assert(#vim.api.nvim_buf_get_extmarks(review.buf, namespace, 0, -1, {}) > 0, "incremental rename did not preview")
  assert(vim.deep_equal(vim.fn.readfile(artifact, "b"), canonical), "rename preview modified the plan")
  local cancel = vim.api.nvim_buf_call(input, function() return vim.fn.maparg("<Esc>", "n", false, true) end)
  cancel.callback()
  assert(#vim.api.nvim_buf_get_extmarks(review.buf, namespace, 0, -1, {}) == 0, "cancel left preview marks")
  assert(vim.deep_equal(vim.fn.readfile(artifact, "b"), canonical), "cancel persisted a rename")
  select_new_definition()
  review.command_set.action_by_id.rename_entity.run({})
  await(function() return rename_input() ~= nil end, "rename popup did not reopen")
  input = rename_input()
  vim.api.nvim_buf_set_lines(input, 0, -1, false, { "dispatch" })
  local confirm = vim.api.nvim_buf_call(input, function() return vim.fn.maparg("<CR>", "n", false, true) end)
  confirm.callback()
  await(function() return not state.busy and state.plan_review ~= review and state.plan_review and state.plan_review.owner.ready end, "rename did not create a new review")
  local renamed = vim.json.decode(table.concat(vim.fn.readfile(artifact, "b"), "\n"))
  assert(renamed.version == document.version + 1, "rename did not advance the plan version")
  assert(renamed.design.proposed["src/change.rs"]:find("pub fn dispatch", 1, true), "rename did not persist the new declaration")
  assert(vim.deep_equal(renamed.design.baseline, document.design.baseline), "rename changed captured baselines")
  assert(vim.deep_equal(vim.fn.readfile(revision_path, "b"), revision), "rename changed an earlier revision")
  assert(vim.deep_equal(vim.fn.readfile(workspace .. "/src/change.rs"), source), "rename edited implementation source")
  review = state.plan_review
  review.command_set.action_by_id.close.run({})
  require("forge.views.harness.controller").command_set().action_by_id.close.run({})
end, debug.traceback)
picker.close(false)
require("forge.views.harness.workspace").release(state)
client.stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(root)
vim.fn.stdpath, vim.notify = original_stdpath, original_notify
if success and collected then
  assert(vim.fs.normalize(vim.fs.abspath(workspace)) == vim.fs.normalize(workspace) and workspace ~= root)
  assert(vim.fs.normalize(vim.fs.abspath(data)) == vim.fs.normalize(data) and data ~= root)
  vim.fn.delete(workspace, "rf")
  vim.fn.delete(data, "rf")
end
assert(success and collected, failure or "host was not collected")
print("plan_review_calls_host: passed")
