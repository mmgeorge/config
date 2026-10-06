vim.loader.enable(false)
local root = vim.fn.getcwd()
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
assert(vim.fn.mkdir(workspace .. "/src", "p") == 1 and vim.fn.mkdir(data, "p") == 1)
local source = { "pub fn leaf() {}", "pub fn run() { leaf(); leaf(); }", "pub struct Client { pub count: usize }", "pub fn update(client: &mut Client) { leaf(); client.count += 1; let _count = client.count; }", "/// Describes the following declaration.", "pub struct Following;" }
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
  local properties
  for _, callable in ipairs(document.design.baseline_calls["src/change.rs"]) do
    if callable.owner == "update" then
      assert(#callable.call == 3 and callable.call[1].name == "leaf")
      properties = vim.tbl_filter(function(occurrence) return occurrence.kind == "property" end, callable.call)
    end
  end
  assert(properties and #properties == 2 and properties[1].kind == "property" and properties[2].kind == "property")
  assert(properties[1].name == "Client::count" and properties[2].name == "Client::count")
  local original_columns = vim.o.columns
  vim.o.columns = 48
  require("forge.views.plan_review").open(state.active_plan)
  await(function() return state.plan_review and state.plan_review.owner.ready end, "PlanReview did not open")
  local review = state.plan_review
  local description_row
  local paragraph = vim.split(document.design.document.description, "\n", { plain = true })[1]
  for index, row in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
    if row == paragraph then description_row = index break end
  end
  assert(description_row, "PlanReview hard-wrapped the saved description paragraph")
  assert(vim.wo[review.win].wrap and vim.wo[review.win].linebreak, "PlanReview does not use native word wrapping")
  vim.o.columns = 160
  vim.cmd("vsplit")
  local split_window = vim.api.nvim_get_current_win()
  local before_resize = vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)
  local function paragraph_height(window)
    return vim.api.nvim_win_text_height(window, { start_row = description_row - 1, end_row = description_row - 1 }).all
  end
  vim.api.nvim_win_set_width(review.win, 110)
  vim.cmd("redraw!")
  local wide_height = paragraph_height(review.win)
  assert(paragraph_height(split_window) > wide_height, "same-buffer split did not wrap the paragraph at its own width")
  vim.api.nvim_win_set_width(review.win, 40)
  vim.cmd("redraw!")
  assert(paragraph_height(review.win) > wide_height, "narrowing PlanReview did not reflow its paragraph")
  vim.api.nvim_win_set_width(review.win, 110)
  vim.cmd("redraw!")
  assert(paragraph_height(review.win) == wide_height, "widening PlanReview retained obsolete line breaks")
  assert(vim.deep_equal(before_resize, vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)), "resize rewrote physical plan rows")
  vim.api.nvim_set_current_win(review.win)
  vim.api.nvim_win_close(split_window, true)
  vim.o.columns = original_columns
  local reference_binding = vim.api.nvim_buf_call(review.buf, function() return vim.fn.maparg("of", "n", false, true) end)
  local plan_reference_binding = vim.api.nvim_buf_call(review.buf, function() return vim.fn.maparg("or", "n", false, true) end)
  assert(reference_binding.buffer == 1 and type(reference_binding.callback) == "function", "LSP reference shortcut is not owned by the plan buffer")
  assert(plan_reference_binding.buffer == 1 and plan_reference_binding.callback == reference_binding.callback, "plan reference shortcuts do not share the same action")
  local leaf_row, leaf_column, call_row, call_count = nil, nil, nil, 0
  for index, row in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
    if row:find("pub fn leaf", 1, true) then leaf_row, leaf_column = index, row:find("leaf", 1, true) - 1 end
    if row:match("^%s+leaf$") then call_row, call_count = index, call_count + 1 end
  end
  assert(leaf_row, "baseline signature is absent")
  assert(call_count <= 1, "review did not deduplicate the saved occurrences")
  assert(call_count == 0 and table.concat(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false), "\n"):find("pub fn run() {...}", 1, true), "Calls body did not start collapsed")
  for _, row in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
    assert(not row:match("^%s*Calls$") and not row:match("^%s*Accesses$"), "collapsed functions exposed a body category")
  end
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
  local property_row, property_column
  for index, row in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
    if row:find("count: usize", 1, true) then property_row, property_column = index, row:find("count", 1, true) - 1 end
  end
  assert(property_row, "captured property declaration is absent")
  assert(table.concat(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false), "\n"):find("pub fn update(client: &mut Client) {...}", 1, true), "property body did not start collapsed")
  vim.api.nvim_set_current_win(review.win)
  vim.api.nvim_win_set_cursor(review.win, { property_row, property_column })
  vim.cmd("silent! normal! zv")
  vim.keymap.set("n", ",", "<C-o>")
  vim.cmd("clearjumps")
  vim.api.nvim_feedkeys("of", "mtx", false)
  await(function() return picker.is_open() end, "property reference picker did not open")
  local property_picker = picker._state_for_test()
  assert(#property_picker.spec.page_list[1].option_list == 1, "duplicate accesses produced duplicate caller results")
  assert(property_picker.spec.page_list[1].option_list[1].value.kind == "property")
  local preview_namespace = vim.api.nvim_create_namespace("forge.plan.references.preview")
  await(function() return #vim.api.nvim_buf_get_extmarks(review.buf, preview_namespace, 0, -1, {}) == 1 end, "property selection did not preview its usage")
  local preview_mark = vim.api.nvim_buf_get_extmarks(review.buf, preview_namespace, 0, -1, { details = true })[1]
  assert(preview_mark[4].line_hl_group == "Visual" and not preview_mark[4].hl_group, "reference preview must use only the Snacks-style line highlight")
  assert(vim.api.nvim_get_current_win() == property_picker.win, "preview took focus from the picker")
  assert(#vim.api.nvim_win_call(review.win, vim.fn.getjumplist)[1] == 0, "property preview added a jump")
  local property_select = vim.api.nvim_buf_call(property_picker.buf, function() return vim.fn.maparg("<CR>", "n", false, true) end)
  property_select.callback()
  await(function() return not picker.is_open() and vim.api.nvim_win_get_cursor(review.win)[1] ~= property_row end, "property selection did not jump")
  local property_cursor = vim.api.nvim_win_get_cursor(review.win)
  assert(vim.api.nvim_buf_get_lines(review.buf, property_cursor[1] - 1, property_cursor[1], false)[1]:match("^%s+Client::count$"))
  local property_location = require("forge.buffer").locate(review.owner.replica, property_cursor[1] - 1, property_cursor[2])
  local property_style = review.owner.replica.sequence.node[property_location.block].entry.metadata.visible_decoration
  assert(#property_style == 2 and property_style[1].capture == "@type" and property_style[2].capture == "@variable.member", "property reference lost its semantic highlights in the host")
  assert(#vim.fn.getjumplist()[1] == 1, "confirmation did not record exactly one origin")
  vim.api.nvim_feedkeys(",", "mtx", false)
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(review.win), { property_row, property_column }), "comma did not return to the property declaration")
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-i>", true, false, true), "ntx", false)
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(review.win), property_cursor), "forward jump did not return to the selected access")
  local expanded_body = table.concat(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false), "\n")
  assert(expanded_body:find("pub fn update(client: &mut Client) {\n  Calls\n    leaf\n  Accesses\n    Client::count\n}", 1, true), "reference selection did not reveal both categories inside the function body")
  local following_comment = false
  for index, row in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
    if row == "/// Describes the following declaration." then
      following_comment = true
      local location = require("forge.buffer").locate(review.owner.replica, index - 1, 0)
      local style = review.owner.replica.sequence.node[location.block].entry.metadata.visible_decoration
      assert(vim.tbl_contains(vim.tbl_map(function(span) return span.capture end, style), "@comment.documentation.rust"), "synthetic body shifted the following documentation highlight")
    end
  end
  assert(following_comment, "following declaration comment is absent")
  review.command_set.action_by_id.jump_entity.run({})
  await(function() return vim.api.nvim_win_get_cursor(review.win)[1] == property_row end, "property definition jump missed its declaration")
  vim.api.nvim_set_current_win(review.win)
  vim.api.nvim_win_set_cursor(review.win, { leaf_row, leaf_column })
  vim.cmd("silent! normal! zv")
  vim.cmd("clearjumps")
  vim.api.nvim_feedkeys("or", "mtx", false)
  await(function() return picker.is_open() end, "reference picker did not open")
  local active = picker._state_for_test()
  assert(active.spec.page_list[1].search, "reference picker is not searchable")
  assert(#active.spec.page_list[1].option_list == 2, "calls did not retain both distinct caller results")
  assert(active.spec.page_list[1].option_list[1].value.owner == "run")
  await(function() return #vim.api.nvim_buf_get_extmarks(review.buf, preview_namespace, 0, -1, {}) == 1 end, "initial call preview is absent")
  local first_preview = vim.api.nvim_win_get_cursor(review.win)
  local first_option = active.frame.lines[active.frame.option_range[1].first]
  local second_option = active.frame.lines[active.frame.option_range[2].first]
  assert(first_option:find("run", 1, true) == second_option:find("update", 1, true), "caller columns are not aligned")
  assert(first_option:find("call", 1, true) == second_option:find("call", 1, true), "kind columns are not aligned")
  local next_reference = vim.api.nvim_buf_call(active.buf, function() return vim.fn.maparg("<Down>", "n", false, true) end)
  next_reference.callback()
  await(function() return vim.api.nvim_win_get_cursor(review.win)[1] ~= first_preview[1]
    and #vim.api.nvim_buf_get_extmarks(review.buf, preview_namespace, 0, -1, {}) == 1 end, "picker movement did not update the preview")
  assert(vim.api.nvim_get_current_win() == active.win, "reference movement changed focus")
  local visible_height = require("forge.views.picker.layout").host_bounds({ review.win }).height
    - vim.api.nvim_win_get_height(active.win) - 2
  assert(vim.api.nvim_win_call(review.win, vim.fn.winline) <= visible_height, "preview is hidden under the picker")
  local previous_reference = vim.api.nvim_buf_call(active.buf, function() return vim.fn.maparg("<Up>", "n", false, true) end)
  previous_reference.callback()
  await(function() return vim.deep_equal(vim.api.nvim_win_get_cursor(review.win), first_preview)
    and #vim.api.nvim_buf_get_extmarks(review.buf, preview_namespace, 0, -1, {}) == 1 end, "previous reference did not restore its preview")
  local select = vim.api.nvim_buf_call(active.buf, function() return vim.fn.maparg("<CR>", "n", false, true) end)
  assert(type(select.callback) == "function", "picker confirmation is unavailable")
  assert(#vim.api.nvim_win_call(review.win, vim.fn.getjumplist)[1] == 0, "reference previews added jumps")
  select.callback()
  await(function() return not picker.is_open() and vim.api.nvim_win_get_cursor(review.win)[1] ~= leaf_row end, "reference selection did not jump")
  assert(vim.api.nvim_win_get_buf(review.win) == review.buf, "reference selection left the plan buffer")
  local cursor = vim.api.nvim_win_get_cursor(review.win)
  local target = vim.api.nvim_buf_get_lines(review.buf, cursor[1] - 1, cursor[1], false)[1]
  assert(target:match("^%s+leaf$"), "reference jump missed the Calls entry: " .. target)
  local call_location = require("forge.buffer").locate(review.owner.replica, cursor[1] - 1, cursor[2])
  local call_style = review.owner.replica.sequence.node[call_location.block].entry.metadata.visible_decoration
  local function_style = vim.tbl_filter(function(span) return span.capture == "@function.call" and span.priority == 200 end, call_style)
  assert(#function_style == 1 and target:sub(function_style[1].range.start.column + 1, function_style[1].range["end"].column) == "leaf", "call reference lost its function highlight in the host")
  assert(#vim.fn.getjumplist()[1] == 1, "call confirmation did not record exactly one origin")
  vim.api.nvim_feedkeys(",", "mtx", false)
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(review.win), { leaf_row, leaf_column }), "comma did not return to the function declaration")
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-i>", true, false, true), "ntx", false)
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(review.win), cursor), "forward jump did not return to the selected call")
  assert(#vim.api.nvim_buf_get_extmarks(review.buf, preview_namespace, 0, -1, {}) == 0, "confirmed reference retained its preview highlight")
  for _, row in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
    assert(not row:find("Reference context:", 1, true), "reference selection appended unformatted context")
  end
  assert(vim.fn.foldclosed(cursor[1]) == -1, "reference destination remains folded")
  review.command_set.action_by_id.jump_entity.run({})
  await(function() return vim.api.nvim_win_get_cursor(review.win)[1] == leaf_row end, "Calls definition jump did not select the plan definition")
  vim.api.nvim_win_set_cursor(review.win, cursor)
  vim.cmd("clearjumps")
  review.command_set.action_by_id.references.run({})
  await(function() return picker.is_open()
    and #vim.api.nvim_buf_get_extmarks(review.buf, preview_namespace, 0, -1, {}) == 1 end, "cancel fixture did not preview")
  local cancel_picker = picker._state_for_test()
  local cancel_next = vim.api.nvim_buf_call(cancel_picker.buf, function() return vim.fn.maparg("<Down>", "n", false, true) end)
  local cancel_previous = vim.api.nvim_buf_call(cancel_picker.buf, function() return vim.fn.maparg("<Up>", "n", false, true) end)
  cancel_next.callback()
  cancel_previous.callback()
  cancel_next.callback()
  await(function() return vim.api.nvim_win_get_cursor(review.win)[1] ~= cursor[1]
    and #vim.api.nvim_buf_get_extmarks(review.buf, preview_namespace, 0, -1, {}) == 1 end, "rapid input did not settle on the latest reference")
  local cancel = vim.api.nvim_buf_call(cancel_picker.buf, function() return vim.fn.maparg("q", "n", false, true) end)
  cancel.callback()
  assert(not picker.is_open() and #vim.api.nvim_buf_get_extmarks(review.buf, preview_namespace, 0, -1, {}) == 0, "cancel retained the preview")
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(review.win), cursor), "cancel did not restore the originating cursor")
  assert(#vim.fn.getjumplist()[1] == 0, "cancelled reference previews added jumps")
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
