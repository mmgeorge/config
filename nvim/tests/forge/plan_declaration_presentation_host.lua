vim.loader.enable(false)
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
assert(vim.fn.mkdir(workspace .. "/src", "p") == 1)
assert(vim.fn.mkdir(data, "p") == 1)
local compact = table.concat({ "pub struct Registry { pub first: u64, pub second: u64, secret: u64 }",
  "#[derive(Debug, Clone)]", "pub struct ArenaPlugin { config: u64 }",
  "impl ArenaPlugin { pub fn new() -> Self; }",
  "impl Default for ArenaPlugin { fn default() -> Self; }", "#[derive(Facet, Debug, Clone, PartialEq, Eq)]\n#[facet(derive(Error))]\npub enum ConfigError {\n  /// Arena dimensions must be valid.\n  ArenaSize,\n  Radius,\n}", "pub(crate) struct CrateState;", "pub(super) fn configure_parent();", "fn main();", "fn hidden_helper();" }, "\n")
compact = "/// Registry keeps the published handles and exposes the shared declarations used by callers throughout the application.\n" .. compact
vim.fn.writefile({ '{"declaration_line_width":60}' }, workspace .. "/.forge.json")
vim.fn.writefile(vim.split(compact, "\n", { plain = true }), workspace .. "/src/change.rs")
local manifest = '[package]\nname = "arena"\nversion = "0.1.0"\nedition = "2024"\n\n[dependencies]\nengine = { version = "1.2", default-features = false, features = ["render"] }\n'
vim.fn.writefile(vim.split(manifest:gsub("\n$", ""), "\n", { plain = true }), workspace .. "/Cargo.toml")
local configuration = {
  ["package.json"] = '{"name":"arena","version":"0.1.0"}\n',
  ["tsconfig.json"] = '{// Type checks\n"version":"0.1.0","compilerOptions":{"strict":true,},}\n',
  ["ci.yaml"] = 'version: "0.1.0"\nscript: |\n  echo build\n',
  ["App.csproj"] = '<Project Version="0.1.0"><PropertyGroup><TargetFramework>net9.0</TargetFramework></PropertyGroup></Project>\n',
}
for path, contents in pairs(configuration) do
  vim.fn.writefile(vim.split(contents:gsub("\n$", ""), "\n", { plain = true }), workspace .. "/" .. path)
end
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
  assert(document.design.baseline["src/change.rs"].text:find("  pub second: u64,", 1, true), "submission did not store formatted declarations")
  assert(bytes(workspace .. "/src/change.rs") == compact .. "\n", "submission changed project source")
  assert(document.design.baseline["Cargo.toml"].text == manifest, "manifest baseline lost values")
  assert(document.design.proposed["Cargo.toml"]:find('version = "0.2.0"', 1, true), "manifest proposal did not change")
  assert(bytes(workspace .. "/Cargo.toml") == manifest, "planning modified the project manifest")
  for path, contents in pairs(configuration) do
    assert(document.design.baseline[path].text == contents, "configuration baseline lost values: " .. path)
    assert(document.design.proposed[path]:find('0.2.0', 1, true), "configuration proposal did not change: " .. path)
    assert(bytes(workspace .. "/" .. path) == contents, "planning modified configuration: " .. path)
  end
  assert(document.design.line_width == 60, "repository formatting width was not retained")
  for _, line in ipairs(vim.split(document.design.baseline["src/change.rs"].text, "\n", { plain = true })) do
    if line:match("^/// ") then assert(#line <= 60, "submitted prose did not wrap") end
  end
  assert(document.design.proposed["src/change.rs"] == "pub fn reviewed_change();\n")
  assert(document.design.document.description == "Revise the registry interface while preserving its ownership boundary.")
  assert(document.design.document.task == "Revise the registry interface.")
  local projection = bytes(plan.working_path)
  require("forge.views.plan_review").open(plan)
  await(function() return state.plan_review and state.plan_review.owner.ready end, "declaration review did not open")
  local review = state.plan_review
  assert(text(review.buf):find(document.design.document.description, 1, true), "change description is missing")
  assert(text(review.buf):find("Task:\n" .. document.design.document.task, 1, true), "task overview is missing or misplaced")
  assert(text(review.buf):find("  pub second: u64,", 1, true), "compact member did not receive display indentation")
  assert(text(review.buf):find("impl Default for ArenaPlugin {}", 1, true), "full view did not abbreviate trait implementation")
  assert(not text(review.buf):find("fn default", 1, true), "full view exposed trait implementation members")
  assert(text(review.buf):find("pub fn new", 1, true), "full view hid inherent methods")
  for path in pairs(configuration) do
    assert(text(review.buf):find("Modified " .. path, 1, true), "configuration diff is missing: " .. path)
  end
  assert(bytes(artifact_path) == artifact and bytes(plan.working_path) == projection, "opening rewrote the plan")
  local file_row, hunk_row, task_row, description_row, changes_row
  for row, line in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
    if line == "Description:" then description_row = row end
    if line == "Task:" then task_row = row end
    if line == "Changes:" then changes_row = row end
    if line:find("Modified src/change.rs", 1, true) then file_row = row end
    if file_row and not hunk_row and line:find("@@", 1, true) then hunk_row = row end
  end
  assert(file_row and hunk_row and task_row and description_row and changes_row, "section or diff headers are missing")
  local function closed(row)
    return vim.api.nvim_win_call(review.win, function() return vim.fn.foldclosed(row) end)
  end
  local function toggle(row)
    vim.api.nvim_win_set_cursor(review.win, { row, 0 })
    review.command_set.action_by_id.toggle.run({})
  end
  assert(closed(description_row) == -1 and closed(changes_row) == -1, "plan sections did not start expanded")
  toggle(task_row)
  assert(closed(task_row + 1) == task_row and closed(description_row) == -1, "task fold hid the description")
  toggle(task_row)
  toggle(description_row)
  assert(closed(description_row + 1) == description_row and closed(file_row) == -1,
    "description fold hid changes or left its body visible")
  toggle(description_row)
  toggle(changes_row)
  assert(closed(file_row) == changes_row and closed(hunk_row) == changes_row and closed(description_row) == -1,
    "changes fold left descendants visible or hid the description")
  toggle(changes_row)
  assert(closed(file_row) == -1 and closed(hunk_row) == -1, "section reopening changed child folds")
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
  local function public_key()
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<S-Tab>", true, false, true), "xt", false)
  end
  local function declaration_row(label)
    for row, line in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
      if line:find(label, 1, true) then return row end
    end
    error("missing declaration: " .. label)
  end
  local function cursor_label()
    local cursor = vim.api.nvim_win_get_cursor(review.win)
    return vim.api.nvim_buf_get_lines(review.buf, cursor[1] - 1, cursor[1], false)[1], cursor[2]
  end
  local enum_row = declaration_row("pub enum ConfigError")
  local attribute_row = declaration_row("#[derive(Facet,")
  assert(closed(enum_row) == enum_row, "Rust enum did not start collapsed")
  toggle(attribute_row)
  assert(closed(enum_row) == -1, "Tab did not open the default enum fold")
  public_key()
  await(function() return review.public_only == true end, "enum default test did not filter")
  assert(closed(declaration_row("pub enum ConfigError")) == -1, "filtering reset an explicitly opened enum")
  public_key()
  await(function() return review.public_only == false end, "enum default test did not restore")
  enum_row = declaration_row("pub enum ConfigError")
  toggle(declaration_row("#[derive(Facet,"))
  assert(closed(enum_row) == enum_row and closed(attribute_row) == -1,
    "declaration fold hid attributes or left its body open")
  local summary = vim.api.nvim_win_call(review.win, function() return vim.fn.foldtextresult(enum_row) end)
  assert(summary:find("pub enum ConfigError {...}", 1, true), "declaration fold summary is wrong: " .. summary)
  public_key()
  await(function() return review.public_only == true end, "fold test did not filter private declarations")
  enum_row = declaration_row("pub enum ConfigError")
  assert(closed(enum_row) == enum_row, "public filtering lost declaration fold intent")
  public_key()
  await(function() return review.public_only == false end, "fold test did not restore private declarations")
  enum_row = declaration_row("pub enum ConfigError")
  assert(closed(enum_row) == enum_row, "full visibility lost declaration fold intent")
  toggle(enum_row)
  assert(closed(declaration_row("ArenaSize,")) == -1, "opening declaration fold did not restore variants")
  vim.api.nvim_win_set_cursor(review.win, { declaration_row("pub fn new"), 18 })
  public_key()
  await(function() return review.public_only == true end, "cursor test did not hide private declarations")
  local label, column = cursor_label()
  assert(label:find("pub fn new", 1, true) and column == 18, "hiding private rows displaced a public declaration cursor")
  public_key()
  await(function() return review.public_only == false end, "cursor test did not expand private declarations")
  label, column = cursor_label()
  assert(label:find("pub fn new", 1, true) and column == 18, "expanding private rows displaced a public declaration cursor")
  vim.api.nvim_win_set_cursor(review.win, { declaration_row("fn hidden_helper"), 18 })
  public_key()
  await(function() return review.public_only == true end, "cursor test did not hide private function")
  label = cursor_label()
  assert(label:find("pub fn reviewed_change", 1, true), "hidden private function did not select the nearest retained line")
  public_key()
  await(function() return review.public_only == false end, "cursor test did not restore complete view")
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
  local saved_second_row
  for row, line in ipairs(vim.split(document.design.baseline["src/change.rs"].text, "\n", { plain = true })) do
    if line:find("pub second:", 1, true) then saved_second_row = row break end
  end
  assert(annotation.anchor.start.side == "baseline" and annotation.anchor.start.line == saved_second_row,
    "comment did not address the formatted saved declaration")
  assert(annotation.anchor.start.column == 2, "formatted member lost its saved byte column")
  local private_row
  for row, line in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
    if line:find("secret:", 1, true) then private_row = row - 1 break end
  end
  assert(private_row, "private field is missing from the complete view")
  vim.api.nvim_win_set_cursor(review.win, { private_row + 1, 0 })
  local private_comment
  review.owner.action("comment", function(value, error_message)
    assert(not error_message, error_message)
    private_comment = value
  end)
  await(function() return private_comment end, "private comment did not attach")
  local _, private_comment_row = review.owner.replica.sequence:position(private_comment.block)
  vim.api.nvim_win_set_cursor(review.win, { private_comment_row + private_comment.row + 1, 0 })
  review.owner.sync_editability()
  vim.api.nvim_buf_set_text(review.buf, private_comment_row + private_comment.row, 0,
    private_comment_row + private_comment.row, 0, { "Review the private field" })
  vim.cmd("write")
  await(function() return not vim.bo[review.buf].modified end, "private comment did not save")
  local comments = bytes(annotation_path)
  public_key()
  await(function() return review.public_only == true end, "Shift+Tab did not enable public visibility")
  assert(text(review.buf):find("Modified Cargo.toml", 1, true), "public filter hid manifest changes")
  assert(text(review.buf):find('version = "0.2.0"', 1, true), "public filter hid TOML values")
  for path in pairs(configuration) do
    assert(text(review.buf):find("Modified " .. path, 1, true), "public filter hid configuration: " .. path)
  end
  assert(text(review.buf):find('<Project Version="0.2.0">', 1, true), "public filter hid XML values")
  assert(text(review.buf):find('"version":"0.2.0"', 1, true), "public filter hid JSON values")
  assert(text(review.buf):find("fn main();", 1, true), "public filter hid the binary entry point")
  assert(text(review.buf):find("pub(crate) struct CrateState;", 1, true), "public filter hid crate visibility")
  assert(text(review.buf):find("pub(super) fn configure_parent();", 1, true), "public filter hid parent visibility")
  assert(not text(review.buf):find("fn hidden_helper();", 1, true), "public filter exposed a private helper")
  assert(not text(review.buf):find("secret:", 1, true), "public filter exposed a private field")
  assert(text(review.buf):find(document.design.document.description, 1, true), "public filter hid change description")
  assert(text(review.buf):find(document.design.document.task, 1, true), "public filter hid task overview")
  assert(text(review.buf):find("#[derive(Debug, Clone)]\npub struct ArenaPlugin {}", 1, true),
    "empty public struct did not compact with its attributes")
  assert(text(review.buf):find("impl Default for ArenaPlugin {}", 1, true), "public view did not abbreviate trait implementation")
  assert(not text(review.buf):find("fn default", 1, true), "public view exposed trait implementation members")
  assert(text(review.buf):find("pub fn new", 1, true), "public view hid inherent methods")
  assert(not text(review.buf):find("config:", 1, true), "empty public struct exposed private state")

  assert(not text(review.buf):find("Review the private field", 1, true), "public filter exposed a private comment")
  assert(text(review.buf):find("pub second:", 1, true) and text(review.buf):find("Review the second member", 1, true),
    "public filter lost a public declaration or its comment")
  local final_row
  for row, line in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
    if line:find("pub fn reviewed_change", 1, true) then final_row = row - 1 break end
  end
  assert(final_row, "public callable is missing")
  vim.api.nvim_win_set_cursor(review.win, { final_row + 1, 0 })
  local final_comment
  review.owner.action("comment", function(value, error_message)
    assert(not error_message, error_message)
    final_comment = value
  end)
  await(function() return final_comment end, "public-only comment did not attach")
  local _, final_comment_row = review.owner.replica.sequence:position(final_comment.block)
  vim.api.nvim_win_set_cursor(review.win, { final_comment_row + final_comment.row + 1, 0 })
  review.owner.sync_editability()
  vim.api.nvim_buf_set_text(review.buf, final_comment_row + final_comment.row, 0,
    final_comment_row + final_comment.row, 0, { "Review the final callable" })
  vim.cmd("write")
  await(function() return not vim.bo[review.buf].modified end, "public-only comment did not save")
  comments = bytes(annotation_path)
  toggle(file_row)
  assert(closed(hunk_row) == file_row, "public filtering broke file folding")
  assert(closed(final_comment_row + 1) == file_row, "file fold exposed its final comment")
  local label = vim.api.nvim_win_call(review.win, function() return vim.inspect(vim.fn.foldtextresult(file_row)) end)
  assert(label:find("Modified src/change.rs", 1, true) and not label:find("@@", 1, true),
    "public file fold label contains hunk contents")

  toggle(file_row)
  public_key()
  await(function() return review.public_only == false end, "Shift+Tab did not restore complete visibility")
  assert(text(review.buf):find("secret:", 1, true) and text(review.buf):find("Review the private field", 1, true),
    "public filtering discarded private declarations or comments")
  assert(bytes(annotation_path) == comments and bytes(artifact_path) == artifact
    and bytes(plan.working_path) == projection, "visibility toggle changed persisted review data")
  vim.api.nvim_win_set_cursor(review.win, { description_row + 1, 0 })
  local description_comment
  review.owner.action("comment", function(value, error_message)
    assert(not error_message, error_message)
    description_comment = value
  end)
  await(function() return description_comment end, "description comment did not attach")
  local _, description_comment_row = review.owner.replica.sequence:position(description_comment.block)
  vim.api.nvim_win_set_cursor(review.win, { description_comment_row + description_comment.row + 1, 0 })
  review.owner.sync_editability()
  vim.api.nvim_buf_set_text(review.buf, description_comment_row + description_comment.row, 0,
    description_comment_row + description_comment.row, 0, { "Clarify the behavior in the description" })
  vim.cmd("write")
  await(function() return not vim.bo[review.buf].modified end, "description comment did not save")
  local description_anchor
  for _, saved in ipairs(vim.json.decode(bytes(annotation_path)).annotation) do
    if saved.source.body == "Clarify the behavior in the description" then description_anchor = saved.anchor end
  end
  assert(description_anchor and description_anchor.start.target_type == "section" and description_anchor.start.section == "overview",
    "description comment did not retain its metadata target")
  toggle(description_row)
  assert(closed(description_comment_row + 1) == description_row, "description fold left its comment visible")
  public_key()
  await(function() return review.public_only == true end, "description visibility test did not toggle")
  assert(closed(description_row) == description_row, "visibility toggle reopened the description")
  assert(text(review.buf):find("Clarify the behavior in the description", 1, true), "public filter hid description comment")
  public_key()
  await(function() return review.public_only == false end, "description visibility test did not restore")
  toggle(description_row)
  assert(closed(description_row) == -1, "description section did not reopen")
  vim.api.nvim_win_set_cursor(review.win, { task_row + 1, 0 })
  local task_comment
  review.owner.action("comment", function(value, error_message)
    assert(not error_message, error_message)
    task_comment = value
  end)
  await(function() return task_comment end, "task comment did not attach")
  local _, task_comment_row = review.owner.replica.sequence:position(task_comment.block)
  vim.api.nvim_win_set_cursor(review.win, { task_comment_row + task_comment.row + 1, 0 })
  review.owner.sync_editability()
  vim.api.nvim_buf_set_text(review.buf, task_comment_row + task_comment.row, 0,
    task_comment_row + task_comment.row, 0, { "Clarify the requested outcome" })
  vim.cmd("write")
  await(function() return not vim.bo[review.buf].modified end, "task comment did not save")
  local task_anchor
  for _, saved in ipairs(vim.json.decode(bytes(annotation_path)).annotation) do
    if saved.source.body == "Clarify the requested outcome" then task_anchor = saved.anchor end
  end
  assert(task_anchor and task_anchor.start.target_type == "section" and task_anchor.start.section == "task",
    "task comment did not retain its separate metadata target")
  toggle(task_row)
  assert(closed(task_comment_row + 1) == task_row, "task fold left its comment visible")
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
