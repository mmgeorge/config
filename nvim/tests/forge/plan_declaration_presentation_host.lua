vim.loader.enable(false)
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
local bevy_navigation = vim.g.forge_test_bevy == true
print("plan_declaration_presentation_host fixtures: " .. workspace .. " | " .. data)
assert(vim.fn.mkdir(workspace .. "/src", "p") == 1)
assert(vim.fn.mkdir(data, "p") == 1)
local toolchain_path = assert(vim.api.nvim_get_runtime_file("rust/forge/rust-toolchain.toml", false)[1])
vim.fn.writefile(vim.fn.readfile(toolchain_path), workspace .. "/rust-toolchain.toml")
local compact = table.concat({ "pub struct Registry { pub first: u64, pub second: u64, secret: u64 }",
  "#[derive(Debug, Clone)]", "pub struct ArenaPlugin { config: u64 }",
  "impl ArenaPlugin { pub fn new() -> Self; }", "pub fn inspect_plugin(value: ArenaPlugin);",
  "impl Default for ArenaPlugin { fn default() -> Self; }", "#[derive(Facet, Debug, Clone, PartialEq, Eq)]\n#[facet(derive(Error))]\npub enum ConfigError {\n  /// Arena dimensions must be valid.\n  ArenaSize,\n  Radius,\n}", "pub(crate) struct CrateState;", "pub(super) fn configure_parent();", "fn main();",
  "use engine::*;", "#[derive(Resource, Default)]", "pub(crate) struct MovementInput { pub(crate) direction: Vec2 }",
  "pub(crate) fn movement_input(mut movement: ResMut<MovementInput>);",
  "pub(crate) fn observe_input(movement: Res<MovementInput>);", "use crate::input_consumer::RemoteState as SharedState;",
  "pub(crate) fn inspect_remote(value: SharedState);",
  "struct HiddenState;", "pub fn inspect_hidden(value: HiddenState);", "pub fn optional_input(value: Option<MovementInput>);",
  "pub fn engine_handle(value: engine::Engine);", "fn hidden_helper();" }, "\n")
compact = "/// Registry keeps the published handles and exposes the shared declarations used by callers throughout the application.\n" .. compact
if bevy_navigation then compact = "use bevy::prelude::*;\n" .. compact end
vim.fn.writefile({ '{"declaration_line_width":60}' }, workspace .. "/.forge.json")
vim.fn.writefile(vim.split(compact, "\n", { plain = true }), workspace .. "/src/change.rs")
vim.fn.writefile({ "/// Exposes movement declarations.", "mod change;", "/// Consumes movement declarations.", "mod input_consumer;" }, workspace .. "/src/lib.rs")
vim.fn.writefile({ "use engine::*;", "use crate::change::MovementInput;", "/// Uses sampled movement intent.",
  "pub(crate) fn move_player(movement: Res<MovementInput>);", "/// Owns remote review state.", "pub(crate) struct RemoteState;" }, workspace .. "/src/input_consumer.rs")
local engine = vim.fn.tempname()
assert(vim.fn.mkdir(engine .. "/src", "p") == 1)
vim.fn.writefile({ '[package]', 'name = "engine"', 'version = "1.2.0"', 'edition = "2024"', '[features]', 'render = []' }, engine .. "/Cargo.toml")
vim.fn.writefile({ 'pub struct Engine;' }, engine .. "/src/lib.rs")
local manifest = '[package]\nname = "arena"\nversion = "0.1.0"\nedition = "2024"\n\n[dependencies]\nengine = { path = ' .. vim.json.encode((engine:gsub("\\", "/"))) .. ', default-features = false, features = ["render"] }\n'
if bevy_navigation then manifest = manifest .. 'bevy = { version = "=0.19.1", default-features = false }\n' end
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
local errors, validation_notifications = {}, {}
vim.notify = function(message, level)
  if level == vim.log.levels.ERROR then errors[#errors + 1] = tostring(message) end
  if tostring(message):find("review validation warning fixture", 1, true) then
    validation_notifications[#validation_notifications + 1] = tostring(message)
  end
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
  local completed = vim.wait(30000, function() return #errors > 0 or predicate() end, 10)
  if not completed and state.transcript_buf and vim.api.nvim_buf_is_valid(state.transcript_buf) then
    message = message .. "\n" .. table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
  end
  assert(completed, message)
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
  plan.validation_warning = { { path = "lib.rs", message = "review validation warning fixture" } }
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
  assert(document.design.validation and document.design.validation.fingerprint, "submission did not retain reference validation")
  local projection = bytes(plan.working_path)
  local original_request = client.request_for
  local pending_open
  client.request_for = function(session_id, method, params, callback)
    if method == "harness.document" and params.operation == "plan_open" then
      pending_open = { session_id, method, params, callback }
      return
    end
    return original_request(session_id, method, params, callback)
  end
  require("forge.views.plan_review").open(plan)
  assert(pending_open, "opening did not request a native snapshot")
  local loading = state.plan_review
  assert(loading and not loading.owner.ready, "held snapshot attached prematurely")
  assert(vim.bo[loading.buf].filetype == "forge", "pending review used Markdown presentation")
  assert(not vim.bo[loading.buf].modifiable and not vim.bo[loading.buf].modified, "pending review allowed edits")
  assert(text(loading.buf) == "Loading plan review…", "pending review exposed the saved Markdown projection")
  assert(vim.wo[loading.win].winbar:find("PlanReview", 1, true)
    and vim.wo[loading.win].winbar:find("Loading review", 1, true), "pending review omitted its navbar")
  assert(bytes(plan.working_path) == projection, "loading changed the saved plan")
  client.request_for = original_request
  original_request(unpack(pending_open))
  await(function() return state.plan_review and state.plan_review.owner.ready end, "declaration review did not open")
  local review = state.plan_review
  assert(vim.wo[review.win].winbar:find("Awaiting review", 1, true), "attached review retained its loading navbar")
  assert(text(review.buf):find(document.design.document.description, 1, true), "change description is missing")
  assert(text(review.buf):find("Task:\n" .. document.design.document.task, 1, true), "task overview is missing or misplaced")
  assert(text(review.buf):find("  pub second: u64,", 1, true), "compact member did not receive display indentation")
  assert(text(review.buf):find("impl Default for ArenaPlugin {}", 1, true), "full view did not abbreviate trait implementation")
  assert(not text(review.buf):find("fn default", 1, true), "full view exposed trait implementation members")
  assert(not text(review.buf):find("Validation:", 1, true), "review exposed validation evidence")
  assert(#validation_notifications == 0, "opening review emitted validation notifications")
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
  local reference_row = declaration_row("pub fn inspect_plugin")
  local reference_text = vim.api.nvim_buf_get_lines(review.buf, reference_row - 1, reference_row, false)[1]
  vim.api.nvim_set_current_win(review.win)
  vim.api.nvim_win_set_cursor(review.win, { reference_row, reference_text:find("ArenaPlugin", 1, true) - 1 })
  review.command_set.action_by_id.jump_entity.run({})
  await(function()
    local selected = vim.api.nvim_win_get_cursor(review.win)
    return vim.api.nvim_buf_get_lines(review.buf, selected[1] - 1, selected[1], false)[1]:find("pub struct ArenaPlugin", 1, true)
  end, "definition navigation did not jump within the baseline declaration diff")
  local function jump_to_movement(needle)
    local row = declaration_row(needle)
    local line = vim.api.nvim_buf_get_lines(review.buf, row - 1, row, false)[1]
    vim.api.nvim_win_set_cursor(review.win, { row, line:find("MovementInput", 1, true) + 3 })
    vim.api.nvim_feedkeys(".", "xt", false)
    await(function()
      local label, column = cursor_label()
      return label:find("pub(crate) struct MovementInput", 1, true)
        and label:sub(column + 1):match("^MovementInput")
    end, "MovementInput definition navigation failed: " .. needle)
  end
  jump_to_movement("ResMut<MovementInput>")
  jump_to_movement("Res<MovementInput>")
  public_key()
  await(function() return review.public_only == true end, "movement test did not filter")
  jump_to_movement("ResMut<MovementInput>")
  jump_to_movement("Res<MovementInput>")
  public_key()
  await(function() return review.public_only == false end, "movement test did not restore")
  local movement_row = declaration_row("pub(crate) struct MovementInput")
  toggle(movement_row)
  assert(closed(movement_row) == movement_row, "movement declaration did not fold")
  jump_to_movement("ResMut<MovementInput>")
  assert(closed(movement_row) == -1, "definition jump did not open the destination fold")

  local held_jump
  client.request_for = function(session_id, method, params, callback)
    if method == "harness.document" and params.operation == "plan_action" and params.input.action == "jump_entity" then
      held_jump = { session_id, method, params, callback }
      return
    end
    return original_request(session_id, method, params, callback)
  end
  local movement_reference = declaration_row("ResMut<MovementInput>")
  local movement_text = vim.api.nvim_buf_get_lines(review.buf, movement_reference - 1, movement_reference, false)[1]
  vim.api.nvim_win_set_cursor(review.win, { movement_reference, movement_text:find("MovementInput", 1, true) + 3 })
  vim.api.nvim_feedkeys(".", "xt", false)
  assert(held_jump and review.owner.navigation_pending, "delayed navigation was not admitted")
  local sequence = review.owner.view.sequence
  review.owner.sync_focus()
  assert(review.owner.view.sequence == sequence and not review.owner.focus_pending, "comment focus superseded navigation")
  client.request_for = original_request
  original_request(unpack(held_jump))
  await(function() local label = cursor_label() return label:find("pub(crate) struct MovementInput", 1, true) end,
    "delayed navigation did not apply")

  held_jump = nil
  client.request_for = function(session_id, method, params, callback)
    if method == "harness.document" and params.operation == "plan_action" and params.input.action == "jump_entity" then
      held_jump = { session_id, method, params, callback }
      return
    end
    return original_request(session_id, method, params, callback)
  end
  vim.api.nvim_win_set_cursor(review.win, { movement_reference, movement_text:find("MovementInput", 1, true) + 3 })
  vim.api.nvim_feedkeys(".", "xt", false)
  assert(held_jump, "stale navigation was not held")
  vim.api.nvim_win_set_cursor(review.win, { reference_row, 0 })
  local retained_cursor = vim.api.nvim_win_get_cursor(review.win)
  client.request_for = original_request
  original_request(unpack(held_jump))
  await(function() return not review.owner.navigation_pending end, "stale navigation did not settle")
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(review.win), retained_cursor), "late navigation moved the cursor")
  local function jump_to_snapshot(needle, name)
    local row = declaration_row(needle)
    local line = vim.api.nvim_buf_get_lines(review.buf, row - 1, row, false)[1]
    local reference = name == "RemoteState" and "SharedState" or name
    local column = assert(line:find(reference, 1, true), "missing reference: " .. reference) - 1
    vim.api.nvim_win_set_cursor(review.win, { row, column })
    vim.api.nvim_feedkeys(".", "xt", false)
    await(function() return vim.api.nvim_get_current_win() ~= review.win end, "declaration snapshot did not open: " .. name)
    assert(vim.api.nvim_get_current_line():find("struct " .. name, 1, true), "snapshot selected the wrong declaration: " .. name)
    vim.api.nvim_feedkeys("q", "xt", false)
    assert(vim.api.nvim_get_current_win() == review.win, "snapshot did not return to review")
  end
  jump_to_snapshot("pub(crate) fn inspect_remote", "RemoteState")
  public_key()
  await(function() return review.public_only == true end, "hidden-type test did not filter")
  jump_to_snapshot("pub fn inspect_hidden", "HiddenState")
  public_key()
  await(function() return review.public_only == false end, "hidden-type test did not restore")
  local trace_status
  client.request_for(review.session_id, "trace.configure", { enabled = true }, function(result, failure)
    assert(not failure, tostring(failure))
    trace_status = result
  end)
  await(function() return trace_status ~= nil end, "jump logging did not enable")
  assert(trace_status.enabled and type(trace_status.path) == "string", "trace status omitted its session log")
  local engine_row = declaration_row("pub fn engine_handle")
  local engine_text = vim.api.nvim_buf_get_lines(review.buf, engine_row - 1, engine_row, false)[1]
  vim.api.nvim_win_set_cursor(review.win, { engine_row, engine_text:find("::Engine", 1, true) + 4 })
  vim.api.nvim_feedkeys(".", "xt", false)
  await(function() return vim.fs.normalize(vim.api.nvim_buf_get_name(0)) == vim.fs.normalize(engine .. "/src/lib.rs") end,
    "dependency definition did not open its source")
  assert(vim.api.nvim_get_current_line() == "pub struct Engine;" and vim.api.nvim_win_get_cursor(0)[2] == 11,
    "dependency definition selected the wrong source position")
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-o>", true, false, true), "xt", false)
  await(function() return vim.api.nvim_get_current_buf() == review.buf and review.owner.attached() end,
    "returning from dependency source lost the review attachment")
  assert(vim.bo[review.buf].filetype == "forge" and vim.wo[review.win].winbar:find("PlanReview", 1, true),
    "returning from dependency source lost review presentation")
  if bevy_navigation then
    for _, public_only in ipairs({ false, true }) do
      if public_only then
        public_key()
        await(function() return review.public_only == true end, "Bevy test did not filter visibility")
      end
      for _, name in ipairs({ "Res", "ResMut" }) do
        local row = declaration_row(name .. "<MovementInput>")
        local line = vim.api.nvim_buf_get_lines(review.buf, row - 1, row, false)[1]
        vim.api.nvim_win_set_cursor(review.win, { row, line:find(name .. "<", 1, true) - 1 })
        vim.api.nvim_feedkeys(".", "xt", false)
        await(function()
          return vim.api.nvim_buf_get_name(0):gsub("\\", "/"):find("bevy_ecs-0.19.1/src/change_detection/params.rs", 1, true)
            and vim.api.nvim_get_current_line():find("pub struct " .. name .. "<", 1, true)
        end, "Bevy prelude navigation did not reach " .. name)
        local column = vim.api.nvim_win_get_cursor(0)[2]
        assert(vim.api.nvim_get_current_line():sub(column + 1):find(name .. "<", 1, true) == 1,
          "Bevy definition selected the wrong column")
        vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-o>", true, false, true), "xt", false)
        await(function() return vim.api.nvim_get_current_buf() == review.buf and review.owner.attached() end,
          "returning from Bevy source lost the review attachment")
        assert(vim.wo[review.win].winbar:find("PlanReview", 1, true), "returning from Bevy lost the navbar")
        jump_to_movement(name .. "<MovementInput>")
      end
    end
    public_key()
    await(function() return review.public_only == false end, "Bevy test did not restore visibility")
  end
  local phases, resolve_count, total_count = {}, 0, 0
  for _, line in ipairs(vim.fn.readfile(trace_status.path)) do
    local record = vim.json.decode(line)
    if record.event == "declaration.jump" and record.payload.status == "completed" then
      local payload = record.payload
      assert(type(payload.duration_ms) == "number" and payload.duration_ms >= 0, "jump phase omitted elapsed time")
      assert(payload.document and payload.sequence and payload.view, "jump phase lost its input identity")
      phases[payload.phase] = true
      if payload.phase == "resolve" then
        resolve_count = resolve_count + 1
        assert(type(payload.member_calls) == "number" and type(payload.glob_branches) == "number"
          and type(payload.files) == "number" and type(payload.loaded_files) == "number", "jump omitted resolution work counts")
        assert(type(payload.resolution) == "table", "jump omitted its resolution outcome")
      elseif payload.phase == "index_file" and payload.available then
        assert(type(payload.path) == "string" and type(payload.bytes) == "number"
          and type(payload.cached) == "boolean", "file indexing omitted its source identity")
        for _, field in ipairs({ "read_ms", "parse_ms", "extract_ms", "load_ms" }) do
          assert(type(payload[field]) == "number" and payload[field] >= 0, "file indexing omitted " .. field)
        end
      elseif payload.phase == "total" then total_count = total_count + 1 end
    end
  end
  assert(phases.preflight and phases.cached_dependency and phases.index_file and phases.action
    and total_count > 0 and resolve_count >= 2, "dependency jump omitted cached-source and resolution timings")
  assert(not phases.cargo_metadata, "cached dependency navigation invoked Cargo metadata")
  print("declaration.jump trace verified: " .. trace_status.path)
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
  local origin_tab = vim.api.nvim_get_current_tabpage()
  client.request_for = function(session_id, method, params, callback)
    if method == "harness.document" and params.operation == "plan_open" then
      vim.schedule(function() callback(nil, "Snapshot unavailable for attachment test") end)
      return
    end
    return original_request(session_id, method, params, callback)
  end
  require("forge.views.plan_review").open(plan)
  local failed_tab = state.plan_review.tab
  assert(vim.wait(10000, function() return state.plan_review == nil and #errors == 1 end, 10), "failed attachment did not settle")
  assert(errors[1]:find("Snapshot unavailable for attachment test", 1, true), "attachment failure was not reported")
  assert(not vim.api.nvim_tabpage_is_valid(failed_tab) and vim.api.nvim_get_current_tabpage() == origin_tab,
    "failed attachment left an orphan review tab")
  assert(bytes(plan.working_path) == projection, "failed attachment changed the saved plan")
  errors = {}
  client.request_for = original_request
  require("forge.views.plan_review").open(plan)
  await(function() return state.plan_review and state.plan_review.owner.ready end, "review did not reopen")
  assert(text(state.plan_review.buf):find("Review the second member", 1, true), "reopening lost the comment")
  assert(bytes(artifact_path) == artifact and bytes(plan.working_path) == projection, "comment reopening rewrote the design")
  local pending_approval
  client.request_for = function(session_id, method, params, callback)
    if method == "plan.acceptance.begin" then pending_approval = { session_id, method, params, callback } return end
    return original_request(session_id, method, params, callback)
  end
  state.plan_review.command_set.action_by_id.accept.run({})
  await(function() return pending_approval end, "approval was not dispatched")
  assert(state.plan_review.owner.attached() and state.plan_review.owner.submission_pending,
    "approval released its document before acknowledgement")
  assert(not vim.bo[state.plan_review.buf].modifiable and not state.plan_review.owner.close(),
    "approval allowed editing or premature document closure")
  client.request_for = original_request
  original_request(unpack(pending_approval))
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
