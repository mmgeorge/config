vim.loader.enable(false)
if not vim.g.forge_plan_calls_grid then
  local test_path = debug.getinfo(1, "S").source:sub(2)
  local child = vim.fn.jobstart({ vim.v.progpath, "--embed", "--headless", "-u", "NONE", "-i", "NONE" }, { rpc = true })
  assert(child > 0)
  local success, failure = xpcall(function()
    vim.rpcrequest(child, "nvim_ui_attach", 160, 48, { rgb = true })
    -- Initialize highlight tracing before rendering content to keep cell attributes valid.
    vim.rpcrequest(child, "nvim_exec_lua", "vim.api.nvim__inspect_cell(1, 0, 0); vim.cmd('redraw!')", {})
    vim.rpcrequest(child, "nvim_exec_lua", "vim.g.forge_plan_calls_grid = true; vim.g.forge_test_plan_behavior = " .. tostring(vim.g.forge_test_plan_behavior == true) .. "; vim.g.forge_test_plan_flows = " .. tostring(vim.g.forge_test_plan_flows == true) .. "; vim.g.forge_test_plan_tests = " .. tostring(vim.g.forge_test_plan_tests == true) .. "; vim.g.forge_plan_definition_only = " .. tostring(vim.g.forge_plan_definition_only == true) .. "; vim.opt.runtimepath:prepend(vim.fn.getcwd() .. '/nvim'); dofile(...)", { test_path })
  end, debug.traceback)
  vim.fn.jobstop(child)
  if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
  print(vim.g.forge_test_plan_flows and "plan_review_flows_host: passed at 160x48" or vim.g.forge_test_plan_tests and "plan_review_tests_host: passed at 160x48" or vim.g.forge_test_plan_behavior and "plan_review_behavior_host: passed at 160x48" or vim.g.forge_plan_definition_only and "plan_review_definition_host: passed at 160x48" or "plan_review_calls_host: passed at 160x48")
  vim.cmd("qa!")
  return
end
local definition_only = vim.g.forge_plan_definition_only == true
local root = vim.fn.getcwd()
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
assert(vim.fn.mkdir(workspace .. "/src", "p") == 1 and vim.fn.mkdir(data, "p") == 1)
vim.fn.writefile({vim.json.encode({checks={version=2,definitions={}}})},workspace .. "/.forge.json")
local source = { "pub fn leaf() {}", "pub fn run() { leaf(); leaf(); }", "pub struct Client { pub count: usize }", "pub fn update(client: &mut Client) { leaf(); client.count += 1; let _count = client.count; }", "/// Describes the following declaration.", "pub struct Following;" }
if definition_only then
  source[3] = "pub struct Client { pub count: usize, secret: usize }"
  source[4] = "pub fn update(client: &mut Client) { leaf(); client.count += 1; let _count = client.count; client.secret += 1; }"
end
if vim.g.forge_test_plan_flows then
  vim.list_extend(source, {
    "pub fn restart_round() {}",
    "pub fn reset_combat() {}",
    "pub struct WaveDirector;",
    "pub struct Weapon;",
  })
end
if vim.g.forge_test_plan_tests then
  vim.list_extend(source, {
    "/// Exercises the old test-only helpers.",
    "#[cfg(test)]",
    "mod tests {",
    "  fn old_helper() {}",
    "  #[test]",
    "  fn old_test() { old_helper(); }",
    "}",
  })
end
if vim.g.forge_test_plan_behavior then
  vim.list_extend(source, {
    "pub fn movement_direction(keys: u32) -> u32 { keys }",
    "pub struct PlayerAbilities { pub cooldown: u32 }",
    "impl PlayerAbilities { pub fn start_dash(&mut self, direction: u32) {} }",
    "pub struct Other;",
    "impl Other { pub fn start_dash(&mut self, direction: u32) {} }",
  })
end
vim.fn.writefile(source, workspace .. "/src/change.rs")
vim.fn.writefile({ "[package]", 'name = "call_navigation"', 'version = "0.1.0"', 'edition = "2021"', "[lib]", 'path = "src/change.rs"' }, workspace .. "/Cargo.toml")
vim.fn.writefile({ "invalid Rust {" }, workspace .. "/unrelated.rs")
local initialized = vim.system({ "git", "init", "--quiet", workspace }, { text = true }):wait(10000)
assert(initialized.code == 0, initialized.stderr)
local executable = data .. "/forge" .. (vim.fn.has("win32") == 1 and ".exe" or "")
assert(vim.uv.fs_copyfile(vim.g.forge_test_executable or require("forge.builder").binary_path(), executable))
local behavior_generator = (vim.g.forge_test_plan_behavior or vim.g.forge_test_plan_tests or vim.g.forge_test_plan_flows) and (vim.g.forge_test_fixture_generator
  or require("forge.builder").artifact_root() .. "/build/release/examples/plan_navigation_fixture" .. (vim.fn.has("win32") == 1 and ".exe" or ""))
local original_stdpath, original_notify = vim.fn.stdpath, vim.notify
vim.fn.stdpath = function(kind) return kind == "data" and data or original_stdpath(kind) end
local errors, warnings = {}, {}
vim.notify = function(message, level)
  if level == vim.log.levels.ERROR then errors[#errors + 1] = tostring(message) end
  if level == vim.log.levels.WARN then warnings[#warnings + 1] = tostring(message) end
end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = executable }) end)
  return function() end
end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
require("forge.views.plan_review.references").show_caller = true
local definition = require("forge.views.plan_review.workspace_definition")
definition.request = function(query, current, receive)
  local name = source[query.line]:sub(query.column + 1):match("^[%w_]+")
  local line = assert(({ count = 3, secret = 3, leaf = 1 })[name], "unexpected fixture definition: " .. tostring(name))
  local cancelled = false
  vim.schedule(function()
    if not cancelled and current() then receive({ path = query.path, line = line, column = assert(source[line]:find(name, 1, true)) - 1 }) end
  end)
  return function() cancelled = true end
end
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
  if vim.g.forge_test_plan_behavior or vim.g.forge_test_plan_tests or vim.g.forge_test_plan_flows then
    local seeded = vim.system({ behavior_generator, data .. "/forge/harness", workspace, state.session.id, state.active_plan.id, vim.g.forge_test_plan_tests and "tests" or vim.g.forge_test_plan_flows and "flows" or "behavior" }, { text = true }):wait(30000)
    assert(seeded.code == 0, (seeded.stderr or "") .. (seeded.stdout or ""))
    state.active_plan = vim.json.decode(seeded.stdout)
  end
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
      assert(#callable.call == (definition_only and 4 or 3) and callable.call[1].name == "leaf")
      properties = vim.tbl_filter(function(occurrence) return occurrence.kind == "property" end, callable.call)
    end
  end
  assert(properties and #properties == (definition_only and 3 or 2) and properties[1].kind == "property" and properties[2].kind == "property")
  assert(properties[1].name == "Client::count" and properties[2].name == "Client::count")
  local original_columns = vim.o.columns
  vim.o.columns = 48
  require("forge.views.plan_review").open(state.active_plan)
  await(function() return state.plan_review and state.plan_review.owner.ready end, "PlanReview did not open")
  local review = state.plan_review
  local section_rows = {}
  for index, row in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
    if row == " Checks " or row == " Verification " then section_rows[vim.trim(row)] = index end
    assert(row ~= " Planned checks " and row ~= " Configured checks ", "obsolete check section title")
  end
  assert(section_rows.Checks and section_rows.Verification and section_rows.Checks < section_rows.Verification,
    "Checks must appear before Verification")
  if vim.g.forge_test_plan_flows then
    vim.o.columns = original_columns
    local function find_flow(token)
      for index, text in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
        if text:find("→", 1, true) and text:find(token, 1, true) then return index, text end
      end
      error("flow token missing: " .. token)
    end
    assert(not table.concat(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false), "\n"):find("```text", 1, true), "flow retained a fenced text block")
    for _, expected in ipairs({ { "restart_round", "@function", "pub fn restart_round" }, { "reset_combat", "@function", "pub fn reset_combat" },
      { "WaveDirector", "@type", "pub struct WaveDirector" }, { "Weapon", "@type", "pub struct Weapon" } }) do
      local row, text = find_flow(expected[1])
      local source_column = assert(text:find(expected[1], 1, true)) - 1
      vim.api.nvim_set_hl(0, expected[2], { fg = 0x22ddaa })
      vim.api.nvim_win_set_cursor(review.win, { row, source_column })
      vim.cmd("normal! zz")
      vim.cmd("redraw!")
      local position = vim.fn.screenpos(review.win, row, source_column + 1)
      local cell = vim.api.nvim__inspect_cell(1, position.row - 1, position.col - 1)
      assert(cell[2].foreground == 0x22ddaa, "flow token did not use declaration highlighting: " .. expected[1] .. " " .. vim.inspect(cell))
      for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(review.buf, -1, { row - 1, 0 }, { row - 1, -1 }, { details = true })) do
        assert(mark[4].hl_group ~= "RenderMarkdownCode", "flow retained the code block background")
      end
      review.command_set.action_by_id.jump_entity.run({})
      await(function() return vim.api.nvim_get_current_buf() == review.buf and vim.api.nvim_get_current_line():find(expected[3], 1, true) end,
        "flow definition did not resolve: " .. expected[1])
      vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-o>", true, false, true), "xt", false)
      assert(vim.api.nvim_get_current_line() == text, "flow jump-back lost its exact diagram row")
    end
    local _, literal = find_flow("*literal*")
    assert(literal:find("*literal*", 1, true), "native flow labels lost literal punctuation")
    review.command_set.action_by_id.close.run({})
    require("forge.views.harness.controller").command_set().action_by_id.close.run({})
    return
  end
  if vim.g.forge_test_plan_tests then
    assert(document.design.proposed["src/change.rs"]:find("mod added_tests", 1, true), "test declarations were removed from saved data")
    for _, expected_public in ipairs({ true, false }) do
      if review.public_only ~= expected_public then
        review.command_set.action_by_id.toggle_public.run({})
        await(function() return review.public_only == expected_public end, "review visibility did not change")
      end
      local visible = table.concat(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false), "\n")
      assert(not visible:find("mod added_tests", 1, true) and not visible:find("old_helper", 1, true)
        and not visible:find("added_case", 1, true), "Rust test declarations leaked into Changes")
    end
    local function locate(predicate)
      for index, text in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
        local source = review.owner.replica.decoration_location(index - 1)
        if predicate(source and source.text or text) then return index, text end
      end
    end
    local function check_test_heading()
      local row, text = locate(function(value) return value:find(" Tests · ", 1, true) == 1 end)
      assert(row, "test heading is missing")
      vim.api.nvim_win_set_cursor(review.win, { row, 0 })
      vim.cmd("normal! zz")
      vim.cmd("redraw!")
      vim.api.nvim__redraw({ valid = false, flush = true })
      local function cell(column)
        local screen = vim.fn.screenpos(review.win, row, column)
        local inspected = vim.api.nvim__inspect_cell(1, screen.row - 1, screen.col - 1)
        return inspected[2]
      end
      assert(cell(2).background == 0xffffff, "Tests label lost its white background")
      assert(cell(#" Tests " + 1).background ~= 0xffffff, "separator retained the label background")
      for kind, color in pairs({ new = 0x50fa7b, modified = 0xffb86c, removed = 0xff5555, reused = 0x888888 }) do
        local column = text:find("%d+ " .. kind)
        if column then
          local highlighted = cell(column)
          assert(highlighted.foreground == color and highlighted.background ~= 0xffffff,
            "test count has incorrect foreground or background: " .. kind .. " " .. vim.inspect(highlighted))
        end
      end
    end
    vim.api.nvim_set_hl(0, "RenderMarkdownH1", { fg = 0, bg = 0xffffff })
    vim.api.nvim_set_hl(0, "ForgeAddRange", { fg = 0x50fa7b })
    vim.api.nvim_set_hl(0, "ForgeModifyRange", { fg = 0xffb86c })
    vim.api.nvim_set_hl(0, "ForgeDeleteRange", { fg = 0xff5555 })
    vim.api.nvim_set_hl(0, "Comment", { fg = 0x888888 })
    for _, width in ipairs({ 80, 120, 160 }) do
      vim.o.columns = width
      check_test_heading()
    end
    local function file_row(path)
      return assert(locate(function(text) return text:match("^%a+%s+" .. vim.pesc(path) .. "$") end), "test file heading missing: " .. path)
    end
    for _, text in ipairs({ "new_case", "changed_case", "removed_case", "reused_case", "new_file_case", "reused_file_case" }) do
      assert(locate(function(row) return row == text end), "test files must expand by default: " .. text)
    end
    local _, modified_header = locate(function(text) return text:match("^Modified%s+src/change%.rs$") end)
    local _, new_header = locate(function(text) return text:match("^New%s+tests/new%.rs$") end)
    assert(modified_header and new_header, "shared diff file status labels are missing")
    for _, expected in ipairs({ { "new_case", 0x002800, "+" }, { "changed_case", 0x2a1f00, "~" }, { "removed_case", 0x200000, "-" } }) do
      local row = assert(locate(function(text) return text == expected[1] end))
      for _, width in ipairs({ 80, 120, 160 }) do
        vim.o.columns = width
        vim.api.nvim_win_set_cursor(review.win, { row, 0 })
        vim.cmd("normal! zz")
        vim.cmd("redraw!")
        local screen = vim.fn.screenpos(review.win, row, 1)
        local edge = vim.api.nvim__inspect_cell(1, screen.row - 1, width - 1)
        assert(edge[2].background == expected[2], "test diff background stopped before the window edge: " .. expected[1])
        local cells = {}
        for column = 1, width do cells[#cells + 1] = vim.fn.screenstring(screen.row, column) end
        local marker_column = assert(table.concat(cells):find(expected[3] .. " " .. expected[1], 1, true), table.concat(cells))
        local marker = vim.api.nvim__inspect_cell(1, screen.row - 1, marker_column - 1)
        assert(marker[1] == expected[3], "test row lost its shared diff gutter: " .. vim.inspect(marker))
        if expected[3] == "~" then
          assert(marker[2].foreground == 0xffb86c, "modified gutter is not orange")
          if width == 80 then
            local description = vim.api.nvim_buf_get_lines(review.buf, row, row + 1, false)[1]
            local ending = vim.fn.screenpos(review.win, row + 1, #description + 1)
            local beginning = vim.fn.screenpos(review.win, row + 1, 3)
            assert(ending.row > beginning.row, "long test descriptions must wrap")
            for screen_row = beginning.row + 1, ending.row do
              for column = 1, width do
                local cell = vim.api.nvim__inspect_cell(1, screen_row - 1, column - 1)
                assert(cell[2].background == expected[2], "wrapped test description lost its diff background: "
                  .. vim.inspect({ row = screen_row, column = column, cell = cell }))
              end
            end
          end
        end
      end
    end
    vim.api.nvim_win_set_cursor(review.win, { file_row("tests/new.rs"), 0 })
    review.command_set.action_by_id.toggle.run({})
    await(function() return not locate(function(text) return text == "new_file_case" end) end, "test file did not fold")
    assert(locate(function(text) return text == "changed_case" end), "file folding hid another file")
    vim.api.nvim_win_set_cursor(review.win, { file_row("tests/new.rs"), 0 })
    review.command_set.action_by_id.toggle.run({})
    await(function() return locate(function(text) return text == "new_file_case" end) end, "test file did not expand")
    vim.api.nvim_win_set_cursor(review.win, { file_row("tests/reused.rs"), 0 })
    review.command_set.action_by_id.toggle.run({})
    await(function() return not locate(function(text) return text == "reused_file_case" end) end, "unrelated file did not fold")
    for _, selected in ipairs({
      { "  Removes obsolete coverage.", "removed_case", "Tests · 2 new · 1 modified · 2 reused" },
      { "  Covers a new test file.", "new_file_case", "Tests · 1 new · 1 modified · 2 reused" },
    }) do
      local row = assert(locate(function(text) return text == selected[1] end), "test description disappeared")
      local version = review.owner.version
      vim.api.nvim_win_set_cursor(review.win, { row, 2 })
      vim.cmd("normal! V")
      review.command_set.action_by_id.delete_tests.run({})
      assert(vim.bo.filetype == "ForgeConfirm", "test description selection did not open delete confirmation")
      vim.fn.maparg("y", "n", false, true).callback()
      check_test_heading()
      await(function() return review.owner.version > version end, "test deletion did not settle")
      check_test_heading()
      assert(not locate(function(text) return text == selected[2] end), "deleted test is still rendered")
      assert(locate(function(text) return vim.trim(text) == selected[3] end), "test deletion lost the count of folded tests")
      assert(not locate(function(text) return text == "reused_file_case" end), "test deletion expanded an unrelated file")
    end
    assert(not locate(function(text) return text:match("^New%s+tests/new%.rs$") end), "last test deletion left an empty file heading")
    review.command_set.action_by_id.close.run({})
    require("forge.views.harness.controller").command_set().action_by_id.close.run({})
    return
  end
  if vim.g.forge_test_plan_behavior then
    vim.o.columns = original_columns
    local planned_row, changed_row
    for index, row in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
      assert(not row:find("[changed]", 1, true), "collapsed signature retained [changed]")
      if row:find("pub fn planned_probe", 1, true) then planned_row = index end
      if row:find("pub fn run", 1, true) then changed_row = index end
    end
    assert(planned_row and changed_row, "new and modified callable signatures are missing")
    vim.api.nvim_win_set_cursor(review.win, { planned_row, 0 })
    review.command_set.action_by_id.toggle.run({})
    await(function()
      return table.concat(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false), "\n"):find("result ← value", 1, true)
    end, "Algorithm did not expand")
    local algorithm_row, nested_indent, branch_indent
    for index, row in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
      if vim.trim(row) == "Algorithm" then algorithm_row = index end
      if vim.trim(row) == "if result ≠ none:" then branch_indent = #(row:match("^%s*") or "") end
      if vim.trim(row) == "return ProbeResult(value = result)" then nested_indent = #(row:match("^%s*") or "") end
    end
    assert(algorithm_row and nested_indent == branch_indent + 2, "Algorithm lost nested pseudocode indentation")
    vim.api.nvim_win_set_cursor(review.win, { changed_row, 0 })
    review.command_set.action_by_id.toggle.run({})
    await(function()
      return table.concat(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false), "\n"):find("Preserve behavior while reviewing", 1, true)
    end, "Change did not expand")
    assert(table.concat(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false), "\n"):find("Change", 1, true), "modified callable lost Change")
    for index, row in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
      if vim.trim(row) == "Algorithm" then algorithm_row = index break end
    end
    local highlighted_row
    for index, row in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
      if vim.trim(row) == "if result ≠ none:" then highlighted_row = index break end
    end
    assert(highlighted_row, "Algorithm conditional is missing")
    vim.api.nvim_set_hl(0, "@keyword.conditional.algorithm", { fg = 0x22ddaa })
    vim.api.nvim_win_set_cursor(review.win, { highlighted_row, 0 })
    vim.cmd("normal! zz")
    vim.cmd("redraw!")
    local screen_row = vim.fn.screenpos(review.win, highlighted_row, 1).row
    local cells = {}
    for column = 1, 160 do cells[#cells + 1] = vim.fn.screenstring(screen_row, column) end
    local screen_text = table.concat(cells)
    local column = assert(screen_text:find("if result", 1, true), screen_text)
    local cell = vim.api.nvim__inspect_cell(1, screen_row - 1, column - 1)
    assert(cell[2].foreground == 0x22ddaa, "Algorithm keyword did not render its Tree-sitter color: " .. vim.inspect(cell))
    vim.api.nvim_set_hl(0, "@type.algorithm", { fg = 0xffaa55 })
    vim.api.nvim_set_hl(0, "@variable.member.algorithm", { fg = 0xaa77dd })
    local constructor_row
    for index, row in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
      if vim.trim(row) == "return ProbeResult(value = result)" then constructor_row = index break end
    end
    assert(constructor_row, "explicit construction is missing")
    vim.api.nvim_win_set_cursor(review.win, { constructor_row, 0 })
    vim.cmd("redraw!")
    local constructor_screen_row = vim.fn.screenpos(review.win, constructor_row, 1).row
    local constructor_cells = {}
    for position = 1, 160 do constructor_cells[#constructor_cells + 1] = vim.fn.screenstring(constructor_screen_row, position) end
    local constructor_text = table.concat(constructor_cells)
    for _, expected in ipairs({ { "ProbeResult", 0xffaa55 }, { "value =", 0xaa77dd } }) do
      local position = assert(constructor_text:find(expected[1], 1, true), constructor_text)
      local rendered = vim.api.nvim__inspect_cell(1, constructor_screen_row - 1, position - 1)
      assert(rendered[2].foreground == expected[2], "constructor highlight did not render: " .. vim.inspect(rendered))
    end
    for _, jump in ipairs({
      { "direction ← movement_direction(0)", "movement_direction", "pub fn movement_direction", "movement_direction" },
      { "abilities.start_dash(direction)", "start_dash", "pub fn start_dash", "start_dash" },
      { "abilities.start_dash(direction)", "abilities", "abilities: &mut PlayerAbilities", "abilities" },
      { "result ← value", "value", "value: Option<u32>", "value" },
      { "abilities.cooldown ← 0", "cooldown", "cooldown: u32", "cooldown" },
    }) do
      local origin_row, origin_column
      for index, text in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
        if vim.trim(text) == jump[1] then origin_row, origin_column = index, assert(text:find(jump[2], 1, true)) - 1 break end
      end
      assert(origin_row, "Algorithm navigation origin missing: " .. jump[1])
      vim.api.nvim_win_set_cursor(review.win, { origin_row, origin_column })
      local window_count = #vim.api.nvim_list_wins()
      review.command_set.action_by_id.jump_entity.run({})
      await(function()
        return vim.api.nvim_get_current_buf() == review.buf and vim.api.nvim_get_current_line():find(jump[3], 1, true)
      end, "Algorithm definition did not resolve: " .. jump[2] .. " " .. table.concat(warnings, "\n"))
      if jump[2] == "start_dash" then
        local before = table.concat(vim.api.nvim_buf_get_lines(review.buf,
          math.max(0, vim.api.nvim_win_get_cursor(review.win)[1] - 4), vim.api.nvim_win_get_cursor(review.win)[1], false), "\n")
        assert(before:find("impl PlayerAbilities", 1, true), "Algorithm selected the wrong receiver type: " .. before)
      end
      assert(vim.api.nvim_get_current_line():sub(vim.api.nvim_win_get_cursor(review.win)[2] + 1):match("^" .. jump[4]),
        "Algorithm definition selected the wrong token: " .. jump[2])
      assert(vim.api.nvim_get_current_win() == review.win and #vim.api.nvim_list_wins() == window_count,
        "Algorithm definition opened another window")
      vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-o>", true, false, true), "xt", false)
      assert(vim.trim(vim.api.nvim_get_current_line()) == jump[1], "Algorithm jump-back lost its origin")
    end
    vim.api.nvim_set_hl(0, "ForgeAddBg", { bg = 0x123425 })
    for _ = 1, 2 do
      for index, row in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
        if row:find("pub fn planned_probe", 1, true) then
          vim.api.nvim_win_set_cursor(review.win, { index, 0 })
          break
        end
      end
      review.command_set.action_by_id.toggle.run({})
    end
    review.owner.action("comment", function(result, failure) assert(result and not failure, failure) end)
    vim.cmd("stopinsert")
    for index, row in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
      if vim.trim(row) == "return ProbeResult(value = result)" then constructor_row = index break end
    end
    local source_namespace = vim.api.nvim_get_namespaces()["ForgePlanDraftSource" .. review.buf]
    local source_background
    for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(review.buf, source_namespace, { constructor_row - 1, 0 }, { constructor_row - 1, -1 }, { details = true })) do
      if mark[4].hl_group == "ForgeAddBg" then source_background = mark[4] end
    end
    assert(source_background and source_background.hl_eol and source_background.end_row == constructor_row,
      "PlanReview repaint lost the full-row background: " .. vim.inspect(source_background))
    for _, width in ipairs({ 160, 80, 120 }) do
      vim.o.columns = width
      vim.api.nvim_win_set_cursor(review.win, { constructor_row, 0 })
      vim.cmd("normal! zz")
      vim.cmd("redraw!")
      local row = vim.fn.screenpos(review.win, constructor_row, 1).row
      local edge = vim.api.nvim__inspect_cell(1, row - 1, width - 1)
      assert(edge[2].background == 0x123425,
        "PlanReview addition background stopped before the window edge at width " .. width .. ": " .. vim.inspect(edge))
    end
    vim.o.columns = original_columns
    vim.api.nvim_set_hl(0, "Normal", { fg = 0xc0c0c0, bg = 0 })
    vim.api.nvim_set_hl(0, "Comment", { fg = 0x888888 })
    local headers = {}
    for index, text in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
      local label = vim.trim(text)
      if vim.tbl_contains({ "Algorithm", "Change", "Calls", "Constructs", "Accesses" }, label) then
        vim.api.nvim_win_set_cursor(review.win, { index, 0 })
        vim.cmd("normal! zz")
        vim.cmd("redraw!")
        local position = vim.fn.screenpos(review.win, index, assert((text:find(label, 1, true))))
        local cell = vim.api.nvim__inspect_cell(1, position.row - 1, position.col - 1)
        assert((cell[2].foreground or 0xc0c0c0) == 0xc0c0c0, label .. " header does not share reference heading styling: " .. vim.inspect(cell))
        headers[label] = true
      end
    end
    assert(headers.Algorithm and headers.Change and headers.Constructs, "behavior and reference heading fixture is incomplete")
    local prose_row, prose_text
    for index, text in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
      if text:find("Decrease cooldown_remaining", 1, true) then prose_row, prose_text = index, text break end
    end
    assert(prose_row, "long Algorithm prose is missing")
    for _, width in ipairs({ 80, 120, 160 }) do
      vim.o.columns = width
      vim.api.nvim_win_set_cursor(review.win, { prose_row, 0 })
      vim.cmd("normal! zz")
      vim.cmd("redraw!")
      local first = vim.fn.screenpos(review.win, prose_row, assert((prose_text:find("Decrease", 1, true))))
      local last = vim.fn.screenpos(review.win, prose_row, #prose_text)
      local prose_cell = vim.api.nvim__inspect_cell(1, first.row - 1, first.col - 1)
      assert(prose_cell[2].foreground == 0x888888, "Algorithm prose is not comment gray")
      local variable = vim.fn.screenpos(review.win, prose_row, assert((prose_text:find("cooldown_remaining", 1, true))))
      local variable_cell = vim.api.nvim__inspect_cell(1, variable.row - 1, variable.col - 1)
      assert(variable_cell[2].foreground == 0xffffff, "Algorithm variable is not white")
      if width < 160 then assert(last.row > first.row, "long Algorithm row did not wrap at " .. width) end
      for row = first.row + 1, last.row do
        local cells = {}
        for column = 1, width do cells[#cells + 1] = vim.fn.screenstring(row, column) end
        local first_text = table.concat(cells):find("%S")
        assert(first_text == first.col, "wrapped Algorithm indentation differs at " .. width .. ": " .. vim.inspect({ first.col, first_text,
          vim.wo[review.win].breakindentopt }))
        local cell = vim.api.nvim__inspect_cell(1, row - 1, first.col - 1)
        assert(cell[2].background == 0x123425, "wrapped Algorithm lost its diff background")
      end
    end
    vim.o.columns = original_columns
    vim.api.nvim_win_set_cursor(review.win, { algorithm_row, 0 })
    vim.cmd("normal! zz")
    vim.g.forge_plan_behavior_verified = true
    if vim.g.forge_test_manual then return end
    review.command_set.action_by_id.close.run({})
    require("forge.views.harness.controller").command_set().action_by_id.close.run({})
    return
  end
  if definition_only then
  assert(not table.concat(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false), "\n"):find("secret: usize", 1, true),
    "private property leaked into the initial Public view")
  local update_row
  for index, row in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
    if row:find("pub fn update", 1, true) then update_row = index break end
  end
  vim.api.nvim_win_set_cursor(review.win, { assert(update_row), 0 })
  review.command_set.action_by_id.toggle.run({})
  await(function()
    return table.concat(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false), "\n"):find("Client::secret", 1, true)
  end, "private property use did not expand")
  local secret_row, secret_column
  for index, row in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
    if vim.trim(row) == "Client::secret" then secret_row, secret_column = index, row:find("secret", 1, true) - 1 end
  end
  assert(secret_row, "private property reference is missing")
  vim.api.nvim_win_set_cursor(review.win, { secret_row, secret_column })
  local window_count = #vim.api.nvim_list_wins()
  review.command_set.action_by_id.open.run({})
  await(function()
    return vim.api.nvim_get_current_buf() == review.buf and vim.api.nvim_get_current_line():find("secret: usize", 1, true)
  end, "private property definition did not reveal in the plan")
  assert(vim.api.nvim_get_current_win() == review.win and #vim.api.nvim_list_wins() == window_count,
    "private property definition opened a popup")
  assert(vim.api.nvim_get_current_line():sub(vim.api.nvim_win_get_cursor(review.win)[2] + 1):match("^secret"),
    "private property definition selected the wrong column")
  assert(review.public_only, "private property jump changed the Public filter")
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-o>", true, false, true), "xt", false)
  assert(vim.trim(vim.api.nvim_get_current_line()) == "Client::secret", "jump-back lost the private property origin")
  review.command_set.action_by_id.jump_entity.run({})
  await(function()
    return vim.api.nvim_get_current_buf() == review.buf and vim.api.nvim_get_current_line():find("secret: usize", 1, true)
  end, "definition did not reuse the revealed declaration")
  assert(#vim.api.nvim_list_wins() == window_count, "definition opened a declaration popup")
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-o>", true, false, true), "xt", false)
  review.command_set.action_by_id.toggle.run({})
  await(function()
    return not table.concat(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false), "\n"):find("Client::secret", 1, true)
  end, "private property function did not collapse")
    local file_row
    for index, row in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
      if row:match("^Modified src/change%.rs ") then file_row = index break end
    end
    vim.api.nvim_win_set_cursor(review.win, { assert(file_row, "source file heading is missing:\n"
      .. table.concat(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false), "\n")), 0 })
    review.command_set.action_by_id.open.run({})
    await(function()
      return vim.fs.normalize(vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf()))
        == vim.fs.normalize(workspace .. "/src/change.rs")
    end, "file heading did not open its actual source buffer")
    assert(vim.api.nvim_get_current_win() == review.win and #vim.api.nvim_list_wins() == window_count,
      "file heading opened a declaration popup")
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-o>", true, false, true), "xt", false)
    assert(vim.api.nvim_get_current_buf() == review.buf, "source jump-back did not return to PlanReview")
    review.command_set.action_by_id.close.run({})
    require("forge.views.harness.controller").command_set().action_by_id.close.run({})
    return
  end
  assert(not vim.wo[review.win].number and not vim.wo[review.win].relativenumber and vim.wo[review.win].statuscolumn == "",
    "PlanReview displayed editor line numbers")
  local description_row
  local paragraph = vim.split(document.design.document.design, "\n", { plain = true })[1]
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
  assert(paragraph_height(review.win) > wide_height, ("narrowing PlanReview did not reflow its paragraph: columns=%d width=%d wide=%d narrow=%d row=%d wrap=%s")
    :format(vim.o.columns, vim.api.nvim_win_get_width(review.win), wide_height, paragraph_height(review.win), description_row, tostring(vim.wo[review.win].wrap)))
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
    assert(not row:match("^%s*Calls$") and not row:match("^%s*Constructs$") and not row:match("^%s*Accesses$"), "collapsed functions exposed a body category")
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
  assert(property_picker.spec.page_list[1].option_list[1].columns[3] == "Client::count",
    "property text did not use the unfolded Accesses row")
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
  assert(expanded_body:find("pub fn update(client: &mut Client) {\n  Calls\n    leaf\n  Constructs\n  Accesses\n    Client::count\n}", 1, true), "reference selection did not reveal both categories inside the function body")
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
  assert(active.spec.height_ratio == 0.3, "plan references did not set their host height limit")
  assert(vim.api.nvim_win_get_height(active.win) <= math.max(4, math.floor(vim.api.nvim_win_get_height(review.win) * 0.3)),
    "plan references exceeded thirty percent of the review height")
  assert(active.spec.page_list[1].search, "reference picker is not searchable")
  assert(#active.spec.page_list[1].option_list == 2, "calls did not retain both distinct caller results")
  assert(active.spec.page_list[1].option_list[1].value.owner == "run")
  await(function() return #vim.api.nvim_buf_get_extmarks(review.buf, preview_namespace, 0, -1, {}) == 1 end, "initial call preview is absent")
  local first_preview = vim.api.nvim_win_get_cursor(review.win)
  local warning_count = #warnings
  vim.api.nvim_exec_autocmds("FocusLost", {})
  vim.cmd("tabnew")
  vim.api.nvim_exec_autocmds("WinResized", {})
  vim.wait(100, function() return false end, 10)
  vim.cmd("tabclose")
  vim.api.nvim_set_current_win(active.win)
  vim.api.nvim_exec_autocmds("FocusGained", {})
  vim.api.nvim_exec_autocmds("WinResized", {})
  vim.wait(100, function() return false end, 10)
  assert(#warnings == warning_count, "focus and tab changes invalidated unchanged plan references: " .. table.concat(warnings, "\n"))
  assert(picker.is_open(), "focus change closed the reference picker")
  local first_option = active.frame.lines[active.frame.option_range[1].first]
  local second_option = active.frame.lines[active.frame.option_range[2].first]
  assert(first_option:find("run", 1, true) == second_option:find("update", 1, true), "caller columns are not aligned")
  assert(first_option:find("leaf", 1, true) == second_option:find("leaf", 1, true), "symbol columns are not aligned")
  assert(vim.deep_equal(active.spec.page_list[1].column_headers, { "Location", "Caller", "Text" }), "reference picker did not replace its symbol column with text")
  assert(active.spec.page_list[1].option_list[1].columns[3] == "leaf", "call context did not use the unfolded Calls row")
  assert(first_option:find("src/change.rs", 1, true) == 3, "unkeyed reference location is not aligned with the title")
  assert(active.frame.lines[active.frame.footer_line - 1] ~= "", "reference picker retained an empty footer row")
  local caller_highlight, filename_highlight, text_highlight = false, false, false
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(active.buf, -1, 0, -1, { details = true })) do
    local text = active.frame.lines[mark[2] + 1]:sub(mark[3] + 1, mark[4].end_col)
    if mark[4].hl_group == "@function" and text == "run" then caller_highlight = true end
    if mark[4].hl_group == "ForgeFileName" and text == "change.rs" then filename_highlight = true end
    if mark[4].hl_group == "@function.call" and text == "leaf" then text_highlight = true end
  end
  assert(caller_highlight and filename_highlight and text_highlight, "references did not render semantic caller, filename, and text styles")
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
  await(function() return picker.is_open()
    and #vim.api.nvim_buf_get_extmarks(review.buf, preview_namespace, 0, -1, {}) == 1 end, "call reference picker did not reopen")
  active = picker._state_for_test()
  vim.api.nvim_win_set_cursor(review.win, { leaf_row, leaf_column })
  require("forge.input").capture(review.owner.replica, review.owner.current_view(review.win), "open")
  warning_count = #warnings
  select = vim.api.nvim_buf_call(active.buf, function() return vim.fn.maparg("<CR>", "n", false, true) end)
  select.callback()
  vim.wait(100, function() return false end, 10)
  assert(vim.api.nvim_win_get_cursor(review.win)[1] == leaf_row, "stale picker selection changed the review cursor")
  assert(not picker.is_open() and #warnings == warning_count + 1, "stale references did not close with exactly one warning")
  vim.api.nvim_exec_autocmds("WinResized", {})
  vim.wait(50, function() return false end, 10)
  assert(#warnings == warning_count + 1, "closed reference picker repeated its stale warning")
  assert(vim.deep_equal(vim.fn.readfile(artifact, "b"), canonical), "reference navigation modified the plan")
  assert(vim.deep_equal(vim.fn.readfile(workspace .. "/src/change.rs"), source), "planning modified implementation source")
  for index, row in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
    if row:find("pub struct Client", 1, true) then
      vim.api.nvim_win_set_cursor(review.win, { index, row:find("Client", 1, true) - 1 })
      break
    end
  end
  review.command_set.action_by_id.references.run({})
  await(function() return picker.is_open() end, "type reference picker did not open")
  local type_option
  for _, option in ipairs(picker._state_for_test().spec.page_list[1].option_list) do
    if option.columns[3]:find("pub fn update", 1, true) then type_option = option break end
  end
  assert(type_option, "type references did not include their complete declaration line")
  assert(vim.tbl_contains(vim.tbl_map(function(span) return span.group end, type_option.column_spans[3]), "@type.rust"),
    "declaration reference text did not reuse Rust syntax captures")
  picker.close(true)
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
  await(function() return not state.busy and state.plan_review == review and review.owner.version == document.version + 1 end, "rename did not update the retained review")
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
if success and vim.g.forge_test_manual and vim.g.forge_plan_behavior_verified then
  print("plan_review_behavior_host: passed at " .. vim.o.columns .. "x" .. vim.o.lines)
  return
end
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
