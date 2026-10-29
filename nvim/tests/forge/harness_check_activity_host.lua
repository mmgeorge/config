vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
vim.loader.enable(false)
if not vim.g.forge_check_activity_grid and not vim.g.forge_check_activity_hold then
  local child = vim.fn.jobstart({ vim.v.progpath, "--embed", "--headless", "-u", "NONE", "-i", "NONE" }, { rpc = true })
  assert(child > 0)
  local success, failure = pcall(function()
    vim.rpcrequest(child, "nvim_ui_attach", 120, 42, { rgb = true })
    vim.rpcrequest(child, "nvim_exec_lua", [[
      vim.g.forge_check_activity_grid = true
      dofile('nvim/tests/forge/harness_check_activity_host.lua')
    ]], {})
  end)
  vim.fn.jobstop(child)
  assert(success, failure)
  print("harness_check_activity_host: passed at 120x42")
  vim.cmd("qa!")
  return
end
local original_directory = vim.fn.getcwd()
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
local hold = vim.g.forge_check_activity_hold or 0
vim.fn.writefile({ vim.json.encode({ checks = {
  ["activity"] = { trigger = { auto = false }, action = { command = { "nu",  "--no-config-file", "-c",
      ("sleep %dms; print 'retained check output'"):format(hold + 1200)  }, label = "Activity fixture", directory = ".", category = "test", after = {} } },
  ["rust-stream"] = { trigger = { auto = false }, action = {
    command = { "nu", "--no-config-file", "--stdin", original_directory .. "/nvim/tools/checks/rust.nu", "test" },
    label = "Rust streaming fixture", category = "test", input = true, after = {},
    inputs = {
      package = { type = "string", description = "Fixture package", default = "stream-fixture" },
      test = { type = "string", description = "Exact fixture test", default = "streaming" },
    },
  } },
  ["empty"] = { trigger = { auto = false }, action = { command = { "nu", "--no-config-file", "-c", "exit 0" }, label = "Empty fixture", directory = ".", category = "test", after = {} } },
  ["unused"] = { trigger = { auto = false }, action = { command = { "nu",  "--no-config-file", "-c", "exit 9"  }, label = "Unused fixture", directory = ".", category = "test", after = {} } }
} }) }, workspace .. "/.forge.json")
local executable = data .. "/forge-host" .. (vim.fn.has("win32") == 1 and ".exe" or "")
assert(vim.uv.fs_copyfile(require("forge.builder").binary_path(), executable))
local original_stdpath, original_notify = vim.fn.stdpath, vim.notify
vim.fn.stdpath = function(kind) return kind == "data" and data or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = executable }) end)
end }
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local errors, answered = {}, {}
vim.notify = function(message, level)
  if level == vim.log.levels.ERROR then errors[#errors + 1] = tostring(message) end
end

local function answer_permissions()
  for _, approval in ipairs(state.approval or {}) do
    if not answered[approval.id] then
      answered[approval.id] = true
      local answers = {}
      for _, item in ipairs(approval.item_list) do
        answers[#answers + 1] = { item_id = item.id, choice_id = "allow_once" }
      end
      client.request({ method = "session.approval.resolve", params = { approval_id = approval.id, answer_list = answers }, session_id = assert(require("forge.session").harness.session).id }, function(_, failure)
        assert(not failure, failure)
      end)
    end
  end
end

---@param method string
---@param params? table
---@return table
local function request(method, params)
  local settled, value, failure
  client.request({ method = method, params = params or {}, session_id = require("forge.event_contract").method_policy(method).session_context ~= "none" and assert(require("forge.session").harness.session).id or nil }, function(result, message) value, failure, settled = result, message, true end)
  assert(vim.wait(10000, function() answer_permissions() return settled end, 10), method .. " timed out")
  assert(not failure, method .. ": " .. tostring(failure))
  return value
end

local function presentation_text()
  local status = state.presentation and state.presentation.status_view
  local lines = vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)
  if status then vim.list_extend(lines, status.content and status.content.lines or {}) end
  return table.concat(lines, "\n")
end

local function assert_status_split()
  local view = state.presentation.status_view
  if not view.content or #view.content.lines == 0 then assert(view.window == nil) return end
  assert(view.window and vim.api.nvim_win_is_valid(view.window))
  assert(not vim.api.nvim_win_get_config(view.window).focusable)
  assert(vim.api.nvim_win_get_height(view.window) <= vim.api.nvim_win_text_height(view.window, { start_row = 0 }).all)
end

local succeeded, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = false,
    harness = { backend = "mock", backends = { mock = { command = { "visible-blocking" }, selectable = true } } } })
  require("forge.views.harness").open()
  assert(vim.wait(10000, function() return state.session ~= nil end, 10), table.concat(errors, "\n"))
  local prompt_failure
  client.request({ method = "session.prompt.submit", params = { text = "Read while checks run" }, session_id = assert(require("forge.session").harness.session).id }, function(_, message) prompt_failure = message end)
  assert(vim.wait(3000, function() return presentation_text():find("Working", 1, true) ~= nil end, 10),
    "provider did not start: " .. tostring(prompt_failure) .. "\n" .. presentation_text() .. "\n" .. table.concat(errors, "\n"))
  request("session.turn.cancel")
  assert(vim.wait(3000, function() return state.presentation and state.presentation.status_view.snapshot and #state.presentation.status_view.snapshot.block == 0 end, 10))
  local run_id = request("session.check.run", { check = { "activity", "empty" } }).run_id
  assert(vim.wait(3000, function()
    answer_permissions()
    return presentation_text():find("Activity fixture", 1, true) ~= nil
      and presentation_text():find("Checks running", 1, true) ~= nil
  end, 10), "live check did not reach the status area")
  local snapshot = request("session.state.get")
  assert(snapshot.checks_running and snapshot.active_check_run == run_id)
  assert(snapshot.status.kind == "idle", "check replaced the primary phase")
  local running = presentation_text()
  local phase_row, checks_row = running:find("Checking", 1, true), running:find("Checks running", 1, true)
  assert(not running:find("User requested", 1, true) and not running:find("Agent requested", 1, true),
    "check progress must not expose requester labels")
  assert(phase_row and checks_row and phase_row < checks_row, running)
  assert(not running:find("retained check output", 1, true), "running status exposed check output")
  assert_status_split()
  if hold > 0 then vim.wait(hold, function() vim.cmd("redraw") return false end, 20) end
  local result
  assert(vim.wait(10000, function()
    result = request("session.check.status", { run_id = run_id })
    return result.state ~= "running"
  end, 20), "check did not finish")
  assert(result.state == "passed", vim.inspect(result))
  assert(vim.wait(3000, function()
    return presentation_text():find("Checks finished", 1, true) ~= nil
      and presentation_text():find("Checks running", 1, true) == nil
  end, 10), "completion did not move the result into history")
  local completed = request("session.state.get")
  assert(not completed.checks_running and (completed.active_check_run == nil or completed.active_check_run == vim.NIL))
  assert(completed.status.kind == "idle", "completion changed primary work")
  local _, count = presentation_text():gsub("Checks finished", "")
  assert(count == 1, "result history was duplicated")
  assert(not presentation_text():find("retained check output", 1, true), "result output was not collapsed")
  local _, check_row = state.presentation.transcript.sequence:position(run_id .. ":checks")
  assert(check_row ~= nil, "check result heading is missing")
  vim.api.nvim_set_current_win(state.transcript_win)
  vim.api.nvim_win_set_cursor(state.transcript_win, { check_row + 1, 0 })
  assert(state.presentation.toggle_heading(state.transcript_win), "check results did not expand")
  local category_row
  assert(vim.wait(3000, function()
    if not state.presentation.transcript.sequence.node[run_id .. ":category:test"] then return false end
    _, category_row = state.presentation.transcript.sequence:position(run_id .. ":category:test")
    return category_row ~= nil
  end, 10), "test category is missing")
  vim.api.nvim_win_set_cursor(state.transcript_win, { category_row + 1, 0 })
  assert(state.presentation.toggle_heading(state.transcript_win), "test category did not expand")
  assert(vim.wait(3000, function()
    return presentation_text():find("Activity fixture", 1, true) ~= nil
  end, 10), "executed check is missing from expanded results")
  assert(not presentation_text():find("Unused fixture", 1, true), "skipped check appeared in expanded results")
  local empty_command
  for _, command in ipairs(result.commands) do
    if command.context.definition_id == "empty" then empty_command = command.id end
  end
  assert(empty_command, "empty check result is missing")
  local function transcript_row(text)
    for index, line in ipairs(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)) do
      if line:find(text, 1, true) then return index - 1 end
    end
  end
  local empty_heading_row
  assert(vim.wait(3000, function()
    empty_heading_row = transcript_row("Empty fixture")
    return empty_heading_row ~= nil
  end, 10), "empty check tool did not load")
  assert(empty_heading_row, "empty check heading is missing")
  vim.api.nvim_win_set_cursor(state.transcript_win, { empty_heading_row + 1, 0 })
  assert(state.presentation.toggle_heading(state.transcript_win), "empty check did not expand")
  assert(vim.wait(3000, function() return presentation_text():find("no output", 1, true) ~= nil end, 10))
  local output_row = transcript_row("no output")
  assert(output_row, "empty output row is missing")
  vim.api.nvim_win_set_cursor(state.transcript_win, { output_row + 1, 0 })
  vim.cmd("normal! zz")
  vim.cmd("redraw!")
  local output_position = vim.fn.screenpos(state.transcript_win, output_row + 1, 1)
  local rendered_output = ""
  for column = 1, 120 do rendered_output = rendered_output .. vim.fn.screenstring(output_position.row, column) end
  assert(rendered_output:find("└  no output", 1, true), "output branch or indentation is wrong: " .. rendered_output)
  assert(not rendered_output:find("▸", 1, true) and not rendered_output:find("▾", 1, true),
    "output row inherited a heading triangle: " .. rendered_output)
  vim.api.nvim_win_set_cursor(state.transcript_win, { empty_heading_row + 1, 0 })
  assert(state.presentation.toggle_heading(state.transcript_win), "empty check did not collapse")
  assert(vim.wait(3000, function() return presentation_text():find("no output", 1, true) == nil end, 10))
  assert(#state.presentation.status_view.snapshot.block == 0, "idle retained native status content")
  assert_status_split()
  assert(#errors == 0, table.concat(errors, "\n"))
  if hold > 0 then vim.wait(hold, function() vim.cmd("redraw") return false end, 20) end

  local session_id = state.session.id
  local exchange = request("session.exchange.list")[1]
  assert(exchange and exchange.completed_at_ms, "fixture exchange did not settle")
  require("forge.views.harness.workspace").release(state)
  state.presentation.close()
  client.host_stop()
  assert(vim.wait(5000, function() return client._client.process == nil end, 10), "first host did not stop")

  -- Seed the provider-owned persisted form of this actual run before the next host opens it.
  local record_path = data .. "/forge/harness/" .. session_id .. "/checks/" .. run_id .. "/record.json"
  local record = vim.json.decode(table.concat(vim.fn.readfile(record_path), "\n"))
  record.run.owner = { kind = "exchange", session_id = session_id, exchange_id = exchange.id, node_count = 0 }
  vim.fn.writefile({ vim.json.encode(record) }, record_path)
  local restored, restore_error
  client.session_initialize(function(value, problem) restored, restore_error = value, problem end, { session_id = session_id })
  assert(vim.wait(10000, function() return restored or restore_error end, 10), "history host did not reopen")
  assert(not restore_error, restore_error)
  require("forge.views.harness.session_navigation").activate(restored, {
    interaction_mode = "reconcile", open_mode = "tab",
  })
  state = require("forge.session").harness
  assert(vim.wait(5000, function() return presentation_text():find("Checks finished", 1, true) ~= nil end, 10),
    "restored exchange hid its check summary inside collapsed activity: " .. presentation_text()
      .. "\nhistory=" .. vim.inspect(vim.tbl_map(function(entry)
        return {id = entry.id, state = entry.state, owner = entry.owner}
      end, request("session.check.list").history))
      .. "\nerrors=" .. vim.inspect(errors))
  local sequence = state.presentation.transcript.sequence
  local function assert_visible_check()
    local summary_id = exchange.id .. ":results:checks"
    local _, row = sequence:position(summary_id)
    assert(row, "check summary disappeared")
    local block = state.presentation.transcript.block[summary_id]
    assert(not block.metadata.node.parent or block.metadata.node.parent == vim.NIL,
      "check summary belongs to the activity fold")
    vim.api.nvim_set_current_win(state.transcript_win)
    vim.api.nvim_win_set_cursor(state.transcript_win, { row + 1, 0 })
    vim.cmd("normal! zz")
    vim.cmd("redraw!")
    local position = vim.fn.screenpos(state.transcript_win, row + 1, 1)
    local rendered = ""
    for column = 1, 120 do rendered = rendered .. vim.fn.screenstring(position.row, column) end
    assert(rendered:find("Checks finished", 1, true), "screen is missing check summary: " .. rendered)
    assert(not presentation_text():find("retained check output", 1, true), "restored output was expanded")
  end
  assert_visible_check()
  for _, expanded in ipairs({ true, false }) do
    local _, row = sequence:position(exchange.id .. ":summary")
    assert(row, "exchange activity heading is missing")
    vim.api.nvim_win_set_cursor(state.transcript_win, { row + 1, 0 })
    assert(state.presentation.toggle_heading(state.transcript_win))
    assert(vim.wait(3000, function()
      local block = state.presentation.transcript.block[exchange.id .. ":summary"]
      return (block.metadata.node.display == "full") == expanded
    end, 10), "activity toggle did not settle")
    assert_visible_check()
  end
  local _, restored_count = presentation_text():gsub("Checks finished", "")
  assert(restored_count == 1, "restored check summary was duplicated")

  local rust_root = workspace .. "/nvim/rust/forge"
  vim.fn.mkdir(rust_root .. "/src", "p")
  vim.fn.writefile({
    '[package]', 'name = "stream-fixture"', 'version = "0.1.0"', 'edition = "2024"',
  }, rust_root .. "/Cargo.toml")
  assert(vim.uv.fs_copyfile(original_directory .. "/nvim/rust/forge/rust-toolchain.toml",
    rust_root .. "/rust-toolchain.toml"))
  vim.fn.writefile({
    "#[test]",
    "fn streaming() {",
    '    while !std::path::Path::new("release-check").exists() {',
    "        std::thread::sleep(std::time::Duration::from_millis(10));",
    "    }",
    "}",
    "#[test]",
    'fn failure() { panic!("fixture failure"); }',
  }, rust_root .. "/src/lib.rs")
  local lock = vim.system({ "cargo", "generate-lockfile", "--offline" }, { cwd = rust_root, text = true }):wait(10000)
  assert(lock.code == 0, lock.stderr)
  local function rust_command(run)
    for _, command in ipairs(run.commands) do
      if command.context and command.context.definition_id == "rust-stream" then return command end
    end
    error("Rust callback command is missing")
  end
  local streaming_id = request("session.check.run", { check = { "rust-stream" }, inputs = { test = "streaming" } }).run_id
  assert(vim.wait(30000, function()
    answer_permissions()
    return presentation_text():find("    running 1 test", 1, true) ~= nil
  end, 10), "Rust callback buffered output until exit: " .. presentation_text())
  local active_rust = request("session.check.status", { run_id = streaming_id })
  assert(active_rust.state == "running", "streaming evidence arrived only after Cargo finished")
  local live_text = presentation_text()
  assert(live_text:find("Rust streaming fixture", 1, true), live_text)
  assert(not live_text:find("Compiling stream-fixture", 1, true), "status retained earlier output lines")
  local _, live_lines = live_text:gsub("    running 1 test", "")
  assert(live_lines == 1, "status must show exactly one latest output line")
  assert_status_split()
  vim.fn.writefile({ "release" }, rust_root .. "/release-check")
  local rust_result
  assert(vim.wait(15000, function()
    rust_result = request("session.check.status", { run_id = streaming_id })
    return rust_result.state ~= "running"
  end, 20), "Rust streaming check did not finish")
  assert(rust_result.state == "passed", vim.inspect(rust_result))
  local report = rust_command(rust_result).report.result[1]
  assert(report.output:find("Compiling stream-fixture", 1, true), "report lost stderr")
  assert(report.output:find("test streaming ... ok", 1, true), "report lost stdout")
  assert(vim.wait(3000, function()
    return presentation_text():find("Checks running", 1, true) == nil
      and presentation_text():find("running 1 test", 1, true) == nil
  end, 10), "completed Rust output did not collapse")
  for _, exact_test in ipairs({ "failure", "missing" }) do
    local failed_id = request("session.check.run", { check = { "rust-stream" }, inputs = { test = exact_test } }).run_id
    local failed
    assert(vim.wait(15000, function()
      failed = request("session.check.status", { run_id = failed_id })
      return failed.state ~= "running"
    end, 20), "Rust failure fixture did not finish")
    assert(failed.state == "failed", "streaming converted a failed or unmatched test into success")
    assert(rust_command(failed).report.result[1].state == "failed")
  end
end, debug.traceback)
require("forge.views.harness.workspace").release(state)
if state.presentation then state.presentation.close() end
client.host_stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(original_directory)
vim.fn.stdpath, vim.notify = original_stdpath, original_notify
assert(succeeded and collected, failure or "check activity host was not collected")
print("harness_check_activity_host: passed")
if not vim.g.forge_check_activity_grid then vim.cmd("qa!") end
