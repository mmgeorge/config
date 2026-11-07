-- Resolve at load time; the scene changes into its workspace before using the helper.
local check_storage = dofile(vim.fs.dirname(vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p")) .. "/check_storage.lua")
local host_executable = dofile(vim.fs.dirname(vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p")) .. "/host_executable.lua")
local M = {}

---Open an isolated real-host scene with persisted mixed check results.
---@return table
function M.open()
  local root = vim.fn.getcwd()
  local workspace, data = vim.fn.tempname(), vim.fn.tempname()
  vim.fn.mkdir(workspace, "p")
  vim.fn.mkdir(data, "p")
  -- Launch the shared pinned executable lease rather than a freshly scanned per-run copy.
  local binary = host_executable()
  local original_stdpath = vim.fn.stdpath
  vim.fn.stdpath = function(kind) return kind == "data" and data or original_stdpath(kind) end
  package.loaded["forge.builder"] = { ensure = function(callback)
    vim.schedule(function() callback({ ok = true, path = binary }) end)
  end }
  local client = require("forge.client")
  client._host_set_launcher_for_test(vim.system)
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = false,
    harness = { provider = "mock", providers = { mock = { command = { "retained-control" }, selectable = true } } } })
  require("forge.views.harness").open()
  local state = require("forge.session").harness
  assert(vim.wait(10000, function() return state.session ~= nil end, 10), "scene startup timed out")
  assert(vim.wait(5000, function() return state.model_list ~= nil end, 10), "scene model catalog timed out")
  local session_id = state.session.id
  require("forge.views.harness.workspace").release(state)
  state.presentation.close()
  for _, buffer in ipairs({ state.transcript_buf, state.composer_buf }) do
    if vim.api.nvim_buf_is_valid(buffer) then
      vim.api.nvim_buf_delete(buffer, { force = true })
    end
  end
  client.host_stop()
  assert(vim.wait(5000, function() return client._client.process == nil end, 10), "scene host did not stop")
  local directory = check_storage.path(data, session_id, "scene")
  vim.fn.mkdir(directory, "p")
  local commands = {}
  local function command(category, name, scope, status, output, report, cases)
    local index = #commands
    local path = directory .. "/" .. index .. ".output"
    -- The host reads a callback report from its report file only while a run is live; saved
    -- history carries the collected report on the command record itself.
    if report then
      local output_list = {}
      for _, result in ipairs(report.result) do output_list[#output_list + 1] = result.output end
      output = table.concat(output_list, "\n")
    end
    vim.fn.writefile(vim.split(output or "", "\n", { plain = true }), path)
    commands[#commands + 1] = {
      id = "scene:" .. index, command = report and "nu .forge/checks/lua-tests.nu affected" or "fixture " .. name, selected = true,
      state = status, started_at_ms = 1000 + index * 100, completed_at_ms = 1050 + index * 100,
      exit_code = status == "failed" and 1 or 0, error = status == "blocked" and "Required input is unavailable" or vim.NIL,
      stdout = path, stderr = path, output = path, output_bytes = vim.fn.getfsize(path), preview = "",
      report = report or vim.NIL,
      context = { definition_id = category .. "-fixture", name = name, area = ".", reason = "Fixture",
        matched_files = { scope }, category = category, directory = ".",
        selection = cases and { kind = "cases", cases = cases } or { kind = "files", files = { scope } },
        resolved = vim.NIL, depends_on = {}, receipt = path .. ".receipt" },
    }
  end
  command("format", "Rust format", "src/config.rs", "passed")
  command("lint", "Clippy", "engine-core", "failed", "error: unused variable `width`\nhelp: remove the unused binding")
  command("test", "Lua fixtures", "nvim/tests/forge/first.lua", "failed", nil, {
    result = {
      { file = "nvim/tests/forge/first.lua", state = "passed", duration_ms = 2274, command = "nvim -l first.lua", output = "first fixture passed" },
      { file = "nvim/tests/forge/second.lua", state = "failed", duration_ms = 103, command = "nvim -l second.lua",
        output = "assertion failed: expected a category heading\ncontext: Unicode λ and 界 remain intact while this long diagnostic wraps across the narrow terminal without dropping any source text or changing the saved output" },
    }, error = vim.NIL,
  })
  command("test", "Lua fixtures", "nvim/tests/forge/first.lua", "passed", nil, {
    result = {
      { file = "nvim/tests/forge/first.lua", state = "passed", duration_ms = 100,
        command = "nvim -l first.lua", output = "first fixture passed again" },
      { file = "nvim/tests/forge/second.lua", state = "passed", duration_ms = 98,
        command = "nvim -l second.lua", output = "second fixture passed after repair" },
    }, error = vim.NIL,
  })
  command("build", "Game", "game", "passed")
  command("check", "Validate assets", "assets", "blocked")
  command("test", "Rust tests", "tests/layout.rs", "passed", "test tests::layout ... ok", nil,
    { { file = "tests/layout.rs", name = "tests::layout", target = "test:layout" } })
  command("format", "Rust format", "src/config.rs", "passed")
  local run = {
    id = "scene", owner = { kind = "session", session_id = session_id }, revision = 0,
    shell = "system", executable = vim.NIL, workspace = workspace, environment_digest = "scene",
    started_at_ms = 1000, completed_at_ms = 1800, state = "failed", commands = commands,
    progress = { matched = {}, missing = {}, different = {}, unverified = {}, warning = {}, source_digest = vim.empty_dict(), declaration_changes = {} },
    workspace_identity = vim.NIL, workspace_digest = vim.empty_dict(),
    context = { automatic = false, trigger = "Scene", task_id = vim.NIL, overrides = vim.empty_dict(), changed_files = {},
      snapshot = { digest = "scene", config = vim.empty_dict(), helper_digest = vim.empty_dict() } },
  }
  vim.fn.writefile({ vim.json.encode({ version = 5, run = run }) }, directory .. "/record.json")
  local restored, failure
  client.session_initialize(function(value, problem) restored, failure = value, problem end, { session_id = session_id })
  assert(vim.wait(10000, function() return restored or failure end, 10), "scene reload timed out")
  assert(not failure, failure)
  require("forge.views.harness.session_navigation").activate(restored, { interaction_mode = "reconcile", open_mode = "tab" })
  state = require("forge.session").harness
  assert(vim.wait(5000, function()
    if not state.presentation or not state.presentation.transcript then return false end
    return pcall(state.presentation.transcript.sequence.position, state.presentation.transcript.sequence, "scene:checks")
  end, 10), "scene was not rendered")
  assert(vim.wait(5000, function() return state.model_list ~= nil end, 10), "restored scene model catalog timed out")
  vim.api.nvim_set_current_win(state.transcript_win)
  local scene = { state = state, root = root, data = data, workspace = workspace }
  local function toggle_at(row, label)
    local revision = state.presentation.transcript.revision
    vim.api.nvim_win_set_cursor(state.transcript_win, { row + 1, 0 })
    assert(state.presentation.toggle_heading(state.transcript_win), "scene node did not toggle: " .. label)
    assert(vim.wait(5000, function() return state.presentation.transcript.revision ~= revision end, 10), "scene toggle timed out")
  end
  function scene.toggle(identity)
    local _, row = state.presentation.transcript.sequence:position(identity)
    assert(row, "scene node is not visible: " .. identity)
    toggle_at(row, identity)
  end
  -- Result rows render inside their scope's group block without a block identity of their own,
  -- so they are addressed by their unique heading text.
  function scene.toggle_row(text)
    local match
    for index, line in ipairs(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)) do
      if line:find(text, 1, true) then
        assert(not match, "scene row heading is ambiguous: " .. text)
        match = index - 1
      end
    end
    assert(match, "scene row is not visible: " .. text)
    toggle_at(match, text)
  end
  function scene.lines()
    return vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)
  end
  function scene.close()
    require("forge.views.harness.workspace").release(state)
    state.presentation.close()
    client.host_stop()
    assert(vim.wait(5000, function() return client._client.process == nil end, 10), "scene host was not collected")
    vim.fn.stdpath = original_stdpath
    vim.api.nvim_set_current_dir(root)
  end
  return scene
end

return M
