-- Unit tests for `forge.views.harness.provider_picker` (provider_picker.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
local mock_client = dofile("nvim/lua/forge/tests/support/mock_client.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["provider_catalog"] = function()
  require("forge").setup()

  dofile("nvim/lua/forge/tests/support/provider_catalog.lua").install()
  local picker = require("forge.views.picker")
  local picker_state = require("forge.views.picker.state")
  local provider_catalog = require("forge.views.harness.provider_catalog")
  local provider_picker = require("forge.views.harness.provider_picker")
  local session = require("forge.session")

  local skill_enabled = { walkthrough = true, imagegen = false }
  local mcp_enabled = false
  local workspace_mcp_enabled = false

  local catalog_requests = mock_client.respond({
    ["session.provider.skill.list"] = function(_, callback)
      callback({
        {
          name = "walkthrough",
          resource_id = "import:workspace-walkthrough",
          scope = "workspace",
          path = "D:/repo/.rulesync/skills/walkthrough/SKILL.md",
          description = "Build a review walkthrough",
          enabled = skill_enabled.walkthrough,
          user_invocable = true,
        },
        {
          name = "imagegen",
          scope = "global",
          path = "D:/config/rulesync-global/.rulesync/skills/imagegen/SKILL.md",
          description = "Generate an image",
          enabled = skill_enabled.imagegen,
          user_invocable = true,
        },
        {
          name = "internal-helper",
          description = "Provider internal skill",
          enabled = true,
          user_invocable = false,
        },
      })
    end,
    ["settings.skill.set_enabled"] = function(request_specification, callback)
      local params = request_specification.params or {}
      assert(params.scope == "workspace" and params.resource_id == "import:workspace-walkthrough",
        "skill toggle must preserve the selected resource identity and workspace scope")
      skill_enabled[params.name] = params.enabled
      callback({ name = params.name, enabled = params.enabled, restart_required = false })
    end,
    ["session.provider.mcp.list"] = function(_, callback)
      callback({
        {
          name = "github-mcp",
          resource_id = "import:global-github",
          scope = "global",
          source_path = "D:/config/rulesync-global/.rulesync/mcp.json",
          transport = "stdio",
          enabled = mcp_enabled,
          status = mcp_enabled and "connected" or "disabled",
          token_count = 704,
          token_estimated = true,
          tools = {
            { name = "actions_list" },
            { name = "user_get" },
          },
        },
        {
          name = "github-mcp",
          resource_id = "import:workspace-github",
          scope = "workspace",
          transport = "stdio",
          enabled = workspace_mcp_enabled,
          status = workspace_mcp_enabled and "connected" or "disabled",
        },
      })
    end,
    ["settings.mcp.set_enabled"] = function(request_specification, callback)
      local params = request_specification.params or {}
      if params.resource_id == "import:workspace-github" then
        assert(params.scope == "workspace", "workspace MCP toggle must write its displayed scope")
        workspace_mcp_enabled = params.enabled
      else
        assert(params.scope == "global" and params.resource_id == "import:global-github",
          "global MCP toggle must preserve the selected resource identity and scope")
        mcp_enabled = params.enabled
      end
      callback({ name = params.name, enabled = params.enabled, restart_required = false })
    end,
  })

  session.harness.session = {
    id = "provider-catalog-test",
    provider = "fixture_alpha",
    workspace = "D:/repo",
  }
  session.harness.capability = {
    catalog = { skill = true, mcp = true, live_mcp_mutation = false },
  }
  provider_catalog.clear()

  local input_buf = buffer.scratch()
  vim.api.nvim_win_set_buf(0, input_buf)
  buffer.set(input_buf, { "$wa" })
  vim.api.nvim_win_set_cursor(0, { 1, 3 })
  local command_source = require("forge.views.harness.completion.command_source").new()
  assert(command_source:enabled(), "$ completion should activate for the first token")
  local completion
  command_source:get_completions({}, function(result) completion = result end)
  assert(completion ~= nil, "$ completion should return provider skills")
  assert(#completion.items == 1, "$ completion should hide disabled skills")
  assert(completion.items[1].label == "$walkthrough", "$ completion should label the enabled skill")
  assert(completion.items[1].textEdit.newText == "walkthrough ", "$ completion should preserve the leading $")

  buffer.set(input_buf, { "use $wa" })
  vim.api.nvim_win_set_cursor(0, { 1, 7 })
  assert(not command_source:enabled(), "$ completion should not activate after ordinary text")
  buffer.set(input_buf, { "$walkthrough explain" })
  vim.api.nvim_win_set_cursor(0, { 1, 20 })
  assert(not command_source:enabled(), "$ completion should stop after the skill selector")

  local host = { window_list = { vim.api.nvim_get_current_win() }, control_win = vim.api.nvim_get_current_win() }
  local inserted_text
  provider_picker.open_skills({
    host = host,
    on_insert = function(text) inserted_text = text end,
  })
  local skill_picker = picker._state_for_test()
  assert(skill_picker ~= nil, "/skills should open the shared picker")
  local skill_page = picker_state.page(skill_picker.state, skill_picker.spec)
  assert(skill_page.option_list[1].label == "✓ walkthrough", "enabled skills should use the check icon")
  assert(skill_page.option_list[2].label == "× imagegen", "disabled skills should use the cross icon")
  assert(#skill_page.option_list == 2, "/skills should hide provider-internal skills")
  assert(vim.deep_equal(skill_page.column_headers, { "Skill", "Source", "Description" }),
    "skills should label source separately from description")
  assert(skill_page.option_list[1].columns[2] == "Workspace" and skill_page.option_list[2].columns[2] == "Global",
    "skills should preserve both source scopes")
  skill_picker.spec.action_list[2].callback({ option = skill_page.option_list[1] })
  skill_picker = picker._state_for_test()
  assert(vim.iter(skill_picker.frame.lines):any(function(line)
    return line:find("D:/repo/.rulesync/skills/walkthrough/SKILL.md", 1, true)
  end), "Tab should reveal the skill's source definition")
  skill_picker.spec.on_confirm({ option = skill_page.option_list[1] })
  assert(inserted_text == "$walkthrough ", "Enter should insert an enabled skill selector")

  skill_picker.spec.action_list[1].callback({ option = skill_page.option_list[1] })
  skill_picker = picker._state_for_test()
  skill_page = picker_state.page(skill_picker.state, skill_picker.spec)
  assert(skill_page.option_list[1].label == "× walkthrough", "Space should disable an enabled skill")
  inserted_text = nil
  assert(skill_picker.spec.on_confirm({ option = skill_page.option_list[1] }) == false,
    "Enter should reject a disabled skill")
  assert(inserted_text == nil, "disabled skills should not enter the composer")
  buffer.set(input_buf, { "$wa" })
  vim.api.nvim_win_set_cursor(0, { 1, 3 })
  completion = nil
  command_source:get_completions({}, function(result) completion = result end)
  assert(completion ~= nil and #completion.items == 0,
    "disabling a skill should remove it from $ completion")
  picker.close(false)

  local mutation
  provider_picker.open_mcp({
    host = host,
    on_mutation = function(result) mutation = result end,
  })
  local mcp_picker = picker._state_for_test()
  assert(mcp_picker ~= nil, "/mcp should open the shared picker")
  local mcp_page = picker_state.page(mcp_picker.state, mcp_picker.spec)
  assert(vim.deep_equal(mcp_page.column_headers, { "Server", "Source", "Transport", "Tokens", "Status" }),
    "MCP picker should label each structured column")
  assert(mcp_page.option_list[1].columns[2] == "Global"
      and mcp_page.option_list[1].columns[3] == "stdio"
      and mcp_page.option_list[1].columns[4] == "~704 tokens",
    "MCP transport and token values should occupy their labeled columns")
  assert(mcp_page.option_list[1].label == "○ github-mcp", "disabled MCPs should use the empty-circle icon")
  assert(mcp_page.option_list[1].detail:find("stdio", 1, true) ~= nil, "MCP rows should show transport")
  assert(mcp_page.option_list[1].detail:find("~704 tokens", 1, true) ~= nil,
    "MCP rows should distinguish estimated token counts")

  mcp_picker.spec.action_list[2].callback({ option = mcp_page.option_list[1] })
  mcp_picker = picker._state_for_test()
  assert(vim.iter(mcp_picker.frame.lines):any(function(line)
    return line:find("Tools: actions_list, user_get", 1, true) ~= nil
  end), "Tab should unfold cached tools into real picker lines")
  assert(vim.iter(mcp_picker.frame.lines):any(function(line)
    return line:find("D:/config/rulesync-global/.rulesync/mcp.json", 1, true)
  end), "MCP details should reveal the source configuration")

  mcp_page = picker_state.page(mcp_picker.state, mcp_picker.spec)
  mcp_picker.spec.action_list[1].callback({ option = mcp_page.option_list[1] })
  assert(mutation and mutation.enabled, "Space should report the completed MCP enable mutation")
  mcp_picker = picker._state_for_test()
  mcp_page = picker_state.page(mcp_picker.state, mcp_picker.spec)
  assert(mcp_page.option_list[1].label == "✓ github-mcp", "connected MCPs should use the check icon")
  assert(vim.tbl_contains(vim.tbl_map(function(request) return request.spec.method end, catalog_requests),
    "settings.mcp.set_enabled"),
    "MCP toggles should use the normalized broker operation")
  assert(mcp_page.option_list[2].columns[2] == "Workspace", "workspace MCP must display its source scope")
  mcp_picker.spec.action_list[1].callback({ option = mcp_page.option_list[2] })
  mcp_picker = picker._state_for_test()
  mcp_page = picker_state.page(mcp_picker.state, mcp_picker.spec)
  assert(mcp_page.option_list[2].label == "✓ github-mcp" and workspace_mcp_enabled and mcp_enabled,
    "workspace MCP toggle must enable that resource while preserving the same-name global resource")

  picker.close(false)
  provider_catalog.clear()
  local initial_requests = mock_client.record()
  provider_picker.open_mcp({ host = host })
  mcp_picker = picker._state_for_test()
  assert(mcp_picker.frame.lines[1] ~= nil, "MCP picker must open before inventory arrives")
  assert(vim.iter(mcp_picker.frame.lines):any(function(line) return line:find("Loading MCP", 1, true) end),
    "pending catalog should show loading feedback")
  picker.close(false)
  initial_requests[1].callback({ { name = "late", enabled = true, status = "connected" } })
  assert(picker._state_for_test() == nil, "closed pending picker must not reopen after inventory arrives")
  provider_picker.open_mcp({ host = host })
  initial_requests[2].callback(nil, "simulated initial failure")
  mcp_picker = picker._state_for_test()
  assert(vim.iter(mcp_picker.frame.lines):any(function(line) return line:find("simulated initial failure", 1, true) end),
    "catalog failures must remain visible in the picker")
  picker.close(false)
  -- The picker polls MCP status from a repeating `vim.uv` timer. A mock timer lets the fixture
  -- advance ticks deterministically instead of waiting for the real interval.
  local active_timer_set = {}
  vim.uv.new_timer = function()
    local timer = {}
    function timer:start(_, _, callback) self.callback = callback active_timer_set[self] = true end
    function timer:stop() active_timer_set[self] = nil end
    function timer:close() active_timer_set[self] = nil end
    function timer:is_active() return active_timer_set[self] == true end
    return timer
  end
  local function flush_scheduled()
    local flushed = false
    vim.schedule(function() flushed = true end)
    wait_until(function() return flushed end, "scheduled callbacks did not run")
  end
  local function advance_ticks(count)
    for _ = 1, count do
      local timer_list = vim.tbl_keys(active_timer_set)
      for _, timer in ipairs(timer_list) do
        if active_timer_set[timer] then timer.callback() end
      end
      flush_scheduled()
    end
  end
  -- Advance at most two refresh intervals; the picker refreshes every 17 ticks.
  local function advance_until(predicate, message)
    for _ = 1, 34 do
      if predicate() then return end
      advance_ticks(1)
    end
    assert(predicate(), message)
  end
  local catalog_reads = 0
  local pending_refresh
  mock_client.respond({
    ["session.provider.mcp.list"] = function(_, callback)
      catalog_reads = catalog_reads + 1
      if catalog_reads == 1 then
        callback({ { name = "starting-server", enabled = true, status = "loading" } })
      else
        pending_refresh = callback
      end
    end,
  })
  provider_picker.open_mcp({ host = host })
  advance_until(function() return pending_refresh ~= nil end,
    "starting servers should refresh their status without reopening the picker")
  advance_ticks(34)
  assert(catalog_reads == 2, "status refreshes must not overlap")
  pending_refresh({ { name = "starting-server", enabled = true, status = "connected" } })
  mcp_picker = picker._state_for_test()
  mcp_page = picker_state.page(mcp_picker.state, mcp_picker.spec)
  assert(mcp_page.option_list[1].label == "✓ starting-server", "refresh should replace starting status")
  pending_refresh = nil
  advance_until(function() return pending_refresh ~= nil end,
    "connected servers must keep refreshing without returning to starting")
  mcp_picker = picker._state_for_test()
  mcp_page = picker_state.page(mcp_picker.state, mcp_picker.spec)
  assert(mcp_page.option_list[1].label == "✓ starting-server", "pending refresh must preserve connected status")
  pending_refresh({ { name = "starting-server", enabled = true, status = "failed", status_detail = "connection closed" } })
  mcp_picker = picker._state_for_test()
  mcp_page = picker_state.page(mcp_picker.state, mcp_picker.spec)
  assert(mcp_page.option_list[1].detail:find("connection closed", 1, true),
    "the open picker must observe later connection failures")
  assert(mcp_page.option_list[1].columns[5] == "failed: connection closed",
    "the Status column must explain why a server failed")
  pending_refresh = nil
  advance_until(function() return pending_refresh ~= nil end, "failed rows must remain refreshable")
  pending_refresh({ { name = "starting-server", enabled = true, status = "unavailable",
    status_detail = "missing environment variable FIXTURE_KEY", transport = "stdio" } })
  mcp_picker = picker._state_for_test()
  mcp_page = picker_state.page(mcp_picker.state, mcp_picker.spec)
  assert(mcp_page.option_list[1].columns[5] == "status unavailable: missing environment variable FIXTURE_KEY",
    "the Status column must retain the unavailable reason")
  mcp_picker.spec.action_list[2].callback({ option = mcp_page.option_list[1] })
  assert(vim.iter(picker._state_for_test().frame.lines):any(function(line)
    return line:find("missing environment variable FIXTURE_KEY", 1, true) ~= nil
  end), "expanded MCP details must preserve the full failure reason")
  picker.close(false)
  catalog_reads = 0
  pending_refresh = nil
  provider_picker.open_mcp({ host = host })
  advance_until(function() return pending_refresh ~= nil end, "second picker should poll")
  picker.close(false)
  local old_refresh = pending_refresh
  mock_client.respond({
    ["session.provider.mcp.list"] = function(_, callback)
      callback({ { name = "replacement", enabled = true, status = "connected" } })
    end,
  })
  provider_picker.open_mcp({ host = host })
  old_refresh({ { name = "obsolete", enabled = true, status = "loading" } })
  mcp_picker = picker._state_for_test()
  mcp_page = picker_state.page(mcp_picker.state, mcp_picker.spec)
  assert(mcp_page.option_list[1].id == "replacement", "closed picker responses must not replace a newer picker")
  picker.close(false)
  mutation = nil
  local fail_refresh = false
  mock_client.respond({
    ["settings.mcp.set_enabled"] = function(_, callback)
      fail_refresh = true
      callback({ name = "server", enabled = true, restart_required = true })
    end,
    ["session.provider.mcp.list"] = function(_, callback)
      if fail_refresh then
        callback(nil, "simulated refresh failure")
      else
        callback({ { name = "server", enabled = false, status = "disabled" } })
      end
    end,
  })
  provider_picker.open_mcp({ host = host, on_mutation = function(result) mutation = result end })
  mcp_picker = picker._state_for_test()
  mcp_page = picker_state.page(mcp_picker.state, mcp_picker.spec)
  mcp_picker.spec.action_list[1].callback({ option = mcp_page.option_list[1] })
  assert(mutation and mutation.restart_required,
    "successful mutation must reach the restart coordinator even if status refresh fails")
  picker.close(false)
end

return T
