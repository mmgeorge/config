vim.loader.enable(false)

local forge = require("forge")

local function assert_equals(actual, expected, message)
  if actual ~= expected then
    error((message or "values differ") .. ": expected " .. vim.inspect(expected) .. ", got " .. vim.inspect(actual), 2)
  end
end

local function assert_true(value, message)
  if not value then error(message or "expected truthy value", 2) end
end

local function invoke(key, mode, buf)
  local mapping = vim.api.nvim_buf_call(buf or 0, function()
    return vim.fn.maparg(key, mode or "n", false, true)
  end)
  assert_true(mapping.callback ~= nil, "missing mapping for " .. key)
  mapping.callback()
end

local function thought_with_tool(tool_list)
  local item, order = {}, {}
  for _, tool in ipairs(tool_list) do
    item[tool.id] = tool
    order[#order + 1] = tool.id
  end
  return {
    id = "interaction",
    state = "running",
    turn = { { state = { kind = "running" }, tool = { order = order, item = item } } },
  }
end

local ok, failure = pcall(function()
  forge.setup({ walkthrough_inventory = "sem", harness = { backend = "mock" } })
  local origin_win = vim.api.nvim_get_current_win()
  local picker = require("forge.views.picker")
  local picker_state = require("forge.views.picker.state")
  local agent_picker = require("forge.views.harness.agent_picker")
  local spawn_picker = require("forge.views.harness.spawn_picker")
  local agent_catalog = require("forge.views.harness.agent_catalog")
  local selected_run_id = nil
  local zeta_interaction = thought_with_tool({
    { id = "ok", status = "completed", output = "done" },
    { id = "failed", status = "failed", output = "denied" },
  })
  local done_exchange = { id = "done", agent_id = "run-done", state = "complete", turn = {} }
  local state = {
    selected_agent_run_id = "run-zeta",
    agent = {
      run = {
        { id = "root-agent", source = "root", definition = "primary", task_name = "Main", state = "ready" },
        {
          id = "run-zeta",
          definition = "zeta",
          state = "ready",
          created_at_ms = os.time() * 1000 - 10000,
          updated_at_ms = os.time() * 1000,
        },
        {
          id = "run-done",
          definition = "explorer",
          state = "ready",
          created_at_ms = 1000,
          updated_at_ms = 61000,
        },
        {
          id = "run-alpha",
          definition = "alpha",
          state = "starting",
          created_at_ms = os.time() * 1000 - 5000,
          updated_at_ms = os.time() * 1000,
        },
      },
      exchange = {},
      summary = {
        ["run-zeta"] = { state = "running", tool_count = 2, failed_count = 1 },
        ["run-done"] = { state = "complete", tool_count = 0, failed_count = 0 },
      },
    },
    timeline = {
      {
        kind = "exchange",
        id = "parent",
        exchange = { id = "parent" },
        agent_by_id = {
          ["run-zeta"] = {
            kind = "agent_lifecycle",
            run = { id = "run-zeta" },
            exchange = { zeta_interaction },
            agent = {},
          },
          ["run-done"] = {
            kind = "agent_lifecycle",
            run = { id = "run-done" },
            exchange = { done_exchange },
            agent = {},
          },
          ["run-alpha"] = {
            kind = "agent_lifecycle",
            run = { id = "run-alpha" },
            exchange = {},
            agent = {},
          },
        },
      },
    },
  }

  local first_by_letter = agent_catalog.resolve(state, "a")
  local first_by_number = agent_catalog.resolve(state, "0")
  assert_equals(first_by_letter.id, "run-alpha", "letter aliases should sort running children alphabetically")
  assert_equals(first_by_number.id, "run-alpha", "numeric aliases should share the zero-based running order")
  local missing_done_alias = agent_catalog.resolve(state, "c")
  assert_equals(missing_done_alias, nil, "running aliases should never include completed children")
  assert_equals(agent_catalog.resolve(state, "explorer").id, "run-done",
    "unique child labels should resolve ended timelines")

  agent_picker.open({
    host = { window_list = { origin_win }, control_win = origin_win },
    state_provider = function() return state end,
    on_select = function(run_id) selected_run_id = run_id end,
  })
  local active = picker._state_for_test()
  local page = picker_state.page(active.state, active.spec)
  assert_equals(vim.api.nvim_get_current_win(), active.win, "agent picker should own modal focus")
  assert_equals(page.option_list[1].label, "Main", "Main should remain the first timeline")
  assert_equals(page.option_list[2].label, "alpha", "running children should sort alphabetically")
  assert_equals(page.option_list[2].section, "Running", "active children should use the Running section")
  assert_equals(page.option_list[3].label, "zeta", "the second running child should follow alphabetically")
  assert_true(page.option_list[3].detail:find("Running for", 1, true) ~= nil,
    "running status should include live elapsed time")
  assert_true(page.option_list[3].detail:find("2 tools called (1 failed)", 1, true) ~= nil,
    "running status should include shared tool totals and failures")
  assert_equals(page.option_list[4].section, "Done", "terminal children should use the Done section")
  assert_true(page.option_list[4].detail:find("Completed in 60s", 1, true) ~= nil,
    "terminal status should use its durable duration")
  assert_equals(picker_state.selected_option(active.state, active.spec).value.run.id, "run-zeta",
    "the active child timeline should be selected initially")

  zeta_interaction.state = "complete"
  state.agent.summary["run-zeta"].state = "complete"
  zeta_interaction.turn[1].state = { kind = "finished", outcome = "completed" }
  state.agent.run[2].updated_at_ms = state.agent.run[2].created_at_ms + 12000
  agent_picker.refresh()
  active = picker._state_for_test()
  assert_equals(picker_state.selected_option(active.state, active.spec).value.run.id, "run-zeta",
    "live status refresh should preserve selection by run identity when sections change")
  invoke("<CR>", "n", active.buf)
  assert_equals(selected_run_id, "run-zeta", "confirming should select the current child timeline")
  assert_equals(vim.api.nvim_get_current_win(), origin_win, "timeline selection should restore Harness focus")

  agent_picker.open({
    host = { window_list = { origin_win }, control_win = origin_win },
    state_provider = function() return state end,
    on_select = function(run_id) selected_run_id = run_id end,
  })
  active = picker._state_for_test()
  active.state.selected_index_by_page.agents = 1
  invoke("<CR>", "n", active.buf)
  assert_equals(selected_run_id, nil, "Main should clear the selected child timeline")

  local spawned_request
  local document = { default_profile = "reviewer", profiles = {
    reviewer = { enabled = true, description = "Review code" },
    disabled = { enabled = false, description = "Hidden" },
  } }
  assert_true(spawn_picker.open({
    host = { window_list = { origin_win }, control_win = origin_win }, document = document,
    on_spawn = function(request) spawned_request = request end,
  }), "profile picker should open")
  active = picker._state_for_test()
  page = picker_state.page(active.state, active.spec)
  assert_equals(page.id, "spawn-profile")
  assert_equals(#page.option_list, 1, "disabled profiles must be hidden")
  assert_equals(page.option_list[1].value, "reviewer")
  invoke("<CR>", "n", active.buf)
  active = picker._state_for_test()
  assert_equals(picker_state.page(active.state, active.spec).id, "spawn-task")
  vim.api.nvim_buf_set_lines(active.input_buf, 0, -1, false, { "Inspect the renderer" })
  invoke("<C-s>", "i", active.input_buf)
  assert_equals(spawned_request.profile, "reviewer")
  assert_equals(spawned_request.text, "Inspect the renderer")
  assert_equals(spawned_request.provider, nil, "task composer must not choose provider")
  assert_equals(spawned_request.model, nil, "task composer must not choose model")
  assert_equals(vim.api.nvim_get_current_win(), origin_win)
  assert_true(spawn_picker.open({
    host = { window_list = { origin_win }, control_win = origin_win }, document = document, profile = "reviewer",
    on_spawn = function() end,
  }), "explicit profile should skip profile selection")
  active = picker._state_for_test()
  assert_equals(picker_state.page(active.state, active.spec).id, "spawn-task")
  picker.close(true)

end)

if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
