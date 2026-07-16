vim.loader.enable(false)

local diff_review = require("diff_review")

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
  return {
    id = "interaction",
    node_list = {
      {
        segment = {
          thought = { { text = "Inspecting", tool = tool_list } },
        },
      },
    },
  }
end

local ok, failure = pcall(function()
  diff_review.setup({ walkthrough_inventory = "sem", harness = { backend = "mock" } })
  local origin_win = vim.api.nvim_get_current_win()
  local picker = require("diff_review.views.picker")
  local picker_state = require("diff_review.views.picker.state")
  local agent_picker = require("diff_review.views.harness.agent_picker")
  local spawn_picker = require("diff_review.views.harness.spawn_picker")
  local agent_catalog = require("diff_review.views.harness.agent_catalog")
  local selected_run_id = nil
  local state = {
    selected_agent_run_id = "run-zeta",
    agent_live = {},
    agent = {
      run = {
        {
          id = "run-zeta",
          definition = "zeta",
          status = "running",
          created_at_ms = os.time() * 1000 - 10000,
          updated_at_ms = os.time() * 1000,
        },
        {
          id = "run-done",
          definition = "explorer",
          status = "completed",
          created_at_ms = 1000,
          updated_at_ms = 61000,
        },
        {
          id = "run-alpha",
          definition = "alpha",
          status = "waiting",
          created_at_ms = os.time() * 1000 - 5000,
          updated_at_ms = os.time() * 1000,
        },
      },
      turn = {
        {
          agent_run_id = "run-zeta",
          interaction = thought_with_tool({
            { id = "ok", status = "completed", output = "done" },
            { id = "failed", status = "failed", output = "denied" },
          }),
        },
      },
    },
  }

  local first_by_letter = agent_catalog.resolve(state.agent, "a")
  local first_by_number = agent_catalog.resolve(state.agent, "0")
  assert_equals(first_by_letter.id, "run-alpha", "letter aliases should sort running children alphabetically")
  assert_equals(first_by_number.id, "run-alpha", "numeric aliases should share the zero-based running order")
  local missing_done_alias = agent_catalog.resolve(state.agent, "c")
  assert_equals(missing_done_alias, nil, "running aliases should never include completed children")
  assert_equals(agent_catalog.resolve(state.agent, "explorer").id, "run-done",
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

  state.agent.run[1].status = "completed"
  state.agent.run[1].updated_at_ms = state.agent.run[1].created_at_ms + 12000
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

  local spawned_definition = nil
  local spawned_prompt = nil
  local definition_list = {
    { name = "explorer", description = "Repository explorer" },
    { name = "worker", description = "Implementation worker" },
  }
  assert_true(spawn_picker.open({
    host = { window_list = { origin_win }, control_win = origin_win },
    definition_list = definition_list,
    on_spawn = function(definition, prompt)
      spawned_definition = definition
      spawned_prompt = prompt
    end,
  }), "spawn picker should open its searchable definition list")
  active = picker._state_for_test()
  assert_true(active.search_win and vim.api.nvim_win_is_valid(active.search_win),
    "spawn definitions should expose a focused search input")
  vim.api.nvim_buf_set_lines(active.search_buf, 0, -1, false, { "work" })
  vim.api.nvim_exec_autocmds("TextChangedI", { buffer = active.search_buf })
  assert_true(vim.wait(500, function()
    return #picker_state.page(active.state, active.spec).option_list == 1
  end, 10), "spawn search should filter agent definitions by name")
  assert_equals(picker_state.page(active.state, active.spec).option_list[1].label, "worker",
    "spawn search should retain the matching definition")
  invoke("<CR>", "i", active.search_buf)
  active = picker._state_for_test()
  assert_true(active.input_win and vim.api.nvim_win_is_valid(active.input_win),
    "selecting a definition should open its attached task input")
  assert_equals(vim.api.nvim_get_current_win(), active.input_win,
    "spawn task input should receive focus before Insert mode starts")
  vim.api.nvim_buf_set_lines(active.input_buf, 0, -1, false, { "Implement the renderer" })
  invoke("<C-s>", "i", active.input_buf)
  assert_equals(spawned_definition, "worker", "task submission should preserve the filtered definition")
  assert_equals(spawned_prompt, "Implement the renderer", "task submission should preserve the prompt")
  assert_equals(vim.api.nvim_get_current_win(), origin_win, "task submission should restore Harness focus")

  assert_true(spawn_picker.open({
    host = { window_list = { origin_win }, control_win = origin_win },
    definition_list = definition_list,
    definition_name = "explorer",
    on_spawn = function() end,
  }), "an exact /spawn definition should open its task input directly")
  active = picker._state_for_test()
  assert_true(active.input_win and vim.api.nvim_win_is_valid(active.input_win),
    "direct definition selection should skip the search page")
  picker.close(true)
  assert_true(not spawn_picker.open({
    host = { window_list = { origin_win }, control_win = origin_win },
    definition_list = definition_list,
    definition_name = "missing",
    on_spawn = function() end,
  }), "unknown direct definitions should fail without opening a picker")
end)

if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
