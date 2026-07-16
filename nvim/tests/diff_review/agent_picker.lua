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

local function invoke(key, mode)
  local mapping = vim.fn.maparg(key, mode or "n", false, true)
  assert_true(mapping.callback ~= nil, "missing mapping for " .. key)
  mapping.callback()
end

local ok, failure = pcall(function()
  diff_review.setup({ walkthrough_inventory = "sem", harness = { backend = "mock" } })
  local selected_run_id = nil
  local spawned_definition = nil
  local spawned_prompt = nil
  local origin_win = vim.api.nvim_get_current_win()
  local agent_picker = require("diff_review.views.harness.agent_picker")
  local picker = require("diff_review.views.picker")
  local options = {
    host = { window_list = { origin_win }, control_win = origin_win },
    agent = {
      run = { { id = "run-1", definition = "explorer", status = "running" } },
      definition = { { name = "explorer", description = "Repository explorer" } },
    },
    on_select = function(run_id) selected_run_id = run_id end,
    on_spawn = function(definition, prompt)
      spawned_definition = definition
      spawned_prompt = prompt
    end,
  }

  agent_picker.open(options)
  local active = picker._state_for_test()
  assert_equals(vim.api.nvim_get_current_win(), active.win, "agent picker should own modal focus")
  local available_index = nil
  for index, entry in ipairs(active.spec.page_list[1].option_list) do
    if entry.value.kind == "definition" then available_index = index end
  end
  active.state.selected_index_by_page.agents = available_index
  invoke("<CR>")
  active = picker._state_for_test()
  assert_true(vim.api.nvim_win_is_valid(active.input_win), "available agent should open an attached task input")
  assert_equals(vim.api.nvim_get_mode().mode:sub(1, 1), "n", "agent task input should open in Normal mode")
  vim.api.nvim_buf_set_lines(active.input_buf, 0, -1, false, { "Inspect the renderer" })
  invoke("<C-s>", "i")
  assert_equals(spawned_definition, "explorer", "task submission should preserve the definition")
  assert_equals(spawned_prompt, "Inspect the renderer", "task submission should preserve the prompt")
  assert_equals(vim.api.nvim_get_current_win(), origin_win, "task submission should restore Harness focus")

  agent_picker.open(options)
  active = picker._state_for_test()
  local running_index = nil
  for index, entry in ipairs(active.spec.page_list[1].option_list) do
    if entry.value.run and entry.value.run.id == "run-1" then running_index = index end
  end
  active.state.selected_index_by_page.agents = running_index
  invoke("<CR>")
  assert_equals(selected_run_id, "run-1", "running agent should select its timeline")
end)

if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
