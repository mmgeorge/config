local function assert_equals(actual, expected, message)
  if actual ~= expected then
    error((message or "values differ") .. ": expected " .. vim.inspect(expected) .. ", got " .. vim.inspect(actual))
  end
end

local function assert_true(value, message)
  if not value then error(message or "expected truthy value") end
end

local function row_matching(buffer, pattern)
  for index, line in ipairs(vim.api.nvim_buf_get_lines(buffer, 0, -1, false)) do
    if line:find(pattern, 1, true) then return index end
  end
  error("picker row not found: " .. pattern)
end

local function enter_mapping()
  local mapping = vim.fn.maparg("<CR>", "n", false, true)
  assert_true(mapping.callback ~= nil, "agent picker should bind Enter")
  mapping.callback()
end

local picker_options = {
  agent = {
    run = {
      { id = "run-1", definition = "explorer", status = "running" },
    },
    definition = {
      { name = "explorer", description = "Repository explorer" },
    },
  },
}

local ok, failure = pcall(function()
  local selected_run_id
  local spawned_definition
  local spawned_prompt
  picker_options.on_select = function(run_id) selected_run_id = run_id end
  picker_options.on_spawn = function(definition, prompt)
    spawned_definition = definition
    spawned_prompt = prompt
  end

  local agent_picker = require("diff_review.views.harness.agent_picker")
  local launcher_window = vim.api.nvim_get_current_win()
  local original_cmd = vim.cmd
  local stopped_insert = false
  vim.cmd = function(command)
    if command == "stopinsert" then stopped_insert = true end
    return original_cmd(command)
  end
  local opened, open_failure = pcall(agent_picker.open, picker_options)
  vim.cmd = original_cmd
  assert_true(opened, open_failure)
  assert_true(stopped_insert, "agent picker should leave Insert mode when it opens")
  local picker_buffer = vim.api.nvim_get_current_buf()
  local definition_row = row_matching(picker_buffer, "Repository explorer")
  vim.api.nvim_win_set_cursor(0, { definition_row, 1 })
  enter_mapping()

  local input_buffer = vim.api.nvim_get_current_buf()
  assert_equals(vim.bo[input_buffer].filetype, "diffreview-harness-agent-task",
    "Enter on an available definition should open its task input")
  vim.api.nvim_buf_set_lines(input_buffer, 0, -1, false, { "Inspect the renderer" })
  local submit_mapping = vim.fn.maparg("<C-s>", "i", false, true)
  assert_true(submit_mapping.callback ~= nil, "agent task input should bind Ctrl-s")
  submit_mapping.callback()
  assert_equals(spawned_definition, "explorer", "task submission should preserve the selected definition")
  assert_equals(spawned_prompt, "Inspect the renderer", "task submission should preserve the entered prompt")
  assert_equals(vim.api.nvim_get_current_win(), launcher_window,
    "task submission should return focus to the window that opened the picker")

  agent_picker.open(picker_options)
  picker_buffer = vim.api.nvim_get_current_buf()
  local running_row = row_matching(picker_buffer, "running")
  vim.api.nvim_win_set_cursor(0, { running_row, 1 })
  enter_mapping()
  assert_equals(selected_run_id, "run-1", "Enter on a running agent should select its timeline")
end)

if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
