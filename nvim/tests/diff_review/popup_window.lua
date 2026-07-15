local function assert_equals(actual, expected, message)
  if actual ~= expected then
    error((message or "values differ") .. ": expected " .. vim.inspect(expected) .. ", got " .. vim.inspect(actual))
  end
end

local function assert_true(value, message)
  if not value then error(message or "expected truthy value") end
end

local function contains(item_list, expected)
  return vim.iter(item_list):any(function(item) return item == expected end)
end

local ok, failure = pcall(function()
  local popup_window = require("diff_review.infra.popup_window")
  local origin_win = vim.api.nvim_get_current_win()
  local original_cmd = vim.cmd
  local original_mode = vim.fn.mode
  local command_list = {}
  vim.cmd = function(command)
    command_list[#command_list + 1] = command
    return original_cmd(command)
  end
  vim.fn.mode = function() return "i" end

  local _, popup_win = popup_window.open({
    relative = "editor",
    width = 36,
    height = 4,
    title = "Mode test",
    filetype = "DiffReviewPopupTest",
  })
  assert_equals(vim.api.nvim_get_current_win(), popup_win, "opening a popup should focus its window")
  assert_true(contains(command_list, "stopinsert"), "opening a popup should leave Insert mode")

  popup_window.close(popup_win)
  assert_equals(vim.api.nvim_get_current_win(), origin_win, "closing a popup should restore its origin window")
  assert_true(contains(command_list, "startinsert"), "closing a popup should restore its origin Insert mode")

  vim.cmd = original_cmd
  vim.fn.mode = original_mode
end)

if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
