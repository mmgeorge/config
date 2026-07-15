vim.loader.enable(false)

local choice_flow = require("diff_review.views.harness.choice_flow")

local function assert_equals(actual, expected, message)
  if not vim.deep_equal(actual, expected) then
    error((message or "values differ") .. "\nexpected: " .. vim.inspect(expected) .. "\nactual: " .. vim.inspect(actual))
  end
end

local parent_win = vim.api.nvim_get_current_win()
local selected
local closed = false
choice_flow.open({
  parent_win = parent_win,
  title = "Approval requested",
  page_list = {
    {
      prompt = "Run command",
      detail = "git status --short",
      option_list = {
        { key = "n", value = "once", label = "Allow once" },
        { key = "e", value = "always", label = "Always allow exact" },
      },
    },
  },
  on_select = function(option, _, close)
    selected = option.value
    close()
  end,
  on_close = function() closed = true end,
})

local state = choice_flow._state_for_test()
assert_equals(vim.api.nvim_win_get_config(state.win).focusable, false, "choice float should not take editor focus")
assert_equals(vim.api.nvim_get_current_win(), parent_win, "choice float should preserve parent focus")
vim.api.nvim_feedkeys("e", "x", false)
assert_equals(choice_flow._state_for_test().selected_index, 2, "jump key should select without confirming")
assert_equals(selected, nil, "jump key should not submit")
vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<CR>", true, false, true), "x", false)
assert_equals(selected, "always", "Enter should submit the selected option")
assert_equals(choice_flow.is_open(), false, "successful selection should close the float")

choice_flow.open({
  parent_win = parent_win,
  title = "Approval requested",
  page_list = { { prompt = "Run command", option_list = { { key = "n", value = "once", label = "Allow once" } } } },
  on_select = function() end,
  on_close = function() closed = true end,
})
closed = false
vim.api.nvim_feedkeys("q", "x", false)
assert_equals(closed, true, "q should hide the float without resolving its request")
assert_equals(choice_flow.is_open(), false, "q should close only the presentation")

print("Harness ChoiceFlow tests passed")
vim.cmd("qa!")
