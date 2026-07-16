vim.loader.enable(false)

local diff_review = require("diff_review")

local function assert_true(value, message)
  if not value then error(message or "expected truthy value", 2) end
end

local function assert_equals(actual, expected, message)
  if not vim.deep_equal(actual, expected) then
    error((message or "values differ") .. "\nexpected: " .. vim.inspect(expected) .. "\nactual: " .. vim.inspect(actual), 2)
  end
end

local function invoke(key, mode)
  local mapping = vim.fn.maparg(key, mode or "n", false, true)
  assert_true(mapping.callback ~= nil, "missing mapping for " .. key)
  mapping.callback()
end

local ok, failure = pcall(function()
  diff_review.setup({ walkthrough_inventory = "sem", harness = { backend = "mock" } })
  local layout = require("diff_review.views.picker.layout")
  local picker_input = require("diff_review.views.picker.input")
  local picker_state = require("diff_review.views.picker.state")
  local picker = require("diff_review.views.picker")

  local spec = {
    page_list = {
      { id = "one", option_list = { { label = "One" }, { label = "Two" } } },
      { id = "two", option_list = { { label = "Three" } } },
    },
  }
  local pure_state = picker_state.new(spec)
  local duplicate_ok = pcall(picker_state.new, {
    page_list = { { id = "same" }, { id = "same" } },
  })
  assert_true(not duplicate_ok, "picker state should reject duplicate page identities")
  picker_state.move(pure_state, spec, -1)
  assert_equals(picker_state.selected_index(pure_state, spec), 2, "selection should wrap")
  picker_state.move_page(pure_state, spec, 1)
  assert_equals(pure_state.page_index, 2, "page movement should clamp")
  picker_state.move_page(pure_state, spec, 1)
  assert_equals(pure_state.page_index, 2, "page movement should stay in range")

  local multiple_spec = {
    page_list = { {
      id = "many",
      selection_mode = "multiple",
      option_list = { { id = "one", label = "One" }, { id = "two", label = "Two" } },
    } },
  }
  local multiple_state = picker_state.new(multiple_spec)
  picker_state.toggle_selected(multiple_state, multiple_spec)
  picker_state.move(multiple_state, multiple_spec, 1)
  picker_state.toggle_selected(multiple_state, multiple_spec)
  assert_equals(#picker_state.selected_option_list(multiple_state, multiple_spec), 2,
    "multiple selection should retain independent choices")

  local stacked = layout.build({
    option_list = { { key = "n", label = "A long option label", detail = "A long detail that cannot share a narrow row" } },
  }, 1, 24)
  assert_true(#stacked.lines >= 4, "narrow layouts should stack option details")

  local input_page = {
    option_list = { { key = "n", label = "Choice" } },
    allow_input = true,
    input_height = 3,
  }
  local compact_input_frame = layout.build(input_page, 1, 80)
  local expanded_input_frame = layout.build(input_page, 1, 80, { input_visible = true })
  assert_equals(compact_input_frame.input_start, nil, "input capability should not reserve inactive rows")
  assert_true(expanded_input_frame.input_start ~= nil, "visible input should reserve attached editor rows")
  assert_true(#expanded_input_frame.lines > #compact_input_frame.lines,
    "visible input should expand the picker from its natural height")

  local origin_win = vim.api.nvim_get_current_win()
  vim.cmd("belowright 3split")
  local composer_win = vim.api.nvim_get_current_win()
  vim.wo[composer_win].winhighlight = "Normal:Normal"
  local composer_winhighlight = vim.wo[composer_win].winhighlight
  vim.o.guicursor = ""
  local restored_guicursor = "a:block-Cursor"
  local selected = nil
  local selected_result = nil
  picker.open({
    host = { window_list = { origin_win, composer_win }, control_win = composer_win },
    page_list = {
      {
        id = "choice",
        title = "Select model",
        subtitle = "Choose one model.",
        option_list = {
          { key = "n", label = "Sol", detail = "Fast coding model", value = "sol" },
          { key = "e", label = "Terra", detail = "Balanced coding model", value = "terra" },
        },
        allow_input = true,
        input_height = 3,
      },
      {
        id = "second",
        title = "Second page",
        option_list = { { key = "n", label = "Continue", value = "continue" } },
      },
    },
    on_confirm = function(result)
      selected = result.option.value
      selected_result = result
    end,
  })
  local active = picker._state_for_test()
  assert_equals(vim.api.nvim_get_current_win(), active.win, "presentation picker should own modal focus")
  assert_equals(vim.o.guicursor, "n:block-DiffReviewHiddenCursor",
    "opening the picker should globally hide its focused cursor")
  assert_equals(vim.api.nvim_win_get_config(active.win).focusable, true, "presentation picker should accept focus")
  local bounds = layout.host_bounds({ origin_win, composer_win })
  local popup_config = vim.api.nvim_win_get_config(active.win)
  assert_equals(popup_config.col, bounds.left, "picker should align with the host left edge")
  assert_equals(popup_config.row, bounds.bottom - popup_config.height - 2, "picker should anchor to the host bottom")
  invoke("e")
  assert_equals(active.state.selected_index_by_page.choice, 2, "jump keys should select without confirming")
  assert_equals(selected, nil, "jump keys should not submit")
  local parent_row = popup_config.row
  local compact_height = popup_config.height
  invoke("<Tab>")
  assert_true(vim.api.nvim_win_is_valid(active.input_win), "Tab should open attached feedback input")
  local expanded_config = vim.api.nvim_win_get_config(active.win)
  assert_true(expanded_config.height > compact_height, "Tab should grow the picker only when feedback opens")
  assert_true(expanded_config.row < parent_row, "an expanded bottom-anchored picker should grow upward")
  assert_equals(expanded_config.row + expanded_config.height, parent_row + compact_height,
    "feedback expansion should preserve the picker's bottom edge")
  local input_border = vim.api.nvim_win_get_config(active.input_win).border
  assert_true(input_border == "none" or vim.iter(input_border):all(function(border) return border == "" end),
    "attached feedback input should not draw a nested outline")
  assert_equals(vim.wo[active.input_win].winbar, "", "borderless feedback input should not display a title")
  assert_equals(vim.wo[active.input_win].foldcolumn, "2",
    "attached feedback input should reserve the shared prompt gutter")
  local input_prompt_gutter = vim.api.nvim_eval_statusline(vim.wo[active.input_win].statuscolumn, {
    winid = active.input_win,
    use_statuscol_lnum = 1,
  })
  assert_equals(input_prompt_gutter.str, "❯ ", "feedback input should render its prompt in the gutter")
  assert_equals(#vim.api.nvim_buf_get_extmarks(active.input_buf, -1, 0, -1, { details = true }), 0,
    "feedback input should not shift buffer text with inline virtual text")
  assert_true(active.frame.lines[active.frame.footer_line]:find("<Tab> options", 1, true) ~= nil,
    "open input help should expose Tab option focus")
  assert_true(active.frame.lines[active.frame.footer_line]:find("<C-c> clear", 1, true) ~= nil,
    "open input help should expose Ctrl-c clearing")
  assert_equals(vim.api.nvim_get_current_win(), active.input_win,
    "opening attached input should transfer focus into its editor")
  vim.api.nvim_buf_set_lines(active.input_buf, 0, -1, false, { "Use this for refactors", "and reviews" })
  local retained_input_win = active.input_win
  invoke("<Tab>", "i")
  assert_equals(vim.api.nvim_get_current_win(), active.win, "Tab should return focus to the picker")
  assert_equals(vim.o.guicursor, "n:block-DiffReviewHiddenCursor",
    "Tab should globally hide the cursor while picker controls own focus")
  assert_true(vim.api.nvim_win_is_valid(retained_input_win), "Tab should keep attached input open")
  invoke("n")
  assert_equals(active.state.selected_index_by_page.choice, 1,
    "option navigation should remain available while attached input stays open")
  assert_equals(picker_input.text(active.input_buf), "Use this for refactors\nand reviews",
    "changing options should retain feedback for the newly selected option")
  invoke("e")
  invoke("<Tab>")
  assert_equals(vim.api.nvim_get_current_win(), active.input_win,
    "Tab on picker controls should refocus the existing input")
  assert_equals(vim.o.guicursor, restored_guicursor,
    "refocusing attached input should restore an explicit visible cursor")
  invoke("<Tab>", "i")
  invoke("<Right>")
  assert_true(vim.api.nvim_win_get_config(active.win).height < expanded_config.height,
    "leaving feedback should shrink the picker to the destination page's natural height")
  assert_equals(active.frame.input_start, nil, "leaving feedback should release attached editor rows")
  assert_equals(active.state.page_index, 2, "Right should move to the next page")
  invoke("<Left>")
  invoke("<Tab>")
  assert_equals(vim.api.nvim_buf_get_lines(active.input_buf, 0, -1, false)[2], "and reviews",
    "page navigation should preserve multiline drafts")
  invoke("go")
  invoke("go")
  assert_equals(vim.api.nvim_get_current_win(), active.input_win, "go should return to attached input")
  assert_equals(vim.api.nvim_get_mode().mode:sub(1, 1), "n",
    "returning to attached input with go should preserve Normal mode")
  invoke("<C-s>", "i")
  assert_equals(selected, "terra", "input submission should preserve the selected option")
  assert_equals(selected_result.text, "Use this for refactors\nand reviews",
    "input submission should attach retained feedback to the selected option")
  assert_true(not picker.is_open(), "successful selection should close the picker")

  picker.open({
    host = { window_list = { origin_win, composer_win }, control_win = composer_win },
    page_list = { {
      id = "clear",
      title = "Clear feedback",
      option_list = { { key = "n", label = "Choice", value = "choice" } },
      allow_input = true,
      input_height = 3,
    } },
  })
  invoke("<Tab>")
  local clear_instance = picker._state_for_test()
  local cleared_input_buf = clear_instance.input_buf
  local expanded_clear_height = vim.api.nvim_win_get_config(clear_instance.win).height
  vim.api.nvim_buf_set_lines(cleared_input_buf, 0, -1, false, { "discard this feedback" })
  invoke("<C-c>", "i")
  assert_equals(clear_instance.input_buf, nil, "Ctrl-c should release picker input state")
  assert_true(not vim.api.nvim_buf_is_valid(cleared_input_buf), "Ctrl-c should destroy the picker input buffer")
  assert_equals(clear_instance.state.draft_by_page.clear, "", "Ctrl-c should clear the page feedback draft")
  assert_equals(vim.api.nvim_get_current_win(), clear_instance.win, "Ctrl-c should restore picker focus")
  assert_true(vim.api.nvim_win_get_config(clear_instance.win).height < expanded_clear_height,
    "Ctrl-c should shrink the picker after closing input")
  assert_true(picker.is_open(), "Ctrl-c should keep the picker itself open")
  assert_equals(vim.o.guicursor, "n:block-DiffReviewHiddenCursor",
    "Ctrl-c should hide the cursor after returning to picker controls")
  picker.close(false)
  assert_equals(vim.wo[composer_win].winhighlight, composer_winhighlight,
    "closing the picker should restore control highlights")
  assert_equals(vim.o.guicursor, restored_guicursor, "closing the picker should restore a visible cursor")

  local closed = false
  picker.open({
    host = { window_list = { origin_win, composer_win }, control_win = composer_win },
    page_list = { { id = "close", title = "Close", option_list = { { key = "n", label = "One" } } } },
    on_close = function() closed = true end,
  })
  invoke("q")
  assert_true(closed, "q should close without resolving")
  assert_true(not picker.is_open(), "q should remove the picker")

  local multiple_result = nil
  picker.open({
    host = { window_list = { origin_win, composer_win }, control_win = composer_win },
    page_list = { {
      id = "multiple",
      title = "Select checks",
      selection_mode = "multiple",
      option_list = {
        { key = "n", id = "format", label = "Format" },
        { key = "e", id = "test", label = "Test" },
        { key = "y", action = "submit", label = "Apply selections" },
      },
    } },
    on_confirm = function(result) multiple_result = result.selected_option_list end,
  })
  invoke("<CR>")
  invoke("e")
  invoke("<CR>")
  invoke("y")
  assert_equals(multiple_result, nil, "multiple-choice jump keys should not submit")
  invoke("<CR>")
  assert_equals(#multiple_result, 2, "multiple-choice submission should return every selected option")

  local replaced_closed = false
  picker.open({
    owner = "first",
    host = { window_list = { origin_win, composer_win }, control_win = composer_win },
    page_list = { { id = "first", option_list = { { label = "First" } } } },
    on_close = function() replaced_closed = true end,
  })
  picker.open({
    owner = "second",
    host = { window_list = { origin_win, composer_win }, control_win = composer_win },
    page_list = { { id = "second", option_list = { { label = "Second" } } } },
  })
  assert_true(replaced_closed, "replacing a picker should notify the previous workflow")
  assert_true(picker.is_open("second"), "picker ownership should distinguish the active workflow")
  assert_true(not picker.is_open("first"), "inactive workflow ownership should not leak")
  picker.close(false)

  local previewed = {}
  local deleted = nil
  local scope_toggled = false
  local search_options = {}
  for option_index = 1, 10 do
    search_options[#search_options + 1] = {
      id = "session-" .. option_index,
      label = option_index == 7 and "Gamma migration" or ("Session " .. option_index),
      value = option_index,
    }
  end
  picker.open({
    owner = "search",
    host = { window_list = { origin_win, composer_win }, control_win = composer_win },
    page_list = { {
      id = "search",
      title = "Harness sessions",
      subtitle = "Search sessions.",
      option_list = search_options,
      search = { choice_keys = { "n", "e", "a", "i", "l", "u", "o", "y" } },
    } },
    action_list = {
      {
        key = "<C-j>",
        modes = { "n", "i" },
        callback = function(context) deleted = context.option.value end,
      },
      {
        key = "<C-o>",
        modes = { "n", "i" },
        callback = function() scope_toggled = true end,
      },
    },
    on_change = function(context) previewed[#previewed + 1] = context.option and context.option.value end,
  })
  active = picker._state_for_test()
  assert_equals(vim.api.nvim_get_current_win(), active.search_win, "search picker should focus its input")
  assert_equals(active.state.query_by_page.search, "", "search should begin empty")
  assert_equals(picker_state.page(active.state, active.spec).option_list[8].key, "y",
    "the first eight search results should receive jump keys")
  assert_equals(picker_state.page(active.state, active.spec).option_list[9].key, nil,
    "search results after eight should not receive jump keys")
  vim.api.nvim_buf_set_lines(active.search_buf, 0, -1, false, { "gma" })
  vim.api.nvim_exec_autocmds("TextChangedI", { buffer = active.search_buf })
  assert_equals(#picker_state.page(active.state, active.spec).option_list, 1, "fuzzy search should filter by session name")
  assert_equals(picker_state.selected_option(active.state, active.spec).label, "Gamma migration",
    "fuzzy search should retain the matching session")
  invoke("<C-j>", "i")
  assert_equals(deleted, 7, "Ctrl-j should act on the filtered selection")
  invoke("<C-o>", "i")
  assert_true(scope_toggled, "Ctrl-o should toggle picker scope from Insert mode")
  vim.cmd("stopinsert")
  vim.api.nvim_buf_set_lines(active.search_buf, 0, -1, false, { "" })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = active.search_buf })
  invoke("o")
  assert_equals(picker_state.selected_option(active.state, active.spec).value, 7,
    "normal-mode jump keys should select a visible search result without confirming")
  assert_equals(previewed[#previewed], 7, "search selection changes should publish preview updates")
  vim.api.nvim_buf_set_lines(active.search_buf, 0, -1, false, { "zzzz" })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = active.search_buf })
  assert_equals(#picker_state.page(active.state, active.spec).option_list, 0,
    "unmatched search text should produce an explicit empty result set")
  assert_true(table.concat(vim.api.nvim_buf_get_lines(active.buf, 0, -1, false), "\n")
      :find("No matching options.", 1, true) ~= nil,
    "an empty result set should render its explanatory row")
  assert_equals(vim.api.nvim_win_call(active.win, vim.fn.winsaveview).topline, 1,
    "an empty result set should reset the presentation view to its title content")
  picker.close(false)
end)

if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
