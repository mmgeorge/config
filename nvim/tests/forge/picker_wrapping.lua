vim.loader.enable(false)

local function assert_true(value, message)
  if not value then error(message, 2) end
end

local function assert_equals(actual, expected, message)
  if not vim.deep_equal(actual, expected) then
    error(message .. "\nexpected: " .. vim.inspect(expected) .. "\nactual: " .. vim.inspect(actual), 2)
  end
end

local function invoke(key)
  local mapping = vim.fn.maparg(key, "n", false, true)
  assert_true(mapping.callback ~= nil, "missing mapping for " .. key)
  mapping.callback()
end

local ok, failure = pcall(function()
  require("forge").setup({ harness = { backend = "mock" } })
  local layout = require("forge.views.picker.layout")
  local render = require("forge.views.picker.render")
  local picker = require("forge.views.picker")
  local description = "Save all four proposed checks with source/configuration change triggers and sequential dependencies. Includes desktop compilation cost."
  local page = {
    id = "wrapped",
    column_headers = { "Answer", "Description" },
    flexible_column = 2,
    wrap_column = 2,
    highlight_selected_line = true,
    footer = "Enter confirm",
    option_list = {
      { key = "n", label = "All automatic", detail = description },
      { key = "e", label = "Build on request", detail = description },
      { key = "i", label = "No automated checks", detail = "Save an empty configuration." },
    },
  }
  local frame = layout.build(page, 1, 70)
  local description_column = assert(frame.lines[1]:find("Description", 1, true))
  local fragments = {}
  for row = frame.option_range[1].first, frame.option_range[1].last do
    fragments[#fragments + 1] = vim.trim(frame.lines[row]:sub(description_column))
    assert_true(vim.fn.strdisplaywidth(frame.lines[row]) <= 66, "wrapped row exceeds available width")
    if row > frame.option_range[1].first then
      assert_true(frame.lines[row]:sub(1, description_column - 1):match("^%s*$"),
        "continuations must leave the answer and key columns empty")
    end
  end
  assert_equals(table.concat(fragments, " "), description, "wrapping lost description text")
  assert_true(frame.option_range[1].last > frame.option_range[1].first, "description did not wrap")
  assert_true(frame.lines[frame.option_range[3].first]:find("No automated checks", 1, true) ~= nil,
    "description width must shrink before the answer column")

  local initial = layout.viewport(frame, 9)
  local second = layout.viewport(layout.build(page, 2, 70), 9, initial.viewport_top)
  assert_equals(second.lines[1], frame.lines[1], "scrolling must keep column headings visible")
  assert_equals(second.lines[second.footer_line], "  Enter confirm", "scrolling must keep controls visible")
  assert_equals(second.primary_range[2].last - second.primary_range[2].first,
    frame.primary_range[2].last - frame.primary_range[2].first, "selection must reveal the complete option when it fits")
  local buffer = vim.api.nvim_create_buf(false, true)
  render.apply(buffer, second)
  local selected_rows = {}
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(buffer, -1, 0, -1, { details = true })) do
    if mark[4].line_hl_group == "ForgePickerSelectedLine" then selected_rows[#selected_rows + 1] = mark[2] + 1 end
  end
  assert_equals(#selected_rows, second.primary_range[2].last - second.primary_range[2].first + 1,
    "selection background must cover every continuation")

  local unicode = "界面设置" .. string.rep("x", 90)
  local styled_page = vim.deepcopy(page)
  styled_page.option_list = { { key = "n", label = "Unicode", detail = unicode,
    column_spans = { [2] = { { first = 0, last = #unicode, group = "String" } } } } }
  local styled = layout.viewport(layout.build(styled_page, 1, 40), 6, nil, nil, 1)
  assert_equals(styled.primary_range[1].key_end, nil, "hidden key must not color continuation text")
  render.apply(buffer, styled)
  local styled_text = {}
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(buffer, -1, 0, -1, { details = true })) do
    if mark[4].hl_group == "String" then
      styled_text[#styled_text + 1] = styled.lines[mark[2] + 1]:sub(mark[3] + 1, mark[4].end_col)
    end
  end
  assert_true(#styled_text > 1, "wrapped captures must survive viewport remapping")
  for _, line in ipairs(styled.lines) do
    assert_true(vim.fn.strdisplaywidth(line) <= 36, "Unicode wrapping exceeds the cell width")
  end
  local full_unicode = layout.build(styled_page, 1, 40)
  local unicode_column = assert(full_unicode.lines[1]:find("Description", 1, true))
  local unicode_parts = {}
  for row = full_unicode.option_range[1].first, full_unicode.option_range[1].last do
    unicode_parts[#unicode_parts + 1] = full_unicode.lines[row]:sub(unicode_column)
  end
  assert_equals(table.concat(unicode_parts), unicode, "long Unicode tokens must retain every character")
  vim.api.nvim_buf_delete(buffer, { force = true })

  local question_view = require("forge.views.harness.plan_question")
  local host_window = vim.api.nvim_get_current_win()
  local submitted = false
  question_view.open({ question_set = { id = "wrapping", questions = { {
    id = "checks", header = "Checks", question = "Which checks should be saved?",
    options = { { label = "All automatic", description = string.rep(description .. " ", 30) },
      { label = "Build on request", description = description } }, allow_freeform = true,
  } } }, answer = {}, current_index = 0 }, {
    transcript_win = host_window, window_list = { host_window }, control_win = host_window,
    answer = function() submitted = true end, ask = function() end, continue = function() end,
  })
  local active = picker._state_for_test()
  assert_equals(active.spec.page_list[1].wrap_column, 2, "question descriptions must use shared wrapping")
  assert_true(active.frame.option_scroll_max > 0, "oversized description must expose vertical scrolling")
  invoke("<PageDown>")
  assert_true(active.frame.option_offset > 0, "Page Down must reveal the rest of an oversized description")
  invoke("<Down>")
  assert_equals(active.frame.selected_index, 2, "navigation must advance by option, not physical row")
  assert_equals(submitted, false, "navigation must not submit an answer")
  invoke("<Tab>")
  vim.api.nvim_buf_set_lines(active.input_buf, 0, -1, false, { "Keep this draft while resizing." })
  local previous_columns = vim.o.columns
  vim.o.columns = 70
  vim.api.nvim_exec_autocmds("VimResized", {})
  vim.wait(50, function() return false end, 10)
  assert_equals(active.frame.selected_index, 2, "resizing must retain the selected answer")
  assert_equals(vim.api.nvim_buf_get_lines(active.input_buf, 0, -1, false), { "Keep this draft while resizing." },
    "resizing must retain the attached input")
  picker.close(false)
  vim.o.columns = previous_columns
end)

if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  print("picker wrapping: passed")
  vim.cmd("qa!")
end
