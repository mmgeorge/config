vim.loader.enable(false)
vim.o.columns, vim.o.lines = 76, 32
require("forge").setup({ harness = { provider = "mock" } })

local picker = require("forge.views.picker")
local requests = {}
local value_field = { id = "amount", kind = "input", value = "10", description = "Timeout amount.",
  input = { kind = "integer", minimum = 0, maximum = 4294967295 } }
local spec = { owner = "field-input-test", title = "Settings", hint_mode = "help", page_navigation = "tabs",
  keymaps = { page_next = "<Tab>", page_previous = "<S-Tab>", next_field = "<C-Right>", previous_field = "<C-Left>" },
  page_list = {
    { id = "first", title = "First", field_navigation = true, field_description_height = 2, option_list = {
      { id = "timeout", label = "Timeout", fields = { value_field,
        { id = "unit", kind = "choice", value = "m", join_previous = true } } },
    } },
    { id = "second", title = "Second", option_list = {} },
  },
  on_field_submit = function(context, value, complete)
    requests[#requests + 1] = { context = context, value = value, complete = complete }
  end,
}

local function press(buffer, key)
  local mapping = vim.api.nvim_buf_call(buffer, function() return vim.fn.maparg(key, "n", false, true) end)
  if mapping.callback then mapping.callback()
  else assert(mapping.rhs == "<Nop>", "missing key: " .. key) end
end

local function begin_edit()
  local instance = picker._state_for_test()
  press(instance.buf, "<CR>")
  local editor = assert(instance.field_editor, "Enter did not open an editor")
  assert(vim.api.nvim_get_current_win() == editor.window)
  assert(vim.bo[editor.buffer].filetype == "ForgePickerFieldInput")
  return editor
end

local function draft(editor, text)
  vim.api.nvim_buf_set_lines(editor.buffer, 0, -1, false, { text })
end

local function assert_draft(editor, text)
  assert(vim.api.nvim_buf_get_lines(editor.buffer, 0, -1, false)[1] == text, "draft was replaced")
end

local success, failure = xpcall(function()
  picker.open(spec)
  local editor = begin_edit()
  assert_draft(editor, "10")
  draft(editor, "not a number")
  press(editor.buffer, "<CR>")
  assert(#requests == 0 and editor.error, "invalid text was saved")
  assert_draft(editor, "not a number")
  assert(table.concat(picker._state_for_test().frame.lines, " "):find("Enter a whole number.", 1, true))
  for _, invalid in ipairs({ "-1", "1.5", "4294967296", "" }) do
    draft(editor, invalid)
    press(editor.buffer, "<CR>")
    assert(#requests == 0 and editor.error)
  end
  draft(editor, "20")
  press(editor.buffer, "<Tab>")
  press(editor.buffer, "<S-Tab>")
  assert(picker._state_for_test().state.page_index == 1, "editing changed categories")
  value_field.value = "11"
  vim.o.columns = 40
  picker.update(spec)
  assert_draft(editor, "20")
  assert(vim.api.nvim_get_current_win() == editor.window, "redraw stole editor focus")
  local parent = vim.api.nvim_win_get_config(picker._state_for_test().win)
  local position = vim.api.nvim_win_get_config(editor.window)
  assert(position.col >= parent.col and position.col + position.width <= parent.col + parent.width)
  press(editor.buffer, "<CR>")
  assert(#requests == 1 and requests[1].value == 20 and editor.pending)
  press(editor.buffer, "<CR>")
  press(editor.buffer, "<Esc>")
  assert(#requests == 1 and picker._state_for_test().field_editor == editor, "pending save was submitted twice or cancelled")
  requests[1].complete("Save failed. Retry.")
  assert(not editor.pending and editor.error == "Save failed. Retry.")
  assert_draft(editor, "20")
  assert(vim.bo[editor.buffer].modifiable)
  press(editor.buffer, "<CR>")
  value_field.value = "20"
  picker.update(spec)
  assert_draft(editor, "20")
  requests[2].complete()
  assert(not picker._state_for_test().field_editor and not vim.api.nvim_win_is_valid(editor.window))
  editor = begin_edit()
  draft(editor, "77")
  press(editor.buffer, "<Esc>")
  assert(value_field.value == "20" and #requests == 2, "cancel saved a draft")
  editor = begin_edit()
  draft(editor, "0")
  press(editor.buffer, "<CR>")
  assert(requests[3].value == 0, "zero timeout was rejected")
  picker.open({ owner = "other-session", page_list = { { id = "other", option_list = {} } } })
  assert(not vim.api.nvim_win_is_valid(editor.window), "replacement picker leaked the editor")
  requests[3].complete()
  assert(picker.is_open("other-session"), "late save replaced another picker")
  spec.page_list[1].option_list[2] = { id = "second-timeout", label = "Other timeout", fields = {
    { id = "amount", kind = "input", value = "123", input = { kind = "integer", minimum = 0 } },
  } }
  spec.page_list[1].selected_index = 2
  picker.open(spec)
  editor = begin_edit()
  assert_draft(editor, "123")
  local instance = picker._state_for_test()
  local position = vim.api.nvim_win_get_config(editor.window)
  local parent = vim.api.nvim_win_get_config(instance.win)
  assert(position.row == parent.row + instance.frame.primary_range[2].first,
    "editor attached to another row with the same field id")
  table.remove(spec.page_list[1].option_list, 2)
  picker.update(spec)
  assert(not picker._state_for_test().field_editor and not vim.api.nvim_win_is_valid(editor.window),
    "removed row retained an active editor")
end, debug.traceback)
picker.close(false)
assert(success, failure)
print("picker_field_input: validation, retry, cancel, redraw, and stale completion passed")
vim.cmd("qa!")
