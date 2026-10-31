local FieldInput = {}

local popup_window = require("forge.infra.popup_window")

---@class ForgePickerFieldEditor
---@field buffer integer
---@field window integer
---@field page_id string
---@field option_id string
---@field field_id string
---@field pending boolean
---@field minimum_width integer
---@field choice boolean
---@field error? string

--- Locate a value using field identity, independently of labels and highlight colors.
local function position(parent_window, frame, field_id, minimum_width)
  local candidates = {}
  local range = frame.primary_range[frame.selected_index]
  if range then
    for _, span in ipairs(range.spans or {}) do
      candidates[#candidates + 1] = { line = range.first, span = span }
    end
  end
  local option_range = frame.option_range[frame.selected_index]
  for _, span in ipairs(frame.content_span_list or {}) do
    if option_range and span.line >= option_range.first and span.line <= option_range.last then
      candidates[#candidates + 1] = { line = span.line, span = span }
    end
  end
  for _, candidate in ipairs(candidates) do
    local span = candidate.span
    if span.field_id == field_id and span.edit_start and span.edit_start >= span.first and span.edit_start < span.last then
      local parent = vim.api.nvim_win_get_config(parent_window)
      local line = frame.lines[candidate.line]
      local column = vim.fn.strdisplaywidth(line:sub(1, span.edit_start))
      local width = math.min(parent.width - column,
        math.max(minimum_width, vim.fn.strdisplaywidth(line:sub(span.edit_start + 1, span.last))))
      return { row = (tonumber(parent.row) or 0) + candidate.line,
        col = (tonumber(parent.col) or 0) + column + 1, width = width }
    end
  end
end

---@param editor ForgePickerFieldEditor
---@param parent_window integer
---@param frame ForgePickerFrame
---@return boolean
function FieldInput.resize(editor, parent_window, frame)
  local anchor = position(parent_window, frame, editor.field_id, editor.minimum_width)
  if not anchor or not vim.api.nvim_win_is_valid(editor.window) then return false end
  anchor.relative = "editor"
  vim.api.nvim_win_set_config(editor.window, anchor)
  return true
end

---@param editor ForgePickerFieldEditor
function FieldInput.focus(editor)
  if not vim.api.nvim_win_is_valid(editor.window) then return end
  vim.api.nvim_set_current_win(editor.window)
  popup_window.set_cursor_hidden(editor.window, false)
  if editor.choice then vim.cmd("stopinsert")
  elseif not editor.pending then vim.cmd("startinsert") end
end

---@param parent_window integer
---@param frame ForgePickerFrame
---@param context ForgePickerSelectionContext
---@param callbacks {submit: fun(), cancel: fun()}
---@return ForgePickerFieldEditor?
function FieldInput.open(parent_window, frame, context, callbacks)
  local input = context.field.input
  local choice = input and input.kind == "choice" or false
  local minimum_width = input and input.maximum and #tostring(input.maximum) or 1
  if choice then
    assert(input.values and #input.values > 0, "choice input requires ordered values")
    for _, value in ipairs(input.values) do minimum_width = math.max(minimum_width, vim.fn.strdisplaywidth(value)) end
  end
  local anchor = position(parent_window, frame, context.field.id, minimum_width)
  if not anchor then return end
  local buffer, window = popup_window.open({ title = "", border = "none", relative = "editor",
    row = anchor.row, col = anchor.col, width = anchor.width, height = 1,
    filetype = "ForgePickerFieldInput", zindex = 95, wrap = false, winfixbuf = true })
  local editor = { buffer = buffer, window = window, page_id = context.page.id,
    option_id = context.option.id, field_id = context.field.id, pending = false, minimum_width = minimum_width, choice = choice }
  vim.b[buffer].miniclue_disable = true
  vim.wo[window].winhighlight = "Normal:Visual"
  local text = choice and (input.initial or tostring(context.field.value)) or tostring(context.field.value)
  if choice and not vim.tbl_contains(input.values, text) then text = input.values[1] end
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, { text })
  vim.api.nvim_win_set_cursor(window, { 1, #text })
  vim.keymap.set({ "n", "i" }, "<CR>", callbacks.submit, { buffer = buffer, silent = true, desc = "Save setting" })
  vim.keymap.set({ "n", "i" }, "<Esc>", callbacks.cancel, { buffer = buffer, silent = true, desc = "Cancel setting edit" })
  vim.keymap.set({ "n", "i" }, "<C-c>", callbacks.cancel, { buffer = buffer, silent = true, desc = "Cancel setting edit" })
  for _, key in ipairs({ "<Tab>", "<S-Tab>" }) do
    vim.keymap.set({ "n", "i" }, key, "<Nop>", { buffer = buffer, silent = true })
  end
  for _, movement in ipairs({ { "<Up>", 1 }, { "<Down>", -1 } }) do
    if choice then
      vim.keymap.set({ "n", "i" }, movement[1], function()
        if editor.pending then return end
        local current = vim.api.nvim_buf_get_lines(buffer, 0, 1, false)[1]
        local position = math.max(1, math.min(#input.values, vim.fn.index(input.values, current) + 1 + movement[2]))
        vim.bo[buffer].modifiable = true
        vim.api.nvim_buf_set_lines(buffer, 0, -1, false, { input.values[position] })
        vim.bo[buffer].modifiable = false
      end, { buffer = buffer, silent = true, desc = movement[2] > 0 and "Increase value" or "Decrease value" })
    else
      vim.keymap.set({ "n", "i" }, movement[1], "<Nop>", { buffer = buffer, silent = true })
    end
  end
  if choice then vim.bo[buffer].modifiable = false end
  FieldInput.focus(editor)
  if not choice then vim.cmd("startinsert!") end
  return editor
end

---@param editor ForgePickerFieldEditor
---@param field ForgePickerField
---@return {value?: string|number, error?: string}
function FieldInput.parse(editor, field)
  local text = table.concat(vim.api.nvim_buf_get_lines(editor.buffer, 0, -1, false), "\n")
  local specification = field.input
  if not specification or specification.kind == "text" then return { value = text } end
  if specification.kind == "choice" then
    if vim.tbl_contains(assert(specification.values), text) then return { value = text } end
    return { error = "Choose a listed value with Up or Down." }
  end
  local value = tonumber(text)
  if not value or value % 1 ~= 0 then return { error = "Enter a whole number." } end
  if specification.minimum and value < specification.minimum then
    return { error = "Enter a whole number of at least " .. specification.minimum .. "." }
  end
  if specification.maximum and value > specification.maximum then
    return { error = "Enter a whole number no greater than " .. specification.maximum .. "." }
  end
  return { value = value }
end

---@param editor ForgePickerFieldEditor
function FieldInput.close(editor)
  if vim.api.nvim_get_current_win() == editor.window then vim.cmd("stopinsert") end
  popup_window.close(editor.window, false)
end

return FieldInput
