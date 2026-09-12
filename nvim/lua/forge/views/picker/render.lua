local PickerRender = {}

local namespace = vim.api.nvim_create_namespace("ForgePicker")

---@param buf integer
---@param frame table
function PickerRender.apply(buf, frame)
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, frame.lines)
  vim.bo[buf].modifiable = false
  vim.api.nvim_buf_clear_namespace(buf, namespace, 0, -1)
  for _, line in ipairs(frame.section_line) do
    vim.api.nvim_buf_add_highlight(buf, namespace, "ForgePickerSection", line - 1, 0, -1)
  end
  for _, content in ipairs(frame.content_range) do
    for line = content.first, content.last do
      vim.api.nvim_buf_add_highlight(buf, namespace, content.group or "ForgePickerText", line - 1, 0, -1)
    end
  end
  for index, range in ipairs(frame.primary_range or frame.option_range) do
    local group = index == frame.selected_index and "ForgePickerSelected"
      or frame.chosen_index_set and frame.chosen_index_set[index] and "ForgePickerChosen"
      or "ForgePickerOption"
    for line = range.first, range.last do
      vim.api.nvim_buf_add_highlight(buf, namespace, group, line - 1, 0, -1)
    end
    if index ~= frame.selected_index then
      vim.api.nvim_buf_add_highlight(buf, namespace, "ForgePickerKey", range.first - 1, 2, 3)
    end
  end
  for _, range in pairs(frame.child_range or {}) do
    for line = range.first, range.last do
      vim.api.nvim_buf_add_highlight(buf, namespace, "ForgePickerText", line - 1, 0, -1)
    end
  end
  vim.api.nvim_buf_add_highlight(buf, namespace, "ForgePickerHint", frame.footer_line - 1, 0, -1)
end

return PickerRender
