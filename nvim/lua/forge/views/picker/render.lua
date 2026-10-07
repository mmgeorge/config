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
  for index, range in pairs(frame.primary_range or frame.option_range) do
    local group = index == frame.selected_index and frame.highlight_selected_text ~= false and "ForgePickerSelected"
      or frame.chosen_index_set and frame.chosen_index_set[index] and "ForgePickerChosen"
      or "ForgePickerOption"
    for line = range.first, range.last do
      vim.api.nvim_buf_set_extmark(buf, namespace, line - 1, 0, {
        end_row = line, end_col = 0, hl_group = group, priority = 100,
      })
      if index == frame.selected_index and frame.highlight_selected_line then
        vim.api.nvim_buf_set_extmark(buf, namespace, line - 1, 0, {
          line_hl_group = "ForgePickerSelectedLine", priority = 500,
        })
      end
    end
    if (index ~= frame.selected_index or frame.highlight_selected_text == false) and range.key_end then
      vim.api.nvim_buf_add_highlight(buf, namespace, "ForgePickerKey", range.first - 1, 2, range.key_end)
    end
    for _, span in ipairs(range.spans or {}) do
      vim.api.nvim_buf_set_extmark(buf, namespace, range.first - 1, span.first, {
        end_col = span.last, hl_group = span.group, priority = math.min(65535, 200 + (span.priority or 0)),
      })
    end
    local highlight = frame.option_highlight_by_index and frame.option_highlight_by_index[index]
    if highlight then
      local start = (frame.lines[range.first] or ""):find(highlight.text, 1, true)
      if start then
        vim.api.nvim_buf_add_highlight(buf, namespace, highlight.group,
          range.first - 1, start - 1, start - 1 + #highlight.text)
      end
    end
  end
  for _, range in pairs(frame.child_range or {}) do
    for line = range.first, range.last do
      vim.api.nvim_buf_add_highlight(buf, namespace, "ForgePickerText", line - 1, 0, -1)
    end
  end
  if frame.footer_line then
    vim.api.nvim_buf_add_highlight(buf, namespace, "ForgePickerHint", frame.footer_line - 1, 0, -1)
  end
end

return PickerRender
