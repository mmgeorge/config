local PickerRender = {}

local namespace = vim.api.nvim_create_namespace("ForgePicker")

--- Color only page content when category navigation shares the buffer.
local function highlight_row(buf, frame, line, group, priority)
  local start = frame.body_start_by_line and frame.body_start_by_line[line] or 0
  if frame.body_start_by_line then
    if start >= #(frame.lines[line] or "") then return end
    vim.api.nvim_buf_set_extmark(buf, namespace, line - 1, start, {
      end_col = #frame.lines[line], hl_group = group, priority = priority or 100,
    })
  else
    vim.api.nvim_buf_set_extmark(buf, namespace, line - 1, 0, {
      end_row = line, end_col = 0, hl_group = group, priority = priority or 100,
    })
  end
end

---@param buf integer
---@param frame table
function PickerRender.apply(buf, frame)
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, frame.lines)
  vim.bo[buf].modifiable = false
  vim.api.nvim_buf_clear_namespace(buf, namespace, 0, -1)
  for _, line in ipairs(frame.section_line) do
    highlight_row(buf, frame, line, "ForgePickerSection")
  end
  for _, content in ipairs(frame.content_range) do
    for line = content.first, content.last do
      highlight_row(buf, frame, line, content.group or "ForgePickerText")
    end
  end
  for _, span in ipairs(frame.content_span_list or {}) do
    vim.api.nvim_buf_set_extmark(buf, namespace, span.line - 1, span.first, {
      end_col = span.last, hl_group = span.group, priority = span.priority or 200,
    })
  end
  for index, range in pairs(frame.primary_range or frame.option_range) do
    local group = index == frame.selected_index and frame.highlight_selected_text ~= false and "ForgePickerSelected"
      or frame.chosen_index_set and frame.chosen_index_set[index] and "ForgePickerChosen"
      or "ForgePickerOption"
    for line = range.first, range.last do
      highlight_row(buf, frame, line, group)
      if index == frame.selected_index and frame.highlight_selected_line then
        if frame.body_start_by_line then
          highlight_row(buf, frame, line, "ForgePickerSelectedLine", 500)
        else
          vim.api.nvim_buf_set_extmark(buf, namespace, line - 1, 0, {
            line_hl_group = "ForgePickerSelectedLine", priority = 500,
          })
        end
      end
    end
    if (index ~= frame.selected_index or frame.highlight_selected_text == false) and range.key_end then
      local offset = frame.body_start_by_line and frame.body_start_by_line[range.first] or 0
      vim.hl.range(buf, namespace, "ForgePickerKey", { range.first - 1, offset + 2 }, { range.first - 1, range.key_end })
    end
    for _, span in ipairs(range.spans or {}) do
      vim.api.nvim_buf_set_extmark(buf, namespace, range.first - 1, span.first, {
        end_col = span.last, hl_group = span.group, priority = math.min(65535, 200 + (span.priority or 0)),
      })
    end
    local highlight = frame.option_highlight_by_index and frame.option_highlight_by_index[index]
    if highlight then
      local offset = frame.body_start_by_line and frame.body_start_by_line[range.first] or 0
      local start = (frame.lines[range.first] or ""):find(highlight.text, offset + 1, true)
      if start then
        vim.hl.range(buf, namespace, highlight.group,
          { range.first - 1, start - 1 }, { range.first - 1, start - 1 + #highlight.text })
      end
    end
  end
  for _, range in pairs(frame.child_range or {}) do
    for line = range.first, range.last do
      highlight_row(buf, frame, line, "ForgePickerText")
    end
  end
  if frame.footer_line then
    highlight_row(buf, frame, frame.footer_line, "ForgePickerHint")
  end
end

return PickerRender
