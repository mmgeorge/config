local PickerFieldLayout = {}

--- Shared field slots for one section, measured independently of selection.
---@class ForgePickerFieldGrid
---@field column_width integer[] Label followed by field slots, or only field slots for inline rows.
---@field fields_per_row integer Number of complete field cells on each visual row.

--- Keep whole field cells together, reducing their count before wrapping cell text.
---@param group table
---@param width integer
---@param prefix_width integer
---@param label_width integer
---@return ForgePickerFieldGrid
local function measure_group(group, width, prefix_width, label_width)
  local label_offset = group.inline and 0 or 1
  local available = width - prefix_width - label_width - label_offset * 2
  for field_count = group.field_count, 1, -1 do
    local field_width = {}
    for _, option in ipairs(group.options) do
      for index = 1, #option.columns - label_offset do
        local slot = (index - 1) % field_count + 1
        field_width[slot] = math.max(field_width[slot] or 0,
          vim.fn.strdisplaywidth(option.columns[index + label_offset]))
      end
    end
    local total = math.max(0, field_count - 1) * 2
    for _, size in ipairs(field_width) do total = total + size end
    if total <= available or field_count == 1 then
      if field_count == 1 then field_width[1] = math.min(field_width[1], math.max(1, available)) end
      local column_width = group.inline and {} or { label_width }
      vim.list_extend(column_width, field_width)
      return { column_width = column_width, fields_per_row = field_count }
    end
  end
  error("structured picker rows require fields")
end

--- Align labels across the page and field slots within each semantic section.
---@param option_list table[] Rows with rendered field columns.
---@param width integer Available display cells including the row prefix.
---@param prefix_width integer Display cells reserved for option shortcuts.
---@return table<integer, ForgePickerFieldGrid>
function PickerFieldLayout.measure(option_list, width, prefix_width)
  local group_by_section, group_by_option = {}, {}
  local label_width = 0
  for index, option in ipairs(option_list) do
    if option.fields then
      local label_offset = option.inline_fields and 0 or 1
      if label_offset == 1 then label_width = math.max(label_width, vim.fn.strdisplaywidth(option.label)) end
      local section = (option.section or "") .. (option.inline_fields and "\0inline" or "\0labelled")
      local group = group_by_section[section]
      if not group then
        group = { inline = option.inline_fields == true, options = {}, field_count = 0 }
        group_by_section[section] = group
      end
      group.options[#group.options + 1] = option
      group.field_count = math.max(group.field_count, #option.columns - label_offset)
      group_by_option[index] = group
    end
  end
  label_width = math.min(label_width, math.max(1, width - prefix_width - 3))
  local measured = {}
  for _, group in pairs(group_by_section) do
    group.grid = measure_group(group, width, prefix_width, group.inline and 0 or label_width)
  end
  for index, group in pairs(group_by_option) do measured[index] = group.grid end
  return measured
end

return PickerFieldLayout
