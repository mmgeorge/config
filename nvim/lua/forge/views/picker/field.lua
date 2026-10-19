local PickerField = {}

---@param value string?
---@param active boolean
---@param column? { width?: integer, selectable?: boolean, compact?: boolean }
---@return string?
function PickerField.render(value, active, column)
  if column and column.width == 0 then return nil end
  local text = value or ""
  if not column or column.selectable ~= false then
    local gap = column and column.compact and "" or " "
    text = (active and "←" or " ") .. gap .. text .. gap .. (active and "→" or " ")
  end
  return text .. string.rep(" ", math.max(0, (column and column.width or 0) - vim.fn.strdisplaywidth(text)))
end

---@class ForgePickerField
---@field id string
---@field kind 'choice'|'toggle'|'input'|'action'
---@field value string|boolean
---@field label? string
---@field description? string
---@field disabled_reason? string
---@field muted? boolean
---@field join_previous? boolean Share the previous field column while retaining separate focus.

---@param field ForgePickerField
---@param active boolean
---@return string
function PickerField.text(field, active)
  local value = field.kind == "toggle" and ((field.value and "● " or "○ ") .. (field.label or ""))
    or ((field.label and (field.label .. ": ") or "") .. tostring(field.value))
  if field.kind == "choice" then return assert(PickerField.render(value, active)) end
  return "  " .. value .. "  "
end

---@param option { label: string, fields: ForgePickerField[], inline_fields?: boolean }
---@param selected_id? string
---@param active boolean
---@return string[], table<integer, table[]>
function PickerField.columns(option, selected_id, active)
  local values, segments = {}, {}
  if not option.inline_fields then values[1] = option.label end
  for _, field in ipairs(option.fields or {}) do
    local focused = active and field.id == (selected_id or option.fields[1].id)
    local value = PickerField.text(field, focused)
    if option.inline_fields then value = vim.trim(value) end
    local group = field.disabled_reason and "ForgePickerHint"
      or focused and "ForgePickerSelected" or field.muted and "ForgePickerHint" or "ForgePickerOption"
    if field.join_previous and #values > 1 then
      local column = #values
      values[column] = values[column]:gsub("%s+$", "")
      local previous = segments[column][#segments[column]]
      previous[1] = previous[1]:gsub("%s+$", "")
      value = " " .. vim.trim(value)
      values[column] = values[column] .. value
      segments[column][#segments[column] + 1] = { value, group }
    else
      values[#values + 1] = value
      segments[#values] = { { value, group } }
    end
  end
  return values, segments
end

return PickerField
