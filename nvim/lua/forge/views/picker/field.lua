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

return PickerField
