local PickerField = {}

---@param value string?
---@param active boolean
---@param column? { width: integer, selectable: boolean }
---@return string?
function PickerField.render(value, active, column)
  if column and column.width == 0 then return nil end
  local text = value or ""
  if not column or column.selectable then
    text = active and ("← %s →"):format(text) or ("  %s  "):format(text)
  end
  return text .. string.rep(" ", math.max(0, (column and column.width or 0) - vim.fn.strdisplaywidth(text)))
end

return PickerField
