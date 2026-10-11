local Duration = {}
local field = require("forge.views.picker.field")

---@class ForgeInputTimeout
---@field amount integer
---@field unit 's'|'m'|'h'

---@param value? ForgeInputTimeout
---@return ForgeInputTimeout
function Duration.value(value)
  if type(value) ~= "table" then return { amount = 10, unit = "m" } end
  return { amount = value.amount, unit = value.unit }
end

---@param value ForgeInputTimeout
---@param focus 'amount'|'unit'
---@return string
function Duration.render(value, focus)
  local amount = value.amount == 0 and "Off" or tostring(value.amount)
  return field.render(amount, focus == "amount") .. "  " .. field.render(value.unit, focus == "unit")
end

---@param value ForgeInputTimeout
---@param focus 'amount'|'unit'
---@param direction integer
---@return ForgeInputTimeout
function Duration.change(value, focus, direction)
  value = Duration.value(value)
  if focus == "amount" then
    value.amount = math.max(0, math.min(4294967295, value.amount + direction))
  else
    local units = { "s", "m", "h" }
    local position = value.unit == "s" and 1 or value.unit == "m" and 2 or 3
    value.unit = units[(position - 1 + direction) % #units + 1]
  end
  return value
end

return Duration
