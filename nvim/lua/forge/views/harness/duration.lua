local Duration = {}

---@class ForgeInputTimeout
---@field amount integer
---@field unit 's'|'m'|'h'

local seconds_by_unit = { s = 1, m = 60, h = 3600 }
local choices = { "15s", "30s", "45s", "60s" }
for minutes = 2, 60 do choices[#choices + 1] = minutes .. "m" end

---@param value? ForgeInputTimeout
---@return ForgeInputTimeout
function Duration.value(value)
  if type(value) ~= "table" then return { amount = 10, unit = "m" } end
  return { amount = value.amount, unit = value.unit }
end

---@param value? ForgeInputTimeout
---@return string
function Duration.render(value)
  value = Duration.value(value)
  local seconds = value.amount * seconds_by_unit[value.unit]
  if seconds <= 60 then return seconds .. "s" end
  if seconds % 60 == 0 then return seconds / 60 .. "m" end
  return seconds .. "s"
end

---@return string[]
function Duration.choices()
  return vim.deepcopy(choices)
end

---@param text string
---@return ForgeInputTimeout
function Duration.parse(text)
  assert(vim.tbl_contains(choices, text), "timeout must be a supported choice")
  local amount, unit = text:match("^(%d+)([sm])$")
  return { amount = assert(tonumber(amount)), unit = unit }
end

---@param value? ForgeInputTimeout
---@return string
function Duration.selection(value)
  value = Duration.value(value)
  local seconds = value.amount * seconds_by_unit[value.unit]
  for _, choice in ipairs(choices) do
    local candidate = Duration.parse(choice)
    if candidate.amount * seconds_by_unit[candidate.unit] >= seconds then return choice end
  end
  return choices[#choices]
end

---@param value? ForgeInputTimeout
---@param direction integer
---@return ForgeInputTimeout
function Duration.change(value, direction)
  value = Duration.value(value)
  local seconds = value.amount * seconds_by_unit[value.unit]
  local selected = direction > 0 and choices[#choices] or choices[1]
  if direction > 0 then
    for _, choice in ipairs(choices) do
      local candidate = Duration.parse(choice)
      if candidate.amount * seconds_by_unit[candidate.unit] > seconds then selected = choice break end
    end
  else
    for index = #choices, 1, -1 do
      local candidate = Duration.parse(choices[index])
      if candidate.amount * seconds_by_unit[candidate.unit] < seconds then selected = choices[index] break end
    end
  end
  return Duration.parse(selected)
end

return Duration
