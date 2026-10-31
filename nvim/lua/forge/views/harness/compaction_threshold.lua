local Threshold = {}

local picker = require("forge.views.picker")
local values = { 64000, 128000, 256000, 512000 }

---@param value number
---@return string
function Threshold.label(value)
  if vim.tbl_contains(values, value) then return tostring(value / 1000) .. "k" end
  return tostring(value)
end

---@param value number
---@param direction integer
---@return number
function Threshold.change(value, direction)
  for index, candidate in ipairs(values) do
    if candidate == value then return values[(index - 1 + direction) % #values + 1] end
  end
  if direction > 0 then
    for _, candidate in ipairs(values) do if candidate > value then return candidate end end
    return values[1]
  end
  for index = #values, 1, -1 do if values[index] < value then return values[index] end end
  return values[#values]
end

---@param options {host: table, value: number, on_select: fun(value: number), on_close: fun()}
function Threshold.open(options)
  local rows, selected = {}, 1
  for index, value in ipairs(values) do
    rows[index] = { id = tostring(value), label = Threshold.label(value), value = value }
    if value == options.value then selected = index end
  end
  picker.close(false)
  picker.open({ owner = "compaction-threshold", host = options.host,
    page_list = { { id = "threshold", title = "Compaction threshold", option_list = rows, selected_index = selected,
      footer = "Enter select  q back" } },
    on_close = options.on_close,
    on_confirm = function(context)
      picker.close(false)
      options.on_select(context.option.value)
    end,
  })
end

return Threshold
