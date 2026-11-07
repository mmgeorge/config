local Threshold = {}

local picker = require("forge.views.picker")
local values = { 64000, 128000, 256000, 512000 }

---@param value number
---@return string
function Threshold.label(value)
  if vim.tbl_contains(values, value) then return tostring(value / 1000) .. "k" end
  return tostring(value)
end

---List presets that Harness accepts for the reported context capacity.
---Harness rejects a threshold at or above the model context window, so an unknown capacity
---keeps every preset and a known one removes the presets it cannot accept.
---@param capacity? number
---@return number[]
function Threshold.options(capacity)
  if not capacity then return vim.deepcopy(values) end
  return vim.tbl_filter(function(value) return value < capacity end, values)
end

---@param value number
---@param direction integer
---@param capacity? number
---@return number
function Threshold.change(value, direction, capacity)
  local option_list = Threshold.options(capacity)
  if #option_list == 0 then return value end
  for index, candidate in ipairs(option_list) do
    if candidate == value then return option_list[(index - 1 + direction) % #option_list + 1] end
  end
  if direction > 0 then
    for _, candidate in ipairs(option_list) do if candidate > value then return candidate end end
    return option_list[1]
  end
  for index = #option_list, 1, -1 do if option_list[index] < value then return option_list[index] end end
  return option_list[#option_list]
end

---@param options {host: table, value: number, capacity?: number, on_select: fun(value: number), on_close: fun()}
function Threshold.open(options)
  local rows, selected = {}, 1
  for index, value in ipairs(Threshold.options(options.capacity)) do
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
