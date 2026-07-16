local PickerFilter = {}

---@param text string
---@param query string
---@return integer?
local function fuzzy_score(text, query)
  local candidate = text:lower()
  local needle = query:lower()
  if needle == "" then return 0 end

  local contiguous_start = candidate:find(needle, 1, true)
  if contiguous_start then
    return 100000 - (contiguous_start * 100) - #candidate
  end

  local candidate_index = 1
  local previous_index = 0
  local score = 0
  for needle_index = 1, #needle do
    local character = needle:sub(needle_index, needle_index)
    local found_index = candidate:find(character, candidate_index, true)
    if not found_index then return nil end
    local gap = previous_index == 0 and found_index - 1 or found_index - previous_index - 1
    score = score + 1000 - (gap * 25)
    previous_index = found_index
    candidate_index = found_index + 1
  end
  return score - #candidate
end

---@param option_list table[]
---@param query string
---@return table[]
function PickerFilter.apply(option_list, query)
  if query == "" then return vim.deepcopy(option_list) end
  local ranked = {}
  for original_index, option in ipairs(option_list) do
    local score = fuzzy_score(tostring(option.search_text or option.label or ""), query)
    if score then
      ranked[#ranked + 1] = { option = option, score = score, original_index = original_index }
    end
  end
  table.sort(ranked, function(left, right)
    if left.score ~= right.score then return left.score > right.score end
    return left.original_index < right.original_index
  end)
  return vim.tbl_map(function(entry) return vim.deepcopy(entry.option) end, ranked)
end

return PickerFilter
