local M = {}

local action_highlight_by_name = {
  Add = "DiffReviewWalkthroughActionAdd",
  Modify = "DiffReviewWalkthroughActionModify",
  Remove = "DiffReviewWalkthroughActionRemove",
}

---@class DiffReviewTaskTreeStyleTerm
---@field text string
---@field hl string?

---@param line string
---@param term_list DiffReviewTaskTreeStyleTerm[]
---@return table[]
local function ordered_segments(line, term_list)
  local segment_list = {}
  local cursor = 1
  for _, term in ipairs(term_list) do
    if term.text ~= "" then
      local start_index, end_index = line:find(term.text, cursor, true)
      if start_index then
        if start_index > cursor then segment_list[#segment_list + 1] = { line:sub(cursor, start_index - 1) } end
        segment_list[#segment_list + 1] = { line:sub(start_index, end_index), term.hl }
        cursor = end_index + 1
      end
    end
  end
  if cursor <= #line then segment_list[#segment_list + 1] = { line:sub(cursor) } end
  if #segment_list == 0 then return { { line } } end
  return segment_list
end

---@param segment_list table[]
---@param entity_name_set table<string, boolean>
---@return table[]
local function highlight_entity_references(segment_list, entity_name_set)
  local result = {}
  for _, segment in ipairs(segment_list) do
    local text = segment[1] or ""
    local base_highlight = segment[2]
    local cursor = 1
    local search_start = 1
    while search_start <= #text do
      local start_index, end_index = text:find("[%a_][%w_]*", search_start)
      if not start_index then break end
      if entity_name_set[text:sub(start_index, end_index)] then
        if start_index > cursor then
          result[#result + 1] = { text:sub(cursor, start_index - 1), base_highlight }
        end
        result[#result + 1] = { text:sub(start_index, end_index), "@type" }
        cursor = end_index + 1
      end
      search_start = end_index + 1
    end
    if cursor <= #text then result[#result + 1] = { text:sub(cursor), base_highlight } end
  end
  return result
end

---Overlay canonical entity references on an existing row style.
---@param segments_for_line? fun(line: string, line_index: integer): table[]
---@param entity_name_set table<string, boolean>
---@return fun(line: string, line_index: integer): table[]
function M.entity_references(segments_for_line, entity_name_set)
  return function(line, line_index)
    local segment_list = segments_for_line and segments_for_line(line, line_index) or { { line } }
    return highlight_entity_references(segment_list, entity_name_set)
  end
end

---Build status-compatible segments for a task heading.
---@param title string
---@return fun(line: string, line_index: integer): table[]
function M.task(title)
  return function(line, line_index)
    if line_index ~= 1 then return { { line } } end
    return ordered_segments(line, {
      { text = title, hl = "DiffReviewWalkthroughItemTitle" },
    })
  end
end

---Build semantic segments for one Add, Modify, or Remove row.
---@param action string
---@param kind string
---@param target string
---@param kind_highlight? string
---@param target_highlight? string
---@return fun(line: string, line_index: integer): table[]
function M.change(action, kind, target, kind_highlight, target_highlight)
  local action_highlight = action_highlight_by_name[action]
  return function(line, line_index)
    if line_index ~= 1 then return { { line } } end
    return ordered_segments(line, {
      { text = action, hl = action_highlight },
      { text = kind, hl = kind_highlight or "@keyword" },
      { text = target, hl = target_highlight or "@type" },
    })
  end
end

---Build status-compatible segments for a repository file boundary.
---@param path string
---@return fun(line: string, line_index: integer): table[]
function M.file(path)
  return function(line, line_index)
    if line_index ~= 1 then return { { line } } end
    return ordered_segments(line, {
      { text = "file", hl = "DiffReviewFileKeyword" },
      { text = path, hl = "DiffReviewWalkthroughItemTitle" },
    })
  end
end

return M
