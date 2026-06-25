---@class DiffReviewHunkIndexBodyRow
---@field line string
---@field section_index integer
---@field old_line integer
---@field new_line integer
---@field old_syntax_row integer
---@field new_syntax_row integer

---@class DiffReviewHunkIndexSection
---@field index integer
---@field header string
---@field old_start integer
---@field old_count integer
---@field new_start integer
---@field new_count integer
---@field context string?

---@class DiffReviewHunkIndex
---@field hunk table
---@field header_line_list string[]
---@field section_list DiffReviewHunkIndexSection[]
---@field body_row_list DiffReviewHunkIndexBodyRow[]

---@class DiffReviewHunkIndexModule
local M = {}

---@param header string
---@return integer old_start
---@return integer old_count
---@return integer new_start
---@return integer new_count
---@return string? context
function M.parse_hunk_header(header)
  local old_start, old_count, new_start, new_count, context =
    tostring(header or ""):match("^@@ %-(%d+),?(%d*) %+(%d+),?(%d*) @@%s*(.*)$")
  return tonumber(old_start) or 0,
    tonumber(old_count ~= "" and old_count or "1") or 1,
    tonumber(new_start) or 0,
    tonumber(new_count ~= "" and new_count or "1") or 1,
    context ~= "" and context or nil
end

---@param diff_line string?
---@return boolean
function M.is_body_line(diff_line)
  local prefix = tostring(diff_line or ""):sub(1, 1)
  return prefix == " " or prefix == "+" or prefix == "-"
end

---@param diff_line string?
---@param old_line integer
---@param new_line integer
---@return integer old_line
---@return integer new_line
local function advance_line(diff_line, old_line, new_line)
  local prefix = tostring(diff_line or ""):sub(1, 1)
  if prefix == " " then
    return old_line + 1, new_line + 1
  elseif prefix == "-" then
    return old_line + 1, new_line
  elseif prefix == "+" then
    return old_line, new_line + 1
  end
  return old_line, new_line
end

---@param line_list string[]
---@return integer added
---@return integer removed
local function diff_stats(line_list)
  local added = 0
  local removed = 0
  for _, line in ipairs(line_list or {}) do
    local prefix = tostring(line or ""):sub(1, 1)
    if prefix == "+" then
      added = added + 1
    elseif prefix == "-" then
      removed = removed + 1
    end
  end
  return added, removed
end

---@param line_list string[]
---@return integer old_count
---@return integer new_count
local function diff_line_count(line_list)
  local old_count = 0
  local new_count = 0
  for _, line in ipairs(line_list or {}) do
    local prefix = tostring(line or ""):sub(1, 1)
    if prefix == " " then
      old_count = old_count + 1
      new_count = new_count + 1
    elseif prefix == "-" then
      old_count = old_count + 1
    elseif prefix == "+" then
      new_count = new_count + 1
    end
  end
  return old_count, new_count
end

---@param hunk table
---@return DiffReviewHunkIndex
function M.from_hunk(hunk)
  local index = {
    hunk = hunk,
    header_line_list = {},
    section_list = {},
    body_row_list = {},
  }
  local current_section = nil
  local old_line = 0
  local new_line = 0
  local old_syntax_row = 0
  local new_syntax_row = 0
  for line in tostring(hunk and hunk.diff or ""):gmatch("[^\n]+") do
    if line:match("^@@ ") then
      local old_start, old_count, new_start, new_count, context = M.parse_hunk_header(line)
      current_section = {
        index = #index.section_list + 1,
        header = line,
        old_start = old_start,
        old_count = old_count,
        new_start = new_start,
        new_count = new_count,
        context = context,
      }
      index.section_list[#index.section_list + 1] = current_section
      old_line = old_start
      new_line = new_start
    elseif current_section and M.is_body_line(line) then
      index.body_row_list[#index.body_row_list + 1] = {
        line = line,
        section_index = current_section.index,
        old_line = old_line,
        new_line = new_line,
        old_syntax_row = old_syntax_row,
        new_syntax_row = new_syntax_row,
      }
      local prefix = line:sub(1, 1)
      if prefix == " " then
        old_syntax_row = old_syntax_row + 1
        new_syntax_row = new_syntax_row + 1
      elseif prefix == "-" then
        old_syntax_row = old_syntax_row + 1
      elseif prefix == "+" then
        new_syntax_row = new_syntax_row + 1
      end
      old_line, new_line = advance_line(line, old_line, new_line)
    elseif not current_section then
      index.header_line_list[#index.header_line_list + 1] = line
    end
  end
  return index
end

---@param hunk table
---@return DiffReviewHunkIndex
function M.ensure(hunk)
  if hunk and hunk.diff_review_hunk_index then return hunk.diff_review_hunk_index end
  local index = M.from_hunk(hunk)
  if hunk then hunk.diff_review_hunk_index = index end
  return index
end

---@param index DiffReviewHunkIndex
---@param hunk_key string
---@param body_start_line integer
---@param body_count integer
---@param chunk_index integer
---@return table? chunk
---@return string? render_key
---@return table? syntax_offset
function M.chunk(index, hunk_key, body_start_line, body_count, chunk_index)
  body_start_line = math.max(1, math.floor(tonumber(body_start_line) or 1))
  body_count = math.max(1, math.floor(tonumber(body_count) or 1))
  chunk_index = math.max(1, math.floor(tonumber(chunk_index) or 1))
  local first_row = index.body_row_list[body_start_line]
  if not first_row then return nil, nil, nil end
  local body_line_list = {}
  local section_index = first_row.section_index
  for row_number = body_start_line, math.min(#index.body_row_list, body_start_line + body_count - 1) do
    local row = index.body_row_list[row_number]
    if not row or row.section_index ~= section_index then return nil, nil, nil end
    body_line_list[#body_line_list + 1] = row.line
  end
  if #body_line_list == 0 then return nil, nil, nil end
  local section = index.section_list[section_index]
  if not section then return nil, nil, nil end
  local old_count, new_count = diff_line_count(body_line_list)
  local context_suffix = section.context and section.context ~= "" and (" " .. section.context) or ""
  local chunk_line_list = vim.deepcopy(index.header_line_list or {})
  chunk_line_list[#chunk_line_list + 1] = ("@@ -%d,%d +%d,%d @@%s"):format(
    first_row.old_line,
    old_count,
    first_row.new_line,
    new_count,
    context_suffix
  )
  vim.list_extend(chunk_line_list, body_line_list)
  local added, removed = diff_stats(body_line_list)
  local chunk = vim.deepcopy(index.hunk or {})
  chunk.diff = table.concat(chunk_line_list, "\n")
  chunk.pos = first_row.new_line
  chunk.added = added
  chunk.removed = removed
  chunk.lazy_estimate = nil
  chunk.raw_hunks = chunk.raw_hunks or (index.hunk and index.hunk.raw_hunks) or { index.hunk }
  local render_key = ("%s:lazy:%d:%d"):format(hunk_key, section_index, chunk_index)
  return chunk, render_key, {
    old = first_row.old_syntax_row,
    new = first_row.new_syntax_row,
  }
end

return M
