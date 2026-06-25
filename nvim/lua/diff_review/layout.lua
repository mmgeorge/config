---@class DiffReviewRowHeightIndex
---@field count integer
---@field total integer
---@field height_by_item integer[]
---@field tree integer[]

---@class DiffReviewLayoutSpan
---@field key string
---@field start_row integer
---@field height integer
---@field metadata table?

---@class DiffReviewViewLayout
---@field row_index DiffReviewRowHeightIndex
---@field span_by_key table<string, DiffReviewLayoutSpan>
---@field key_by_item string[]
---@field item_index_by_key table<string, integer>

---@class DiffReviewFileBodyBlock
---@field key string
---@field height integer
---@field kind string?
---@field metadata table?

---@class DiffReviewFileBodyLayout
---@field blocks DiffReviewFileBodyBlock[]
---@field view DiffReviewViewLayout
---@field block_by_key table<string, DiffReviewFileBodyBlock>

---@class DiffReviewLayoutModule
local M = {}

---@param value any
---@return integer
local function normalized_height(value)
  return math.max(0, math.floor(tonumber(value) or 0))
end

---@param index DiffReviewRowHeightIndex
---@param item_index integer
---@param delta integer
function M.row_index_add(index, item_index, delta)
  while item_index <= index.count do
    index.tree[item_index] = (index.tree[item_index] or 0) + delta
    item_index = item_index + bit.band(item_index, -item_index)
  end
end

---@param heights integer[]?
---@return DiffReviewRowHeightIndex
function M.new_row_height_index(heights)
  local index = {
    count = 0,
    total = 0,
    height_by_item = {},
    tree = {},
  }
  M.rebuild_row_height_index(index, heights or {})
  return index
end

---@param index DiffReviewRowHeightIndex
---@param heights integer[]
function M.rebuild_row_height_index(index, heights)
  index.count = #heights
  index.total = 0
  index.height_by_item = {}
  index.tree = {}

  for item_index, height in ipairs(heights) do
    local normalized = normalized_height(height)
    index.height_by_item[item_index] = normalized
    index.total = index.total + normalized
    M.row_index_add(index, item_index, normalized)
  end
end

---@param index DiffReviewRowHeightIndex
---@param item_index integer
---@return integer
function M.prefix_sum(index, item_index)
  item_index = math.min(index.count, math.max(0, math.floor(tonumber(item_index) or 0)))
  local sum = 0
  while item_index > 0 do
    sum = sum + (index.tree[item_index] or 0)
    item_index = item_index - bit.band(item_index, -item_index)
  end
  return sum
end

---@param index DiffReviewRowHeightIndex
---@param item_index integer
---@param height integer
function M.set_height(index, item_index, height)
  item_index = math.floor(tonumber(item_index) or 0)
  if item_index < 1 or item_index > index.count then return end
  local next_height = normalized_height(height)
  local previous_height = index.height_by_item[item_index] or 0
  local delta = next_height - previous_height
  if delta == 0 then return end
  index.height_by_item[item_index] = next_height
  index.total = index.total + delta
  M.row_index_add(index, item_index, delta)
end

---@param index DiffReviewRowHeightIndex
---@param row integer
---@return integer?
function M.item_at_row(index, row)
  row = math.floor(tonumber(row) or 0)
  if row < 1 or row > index.total then return nil end

  local item_index = 0
  local bit_mask = 1
  while bit_mask * 2 <= index.count do
    bit_mask = bit_mask * 2
  end

  local remaining = row
  while bit_mask > 0 do
    local next_index = item_index + bit_mask
    if next_index <= index.count and (index.tree[next_index] or 0) < remaining then
      item_index = next_index
      remaining = remaining - (index.tree[next_index] or 0)
    end
    bit_mask = math.floor(bit_mask / 2)
  end

  return item_index + 1
end

---@param index DiffReviewRowHeightIndex
---@param item_index integer
---@return integer?
function M.row_start_for_item(index, item_index)
  item_index = math.floor(tonumber(item_index) or 0)
  if item_index < 1 or item_index > index.count then return nil end
  return M.prefix_sum(index, item_index - 1) + 1
end

---@param index DiffReviewRowHeightIndex
---@param item_index integer
---@return integer?
function M.row_end_for_item(index, item_index)
  item_index = math.floor(tonumber(item_index) or 0)
  if item_index < 1 or item_index > index.count then return nil end
  return M.prefix_sum(index, item_index)
end

---@param key_by_item string[]?
---@param height_by_item integer[]?
---@return DiffReviewViewLayout
function M.new_view_layout(key_by_item, height_by_item)
  key_by_item = key_by_item or {}
  height_by_item = height_by_item or {}
  local layout = {
    row_index = M.new_row_height_index(height_by_item),
    span_by_key = {},
    key_by_item = vim.deepcopy(key_by_item),
    item_index_by_key = {},
  }
  M.rebuild_span(layout)
  return layout
end

---@param layout DiffReviewViewLayout
function M.rebuild_span(layout)
  layout.span_by_key = {}
  layout.item_index_by_key = {}
  for item_index, key in ipairs(layout.key_by_item or {}) do
    layout.item_index_by_key[key] = item_index
    local start_row = M.row_start_for_item(layout.row_index, item_index)
    local end_row = M.row_end_for_item(layout.row_index, item_index)
    if start_row and end_row then
      layout.span_by_key[key] = {
        key = key,
        start_row = start_row,
        height = math.max(0, end_row - start_row + 1),
      }
    end
  end
end

---@param layout DiffReviewViewLayout
---@param key string
---@return integer?
function M.item_index(layout, key)
  if not layout.item_index_by_key then M.rebuild_span(layout) end
  return layout.item_index_by_key and layout.item_index_by_key[key] or nil
end

---@param layout DiffReviewViewLayout
---@param key string
---@param height integer
function M.set_span_height(layout, key, height)
  local item_index = M.item_index(layout, key)
  if not item_index then return end
  local previous_height = layout.row_index.height_by_item[item_index] or 0
  M.set_height(layout.row_index, item_index, height)
  local next_height = layout.row_index.height_by_item[item_index] or 0
  local delta = next_height - previous_height
  if delta == 0 then return end
  for index = item_index, #(layout.key_by_item or {}) do
    local item_key = layout.key_by_item[index]
    local span = layout.span_by_key[item_key]
    if span then
      if index == item_index then
        span.height = next_height
      else
        span.start_row = span.start_row + delta
      end
    end
  end
end

---@param blocks DiffReviewFileBodyBlock[]?
---@return DiffReviewFileBodyLayout
function M.new_file_body_layout(blocks)
  local key_by_item = {}
  local height_by_item = {}
  local block_by_key = {}
  local body_blocks = {}
  for _, block in ipairs(blocks or {}) do
    local key = tostring(block.key)
    local copy = vim.deepcopy(block)
    copy.key = key
    copy.height = normalized_height(copy.height)
    body_blocks[#body_blocks + 1] = copy
    block_by_key[key] = copy
    key_by_item[#key_by_item + 1] = key
    height_by_item[#height_by_item + 1] = copy.height
  end
  return {
    blocks = body_blocks,
    view = M.new_view_layout(key_by_item, height_by_item),
    block_by_key = block_by_key,
  }
end

---@param body_layout DiffReviewFileBodyLayout
---@param key string
---@param height integer
function M.set_block_height(body_layout, key, height)
  local block = body_layout.block_by_key and body_layout.block_by_key[key] or nil
  if block then block.height = normalized_height(height) end
  M.set_span_height(body_layout.view, key, height)
end

---@param view_layout DiffReviewViewLayout
---@param first_row integer
---@param last_row integer
---@return DiffReviewLayoutSpan[]
function M.spans_in_row_range(view_layout, first_row, last_row)
  local span_list = {}
  first_row = math.max(1, math.floor(tonumber(first_row) or 1))
  last_row = math.max(first_row, math.floor(tonumber(last_row) or first_row))
  local first_item = M.item_at_row(view_layout.row_index, first_row)
  local last_item = M.item_at_row(view_layout.row_index, math.min(last_row, view_layout.row_index.total))
  if not (first_item and last_item) then return span_list end
  for item_index = first_item, last_item do
    local key = view_layout.key_by_item[item_index]
    local span = key and view_layout.span_by_key[key] or nil
    if span then span_list[#span_list + 1] = span end
  end
  return span_list
end

---@param body_layout DiffReviewFileBodyLayout
---@param first_row integer
---@param last_row integer
---@return DiffReviewLayoutSpan[]
function M.body_spans_in_row_range(body_layout, first_row, last_row)
  return M.spans_in_row_range(body_layout.view, first_row, last_row)
end

return M
