local layout = require("diff_review.layout")

---@class DiffReviewRowTreeNode
---@field key string
---@field row_count integer
---@field metadata table?

---@class DiffReviewRowTree
---@field node_list DiffReviewRowTreeNode[]
---@field line_list table[]
---@field entry_by_line table<integer, table>
---@field highlight_list table[]
---@field line_highlight_list table[]
---@field extmark_list table[]
---@field boundary_by_line table<integer, any>
---@field content_length_by_line table<integer, integer>
---@field layout DiffReviewViewLayout
---@field node_by_key table<string, DiffReviewRowTreeNode>
---@field revision integer

---@class DiffReviewRowTreeModule
local M = {}

---@param value any
---@return integer
local function row_count(value)
  return math.max(0, math.floor(tonumber(value) or 0))
end

---@param node DiffReviewRowTreeNode
---@param item_index integer
---@param row_start integer
---@return DiffReviewLayoutSpan
local function span_from_node(node, item_index, row_start)
  return {
    key = node.key,
    start_row = row_start,
    height = node.row_count,
    metadata = node.metadata or {},
    item_index = item_index,
  }
end

---@param tree DiffReviewRowTree
local function sync_block_span(tree)
  for item_index, node in ipairs(tree.node_list or {}) do
    local key = tree.layout and tree.layout.key_by_item and tree.layout.key_by_item[item_index] or nil
    local span = key and tree.layout.span_by_key[key] or nil
    local block = node.metadata and node.metadata.block or nil
    if span and block then
      block.logical_start_line = span.start_row
      block.logical_count = span.height
    end
  end
end

---@param tree DiffReviewRowTree
function M.rebuild(tree)
  local key_by_item = {}
  local height_by_item = {}
  tree.node_by_key = {}
  for _, node in ipairs(tree.node_list or {}) do
    key_by_item[#key_by_item + 1] = node.key
    height_by_item[#height_by_item + 1] = row_count(node.row_count)
    tree.node_by_key[node.key] = node
  end
  tree.layout = layout.new_view_layout(key_by_item, height_by_item)
  for item_index, node in ipairs(tree.node_list or {}) do
    local row_start = layout.row_start_for_item(tree.layout.row_index, item_index) or 1
    tree.layout.span_by_key[node.key] = span_from_node(node, item_index, row_start)
  end
  sync_block_span(tree)
  tree.revision = (tree.revision or 0) + 1
end

---@param opts? table
---@return DiffReviewRowTree
function M.new(opts)
  opts = opts or {}
  local tree = {
    node_list = opts.node_list or {},
    line_list = opts.line_list or {},
    entry_by_line = opts.entry_by_line or {},
    highlight_list = opts.highlight_list or {},
    line_highlight_list = opts.line_highlight_list or {},
    extmark_list = opts.extmark_list or {},
    boundary_by_line = opts.boundary_by_line or {},
    content_length_by_line = opts.content_length_by_line or {},
    layout = layout.new_view_layout({}, {}),
    node_by_key = {},
    revision = 0,
  }
  M.rebuild(tree)
  return tree
end

---@param opts table
---@return DiffReviewRowTree
function M.from_status_state(opts)
  opts = opts or {}
  local line_list = opts.lines or {}
  local node_list = {}
  local line_number = 1
  while line_number <= #line_list do
    local line_value = line_list[line_number]
    local entry = opts.entries and opts.entries[line_number] or nil
    local block = type(line_value) == "table" and line_value.__diff_review_lazy_hunk and line_value.block or nil
    if block then
      local consumed = 1
      while line_number + consumed <= #line_list do
        local next_value = line_list[line_number + consumed]
        local next_block = type(next_value) == "table" and next_value.__diff_review_lazy_hunk and next_value.block or nil
        if next_block ~= block then break end
        consumed = consumed + 1
      end
      local count = math.max(1, row_count(block.logical_count or block.estimate or 1))
      local key = block.render_key or block.hunk_key or (entry and entry.id) or ("lazy:" .. tostring(line_number))
      node_list[#node_list + 1] = {
        key = tostring(key),
        row_count = count,
        metadata = {
          line = line_number,
          entry = entry,
          block = block,
        },
      }
      line_number = line_number + consumed
    else
      local key = entry and entry.id or ("line:" .. tostring(line_number))
      node_list[#node_list + 1] = {
        key = tostring(key),
        row_count = 1,
        metadata = {
          line = line_number,
          entry = entry,
        },
      }
      line_number = line_number + 1
    end
  end
  return M.new({
    node_list = node_list,
    line_list = line_list,
    entry_by_line = opts.entries or {},
    highlight_list = opts.highlights or {},
    line_highlight_list = opts.line_highlights or {},
    extmark_list = opts.extmarks or {},
    boundary_by_line = opts.boundary_lines or {},
    content_length_by_line = opts.content_lengths or {},
  })
end

---@param tree DiffReviewRowTree
---@param key string
---@return DiffReviewLayoutSpan?
function M.span(tree, key)
  return tree and tree.layout and tree.layout.span_by_key[tostring(key)] or nil
end

---@param tree DiffReviewRowTree
---@param key string
---@param node_list DiffReviewRowTreeNode[]
function M.replace_node(tree, key, node_list)
  if not tree then return end
  key = tostring(key)
  local replacement = vim.deepcopy(node_list or {})
  for item_index, node in ipairs(tree.node_list or {}) do
    if node.key == key then
      table.remove(tree.node_list, item_index)
      for replacement_index = #replacement, 1, -1 do
        table.insert(tree.node_list, item_index, replacement[replacement_index])
      end
      M.rebuild(tree)
      return
    end
  end
end

---@param tree DiffReviewRowTree
---@param key string
---@param row_count_value integer
function M.set_row_count(tree, key, row_count_value)
  if not tree then return end
  key = tostring(key)
  local node = tree.node_by_key and tree.node_by_key[key] or nil
  if not node then
    for _, candidate in ipairs(tree.node_list or {}) do
      if candidate.key == key then
        node = candidate
        break
      end
    end
  end
  if not node then return end
  node.row_count = row_count(row_count_value)
  layout.set_span_height(tree.layout, key, node.row_count)
  local span = tree.layout.span_by_key[key]
  if span then span.metadata = node.metadata or span.metadata end
  sync_block_span(tree)
  tree.revision = (tree.revision or 0) + 1
end

---@param tree DiffReviewRowTree
---@param first_row integer
---@param last_row integer
---@return DiffReviewLayoutSpan[]
function M.spans_in_range(tree, first_row, last_row)
  if not (tree and tree.layout) then return {} end
  return layout.spans_in_row_range(tree.layout, first_row, last_row)
end

---@param tree DiffReviewRowTree
---@param row integer
---@return DiffReviewLayoutSpan?
---@return DiffReviewRowTreeNode?
function M.span_at_row(tree, row)
  if not (tree and tree.layout and tree.layout.row_index) then return nil, nil end
  local item_index = layout.item_at_row(tree.layout.row_index, row)
  local key = item_index and tree.layout.key_by_item[item_index] or nil
  local span = key and tree.layout.span_by_key[key] or nil
  local node = item_index and tree.node_list and tree.node_list[item_index] or nil
  return span, node
end

---@param tree DiffReviewRowTree
---@return table<string, integer>
function M.entry_line_index(tree)
  local line_by_id = {}
  if not (tree and tree.layout) then return line_by_id end
  for item_index, node in ipairs(tree.node_list or {}) do
    local key = tree.layout.key_by_item[item_index]
    local span = key and tree.layout.span_by_key[key] or nil
    local entry = node.metadata and node.metadata.entry or nil
    if span and entry and entry.id and line_by_id[entry.id] == nil then
      line_by_id[entry.id] = span.start_row
    end
    local block = node.metadata and node.metadata.block or nil
    if span and block and block.hunk_key and line_by_id[block.hunk_key] == nil then
      line_by_id[block.hunk_key] = span.start_row
    end
  end
  return line_by_id
end

return M
