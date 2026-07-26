local M = {}

local display_text = require("diff_review.render.display_text")

---@class DiffReviewTaskTreeNode
---@field id string
---@field text string
---@field branch? boolean
---@field first_prefix? string
---@field continuation_prefix? string
---@field child_prefix? string
---@field heading_detail? string
---@field detail? string
---@field children? DiffReviewTaskTreeNode[]
---@field gap_before_children? boolean
---@field foldable? boolean
---@field default_folded? boolean
---@field source_line? integer
---@field target? table
---@field json_path? string
---@field path? string
---@field gap_after? boolean
---@field segments_for_line? fun(line: string, index: integer): table[]
---@field metadata? table

---@class DiffReviewTaskTreeRow
---@field id string
---@field text string
---@field segments table[]
---@field parent_id? string
---@field source_line? integer
---@field target? table
---@field json_path? string
---@field path? string
---@field fold_id? string
---@field fold_target_id? string
---@field default_folded? boolean
---@field ancestor_ids string[]
---@field metadata? table

---@param is_last boolean
---@return string
function M.branch(is_last)
  return is_last and "└─ " or "├─ "
end

---@param is_last boolean
---@return string
function M.continuation(is_last)
  return is_last and "   " or "│  "
end

---@param text string
---@param width integer
---@param first_prefix string
---@param continuation_prefix string
---@return string[]
function M.wrap(text, width, first_prefix, continuation_prefix)
  return display_text.wrap(text, width, first_prefix, continuation_prefix)
end

---@class DiffReviewTaskTreeParsedLine
---@field first_prefix string
---@field continuation_prefix string
---@field body string

---Parse one rendered task-tree line into semantic wrapping parts.
---@param line string
---@return DiffReviewTaskTreeParsedLine?
function M.parse_line(line)
  local prefix, branch, body = line:match("^(%s*[│ ]*)([├└]─%s+)(%S.*)$")
  if not prefix or not branch or not body then return nil end
  local continuation = branch:find("├", 1, true) and "│  " or "   "
  return {
    first_prefix = prefix .. branch,
    continuation_prefix = prefix .. continuation,
    body = body,
  }
end

---@param value string[]
---@param item string
---@return string[]
local function append_copy(value, item)
  local result = vim.deepcopy(value)
  result[#result + 1] = item
  return result
end

---@param node DiffReviewTaskTreeNode
---@param line string
---@param line_index integer
---@return table[]
local function line_segments(node, line, line_index)
  if node.segments_for_line then return node.segments_for_line(line, line_index) end
  return { { line } }
end

---@param row_list DiffReviewTaskTreeRow[]
---@param node DiffReviewTaskTreeNode
---@param sibling_index integer
---@param sibling_count integer
---@param width integer
---@param ancestor_prefix string
---@param parent_id string?
---@param ancestor_ids string[]
local function append_node(row_list, node, sibling_index, sibling_count, width, ancestor_prefix, parent_id, ancestor_ids)
  local is_last = sibling_index == sibling_count
  local has_branch = node.branch ~= false
  local first_prefix
  local continuation_prefix
  local child_prefix
  if has_branch then
    first_prefix = ancestor_prefix .. M.branch(is_last)
    continuation_prefix = ancestor_prefix .. M.continuation(is_last)
    child_prefix = continuation_prefix
  else
    first_prefix = ancestor_prefix .. (node.first_prefix or "")
    continuation_prefix = ancestor_prefix .. (node.continuation_prefix or node.first_prefix or "")
    child_prefix = ancestor_prefix
      .. (node.child_prefix or node.continuation_prefix or node.first_prefix or "")
  end

  local wrapped_line_list = M.wrap(node.text, math.max(width or 1, 1), first_prefix, continuation_prefix)
  if node.heading_detail and node.heading_detail ~= "" then
    vim.list_extend(
      wrapped_line_list,
      M.wrap(node.heading_detail, math.max(width or 1, 1), continuation_prefix, continuation_prefix)
    )
  end
  local node_ancestor_ids = append_copy(ancestor_ids, node.id)
  local child_list = node.children or {}
  local foldable = node.foldable ~= false and #child_list > 0
  for line_index, line in ipairs(wrapped_line_list) do
    local is_fold_anchor = foldable and line_index == #wrapped_line_list
    local is_identity_row = is_fold_anchor or (not foldable and line_index == 1)
    row_list[#row_list + 1] = {
      id = is_identity_row and node.id or ("%s:line:%d"):format(node.id, line_index),
      text = line,
      segments = line_segments(node, line, line_index),
      parent_id = parent_id,
      source_line = node.source_line,
      target = node.target,
      json_path = node.json_path,
      path = node.path,
      fold_id = is_fold_anchor and node.id or nil,
      fold_target_id = foldable and not is_fold_anchor and node.id or parent_id,
      default_folded = is_fold_anchor and node.default_folded == true or nil,
      ancestor_ids = vim.deepcopy(ancestor_ids),
      metadata = is_identity_row and node.metadata or nil,
    }
  end

  if node.detail and node.detail ~= "" then
    local detail_line_list = M.wrap(node.detail, math.max(width or 1, 1), child_prefix, child_prefix)
    for detail_index, detail_line in ipairs(detail_line_list) do
      row_list[#row_list + 1] = {
        id = ("%s:detail:%d"):format(node.id, detail_index),
        text = detail_line,
        segments = { { detail_line } },
        parent_id = node.id,
        source_line = node.source_line,
        target = node.target,
        json_path = node.json_path,
        path = node.path,
        fold_target_id = node.id,
        ancestor_ids = vim.deepcopy(node_ancestor_ids),
        metadata = node.metadata,
      }
    end
  end

  if node.gap_before_children and #child_list > 0 then
    row_list[#row_list + 1] = {
      id = node.id .. ":child-gap",
      text = "",
      segments = { { "" } },
      parent_id = node.id,
      source_line = node.source_line,
      target = node.target,
      json_path = node.json_path,
      path = node.path,
      fold_target_id = node.id,
      ancestor_ids = vim.deepcopy(node_ancestor_ids),
      metadata = node.metadata,
    }
  end

  for child_index, child in ipairs(child_list) do
    append_node(row_list, child, child_index, #child_list, width, child_prefix, node.id, node_ancestor_ids)
  end

  if node.gap_after then
    row_list[#row_list + 1] = {
      id = node.id .. ":gap",
      text = "",
      segments = { { "" } },
      parent_id = node.id,
      source_line = node.source_line,
      target = node.target,
      json_path = node.json_path,
      path = node.path,
      fold_target_id = node.id,
      ancestor_ids = vim.deepcopy(node_ancestor_ids),
      metadata = node.metadata,
    }
  end
end

---Render normalized task nodes into width-aware semantic rows.
---@param node_list DiffReviewTaskTreeNode[]
---@param width integer
---@return DiffReviewTaskTreeRow[]
function M.render(node_list, width)
  local row_list = {}
  for node_index, node in ipairs(node_list or {}) do
    append_node(row_list, node, node_index, #node_list, width, "", nil, {})
  end
  return row_list
end

return M
