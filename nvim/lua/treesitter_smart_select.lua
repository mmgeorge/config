-- Rules to impl:
--   Mark @comment nodes
--     On select, first see if attached to !comment and !decorator. Respect newlines.
--     If so, select that
--     Otherwise, select last attached comment
--   Mark @decorator nodes: .
--     On select, first see if attached to !comment and !decorator. Respect newlines.
--     If so, select that
--
--   On select_node
--     Go backwards & select all decorator or comment. Respect newlines.

--   ** Maybe**? On expand, before navigating to parent, first select all siblings (respect newline)

-- if @list type, select all children first
--   set bool selected inner to true
--   on next select, select outer

local cursor_stack = {}
local selected_nodes = {}

-- Allows selecting lines and decorators. Otherwise, selection will automatically move
-- to the core node type (e.g., move to function, etc.)
local selecting_related_nodes_enabled = true

local related_node_types = {
  --rust
  "attribute_item",
  "line_comment",
  -- lua
  "comment",
}

local function is_related_node(node)
  for _, v in ipairs(related_node_types) do
    if v == node:type() then
      return true
    end
  end
  return false
end

--- Push the current visual selection or cursor position onto cursor_stack
local function remember_current_selection()
  local mode = vim.fn.mode()
  local start_row, start_col, end_row, end_col

  if mode == "v" or mode == "V" then
    -- Get the start and end of the visual selection
    local start_pos = vim.fn.getpos("'<")
    local end_pos   = vim.fn.getpos("'>")
    start_row       = start_pos[2] - 1
    start_col       = start_pos[3] - 1
    end_row         = end_pos[2] - 1
    end_col         = end_pos[3] - 1
    -- In visual-line, select all columns
    if mode == "V" then
      start_col = 0
      end_col = math.max(0, #vim.fn.getline(end_row + 1))
    end
    -- Ensure start is always before end
    if start_row > end_row or (start_row == end_row and start_col > end_col) then
      start_row, end_row = end_row, start_row
      start_col, end_col = end_col, start_col
    end
  else
    -- If not in visual mode, push current cursor position as zero-width selection
    local pos = vim.api.nvim_win_get_cursor(0)
    start_row = pos[1] - 1
    start_col = pos[2]
    end_row   = start_row
    end_col   = start_col
  end


  table.insert(cursor_stack, {
    start_row = start_row,
    end_row   = end_row,
    start_col = start_col,
    end_col   = end_col
  })
end

local augroup_name = "ClearCursorStackOnVisualLeave"
vim.api.nvim_create_augroup(augroup_name, { clear = true })

--- Create an autocommand to clear global 'cursor_stack' on Visual mode exit, only once.
local function setup_visual_exit_autocmd()
  vim.api.nvim_create_autocmd("ModeChanged", {
    group = augroup_name,
    pattern = "v:*",
    desc = "Clear global cursor_stack and remove self on leaving visual mode.",
    callback = function(args)
      selected_nodes = {}
      cursor_stack = {}
    end,
    once = true, -- Ensure it triggers only once
  })
end

-- For the given node, get the start of the left-most relevant sibling.
-- Relevant siblings include comments, macros, and decorators
local function related_leading_sibling_start(node)
  local start_row, start_col = node:range()
  local sibling = node:prev_named_sibling()

  while sibling do
    if is_related_node(sibling) then
      local s_row, s_col = sibling:range()
      start_row = s_row
      start_col = s_col
    else
      break
    end
    sibling = sibling:prev_named_sibling()
  end

  return start_row, start_col
end

-- For the given node (should pass is_related_node), get the last rightward sibling that is not a
-- related node type.
local function first_core_right_sibling(node)
  local sibling = node

  while sibling and is_related_node(sibling) do
    sibling = sibling:next_named_sibling()
  end

  return sibling
end

local function has_trailing_whitespace_only(line, e)
  while e <= #line do
    local c = line:sub(e + 1, e + 1)
    -- if not c:match("%s") then
    if not c:match('^%s*$') then
      return false
    else
      e = e + 1
    end
  end

  return true
end

local function has_leading_whitespace_only(line, s)
  while s > 0 do
    if not line:sub(s, s):match("%s") then
      return false
    end
    s = s - 1
  end

  return true
end


--- Determines if a Tree-sitter node is the last child in its parent (has no right siblings)
---@param node userdata TSNode
---@return boolean
local function is_last_child(node)
  local parent = node:parent()
  if not parent then
    return false
  end

  local last_child = parent:child(parent:child_count() - 1)
  print("last", vim.inspect(last_child:id(), node:id()))
  return last_child:id() == node:id()
end


local function select_node(node, is_list_arg)
  table.insert(selected_nodes, node)
  local ts_utils = require("nvim-treesitter.ts_utils")
  -- local start_row, start_col, end_row, end_col = node:range()
  local _, _, end_row, end_col = node:range()
  local start_row, start_col = related_leading_sibling_start(node)

  -- Get buffer lines
  local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
  local last_buf_line = #lines

  local match = nil;
  if has_leading_whitespace_only(lines[start_row + 1], start_col) then
    match = "%s";
  end

  if is_list_arg then
    if not node:next_named_sibling() then
      match = "[%s,]"
    end
  end

  if match then
    local line = lines[start_row + 1] or ""
    local s = start_col
    while s > 0 and line:sub(s, s):match(match) do
      s = s - 1
    end
    start_col = s
  end

  -- Extend end_col to include any trailing , ; . or ?
  -- local end_line = lines[end_row + 1] or ""
  -- local e = end_col
  -- -- -- cover the case where end_col might be at the last char, extend if needed
  -- while e <= #end_line do
  --   local c = end_line:sub(e + 1, e + 1)
  --   if c:match("[%s,;%.%?]") then
  --     e = e + 1
  --   else
  --     break
  --   end
  -- end
  -- end_col = e

  -- Grab punctuation immediately following the selection
  local end_line = lines[end_row + 1] or ""
  local c = end_line:sub(end_col + 1, end_col + 1)
  if c:match("[,;%.%?]") then
    end_col = end_col + 1
  end

  -- Extend end_col to include any trailing spaces
  if is_list_arg or has_trailing_whitespace_only(lines[end_row + 1], end_col + 1) then
    local end_line = lines[end_row + 1] or ""
    local e = end_col
    -- -- cover the case where end_col might be at the last char, extend if needed
    while e <= #end_line do
      local c = end_line:sub(e + 1, e + 1)
      if c:match("%s") then
        e = e + 1
      else
        break
      end
    end
    end_col = e
  end

  -- Extend end_row to include trailing empty lines and newlines after the node
  if has_trailing_whitespace_only(lines[end_row + 1], end_col + 1) then
    local i = end_row + 1
    while i < last_buf_line and (lines[i + 1]:match('^%s*$') or lines[i + 1] == '') do
      end_row = i
      end_col = #lines[i + 1]
      i = i + 1
    end
  end

  -- Not sure why this adjustment is needed. Indexing? But if we don't adjust
  -- here, then for let x = [te|st, foo] will select the f?
  end_col = end_col - 1

  -- Finally, check if we should expand the selection to remove any trailing newlines.
  local end_line = lines[end_row + 1] or ""
  local has_leading_whitespace_only = has_leading_whitespace_only(lines[start_row + 1], start_col - 1)
  if has_leading_whitespace_only and end_col + 1 == #end_line then
    end_col = #end_line
  end

  vim.api.nvim_buf_set_mark(0, '<', start_row + 1, start_col, {})
  vim.api.nvim_buf_set_mark(0, '>', end_row + 1, end_col, {})
  vim.cmd("normal! gvo")
  remember_current_selection()
end


function get_closest_node(tree)
  local bufnr = vim.api.nvim_get_current_buf()
  local winid = vim.api.nvim_get_current_win()
  local cursor_pos = vim.api.nvim_win_get_cursor(winid) -- {row, col}, 1-indexed
  local cursor_row = cursor_pos[1] - 1

  -- Move cursor_col to the first non-whitespace character to the right
  -- We need this due to how we expand the selection to include leading whitespace
  local line = vim.api.nvim_get_current_line()
  local col = cursor_pos[2] -- Lua indices are 1-based, but Neovim columns are 0-based
  local next_non_ws = line:find("%S", col + 1)
  local cursor_col = next_non_ws and (next_non_ws - 1) or cursor_pos[2]

  local node
  local selected_node = selected_nodes[#selected_nodes]

  if selected_node then
    if is_related_node(selected_node) then
      node = first_core_right_sibling(selected_node)
    else
      node = selected_node:parent()
    end
  else
    local root = tree:root()
    local desc_node = root:named_descendant_for_range(cursor_row, cursor_col, cursor_row, cursor_col)
    if not desc_node then
      -- Fallback to any descendant (named or anonymous) if no specific named one is at the exact point/range
      desc_node = root:descendant_for_range(cursor_row, cursor_col, cursor_row, cursor_col)
    end

    if not desc_node then
      vim.notify(
        "Error: Could not find a Tree-sitter node at the cursor position. Position: r" ..
        cursor_row .. " c" .. cursor_col,
        vim.log.levels.ERROR
      )
      return
    end
    node = first_core_right_sibling(desc_node)
  end

  return node
end

function query_for(lang)
  -- Load the 'parent' query for the current language.
  -- This expects a file like 'queries/rust/parent.scm' to be in your runtimepath.
  local query = vim.treesitter.query.get(lang, "parent")
  if not query or query == "" then
    vim.notify(
      "Error: Could not load query 'parent' for language '" .. lang .. "'. " ..
      "Ensure queries/" ..
      lang .. "/parent.scm exists in your Neovim runtime path (e.g., ~/.config/nvim/queries/" ..
      lang .. "/parent.scm). " ..
      "Query content was nil or empty.",
      vim.log.levels.ERROR
    )
    return
  end

  return query
end

local function get_tree(bufnr, lang)
  local parser = vim.treesitter.get_parser(bufnr, lang)
  if not parser then
    vim.notify("Error: No Tree-sitter parser found for language: " .. lang, vim.log.levels.ERROR)
    return
  end

  -- Parse the current buffer's content. Select the first (and usually only) tree
  -- TODO: Handle multiple trees?
  local tree = parser:parse()[1]
  if not tree then
    vim.notify("Error: Could not parse the buffer.", vim.log.levels.ERROR)
    return
  end

  return tree
end

local function get_capture_index(query, capture_name)
  for i, name in ipairs(query.captures) do
    if name == capture_name then
      return i
    end
  end
end

function matches(query, capture_name, node, bufnr)
  if not node then
    return false
  end

  local index = get_capture_index(query, capture_name)
  for _, match in query:iter_matches(node, bufnr) do
    local matched_group = match[index]
    if matched_group and matched_group[1]:id() == node:id() then
      return true
    end
  end
  return false
end

-- Lua function to select the nearest parent node based on a Tree-sitter query.
--
-- This function assumes you have a query file named 'parent.scm'
-- (e.g., queries/rust/parent.scm) that defines what a "parent" node is.
function select_parent()
  setup_visual_exit_autocmd()

  local bufnr = vim.api.nvim_get_current_buf()
  local ftype = vim.bo[bufnr].filetype
  local lang = require("nvim-treesitter.parsers").ft_to_lang(ftype)
  local query = query_for(lang)
  local tree = get_tree(bufnr, lang)
  local node = get_closest_node(tree)

  while node do
    local parent = node:parent();
    -- if matches(query, "list_arg", node, bufnr) and matches(query, "list", parent, bufnr) then
    if matches(query, "list", parent, bufnr) then
      select_node(node, true)
      return
    end

    if matches(query, "parent", node, bufnr) then
      if not selecting_related_nodes_enabled then
        node = first_core_right_sibling(node)
      end

      -- In some cases, we perfer to always jump to the parent of the
      -- current node depending on the lang
      if matches(query, "jump", parent, bufnr) then
        node = parent
      end

      select_node(node)
      return
    end

    node = node:parent()
  end
end

function undo_select_parent()
  if #cursor_stack ~= 0 then
    local cursor = table.remove(cursor_stack)
    table.remove(selected_nodes)
    vim.api.nvim_buf_set_mark(0, '<', cursor.start_row + 1, cursor.start_col, {})
    vim.api.nvim_buf_set_mark(0, '>', cursor.end_row + 1, cursor.end_col, {})
    vim.cmd("normal! gvo")
  end
end

return {
  select_parent = select_parent,
  undo_select_parent = undo_select_parent
}
