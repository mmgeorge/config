local cursor_stack = {}
local selected_nodes = {}

-- Allows selecting lines and decorators. Otherwise, selection will automatically move
-- to the core node type (e.g., move to function, etc.)
local selecting_related_nodes_enabled = true

local decorator_types = {
  --rust
  "attribute_item",
}

local function is_comment(node)
  return node:type():find("comment", 1, true) ~= nil
end


local function is_attachment(node)
  if is_comment(node) then
    return true
  end

  for _, v in ipairs(decorator_types) do
    if v == node:type() then
      return true
    end
  end

  return false
end

local function is_not_attachment(node)
  return not is_attachment(node)
end

local function joinable(a, b)
  local a_start_row, _, a_end_row, _ = a:range()
  local b_start_row, _, b_end_row, _ = b:range()

  if a_start_row < b_start_row then
    return b_start_row - a_end_row <= 1
  else
    return a_end_row - b_start_row <= 1
  end
end

local function find_sibling_left(node, predicate)
  while node do
    if (predicate(node)) then
      return node
    end
    node = node:prev_named_sibling()
  end

  return nil
end

local function find_last_sibling_left(node, predicate)
  local next = node:prev_named_sibling()

  while next and joinable(node, next) and predicate(next) do
    node = next
    next = node:prev_named_sibling()
  end

  return node
end

local function find_sibling_right(node, predicate)
  while node do
    if (predicate(node)) then
      return node
    end
    node = node:next_named_sibling()
  end

  return nil
end

local function find_last_sibling_right(node, predicate)
  local next = node:next_named_sibling()

  while next and joinable(node, next) and predicate(next) do
    node = next
    next = node:next_named_sibling()
  end

  return node
end

local function search_line_before(line, i, match)
  while i > 0 and line:sub(i, i):match(match) do
    i = i - 1
  end
  return i
end

local function search_line_after(line, i, match)
  while i <= #line and line:sub(i, i):match('^%s*$') do
    i = i + 1
  end

  return i
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

local function get_selection_start(lines, node, is_list_arg)
  local start_row, start_col = node:range()
  local line = lines[start_row + 1]

  -- If are the last list arg, expand to capture any preceeding ,
  if is_list_arg and not node:next_named_sibling() then
    return start_row, search_line_before(line, start_col, "[%s,]")
  end

  -- If left of the line we have only whitespace, start at 0
  if search_line_before(line, start_col, "%s") == 0 then
    start_col = 0
  end

  return start_row, start_col
end

local function get_selection_end(lines, node, at_start)
  local _, _, end_row, end_col = node:range()
  local line                   = lines[end_row + 1]

  -- Grab punctuation immediately following the selection
  if line:sub(end_col + 1, end_col + 1):match("[,;%.%?]") then
    end_col = end_col + 1
  end

  -- Extend end_col to include any trailing spaces
  -- if is_list_arg or has_trailing_whitespace_only(line, end_col + 1) then
  --   end_col = search_after(line, end_col + 1, "%s")
  -- end

  -- If we are at the end of the line, then include any trailing lines after
  if search_line_after(line, end_col, '^%s*$') == #line then
    local i = end_row + 1
    while i < #lines and (lines[i + 1]:match('^%s*$') or lines[i + 1] == '') do
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
  if at_start and end_col + 1 == #end_line then
    end_col = #end_line
  end

  return end_row, end_col
end


local function select_node(node, is_list_arg, is_list)
  table.insert(selected_nodes, node)

  local start_node = node
  local end_node = node

  -- If we have a list, select the first and last child
  if is_list then
    start_node = node:named_child(0)
    end_node = node:named_child(node:named_child_count() - 1)
  end

  if is_not_attachment(start_node) then
    -- Grab any attachments that preceed the selected node
    start_node = find_last_sibling_left(start_node, is_attachment)
  elseif is_comment(start_node) then
    -- Grab any preceding comments
    start_node = find_last_sibling_left(start_node, is_comment)
  end

  local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
  local start_row, start_col = get_selection_start(lines, start_node, is_list_arg)
  local end_row, end_col = get_selection_end(lines, end_node, start_col == 0)

  vim.api.nvim_buf_set_mark(0, '<', start_row + 1, start_col, {})
  vim.api.nvim_buf_set_mark(0, '>', end_row + 1, end_col, {})
  vim.cmd("normal! gvo")
  remember_current_selection()
end

function find_closest_node(tree)
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
  local root = tree:root()
  local node = root:named_descendant_for_range(cursor_row, cursor_col, cursor_row, cursor_col)
  if not node then
    vim.notify(
      "Error: Could not find a Tree-sitter node at the cursor position. Position: r" ..
      cursor_row .. " c" .. cursor_col,
      vim.log.levels.ERROR
    )
    return
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

  local node = selected_nodes[#selected_nodes]
  if not node then
    node = find_closest_node(tree)
  else
    local next = nil

    -- If the node is a comment, try to select any attached comments
    -- if is_comment(node) then
    --   last = find_last_sibling_right(node, is_comment)
    --   if node:id() ~= last:id() then
    --     next = last
    --   end
    -- end

    -- If the previous node is an attachment type, find what it's anchored to
    if not next and is_attachment(node) then
      next = find_sibling_right(node, is_not_attachment)
    end

    -- Otherwise, go up to the parent
    node = next or node:parent()
  end

  while node do
    local parent = node:parent();
    if matches(query, "list", parent, bufnr) then
      return select_node(node, true)
    end

    if matches(query, "list", node, bufnr) then
      return select_node(node, false, true)
    end

    if matches(query, "parent", node, bufnr) then
      -- In some cases, we prefer to always jump to the parent of the
      -- current node depending on the lang
      if matches(query, "jump", parent, bufnr) then
        node = parent
      end

      -- If the node is a comment, move to the last attached comment. We
      -- will then select all of them
      if is_comment(node) then
        node = find_last_sibling_right(node, is_comment)
      end

      return select_node(node)
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
