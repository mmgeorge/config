local cursor_stack = {}
local selected_nodes = {}
local last_was_expand = false

local decorator_types = {
  -- rust
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

  if a_end_row < b_start_row then
    return b_start_row - a_end_row <= 1
  else
    return a_start_row - b_end_row <= 1
  end
end

local function find_sibling_left(node, predicate)
  while node do
    if (predicate(node)) then
      return node
    end

    local prev = node:prev_named_sibling()
    if prev and not joinable(prev, node) then
      return node
    end

    node = prev
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

    local next = node:next_named_sibling()
    if next and not joinable(next, node) then
      return node
    end

    node = next
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
  while i + 1 <= #line and line:sub(i + 1, i + 1):match(match) do
    i = i + 1
  end

  return i
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

local function get_selection_end(lines, node, at_start, is_list_arg)
  local _, _, end_row, end_col = node:range()
  local line                   = lines[end_row + 1]

  -- Grab punctuation immediately following the selection
  if at_start or is_list_arg then
    if line:sub(end_col + 1, end_col + 1):match("[,;%.%?]") then
      end_col = end_col + 1
    end
  end

  -- Grab other types of non-closing punctuation
  if line:sub(end_col + 1, end_col + 1):match("[!%?]") then
    end_col = end_col + 1
  end

  -- If we are at the end of the line, then include any trailing lines after
  --
  -- NOTE: avoid selecting newlines after comments as in the case where we don't have an
  -- attached node, its likely because we are commenting / uncommenting code
  if not is_comment(node) and search_line_after(line, end_col, '^%s*$') == #line then
    local i = end_row + 1
    while i < #lines and (lines[i + 1]:match('^%s*$') or lines[i + 1] == '') do
      end_row = i
      end_col = #lines[i + 1]
      i = i + 1
    end
  end

  -- If left of the line we have only whitespace, start at 0
  -- end_col = search_line_after(line, end_col + 1, "%s")

  -- Not sure why this adjustment is needed. Indexing? But if we don't adjust
  -- here, then for let x = [te|st, foo] will select the f?
  end_col = end_col - 1

  -- Finally, check if we should expand the selection to remove any trailing newlines.
  local end_line = lines[end_row + 1]
  if at_start and search_line_after(end_line, end_col + 1, '^%s*$') == #end_line then
    end_col = #end_line
  end

  return end_row, end_col
end

local function select_chain(lines, tree, node)
  print("select_chain")
  local start_row, start_col, end_row, end_col = node:range()
  local line = lines[start_row + 1]
  local did_capture_dot_start = false

  -- end_col = end_col - 1 -- node:range() is not inclusive?

  if line:sub(start_col, start_col):match("%.") then
    print("select_chain:capture_dot_left")
    start_col = start_col - 1
    did_capture_dot_start = true
  end

  -- arguments case: foo.b|az()
  if line:sub(end_col + 1, end_col + 1):match("%(") then
    local root = tree:root()
    local args = root:named_descendant_for_range(end_row, end_col, end_row, end_col)
    local _, _, args_end_row, args_end_col = args:range()
    local end_line = lines[args_end_row + 1]

    -- Should we capture the entire line?
    if search_line_before(line, start_col, "%s") == 0
        and search_line_after(end_line, args_end_col, '^%s*$') == #end_line then
      start_col = 0
      args_end_col = #end_line + 1 -- sub below
    end

    return select_region(node, start_row, start_col, args_end_row, args_end_col - 1)
    -- call(f|oo.value) select foo. if we are the first item
  elseif not did_capture_dot_start and line:sub(end_col + 1, end_col + 1):match("%.") then
    print("select_chain:capture_dot_right")
    end_col = end_col + 1
  end

  -- Should we capture the entire line?
  local end_line = lines[end_row + 1]
  if search_line_before(line, start_col, "%s") == 0
      and search_line_after(end_line, end_col, '^%s*$') == #end_line then
    start_col = 0
    end_col = #end_line + 1 -- sub below
  end

  -- no arguments case: foo.in|ner.baz()
  return select_region(node, start_row, start_col, end_row, end_col - 1)
end

local function select_node(bufnr, query, tree, node, is_list_arg, is_list, end_node)
  -- If the node is a comment, move to the last attached comment. We
  -- will then select all of them
  if is_comment(node) then
    node = find_last_sibling_right(node, is_comment)
  end

  local start_node = node
  end_node = end_node or node
  local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)

  -- Special handling for chain arguments, e.g, bax.baz().foo()
  if matches(query, "chain", node, bufnr) then
    return select_chain(lines, tree, node)
  end

  -- If we have a list, select the first and last child
  if is_list then
    if node:named_child_count() == 0 then
      return false
    end

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

  local start_row, start_col = get_selection_start(lines, start_node, is_list_arg)
  local end_row, end_col = get_selection_end(lines, end_node, start_col == 0, is_list_arg)

  return select_region(node, start_row, start_col, end_row, end_col)
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
    -- Create and return an empty query object if not found
    return vim.treesitter.query.parse(lang, "")
  end


  return query
end

local function get_tree(bufnr, lang)
  local parser = vim.treesitter.get_parser(bufnr, lang, { error = false })
  if not parser then
    vim.notify("Error: No Tree-sitter parser found for language: " .. lang, vim.log.levels.ERROR)
    return
  end

  -- Parse the current buffer's content. Select the first (and usually only) tree
  -- TODO: Handle multiple trees?
  local tree = parser:parse(true)[1]
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

function match_get(query, capture_name, node, bufnr)
  if not node then
    return nil
  end

  local index = get_capture_index(query, capture_name)
  for _, match in query:iter_matches(node, bufnr) do
    local matched_group = match[index]
    if matched_group then
      return matched_group[1]
    end
  end

  return nil
end

function matches_elseif(query, node, bufnr)
  if not node then
    return false
  end

  local index = get_capture_index(query, "elseifblock")
  for _, match in query:iter_matches(node, bufnr) do
    local matched_group = match[index]
    if matched_group and matched_group[1]:id() == node:id() then
      return true
    end
  end
  return false
end

function matches_parent(query, capture_name, node, bufnr)
  node = node:parent()

  while node do
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

    node = node:parent()
  end

  return false
end

function matches_in_parent(query, capture_name, node, parent, bufnr)
  if not node then
    return false
  end

  local index = get_capture_index(query, capture_name)
  for _, match in query:iter_matches(parent, bufnr) do
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
    -- If the previous node is an attachment type, find what it's anchored to
    if not next and is_attachment(node) then
      next = find_sibling_right(node, is_not_attachment)
    end

    -- We picked a branch of an if_else previous. Move up to the parent of the if_else.
    if not next and matches(query, "else", node, bufnr) then
      local parent = node:parent()
      while parent and not matches(query, "if_container", parent, bufnr) do
        parent = parent:parent()
      end

      next = parent
    end

    -- Did we select the same node?
    if next and next:id() == node:id() then
      next = nil
    end

    -- Otherwise, go up to the parent
    node = next or node:parent()
  end

  local did_select = false

  while node do
    if try_select_node(node, query, tree, bufnr) then
      return
    end

    node = node:parent()
  end
end

function try_select_node(node, query, tree, bufnr)
  local parent = node:parent();
  if not parent then
    -- At root
    return false
  end

  local start_row, start_col, end_row, end_col = node:range()
  -- Avoid selecting nodes that are a single char
  if start_row == end_row and start_col == end_col - 1 then
    return false
  elseif matches(query, "jump", node, bufnr) then
    return false
  elseif matches_in_parent(query, "jump", node, parent, bufnr) then
    return false
  elseif matches(query, "outer_only", node, bufnr) and matches_parent(query, "outer_only", node, bufnr) then
    return false
  elseif matches(query, "elseif", node, bufnr) then
    local end_node = match_get(query, "end", node, bufnr)
    return select_node(bufnr, query, tree, node, false, false, end_node)
  elseif matches(query, "if", node, bufnr) then
    local end_node = match_get(query, "end", node, bufnr)
    return select_node(bufnr, query, tree, node, false, false, end_node)
    -- Select a list arg
  elseif matches(query, "list", parent, bufnr) then
    return select_node(bufnr, query, tree, node, true)
    -- Select inner list
  elseif matches(query, "list", node, bufnr) then
    return select_node(bufnr, query, tree, node, false, true)
  else
    return select_node(bufnr, query, tree, node)
  end
end

--- Get the current visual selection (region) as start and end positions
--- @return table {start = {line, col}, finish = {line, col}}
-- local function get_visual_selection_region()
--   local start_pos = vim.api.nvim_buf_get_mark(0, '<')
--   local end_pos = vim.api.nvim_buf_get_mark(0, '>')
--   return start_pos[1], start_pos[2], end_pos[1], end_pos[2]
-- end
function select_region(node, start_row, start_col, end_row, end_col)
  local last = cursor_stack[#cursor_stack]
  if last and
      last.start_row == start_row and
      last.end_row == end_row and
      last.start_col == start_col and
      last.end_col == end_col then
    -- Ended up wi the same selection, try again
    return false
  end

  table.insert(selected_nodes, node)
  print(node:type())

  vim.api.nvim_buf_set_mark(0, '<', start_row + 1, start_col, {})
  vim.api.nvim_buf_set_mark(0, '>', end_row + 1, end_col, {})
  vim.cmd("normal! gvo")

  last_was_expand = true
  table.insert(cursor_stack, {
    start_row = start_row,
    end_row   = end_row,
    start_col = start_col,
    end_col   = end_col
  })

  return true
end

function undo_select_parent()
  if #cursor_stack ~= 0 then
    if last_was_expand then
      -- Pop the last
      last_was_expand = false
      table.remove(cursor_stack)
      table.remove(selected_nodes)
    end

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
