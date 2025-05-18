
local M = {}

M.get_uuid = function(opts)
  local templates = {
    v4 = "xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx",
  }

  local uuid = string.gsub(templates.v4, "[xy]", function(c)
    local r = math.random()

    -- if c == 'x', generate a random hex digit (0-15)
    -- if c == 'y', generate a random hex digit (8-11)
    local v = c == "x" and math.floor(r * 0x10) or (math.floor(r * 0x4) + 8)

    return string.format("%x", v)
  end)

  -- Convert to upper case if requested (always lower case by default)
  -- if opts.case == "upper" then
  --   uuid = string.upper(uuid)
  -- end
  --
  -- if opts.quotes == "double" then
  --   uuid = '"' .. uuid .. '"'
  -- elseif opts.quotes == "single" then
  --   uuid = "'" .. uuid .. "'"
  -- end
  --
  -- if opts.prefix ~= "" then
  --   uuid = opts.prefix .. uuid
  -- end
  --
  -- if opts.suffix ~= "" then
  --   uuid = uuid .. opts.suffix
  -- end
  --
  return uuid
end

M.insert_v4 = function(opts)
  local uuid = M.get_uuid(opts)
  local after = true

  vim.api.nvim_put({ uuid }, "c", after, true)
end

local function tbl_length(T)
  local count = 0
  for _ in pairs(T) do count = count + 1 end
  return count
end

-- See https://github.com/ibhagwan/fzf-lua/blob/6ee73fdf2a79bbd74ec56d980262e29993b46f2b/lua/fzf-lua/utils.lua
M.get_visual_selection = function ()
  -- this will exit visual mode
  -- use 'gv' to reselect the text
  local _, csrow, cscol, cerow, cecol
  local mode = vim.fn.mode()
  if mode == "v" or mode == "V" or mode == "" then
    -- if we are in visual mode use the live position
    _, csrow, cscol, _ = unpack(vim.fn.getpos("."))
    _, cerow, cecol, _ = unpack(vim.fn.getpos("v"))
    if mode == "V" then
      -- visual line doesn't provide columns
      cscol, cecol = 0, 999
    end
    -- exit visual mode
    vim.api.nvim_feedkeys(
      vim.api.nvim_replace_termcodes("<Esc>",
        true, false, true), "n", true)
  else
    -- otherwise, use the last known visual position
    _, csrow, cscol, _ = unpack(vim.fn.getpos("'<"))
    _, cerow, cecol, _ = unpack(vim.fn.getpos("'>"))
  end
  -- swap vars if needed
  if cerow < csrow then csrow, cerow = cerow, csrow end
  if cecol < cscol then cscol, cecol = cecol, cscol end
  local lines = vim.fn.getline(csrow, cerow)
  -- local n = cerow-csrow+1
  local n = tbl_length(lines)
  if n <= 0 then return "" end
  lines[n] = string.sub(lines[n], 1, cecol)
  lines[1] = string.sub(lines[1], cscol)
  return table.concat(lines, "\n")
end

--- Get the best match at a given point
--- If the point is inside a node, the smallest node is returned
--- If the point is not inside a node, the closest node is returned (if opts.lookahead or opts.lookbehind is true)
---@param matches table list of matches
---@param row number 0-indexed
---@param col number 0-indexed
---@param opts table lookahead and lookbehind options
local function best_match_at_point(matches, row, col, opts)
  local ts_utils = require "nvim-treesitter.ts_utils"
  
  local match_length
  local smallest_range
  local earliest_start

  local lookahead_match_length
  local lookahead_largest_range
  local lookahead_earliest_start
  local lookbehind_match_length
  local lookbehind_largest_range
  local lookbehind_earliest_start

  for _, m in pairs(matches) do
    if m.node and vim.treesitter.is_in_node_range(m.node, row, col) then
      local length = ts_utils.node_length(m.node)
      if not match_length or length < match_length then
        smallest_range = m
        match_length = length
      end
      -- for nodes with same length take the one with earliest start
      if match_length and length == smallest_range then
        local start = m.start
        if start then
          local _, _, start_byte = m.start.node:start()
          if not earliest_start or start_byte < earliest_start then
            smallest_range = m
            match_length = length
            earliest_start = start_byte
          end
        end
      end
    elseif opts.lookahead then
      local start_line, start_col, start_byte = m.node:start()
      if start_line > row or start_line == row and start_col > col then
        local length = ts_utils.node_length(m.node)
        if
          not lookahead_earliest_start
          or lookahead_earliest_start > start_byte
          or (lookahead_earliest_start == start_byte and lookahead_match_length < length)
        then
          lookahead_match_length = length
          lookahead_largest_range = m
          lookahead_earliest_start = start_byte
        end
      end
    elseif opts.lookbehind then
      local start_line, start_col, start_byte = m.node:start()
      if start_line < row or start_line == row and start_col < col then
        local length = ts_utils.node_length(m.node)
        if
          not lookbehind_earliest_start
          or lookbehind_earliest_start < start_byte
          or (lookbehind_earliest_start == start_byte and lookbehind_match_length > length)
        then
          lookbehind_match_length = length
          lookbehind_largest_range = m
          lookbehind_earliest_start = start_byte
        end
      end
    end
  end

  local get_range = function(match)
    if match.metadata ~= nil then
      return match.metadata.range
    end

    return { match.node:range() }
  end

  if smallest_range then
    if smallest_range.start then
      local start_range = get_range(smallest_range.start)
      local node_range = get_range(smallest_range)
      return { start_range[1], start_range[2], node_range[3], node_range[4] }, smallest_range.node
    else
      return get_range(smallest_range), smallest_range.node
    end
  elseif lookahead_largest_range then
    return get_range(lookahead_largest_range), lookahead_largest_range.node
  elseif lookbehind_largest_range then
    return get_range(lookbehind_largest_range), lookbehind_largest_range.node
  end
end

function M.select_textobject(query_string, query_group, keymap_mode) 
  local bufnr, textobject = M.textobject_at_point(query_string, query_group, nil, nil, {})
  local ts_utils = require "nvim-treesitter.ts_utils"

  if textobject then
    -- local selection_mode = M.detect_selection_mode(query_string, keymap_mode)
    -- if
    --   val_or_return(surrounding_whitespace, {
    --     query_string = query_string,
    --     selection_mode = selection_mode,
    --   })
    -- then
    --   textobject = include_surrounding_whitespace(bufnr, textobject, selection_mode)
    -- end
    ts_utils.update_selection(bufnr, textobject, "v")
  end
end

function M.detect_selection_mode(query_string, keymap_mode)
  local configs = require "nvim-treesitter.configs"
  
  -- Update selection mode with different methods based on keymapping mode
  local keymap_to_method = {
    o = "operator-pending",
    s = "visual",
    v = "visual",
    x = "visual",
  }
  local method = keymap_to_method[keymap_mode]

  local config = configs.get_module "textobjects.select"
  local selection_modes = val_or_return(config.selection_modes, { query_string = query_string, method = method })
  local selection_mode
  if type(selection_modes) == "table" then
    selection_mode = selection_modes[query_string] or "v"
  else
    selection_mode = selection_modes or "v"
  end

  local ret_value = selection_mode
  local mode = vim.fn.mode(1)
  local is_normal_or_charwise_v = mode == "n" or mode == "v"

  if not is_normal_or_charwise_v then
    -- According to "mode()" mapping, if we are in operator pending mode or visual mode,
    -- then last char is {v,V,<C-v>}, exept for "no", which is "o", in which case we honor
    -- last set `selection_mode`
    local visual_mode = mode:sub(#mode)
    ret_value = visual_mode == "o" and selection_mode or visual_mode
  end

  return ret_value == "n" and "v" or ret_value
end

function M.textobject_at_point(query_string, query_group, pos, bufnr, opts)
  local parsers = require "nvim-treesitter.parsers"
  local queries = require "nvim-treesitter.query"
  local ts_utils = require "nvim-treesitter.ts_utils"
  local ts = require "nvim-treesitter.compat"  query_group = query_group or "textobjects"

  opts = opts or {}
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  local lang = parsers.get_buf_lang(bufnr)
  if not lang then
    return
  end

  local row, col = unpack(pos or vim.api.nvim_win_get_cursor(0))
  row = row - 1

  if not string.match(query_string, "^@.*") then
    error 'Captures must start with "@"'
    return
  end

  local matches = queries.get_capture_matches_recursively(bufnr, query_string, query_group)
  if string.match(query_string, "^@.*%.outer$") then
    local range, node = best_match_at_point(matches, row, col, opts)
    return bufnr, range, node
  else
    -- query string is @*.inner or @*
    -- First search the @*.outer instead, and then search the @*.inner within the range of the @*.outer
    local query_string_outer = string.gsub(query_string, "%..*", ".outer")
    if query_string_outer == query_string then
      query_string_outer = query_string .. ".outer"
    end

    local matches_outer = queries.get_capture_matches_recursively(bufnr, query_string_outer, query_group)
    if #matches_outer == 0 then
      -- Outer query doesn't exist or doesn't match anything
      -- Return the best match from the entire buffer, just like the @*.outer case
      local range, node = best_match_at_point(matches, row, col, opts)
      return bufnr, range, node
    end

    -- Note that outer matches don't perform lookahead
    local range_outer, node_outer = best_match_at_point(matches_outer, row, col, {})
    if range_outer == nil then
      -- No @*.outer found
      -- Return the best match from the entire buffer, just like the @*.outer case
      local range, node = best_match_at_point(matches, row, col, opts)
      return bufnr, range, node
    end

    local matches_within_outer = {}
    for _, match in ipairs(matches) do
      if M.node_contains(node_outer, { match.node:range() }) then
        table.insert(matches_within_outer, match)
      end
    end
    if #matches_within_outer == 0 then
      -- No @*.inner found within the range of the @*.outer
      -- Return the best match from the entire buffer, just like the @*.outer case
      local range, node = best_match_at_point(matches, row, col, opts)
      return bufnr, range, node
    else
      -- Find the best match from the cursor position
      local range, node = best_match_at_point(matches_within_outer, row, col, opts)
      if range ~= nil then
        return bufnr, range, node
      else
        -- If failed,
        -- find the best match within the range of the @*.outer
        -- starting from the outer range's start position (not the cursor position)
        -- with lookahead enabled
        range, node = best_match_at_point(matches_within_outer, range_outer[1], range_outer[2], { lookahead = true })
        return bufnr, range, node
      end
    end
  end
end

M.get_position_before_postfix = function()
  local pos = vim.api.nvim_win_get_cursor(0) -- get current cursor position
  local row = pos[1] - 1 -- must convert to 0 indexed
  local line = vim.api.nvim_get_current_line()
  local dot_position = line:find("%.") 

  if dot_position then 
    return { row, dot_position - 2 } -- Before dot + 1 based index
  end

  return nil
end

-- Type postfix snippet
M.type_node = function()
  local node = vim.treesitter.get_node({
    pos = M.get_position_before_postfix()
  })

  local function is_type(ty)
    if 
      -- ts
      ty == "type_identifier" or
      ty == "predefined_type" or
      -- rust
      ty == "primitive_type" or 
      ty == "scoped_type_identifier" or 
      ty == "generic_type" or 
      ty == "type_identifier"  then
      return true
    end 

    return false
  end

  while node do 
    local ty = node:type()

    local parent = node:parent(); 
    local parent_ty = parent and parent:type()

    if is_type(ty) and not is_type(parent_ty) then
      return node
    end

    node = parent
  end 

  return nil
end 

-- Expression postfix snippet
M.expr_node = function()
  local node = vim.treesitter.get_node({
    pos = M.get_position_before_postfix()
  })

  while node do 
    local ty = node:type()
    if 
      ty == "call_expression" or 
      ty == "integer_literal" or 
      ty == "string_literal" or 
      ty == "float_literal" then 
      return node
    end 

    node = node:parent()
  end 

  return nil
end

-- Create a postfix snippet
M.postfix = function(options)
  return { 
    trigger = options.trigger, 
    execute = function ()
      local node = options.node()
      if node == nil then
        return nil
      end

      local row, col = node:start()
      local text = vim.treesitter.get_node_text(node, 0):gsub('%.%w*$', '')  -- remove postfix
      local pos = vim.api.nvim_win_get_cursor(0)
      return {
        body = options.body(text), 
        clear_region = {
          from = { row, col },
          to = { pos[1] - 1, pos[2] } 
        }
      } 
    end, 
  }  
end


return M
