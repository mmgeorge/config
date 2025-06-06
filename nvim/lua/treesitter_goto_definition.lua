-- Definition based navigation module
-- Originally from <https://github.com/nvim-treesitter/nvim-treesitter-refactor/>

local ts_utils = require "nvim-treesitter.ts_utils"
local locals = require "nvim-treesitter.locals"
local configs = require "nvim-treesitter.configs"
local api = vim.api

local M = {}

local function index_of(tbl, obj)
  for i, o in ipairs(tbl) do
    if o == obj then
      return i
    end
  end
end

local get_node_text = vim.treesitter.get_node_text or vim.treesitter.query.get_node_text

function M.goto_definition(bufnr, fallback_function)
  local bufnr = bufnr or api.nvim_get_current_buf()
  local node_at_point = ts_utils.get_node_at_cursor()

  if not node_at_point then
    return
  end

  local definition = locals.find_definition(node_at_point, bufnr)

  if fallback_function and definition == node_at_point then
    fallback_function()
  else
    ts_utils.goto_node(definition)
  end
end

function M.goto_definition_lsp_fallback(bufnr)
  M.goto_definition(bufnr, vim.lsp.buf.definition)
end

--- Get definitions of bufnr (unique and sorted by order of appearance).
local function get_definitions(bufnr)
  local local_nodes = locals.get_locals(bufnr)

  -- Make sure the nodes are unique.
  local nodes_set = {}
  for _, loc in ipairs(local_nodes) do
    if loc.definition then
      locals.recurse_local_nodes(loc.definition, function(_, node, _, match)
        -- lua doesn't compare tables by value,
        -- use the value from byte count instead.
        local _, _, start = node:start()
        nodes_set[start] = { node = node, type = match or "" }
      end)
    end
  end

  -- Sort by order of appearance.
  local definition_nodes = vim.tbl_values(nodes_set)
  table.sort(definition_nodes, function(a, b)
    local _, _, start_a = a.node:start()
    local _, _, start_b = b.node:start()
    return start_a < start_b
  end)

  return definition_nodes
end

function M.list_definitions(bufnr)
  local bufnr = bufnr or api.nvim_get_current_buf()
  local definitions = get_definitions(bufnr)

  if #definitions < 1 then
    return
  end

  local qf_list = {}

  for _, node in ipairs(definitions) do
    local lnum, col, _ = node.node:start()
    local type = string.upper(node.type:sub(1, 1))
    local text = get_node_text(node.node, bufnr) or ""
    table.insert(qf_list, {
      bufnr = bufnr,
      lnum = lnum + 1,
      col = col + 1,
      text = text,
      type = type,
    })
  end

  vim.fn.setqflist(qf_list, "r")
  api.nvim_command "copen"
end

function M.list_definitions_toc()
  local winnr = api.nvim_get_current_win()
  local bufnr = api.nvim_win_get_buf(winnr)
  local definitions = get_definitions(bufnr)

  if #definitions < 1 then
    return
  end

  local loc_list = {}

  -- Force some types to act like they are parents
  -- instead of neighbors of the next nodes.
  local containers = {
    ["function"] = true,
    ["type"] = true,
    ["method"] = true,
  }

  local parents = {}

  for _, def in ipairs(definitions) do
    -- Get indentation level by putting all parents in a stack.
    -- The length of the stack minus one is the current level of indentation.
    local n = #parents
    for i = 1, n do
      local index = n + 1 - i
      local parent_def = parents[index]
      if
          ts_utils.is_parent(parent_def.node, def.node)
          or (containers[parent_def.type] and ts_utils.is_parent(parent_def.node:parent(), def.node))
      then
        break
      else
        parents[index] = nil
      end
    end
    parents[#parents + 1] = def

    local lnum, col, _ = def.node:start()
    local type = string.upper(def.type:sub(1, 1))
    local text = get_node_text(def.node, bufnr) or ""
    table.insert(loc_list, {
      bufnr = bufnr,
      lnum = lnum + 1,
      col = col + 1,
      text = string.rep("  ", #parents - 1) .. text,
      type = type,
    })
  end

  vim.fn.setloclist(winnr, loc_list, "r")
  -- The title needs to end with `TOC`,
  -- so Neovim displays it like a TOC instead of an error list.
  vim.fn.setloclist(winnr, {}, "a", { title = "Definitions TOC" })
  api.nvim_command "lopen"
end

function M.goto_adjacent_usage(bufnr, delta)
  local bufnr = bufnr or api.nvim_get_current_buf()
  local node_at_point = ts_utils.get_node_at_cursor()
  if not node_at_point then
    return
  end

  local def_node, scope = locals.find_definition(node_at_point, bufnr)
  local usages = locals.find_usages(def_node, scope, bufnr)

  local index = index_of(usages, node_at_point)
  if not index then
    return
  end

  local target_index = (index + delta + #usages - 1) % #usages + 1
  ts_utils.goto_node(usages[target_index])
end

function M.goto_next_usage(bufnr)
  return M.goto_adjacent_usage(bufnr, 1)
end

function M.goto_previous_usage(bufnr)
  return M.goto_adjacent_usage(bufnr, -1)
end

function M.attach(bufnr)
  local config = configs.get_module "refactor.navigation"

  for fn_name, mapping in pairs(config.keymaps) do
    if mapping then
      local cmd = string.format([[:lua require'nvim-treesitter-refactor.navigation'.%s(%d)<CR>]], fn_name, bufnr)
      api.nvim_buf_set_keymap(bufnr, "n", mapping, cmd, { silent = true, noremap = true, desc = fn_name })
    end
  end
end

function M.detach(bufnr)
  local config = configs.get_module "refactor.navigation"

  for _, mapping in pairs(config.keymaps) do
    if mapping then
      api.nvim_buf_del_keymap(bufnr, "n", mapping)
    end
  end
end

return M
