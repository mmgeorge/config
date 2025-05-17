-- Lua function to select the nearest parent node based on a Tree-sitter query.
--
-- This function assumes you have a query file named 'parent.scm'
-- (e.g., queries/rust/parent.scm) that defines what a "parent" node is.
-- The query should capture the desired node, for instance, using @_start and @_end
-- for its range, as in the example provided by the user:
-- (([ (function_item) ... ] @_start @_end) (#make-range! "parent" @_start @_end))

local cursor_stack = {}

--- Push the current visual selection or cursor position onto cursor_stack
local function remember_current_selection()
  local mode = vim.fn.mode()
  local start_row, start_col, end_row, end_col

  if mode == "v" or mode == "V" then
    -- Get the start and end of the visual selection
    local start_pos = vim.fn.getpos("'<")
    local end_pos   = vim.fn.getpos("'>")
    start_row = start_pos[2] - 1
    start_col = start_pos[3] - 1
    end_row   = end_pos[2] - 1
    end_col   = end_pos[3] - 1
    -- In visual-line, select all columns
    if mode == "V" then
      start_col = 0
      end_col = math.max(0, #vim.fn.getline(end_row+1))
    end
    print("Push it v", start_row .. " " .. end_row)
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
    print("Push it non -v", start_row .. " " .. end_row)
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
      cursor_stack = {}
    end,
    once = true, -- Ensure it triggers only once
  })
end
  

function select_parent()
  setup_visual_exit_autocmd()

  local bufnr = vim.api.nvim_get_current_buf()
  local winid = vim.api.nvim_get_current_win()
  local cursor_pos = vim.api.nvim_win_get_cursor(winid) -- {row, col}, 1-indexed
  local cursor_row = cursor_pos[1] - 1
  local cursor_col = cursor_pos[2]

  local lang = vim.bo[bufnr].filetype
  if type(lang) ~= "string" or lang == "" then
    vim.notify(
      "Error: Buffer language (filetype) is not a valid string. Actual value: " .. vim.inspect(lang),
      vim.log.levels.ERROR
    )
    return
  end

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

  local root = tree:root()
  local node = root:named_descendant_for_range(cursor_row, cursor_col, cursor_row, cursor_col)
  if not node then
    -- Fallback to any descendant (named or anonymous) if no specific named one is at the exact point/range
    node = root:descendant_for_range(cursor_row, cursor_col, cursor_row, cursor_col)
  end
  vim.notify("Got node")

  if not node then
    vim.notify(
      "Error: Could not find a Tree-sitter node at the cursor position. Position: r" .. cursor_row .. " c" .. cursor_col,
      vim.log.levels.ERROR
    )
    return
  end

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

  -- Find the capture ID for "@_start" as defined in the user's query.
  local capture_start_id = -1
  for i, name in ipairs(query.captures) do
    if name == "_start" then
      capture_start_id = i
      break
    end
  end

  if capture_start_id == -1 then
    vim.notify(
      "Error: The 'parent' query does not contain the '@_start' capture. " ..
      "The query needs to identify the main node with '@_start'.",
      vim.log.levels.ERROR)
    return
  end
    
  local parent = nil
  while node do
    for _, match in query:iter_matches(node, bufnr) do
      local matched = match[capture_start_id][1]
      if matched and matched:id() == node:id() then
        parent = node
        goto found_parent_node
      end
    end
    node = node:parent()
  end

  ::found_parent_node::

  if parent then
    select_node(parent)
  end
end

local related_node_types = {
  "attribute_item", 
  "line_comment"
}

local function is_related_node(node)
  for _, v in ipairs(related_node_types) do
    if v == node:type() then
      return true
    end
  end
  return false
end

-- For the given node, get the start of the left-most relevant sibling. 
-- Relevant siblings include comments, macros, and decorators 
function related_leading_sibling_start(node)
  local start_row, start_col = node:range()
  
  -- Traverse backwards and include leading (attribute_item) siblings
  local parent = node:parent()
  if parent then
    local siblings = {}
    for i = 0, parent:child_count() - 1 do
      siblings[i + 1] = parent:child(i)
    end

    local i = 1
    while i <= #siblings do
      if siblings[i]:id() == node:id() then
        -- scan leftwards for (attribute_item) siblings
        local s = i - 1
        while s >= 1 do
          local sibling = siblings[s]
          if is_related_node(sibling) then
            local s_row, s_col = sibling:range()
            start_row = s_row
            -- start_col must be reached to the leftmost attr
            start_col = 0
            s = s - 1
          else
            break
          end
        end
        break
      end
      i = i + 1
    end
  end

  return start_row, start_col
end


function select_node(node)
  local ts_utils = require("nvim-treesitter.ts_utils")
  local _, _, end_row, end_col = node:range()
  local start_row, start_col =  related_leading_sibling_start(node)
  start_col = 0

  -- Get buffer lines
  local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
  local last_buf_line = #lines

  -- Extend end_row to include trailing empty lines after the node
  local i = end_row + 1
  while i < last_buf_line and lines[i + 1]:match('^%s*$') do
    end_row = i
    end_col = #lines[i + 1]
    i = i + 1
  end

  vim.api.nvim_buf_set_mark(0, '<', start_row + 1, start_col, {})
  vim.api.nvim_buf_set_mark(0, '>', end_row + 1, end_col, {})
  vim.cmd("normal! gvo")
  remember_current_selection()
end



function undo_select_node()
  if #cursor_stack ~= 0 then
    local cursor = table.remove(cursor_stack)
    vim.api.nvim_buf_set_mark(0, '<', cursor.start_row + 1, cursor.start_col, {})
    vim.api.nvim_buf_set_mark(0, '>', cursor.end_row + 1, cursor.end_col, {})
    vim.cmd("normal! gvo")
  end

end


local key = require("../keys").key

return {
  {
    "nvim-treesitter/nvim-treesitter",
    dependencies = {
      "nvim-treesitter/nvim-treesitter-textobjects",
    },
    -- keys = {
    --   {
    --     "<C-q>",
    --     function()
    --       select_parent()
    --     end,
    --     mode = { "n", "i", "v", "x", "s", "o" }
    --   },
    --   {
    --     "p",
    --     function()
    --       select_parent()
    --     end,
    --     mode = { "v", "x" }
    --   },
    --   {
    --     "P",
    --     function()
    --       select_child()
    --     end,
    --     mode = { "v", "x" }
    --   }
    --
    -- },
    -- build = ":TSUpdate",
    config = function()
      local configs = require("nvim-treesitter.configs")

      vim.filetype.add({ extension = { frag = "frag" } })
      vim.treesitter.language.register('glsl', { 'frag' })
      vim.filetype.add({ extension = { vert = "vert" } })
      vim.treesitter.language.register('glsl', { 'vert' })
      vim.filetype.add({ extension = { wgslx = "wgslx" } })
      vim.treesitter.language.register('wgsl', { 'wgslx' })

      ---@diagnostic disable-next-line: missing-fields
      configs.setup({
        ensure_installed = {
          "rust",
          "typescript",
          "tsx",
          "lua",
          "vim",
          "vimdoc",
          "query",
          "javascript",
          "css",
          "html",
          "wgsl",
          "glsl",
          "c_sharp",
          "toml",
          "slang"
        },
        sync_install = false,
        highlight = {
          enable = true,
          -- Needed to play nixe with vim-jsx-pretty (vim-polygot)
          -- https://github.com/nvim-treesitter/nvim-treesitter/issues/1019
          -- additional_vim_regex_highlighting = true
        },
        indent = {
          -- This does not work well
          enable = false,
          -- vim-jsx-pretty provides better indent. See typescript config
          -- disable = { "tsx" }
        },
        autotag = {
          enable = true
        }
      })

      ---@diagnostic disable-next-line: missing-fields
      require('nvim-treesitter.configs').setup({
        incremental_selection = {
          -- This causes random crashes.
          enable = false,
          keymaps = {
            init_selection = true,
            node_incremental = key("v"),
            node_decremental = key("V"),
            -- scope_incremental = key("v"),
            -- scope_decremental = key("V"),
          },
        },

        textobjects = {
          select = {
            enable = true,
            lookahead = true,
            include_surrounding_whitespace = true,
            keymaps = {
              -- You can use the capture groups defined in textobjects.scm
              -- ['uf'] = '@xfunction.inner',
              ['f'] = '@xfunction.outer',
              ['ot'] = '@jsx_element.outer',
              ['ut'] = '@jsx_element.inner',
              ['uy'] = '@parameter.inner',
              ['oy'] = '@parameter.outer',
              -- ['ub'] = '@block.inner',
              -- ['o'] = '@xparent.outer',
              ['uq'] = '@conditional.inner',
              ['oq'] = '@call.outer',
            },
          },
          move = {
            enable = false,
            set_jumps = true, -- whether to set jumps in the jumplist
            goto_next_start = {
              -- [']m'] = '@function.outer',
              -- [']]'] = '@class.inner',
            },
            goto_next_end = {
              -- ['cf'] = '@function.outer',
              -- [']['] = '@class.outer',
            },
            goto_previous_start = {
              -- ['[m'] = '@function.outer',
              -- ['[['] = '@class.inner',
            },
            goto_previous_end = {
              -- ['[M'] = '@function.outer',
              -- ['[]'] = '@class.outer',
            },
          }
        }
      })
    end
  },
  -- {
  --   "RRethy/nvim-treesitter-textsubjects",
  --   dependencies = {
  --     'nvim-treesitter/nvim-treesitter'
  --   },
  --   config = function()
  --     require('nvim-treesitter-textsubjects').configure({
  --       prev_selection = nil,
  --       keymaps = {
  --         -- ['<C-l>'] = 'textsubjects-smart',
  --         ['<C-l>'] = 'textsubjects-container-outer',
  --         -- ['i;'] = 'textsubjects-container-inner',
  --       },
  --     })
  --   end
  -- }
  -- {
  --   'mawkler/jsx-element.nvim',
  --   dependencies = {
  --     'nvim-treesitter/nvim-treesitter',
  --     'nvim-treesitter/nvim-treesitter-textobjects',
  --   },
  --   keys = {
  --     {
  --       "pot",
  --       function ()
  --         require('treesj').toggle()
  --       end,
  --       mode = { "n" },
  --       desc = "Oil file browser",
  --     }
  --   },
  --   ft = { 'typescriptreact', 'javascriptreact', 'javascript' },
  --   config = function ()
  --     require"jsx-element".setup({
  --       keymaps = false
  --     })
  --   end
  -- }
}
