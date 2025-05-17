-- Lua function to select the nearest parent node based on a Tree-sitter query.
--
-- This function assumes you have a query file named 'parent.scm'
-- (e.g., queries/rust/parent.scm) that defines what a "parent" node is.
-- The query should capture the desired node, for instance, using @_start and @_end
-- for its range, as in the example provided by the user:
-- (([ (function_item) ... ] @_start @_end) (#make-range! "parent" @_start @_end))

function select_parent()
  local bufnr = vim.api.nvim_get_current_buf()
  local winid = vim.api.nvim_get_current_win()
  local cursor_pos = vim.api.nvim_win_get_cursor(winid) -- {row, col}, 1-indexed
  local cursor_row = cursor_pos[1] - 1
  local cursor_col = cursor_pos[2] - 1

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
      "Ensure queries/" .. lang .. "/parent.scm exists in your Neovim runtime path (e.g., ~/.config/nvim/queries/"..lang.."/parent.scm). " ..
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
    print(vim.inspect(parent))
    local start_row, start_col, end_row, end_col = parent:range()
    vim.api.nvim_buf_set_mark(0, '<', start_row + 1, start_col, {})
    vim.api.nvim_buf_set_mark(0, '>', end_row + 1, end_col, {})
    vim.cmd("normal! gvo")

    -- Notify the user
    vim.notify("Selected nearest parent: " .. start_row .. "->" .. end_row, vim.log.levels.INFO)
  else
    vim.notify("No parent node found matching the 'parent' query.", vim.log.levels.INFO)
  end


end

-- Example of how to make it a command:
-- vim.api.nvim_create_user_command("SelectNearestParent", SelectNearestParent, {})

-- Example of how to map it to a key (e.g., <leader>ap for 'ancestor parent'):
-- vim.keymap.set("n", "<leader>ap", SelectNearestParent, { noremap = true, silent = true, desc = "Select Nearest Parent (Query)" })

function select_child()
  
end

function select_sibling()
  
end


local key = require("../keys").key

return {
  {
    "nvim-treesitter/nvim-treesitter",
    -- tag = "v0.9.3",
    dependencies = {
      -- Does NOT work with c_sharp
      "nvim-treesitter/nvim-treesitter-textobjects",
    },
    keys = {
      {
        "<C-q>", 
        function ()
          select_parent()
        end,
        mode = { "n", "i", "v", "x", "s", "o" }
      },
      {
        "P", 
        function ()
          select_parent()
        end,
        mode = { "v", "x" }
      }
    },
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
