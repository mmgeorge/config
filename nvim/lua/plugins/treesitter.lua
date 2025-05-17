local key = require("../keys").key

return {
  {
    "nvim-treesitter/nvim-treesitter",
    dependencies = {
      "nvim-treesitter/nvim-treesitter-textobjects",
    },
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
