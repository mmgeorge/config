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
          "json",
          "query",
          "javascript",
          "css",
          "html",
          "wgsl",
          "glsl",
          "c_sharp",
          "toml",
          "slang",
          "yaml",
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
          enable = true,
          -- vim-jsx-pretty provides better indent. See typescript config
          -- disable = { "tsx" }
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
              -- ['uy'] = '@parameter.inner',
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
  {
    "windwp/nvim-ts-autotag",
    opts = {}
  },
  -- {
  --   "nvim-treesitter/nvim-treesitter-context",
  --   dependencies = {
  --     "nvim-treesitter/nvim-treesitter",
  --   },
  --   config = function()
  --     require("treesitter-context").setup({
  --       enable = true,            -- Enable this plugin (Can be enabled/disabled later via commands)
  --       multiwindow = false,      -- Enable multiwindow support.
  --       max_lines = 0,            -- How many lines the window should span. Values <= 0 mean no limit.
  --       min_window_height = 0,    -- Minimum editor window height to enable context. Values <= 0 mean no limit.
  --       line_numbers = true,
  --       multiline_threshold = 20, -- Maximum number of lines to show for a single context
  --       trim_scope = 'outer',     -- Which context lines to discard if `max_lines` is exceeded. Choices: 'inner', 'outer'
  --       mode = 'topline',         -- Line used to calculate context. Choices: 'cursor', 'topline'
  --       -- Separator between context and content. Should be a single character string, like '-'.
  --       -- When separator is set, the context will only show up when there are at least 2 lines above cursorline.
  --       separator = nil,
  --       zindex = 20,     -- The Z-index of the context window
  --       on_attach = nil, -- (fun(buf: integer): boolean) return false to disable attaching
  --     })
  --   end
  -- }
  -- {
  --   -- Provides much better indenting than treesitter
  --   "https://github.com/MaxMEllon/vim-jsx-pretty",
  --   config = function ()
  --     require("vim-jsx-pretty").setup({})
  --   end
  -- }
}
