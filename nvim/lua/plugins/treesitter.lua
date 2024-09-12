local key = require("../keys").key

return {
  {
    "nvim-treesitter/nvim-treesitter",
    dependencies = {
      -- Does NOT work with c_sharp
      "nvim-treesitter/nvim-treesitter-textobjects",
    },
    -- build = ":TSUpdate",
    config = function () 
      local configs = require("nvim-treesitter.configs")

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
          "toml"
        },
        sync_install = false,
        highlight = { 
          enable = true,
          -- Needed to play nixe with vim-jsx-pretty (vim-polygot)
          -- https://github.com/nvim-treesitter/nvim-treesitter/issues/1019 
          additional_vim_regex_highlighting = true
        },
        indent = { 
          enable = true, 
          -- vim-jsx-pretty provides better indent. See typescript config
          disable = { "tsx" }
        }, 
        autotag = {
          enable = true
        }
      })
      vim.filetype.add({extension = {frag = "frag"}})
      vim.treesitter.language.register('glsl', { 'frag' })

      vim.filetype.add({extension = {vert = "vert"}})
      vim.treesitter.language.register('glsl', { 'vert' })

      vim.filetype.add({extension = {wgslx = "wgslx"}})
      vim.treesitter.language.register('wgsl', { 'wgslx' })

      require('nvim-treesitter.configs').setup({
        incremental_selection = {
          enable = true,
          keymaps = {
            node_incremental = key("v"),
            node_decremental = key("V"),  
            --scope_incremental = "k",
            -- scope_decremental = "l",
          },
        },

        textobjects = {
          select = {
            enable = true,
            lookahead = true,
            keymaps = {
              -- You can use the capture groups defined in textobjects.scm
              [key('of')] = '@function.outer',
              [key('if')] = '@function.inner',
              [key('oc')] = '@class.outer',
              [key('ic')] = '@class.inner',
            },
          },
          move = {
            enable = true,
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
  --   "yioneko/nvim-yati", 
  --   dependencies = {
  --     "nvim-treesitter/nvim-treesitter",
  --   },
  --   config = function ()
  --     require("nvim-treesitter.configs").setup {
  --       yati = {
  --         enable = true,
  --         -- Disable by languages, see `Supported languages`
  --         -- disable = { "tsx" },
  --
  --         -- Whether to enable lazy mode (recommend to enable this if bad indent happens frequently)
  --         default_lazy = true,
  --
  --         -- Determine the fallback method used when we cannot calculate indent by tree-sitter
  --         --   "auto": fallback to vim auto indent
  --         --   "asis": use current indent as-is
  --         --   "cindent": see `:h cindent()`
  --         -- Or a custom function return the final indent result.
  --         default_fallback = "auto"
  --       },
  --       indent = {
  --         enable = false -- disable builtin indent module
  --       }
  --     }
  --   end
  -- }
  -- {
  --    "nvim-treesitter/nvim-treesitter-context",
  --    config = function()
  --       require'treesitter-context'.setup{
  --          enable = false, -- Enable this plugin (Can be enabled/disabled later via commands)
  --          max_lines = 0, -- How many lines the window should span. Values <= 0 mean no limit.
  --          min_window_height = 0, -- Minimum editor window height to enable context. Values <= 0 mean no limit.
  --          line_numbers = true,
  --          multiline_threshold = 20, -- Maximum number of lines to show for a single context
  --          trim_scope = 'outer', -- Which context lines to discard if `max_lines` is exceeded. Choices: 'inner', 'outer'
  --          mode = 'cursor',  -- Line used to calculate context. Choices: 'cursor', 'topline'
  --          -- Separator betweenhttps://configure.zsa.io/ergodox-ez/layouts/Wr4YM/latest/1 context and content. Should be a single character string, like '-'.
  --          -- When separator is set, the context will only show up when there are at least 2 lines above cursorline.
  --          separator = nil,
  --          zindex = 20, -- The Z-index of the context window
  --          on_attach = nil, -- (fun(buf: integer): boolean) return false to disable attaching
  --         }
  --    end
  -- }
}
