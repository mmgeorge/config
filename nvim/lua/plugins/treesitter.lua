return {
   {
      "nvim-treesitter/nvim-treesitter",
      dependencies = {
         "nvim-treesitter/nvim-treesitter-textobjects",
      },
      build = ":TSUpdate",
      config = function () 
         local configs = require("nvim-treesitter.configs")

         configs.setup({
               ensure_installed = {
                  "rust",
                  "typescript",
                  "lua",
                  "vim",
                  "vimdoc",
                  "query",
                  "javascript",
                  "html"
               },
               sync_install = false,
               highlight = { enable = true },
               indent = { enable = true },  
         })
         
         require('nvim-treesitter.configs').setup({
               incremental_selection = {
                  enable = true,
                  keymaps = {
                     node_incremental = "k",
                     node_decremental = "l",  
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
                        ['of'] = '@function.outer',
                        ['if'] = '@function.inner',
                        ['oc'] = '@class.outer',
                        ['ic'] = '@class.inner',
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
   }
}
