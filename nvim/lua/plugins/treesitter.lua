return {
   "nvim-treesitter/nvim-treesitter",
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
            init_selection = "ik",
            node_incremental = "k",
            node_decremental = "l",
            -- scope_incremental = "grc",
          },
        }
                                              })
   end
   }
