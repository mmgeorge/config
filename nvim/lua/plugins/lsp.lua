return {
   -- Lsp package manager
   {
      "williamboman/mason.nvim",
      config = function()
         require("mason").setup()
      end
   },

   -- Bridge mason.nvim & lspconfig
   {
      "williamboman/mason-lspconfig.nvim",
      config = function()
         require("mason-lspconfig").setup({
               ensure_installed = { "lua_ls", "tsserver" }

         })
      end
   },

   -- Setup servers via lspconfig --
   {
      "neovim/nvim-lspconfig",
      config = function()
         local lspconfig = require("lspconfig")
         lspconfig.lua_ls.setup({})
         lspconfig.rust_analyzer.setup({})
         lspconfig.tsserver.setup({})
      end
   },

   -- Log startup status --
   {
      "j-hui/fidget.nvim",
      opts = {
         -- options
      },
   },

   -- {
   --    "aznhe21/actions-preview.nvim",
   --    config = function()
   --       -- vim.keymap.set({ "v", "n" }, "olf", require("actions-preview").code_actions)
   --    end,
   -- },

   -- {
   --    'filipdutescu/renamer.nvim',
   --    branch = 'master',
   --    requires = { {'nvim-lua/plenary.nvim'} }
   -- },

   
   -- Very cool idea, but visual distracting. Make as a toggle?
   -- Could toggle, see: <https://github.com/ErichDonGubler/lsp_lines.nvim>
   -- {
   --    "https://git.sr.ht/~whynothugo/lsp_lines.nvim",
   --    config = function()
   --       require("lsp_lines").setup()
   --    end,
   -- }
   {
      
      
   },
   
   {
      "folke/trouble.nvim",
      -- dependencies = { "nvim-tree/nvim-web-devicons" },
      opts = {
         position = "bottom", -- position of the list can be: bottom, top, left, right
         height = 10, -- height of the trouble list when position is top or bottom
         width = 50, -- width of the list when position is left or right
         icons = false, -- use devicons for filenames
         mode = "workspace_diagnostics", -- "workspace_diagnostics", "document_diagnostics", "quickfix", "lsp_references", "loclist"
         severity = { vim.diagnostic.severity.ERROR,
                      vim.diagnostic.severity.WARN
         }, -- nil (ALL) or vim.diagnostic.severity.ERROR | WARN | INFO | HINT
         fold_open = "", --"", -- icon used for open folds
         fold_closed = "", -- "", -- icon used for closed folds
         group = true, -- group results by file
         padding = false, -- add an extra new line on top of the list
         cycle_results = true, -- cycle item list when reaching beginning or end of list
         action_keys = { -- key mappings for actions in the trouble list
            -- map to {} to remove a mapping, for example:
            -- close = {},
            close = {}, -- close the list
            -- cancel = "<C-g>", -- cancel the preview and get back to your last window / buffer / cursor
            refresh = { "r" }, -- manually refresh
            jump = { "<enter>" }, -- jump to the diagnostic or open / close folds
            open_split = {  }, -- open buffer in new split
            open_vsplit = {  }, -- open buffer in new vsplit
            open_tab = {  }, -- open buffer in new tab
            jump_close = {}, -- jump to the diagnostic and close the list
            toggle_mode = { "p" }, -- toggle between "workspace" and "document" diagnostics mode
            switch_severity = { }, -- switch "diagnostics" severity filter level to HINT / INFO / WARN / ERROR
            toggle_preview = {}, -- toggle auto_preview
            hover = {}, -- opens a small popup with the full multiline message
            preview = {}, -- preview the diagnostic location
            open_code_href = {}, -- if present, open a URI with more information about the diagnostic error
            close_folds = {}, -- close all folds
            open_folds = {}, -- open all folds
            toggle_fold = { "f" }, -- toggle fold of current file
            previous = { "a" }, -- previous item
            next = { "d" }, -- next item
            help = { "?"}, -- help menu
         },
         multiline = true, -- render multi-line messages
         indent_lines = true, -- add an indent guide below the fold icons
         win_config = { border = "single" }, -- window configuration for floating windows. See |nvim_open_win()|.
         auto_open = false, -- automatically open the list when you have diagnostics
         auto_close = false, -- automatically close the list when you have no diagnostics
         auto_preview = true, -- automatically preview the location of the diagnostic. <esc> to close preview and go back to last window
         auto_fold = false, -- automatically fold a file trouble list at creation
         auto_jump = {"lsp_definitions"}, -- for the given modes, automatically jump if there is only a single result
         include_declaration = { "lsp_references", "lsp_implementations", "lsp_definitions"  }, -- for the given modes, include the declaration of the current symbol in the results
         signs = {
            -- icons / text used for a diagnostic
            -- error = "",
            -- warning = "",
            -- hint = "",
            -- information = "",
            -- other = "",
            error = "E",
            warning = "W",
            hint = "H",
            information = "I",
         },
         use_diagnostic_signs = false -- enabling this will use the signs defined in your lsp client
      },
   }
}
