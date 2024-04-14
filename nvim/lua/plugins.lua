return {
   {
      "folke/which-key.nvim",
      event = "VeryLazy",
      init = function()
         vim.o.timeout = true
         vim.o.timeoutlen = 300
         -- vim.keymap.del('n', 'd'); 
      end,
      config = function()
         -- vim.keymap.del('n', 'di'); 
      end,
      opts = {
         -- your configuration comes here
         -- or leave it empty to use the default settings
         -- refer to the configuration section below
      }
   },
   -- {
   --    "max397574/better-escape.nvim",
   --    config = function()
   --       require("better_escape").setup {
   --             mapping = { 'jk' }
   --       }
   --    end
   -- }
}
