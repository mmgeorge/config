return {
  {
    "yioneko/nvim-vtsls",
    config = function()
      require("vtsls").config {}
    end
  },
  -- {
  --   "pmizio/typescript-tools.nvim",
  --   dependencies = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig", 'saghen/blink.cmp' },
  --   config = function()
  --     require("typescript-tools").setup {
  --       settings = {
  --         -- publish_diagnostic_on = "insert_leave",
  --         -- tsserver_file_preferences = {
  --         --   autoImportSpecifierExcludeRegexes = {
  --         --     "node_modules/lucide-solid",
  --         --     "lucide-solid$",
  --         --     "lucide-solid/icons/index",
  --         --     "solid-js/types",
  --         --     "solid-js/web/types",
  --         --     "solid-js/store/types/server",
  --         --   }
  --         --   -- autoImportFileExcludePatterns = {
  --         --   -- "lucide-solid"
  --         --   -- }
  --         -- },
  --         -- JSXCloseTag
  --         -- WARNING: it is disabled by default (maybe you configuration or distro already uses nvim-ts-autotag,
  --         -- that maybe have a conflict if enable this feature. )
  --         -- jsx_close_tag = {
  --         --   enable = false,
  --         --   filetypes = { "javascriptreact", "typescriptreact" },
  --         -- }
  --       },
  --     }
  --   end,
  --
  -- },
  {
    -- Provides much better indenting than treesitter
    "https://github.com/MaxMEllon/vim-jsx-pretty",
    config = function()
      -- require("vim-jsx-pretty").setup({})
    end
  }
}
