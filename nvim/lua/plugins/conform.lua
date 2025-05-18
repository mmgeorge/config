return {
  {
    'stevearc/conform.nvim',
    opts = {
      format_after_save = {
        lsp_format = "fallback",
      },
      json = { "prettier" },
      -- format_on_save = {
      --   -- These options will be passed to conform.format()
      --   timeout_ms = 200,
      --   lsp_format = "fallback",
      -- },
    },
  }
}
