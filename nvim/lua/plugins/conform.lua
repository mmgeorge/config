return {
  {
    'stevearc/conform.nvim',
    opts = {
      format_after_save = {
        lsp_format = "fallback",
      },
      -- tsx = { "trim_whitespace" },
      -- tsx = { "trim_whitespace" },
      formatters_by_ft  = {
        ["*"] = { lsp_format,  "trim_whitespace"  },
      }
      -- format_on_save = {
      --   -- These options will be passed to conform.format()
      --   timeout_ms = 200,
      --   lsp_format = "fallback",
      -- },
    },
  }
}
