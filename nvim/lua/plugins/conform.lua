return {
  {
    'stevearc/conform.nvim',
    opts = {
      -- format_after_save = {
      --   lsp_format = "fallback",
      -- },
      format_after_save = function(bufnr)
        local ftype = vim.bo[bufnr].filetype
        if ftype == "typescriptreact" or ftype == "typescript" then
          if vim.fn.exists(":LspEslintFixAll") == 2 then
            vim.cmd(":LspEslintFixAll")
            return nil
          end
        end

        return { lsp_format = "fallback" }
      end,
      -- tsx = { "trim_whitespace" },
      formatters_by_ft  = {
        ["*"] = { lsp_format, "trim_whitespace" },
      }
      -- format_on_save = {
      --   -- These options will be passed to conform.format()
      --   timeout_ms = 200,
      --   lsp_format = "fallback",
      -- },
    },
  }
}
