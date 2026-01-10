return {
  {
    'stevearc/conform.nvim',
    opts = {
      formatters = {
        remove_unused = {
          format = function(self, ctx, lines, callback)
            -- Call the LSP code action
            vim.lsp.buf.code_action({
              context = { only = { "quickfix" } },
              filter = function(action)
                return action.title == "Remove all unused imports"
              end,
              apply = true,
            })

            -- Since code actions are async and modify the buffer directly,
            -- we just return the current lines to conform.
            callback(nil, lines)
          end,
        },
      },
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
        -- Run trim_whitespace first, rustfmt can run into issues.
        ["*"] = { "trim_whitespace", lsp_format },
        ["rust"] = { "trim_whitespace",  "rustfmt" },
        ["slang"] = { "trim_whitespace" },
      }
      -- format_on_save = {
      --   -- These options will be passed to conform.format()
      --   timeout_ms = 200,
      --   lsp_format = "fallback",
      -- },
    },
  }
}
