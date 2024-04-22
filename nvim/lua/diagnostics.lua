-- Define colors for diagnostics
-- local diagnostic_colors = {
--     Error = "#ff0000", -- Red for errors
--     Warning = "#ff8800", -- Orange for warnings
--     Information = "#00ff00", -- Green for information
--     Hint = "#888888", -- Gray for hints
-- }

-- -- Apply the colors to LSP diagnostics
-- local function apply_diagnostic_colors()
--     for severity, color in pairs(diagnostic_colors) do
--         vim.cmd(string.format("hi LspDiagnosticsDefault%s guifg=%s", severity, color))
--     end
-- end

-- Apply colors after LSP is set up
-- vim.cmd("autocmd User LspDiagnosticsChanged :lua apply_diagnostic_colors()")

vim.diagnostic.config({
  virtual_text = {
    severity = {
        -- vim.diagnostic.severity.WARN,
        vim.diagnostic.severity.ERROR,
     }
  },
  signs = false,
  underline = {
     severity = {
        vim.diagnostic.severity.WARN,
        vim.diagnostic.severity.ERROR,
     }
  },
  update_in_insert = true,
  severity_sort = false,
})
