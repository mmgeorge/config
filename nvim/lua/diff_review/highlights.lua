local M = {}

function M.setup()
  local function get_bg(name)
    local hl = vim.api.nvim_get_hl(0, { name = name, link = false })
    return hl.bg
  end

  local add_bg = get_bg("DiffAdd") or "#002200"
  local del_bg = get_bg("DiffDelete") or "#220000"
  local normal = vim.api.nvim_get_hl(0, { name = "Normal", link = false })
  local header_fg = normal.fg or "#c0c0c0"

  vim.api.nvim_set_hl(0, "DiffReviewAddBg", { bg = add_bg })
  vim.api.nvim_set_hl(0, "DiffReviewDeleteBg", { bg = del_bg })
  vim.api.nvim_set_hl(0, "DiffReviewAddLineNr", { fg = "#50fa7b", bg = add_bg, bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewDeleteLineNr", { fg = "#ff5555", bg = del_bg, bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewContextLineNr", { fg = "#555555" })
  vim.api.nvim_set_hl(0, "DiffReviewContextBg", {})
  vim.api.nvim_set_hl(0, "DiffReviewAddRange", { fg = "#50fa7b" })
  vim.api.nvim_set_hl(0, "DiffReviewDeleteRange", { fg = "#ff5555" })
  vim.api.nvim_set_hl(0, "DiffReviewDirName", { fg = "#7f8790", nocombine = true, ctermfg = 8 })
  vim.api.nvim_set_hl(0, "DiffReviewFileName", { fg = "#ffffff", nocombine = true, ctermfg = 15 })
  vim.api.nvim_set_hl(0, "SnacksDiffHunkHeader", { bg = "#1e1e1e" })
  vim.api.nvim_set_hl(0, "DiffReviewActiveHunkHeader", { bg = "#303446" })
  vim.api.nvim_set_hl(0, "DiffReviewHunkHeader", { fg = header_fg, bg = "#1e1e1e", nocombine = true })
  vim.api.nvim_set_hl(0, "DiffReviewStatusHeader", { fg = "#f8f8f2", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewStatusHint", { fg = "#9ca3af" })
  vim.api.nvim_set_hl(0, "DiffReviewStatusHintKey", { fg = "#f8f8f2", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewStatusFold", { fg = "#9ca3af" })
  vim.api.nvim_set_hl(0, "DiffReviewStatusPath", { fg = "#d4d4d4" })
  vim.api.nvim_set_hl(0, "DiffReviewStatusObjectId", { fg = "#6b7280" })
  vim.api.nvim_set_hl(0, "DiffReviewStatusBranch", { fg = "#f1fa8c", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewStatusRemote", { fg = "#00afff", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewStatusLabel", { fg = "#c0c0c0", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewHunkContext", {
    fg = header_fg,
    bg = "#1e1e1e",
    underline = true,
    nocombine = true,
  })
end

return M
