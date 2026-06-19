local M = {}

function M.setup()
  local function get_bg(name)
    local hl = vim.api.nvim_get_hl(0, { name = name, link = false })
    return hl.bg
  end

  local add_bg = "#103f10"
  local del_bg = "#2b0000"
  local normal = vim.api.nvim_get_hl(0, { name = "Normal", link = false })
  local header_fg = normal.fg or "#c0c0c0"

  vim.api.nvim_set_hl(0, "DiffReviewAddBg", { bg = add_bg })
  vim.api.nvim_set_hl(0, "DiffReviewDeleteBg", { bg = del_bg })
  vim.api.nvim_set_hl(0, "DiffReviewAddLineNr", { fg = "#50fa7b", bg = add_bg, bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewDeleteLineNr", { fg = "#ff5555", bg = del_bg, bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewCompactAddLineNr", { fg = "#50fa7b", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewCompactDeleteLineNr", { fg = "#ff5555", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewContextLineNr", { fg = "#555555" })
  vim.api.nvim_set_hl(0, "DiffReviewContextBg", {})
  vim.api.nvim_set_hl(0, "DiffReviewAddRange", { fg = "#50fa7b" })
  vim.api.nvim_set_hl(0, "DiffReviewDeleteRange", { fg = "#ff5555" })
  vim.api.nvim_set_hl(0, "DiffReviewDirName", { fg = "#7f8790", nocombine = true, ctermfg = 8 })
  vim.api.nvim_set_hl(0, "DiffReviewFileName", { fg = "#ffffff", nocombine = true, ctermfg = 15 })
  vim.api.nvim_set_hl(0, "SnacksDiffHunkHeader", { bg = "#1e1e1e" })
  vim.api.nvim_set_hl(0, "DiffReviewActiveHunkHeader", { bg = "#303446" })
  vim.api.nvim_set_hl(0, "DiffReviewHunkHeader", { fg = header_fg, nocombine = true })
  vim.api.nvim_set_hl(0, "DiffReviewStatusHeader", { fg = "#f8f8f2", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewStatusHint", { fg = "#9ca3af" })
  vim.api.nvim_set_hl(0, "DiffReviewStatusHintKey", { fg = "#f8f8f2", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewStatusFold", { fg = "#9ca3af" })
  vim.api.nvim_set_hl(0, "DiffReviewStatusPath", { fg = "#d4d4d4" })
  vim.api.nvim_set_hl(0, "DiffReviewStatusFileNew", { fg = "#50fa7b", bold = true, italic = true })
  vim.api.nvim_set_hl(0, "DiffReviewStatusFileModified", { fg = "#61afef", bold = true, italic = true })
  vim.api.nvim_set_hl(0, "DiffReviewStatusFileDeleted", { fg = "#ff5555", bold = true, italic = true })
  vim.api.nvim_set_hl(0, "DiffReviewStatusObjectId", { fg = "#6b7280" })
  vim.api.nvim_set_hl(0, "DiffReviewStatusCommitType", { fg = "#5bff94", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewStatusDate", { link = "Comment" })
  vim.api.nvim_set_hl(0, "DiffReviewStatusOpen", { fg = "#50fa7b", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewStatusClosed", { fg = "#6b7280", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewStatusBranch", { fg = "#f1fa8c", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewStatusRemote", { fg = "#00afff", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewStatusLabel", { fg = "#c0c0c0", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewStatusPR", { fg = "#d4d4d4" })
  vim.api.nvim_set_hl(0, "DiffReviewStatusFetching", { fg = "#9ca3af", italic = true })
  vim.api.nvim_set_hl(0, "DiffReviewHunkContext", {
    fg = header_fg,
    underline = true,
    nocombine = true,
  })
  vim.api.nvim_set_hl(0, "DiffReviewHunkBoundary", {
    fg = "#9ca3af",
    italic = true,
    nocombine = true,
  })
  vim.api.nvim_set_hl(0, "DiffReviewFileRevisionHeader", { fg = "#ff5555", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewPrDirty", { fg = "#ff5555", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewReviewComment", { fg = "#d4d4d4" })
  vim.api.nvim_set_hl(0, "DiffReviewReviewCommentHeader", { fg = "#61afef", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewReviewPending", { fg = "#e5c07b", italic = true })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughRegion", { bg = "#2d3a55" })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughRegionAdd", { bg = add_bg })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughRegionDelete", { bg = del_bg })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughStale", { fg = "#e5c07b", italic = true })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughJustification", { fg = "#e5c07b", italic = true })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughComment", { fg = "#d4d4d4" })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughCalloutImportant", { fg = "#e5c07b", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughCalloutLimitation", { fg = "#61afef", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughCalloutTemporary", { fg = "#c678dd", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughCalloutRisk", { fg = "#ff5555", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughCalloutFollowup", { fg = "#56b6c2", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughCalloutDeviation", { fg = "#ff5555", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughCalloutWorkaround", { fg = "#e5c07b", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughActionAdd", { fg = "#50fa7b", bold = true, italic = true })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughActionUpdate", { fg = "#61afef", bold = true, italic = true })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughActionMove", { fg = "#c678dd", bold = true, italic = true })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughActionRemove", { fg = "#ff5555", bold = true, italic = true })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughActionSplit", { fg = "#56b6c2", bold = true, italic = true })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughType", { fg = "#5bff94" })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughItemTitle", { fg = "#ffffff", bold = true })
end

return M
