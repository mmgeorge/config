--- Defines every DiffReview highlight group (add/delete/modify backgrounds, gutter,
--- inline change, fold, status file-state, header, hunk context, and review/walkthrough
--- groups) at setup time, deriving backgrounds from the active colorscheme.

local M = {}

function M.setup()
  local palette = require("theme.palette")
  local function get_bg(name)
    local hl = vim.api.nvim_get_hl(0, { name = name, link = false })
    return hl.bg
  end

  local add_bg = "#002800"
  local del_bg = "#200000"
  local modify_bg = "#2a1f00"
  local modify_fg = "#ffb86c"
  local inline_add_bg = "#0b6b2a"
  local inline_del_bg = "#6a1010"
  local review_comment_bg = "#000000"
  local normal = vim.api.nvim_get_hl(0, { name = "Normal", link = false })
  local header_fg = normal.fg or "#c0c0c0"
  local keyword_modifier = vim.api.nvim_get_hl(0, { name = "@keyword.modifier", link = false })
  local type_hl = vim.api.nvim_get_hl(0, { name = "Type", link = false })
  local statement_hl = vim.api.nvim_get_hl(0, { name = "Statement", link = false })
  local variable_hl = vim.api.nvim_get_hl(0, { name = "@variable", link = false })

  vim.api.nvim_set_hl(0, "DiffReviewAddBg", { bg = add_bg })
  vim.api.nvim_set_hl(0, "DiffReviewDeleteBg", { bg = del_bg })
  vim.api.nvim_set_hl(0, "DiffReviewModifyBg", { bg = modify_bg })
  vim.api.nvim_set_hl(0, "DiffReviewInlineAddBg", { bg = inline_add_bg, nocombine = true })
  vim.api.nvim_set_hl(0, "DiffReviewInlineDeleteBg", { bg = inline_del_bg, nocombine = true })
  vim.api.nvim_set_hl(0, "DiffReviewAddLineNr", { fg = "#50fa7b", bg = add_bg, bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewDeleteLineNr", { fg = "#ff5555", bg = del_bg, bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewModifyLineNr", { fg = modify_fg, bg = modify_bg, bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewContextLineNr", { fg = "#555555" })
  vim.api.nvim_set_hl(0, "DiffReviewContextBg", {})
  vim.api.nvim_set_hl(0, "DiffReviewAddRange", { fg = "#50fa7b" })
  vim.api.nvim_set_hl(0, "DiffReviewDeleteRange", { fg = "#ff5555" })
  vim.api.nvim_set_hl(0, "DiffReviewModifyRange", { fg = modify_fg })
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
  vim.api.nvim_set_hl(0, "DiffReviewHarnessPrompt", { fg = palette.light_blue, bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewHarnessThought", { fg = palette.white, bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewHarnessThinking", { fg = palette.white, bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewTimelineStatus", { fg = palette.white })
  vim.api.nvim_set_hl(0, "DiffReviewTimelineStatusSpinner", { fg = palette.green, bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewHarnessResponse", { fg = palette.white, bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewHarnessGoal", { fg = palette.green, bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewHarnessRead", { fg = palette.white, bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewHarnessWrite", { fg = palette.orange, bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewHarnessFull", { fg = palette.red, bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewHarnessYolo", { fg = palette.purple, bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewHarnessCommentary", { italic = true })
  vim.api.nvim_set_hl(0, "DiffReviewHarnessCommand", { fg = palette.light_blue, bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewHarnessOption", { fg = palette.red })
  vim.api.nvim_set_hl(0, "DiffReviewPickerSelected", { fg = palette.light_blue, bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewPickerChosen", { fg = palette.green, bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewPickerKey", { fg = palette.yellow, bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewPickerHint", { fg = palette.gray })
  vim.api.nvim_set_hl(0, "DiffReviewPickerSection", { fg = palette.white, bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewPickerOption", { fg = header_fg })
  vim.api.nvim_set_hl(0, "DiffReviewPickerText", { fg = header_fg })
  vim.api.nvim_set_hl(0, "DiffReviewPickerQuestion", { fg = palette.white })
  vim.api.nvim_set_hl(0, "DiffReviewPickerAnswer", { fg = palette.light_blue })
  vim.api.nvim_set_hl(0, "DiffReviewHiddenCursor", {
    fg = palette.black,
    bg = palette.black,
    blend = 100,
    nocombine = true,
  })
  vim.api.nvim_set_hl(0, "DiffReviewHarnessArgument", { fg = palette.white })
  vim.api.nvim_set_hl(0, "DiffReviewHarnessOutput", { fg = palette.gray })
  vim.api.nvim_set_hl(0, "DiffReviewHarnessToolSuccess", { fg = palette.green })
  vim.api.nvim_set_hl(0, "DiffReviewHarnessToolFailure", { fg = palette.red })
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
  -- Shared comment text/header groups: PR-overview review/comment list rows, the review
  -- summary field, and standalone PR comments. No background — only the inline comment BOXES
  -- (the dark gray cards below diff rows) carry one, via the dedicated *Box groups below.
  vim.api.nvim_set_hl(0, "DiffReviewReviewComment", { fg = "#d4d4d4" })
  vim.api.nvim_set_hl(0, "DiffReviewReviewCommentHeader", { fg = "#61afef", bold = true })
  -- Inline comment box (header/footer rule + body) anchored under a diff row: dark gray card
  -- with white text/border so it reads as distinct from the code it comments on.
  vim.api.nvim_set_hl(0, "DiffReviewReviewCommentBox", { fg = "#ffffff", bg = review_comment_bg })
  vim.api.nvim_set_hl(0, "DiffReviewReviewCommentBoxHeader", { fg = "#ffffff", bg = review_comment_bg, bold = true })
  -- Cleared (no background) group for rendered markdown headings inside diff_review regions.
  -- render-markdown paints a heading-line background (hl_eol) that would otherwise override the
  -- comment box's dark gray on a `# heading` row; pointing its backgrounds at this empty group
  -- lets the row keep its underlying line background — the box dark gray, or the normal
  -- description background — so the heading no longer shows a mismatched tint.
  vim.api.nvim_set_hl(0, "DiffReviewMarkdownHeadingBg", {})
  vim.api.nvim_set_hl(0, "DiffReviewReviewPending", { fg = "#e5c07b", italic = true })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughRegion", { bg = "#2d3a55" })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughRegionAdd", { bg = add_bg })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughRegionDelete", { bg = del_bg })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughStale", { fg = "#e5c07b", italic = true })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughJustification", { fg = "#e5c07b", italic = true })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughComment", { fg = "#d4d4d4" })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughLocation", { fg = "#9ca3af" })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughCalloutImportant", { fg = "#e5c07b", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughCalloutLimitation", { fg = "#61afef", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughCalloutTemporary", { fg = "#c678dd", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughCalloutRisk", { fg = "#ff5555", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughCalloutFollowup", { fg = "#56b6c2", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughCalloutDeviation", { fg = "#ff5555", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughCalloutWorkaround", { fg = "#e5c07b", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughActionAdd", { fg = "#50fa7b", bold = true, italic = true })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughActionModify", { fg = "#61afef", bold = true, italic = true })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughActionRemove", { fg = "#ff5555", bold = true, italic = true })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughType", { fg = "#5bff94" })
  vim.api.nvim_set_hl(0, "DiffReviewWalkthroughItemTitle", { fg = "#ffffff", bold = true })
  if keyword_modifier.fg == normal.fg then
    vim.api.nvim_set_hl(0, "@keyword.modifier", { fg = "#ff79c6", bold = true })
  end
  if type_hl.fg == normal.fg then
    vim.api.nvim_set_hl(0, "Type", { fg = "#8be9fd" })
    vim.api.nvim_set_hl(0, "@type", { fg = "#8be9fd" })
  end
  if statement_hl.fg == normal.fg then
    vim.api.nvim_set_hl(0, "Statement", { fg = "#ff79c6", bold = true })
    vim.api.nvim_set_hl(0, "@keyword", { fg = "#ff79c6", bold = true })
  end
  if variable_hl.fg == normal.fg then
    vim.api.nvim_set_hl(0, "@variable", { fg = "#f8f8f2" })
  end
end

return M
