--- Defines every Forge highlight group (add/delete/modify backgrounds, gutter,
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

  vim.api.nvim_set_hl(0, "ForgeAddBg", { bg = add_bg })
  vim.api.nvim_set_hl(0, "ForgeDeleteBg", { bg = del_bg })
  vim.api.nvim_set_hl(0, "ForgeModifyBg", { bg = modify_bg })
  vim.api.nvim_set_hl(0, "ForgeInlineAddBg", { bg = inline_add_bg, nocombine = true })
  vim.api.nvim_set_hl(0, "ForgeInlineDeleteBg", { bg = inline_del_bg, nocombine = true })
  vim.api.nvim_set_hl(0, "ForgeAddLineNr", { fg = "#50fa7b", bg = add_bg, bold = true })
  vim.api.nvim_set_hl(0, "ForgeDeleteLineNr", { fg = "#ff5555", bg = del_bg, bold = true })
  vim.api.nvim_set_hl(0, "ForgeModifyLineNr", { fg = modify_fg, bg = modify_bg, bold = true })
  vim.api.nvim_set_hl(0, "ForgeContextLineNr", { fg = "#555555" })
  vim.api.nvim_set_hl(0, "ForgeContextBg", {})
  vim.api.nvim_set_hl(0, "ForgeAddRange", { fg = "#50fa7b" })
  vim.api.nvim_set_hl(0, "ForgeDeleteRange", { fg = "#ff5555" })
  vim.api.nvim_set_hl(0, "ForgeModifyRange", { fg = modify_fg })
  vim.api.nvim_set_hl(0, "ForgeDirName", { fg = "#7f8790", nocombine = true, ctermfg = 8 })
  vim.api.nvim_set_hl(0, "ForgeFileName", { fg = "#ffffff", nocombine = true, ctermfg = 15 })
  vim.api.nvim_set_hl(0, "SnacksDiffHunkHeader", { bg = "#1e1e1e" })
  vim.api.nvim_set_hl(0, "ForgeActiveHunkHeader", { bg = "#303446" })
  vim.api.nvim_set_hl(0, "ForgeHunkHeader", { fg = header_fg, nocombine = true })
  vim.api.nvim_set_hl(0, "ForgeStatusHeader", { fg = "#f8f8f2", bold = true })
  vim.api.nvim_set_hl(0, "ForgeStatusHint", { fg = "#9ca3af" })
  vim.api.nvim_set_hl(0, "ForgeStatusHintKey", { fg = "#f8f8f2", bold = true })
  vim.api.nvim_set_hl(0, "ForgeStatusFold", { fg = "#9ca3af" })
  vim.api.nvim_set_hl(0, "ForgeStatusPath", { fg = "#d4d4d4" })
  vim.api.nvim_set_hl(0, "ForgeStatusFileNew", { fg = "#50fa7b", bold = true, italic = true })
  vim.api.nvim_set_hl(0, "ForgeStatusFileModified", { fg = "#61afef", bold = true, italic = true })
  vim.api.nvim_set_hl(0, "ForgeStatusFileDeleted", { fg = "#ff5555", bold = true, italic = true })
  vim.api.nvim_set_hl(0, "ForgeStatusFileRenamed", { fg = "#c678dd", bold = true, italic = true })
  vim.api.nvim_set_hl(0, "ForgeStatusObjectId", { fg = "#6b7280" })
  vim.api.nvim_set_hl(0, "ForgeStatusCommitType", { fg = "#5bff94", bold = true })
  vim.api.nvim_set_hl(0, "ForgeStatusDate", { link = "Comment" })
  vim.api.nvim_set_hl(0, "ForgeStatusOpen", { fg = "#50fa7b", bold = true })
  vim.api.nvim_set_hl(0, "ForgeStatusClosed", { fg = "#6b7280", bold = true })
  vim.api.nvim_set_hl(0, "ForgeStatusBranch", { fg = "#f1fa8c", bold = true })
  vim.api.nvim_set_hl(0, "ForgeStatusRemote", { fg = "#00afff", bold = true })
  vim.api.nvim_set_hl(0, "ForgeStatusLabel", { fg = "#c0c0c0", bold = true })
  vim.api.nvim_set_hl(0, "ForgeHarnessPrompt", { fg = palette.light_blue, bold = true })
  vim.api.nvim_set_hl(0, "ForgeHarnessThought", { fg = palette.white, bold = true })
  vim.api.nvim_set_hl(0, "ForgeHarnessThinking", { fg = palette.white, bold = true })
  vim.api.nvim_set_hl(0, "ForgeTimelineStatus", { fg = palette.white })
  vim.api.nvim_set_hl(0, "ForgeTimelineStatusSpinner", { fg = palette.green, bold = true })
  vim.api.nvim_set_hl(0, "ForgeHarnessResponse", { fg = palette.white, bold = true })
  vim.api.nvim_set_hl(0, "ForgeHarnessGoal", { fg = palette.green, bold = true })
  vim.api.nvim_set_hl(0, "ForgeHarnessRead", { fg = palette.white, bold = true })
  vim.api.nvim_set_hl(0, "ForgeHarnessWrite", { fg = palette.orange, bold = true })
  vim.api.nvim_set_hl(0, "ForgeHarnessFull", { fg = palette.red, bold = true })
  vim.api.nvim_set_hl(0, "ForgeHarnessYolo", { fg = palette.purple, bold = true })
  vim.api.nvim_set_hl(0, "ForgeHarnessPlan", { fg = palette.purple, bold = true })
  vim.api.nvim_set_hl(0, "ForgeHarnessCommentary", { italic = true })
  vim.api.nvim_set_hl(0, "ForgeHarnessCommand", { fg = palette.light_blue, bold = true })
  vim.api.nvim_set_hl(0, "ForgeHarnessOption", { fg = palette.red })
  vim.api.nvim_set_hl(0, "ForgePickerSelected", { fg = palette.light_blue, bold = true })
  vim.api.nvim_set_hl(0, "ForgePickerChosen", { fg = palette.green, bold = true })
  vim.api.nvim_set_hl(0, "ForgePickerKey", { fg = palette.yellow, bold = true })
  vim.api.nvim_set_hl(0, "ForgePickerHint", { fg = palette.gray })
  vim.api.nvim_set_hl(0, "ForgePickerSection", { fg = palette.white, bold = true })
  vim.api.nvim_set_hl(0, "ForgePickerOption", { fg = header_fg })
  vim.api.nvim_set_hl(0, "ForgePickerText", { fg = header_fg })
  vim.api.nvim_set_hl(0, "ForgePickerQuestion", { fg = palette.white })
  vim.api.nvim_set_hl(0, "ForgePickerAnswer", { fg = palette.light_blue })
  vim.api.nvim_set_hl(0, "ForgeHiddenCursor", {
    fg = palette.black,
    bg = palette.black,
    blend = 100,
    nocombine = true,
  })
  vim.api.nvim_set_hl(0, "ForgeHarnessArgument", { fg = palette.white })
  vim.api.nvim_set_hl(0, "ForgeHarnessMcpName", { fg = palette.light_blue, bold = true })
  vim.api.nvim_set_hl(0, "ForgeHarnessMcpArguments", { fg = palette.gray })
  vim.api.nvim_set_hl(0, "ForgeHarnessOutput", { fg = palette.gray })
  vim.api.nvim_set_hl(0, "ForgeHarnessToolSuccess", { fg = palette.green })
  vim.api.nvim_set_hl(0, "ForgeHarnessToolFailure", { fg = palette.red })
  vim.api.nvim_set_hl(0, "ForgeStatusPR", { fg = "#d4d4d4" })
  vim.api.nvim_set_hl(0, "ForgeStatusFetching", { fg = "#9ca3af", italic = true })
  vim.api.nvim_set_hl(0, "ForgeHunkContext", {
    fg = header_fg,
    underline = true,
    nocombine = true,
  })
  vim.api.nvim_set_hl(0, "ForgeHunkBoundary", {
    fg = "#9ca3af",
    italic = true,
    nocombine = true,
  })
  vim.api.nvim_set_hl(0, "ForgeFileRevisionHeader", { fg = "#ff5555", bold = true })
  vim.api.nvim_set_hl(0, "ForgePrDirty", { fg = "#ff5555", bold = true })
  -- Shared comment text/header groups: PR-overview review/comment list rows, the review
  -- summary field, and standalone PR comments. No background — only the inline comment BOXES
  -- (the dark gray cards below diff rows) carry one, via the dedicated *Box groups below.
  vim.api.nvim_set_hl(0, "ForgeReviewComment", { fg = "#d4d4d4" })
  vim.api.nvim_set_hl(0, "ForgeReviewCommentHeader", { fg = "#61afef", bold = true })
  -- Inline comment box (header/footer rule + body) anchored under a diff row: dark gray card
  -- with white text/border so it reads as distinct from the code it comments on.
  vim.api.nvim_set_hl(0, "ForgeReviewCommentBox", { fg = "#ffffff", bg = review_comment_bg })
  vim.api.nvim_set_hl(0, "ForgeReviewCommentBoxHeader", { fg = "#ffffff", bg = review_comment_bg, bold = true })
  -- Cleared (no background) group for rendered markdown headings inside forge regions.
  -- render-markdown paints a heading-line background (hl_eol) that would otherwise override the
  -- comment box's dark gray on a `# heading` row; pointing its backgrounds at this empty group
  -- lets the row keep its underlying line background — the box dark gray, or the normal
  -- description background — so the heading no longer shows a mismatched tint.
  vim.api.nvim_set_hl(0, "ForgeMarkdownHeadingBg", {})
  vim.api.nvim_set_hl(0, "ForgeReviewPending", { fg = "#e5c07b", italic = true })
  vim.api.nvim_set_hl(0, "ForgeWalkthroughRegion", { bg = "#2d3a55" })
  vim.api.nvim_set_hl(0, "ForgeWalkthroughRegionAdd", { bg = add_bg })
  vim.api.nvim_set_hl(0, "ForgeWalkthroughRegionDelete", { bg = del_bg })
  vim.api.nvim_set_hl(0, "ForgeWalkthroughStale", { fg = "#e5c07b", italic = true })
  vim.api.nvim_set_hl(0, "ForgeWalkthroughJustification", { fg = "#e5c07b", italic = true })
  vim.api.nvim_set_hl(0, "ForgeWalkthroughComment", { fg = "#d4d4d4" })
  vim.api.nvim_set_hl(0, "ForgeWalkthroughLocation", { fg = "#9ca3af" })
  vim.api.nvim_set_hl(0, "ForgeWalkthroughCalloutImportant", { fg = "#e5c07b", bold = true })
  vim.api.nvim_set_hl(0, "ForgeWalkthroughCalloutLimitation", { fg = "#61afef", bold = true })
  vim.api.nvim_set_hl(0, "ForgeWalkthroughCalloutTemporary", { fg = "#c678dd", bold = true })
  vim.api.nvim_set_hl(0, "ForgeWalkthroughCalloutRisk", { fg = "#ff5555", bold = true })
  vim.api.nvim_set_hl(0, "ForgeWalkthroughCalloutFollowup", { fg = "#56b6c2", bold = true })
  vim.api.nvim_set_hl(0, "ForgeWalkthroughCalloutDeviation", { fg = "#ff5555", bold = true })
  vim.api.nvim_set_hl(0, "ForgeWalkthroughCalloutWorkaround", { fg = "#e5c07b", bold = true })
  vim.api.nvim_set_hl(0, "ForgeWalkthroughActionAdd", { fg = "#50fa7b", bold = true, italic = true })
  vim.api.nvim_set_hl(0, "ForgeWalkthroughActionModify", { fg = "#61afef", bold = true, italic = true })
  vim.api.nvim_set_hl(0, "ForgeWalkthroughActionRemove", { fg = "#ff5555", bold = true, italic = true })
  vim.api.nvim_set_hl(0, "ForgeWalkthroughActionRename", { fg = "#c678dd", bold = true, italic = true })
  vim.api.nvim_set_hl(0, "ForgeWalkthroughType", { fg = "#5bff94" })
  vim.api.nvim_set_hl(0, "ForgeWalkthroughItemTitle", { fg = "#ffffff", bold = true })
  vim.api.nvim_set_hl(0, "ForgeFileKeyword", { fg = palette.yellow })
  vim.api.nvim_set_hl(0, "ForgeFileStatusNew", { fg = "#50fa7b" })
  vim.api.nvim_set_hl(0, "ForgeFileStatusModified", { fg = "#61afef" })
  vim.api.nvim_set_hl(0, "ForgeFileStatusDeleted", { fg = "#ff5555" })
  vim.api.nvim_set_hl(0, "ForgeFileStatusRenamed", { fg = "#c678dd" })
  vim.api.nvim_set_hl(0, "ForgeDependencyName", { fg = palette.teal })
  vim.api.nvim_set_hl(0, "ForgePlanMetadata", { link = "Normal" })
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
