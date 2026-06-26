--- Owns the plugin's shared Neovim namespaces, extmark priorities, and icon glyphs, created
--- once at load so every layer (render, views, git) reaches them through a direct require
--- instead of the init facade.
---
--- `nvim_create_namespace` returns a stable id per name, so creating the namespaces here rather
--- than on init changes no ids.
---@class DiffReviewUiModule
local M = {}

-- Extmark namespaces.
M.preview_ns = vim.api.nvim_create_namespace("diff_review_preview")
M.gutter_visual_ns = vim.api.nvim_create_namespace("diff_review_visual_gutter")
M.hunk_header_ns = vim.api.nvim_create_namespace("diff_review_headers")
M.active_hunk_header_ns = vim.api.nvim_create_namespace("diff_review_active_hunk")
M.status_ns = vim.api.nvim_create_namespace("diff_review_status")
M.status_decorate_ns = vim.api.nvim_create_namespace("diff_review_status_decorate")

-- Extmark priorities for the hunk-header overlays.
M.hunk_header_priority = 20
M.active_hunk_header_priority = 200

-- Icon glyphs used across the PR, review, and status surfaces.
M.comment_icon = "󰅺"
M.reply_icon = "↳"
M.pending_review_icon = "◷"
M.codeowner_review_icon = "⚠"
M.milestone_icon = "◆"

return M
