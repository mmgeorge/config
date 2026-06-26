local commands = require("diff_review.views.commands")
---@class DiffReviewPRViewOptions
---@field cwd? string
---@field repo? string

local M = {}

---@param pr DiffReviewGhPR
---@param opts? DiffReviewPRViewOptions
---@return integer? buf
function M.open(pr, opts)
  return commands.open_pr(pr, opts)
end

return M
