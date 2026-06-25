---@class DiffReviewPRViewOptions
---@field cwd? string
---@field repo? string

local M = {}

---@param pr DiffReviewGhPR
---@param opts? DiffReviewPRViewOptions
---@return integer? buf
function M.open(pr, opts)
  return require("diff_review").open_pr(pr, opts)
end

return M
