--- Convenience wrapper for opening pull request view buffers.
---@class DiffReviewPRViewModule
local M = {}

local commands = require("diff_review.views.commands")

---@class DiffReviewPRViewOptions
---@field cwd? string
---@field repo? string

--- Opens a GitHub pull request overview buffer for the given PR descriptor.
---@param pr DiffReviewGhPR Pull request descriptor.
---@param opts? DiffReviewPRViewOptions Open PR options table.
---@return integer? buf Buffer handle or nil on failure.
function M.open(pr, opts)
  return commands.open_pr(pr, opts)
end

return M
