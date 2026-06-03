---@class DiffReviewConfig
---@field status_buffer_name string
---@field pr_buffer_name string

---@class DiffReviewConfigModule
---@field defaults DiffReviewConfig
---@field options DiffReviewConfig
---@field setup fun(opts?: DiffReviewConfig): DiffReviewConfig

---@type DiffReviewConfigModule
local M = {}

---@type DiffReviewConfig
M.defaults = {
  status_buffer_name = "DiffReviewStatus",
  pr_buffer_name = "DiffReviewPR",
}

---@type DiffReviewConfig
M.options = vim.deepcopy(M.defaults)

---@param opts? DiffReviewConfig
---@return DiffReviewConfig
function M.setup(opts)
  M.options = vim.tbl_deep_extend("force", vim.deepcopy(M.defaults), opts or {})
  return M.options
end

return M
