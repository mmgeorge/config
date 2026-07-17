---@class DiffReviewHarnessBackendDescriptor
---@field kind string
---@field command string[]
---@field permission_note string

local M = {}

---@param harness_config DiffReviewHarnessConfig
---@return DiffReviewHarnessBackendDescriptor
function M.descriptor(harness_config)
  local backend = harness_config.backends.copilot or {}
  return {
    kind = "copilot",
    command = vim.deepcopy(backend.command or {}),
    permission_note = "Copilot routes approvals through Harness permission policy",
  }
end

return M
