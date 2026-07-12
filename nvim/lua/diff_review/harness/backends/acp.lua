---@class DiffReviewHarnessBackendDescriptor
---@field kind string
---@field command string[]
---@field permission_note string

local M = {}

---@param harness_config DiffReviewHarnessConfig
---@return DiffReviewHarnessBackendDescriptor
function M.descriptor(harness_config)
  local backend = harness_config.backends.acp or {}
  return {
    kind = "acp",
    command = vim.deepcopy(backend.command or {}),
    permission_note = "ACP permissions are best effort for provider-private tools",
  }
end

return M

