---@class ForgeHarnessProviderDescriptor
---@field kind string
---@field command string[]
---@field permission_note string

local M = {}

---@param harness_config ForgeHarnessConfig
---@return ForgeHarnessProviderDescriptor
function M.descriptor(harness_config)
  local provider = harness_config.providers.copilot or {}
  return {
    kind = "copilot",
    command = vim.deepcopy(provider.command or {}),
    permission_note = "Copilot routes approvals through Harness permission policy",
  }
end

return M
