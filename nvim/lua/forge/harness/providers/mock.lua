local M = {}

---@param harness_config ForgeHarnessConfig
---@return ForgeHarnessProviderDescriptor
function M.descriptor(harness_config)
  local provider = harness_config.providers.mock or {}
  return { kind = "mock", command = vim.deepcopy(provider.command or { "mock" }), permission_note = "Mock provider" }
end

return M

