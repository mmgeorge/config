local M = {}

---@param harness_config ForgeHarnessConfig
---@return ForgeHarnessProviderDescriptor
function M.descriptor(harness_config)
  local provider = harness_config.providers.codex or {}
  return {
    kind = "codex",
    command = vim.deepcopy(provider.command or { "codex", "app-server" }),
    permission_note = "Codex enforces READ and WRITE through native approval and sandbox policies",
  }
end

return M

