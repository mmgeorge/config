local M = {}

---@param harness_config DiffReviewHarnessConfig
---@return DiffReviewHarnessBackendDescriptor
function M.descriptor(harness_config)
  local backend = harness_config.backends.codex or {}
  return {
    kind = "codex",
    command = vim.deepcopy(backend.command or { "codex", "app-server" }),
    permission_note = "Codex enforces READ and WRITE through native approval and sandbox policies",
  }
end

return M

