local M = {}

---@param harness_config DiffReviewHarnessConfig
---@return DiffReviewHarnessBackendDescriptor
function M.descriptor(harness_config)
  local backend = harness_config.backends.mock or {}
  return { kind = "mock", command = vim.deepcopy(backend.command or { "mock" }), permission_note = "Mock backend" }
end

return M

