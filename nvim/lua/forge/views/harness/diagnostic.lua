local M = {}

---@class ForgeHarnessDiagnostic
---@field text string
---@field owner string
---@field category string
---@field severity 'error'|'warning'|'info'
---@field recovery string?
---@field clear_when string?
---@field failed boolean
---@field waiting boolean
---@field animated boolean

---@class ForgeHarnessDiagnosticOptions
---@field owner string?
---@field category string?
---@field severity 'error'|'warning'|'info'?
---@field recovery string?
---@field clear_when string?

---Publish a notice whose lifecycle belongs to one operation or synchronization owner.
---@param state table
---@param field string
---@param text string?
---@param options ForgeHarnessDiagnosticOptions?
function M.set(state, field, text, options)
  state.diagnostic = state.diagnostic or {}
  state[field] = text
  if not text then state.diagnostic[field] = nil return end
  local value = vim.tbl_extend('force', { owner = field, category = field, severity = 'info',
    recovery = 'retry', clear_when = 'matching successful observation' }, options or {})
  value.text, value.animated = text, false
  value.failed, value.waiting = value.severity == 'error', value.recovery == 'retry'
  state.diagnostic[field] = value
end

---@param state table
---@param field string
---@param severity 'error'|'warning'|'info'
---@return ForgeHarnessDiagnostic?
function M.get(state, field, severity)
  local text = state[field]
  if not text then return end
  local saved = state.diagnostic and state.diagnostic[field]
  if saved and saved.text == text then return saved end
  return { text = text, owner = field, category = field, severity = severity,
    failed = severity == 'error', waiting = severity == 'info', animated = false }
end

return M
