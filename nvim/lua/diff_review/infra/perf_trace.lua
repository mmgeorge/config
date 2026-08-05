--- Owns diff-scope performance tracing for render, GitStatus, PR, and shared UI paths. Builds
--- per-buffer payloads and writes structured events through infra/perf without depending on views.
---@class DiffReviewPerfTraceModule
local M = {}

local perf = require("diff_review.infra.perf")
local session = require("diff_review.session")

--- Build the perf payload for a status buffer from its live state, enriching with cursor, line
--- count, and viewport fields when the buffer is current.
---@param buf integer?
---@param extra? table
---@return table
function M.payload(buf, extra)
  local state = buf and session.states and session.states[buf] or nil
  if not state and session.status and (not buf or session.status.buf == buf) then state = session.status end
  local payload = vim.deepcopy(extra or {})
  payload.buf = buf
  payload.view_kind = state and state.view_kind or nil
  if buf and vim.api.nvim_buf_is_valid(buf) then
    payload.line_count = vim.api.nvim_buf_line_count(buf)
  end
  if buf and vim.api.nvim_get_current_buf() == buf then
    local cursor = vim.api.nvim_win_get_cursor(0)
    payload.cursor_row = cursor[1]
    payload.cursor_col = cursor[2]
  end
  local viewport = state and state.diff_viewport or nil
  if viewport and viewport.enabled then
    payload.viewport_top = viewport.top
    payload.viewport_total = viewport.total
    payload.viewport_logical_total = viewport.logical_total
    payload.viewport_render_count = viewport.render_count
  end
  return payload
end

---@param event string
---@param buf integer?
---@param extra table?
function M.event(event, buf, extra)
  local payload = M.payload(buf, extra)
  if perf and perf.enabled("diff") then perf.event("diff", event, payload) end
end

---@param event string
---@param buf integer?
---@param extra table?
---@param callback fun(): ...
---@return any
function M.span(event, buf, extra, callback)
  local payload = M.payload(buf, extra)
  if perf and perf.enabled("diff") then return perf.span("diff", event, payload, callback) end
  return callback()
end

return M
