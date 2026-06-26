--- Owns performance tracing for the diff-review render paths: builds the per-buffer perf payload
--- and fans each event/span out to the JSON perf log (infra/perf) and, when enabled, the GitStatus
--- debug perf log. Lives in infra so render, git, and view callers reach it downward rather than
--- through views/status.
---
--- The debug sink (views/status/status_debug) is injected via set_debug_sink, so infra never
--- depends on a view.
---@class DiffReviewPerfTraceModule
local M = {}

local perf = require("diff_review.infra.perf")
local session = require("diff_review.session")

--- The GitStatus debug perf sink, registered at load by status_debug (infra must not require views).
---@type table?
local debug_sink = nil

--- Register the GitStatus debug perf sink so spans and events also reach the dev perf log.
---@param sink table
function M.set_debug_sink(sink)
  debug_sink = sink
end

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
  if perf and perf.enabled() then perf.event(event, payload) end
  if not (debug_sink and debug_sink.perf_event) then return end
  debug_sink.perf_event(event, payload)
end

---@param event string
---@param buf integer?
---@param extra table?
---@param callback fun(): ...
---@return any
function M.span(event, buf, extra, callback)
  local payload = M.payload(buf, extra)
  if perf and perf.enabled() then
    return perf.span(event, payload, function()
      if not (debug_sink and debug_sink.perf_span and debug_sink.perf_enabled and debug_sink.perf_enabled()) then return callback() end
      return debug_sink.perf_span(event, payload, callback)
    end)
  end
  if not (debug_sink and debug_sink.perf_span and debug_sink.perf_enabled and debug_sink.perf_enabled()) then return callback() end
  return debug_sink.perf_span(event, payload, callback)
end

return M
