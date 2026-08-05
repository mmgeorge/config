---@alias DiffReviewPerfScope "diff"|"harness"

---@class DiffReviewPerfScopeConfig
---@field enabled boolean
---@field log_path string?
---@field slow_threshold_ms number
---@field sample_rate number
---@field flush_delay_ms? integer

---@class DiffReviewPerfConfig
---@field diff DiffReviewPerfScopeConfig
---@field harness DiffReviewPerfScopeConfig

---@class DiffReviewPerfModule
---@field options DiffReviewPerfConfig
---@field sequence table<DiffReviewPerfScope, integer>
---@field queue table<DiffReviewPerfScope, string[]?>
---@field flush_pending table<DiffReviewPerfScope, boolean>
local M = {}

M.options = {
  diff = { enabled = false, log_path = nil, slow_threshold_ms = 8, sample_rate = 1, flush_delay_ms = 25 },
  harness = { enabled = false, log_path = nil, slow_threshold_ms = 8, sample_rate = 1, flush_delay_ms = 25 },
}
M.sequence = { diff = 0, harness = 0 }
M.queue = { diff = nil, harness = nil }
M.flush_pending = { diff = false, harness = false }

---@param options? DiffReviewPerfConfig
function M.setup(options)
  M.options = vim.tbl_deep_extend("force", vim.deepcopy(M.options), options or {})
end

---@param options table?
function M.configure_from_diff_review_options(options)
  options = options or {}
  M.setup({
    diff = {
      enabled = options.diff_logging == true,
      log_path = options.diff_log_path,
      slow_threshold_ms = tonumber(options.perf_slow_threshold_ms) or M.options.diff.slow_threshold_ms,
      sample_rate = tonumber(options.perf_sample_rate) or M.options.diff.sample_rate,
    },
    harness = {
      enabled = options.harness_logging == true,
      log_path = options.harness_log_path,
      slow_threshold_ms = tonumber(options.perf_slow_threshold_ms) or M.options.harness.slow_threshold_ms,
      sample_rate = tonumber(options.perf_sample_rate) or M.options.harness.sample_rate,
    },
  })
end

---@return integer
function M.now()
  local uv = vim.uv or vim.loop
  return uv.hrtime()
end

---@param started integer
---@return number
function M.elapsed_ms(started)
  local elapsed = M.now() - started
  return math.floor((elapsed / 1000000) * 1000 + 0.5) / 1000
end

---@param scope DiffReviewPerfScope
---@return DiffReviewPerfScopeConfig
local function scope_options(scope)
  return M.options[scope]
end

---@param scope DiffReviewPerfScope
---@return boolean
function M.enabled(scope)
  return scope_options(scope).enabled == true
end

---@param scope DiffReviewPerfScope
---@return string
function M.log_path(scope)
  local options = scope_options(scope)
  if options.log_path and options.log_path ~= "" then return options.log_path end
  return vim.fn.stdpath("cache") .. "/diff-review-" .. scope .. "-perf.log"
end

---@param scope DiffReviewPerfScope
---@param err any
local function notify_write_error(scope, err)
  if not err then return end
  pcall(vim.schedule, function()
    vim.notify("DiffReview " .. scope .. " perf log failed: " .. tostring(err), vim.log.levels.WARN, { title = "DiffReview" })
  end)
end

---@param scope DiffReviewPerfScope
function M.flush(scope)
  local line = M.queue[scope]
  M.queue[scope] = nil
  M.flush_pending[scope] = false
  if not (line and #line > 0) then return end
  local text = table.concat(line, "\n") .. "\n"
  local uv = vim.uv or vim.loop
  if uv and uv.fs_open and uv.fs_write and uv.fs_close then
    uv.fs_open(M.log_path(scope), "a", 438, function(open_err, fd)
      if open_err or not fd then
        notify_write_error(scope, open_err or "open failed")
        return
      end
      uv.fs_write(fd, text, -1, function(write_err)
        uv.fs_close(fd, function(close_err)
          notify_write_error(scope, write_err or close_err)
        end)
      end)
    end)
    return
  end
  local ok, err = pcall(vim.fn.writefile, line, M.log_path(scope), "a")
  if not ok then notify_write_error(scope, err) end
end

---@param payload table
---@return table
local function copied_payload(payload)
  if type(payload) ~= "table" then return {} end
  return vim.deepcopy(payload)
end

---@param scope DiffReviewPerfScope
---@param event string
---@param payload table?
function M.event(scope, event, payload)
  if not M.enabled(scope) then return end
  M.sequence[scope] = M.sequence[scope] + 1
  local record = copied_payload(payload or {})
  record.seq = M.sequence[scope]
  record.scope = scope
  record.event = event
  record.time = os.date("%Y-%m-%d %H:%M:%S")
  record.kind = "diff_review.infra.perf"
  M.queue[scope] = M.queue[scope] or {}
  M.queue[scope][#M.queue[scope] + 1] = vim.json.encode(record)
  if M.flush_pending[scope] then return end
  M.flush_pending[scope] = true
  vim.defer_fn(function()
    M.flush(scope)
  end, scope_options(scope).flush_delay_ms or 25)
end

---@param scope DiffReviewPerfScope
---@param event string
---@param payload table?
---@param callback fun(): any
---@return any
function M.span(scope, event, payload, callback)
  if not M.enabled(scope) then return callback() end
  local started = M.now()
  local function pack_result(...)
    return { n = select("#", ...), ... }
  end
  local result = pack_result(pcall(callback))
  local ok = result[1]
  local next_payload = copied_payload(payload or {})
  next_payload.ms = M.elapsed_ms(started)
  if not ok then
    next_payload.error = tostring(result[2])
    M.event(scope, event .. ".error", next_payload)
    error(result[2], 0)
  end
  local options = scope_options(scope)
  if next_payload.ms >= (tonumber(options.slow_threshold_ms) or 0) then
    M.event(scope, event, next_payload)
  elseif (tonumber(options.sample_rate) or 1) >= 1 then
    M.event(scope, event, next_payload)
  end
  ---@diagnostic disable-next-line: deprecated
  local unpack_value = table.unpack or unpack
  return unpack_value(result, 2, result.n)
end

return M
