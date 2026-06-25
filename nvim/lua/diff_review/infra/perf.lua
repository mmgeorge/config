---@class DiffReviewPerfConfig
---@field enabled boolean
---@field log_path string?
---@field slow_threshold_ms number
---@field sample_rate number
---@field flush_delay_ms integer

---@class DiffReviewPerfModule
---@field options DiffReviewPerfConfig
---@field sequence integer
---@field queue string[]?
---@field flush_pending boolean
local M = {}

M.options = {
  enabled = false,
  log_path = nil,
  slow_threshold_ms = 8,
  sample_rate = 1,
  flush_delay_ms = 25,
}
M.sequence = 0
M.queue = nil
M.flush_pending = false

---@param options? DiffReviewPerfConfig
function M.setup(options)
  M.options = vim.tbl_deep_extend("force", vim.deepcopy(M.options), options or {})
end

---@param options table?
function M.configure_from_diff_review_options(options)
  options = options or {}
  M.setup({
    enabled = options.diff_profile_enabled == true,
    log_path = options.diff_profile_log_path,
    slow_threshold_ms = tonumber(options.diff_profile_slow_threshold_ms) or M.options.slow_threshold_ms,
    sample_rate = tonumber(options.diff_profile_sample_rate) or M.options.sample_rate,
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

---@return string
function M.log_path()
  if M.options.log_path and M.options.log_path ~= "" then return M.options.log_path end
  return vim.fn.stdpath("cache") .. "/diff-review-perf.log"
end

---@return boolean
function M.enabled()
  return M.options.enabled == true
end

---@param err any
local function notify_write_error(err)
  if not err then return end
  pcall(vim.schedule, function()
    vim.notify("DiffReview perf log failed: " .. tostring(err), vim.log.levels.WARN, { title = "DiffReview" })
  end)
end

function M.flush()
  local line = M.queue
  M.queue = nil
  M.flush_pending = false
  if not (line and #line > 0) then return end
  local text = table.concat(line, "\n") .. "\n"
  local uv = vim.uv or vim.loop
  if uv and uv.fs_open and uv.fs_write and uv.fs_close then
    uv.fs_open(M.log_path(), "a", 438, function(open_err, fd)
      if open_err or not fd then
        notify_write_error(open_err or "open failed")
        return
      end
      uv.fs_write(fd, text, -1, function(write_err)
        uv.fs_close(fd, function(close_err)
          notify_write_error(write_err or close_err)
        end)
      end)
    end)
    return
  end
  local ok, err = pcall(vim.fn.writefile, line, M.log_path(), "a")
  if not ok then notify_write_error(err) end
end

---@param payload table
---@return table
local function copied_payload(payload)
  if type(payload) ~= "table" then return {} end
  return vim.deepcopy(payload)
end

---@param event string
---@param payload table?
function M.event(event, payload)
  if not M.enabled() then return end
  M.sequence = M.sequence + 1
  local record = copied_payload(payload or {})
  record.seq = M.sequence
  record.event = event
  record.time = os.date("%Y-%m-%d %H:%M:%S")
  record.kind = "diff_review.infra.perf"
  M.queue = M.queue or {}
  M.queue[#M.queue + 1] = vim.json.encode(record)
  if M.flush_pending then return end
  M.flush_pending = true
  vim.defer_fn(function()
    M.flush()
  end, M.options.flush_delay_ms or 25)
end

---@param event string
---@param payload table?
---@param callback fun(): any
---@return any
function M.span(event, payload, callback)
  if not M.enabled() then return callback() end
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
    M.event(event .. ".error", next_payload)
    error(result[2], 0)
  end
  if next_payload.ms >= (tonumber(M.options.slow_threshold_ms) or 0) then
    M.event(event, next_payload)
  elseif (tonumber(M.options.sample_rate) or 1) >= 1 then
    M.event(event, next_payload)
  end
  local unpack_value = table.unpack or unpack
  return unpack_value(result, 2, result.n)
end

return M
