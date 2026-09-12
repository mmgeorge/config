---@alias ForgePerfScope "diff"|"harness"

---@class ForgePerfScopeConfig
---@field enabled boolean
---@field log_path string?
---@field slow_threshold_ms number
---@field sample_rate number
---@field flush_delay_ms? integer

---@class ForgePerfConfig
---@field diff ForgePerfScopeConfig
---@field harness ForgePerfScopeConfig

---@class ForgePerfModule
---@field options ForgePerfConfig
---@field sequence table<ForgePerfScope, integer>
---@field queue table<ForgePerfScope, string[]?>
---@field flush_pending table<ForgePerfScope, boolean>
local M = {}
local MAX_LOG_BYTES = 15 * 1024 * 1024
local MAX_QUEUE_BYTES = 256 * 1024
local MAX_RECORD_BYTES = 16 * 1024
local METADATA_FIELD = {
  buf = true, view_kind = true, line_count = true, cursor_row = true, cursor_col = true,
  viewport_top = true, viewport_total = true, viewport_logical_total = true, viewport_render_count = true,
  ms = true, elapsed_ms = true, duration_ms = true, count = true, rows = true, bytes = true, spans = true,
  source_bytes = true, result_bytes = true, file_count = true, hunk_count = true, changed_lines = true,
  added = true, deleted = true, phase = true, source = true, operation = true, operation_id = true,
  request_id = true, session_id = true, provider = true, method = true, event_type = true,
  status = true, code = true, enabled = true, cached = true, cancelled = true, generation = true,
  revision = true, queue_bytes = true, queue_count = true, active_jobs = true, retained_bytes = true,
  render_count = true,
}
local writing = { diff = false, harness = false }
local queue_bytes = { diff = 0, harness = 0 }

M.options = {
  diff = { enabled = false, log_path = nil, slow_threshold_ms = 8, sample_rate = 1, flush_delay_ms = 25 },
  harness = { enabled = false, log_path = nil, slow_threshold_ms = 8, sample_rate = 1, flush_delay_ms = 25 },
}
M.sequence = { diff = 0, harness = 0 }
M.queue = { diff = nil, harness = nil }
M.flush_pending = { diff = false, harness = false }

---@param options? ForgePerfConfig
function M.setup(options)
  M.options = vim.tbl_deep_extend("force", vim.deepcopy(M.options), options or {})
end

---@param options table?
function M.configure_from_forge_options(options)
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

---@param scope ForgePerfScope
---@return ForgePerfScopeConfig
local function scope_options(scope)
  return M.options[scope]
end

---@param scope ForgePerfScope
---@return boolean
function M.enabled(scope)
  return scope_options(scope).enabled == true
end

---@param scope ForgePerfScope
---@return string
function M.log_path(scope)
  local options = scope_options(scope)
  if options.log_path and options.log_path ~= "" then return options.log_path end
  return vim.fs.joinpath(vim.fn.stdpath("cache"), "forge", scope .. "-perf.log")
end

---@param scope ForgePerfScope
---@param err any
local function notify_write_error(scope, err)
  if not err then return end
  pcall(vim.schedule, function()
    vim.notify("Forge " .. scope .. " perf log failed: " .. tostring(err), vim.log.levels.WARN, { title = "Forge" })
  end)
end

---@param scope ForgePerfScope
function M.flush(scope)
  if writing[scope] then return end
  local line = M.queue[scope]
  M.queue[scope] = nil
  queue_bytes[scope] = 0
  M.flush_pending[scope] = false
  if not (line and #line > 0) then return end
  writing[scope] = true
  local text = table.concat(line, "\n") .. "\n"
  local path = M.log_path(scope)
  local default_path = not scope_options(scope).log_path or scope_options(scope).log_path == ""
  local uv = vim.uv or vim.loop
  local function finished(err)
    writing[scope] = false
    notify_write_error(scope, err)
    if M.queue[scope] then vim.schedule(function() M.flush(scope) end) end
  end
  if uv and uv.fs_open and uv.fs_write and uv.fs_close then
    local function append_log()
      uv.fs_stat(path, function(_, stat)
      local mode = stat and stat.size + #text > MAX_LOG_BYTES and "w" or "a"
      uv.fs_open(path, mode, 438, function(open_err, fd)
        if open_err or not fd then
          finished(open_err or "open failed")
          return
        end
        uv.fs_write(fd, text, -1, function(write_err)
          uv.fs_close(fd, function(close_err)
            finished(write_err or close_err)
          end)
        end)
      end)
      end)
    end
    if default_path then
      uv.fs_mkdir(vim.fs.dirname(path), 448, function(mkdir_err)
        if mkdir_err and not tostring(mkdir_err):find("EEXIST", 1, true) then
          finished(mkdir_err)
          return
        end
        append_log()
      end)
    else
      append_log()
    end
    return
  end
  local ok, err = pcall(function()
    if default_path then vim.fn.mkdir(vim.fs.dirname(path), "p") end
    local stat = uv.fs_stat(path)
    vim.fn.writefile(line, path, stat and stat.size + #text > MAX_LOG_BYTES and "" or "a")
  end)
  finished(not ok and err or nil)
end

---@param payload table
---@return table
function M.payload(payload)
  if type(payload) ~= "table" then return {} end
  local record, count = {}, 0
  for key, value in pairs(payload) do
    if count >= 32 then break end
    count = count + 1
    if METADATA_FIELD[key] then
      local kind = type(value)
      if kind == "string" then record[key] = value:sub(1, 256)
      elseif kind == "boolean" or kind == "number" then record[key] = value end
    end
  end
  return record
end

---@param scope ForgePerfScope
---@param event string
---@param payload table?
function M.event(scope, event, payload)
  if not M.enabled(scope) then return end
  M.sequence[scope] = M.sequence[scope] + 1
  local record = M.payload(payload or {})
  record.seq = M.sequence[scope]
  record.scope = scope
  record.event = event:sub(1, 256)
  record.time = os.date("%Y-%m-%d %H:%M:%S")
  record.kind = "forge.infra.perf"
  local encoded = vim.json.encode(record)
  if #encoded > MAX_RECORD_BYTES then return end
  if queue_bytes[scope] + #encoded + 1 > MAX_QUEUE_BYTES then return end
  M.queue[scope] = M.queue[scope] or {}
  M.queue[scope][#M.queue[scope] + 1] = encoded
  queue_bytes[scope] = queue_bytes[scope] + #encoded + 1
  if M.flush_pending[scope] then return end
  M.flush_pending[scope] = true
  vim.defer_fn(function()
    M.flush(scope)
  end, scope_options(scope).flush_delay_ms or 25)
end

---@param scope ForgePerfScope
---@param event string
---@param payload table?
---@param callback fun(): any
---@return any ...
function M.span(scope, event, payload, callback)
  if not M.enabled(scope) then return callback() end
  local started = M.now()
  local function pack_result(...)
    return { n = select("#", ...), ... }
  end
  local result = pack_result(pcall(callback))
  local ok = result[1]
  local next_payload = M.payload(payload or {})
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
