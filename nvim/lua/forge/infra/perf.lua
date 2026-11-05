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
---@field rejected_record_count table<ForgePerfScope, integer>
---@field _test_file_limit_bytes? integer Fixture-only physical file budget.
local M = {}
local MAX_LOG_BYTES = 100 * 1024 * 1024
local ROTATED_LOG_FILES = 10
local MAX_QUEUE_RECORDS = 4096
local METADATA_FIELD = {
  buf = true, view_kind = true, line_count = true, cursor_row = true, cursor_col = true,
  viewport_top = true, viewport_total = true, viewport_logical_total = true, viewport_render_count = true,
  ms = true, elapsed_ms = true, duration_ms = true, count = true, rows = true, bytes = true, spans = true,
  source_bytes = true, result_bytes = true, file_count = true, hunk_count = true, changed_lines = true,
  added = true, deleted = true, phase = true, source = true, operation = true, operation_id = true,
  request_id = true, session_id = true, provider = true, method = true, event_type = true,
  command_id = true, binding_id = true, invocation_id = true, attempt_id = true,
  consumed_sequence = true, event_sequence = true, outcome = true,
  status = true, code = true, enabled = true, cached = true, cancelled = true, generation = true,
  revision = true, queue_bytes = true, queue_count = true, active_jobs = true, retained_bytes = true,
  render_count = true,
  span_id = true,
  document = true, text_edits = true, metadata_edits = true,
  preflight_ms = true, view_capture_ms = true, fold_capture_ms = true,
  sequence_and_folds_ms = true, text_ms = true, metadata_ms = true,
  regions_ms = true, decorations_ms = true, fold_refresh_ms = true, view_restore_ms = true,
  buffer_api_ms = true, editable_callback_ms = true, editable_callback_count = true,
  slices = true, maximum_ms = true, maximum_prepare_ms = true, maximum_commit_ms = true,
  base_revision = true, received_revision = true, entry_count = true, operation_count = true,
  host_id = true, native_sequence = true, phase_ms = true, command_count = true,
  blocker_id = true, blocker_operation = true, blocker_phase = true, dropped = true,
}
local writing = { diff = false, harness = false }
M.rejected_record_count = { diff = 0, harness = 0 }
local saturation_reported = { diff = false, harness = false }
local queue_bytes = { diff = 0, harness = 0 }
local loop_timer
local update_loop_watch
local chunk_sequence = 0

M.options = {
  diff = { enabled = false, log_path = nil, slow_threshold_ms = 8, sample_rate = 1, flush_delay_ms = 25 },
  harness = { enabled = false, log_path = nil, slow_threshold_ms = 8, sample_rate = 1, flush_delay_ms = 25 },
}
M.sequence = { diff = 0, harness = 0 }
M.queue = { diff = nil, harness = nil }
M.flush_pending = { diff = false, harness = false }

---@param options? {diff?: {enabled?: boolean, log_path?: string, slow_threshold_ms?: number, sample_rate?: number, flush_delay_ms?: integer}, harness?: {enabled?: boolean, log_path?: string, slow_threshold_ms?: number, sample_rate?: number, flush_delay_ms?: integer}}
function M.setup(options)
  M.options = vim.tbl_deep_extend("force", vim.deepcopy(M.options), options or {})
  update_loop_watch()
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

--- Iterate complete JSONL records, splitting only records larger than the file budget.
---@param lines string[]
---@param file_limit integer
---@return fun(): string?
local function physical_records(lines, file_limit)
  local index, chunk = 0, nil
  return function()
    if not chunk then
      index = index + 1
      local encoded = lines[index]
      if not encoded then return nil end
      if #encoded + 1 <= file_limit then return encoded end
      chunk_sequence = chunk_sequence + 1
      local id = tostring(vim.fn.getpid()) .. ":" .. tostring(M.now()) .. ":" .. tostring(chunk_sequence)
      local envelope = { trace_chunk = { id = id, sequence = 9007199254740991, part_count = 9007199254740991, payload_hex = "" } }
      local overhead = #vim.json.encode(envelope) + 1
      local part_bytes = math.min(256 * 1024, math.floor((file_limit - overhead) / 2))
      assert(part_bytes > 0, "trace file budget cannot hold a chunk envelope")
      chunk = { encoded = encoded, offset = 1, part_bytes = part_bytes, envelope = envelope }
      envelope.trace_chunk.sequence = 0
      envelope.trace_chunk.part_count = math.ceil(#encoded / part_bytes)
    end
    local part = chunk.envelope.trace_chunk
    local payload = chunk.encoded:sub(chunk.offset, chunk.offset + chunk.part_bytes - 1)
    part.payload_hex = payload:gsub(".", function(byte) return string.format("%02x", string.byte(byte)) end)
    local encoded = vim.json.encode(chunk.envelope)
    part.sequence = part.sequence + 1
    chunk.offset = chunk.offset + #payload
    if chunk.offset > #chunk.encoded then chunk = nil end
    return encoded
  end
end

---@param scope ForgePerfScope
function M.flush(scope)
  if writing[scope] then return end
  local lines = M.queue[scope]
  M.queue[scope] = nil
  queue_bytes[scope] = 0
  M.flush_pending[scope] = false
  saturation_reported[scope] = false
  if not (lines and #lines > 0) then return end
  writing[scope] = true
  local file_limit = M._test_file_limit_bytes or MAX_LOG_BYTES
  local next_record = physical_records(lines, file_limit)
  local pending = next_record()
  local path = M.log_path(scope)
  local default_path = not scope_options(scope).log_path or scope_options(scope).log_path == ""
  local uv = vim.uv or vim.loop
  local function finished(err)
    writing[scope] = false
    notify_write_error(scope, err)
    if M.queue[scope] then vim.schedule(function() M.flush(scope) end) end
  end
  if uv and uv.fs_open and uv.fs_write and uv.fs_close then
    local append_next
    local function rotate(generation)
      if generation == 0 then append_next() return end
      local source = generation == 1 and path or path .. "." .. (generation - 1)
      uv.fs_rename(source, path .. "." .. generation, function(err)
        if err and not tostring(err):find("ENOENT", 1, true) then finished(err) return end
        rotate(generation - 1)
      end)
    end
    append_next = function()
      if not pending then finished(nil) return end
      uv.fs_stat(path, function(stat_err, stat)
        if stat_err and not tostring(stat_err):find("ENOENT", 1, true) then finished(stat_err) return end
        local size = stat and stat.size or 0
        if size + #pending + 1 > file_limit then
          uv.fs_unlink(path .. "." .. ROTATED_LOG_FILES, function(err)
            if err and not tostring(err):find("ENOENT", 1, true) then finished(err) return end
            rotate(ROTATED_LOG_FILES)
          end)
          return
        end
        local batch, bytes = {}, 0
        while pending and size + bytes + #pending + 1 <= file_limit do
          batch[#batch + 1] = pending
          bytes = bytes + #pending + 1
          pending = next_record()
        end
        local text = table.concat(batch, "\n") .. "\n"
        uv.fs_open(path, "a", 438, function(open_err, fd)
          if open_err or not fd then finished(open_err or "open failed") return end
          local function append(offset)
            uv.fs_write(fd, text:sub(offset), -1, function(write_err, written)
              if not write_err and written and written > 0 and offset + written <= #text then append(offset + written) return end
              uv.fs_close(fd, function(close_err)
                if write_err or close_err or not written or written == 0 then finished(write_err or close_err or "empty log write") return end
                append_next()
              end)
            end)
          end
          append(1)
        end)
      end)
    end
    if default_path then
      uv.fs_mkdir(vim.fs.dirname(path), 448, function(err)
        if err and not tostring(err):find("EEXIST", 1, true) then finished(err) return end
        append_next()
      end)
    else
      append_next()
    end
    return
  end
  local ok, err = pcall(function()
    if default_path then vim.fn.mkdir(vim.fs.dirname(path), "p") end
    while pending do
      local stat = uv.fs_stat(path)
      if stat and stat.size + #pending + 1 > file_limit then
        for generation = ROTATED_LOG_FILES, 1, -1 do
          local source = generation == 1 and path or path .. "." .. (generation - 1)
          local destination = path .. "." .. generation
          if generation == ROTATED_LOG_FILES then uv.fs_unlink(destination) end
          if uv.fs_stat(source) then assert(uv.fs_rename(source, destination)) end
        end
      end
      vim.fn.writefile({ pending }, path, "a")
      pending = next_record()
    end
  end)
  finished(not ok and err or nil)
end

---@param payload table
---@return table
function M.payload(payload)
  if type(payload) ~= "table" then return {} end
  local record = {}
  for key, value in pairs(payload) do
    if METADATA_FIELD[key] then
      local kind = type(value)
      if kind == "string" then record[key] = key:match("_id$") and value or value:sub(1, 256)
      elseif kind == "boolean" or kind == "number" then record[key] = value end
    end
  end
  return record
end

---@param scope ForgePerfScope
---@param event string
---@param payload table?
---@return "queued"|"saturated"|"disabled"
function M.event(scope, event, payload)
  if not M.enabled(scope) then return "disabled" end
  M.sequence[scope] = M.sequence[scope] + 1
  local record = M.payload(payload or {})
  record.seq = M.sequence[scope]
  record.scope = scope
  record.event = event:sub(1, 256)
  record.time = os.date("%Y-%m-%d %H:%M:%S")
  record.kind = "forge.infra.perf"
  record.pid = vim.fn.getpid()
  record.monotonic_us = math.floor(M.now() / 1000)
  local encoded = vim.json.encode(record)
  if #(M.queue[scope] or {}) >= MAX_QUEUE_RECORDS then
    M.rejected_record_count[scope] = M.rejected_record_count[scope] + 1
    if not saturation_reported[scope] then
      saturation_reported[scope] = true
      notify_write_error(scope, "4096-record diagnostic queue saturated; inspect rejected_record_count")
    end
    return "saturated"
  end
  M.queue[scope] = M.queue[scope] or {}
  M.queue[scope][#M.queue[scope] + 1] = encoded
  queue_bytes[scope] = queue_bytes[scope] + #encoded + 1
  if M.flush_pending[scope] then return "queued" end
  M.flush_pending[scope] = true
  vim.defer_fn(function()
    M.flush(scope)
  end, scope_options(scope).flush_delay_ms or 25)
  return "queued"
end

update_loop_watch = function()
  if loop_timer then loop_timer:stop() loop_timer:close() loop_timer = nil end
  if not M.enabled("harness") then return end
  local timer = assert(vim.uv.new_timer())
  loop_timer = timer
  local previous = M.now()
  timer:start(250, 250, vim.schedule_wrap(function()
    if loop_timer ~= timer then return end
    local now = M.now()
    local delay = (now - previous) / 1e6 - 250
    previous = now
    if delay >= 100 then M.event("harness", "ui.loop_lag", { elapsed_ms = delay }) end
  end))
end

--- Measures a synchronous UI boundary without recording callback arguments or results.
---@param scope ForgePerfScope
---@param event string
---@param payload table?
---@param callback fun(): any
---@return any ...
function M.trace(scope, event, payload, callback)
  if not M.enabled(scope) then return callback() end
  local started = M.now()
  local fields = M.payload(payload or {})
  fields.span_id = tostring(M.sequence[scope] + 1)
  fields.phase = "begin"
  M.event(scope, event, fields)
  local result = { n = 0 }
  local function capture(...)
    result = { n = select("#", ...), ... }
  end
  capture(pcall(callback))
  fields.phase = "end"
  fields.status = result[1] and "ok" or "error"
  fields.elapsed_ms = M.elapsed_ms(started)
  M.event(scope, event, fields)
  if not result[1] then error(result[2], 0) end
  return (table.unpack or unpack)(result, 2, result.n)
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
