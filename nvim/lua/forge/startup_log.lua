local M = {}
local rotation_bytes = 2 * 1024 * 1024
local redraw_watch = {}
local redraw_namespace
local logging_us = 0
local input_namespace
local input_buffer, input_at
local host_startup = { ["host.start"] = true, ["host.binary.result"] = true, ["host.spawn"] = true,
  ["host.spawned"] = true, ["host.first_stdout"] = true, ["host.lease.finished"] = true, ["host.initialize.finished"] = true }

---@return nil
function M.observe_input()
  if input_namespace then return end
  input_namespace = vim.api.nvim_create_namespace("ForgeInputTiming")
  vim.on_key(function(_, typed)
    if typed == "" then return end
    input_at = vim.uv.hrtime()
    input_buffer = vim.api.nvim_get_current_buf()
  end, input_namespace)
end

---@param buffer integer
---@return integer?
function M.input_time(buffer)
  if input_buffer == buffer and input_at and vim.uv.hrtime() - input_at < 5e9 then return input_at end
end

---@return string
function M.path()
  return vim.fs.joinpath(vim.fn.stdpath("cache"), "rust-sidecar", "forge", "startup-" .. vim.fn.getpid() .. ".jsonl")
end

local writer = require("forge.infra.jsonl_log").new(function() return M.path() end, function(failure)
  vim.schedule(function() vim.notify("Forge startup log: " .. failure, vim.log.levels.ERROR) end)
end, rotation_bytes)

---@param event string
---@param fields? table<string, any> Diagnostic metadata, excluding request bodies and credentials.
---@param captured_at? integer Original monotonic event time when writing after a scheduled callback.
function M.write(event, fields, captured_at)
  captured_at = captured_at or vim.uv.hrtime()
  if vim.in_fast_event() then
    vim.schedule(function() M.write(event, fields, captured_at) end)
    return
  end
  local started = vim.uv.hrtime()
  if host_startup[event] then require("forge.harness_startup_log").host(event, fields) end
  writer.append({ time = os.date("!%Y-%m-%dT%H:%M:%SZ"), pid = vim.fn.getpid(), event = event, fields = fields,
    monotonic_us = math.floor(captured_at / 1000), logging_total_us = logging_us })
  logging_us = logging_us + math.floor((vim.uv.hrtime() - started) / 1000)
end

---@param event string
---@param fields? table<string, any>
---@return fun(failure?: string) finish
function M.span(event, fields)
  local started = vim.uv.hrtime()
  local complete = false
  M.write(event .. ".begin", fields)
  local function waiting()
    if complete then return end
    M.write(event .. ".waiting", { elapsed_ms = math.floor((vim.uv.hrtime() - started) / 1e6), context = fields })
    vim.defer_fn(waiting, 5000)
  end
  vim.defer_fn(waiting, 5000)
  return function(failure)
    if complete then return end
    complete = true
    M.write(event .. ".end", { elapsed_ms = math.floor((vim.uv.hrtime() - started) / 1e6), error = failure, context = fields })
  end
end

---@param buffer integer Populated buffer whose first displayed redraw is measured.
---@param document string Document identifier used to correlate startup events.
---@param started_at integer Command-entry monotonic time in nanoseconds.
---@param event? string Event name for a body redraw. Defaults to the first status redraw.
---@param context? table Additional identity fields carried into the redraw event.
function M.watch_redraw(buffer, document, started_at, event, context)
  if not redraw_namespace then
    redraw_namespace = vim.api.nvim_create_namespace("ForgeStartupRedraw")
    vim.api.nvim_set_decoration_provider(redraw_namespace, {
      on_start = function()
        local redraw_started = vim.uv.hrtime()
        for watched, trace in pairs(redraw_watch) do
          if not vim.api.nvim_buf_is_valid(trace.buffer) or vim.uv.hrtime() - trace.armed_at > 30e9 then
            redraw_watch[watched] = nil
          else
            trace.visible = false
            trace.redraw_started = redraw_started
          end
        end
        return next(redraw_watch) ~= nil
      end,
      on_win = function(_, _, drawn_buffer)
        for _, trace in pairs(redraw_watch) do
          if trace.buffer == drawn_buffer then trace.visible = true end
        end
        return false
      end,
      on_end = function()
        local finished_at = vim.uv.hrtime()
        for watched, trace in pairs(redraw_watch) do
          if trace.visible then
            redraw_watch[watched] = nil
            local fields = vim.tbl_extend("force", trace.context or {}, { document = trace.document, buffer = trace.buffer,
              elapsed_us = math.floor((finished_at - trace.started_at) / 1000),
              ready_to_redraw_start_us = math.floor((trace.redraw_started - trace.armed_at) / 1000),
              redraw_us = math.floor((finished_at - trace.redraw_started) / 1000),
              ready_to_redraw_us = math.floor((finished_at - trace.armed_at) / 1000) })
            vim.schedule(function() M.write(trace.event, fields, finished_at) end)
          end
        end
      end,
    })
  end
  local key = table.concat({ buffer, event or "status.first_redraw", context and context.operation or "", context and context.phase or "" }, ":")
  redraw_watch[key] = { buffer = buffer, document = document, started_at = started_at, armed_at = vim.uv.hrtime(),
    event = event or "status.first_redraw", context = context }
end

return M
