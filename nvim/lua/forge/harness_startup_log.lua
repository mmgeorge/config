local M = {}
local sequence = 0
local watched = {}
local active = {}
local redraw_namespace

---@class ForgeHarnessStartupTrace
---@field id string
---@field finished boolean
---@field mark fun(phase: string, fields?: table)
---@field finish fun(status: string, fields?: table)
---@field watch fun(buffer: integer)

---@return string
function M.path()
  return vim.fs.joinpath(vim.fn.stdpath("cache"), "rust-sidecar", "forge", "harness-startup-" .. vim.fn.getpid() .. ".jsonl")
end

local function report(failure)
  vim.schedule(function() vim.notify("Harness startup log: " .. tostring(failure), vim.log.levels.ERROR) end)
end

local writer = require("forge.infra.jsonl_log").new(function() return M.path() end, report)

---@param phase string
---@param fields? table
function M.host(phase, fields)
  for _, trace in pairs(active) do trace.mark(phase, fields) end
end

---@param fields? table Metadata only, excluding prompts, source text, and credentials.
---@return ForgeHarnessStartupTrace
function M.begin(fields)
  sequence = sequence + 1
  local started = vim.uv.hrtime()
  local previous = started
  local trace = { id = vim.fn.getpid() .. ":" .. sequence, finished = false }
  active[trace.id] = trace
  function trace.mark(phase, metadata)
    if trace.finished then return end
    local now = vim.uv.hrtime()
    writer.append({ time = os.date("!%Y-%m-%dT%H:%M:%SZ"), pid = vim.fn.getpid(), startup_id = trace.id,
      phase = phase, elapsed_ms = (now - started) / 1e6, since_previous_ms = (now - previous) / 1e6, fields = metadata })
    previous = now
  end
  function trace.finish(status, metadata)
    if trace.finished then return end
    trace.mark(status, metadata)
    trace.finished = true
    active[trace.id] = nil
    for buffer, entry in pairs(watched) do if entry.trace == trace then watched[buffer] = nil end end
  end
  function trace.watch(buffer)
    if trace.finished then return end
    if not redraw_namespace then
      redraw_namespace = vim.api.nvim_create_namespace("ForgeHarnessStartupRedraw")
      vim.api.nvim_set_decoration_provider(redraw_namespace, {
        on_start = function()
          for _, entry in pairs(watched) do entry.visible = false end
          return next(watched) ~= nil
        end,
        on_win = function(_, _, drawn_buffer)
          if watched[drawn_buffer] then watched[drawn_buffer].visible = true end
          return false
        end,
        on_end = function()
          for _, entry in pairs(watched) do
            if entry.visible then entry.trace.finish("first_redraw") end
          end
        end,
      })
    end
    if watched[buffer] then watched[buffer].trace.finish("superseded") end
    watched[buffer] = { trace = trace, visible = false }
    vim.api.nvim_create_autocmd("BufWipeout", { buffer = buffer, once = true,
      callback = function() trace.finish("closed_before_redraw") end })
  end
  trace.mark("open", fields)
  return trace
end

return M
