local M = {}
local sequence = 0
local pending, writing, directory_ready = {}, false, false
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

local function flush()
  if writing or #pending == 0 then return end
  writing = true
  local path = M.path()
  if not directory_ready then
    local success, failure = pcall(vim.fn.mkdir, vim.fs.dirname(path), "p")
    if not success then writing = false report(failure) return end
    directory_ready = true
  end
  local contents = table.concat(pending)
  pending = {}
  vim.uv.fs_open(path, "a", 384, function(failure, descriptor)
    if failure then writing = false report(failure) return end
    local offset = 0
    local function write_next()
      vim.uv.fs_write(descriptor, contents:sub(offset + 1), -1, function(write_error, count)
        if write_error or not count or count == 0 then
          vim.uv.fs_close(descriptor, function() end)
          writing = false
          report(write_error or "No progress writing startup record")
          vim.schedule(flush)
          return
        end
        offset = offset + count
        if offset < #contents then write_next() return end
        vim.uv.fs_close(descriptor, function(close_error)
          writing = false
          if close_error then report(close_error) end
          vim.schedule(flush)
        end)
      end)
    end
    write_next()
  end)
end

local function record(value)
  local success, encoded = pcall(vim.json.encode, value)
  if not success then report(encoded) return end
  pending[#pending + 1] = encoded .. "\n"
  vim.schedule(flush)
end

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
    record({ time = os.date("!%Y-%m-%dT%H:%M:%SZ"), pid = vim.fn.getpid(), startup_id = trace.id,
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
