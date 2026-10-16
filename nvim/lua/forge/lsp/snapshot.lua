local source = require("forge.lsp.source")
local M = {}

---@class ForgeLspSnapshot
---@field path string
---@field lines string[]
---@field declarations table[]
---@field modified boolean
---@field buffer? integer
---@field version? integer
---@field stamp? string
---@field complete boolean
---@field warning? string

local function stamp(path)
  local stat = vim.uv.fs_stat(path)
  return stat and table.concat({ stat.size, stat.mtime.sec, stat.mtime.nsec, stat.ctime.sec, stat.ctime.nsec }, ":") or "missing"
end

---@param authorize function
---@param analyze function
---@return table
function M.new(authorize, analyze)
  local captured = {}
  local session = {}

  ---@return boolean, string?
  function session.current()
    for path, snapshot in pairs(captured) do
      if snapshot.buffer then
        if not vim.api.nvim_buf_is_valid(snapshot.buffer) or vim.api.nvim_buf_get_changedtick(snapshot.buffer) ~= snapshot.version then
          return false, "source_changed: " .. path .. " changed during the request."
        end
      elseif stamp(path) ~= snapshot.stamp then return false, "source_changed: " .. path .. " changed during the request." end
    end
    return true
  end

  ---@param path string
  ---@param receive fun(snapshot: ForgeLspSnapshot?, failure: string?)
  function session.get(path, receive)
    path = source.canonical(path)
    if captured[path] then receive(captured[path]) return end
    authorize({ path }, function(allowed, failure)
      if failure or not allowed[path] then receive(nil, failure or ("access_denied: " .. path)) return end
      local buffer = source.loaded(path)
      local snapshot = { path = path, buffer = buffer, modified = buffer and vim.bo[buffer].modified or false }
      local function accept(contents)
        snapshot.lines = source.rows(contents)
        captured[path] = snapshot
        analyze(path, contents, function(result, analysis_error)
          if analysis_error then
            snapshot.declarations, snapshot.complete = {}, false
            snapshot.warning = "Structural analysis unavailable for " .. path .. ": " .. analysis_error
            local current, stale = session.current()
            if not current then receive(nil, stale) else receive(snapshot) end
            return
          end
          snapshot.declarations = result.declarations or {}
          snapshot.complete = result.complete
          local current, stale = session.current()
          if not current then receive(nil, stale) else receive(snapshot) end
        end)
      end
      if buffer then
        snapshot.version = vim.api.nvim_buf_get_changedtick(buffer)
        accept(table.concat(vim.api.nvim_buf_get_lines(buffer, 0, -1, false), "\n") .. (vim.bo[buffer].endofline and "\n" or ""))
      else
        snapshot.stamp = stamp(path)
        source.read(path, nil, function(contents, read_error)
          if read_error then receive(nil, read_error) return end
          if stamp(path) ~= snapshot.stamp then receive(nil, "source_changed: " .. path .. " changed during the request.") return end
          accept(contents)
        end)
      end
    end)
  end
  return session
end

---@param snapshot ForgeLspSnapshot
---@param line integer
---@return table?
function M.enclosing(snapshot, line, column)
  local selected
  for _, declaration in ipairs(snapshot.declarations) do
    local starts_before = not column or line > declaration.range_start_line or line == declaration.range_start_line and column >= declaration.range_start_column or line < declaration.range_start_line
    local ends_after = not column or line < declaration.range_end_line or line == declaration.range_end_line and column < declaration.range_end_column
    if line >= declaration.start_line and line <= declaration.end_line and starts_before and ends_after
      and (not selected or declaration.depth > selected.depth) then selected = declaration end
  end
  return selected
end

return M
