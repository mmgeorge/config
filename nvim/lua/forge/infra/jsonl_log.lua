local M = {}

---@class ForgeJsonlLog
---@field append fun(record: table)
---@field idle fun(): boolean

---@param directory string
---@param complete fun(failure?: string)
local function ensure_directory(directory, complete)
  vim.uv.fs_mkdir(directory, 448, function(failure)
    if not failure or failure:match("^EEXIST:") then complete() return end
    local parent = vim.fs.dirname(directory)
    if not failure:match("^ENOENT:") or not parent or parent == directory then complete(failure) return end
    ensure_directory(parent, function(parent_failure)
      if parent_failure then complete(parent_failure) return end
      vim.uv.fs_mkdir(directory, 448, function(create_failure)
        if create_failure and create_failure:match("^EEXIST:") then complete()
        else complete(create_failure) end
      end)
    end)
  end)
end

--- Creates one ordered asynchronous writer for a process-owned JSONL path.
--- Rotation applies between batches and never splits or truncates an individual record.
---@param path fun(): string
---@param report fun(failure: string)
---@param rotation_bytes? integer
---@return ForgeJsonlLog
function M.new(path, report, rotation_bytes)
  local pending = {}
  local writing, scheduled, resume = false, false, false
  local directory_ready
  local flush
  local function schedule()
    if writing or scheduled or #pending == 0 then return end
    scheduled = true
    vim.schedule(function() scheduled = false flush() end)
  end
  flush = function()
    if writing or #pending == 0 then return end
    writing = true
    local target = path()
    local contents = table.concat(pending)
    pending = {}
    local offset = 0
    local function fail(failure)
      if offset < #contents then table.insert(pending, 1, contents:sub(offset + 1)) end
      resume = resume or offset > 0
      writing = false
      report(tostring(failure))
    end
    local function open()
      vim.uv.fs_open(target, "a", 384, function(failure, descriptor)
        if failure or not descriptor then fail(failure or "Log file did not open") return end
        local function write_next()
          vim.uv.fs_write(descriptor, contents:sub(offset + 1), -1, function(write_failure, count)
            if write_failure or not count or count == 0 then
              vim.uv.fs_close(descriptor, function() fail(write_failure or "Log write made no progress") end)
              return
            end
            offset = offset + count
            if offset < #contents then write_next() return end
            vim.uv.fs_close(descriptor, function(close_failure)
              writing, resume = false, false
              if close_failure then report(tostring(close_failure)) end
              schedule()
            end)
          end)
        end
        write_next()
      end)
    end
    local function rotate()
      if not rotation_bytes or resume then open() return end
      vim.uv.fs_stat(target, function(failure, stat)
        if failure and not failure:match("^ENOENT:") then fail(failure) return end
        if not stat or stat.size < rotation_bytes then open() return end
        vim.uv.fs_rename(target, target .. ".1", function(rename_failure)
          if rename_failure then fail(rename_failure) return end
          open()
        end)
      end)
    end
    local directory = vim.fs.dirname(target)
    if directory_ready == directory then rotate() return end
    ensure_directory(directory, function(failure)
      if failure then fail(failure) return end
      directory_ready = directory
      rotate()
    end)
  end
  return {
    append = function(record)
      local success, encoded = pcall(vim.json.encode, record)
      if not success then report(tostring(encoded)) return end
      pending[#pending + 1] = encoded .. "\n"
      schedule()
    end,
    idle = function() return not writing and not scheduled and #pending == 0 end,
  }
end

return M
