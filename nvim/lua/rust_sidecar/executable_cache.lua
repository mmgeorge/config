local M = {}
local pending = {}

---@param path string
---@param stat {dev: integer, ino: integer, size: integer, mtime: {sec: integer, nsec: integer}, ctime: {sec: integer, nsec: integer}}
---@return string
local function identity(path, stat)
  return vim.fn.sha256(vim.json.encode({ path, stat.dev, stat.ino, stat.size,
    stat.mtime.sec, stat.mtime.nsec, stat.ctime.sec, stat.ctime.nsec }))
end

---@param root string
---@param source string
---@param executable string
local function prune(root, source, executable)
  local source_stat = vim.uv.fs_stat(source)
  if not source_stat then return end
  local current = identity(source, source_stat)
  local directory = vim.uv.fs_scandir(root)
  if not directory then return end
  local function next_entry()
    local name, kind = vim.uv.fs_scandir_next(directory)
    if not name then return end
    if kind == "directory" and name ~= current then
      local path = vim.fs.joinpath(root, name)
      -- Windows refuses deletion while another editor holds its read lease or runs the image.
      local removed = vim.uv.fs_unlink(vim.fs.joinpath(path, executable))
      if removed then vim.uv.fs_rmdir(path) end
    end
    vim.schedule(next_entry)
  end
  next_entry()
end

---@param source string
---@param root string
---@param executable string
---@param callback fun(lease?: RustSidecarLease, failure?: string)
function M.acquire(source, root, executable, callback)
  local started = vim.uv.hrtime()
  local stat, stat_error = vim.uv.fs_stat(source)
  if not stat or stat.type ~= "file" then
    callback(nil, "Failed to copy Rust sidecar executable: " .. tostring(stat_error or "source is not a file"))
    return
  end
  source = vim.uv.fs_realpath(source) or source
  local key = identity(source, stat)
  local cache_root = vim.fs.joinpath(root, "executables", vim.fn.sha256(source))
  local directory = vim.fs.joinpath(cache_root, key)
  local path = vim.fs.joinpath(directory, executable)
  local lookup_ms = (vim.uv.hrtime() - started) / 1e6
  local function deliver(hit, copy_ms)
    local pin_started = vim.uv.hrtime()
    local release, failure, code = require("rust_sidecar.windows_image").pin(path)
    local pin_ms = (vim.uv.hrtime() - pin_started) / 1e6
    if not release then
      if code == 2 or code == 3 then vim.schedule(function() M.acquire(source, root, executable, callback) end)
      else callback(nil, failure) end
      return
    end
    callback({ path = path, release = release, cache_hit = hit, cache_key = key,
      source_path = source, file_bytes = stat.size, copy_ms = copy_ms, pin_ms = pin_ms, lookup_ms = lookup_ms,
      acquire_ms = (vim.uv.hrtime() - started) / 1e6 })
    vim.defer_fn(function() prune(cache_root, source, executable) end, 1000)
  end
  local cached = vim.uv.fs_stat(path)
  if cached and cached.type == "file" and cached.size == stat.size then deliver(true, 0) return end
  if pending[path] then pending[path][#pending[path] + 1] = { callback = callback, started = started } return end
  pending[path] = { { callback = callback, started = started } }
  local function finish(failure, copy_ms)
    local callbacks = pending[path]
    pending[path] = nil
    for _, waiter in ipairs(callbacks) do
      if failure then waiter.callback(nil, failure)
      else
        local pin_started = vim.uv.hrtime()
        local release, pin_error, code = require("rust_sidecar.windows_image").pin(path)
        local pin_ms = (vim.uv.hrtime() - pin_started) / 1e6
        if not release and (code == 2 or code == 3) then
          vim.schedule(function() M.acquire(source, root, executable, waiter.callback) end)
        elseif not release then waiter.callback(nil, pin_error)
        else waiter.callback({ path = path, release = release, cache_hit = false, cache_key = key,
          source_path = source, file_bytes = stat.size, copy_ms = copy_ms, pin_ms = pin_ms, lookup_ms = lookup_ms,
          acquire_ms = (vim.uv.hrtime() - waiter.started) / 1e6 }) end
      end
    end
    vim.defer_fn(function() prune(cache_root, source, executable) end, 1000)
  end
  local created, create_error = pcall(vim.fn.mkdir, directory, "p")
  if not created then finish(tostring(create_error), 0) return end
  local temporary = vim.fs.joinpath(directory, ("%s-%s.tmp"):format(vim.fn.getpid(), vim.uv.hrtime()))
  local copy_started = vim.uv.hrtime()
  vim.uv.fs_copyfile(source, temporary, 0, vim.schedule_wrap(function(copy_error)
    local copy_ms = (vim.uv.hrtime() - copy_started) / 1e6
    if copy_error then vim.uv.fs_unlink(temporary) finish("Failed to copy Rust sidecar executable: " .. tostring(copy_error), copy_ms) return end
    local current = vim.uv.fs_stat(source)
    if not current or identity(source, current) ~= key then
      vim.uv.fs_unlink(temporary)
      local callbacks = pending[path]
      pending[path] = nil
      for _, waiter in ipairs(callbacks) do M.acquire(source, root, executable, waiter.callback) end
      return
    end
    vim.uv.fs_rename(temporary, path, vim.schedule_wrap(function(rename_error)
      if rename_error then
        local published = vim.uv.fs_stat(path)
        vim.uv.fs_unlink(temporary)
        if not published or published.size ~= stat.size then
          finish("Failed to publish Rust sidecar executable: " .. tostring(rename_error), copy_ms)
          return
        end
      end
      finish(nil, copy_ms)
    end))
  end))
end

return M
