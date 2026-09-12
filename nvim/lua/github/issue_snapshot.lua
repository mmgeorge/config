local M = {}

local MAX_FILE_BYTES = 16 * 1024 * 1024
local MAX_CACHE_BYTES = 32 * 1024 * 1024
local MAX_REPOSITORIES = 16
local MAX_READS = 4
local MAX_QUEUED_READS = 16
local READ_BYTES = 64 * 1024

---@class GithubIssueSnapshotEntry
---@field path string
---@field issues table[]
---@field signature string?
---@field bytes integer
---@field touched integer
---@field revision? integer
---@field prepare? fun(done: fun(failure: string?, revision: integer?, ready: boolean?))
---@field watcher { stop: fun(self: table), close: fun(self: table) }?
---@class GithubIssueSnapshotLoad
---@field repo string
---@field path string
---@field entry GithubIssueSnapshotEntry
---@field callback fun(failure: string?)[]
---@field reload boolean
---@field attempt integer
---@field force boolean
---@field prepare? fun(done: fun(failure: string?, revision: integer?, ready: boolean?))

---@type table<string, GithubIssueSnapshotEntry>
local entry_by_repo = {}
---@type table<string, GithubIssueSnapshotLoad>
local request_by_repo = {}
local cache_bytes = 0
local active_reads = 0
local access = 0
---@type GithubIssueSnapshotLoad[]
local queued_read = {}
local pump

---@param failure string?
local function report(failure)
  if failure and failure ~= "Snapshot load invalidated" then
    local ok, notification_failure = pcall(vim.notify, "GitHub issue snapshot: " .. failure, vim.log.levels.ERROR)
    if not ok then pcall(vim.api.nvim_err_writeln, tostring(notification_failure)) end
  end
end

---@param entry GithubIssueSnapshotEntry
local function dispose(entry)
  cache_bytes = cache_bytes - entry.bytes
  if entry.watcher then
    entry.watcher:stop()
    entry.watcher:close()
    entry.watcher = nil
  end
end

---@param repo string
---@param additional integer
---@param adding boolean
---@return boolean
local function make_room(repo, additional, adding)
  while cache_bytes + additional > MAX_CACHE_BYTES or (adding and vim.tbl_count(entry_by_repo) >= MAX_REPOSITORIES) do
    local oldest_repo, oldest
    for candidate, entry in pairs(entry_by_repo) do
      if candidate ~= repo and not request_by_repo[candidate] and (not oldest or entry.touched < oldest.touched) then
        oldest_repo, oldest = candidate, entry
      end
    end
    if not oldest then return false end
    dispose(oldest)
    entry_by_repo[oldest_repo] = nil
  end
  return true
end

---@param repo string
---@param entry GithubIssueSnapshotEntry
local function watch(repo, entry)
  if entry.watcher then return end
  local watcher = vim.uv.new_fs_event()
  if not watcher then report("Cannot allocate a snapshot watcher for " .. entry.path) return end
  local started, start_failure = watcher:start(vim.fs.dirname(entry.path), {}, vim.schedule_wrap(function(failure, filename)
    if entry_by_repo[repo] ~= entry then return end
    if failure then
      watcher:stop()
      watcher:close()
      entry.watcher = nil
      report("watch failed for " .. entry.path .. ": " .. tostring(failure))
      return
    end
    if filename == nil or filename == vim.fs.basename(entry.path) then
      M.load(repo, entry.path, nil, true, entry.prepare)
    end
  end))
  if started then
    watcher:unref()
    entry.watcher = watcher
  else
    watcher:close()
    if not tostring(start_failure):find("ENOENT", 1, true) then
      report("Cannot watch " .. entry.path .. ": " .. tostring(start_failure))
    end
  end
end

---@param stat table
---@return string
local function signature(stat)
  local mtime = stat.mtime or {}
  return table.concat({ stat.size or 0, stat.ino or 0, mtime.sec or 0, mtime.nsec or 0 }, ":")
end

---@param value unknown
---@param repo string
---@return table[]?
---@return string?
local function validate(value, repo)
  if type(value) ~= "table" or value.repo ~= repo or value.state ~= "open" or type(value.issues) ~= "table" or not vim.islist(value.issues)
      or value.issue_count ~= #value.issues then
    return nil, "Invalid completion snapshot identity or record count"
  end
  for _, issue in ipairs(value.issues) do
    if type(issue) ~= "table" or issue.repo ~= repo or type(issue.number) ~= "number" or issue.number < 1
        or issue.number % 1 ~= 0 or type(issue.title) ~= "string" or type(issue.state) ~= "string"
        or type(issue.url) ~= "string" then
      return nil, "Invalid completion issue record"
    end
  end
  return value.issues, nil
end

---@param request GithubIssueSnapshotLoad
local function read(request)
  request.attempt = request.attempt + 1
  local descriptor
  local chunks = {}
  local size = 0
  local offset = 0
  local source_signature
  local expected_revision
  local loaded_revision

  local function finish(failure, issues)
    local function closed(close_failure)
      chunks = {}
      failure = failure or close_failure
      if request_by_repo[request.repo] ~= request or entry_by_repo[request.repo] ~= request.entry then
        failure = "Snapshot load invalidated"
      elseif request.reload and request.attempt < 3 then
        request.reload = false
        request.force = true
        read(request)
        return
      elseif request.reload then
        failure = "Completion snapshot kept changing during preload"
      end
      if not failure and issues then
        local entry = request.entry
        if make_room(request.repo, size - entry.bytes, false) then
          cache_bytes = cache_bytes + size - entry.bytes
          entry.issues, entry.bytes, entry.signature = issues, size, source_signature
          entry.revision = loaded_revision
        else
          failure = "Completion snapshot cache exceeds its 32 MiB byte budget"
        end
      end
      if request_by_repo[request.repo] == request then request_by_repo[request.repo] = nil end
      active_reads = active_reads - 1
      if entry_by_repo[request.repo] == request.entry then watch(request.repo, request.entry) end
      for _, callback in ipairs(request.callback) do
        local ok, callback_failure = pcall(callback, failure)
        if not ok then report("preload callback failed: " .. tostring(callback_failure)) end
      end
      pump()
    end
    if descriptor then
      local owned = descriptor
      descriptor = nil
      vim.uv.fs_close(owned, vim.schedule_wrap(closed))
    else
      closed(nil)
    end
  end

  local function next_chunk()
    if request_by_repo[request.repo] ~= request then finish("Snapshot load invalidated") return end
    if offset == size then
      vim.uv.fs_fstat(descriptor, vim.schedule_wrap(function(failure, stat)
        if failure then finish(tostring(failure)) return end
        if signature(stat) ~= source_signature then
          request.reload = true
          finish(nil)
          return
        end
        local encoded = table.concat(chunks)
        chunks = {}
        local ok, value = pcall(vim.json.decode, encoded)
        if not ok then finish("Invalid completion snapshot JSON: " .. tostring(value)) return end
        if expected_revision ~= nil and (type(value) ~= "table" or value.revision ~= expected_revision) then
          request.reload = true
          finish(nil)
          return
        end
        loaded_revision = type(value) == "table" and value.revision or nil
        local issues, validation_failure = validate(value, request.repo)
        finish(validation_failure, issues)
      end))
      return
    end
    vim.uv.fs_read(descriptor, math.min(READ_BYTES, size - offset), offset, vim.schedule_wrap(function(failure, bytes)
      if failure then finish(tostring(failure)) return end
      if not bytes or bytes == "" then finish("Completion snapshot ended before its recorded size") return end
      chunks[#chunks + 1] = bytes
      offset = offset + #bytes
      next_chunk()
    end))
  end

  local function open()
    vim.uv.fs_open(request.path, "r", 438, vim.schedule_wrap(function(failure, opened)
      if failure then
        if tostring(failure):find("ENOENT", 1, true) then
          if expected_revision ~= nil then request.reload = true finish(nil) else finish(nil, {}) end
        else finish(tostring(failure)) end
        return
      end
      descriptor = opened
      vim.uv.fs_fstat(descriptor, vim.schedule_wrap(function(stat_failure, stat)
        if stat_failure then finish(tostring(stat_failure)) return end
        size = stat.size
        source_signature = signature(stat)
        if size > MAX_FILE_BYTES then finish("Completion snapshot exceeds its 16 MiB file limit") return end
        if stat.type ~= "file" then finish("Completion snapshot is not a regular file") return end
        if not request.force and request.entry.signature == source_signature
          and (expected_revision == nil or request.entry.revision == expected_revision) then finish(nil) return end
        next_chunk()
      end))
    end))
  end
  if not request.prepare then open() return end
  local accepting = true
  local function prepared(failure, revision, ready)
    if not accepting then return end
    accepting = false
    if request_by_repo[request.repo] ~= request then finish("Snapshot load invalidated") return end
    if failure then finish(failure) return end
    if ready == false then finish(nil, {}) return end
    expected_revision = revision
    open()
  end
  local succeeded, failure = pcall(request.prepare, prepared)
  if not succeeded then prepared(tostring(failure)) end
end

pump = function()
  while active_reads < MAX_READS and #queued_read > 0 do
    local request = table.remove(queued_read, 1)
    if request_by_repo[request.repo] == request and entry_by_repo[request.repo] == request.entry then
      active_reads = active_reads + 1
      read(request)
    else
      for _, callback in ipairs(request.callback) do
        local ok, failure = pcall(callback, "Snapshot load invalidated")
        if not ok then report("preload callback failed: " .. tostring(failure)) end
      end
    end
  end
end

---@param repo string
---@param path string
---@param callback? fun(failure: string?)
---@param force? boolean
---@param prepare? fun(done: fun(failure: string?, revision: integer?, ready: boolean?))
function M.load(repo, path, callback, force, prepare)
  callback = callback or report
  ---@type GithubIssueSnapshotLoad?
  local pending = request_by_repo[repo]
  if pending and pending.path ~= path then M.invalidate(repo) pending = nil end
  if pending then
    if #pending.callback >= 64 then callback("Completion snapshot waiter capacity is full") return end
    pending.callback[#pending.callback + 1] = callback
    if prepare and not pending.prepare then
      pending.prepare, pending.entry.prepare, pending.reload = prepare, prepare, true
    end
    pending.reload = pending.reload or force == true or pending.path ~= path
    return
  end
  if #queued_read >= MAX_QUEUED_READS then callback("Completion snapshot read queue is full") return end
  ---@type GithubIssueSnapshotEntry?
  local entry = entry_by_repo[repo]
  if entry and entry.path ~= path then M.invalidate(repo) entry = nil end
  if not entry then
    if not make_room(repo, 0, true) then callback("Completion snapshot repository capacity is full") return end
    entry = { path = path, issues = {}, bytes = 0, touched = access }
    entry_by_repo[repo] = entry
  end
  access = access + 1
  entry.touched = access
  entry.prepare = prepare or entry.prepare
  watch(repo, entry)
  local request = { repo = repo, path = path, entry = entry, callback = { callback }, reload = false, attempt = 0, force = force == true, prepare = entry.prepare }
  request_by_repo[repo] = request
  queued_read[#queued_read + 1] = request
  pump()
end

---Borrows immutable completion records without filesystem access or JSON decoding.
---@param repo string
---@return table[]
function M.records(repo)
  local entry = entry_by_repo[repo]
  if not entry then return {} end
  access = access + 1
  entry.touched = access
  return entry.issues
end

---Invalidates cached records and prevents pending reads from republishing them.
---@param repo string
function M.invalidate(repo)
  request_by_repo[repo] = nil
  local entry = entry_by_repo[repo]
  entry_by_repo[repo] = nil
  if entry then dispose(entry) end
  pump()
end

---Closes cache watchers and invalidates readers, whose callbacks retain descriptor ownership.
function M.clear()
  for repo in pairs(entry_by_repo) do M.invalidate(repo) end
end

return M
