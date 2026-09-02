--- Stores persistent per-worktree paths that DiffReview projects into its virtual Ignored section.
--- Keeps pending stage suppressions separate from durable markers so failed Git work restores them.

local notifications = require("diff_review.infra.notifications")
local paths = require("diff_review.infra.paths")
local section_map = require("diff_review.views.status.section_map")

local M = {}

local current_version = 1

---@class DiffReviewIgnoredPathPayload
---@field version integer
---@field root string
---@field ignored_paths string[]

---@class DiffReviewIgnoredPathIo
---@field read_async fun(path: string, callback: fun(content?: string, error?: string))
---@field write_atomic_async fun(path: string, content: string, callback: fun(error?: string))

---@class DiffReviewIgnoredPathState
---@field root string
---@field key string
---@field loaded boolean
---@field loading boolean
---@field waiter_list fun(ok: boolean)[]
---@field ignored_path_set table<string, boolean>
---@field suppression_by_token table<string, table<string, boolean>>
---@field write_generation integer
---@field write_running boolean
---@field write_pending boolean

---@type table<string, DiffReviewIgnoredPathState>
local state_by_key = {}
local data_dir_for_test = nil ---@type string?
local io_for_test = nil ---@type DiffReviewIgnoredPathIo?

--- Normalizes a repository root path into a canonical cache key.
---@param root string Git repository root path.
---@return string key Canonical root key string.
local function root_key(root)
  local normalized = paths.normalize_path(root):gsub("\\", "/")
  return vim.fn.has("win32") == 1 and normalized:lower() or normalized
end

--- Normalizes and validates a relative repository file path.
---@param path string Relative file path string.
---@return string? normalized Clean relative path, or nil if invalid.
local function normalize_relative_path(path)
  if type(path) ~= "string" or path == "" or path:find("\0", 1, true) then return nil end
  local normalized = path:gsub("\\", "/"):gsub("^%./", "")
  if normalized == "" or normalized == "." or normalized == ".." or normalized:sub(1, 3) == "../" then return nil end
  if normalized:match("^%a:/") or normalized:sub(1, 1) == "/" then return nil end
  return normalized
end

--- Sorts and removes duplicates from a list of relative repository paths.
---@param path_list string[] Array of relative path strings.
---@return string[] normalized_list Sorted and de-duplicated path array.
local function normalize_path_list(path_list)
  local normalized_list = {}
  local seen_path = {}
  for _, path in ipairs(path_list or {}) do
    local normalized = normalize_relative_path(path)
    if normalized and not seen_path[normalized] then
      seen_path[normalized] = true
      normalized_list[#normalized_list + 1] = normalized
    end
  end
  table.sort(normalized_list)
  return normalized_list
end

--- Obtains or initializes the in-memory ignored path state record for a repository root.
---@param root string Git repository root path.
---@return DiffReviewIgnoredPathState state Store state record.
local function state_for_root(root)
  local key = root_key(root)
  local state = state_by_key[key]
  if state then return state end
  state = {
    root = root,
    key = key,
    loaded = false,
    loading = false,
    waiter_list = {},
    ignored_path_set = {},
    suppression_by_token = {},
    write_generation = 0,
    write_running = false,
    write_pending = false,
  }
  state_by_key[key] = state
  return state
end

--- Resolves the JSON persistence file path for a repository root.
---@param root string Git repository root path.
---@return string path Absolute JSON file path.
local function store_path(root)
  local directory = data_dir_for_test
    or vim.fs.joinpath(vim.fn.stdpath("data"), "diff-review", "status-ignored")
  return vim.fs.joinpath(directory, vim.fn.sha256(root_key(root)) .. ".json")
end

--- Dispatches asynchronous read callback to the Neovim main event loop.
---@param callback fun(content?: string, error?: string) Read completion callback.
---@param content? string File content string.
---@param error? string Error message string.
local function schedule_read_result(callback, content, error)
  vim.schedule(function() callback(content, error) end)
end

--- Asynchronously reads file contents via libuv filesystem APIs.
---@param path string Target file path.
---@param callback fun(content?: string, error?: string) Completion callback.
local function read_async(path, callback)
  vim.uv.fs_open(path, "r", 438, function(open_error, descriptor)
    if open_error then
      local message = tostring(open_error)
      if message:find("ENOENT", 1, true) then
        schedule_read_result(callback, nil, nil)
      else
        schedule_read_result(callback, nil, message)
      end
      return
    end
    vim.uv.fs_fstat(descriptor, function(stat_error, stat)
      if stat_error or not stat then
        vim.uv.fs_close(descriptor, function()
          schedule_read_result(callback, nil, tostring(stat_error or "Unable to stat ignored-path store"))
        end)
        return
      end
      vim.uv.fs_read(descriptor, stat.size, 0, function(read_error, content)
        vim.uv.fs_close(descriptor, function()
          if read_error then
            schedule_read_result(callback, nil, tostring(read_error))
          else
            schedule_read_result(callback, content or "", nil)
          end
        end)
      end)
    end)
  end)
end

--- Dispatches asynchronous write callback to the Neovim main event loop.
---@param callback fun(error?: string) Write completion callback.
---@param error? string Error message string.
local function schedule_write_result(callback, error)
  vim.schedule(function() callback(error) end)
end

--- Writes file contents atomically via temporary file creation, fsync, and atomic rename.
---@param path string Target destination path.
---@param content string File content string.
---@param callback fun(error?: string) Completion callback.
local function write_atomic_async(path, content, callback)
  local directory = vim.fs.dirname(path)
  local directory_ok, directory_error = pcall(vim.fn.mkdir, directory, "p")
  if not directory_ok then
    schedule_write_result(callback, tostring(directory_error))
    return
  end
  local temporary_path = path .. (".tmp-%s"):format(tostring(vim.uv.hrtime()))
  vim.uv.fs_open(temporary_path, "w", 420, function(open_error, descriptor)
    if open_error then
      schedule_write_result(callback, tostring(open_error))
      return
    end
    local function finish_write(write_error)
      vim.uv.fs_close(descriptor, function(close_error)
        if write_error or close_error then
          vim.uv.fs_unlink(temporary_path, function()
            schedule_write_result(callback, tostring(write_error or close_error))
          end)
          return
        end
        vim.uv.fs_rename(temporary_path, path, function(rename_error)
          if rename_error then
            vim.uv.fs_unlink(temporary_path, function()
              schedule_write_result(callback, tostring(rename_error))
            end)
          else
            schedule_write_result(callback, nil)
          end
        end)
      end)
    end
    local offset = 0
    local function write_remaining()
      if offset >= #content then
        vim.uv.fs_fsync(descriptor, finish_write)
        return
      end
      vim.uv.fs_write(descriptor, content:sub(offset + 1), offset, function(write_error, written_byte_count)
        if write_error or not written_byte_count or written_byte_count <= 0 then
          finish_write(write_error or "Ignored-path write made no progress")
          return
        end
        offset = offset + written_byte_count
        write_remaining()
      end)
    end
    write_remaining()
  end)
end

--- Returns the active or test-injected I/O backend adapter.
---@return DiffReviewIgnoredPathIo io Active I/O backend table.
local function io_backend()
  return io_for_test or {
    read_async = read_async,
    write_atomic_async = write_atomic_async,
  }
end

--- Serializes in-memory ignored path state to a JSON string payload.
---@param state DiffReviewIgnoredPathState In-memory state record.
---@return string json Serialized JSON string.
local function encode_state(state)
  local ignored_path_list = {}
  for path in pairs(state.ignored_path_set) do ignored_path_list[#ignored_path_list + 1] = path end
  table.sort(ignored_path_list)
  return vim.json.encode({
    version = current_version,
    root = state.key,
    ignored_paths = ignored_path_list,
  } --[[@as DiffReviewIgnoredPathPayload]])
end

--- Debounces and triggers atomic asynchronous disk persistence for dirty state.
---@param state DiffReviewIgnoredPathState Target store state record.
local function request_write(state)
  state.write_generation = state.write_generation + 1
  state.write_pending = true
  if state.write_running then return end

  local function start_write()
    if state.write_running or not state.write_pending then return end
    state.write_running = true
    state.write_pending = false
    local generation = state.write_generation
    local content = encode_state(state)
    io_backend().write_atomic_async(store_path(state.root), content, function(write_error)
      state.write_running = false
      if write_error then
        notifications.error("Unable to persist DiffReview ignored paths: " .. tostring(write_error))
        if state.write_generation ~= generation then
          state.write_pending = true
          start_write()
        end
        return
      end
      if state.write_generation ~= generation then
        state.write_pending = true
        start_write()
      end
    end)
  end

  start_write()
end

--- Parses and validates JSON payload into in-memory ignored path state.
---@param state DiffReviewIgnoredPathState Target store state record.
---@param content? string Raw JSON string content.
---@return boolean ok True if payload was successfully parsed.
---@return string? error Error message string if validation failed.
local function decode_state(state, content)
  if content == nil or content == "" then return true, nil end
  local decode_ok, payload = pcall(vim.json.decode, content)
  if not decode_ok or type(payload) ~= "table" then return false, "invalid JSON" end
  if payload.version ~= current_version then return false, "unsupported version" end
  if payload.root ~= state.key then return false, "worktree root does not match" end
  if type(payload.ignored_paths) ~= "table" or not vim.islist(payload.ignored_paths) then
    return false, "ignored_paths must be a list"
  end
  local normalized_list = normalize_path_list(payload.ignored_paths)
  if #normalized_list ~= #payload.ignored_paths then return false, "ignored_paths contains an invalid or duplicate path" end
  state.ignored_path_set = {}
  for _, path in ipairs(normalized_list) do state.ignored_path_set[path] = true end
  return true, nil
end

--- Resolves all queued asynchronous load waiters with the completion status.
---@param state DiffReviewIgnoredPathState Target store state record.
---@param ok boolean True if load succeeded.
local function finish_load(state, ok)
  state.loading = false
  state.loaded = true
  local waiter_list = state.waiter_list
  state.waiter_list = {}
  for _, waiter in ipairs(waiter_list) do waiter(ok) end
end

--- Loads a worktree's persistent ignored paths asynchronously without blocking the initial status render.
---@param root string Git repository root path.
---@param callback fun(ok: boolean) Callback invoked when load finishes.
function M.load_async(root, callback)
  local state = state_for_root(root)
  if state.loaded then
    vim.schedule(function() callback(true) end)
    return
  end
  state.waiter_list[#state.waiter_list + 1] = callback
  if state.loading then return end
  state.loading = true
  io_backend().read_async(store_path(root), function(content, read_error)
    if read_error then
      notifications.error("Unable to load DiffReview ignored paths: " .. tostring(read_error))
      finish_load(state, false)
      return
    end
    local valid, validation_error = decode_state(state, content)
    if not valid then
      notifications.error("Unable to load DiffReview ignored paths: " .. tostring(validation_error))
    end
    finish_load(state, valid)
  end)
end

--- Returns the set of effective ignored paths after filtering out active stage suppressions.
---@param state DiffReviewIgnoredPathState Target store state record.
---@return table<string, boolean> set Effective ignored paths dictionary.
local function effective_path_set(state)
  local effective = vim.deepcopy(state.ignored_path_set)
  for _, suppressed_path_set in pairs(state.suppression_by_token) do
    for path in pairs(suppressed_path_set) do effective[path] = nil end
  end
  return effective
end

--- Projects durable ignored paths into the status section model, creating the virtual Ignored section.
---@param root string Git repository root path.
---@param section_list DiffReviewStatusSection[] Array of status sections.
---@return DiffReviewStatusSection[] sections Updated array of status sections.
function M.project(root, section_list)
  return section_map.apply_ignored_paths(section_list, effective_path_set(state_for_root(root)))
end

--- Adds relative file paths to the persistent ignored set and triggers persistence.
---@param root string Git repository root path.
---@param path_list string[] Array of relative paths to ignore.
---@return boolean changed True if the set of ignored paths was modified.
function M.ignore_paths(root, path_list)
  local state = state_for_root(root)
  local changed = false
  for _, path in ipairs(normalize_path_list(path_list)) do
    if not state.ignored_path_set[path] then
      state.ignored_path_set[path] = true
      changed = true
    end
  end
  if changed then request_write(state) end
  return changed
end

--- Removes relative file paths from the persistent ignored set and triggers persistence.
---@param root string Git repository root path.
---@param path_list string[] Array of relative paths to unignore.
---@return boolean changed True if any path was removed from the set.
function M.unignore_paths(root, path_list)
  local state = state_for_root(root)
  local changed = false
  for _, path in ipairs(normalize_path_list(path_list)) do
    if state.ignored_path_set[path] then
      state.ignored_path_set[path] = nil
      changed = true
    end
  end
  if changed then request_write(state) end
  return changed
end

--- Suppresses ignored markers while their whole-file stage mutations remain pending.
---@param root string Git repository root path.
---@param token string|integer Unique stage transaction token.
---@param path_list string[] Array of relative paths being staged.
function M.begin_stage(root, token, path_list)
  local suppressed_path_set = {}
  for _, path in ipairs(normalize_path_list(path_list)) do suppressed_path_set[path] = true end
  state_for_root(root).suppression_by_token[tostring(token)] = suppressed_path_set
end

--- Retires one stage suppression and removes markers only for targets Git completed.
---@param root string Git repository root path.
---@param token string|integer Stage transaction token.
---@param completed_path_list string[] Array of successfully staged paths.
function M.finish_stage(root, token, completed_path_list)
  local state = state_for_root(root)
  state.suppression_by_token[tostring(token)] = nil
  local changed = false
  for _, path in ipairs(normalize_path_list(completed_path_list)) do
    if state.ignored_path_set[path] then
      state.ignored_path_set[path] = nil
      changed = true
    end
  end
  if changed then request_write(state) end
end

--- Cancels a stage suppression token and restores ignored status for pending paths.
---@param root string Git repository root path.
---@param token string|integer Stage transaction token.
function M.cancel_stage(root, token)
  state_for_root(root).suppression_by_token[tostring(token)] = nil
end

--- Checks whether a relative path is present in the persistent ignored set.
---@param root string Git repository root path.
---@param path string Relative file path string.
---@return boolean exists True if the path is ignored.
function M.contains(root, path)
  local normalized = normalize_relative_path(path)
  return normalized ~= nil and state_for_root(root).ignored_path_set[normalized] == true
end

--- Asynchronously retrieves the sorted list of effective ignored paths.
---@param root string Git repository root path.
---@param callback fun(path_list: string[]?) Callback receiving the path list or nil on error.
function M.paths_async(root, callback)
  M.load_async(root, function(loaded)
    if not loaded then
      callback(nil)
      return
    end
    local path_list = vim.tbl_keys(effective_path_set(state_for_root(root)))
    table.sort(path_list)
    callback(path_list)
  end)
end

--- Overrides data directory path for unit test isolation.
---@param path? string Custom data directory path.
function M.set_data_dir_for_test(path)
  data_dir_for_test = path
end

--- Injects a custom I/O backend for testing filesystem failures.
---@param backend? DiffReviewIgnoredPathIo Custom I/O adapter.
function M.set_io_for_test(backend)
  io_for_test = backend
end

--- Resets all in-memory store states and test overrides.
function M.reset_for_test()
  state_by_key = {}
  data_dir_for_test = nil
  io_for_test = nil
end

M._normalize_relative_path = normalize_relative_path
M._store_path = store_path

return M
