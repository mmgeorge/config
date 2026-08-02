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

---@param root string
---@return string
local function root_key(root)
  local normalized = paths.normalize_path(root):gsub("\\", "/")
  return vim.fn.has("win32") == 1 and normalized:lower() or normalized
end

---@param path string
---@return string?
local function normalize_relative_path(path)
  if type(path) ~= "string" or path == "" or path:find("\0", 1, true) then return nil end
  local normalized = path:gsub("\\", "/"):gsub("^%./", "")
  if normalized == "" or normalized == "." or normalized == ".." or normalized:sub(1, 3) == "../" then return nil end
  if normalized:match("^%a:/") or normalized:sub(1, 1) == "/" then return nil end
  return normalized
end

---@param path_list string[]
---@return string[]
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

---@param root string
---@return DiffReviewIgnoredPathState
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

---@param root string
---@return string
local function store_path(root)
  local directory = data_dir_for_test
    or vim.fs.joinpath(vim.fn.stdpath("data"), "diff-review", "status-ignored")
  return vim.fs.joinpath(directory, vim.fn.sha256(root_key(root)) .. ".json")
end

---@param callback fun(content?: string, error?: string)
local function schedule_read_result(callback, content, error)
  vim.schedule(function() callback(content, error) end)
end

---@param path string
---@param callback fun(content?: string, error?: string)
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

---@param callback fun(error?: string)
local function schedule_write_result(callback, error)
  vim.schedule(function() callback(error) end)
end

---@param path string
---@param content string
---@param callback fun(error?: string)
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

---@return DiffReviewIgnoredPathIo
local function io_backend()
  return io_for_test or {
    read_async = read_async,
    write_atomic_async = write_atomic_async,
  }
end

---@param state DiffReviewIgnoredPathState
---@return string
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

---@param state DiffReviewIgnoredPathState
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

---@param state DiffReviewIgnoredPathState
---@param content? string
---@return boolean, string?
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

---@param state DiffReviewIgnoredPathState
---@param ok boolean
local function finish_load(state, ok)
  state.loading = false
  state.loaded = true
  local waiter_list = state.waiter_list
  state.waiter_list = {}
  for _, waiter in ipairs(waiter_list) do waiter(ok) end
end

--- Load one worktree's ignored paths without blocking the first status render.
---@param root string
---@param callback fun(ok: boolean)
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

---@param state DiffReviewIgnoredPathState
---@return table<string, boolean>
local function effective_path_set(state)
  local effective = vim.deepcopy(state.ignored_path_set)
  for _, suppressed_path_set in pairs(state.suppression_by_token) do
    for path in pairs(suppressed_path_set) do effective[path] = nil end
  end
  return effective
end

--- Project durable ignored paths over the Git and optimistic section model.
---@param root string
---@param section_list DiffReviewStatusSection[]
---@return DiffReviewStatusSection[]
function M.project(root, section_list)
  return section_map.apply_ignored_paths(section_list, effective_path_set(state_for_root(root)))
end

---@param root string
---@param path_list string[]
---@return boolean changed
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

---@param root string
---@param path_list string[]
---@return boolean changed
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

--- Suppress ignored markers while their whole-file stage mutations remain unresolved.
---@param root string
---@param token string|integer
---@param path_list string[]
function M.begin_stage(root, token, path_list)
  local suppressed_path_set = {}
  for _, path in ipairs(normalize_path_list(path_list)) do suppressed_path_set[path] = true end
  state_for_root(root).suppression_by_token[tostring(token)] = suppressed_path_set
end

--- Retire one stage suppression and delete markers only for targets Git completed.
---@param root string
---@param token string|integer
---@param completed_path_list string[]
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

---@param root string
---@param token string|integer
function M.cancel_stage(root, token)
  state_for_root(root).suppression_by_token[tostring(token)] = nil
end

---@param root string
---@param path string
---@return boolean
function M.contains(root, path)
  local normalized = normalize_relative_path(path)
  return normalized ~= nil and state_for_root(root).ignored_path_set[normalized] == true
end

---@param path? string
function M.set_data_dir_for_test(path)
  data_dir_for_test = path
end

---@param backend? DiffReviewIgnoredPathIo
function M.set_io_for_test(backend)
  io_for_test = backend
end

function M.reset_for_test()
  state_by_key = {}
  data_dir_for_test = nil
  io_for_test = nil
end

M._normalize_relative_path = normalize_relative_path
M._store_path = store_path

return M
