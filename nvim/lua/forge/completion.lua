local M = {}

local MAX_VALUES = 20000
local MAX_BYTES = 2 * 1024 * 1024
local MAX_RESULTS = 200
local VALUE_INDEX_BYTES = 64
local SNAPSHOT_ACCOUNTING_BYTES = 4096
local RETRY_DELAY_MS = 1000

---@class ForgeCompletionSnapshot
---@field identity string
---@field generation string
---@field revision integer
---@field values string[]
---@field truncated boolean

---@alias ForgeCompletionDone fun(snapshot: ForgeCompletionSnapshot?, failure: string?)
---@alias ForgeCompletionRequest fun(identity: string, generation: string, done: ForgeCompletionDone)

---@class CompletionCache
---@field identity string
---@field generation string
---@field request ForgeCompletionRequest
---@field snapshot ForgeCompletionSnapshot?
---@field pending table?
---@field stale boolean
---@field retry_at number
---@field accounted_bytes integer
---@field version table

---@param message string
local function notify(message)
  vim.notify("Forge revision completion: " .. message, vim.log.levels.ERROR)
end

---@param snapshot ForgeCompletionSnapshot
---@param cache CompletionCache
---@return string[]?, string?
local function validate(snapshot, cache)
  if type(snapshot) ~= "table" or snapshot.identity ~= cache.identity or snapshot.generation ~= cache.generation then
    return nil, "snapshot identity or host generation was superseded"
  end
  if type(snapshot.revision) ~= "number" or snapshot.revision < 1 or snapshot.revision > 9007199254740991
      or snapshot.revision % 1 ~= 0 then
    return nil, "snapshot revision is invalid"
  end
  if cache.snapshot and snapshot.revision <= cache.snapshot.revision then
    return nil, "snapshot revision was superseded"
  end
  if type(snapshot.values) ~= "table" or #snapshot.values > MAX_VALUES or type(snapshot.truncated) ~= "boolean" then
    return nil, "snapshot candidate shape or count is invalid"
  end
  local count = #snapshot.values
  local total = 0
  for key in pairs(snapshot.values) do
    if type(key) ~= "number" or key % 1 ~= 0 or key < 1 or key > count then
      return nil, "snapshot candidates must be a dense list"
    end
    total = total + 1
  end
  if total ~= count then return nil, "snapshot candidates must be a dense list" end
  local bytes = SNAPSHOT_ACCOUNTING_BYTES + count * VALUE_INDEX_BYTES
  for index = 1, count do
    local value = snapshot.values[index]
    if type(value) ~= "string" or value == "" or value:find("[%z\1-\32\127]") then
      return nil, "snapshot contains an invalid revision argument"
    end
    if index > 1 and snapshot.values[index - 1] >= value then
      return nil, "snapshot candidates must be strictly sorted"
    end
    bytes = bytes + #value
    if bytes > MAX_BYTES then return nil, "snapshot candidate byte limit exceeded" end
  end
  local values = {}
  for index = 1, count do values[index] = snapshot.values[index] end
  return values
end

---Creates one list cache whose request callback starts only from the scheduled refresh.
---@param identity string
---@param generation string
---@param request ForgeCompletionRequest
---@return CompletionCache
function M.new(identity, generation, request)
  assert(type(identity) == "string" and identity ~= "", "completion requires list identity")
  assert(type(generation) == "string" and generation ~= "", "completion requires host generation")
  assert(type(request) == "function", "completion requires an asynchronous request")
  return { identity = identity, generation = generation, request = request, stale = true, retry_at = 0, accounted_bytes = 0, version = {} }
end

---Publishes a validated snapshot or notifies while preserving the previously accepted snapshot.
---@param cache CompletionCache
---@param snapshot ForgeCompletionSnapshot
---@return boolean
function M.replace(cache, snapshot)
  local values, failure = validate(snapshot, cache)
  if not values then
    notify(failure or "snapshot validation failed")
    return false
  end
  local accounted_bytes = SNAPSHOT_ACCOUNTING_BYTES + #values * VALUE_INDEX_BYTES
  for index = 1, #values do accounted_bytes = accounted_bytes + #values[index] end
  cache.snapshot = {
    identity = snapshot.identity,
    generation = snapshot.generation,
    revision = snapshot.revision,
    values = values,
    truncated = snapshot.truncated,
  }
  cache.accounted_bytes = accounted_bytes
  cache.stale = false
  cache.retry_at = 0
  return true
end

---Coalesces one pending refresh and delays retries for one second after a failure.
---@param cache CompletionCache
---@return boolean scheduled
function M.refresh(cache)
  if cache.pending or not cache.stale or vim.uv.now() < cache.retry_at then return false end
  local pending = {}
  local generation = cache.generation
  local version = cache.version
  cache.pending = pending
  vim.schedule(function()
    if cache.pending ~= pending or cache.generation ~= generation then return end
    local delivered = false
    ---@type ForgeCompletionDone
    local function done(snapshot, failure)
      if delivered then return end
      delivered = true
      vim.schedule(function()
        if cache.pending ~= pending or cache.generation ~= generation then
          notify("refresh response belongs to a superseded host generation")
          return
        end
        cache.pending = nil
        if cache.version ~= version then
          cache.stale = true
          notify("refresh was superseded by repository invalidation")
          return
        end
        if failure or not snapshot then
          cache.stale = true
          cache.retry_at = vim.uv.now() + RETRY_DELAY_MS
          notify(failure or "refresh returned no snapshot")
          return
        end
        if not M.replace(cache, snapshot) then
          cache.stale = true
          cache.retry_at = vim.uv.now() + RETRY_DELAY_MS
        end
      end)
    end
    local ok, failure = pcall(cache.request, cache.identity, generation, done)
    if not ok then
      if delivered then notify(tostring(failure)) else done(nil, tostring(failure)) end
    end
  end)
  return true
end

---Returns at most 200 prefix matches without repository I/O or synchronous request dispatch.
---@param cache CompletionCache
---@param prefix string
---@return string[]
function M.values(cache, prefix)
  M.refresh(cache)
  local snapshot = cache.snapshot
  if not snapshot then return {} end
  local values = snapshot.values
  local first, last = 1, #values + 1
  while first < last do
    local middle = math.floor((first + last) / 2)
    if values[middle] < prefix then first = middle + 1 else last = middle end
  end
  local result = {}
  for index = first, math.min(#values, first + MAX_RESULTS - 1) do
    local value = values[index]
    if value:sub(1, #prefix) ~= prefix then break end
    result[#result + 1] = value
  end
  return result
end

---Retains stale candidates and prevents an already-started refresh from becoming current.
---@param cache CompletionCache
function M.invalidate(cache)
  cache.version = {}
  cache.stale = true
  cache.retry_at = 0
end

---Drops candidates when the host generation changes and rejects delayed old-host responses.
---@param cache CompletionCache
---@param generation string
function M.clear_generation(cache, generation)
  assert(type(generation) == "string" and generation ~= "", "completion requires host generation")
  if generation == cache.generation then return end
  cache.generation = generation
  cache.snapshot = nil
  cache.pending = nil
  cache.stale = true
  cache.retry_at = 0
  cache.accounted_bytes = 0
end

return M
