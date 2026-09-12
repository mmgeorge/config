local M = {}

local client = require("forge.client")
local completion = require("forge.completion")
local MAX_BYTES = 2 * 1024 * 1024
local REFRESH_MS = 10000

---@type CompletionCache?
local cache
local workspace = ""
local preparing = false
local refresh_at = 0
local retry_at = 0
local truncated_notice = false

---@param message string
local function notify(message)
  vim.notify("Forge revision completion: " .. message, vim.log.levels.ERROR)
end

---@param value any
---@param maximum integer
---@return boolean
local function counter(value, maximum)
  return type(value) == "number" and value >= 0 and value <= maximum and value % 1 == 0
end

---@param identity string
---@param generation string
---@param done ForgeCompletionDone
local function request_candidates(identity, generation, done)
  refresh_at = vim.uv.now() + REFRESH_MS
  local chunks, offset, revision, total_bytes, count, truncated = {}, 0, nil, nil, nil, nil
  local repository, reference_digest
  local function request_page()
    if not client._client or tostring(client._client.generation) ~= generation then
      done(nil, "Forge host generation changed during revision refresh")
      return
    end
    client.request_host("repository.revisions", { workspace = identity, revision = revision, offset = offset, repository = repository, reference_digest = reference_digest }, function(page, failure)
      if failure then done(nil, failure) return end
      if type(page) ~= "table" or not counter(page.revision, 9007199254740991) or page.revision == 0
          or not counter(page.total_bytes, MAX_BYTES) or not counter(page.count, 20000)
          or page.offset ~= offset or not counter(page.next_offset, MAX_BYTES)
          or page.next_offset < offset or page.next_offset > page.total_bytes
          or type(page.repository) ~= "string" or #page.repository > 65536
          or type(page.reference_digest) ~= "string" or not page.reference_digest:match("^[0-9a-f]+$") or #page.reference_digest ~= 64
          or type(page.truncated) ~= "boolean" or type(page.data) ~= "string" or #page.data > 174764 then
        done(nil, "invalid revision page metadata")
        return
      end
      if revision and (page.revision ~= revision or page.total_bytes ~= total_bytes or page.count ~= count or page.truncated ~= truncated or page.repository ~= repository or page.reference_digest ~= reference_digest) then
        done(nil, "revision snapshot changed between pages")
        return
      end
      local ok, decoded = pcall(vim.base64.decode, page.data)
      if not ok or #decoded ~= page.next_offset - offset or (decoded == "" and page.next_offset ~= page.total_bytes) then
        done(nil, "invalid revision page payload")
        return
      end
      repository, reference_digest = page.repository, page.reference_digest
      revision, total_bytes, count, truncated = page.revision, page.total_bytes, page.count, page.truncated
      chunks[#chunks + 1] = decoded
      offset = page.next_offset
      if offset < total_bytes then request_page() return end
      local bytes = table.concat(chunks)
      if (count == 0 and bytes ~= "") or (count > 0 and bytes:sub(-1) ~= "\0") then
        done(nil, "revision snapshot has incomplete argument framing")
        return
      end
      local values = {}
      local start = 1
      for boundary in bytes:gmatch("()%z") do
        values[#values + 1] = bytes:sub(start, boundary - 1)
        start = boundary + 1
      end
      if #values ~= count then done(nil, "revision snapshot candidate count differs") return end
      if truncated and not truncated_notice then
        vim.notify("Revision completion is truncated. You can still enter a revision manually.", vim.log.levels.WARN)
      end
      truncated_notice = truncated
      done({ identity = identity, generation = generation, revision = revision, values = values, truncated = truncated }, nil)
    end)
  end
  request_page()
end

local function prepare()
  if preparing or workspace == "" or vim.uv.now() < retry_at then return end
  preparing = true
  local identity = workspace
  vim.schedule(function()
    client.start(function(host, failure)
      preparing = false
      if failure then
        retry_at = vim.uv.now() + 1000
        notify(failure)
        return
      end
      if identity ~= workspace then return end
      local generation = tostring(host.generation)
      if not cache or cache.identity ~= identity or cache.generation ~= generation then
        cache = completion.new(identity, generation, request_candidates)
        truncated_notice = false
      end
      completion.refresh(cache)
    end)
  end)
end

---Captures command context outside the synchronous completion callback.
function M.setup()
  workspace = vim.fn.getcwd()
  local group = vim.api.nvim_create_augroup("ForgeRevisionCompletion", { clear = true })
  vim.api.nvim_create_autocmd({ "CmdlineEnter", "DirChanged" }, {
    group = group,
    callback = function()
      local current = vim.fn.getcwd()
      if current ~= workspace then
        workspace = current
        cache = nil
        retry_at = 0
        refresh_at = 0
      end
    end,
  })
end

---@param prefix string
---@return string[]
function M.values(prefix)
  local host = client._client
  if not cache or not host or not host.ready or cache.generation ~= tostring(host.generation) then
    prepare()
    return {}
  end
  if vim.uv.now() >= refresh_at and not cache.pending then completion.invalidate(cache) end
  return completion.values(cache, prefix)
end

return M
