local client = require("diff_review.harness.client")
local session = require("diff_review.session")

local M = {}

---@class DiffReviewProviderCatalogEntry
---@field value table[]?
---@field callback_list (fun(value: table[]?, error: string?))[]

---@type table<string, DiffReviewProviderCatalogEntry>
local skill_cache = {}
---@type table<string, DiffReviewProviderCatalogEntry>
local mcp_cache = {}

---@return string
local function cache_key()
  local harness = session.harness
  local provider_session = harness.session or {}
  return table.concat({
    provider_session.backend or "active",
    provider_session.workspace or "",
    provider_session.id or "",
  }, "\0")
end

---@param cache table<string, DiffReviewProviderCatalogEntry>
---@param method string
---@param force boolean?
---@param callback fun(value: table[]?, error: string?)
local function request_catalog(cache, method, force, callback)
  local key = cache_key()
  local entry = cache[key]
  if not entry or force then
    entry = { value = nil, callback_list = {} }
    cache[key] = entry
  end
  if entry.value then
    callback(vim.deepcopy(entry.value), nil)
    return
  end
  entry.callback_list[#entry.callback_list + 1] = callback
  if #entry.callback_list > 1 then return end
  client.request(method, {}, function(value, request_error)
    local callback_list = entry.callback_list
    entry.callback_list = {}
    if not request_error then entry.value = type(value) == "table" and value or {} end
    for _, pending_callback in ipairs(callback_list) do
      pending_callback(entry.value and vim.deepcopy(entry.value) or nil, request_error)
    end
  end)
end

---@param force boolean?
---@param callback fun(value: table[]?, error: string?)
function M.skills(force, callback)
  request_catalog(skill_cache, "backend.skills", force, callback)
end

---@param force boolean?
---@param callback fun(value: table[]?, error: string?)
function M.mcp(force, callback)
  request_catalog(mcp_cache, "backend.mcp", force, callback)
end

---@param name string
---@param enabled boolean
---@param callback fun(result: table?, error: string?)
function M.set_skill_enabled(name, enabled, callback)
  client.request("backend.skills.set_enabled", { name = name, enabled = enabled }, function(result, request_error)
    skill_cache[cache_key()] = nil
    callback(result, request_error)
  end)
end

---@param name string
---@param enabled boolean
---@param callback fun(result: table?, error: string?)
function M.set_mcp_enabled(name, enabled, callback)
  client.request("backend.mcp.set_enabled", { name = name, enabled = enabled }, function(result, request_error)
    mcp_cache[cache_key()] = nil
    callback(result, request_error)
  end)
end

function M.clear()
  skill_cache = {}
  mcp_cache = {}
end

return M
