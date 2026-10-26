local client = require("forge.client")
local session = require("forge.session")

local M = {}

---@class ForgeProviderCatalogEntry
---@field value table[]?
---@field callback_list (fun(value: table[]?, error: string?))[]

---@type table<string, ForgeProviderCatalogEntry>
local skill_cache = {}
---@type table<string, ForgeProviderCatalogEntry>
local mcp_cache = {}
---@type table<string, ForgeProviderCatalogEntry>
local model_cache = {}

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

---@param cache table<string, ForgeProviderCatalogEntry>
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

---@param force boolean?
---@param callback fun(value: table[]?, error: string?)
function M.models(force, callback)
  local state = session.harness
  local backend = state.session and state.session.backend
  local function deliver(value, failure)
    if failure then callback(nil, failure) return end
    local models = vim.deepcopy(value or {})
    local active = state.session or {}
    local active_model = active.model == "default" and active.resolved_model or active.model
    local context_size = active.context_usage and tonumber(active.context_usage.size)
    local selected = vim.tbl_extend("force", state.session or {}, state.configuring_config or {},
      state.pending_config or {}, state.task_config or {})
    for _, model in ipairs(models) do
      if model.id == selected.model or (selected.model == "default" and model.is_default) then
        if selected.effort and selected.effort ~= "default" then model.selected_reasoning = selected.effort end
        if selected.context_window ~= nil then model.selected_context_window = selected.context_window end
      end
      if context_size and context_size > 0
        and (model.id == active_model or (active_model == "default" and model.is_default)) then
        local context_id = active.context_window or model.default_context_window or "default"
        model.context_window = model.context_window or {}
        ---@type {id: string, token_limit: integer?}?
        local active_context
        for _, context in ipairs(model.context_window) do
          if context.id == context_id then active_context = context break end
        end
        if not active_context then
          active_context = { id = context_id }
          model.context_window[#model.context_window + 1] = active_context
        end
        active_context.token_limit = context_size
      end
    end
    callback(models, nil)
  end
  if not force and backend == state.model_backend and type(state.model_list) == "table" then
    deliver(state.model_list, nil)
    return
  end
  local key = cache_key()
  request_catalog(model_cache, "backend.models", force, function(value, failure)
    if key ~= cache_key() then return end
    if not failure then
      state.model_backend, state.model_list = backend, vim.deepcopy(value or {})
    end
    deliver(value, failure)
  end)
end

local function mutate(method, cache, name, enabled, callback, resource_id)
  local key = cache_key()
  client.request(method, { name = name, enabled = enabled, resource_id = resource_id }, function(result, request_error)
    cache[key] = nil
    if request_error or type(result) ~= "table" or not result.id then
      callback(result, request_error)
      return
    end
    local deadline = vim.uv.now() + 60000
    local function inspect()
      if cache_key() ~= key then
        callback(nil, "The selected session changed. Configuration continues in its original session.")
        return
      end
      client.request("settings.get", {}, function(snapshot, snapshot_error)
        if snapshot_error then callback(nil, snapshot_error) return end
        local application = snapshot and snapshot.application or {}
        if application.operation_id == result.id and application.state == "applied" then
          cache[key] = nil
          callback({ id = result.id, name = name, enabled = enabled, restart_required = false }, nil)
          return
        end
        if application.operation_id and application.operation_id ~= result.id then
          callback(nil, "A newer configuration superseded this selection.")
          return
        end
        if vim.tbl_contains({ "failed", "cancelled", "outcome_unknown", "superseded" }, application.state) then
          client.request("task.operation", { operation_id = result.id }, function(operation, operation_error)
            callback(nil, operation_error or (operation and operation.error) or "Configuration " .. application.state)
          end)
          return
        end
        if vim.uv.now() >= deadline then
          callback(nil, "Configuration is still pending. Check the timeline for its outcome.")
          return
        end
        vim.defer_fn(inspect, 100)
      end)
    end
    inspect()
  end)
end

---@param name string
---@param enabled boolean
---@param callback fun(result: table?, error: string?)
---@param resource_id string?
function M.set_skill_enabled(name, enabled, callback, resource_id)
  mutate("backend.skills.set_enabled", skill_cache, name, enabled, callback, resource_id)
end

---@param name string
---@param enabled boolean
---@param callback fun(result: table?, error: string?)
---@param resource_id string?
function M.set_mcp_enabled(name, enabled, callback, resource_id)
  mutate("backend.mcp.set_enabled", mcp_cache, name, enabled, callback, resource_id)
end

function M.clear()
  skill_cache = {}
  mcp_cache = {}
  model_cache = {}
end

return M
