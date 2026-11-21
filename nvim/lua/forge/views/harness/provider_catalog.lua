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
---The latest model catalog each provider returned, which labels a view's model while that view's
---own catalog loads. A model's label belongs to its provider, so another session's catalog names it.
---@type table<string, table[]>
local latest_model_list_by_provider = {}

---@param harness? table
---@return string
local function cache_key(harness)
  harness = harness or session.harness
  local provider_session = harness.session or {}
  return table.concat({
    provider_session.provider or "active",
    provider_session.workspace or "",
    provider_session.id or "",
  }, "\0")
end

---@param cache table<string, ForgeProviderCatalogEntry>
---@param method string
---@param force boolean?
---@param callback fun(value: table[]?, error: string?)
local function request_catalog(cache, method, force, callback)
  local owner_session_id = session.harness.session and session.harness.session.id
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
  client.request({ session_id = owner_session_id, method = method, params = {} }, function(value, request_error)
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
  request_catalog(skill_cache, "session.provider.skill.list", force, callback)
end

---@param force boolean?
---@param callback fun(value: table[]?, error: string?)
function M.mcp(force, callback)
  request_catalog(mcp_cache, "session.provider.mcp.list", force, callback)
end

---@param force boolean?
---@param callback fun(value: table[]?, error: string?)
function M.models(force, callback)
  local state = session.harness
  local provider = state.session and state.session.provider
  local revision = state.model_catalog_revision
  local function deliver(value, failure)
    if failure then callback(nil, failure) return end
    local models = vim.deepcopy(value or {})
    local active = state.session or {}
    local active_model = active.model == "default" and active.resolved_model or active.model
    local context_size = active.context_usage and tonumber(active.context_usage.size)
    local selected = state.session or {}
    for _, model in ipairs(models) do
      if model.id == selected.model or (selected.model == "default" and model.is_default) then
        -- The session's own thinking selection, `default` included, outranks a saved preference.
        if selected.effort then model.selected_reasoning = selected.effort end
        if selected.context_window ~= nil then model.selected_context_window = selected.context_window end
      end
      if context_size and context_size > 0
        and (model.id == active_model or (active_model == "default" and model.is_default)) then
        local context_id = active.context_window or model.default_context_window or "default"
        ---@type {id: string, token_limit: integer?}?
        local active_context
        for _, context in ipairs(model.context_window or {}) do
          if context.id == context_id then active_context = context break end
        end
        if active_context then active_context.token_limit = context_size
        else model.observed_context_token_limit = context_size end
      end
    end
    callback(models, nil)
  end
  if not force and provider == state.model_provider and type(state.model_list) == "table"
    and not state.model_list_provisional then
    deliver(state.model_list, nil)
    return
  end
  local latest = provider and latest_model_list_by_provider[provider]
  if latest and state.model_provider ~= provider then
    -- A provider switch leaves the winbar naming the previous provider's catalog; it names the
    -- model from this provider's latest catalog at once instead of by its raw id.
    state.model_provider, state.model_list, state.model_list_provisional = provider, vim.deepcopy(latest), true
    require("forge.views.harness.controller").refresh_winbar()
  end
  local key = cache_key()
  request_catalog(model_cache, "session.provider.model.list", force, function(value, failure)
    if state ~= session.harness or key ~= cache_key() then return end
    if revision ~= state.model_catalog_revision then
      M.models(false, callback)
      return
    end
    if not failure then
      if provider then latest_model_list_by_provider[provider] = vim.deepcopy(value or {}) end
      state.model_provider, state.model_list, state.model_list_provisional = provider, vim.deepcopy(value or {}), nil
      -- The winbar names the session's model by its catalog label, so it must redraw once the
      -- catalog arrives instead of keeping the raw selection id such as `opus`.
      require("forge.views.harness.controller").refresh_winbar()
    end
    deliver(value, failure)
  end)
end

---Keep the last labels visible while the next request samples the new native catalog.
---@param state table
function M.invalidate_models(state)
  model_cache[cache_key(state)] = nil
  state.model_catalog_revision = (state.model_catalog_revision or 0) + 1
  state.model_list_provisional = true
end

local function mutate(method, cache, name, enabled, callback, resource_id, scope)
  local owner_session_id = session.harness.session and session.harness.session.id
  local key = cache_key()
  client.request({ session_id = owner_session_id, method = method,
    params = { name = name, enabled = enabled, resource_id = resource_id, scope = scope } }, function(result, failure)
    cache[key] = nil
    callback(result, failure)
  end)
end

---@param name string
---@param enabled boolean
---@param callback fun(result: table?, error: string?)
---@param resource_id string?
---@param scope ForgeResourceScope?
function M.set_skill_enabled(name, enabled, callback, resource_id, scope)
  mutate("settings.skill.set_enabled", skill_cache, name, enabled, callback, resource_id, scope)
end

---@param name string
---@param enabled boolean
---@param callback fun(result: table?, error: string?)
---@param resource_id string?
---@param scope ForgeResourceScope?
function M.set_mcp_enabled(name, enabled, callback, resource_id, scope)
  mutate("settings.mcp.set_enabled", mcp_cache, name, enabled, callback, resource_id, scope)
end

---Edit the next binding's settings without addressing the current provider session.
---@param kind "skill"|"mcp"
---@param workspace string
---@param resource table
---@param enabled boolean
---@param callback fun(result: table?, error: string?)
function M.set_resource_default(kind, workspace, resource, enabled, callback)
  client.request({ method = "settings." .. kind .. ".set_enabled", params = {
    workspace = workspace, name = resource.name, enabled = enabled, resource_id = resource.resource_id,
    scope = resource.scope,
  } }, callback)
end

function M.clear()
  skill_cache = {}
  mcp_cache = {}
  model_cache = {}
  latest_model_list_by_provider = {}
end

return M
