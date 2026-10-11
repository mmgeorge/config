local client = require("forge.client")

---@class ForgeSubagentModel
---@field id string
---@field reasoning string[]
---@field is_default? boolean

---@class ForgeSubagentModelCatalog
---@field models ForgeSubagentModel[]
---@field capability {fast_mode: boolean, ultrafast_mode: boolean}

---@class ForgeSubagentSelection
---@field state ForgeHarnessPresentationState
---@field catalog table<string, ForgeSubagentModelCatalog>
local Selection = {}
Selection.__index = Selection

local provider_list = { "inherit", "codex", "copilot", "claude", "antigravity" }

local function cycle(values, value, direction)
  local position = direction > 0 and 0 or 1
  for index, candidate in ipairs(values) do if candidate == value then position = index break end end
  return values[(position - 1 + direction) % #values + 1]
end

---@param catalog ForgeSubagentModelCatalog
---@param value string
---@param root table
---@return ForgeSubagentModel?
local function selected_model(catalog, value, root)
  local selected = value == "inherit" and (root.resolved_model or root.model) or value
  for _, model in ipairs(catalog.models) do
    if model.id == selected or selected == "default" and model.is_default then return model end
  end
  if selected == "default" then return catalog.models[1] end
end

---@param catalog ForgeSubagentModelCatalog
---@return string[]
local function speed_values(catalog)
  local values = { "default" }
  if catalog.capability.fast_mode then values[#values + 1] = "fast" end
  if catalog.capability.ultrafast_mode then values[#values + 1] = "ultrafast" end
  return values
end

---@param field string
---@param values table<string, string>
---@param catalog ForgeSubagentModelCatalog
---@param root table
---@return string[]
local function choices(field, values, catalog, root)
  local result = { "default" }
  if field == "model" then
    local provider = values.provider == "inherit" and root.backend or values.provider
    if provider == root.backend and selected_model(catalog, "inherit", root) then table.insert(result, 1, "inherit") end
    for _, model in ipairs(catalog.models) do
      if not vim.tbl_contains(result, model.id) then result[#result + 1] = model.id end
    end
  elseif field == "reasoning_effort" then
    local model = selected_model(catalog, values.model, root)
    if model then
      if root.effort == nil or root.effort == "default" or vim.tbl_contains(model.reasoning or {}, root.effort) then
        table.insert(result, 1, "inherit")
      end
      for _, effort in ipairs(model.reasoning or {}) do
        if not vim.tbl_contains(result, effort) then result[#result + 1] = effort end
      end
    end
  else
    result = speed_values(catalog)
    if vim.tbl_contains(result, root.service_tier or "default") then table.insert(result, 1, "inherit") end
  end
  return result
end

---Owns the bounded provider catalog cache for one profile editor lifetime.
---@param state ForgeHarnessPresentationState
---@return ForgeSubagentSelection
function Selection.new(state)
  return setmetatable({ state = state, catalog = {} }, Selection)
end

---Cycles one setting and reconciles dependent settings before returning any changes.
---@param values table<string, string>
---@param field string
---@param direction integer
---@param done fun(values: table<string, string>?, failure?: string)
function Selection:cycle(values, field, direction, done)
  local root = vim.deepcopy(self.state.session)
  if not root then done(nil, "Harness session changed. Reopen the profile editor") return end
  local updated = vim.deepcopy(values)
  local catalog_cache = self.catalog
  if field == "provider" then updated.provider = cycle(provider_list, values.provider, direction) end
  local provider = updated.provider == "inherit" and root.backend or updated.provider
  local function apply(catalog, failure)
    if failure then done(nil, failure) return end
    if not catalog or not catalog.models or #catalog.models == 0 or not catalog.capability then
      done(nil, "No models available for " .. provider) return
    end
    catalog_cache[provider] = catalog
    if field == "provider" then updated.model = catalog.models[1].id
    else updated[field] = cycle(choices(field, values, catalog, root), values[field], direction) end
    if field == "provider" or field == "model" then
      if not selected_model(catalog, updated.model, root) then
        done(nil, "Model " .. updated.model .. " is not available for " .. provider) return
      end
      local thinking = choices("reasoning_effort", updated, catalog, root)
      if not vim.tbl_contains(thinking, updated.reasoning_effort) then updated.reasoning_effort = "default" end
      local speed = choices("service_tier", updated, catalog, root)
      if not vim.tbl_contains(speed, updated.service_tier) then updated.service_tier = "default" end
    end
    done({ provider = updated.provider, model = updated.model,
      reasoning_effort = updated.reasoning_effort, service_tier = updated.service_tier })
  end
  if catalog_cache[provider] then apply(catalog_cache[provider])
  else client.request_for(root.id, "agent.models", { provider = provider }, apply) end
end

return Selection
