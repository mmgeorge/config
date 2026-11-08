local M = {}

---@param models table[]?
---@param id string?
---@return table?
function M.find(models, id)
  for _, model in ipairs(models or {}) do
    if model.id == id or id == "default" and model.is_default then return model end
  end
end

--- Name a model by the provider's display label, falling back to its selection id.
--- Only an exact id match is labeled, so the `default` selection keeps its own name rather
--- than borrowing the label of whichever model the provider currently marks as default.
---@param models table[]?
---@param id string
---@return string
function M.label(models, id)
  for _, model in ipairs(models or {}) do
    if model.id == id then return model.label or id end
  end
  return id
end

---@param model table?
---@param capability table
---@return string[]
function M.speeds(model, capability)
  local values = { "default" }
  for _, tier in ipairs(model and model.service_tiers or {}) do
    if tier == "fast" and capability.fast_mode or tier == "ultrafast" and capability.ultrafast_mode then
      values[#values + 1] = tier
    end
  end
  return values
end

--- Step through a list of choices, wrapping at both ends. An unknown current value starts
--- from the first choice when moving forward and from the last when moving backward.
---@generic Choice
---@param values Choice[]
---@param value Choice?
---@param direction integer
---@return Choice?
function M.cycle(values, value, direction)
  if #values == 0 then return value end
  local position = direction > 0 and 0 or 1
  for index, candidate in ipairs(values) do if candidate == value then position = index break end end
  return values[(position - 1 + direction) % #values + 1]
end

--- List the provider ids a picker may offer, sorted by id. Non-selectable providers appear only
--- when they are `current`, so a fixture or configured provider stays visible while selected.
---@param accept? fun(descriptor: ForgeHarnessProviderConfig): boolean
---@param current? string
---@return string[]
function M.providers(accept, current)
  local result = {}
  for provider, descriptor in pairs(require("forge.client").provider_map()) do
    if (descriptor.selectable ~= false or provider == current) and (not accept or accept(descriptor)) then
      result[#result + 1] = provider
    end
  end
  table.sort(result)
  return result
end

--- Get a provider's display label, falling back to its id.
---@param provider string
---@return string
function M.provider_label(provider)
  local descriptor = require("forge.client").provider_map()[provider]
  return descriptor and descriptor.label or provider
end

return M
