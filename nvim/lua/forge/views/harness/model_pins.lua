local ModelPins = {}
local client = require("forge.client")

---@type table<string, table<string, boolean>>
local pinned_by_provider = {}
---@type table<string, boolean>
local pending_by_provider = {}
---@type table<string, integer>
local revision_by_provider = {}

---@param provider string
---@param identifier_list string[]
local function install(provider, identifier_list)
  local pinned_id_set = {}
  for _, identifier in ipairs(identifier_list) do pinned_id_set[identifier] = true end
  pinned_by_provider[provider] = pinned_id_set
end

--- Return the current globally shared pin membership for one provider.
---@param provider string
---@return table<string, boolean>?
function ModelPins.get(provider)
  return pinned_by_provider[provider]
end

--- Refresh persisted pins without replacing a newer acknowledged write.
---@param provider string
---@param callback fun(pinned_id_set: table<string, boolean>?, failure: string?)
function ModelPins.refresh(provider, callback)
  local revision = revision_by_provider[provider] or 0
  client.request({ method = "provider.model.pin.list", params = { provider = provider } }, function(identifier_list, failure)
    if failure then callback(nil, failure) return end
    if revision == (revision_by_provider[provider] or 0) then install(provider, identifier_list) end
    callback(ModelPins.get(provider))
  end)
end

--- Persist an explicit pin state and publish membership only after success.
---@param provider string
---@param model string
---@param pinned boolean
---@param callback fun(pinned_id_set: table<string, boolean>?, failure: string?)
---@return boolean accepted
function ModelPins.set(provider, model, pinned, callback)
  if pending_by_provider[provider] then return false end
  pending_by_provider[provider] = true
  client.request({ method = "provider.model.pin.set", params = { provider = provider, model = model, pinned = pinned } }, function(identifier_list, failure)
    pending_by_provider[provider] = nil
    if failure then callback(nil, failure) return end
    revision_by_provider[provider] = (revision_by_provider[provider] or 0) + 1
    install(provider, identifier_list)
    callback(ModelPins.get(provider))
  end)
  return true
end

return ModelPins
