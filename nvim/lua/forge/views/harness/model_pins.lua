local ModelPins = {}
local client = require("forge.client")

---@type table<string, table<string, boolean>>
local pinned_by_backend = {}
---@type table<string, boolean>
local pending_by_backend = {}
---@type table<string, integer>
local revision_by_backend = {}

---@param backend string
---@param identifier_list string[]
local function install(backend, identifier_list)
  local pinned_id_set = {}
  for _, identifier in ipairs(identifier_list) do pinned_id_set[identifier] = true end
  pinned_by_backend[backend] = pinned_id_set
end

--- Return the current globally shared pin membership for one backend.
---@param backend string
---@return table<string, boolean>?
function ModelPins.get(backend)
  return pinned_by_backend[backend]
end

--- Refresh persisted pins without replacing a newer acknowledged write.
---@param backend string
---@param callback fun(pinned_id_set: table<string, boolean>?, failure: string?)
function ModelPins.refresh(backend, callback)
  local revision = revision_by_backend[backend] or 0
  client.request("backend.model_pins", { backend = backend }, function(identifier_list, failure)
    if failure then callback(nil, failure) return end
    if revision == (revision_by_backend[backend] or 0) then install(backend, identifier_list) end
    callback(ModelPins.get(backend))
  end)
end

--- Persist an explicit pin state and publish membership only after success.
---@param backend string
---@param model string
---@param pinned boolean
---@param callback fun(pinned_id_set: table<string, boolean>?, failure: string?)
---@return boolean accepted
function ModelPins.set(backend, model, pinned, callback)
  if pending_by_backend[backend] then return false end
  pending_by_backend[backend] = true
  client.request("backend.model_pin", { backend = backend, model = model, pinned = pinned }, function(identifier_list, failure)
    pending_by_backend[backend] = nil
    if failure then callback(nil, failure) return end
    revision_by_backend[backend] = (revision_by_backend[backend] or 0) + 1
    install(backend, identifier_list)
    callback(ModelPins.get(backend))
  end)
  return true
end

return ModelPins
