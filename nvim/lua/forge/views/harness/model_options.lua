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

return M
