local ModelOrder = {}

---@class ForgeHarnessModel
---@field service_tiers? string[] Supported processing tiers from the provider's model catalog.
---@field id string
---@field reasoning? string[]
---@field default_reasoning? string
---@field selected_reasoning? string
---@field context_window? {id: string, token_limit: integer?}[]
---@field default_context_window? string
---@field selected_context_window? string
---@field vision? boolean
---@field description? string
---@field is_default? boolean
---@field picker_reasoning? string
---@field picker_context_window? string

--- Preserve the source catalog while applying the provider policy and pins first.
---@param model_list ForgeHarnessModel[]
---@param provider string
---@param pinned_id_set? table<string, boolean>
---@return ForgeHarnessModel[]
function ModelOrder.order(model_list, provider, pinned_id_set)
  local ranked_list = {}
  for ordinal, model in ipairs(model_list) do ranked_list[ordinal] = { model = model } end
  local ordered_list = {}
  for _, pinned in ipairs({ true, false }) do
    for _, entry in ipairs(ranked_list) do
      if ((pinned_id_set or {})[entry.model.id] == true) == pinned then
        ordered_list[#ordered_list + 1] = entry.model
      end
    end
  end
  return ordered_list
end

return ModelOrder
