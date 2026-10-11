local ModelOrder = {}

---@class ForgeHarnessModel
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

---@class ForgeHarnessModelRank
---@field model ForgeHarnessModel
---@field ordinal integer
---@field family integer
---@field version integer[]
---@field variant integer

local claude_variant = { opus = 1, sonnet = 2, haiku = 3 }

---@param model ForgeHarnessModel
---@param ordinal integer
---@return ForgeHarnessModelRank
local function rank(model, ordinal)
  local identifier = model.id:lower()
  local family, version, variant = 4, nil, 0
  if identifier == "auto" then
    family = 0
  else
    local claude_kind, claude_version = identifier:match("^claude%-(%a+)%-([%d%.]+)")
    if claude_kind and claude_variant[claude_kind] then
      family, version, variant = 1, claude_version, claude_variant[claude_kind]
    elseif identifier:match("^gpt%-%d") then
      family, version = 2, identifier:match("^gpt%-([%d%.]+)")
    elseif identifier:match("^gemini%-%d") then
      family, version = 3, identifier:match("^gemini%-([%d%.]+)")
    end
  end
  local component_list = {}
  for component in (version or ""):gmatch("%d+") do component_list[#component_list + 1] = tonumber(component) end
  return { model = model, ordinal = ordinal, family = family, version = component_list, variant = variant }
end

---@param left ForgeHarnessModelRank
---@param right ForgeHarnessModelRank
---@return boolean
local function copilot_before(left, right)
  if left.family ~= right.family then return left.family < right.family end
  if left.family == 4 then return left.ordinal < right.ordinal end
  for index = 1, math.max(#left.version, #right.version) do
    local left_component, right_component = left.version[index] or 0, right.version[index] or 0
    if left_component ~= right_component then return left_component > right_component end
  end
  if left.variant ~= right.variant then return left.variant < right.variant end
  if left.model.id ~= right.model.id then return left.model.id < right.model.id end
  return left.ordinal < right.ordinal
end

local policy = { copilot = copilot_before }

--- Preserve the source catalog while applying the backend policy and pins first.
---@param model_list ForgeHarnessModel[]
---@param backend string
---@param pinned_id_set? table<string, boolean>
---@return ForgeHarnessModel[]
function ModelOrder.order(model_list, backend, pinned_id_set)
  local ranked_list = {}
  for ordinal, model in ipairs(model_list) do ranked_list[ordinal] = rank(model, ordinal) end
  local before = policy[backend]
  if before then table.sort(ranked_list, before) end
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
