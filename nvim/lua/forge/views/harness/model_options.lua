local M = {}

---@param models table[]?
---@param id string?
---@return table?
function M.find(models, id)
  for _, model in ipairs(models or {}) do
    if model.id == id or id == "default" and model.is_default then return model end
  end
end

---@param model table
---@return string
local function described_model(model)
  if type(model.label) == "string" and model.label ~= model.id then return ("%s (%s)"):format(model.id, model.label) end
  return model.id
end

---@param models table[]
---@return string
local function described_model_list(models)
  local description_list = {}
  for _, model in ipairs(models) do description_list[#description_list + 1] = described_model(model) end
  return #description_list > 0 and table.concat(description_list, ", ") or "none"
end

--- List the catalog models a typed name selects. An exact id, or `default` for the provider's
--- default model, wins; otherwise the name selects every model whose id or display label equals it
--- without regard to case, so the label the winbar and picker show selects its model.
---@param models table[]
---@param name string
---@return { id: string, model: table }[]
local function matching_model_list(models, name)
  local match_list = {}
  for _, model in ipairs(models) do
    if model.id == name then return { { id = model.id, model = model } } end
    if name == "default" and model.is_default then match_list[#match_list + 1] = { id = "default", model = model } end
  end
  if #match_list > 0 then return match_list end
  local folded_name = name:lower()
  for _, model in ipairs(models) do
    if model.id:lower() == folded_name or type(model.label) == "string" and model.label:lower() == folded_name then
      match_list[#match_list + 1] = { id = model.id, model = model }
    end
  end
  return match_list
end

--- Resolve the arguments of `/model <model> [thinking] [prompt]` against a provider catalog. The
--- model is the longest leading run of words that names a catalog model by id or display label, so
--- a label with spaces such as `Haiku 5.5` selects its model. The word after the model is its
--- thinking level, which must be one the model offers or `default`; any text after that level is
--- the prompt, which only a queued entry may carry. A name that matches no model or several models
--- is refused with the models it could have meant.
---@param models table[]
---@param arguments string
---@param accepts_prompt boolean
---@return { model: string, effort: string? }? selection
---@return string? prompt
---@return string? failure
function M.resolve_command(models, arguments, accepts_prompt)
  local word_list = {}
  for start, finish in arguments:gmatch("()%S+()") do word_list[#word_list + 1] = { start = start, finish = finish } end
  if #word_list == 0 then return nil, nil, "name a model after /model" end
  for name_length = #word_list, 1, -1 do
    local name = arguments:sub(word_list[1].start, word_list[name_length].finish - 1)
    local match_list = matching_model_list(models, name)
    if #match_list > 1 then
      local matched_models = {}
      for _, match in ipairs(match_list) do matched_models[#matched_models + 1] = match.model end
      return nil, nil, ("the model name %q is ambiguous; it matches %s"):format(name, described_model_list(matched_models))
    end
    local match = match_list[1]
    if match then
      local effort_word = word_list[name_length + 1]
      if not effort_word then return { model = match.id }, "", nil end
      local effort = arguments:sub(effort_word.start, effort_word.finish - 1)
      local reasoning = match.model.reasoning or {}
      if effort ~= "default" and not vim.tbl_contains(reasoning, effort) then
        return nil, nil, ("%s has no thinking level %q; it offers %s"):format(described_model(match.model), effort,
          table.concat(vim.list_extend({ "default" }, vim.deepcopy(reasoning)), ", "))
      end
      local prompt_word = word_list[name_length + 2]
      local prompt = prompt_word and arguments:sub(prompt_word.start) or ""
      if prompt ~= "" and not accepts_prompt then
        return nil, nil, "a /model command that carries a prompt must be queued"
      end
      return { model = match.id, effort = effort }, prompt, nil
    end
  end
  return nil, nil, ("no model is named %q; the provider offers %s"):format(
    arguments:sub(word_list[1].start, word_list[#word_list].finish - 1), described_model_list(models))
end

--- Name a model by the provider's display label, falling back to a saved label and then its
--- selection id. Only an exact id match is labeled, so the `default` selection keeps its own name
--- rather than borrowing the label of whichever model the provider currently marks as default.
---@param models table[]?
---@param id string
---@param saved_label? string The label recorded when the model was selected, used until the catalog loads.
---@return string
function M.label(models, id, saved_label)
  for _, model in ipairs(models or {}) do
    if model.id == id then return model.label or saved_label or id end
  end
  return saved_label or id
end

--- Name the thinking level a provider uses for `effort` on `model`. A `default` selection sends no
--- level, so the provider applies its own default: the catalog's `default_reasoning` when it names
--- one of the model's levels, and an unnamed provider default otherwise, which reads `default`.
---@param model table?
---@param effort string?
---@return string
function M.thinking(model, effort)
  if effort and effort ~= "default" then return effort end
  local default_reasoning = model and model.default_reasoning
  if default_reasoning and vim.tbl_contains(model.reasoning or {}, default_reasoning) then return default_reasoning end
  return "default"
end

--- List the thinking choices offered for `model`, in provider order. A model whose provider
--- default is not one of its named levels also offers `default`, first, so the choice the session
--- holds is always one the picker and the effort keys can show and return to.
---@param model table?
---@return string[]
function M.reasoning_choices(model)
  local reasoning = model and model.reasoning or {}
  if #reasoning == 0 or M.thinking(model, "default") ~= "default" then return vim.deepcopy(reasoning) end
  return vim.list_extend({ "default" }, reasoning)
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
