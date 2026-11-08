local BackgroundConfiguration = {}
BackgroundConfiguration.__index = BackgroundConfiguration
local store = require("forge.integrations.background_settings")
local client = require("forge.client")
local picker = require("forge.views.picker")
local notifications = require("forge.infra.notifications")
local model_options = require("forge.views.harness.model_options")
local threshold = require("forge.views.harness.compaction_threshold")

---@class ForgeBackgroundConfigurationOptions
---@field session_id string Root session that owns catalog requests for any Background provider.
---@field host table
---@field current fun(): boolean
---@field changed fun()
---@field reopen fun()

---@class ForgeBackgroundModelCatalog
---@field models ForgeHarnessModel[]
---@field capability {fast_mode?: boolean, ultrafast_mode?: boolean}

---@class ForgeBackgroundConfiguration
---@field options ForgeBackgroundConfigurationOptions
---@field snapshot? ForgeBackgroundSettings
---@field catalog? ForgeBackgroundModelCatalog Model catalog of `catalog_key`, the provider it was read for.
---@field catalog_key? string
---@field pending boolean
---@field failure? string
---@field revision integer

--- Report whether a provider can run tool-free Background sessions.
---@param descriptor ForgeHarnessProviderConfig
---@return boolean
local function supports_background(descriptor)
  return descriptor.background_generation ~= nil
    or ((descriptor.capability or {}).integration or {}).background_generation == true
end

---@param options ForgeBackgroundConfigurationOptions
---@return ForgeBackgroundConfiguration
function BackgroundConfiguration.new(options)
  local self = setmetatable({ options = options, pending = false, revision = 0 }, BackgroundConfiguration)
  store.request(nil, nil, function(snapshot, failure)
    if not options.current() then return end
    self.snapshot, self.failure = snapshot, failure
    if failure then notifications.error(failure, "Background settings") end
    self:load_models()
    options.changed()
  end)
  return self
end

---@return string?
function BackgroundConfiguration:provider()
  return self.snapshot and store.provider(self.snapshot)
end

--- Read the selected provider's catalog through the root session, which can list any provider.
---@param on_loaded? fun(catalog: ForgeBackgroundModelCatalog)
function BackgroundConfiguration:load_models(on_loaded)
  local provider = self:provider()
  if not provider then return end
  if self.catalog_key == provider then
    if on_loaded and self.catalog then on_loaded(self.catalog) end
    return
  end
  self.catalog_key, self.catalog = provider, nil
  self.revision = self.revision + 1
  local revision = self.revision
  client.request({ session_id = self.options.session_id, method = "session.agent.model.list", params = { provider = provider } },
    function(catalog, failure)
      if not self.options.current() or revision ~= self.revision then return end
      if not failure and (type(catalog) ~= "table" or type(catalog.models) ~= "table") then
        failure = "Invalid model catalog for " .. provider
      end
      if failure then
        self.failure = failure
        notifications.error(failure, "Background models")
      else
        self.failure, self.catalog = nil, catalog
        if on_loaded then on_loaded(catalog) end
      end
      self.options.changed()
    end)
end

---@return ForgeHarnessModel[]?
function BackgroundConfiguration:model_list()
  return self.catalog and self.catalog.models
end

---@return ForgeBackgroundModel?
function BackgroundConfiguration:model()
  local provider = self:provider()
  return provider and self.snapshot.settings.models[provider] or nil
end

function BackgroundConfiguration:descriptor()
  local selected = self:model()
  return model_options.find(self:model_list(), selected and selected.model)
end

---@return string[]
function BackgroundConfiguration:speed_values()
  return model_options.speeds(self:descriptor(), self.catalog and self.catalog.capability or {})
end

function BackgroundConfiguration:fields()
  local selected = self:model()
  local provider = self:provider()
  local disabled = self.pending and "Saving Background settings" or not self.snapshot and (self.failure or "Loading Background settings") or nil
  local catalog_disabled = disabled or not self.catalog and (self.failure or "Loading Background models") or nil
  local policy = self.snapshot and self.snapshot.settings.auto_compaction
  local fields = {
    provider = { id = "background-provider", kind = "choice", label = "Provider",
      value = provider and model_options.provider_label(provider) or "Loading…", disabled_reason = disabled },
    model = { id = "background-model", kind = "choice",
      value = selected and model_options.label(self:model_list(), selected.model) or (self.snapshot and "Choose a model" or "Loading…"),
      disabled_reason = catalog_disabled },
    thinking = { id = "background-thinking", kind = "choice", label = "Thinking", value = selected and selected.thinking or "Default",
      disabled_reason = catalog_disabled },
    compact = { id = "background-compact", kind = "toggle", label = "Compact", value = policy and policy.enabled == true,
      disabled_reason = disabled },
    threshold = { id = "background-threshold", kind = "choice", label = "Threshold", value = threshold.label(policy and policy.threshold_tokens or 128000),
      disabled_reason = disabled, muted = not (policy and policy.enabled) },
  }
  local capability = self.catalog and self.catalog.capability or {}
  if #self:speed_values() > 1 or (capability.fast_mode or capability.ultrafast_mode)
    and selected and selected.service_tier and selected.service_tier ~= "default" then
    local tier = selected and selected.service_tier or "default"
    local value = vim.tbl_contains(self:speed_values(), tier) and tier or (tier .. " (unsupported)")
    fields.speed = { id = "background-speed", kind = "choice", label = "Speed",
      value = value, disabled_reason = disabled }
  end
  return fields
end

---@param preferences ForgeBackgroundPreferences
---@param back? boolean
function BackgroundConfiguration:save(preferences, back)
  if self.pending then return end
  self.pending = true
  self.options.changed()
  store.request(self.snapshot, preferences, function(snapshot, failure)
    if not self.options.current() then return end
    self.pending = false
    if failure then
      notifications.error(failure, "Background settings")
      store.request(nil, nil, function(current, read_failure)
        if not self.options.current() then return end
        if read_failure then notifications.error(read_failure, "Background settings") end
        if current then self.snapshot = current self:load_models() end
        if back then self.options.reopen() else self.options.changed() end
      end)
      return
    end
    self.snapshot = snapshot
    self:load_models()
    if back then self.options.reopen() else self.options.changed() end
  end)
end

---@return ForgeBackgroundPreferences
function BackgroundConfiguration:preferences()
  return vim.deepcopy(self.snapshot.settings)
end

--- Save one model choice for `provider`, keeping only a speed the chosen model supports.
---@param preferences ForgeBackgroundPreferences
---@param provider string
---@param catalog ForgeBackgroundModelCatalog
---@param model string
---@param thinking? string
---@return ForgeBackgroundPreferences
local function with_model(preferences, provider, catalog, model, thinking)
  local choice = preferences.models[provider] or {}
  preferences.models[provider] = choice
  choice.model, choice.thinking = model, thinking
  local speeds = model_options.speeds(model_options.find(catalog.models, model), catalog.capability or {})
  if not vim.tbl_contains(speeds, choice.service_tier or "default") then choice.service_tier = "default" end
  return preferences
end

---@param model string
---@param thinking? string
---@param back? boolean
function BackgroundConfiguration:select_model(model, thinking, back)
  self:save(with_model(self:preferences(), self:provider(), self.catalog, model, thinking), back)
end

--- Switch the Background provider. A provider with a saved model keeps it; otherwise the
--- catalog's default model is chosen before saving, so the host never stores a provider
--- without a model to run.
---@param direction integer
function BackgroundConfiguration:change_provider(direction)
  local current = self:provider()
  local provider = model_options.cycle(model_options.providers(supports_background, current), current, direction)
  if not provider or provider == current then return end
  local preferences = self:preferences()
  preferences.provider = provider
  if preferences.models[provider] then self:save(preferences) return end
  self.pending = true
  self.options.changed()
  client.request({ session_id = self.options.session_id, method = "session.agent.model.list", params = { provider = provider } },
    function(catalog, failure)
      if not self.options.current() then return end
      self.pending = false
      local model = not failure and type(catalog) == "table"
        and (model_options.find(catalog.models, "default") or (catalog.models or {})[1]) or nil
      if not model then
        notifications.error(failure or ("No models available for " .. provider), "Background models")
        self.options.changed()
        return
      end
      self:save(with_model(preferences, provider, catalog, model.id, model.default_reasoning or (model.reasoning or {})[1]))
    end)
end

---@param id string
---@param direction integer
---@return boolean handled
function BackgroundConfiguration:change(id, direction)
  if not id:match("^background%-") then return false end
  if not self.snapshot or self.pending then return true end
  local model_list = self:model_list()
  if id == "background-provider" then
    self:change_provider(direction)
  elseif id == "background-compact" then
    local preferences = self:preferences()
    preferences.auto_compaction.enabled = not preferences.auto_compaction.enabled
    self:save(preferences)
  elseif id == "background-threshold" then
    local preferences = self:preferences()
    preferences.auto_compaction.threshold_tokens = threshold.change(preferences.auto_compaction.threshold_tokens, direction)
    self:save(preferences)
  elseif id == "background-model" and model_list and #model_list > 0 then
    local selected = self:model()
    local ids = vim.tbl_map(function(model) return model.id end, model_list)
    local model = model_options.find(model_list, model_options.cycle(ids, selected and selected.model, direction))
    self:select_model(model.id, model.default_reasoning or model.reasoning and model.reasoning[1])
  elseif id == "background-thinking" then
    local model = self:descriptor()
    local values = model and model.reasoning or {}
    if #values == 0 then return true end
    self:select_model(model.id, model_options.cycle(values, self:model().thinking, direction))
  elseif id == "background-speed" then
    local values = self:speed_values()
    if #values == 1 then return true end
    local preferences = self:preferences()
    local choice = preferences.models[self:provider()]
    choice.service_tier = model_options.cycle(values, choice.service_tier or "default", direction)
    self:save(preferences)
  end
  return true
end

---@param id string
---@return boolean handled
function BackgroundConfiguration:open(id)
  if not id:match("^background%-") then return false end
  if not self.snapshot or self.pending then return true end
  if id == "background-provider" then return self:change(id, 1) end
  if id == "background-model" then
    local model_list = self:model_list()
    if not model_list or #model_list == 0 then return true end
    local selected = self:model()
    local models = vim.deepcopy(model_list)
    for _, model in ipairs(models) do
      model.context_window = {}
      if selected and model.id == selected.model then model.selected_reasoning = tostring(selected.thinking) end
    end
    picker.close(false)
    require("forge.views.harness.model_picker").open({ host = self.options.host, model_list = models,
      provider = self:provider(),
      current_model = selected and selected.model, is_current = self.options.current, on_close = self.options.reopen,
      on_confirm = function(value)
        picker.close(false)
        self:select_model(value.model, value.effort, true)
      end })
    return true
  end
  if id == "background-compact" or id == "background-speed" then return self:change(id, 1) end
  if id == "background-threshold" then
    threshold.open({ host = self.options.host, value = self.snapshot.settings.auto_compaction.threshold_tokens,
      on_close = self.options.reopen, on_select = function(value)
        if not self.options.current() then return end
        local preferences = self:preferences()
        preferences.auto_compaction.threshold_tokens = value
        self:save(preferences, true)
      end })
    return true
  end
  local options = {}
  local model = self:descriptor()
  local selected = self:model()
  for _, value in ipairs(model and model.reasoning or {}) do options[#options + 1] = { id = value, label = value } end
  if #options == 0 then return true end
  local selected_index = 1
  for index, option in ipairs(options) do if selected and option.id == tostring(selected.thinking) then selected_index = index end end
  picker.close(false)
  picker.open({ owner = "harness-" .. id, host = self.options.host,
    page_list = { { id = id, title = "Background thinking", option_list = options, selected_index = selected_index, footer = "Enter select  q back" } },
    on_close = self.options.reopen, on_confirm = function(result)
      picker.close(false)
      self:select_model(model.id, result.option.id, true)
    end })
  return true
end

return BackgroundConfiguration
