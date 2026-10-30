local BackgroundConfiguration = {}
BackgroundConfiguration.__index = BackgroundConfiguration
local store = require("forge.integrations.background_settings")
local client = require("forge.client")
local picker = require("forge.views.picker")
local notifications = require("forge.infra.notifications")
local model_options = require("forge.views.harness.model_options")

---@class ForgeBackgroundConfigurationOptions
---@field backend string
---@field session_id string
---@field host table
---@field capability fun(): table
---@field current fun(): boolean
---@field changed fun()
---@field reopen fun()

---@class ForgeBackgroundConfiguration
---@field options ForgeBackgroundConfigurationOptions
---@field snapshot? ForgeBackgroundSettings
---@field model_list? ForgeHarnessModel[]
---@field catalog_key? string
---@field pending boolean
---@field failure? string
---@field revision integer
---@field fields fun(self: ForgeBackgroundConfiguration): table<string, ForgePickerField>
---@field change fun(self: ForgeBackgroundConfiguration, id: string, direction: integer): boolean
---@field open fun(self: ForgeBackgroundConfiguration, id: string): boolean
---@field load_models fun(self: ForgeBackgroundConfiguration)
---@field model fun(self: ForgeBackgroundConfiguration): ForgeBackgroundModel?
---@field descriptor fun(self: ForgeBackgroundConfiguration): table?
---@field save fun(self: ForgeBackgroundConfiguration, preferences: ForgeBackgroundPreferences, back?: boolean)
---@field preferences fun(self: ForgeBackgroundConfiguration): ForgeBackgroundPreferences
---@field select_model fun(self: ForgeBackgroundConfiguration, model: string, thinking?: string, back?: boolean)
---@field speed_values fun(self: ForgeBackgroundConfiguration): string[]

function BackgroundConfiguration:speed_values()
  return model_options.speeds(self:descriptor(), self.options.capability())
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

function BackgroundConfiguration:load_models()
  if not self.snapshot then return end
  local key = self.options.backend
  if self.catalog_key == key then return end
  self.catalog_key, self.model_list = key, nil
  self.revision = self.revision + 1
  local revision = self.revision
  local function accept(models, failure)
    if not self.options.current() or revision ~= self.revision then return end
    if failure then
      self.failure = failure
      notifications.error(failure, "Background models")
    else
      self.failure, self.model_list = nil, models
    end
    self.options.changed()
  end
  client.request({ session_id = self.options.session_id, method = "session.provider.model.list", params = {} }, accept)
end

function BackgroundConfiguration:model()
  if not self.snapshot then return end
  return store.model(self.snapshot, self.options.backend)
end

function BackgroundConfiguration:descriptor()
  local selected = self:model()
  return model_options.find(self.model_list, selected and selected.model)
end

function BackgroundConfiguration:fields()
  local selected = self:model()
  local disabled = self.pending and "Saving Background settings" or not self.snapshot and (self.failure or "Loading Background settings") or nil
  local catalog_disabled = disabled or not self.model_list and (self.failure or "Loading Background models") or nil
  local policy = self.snapshot and self.snapshot.settings.auto_compaction
  local fields = {
    model = { id = "background-model", kind = "choice", value = selected and selected.model or "Loading…",
      disabled_reason = catalog_disabled },
    thinking = { id = "background-thinking", kind = "choice", label = "Thinking", value = selected and selected.thinking or "Default",
      disabled_reason = catalog_disabled },
    compact = { id = "background-compact", kind = "toggle", label = "Automatic", value = policy and policy.enabled == true,
      disabled_reason = disabled },
    threshold = { id = "background-threshold", kind = "input", label = "Threshold", value = tostring(policy and policy.threshold_tokens or 128000),
      disabled_reason = disabled, muted = not (policy and policy.enabled) },
  }
  local capability = self.options.capability()
  if #self:speed_values() > 1 or (capability.fast_mode or capability.ultrafast_mode)
    and selected and selected.service_tier and selected.service_tier ~= "default" then
    local tier = selected and selected.service_tier or "default"
    local value = vim.tbl_contains(self:speed_values(), tier) and tier or (tier .. " (unsupported)")
    fields.speed = { id = "background-speed", kind = "choice", label = "Speed",
      value = value, disabled_reason = disabled }
  end
  return fields
end

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

function BackgroundConfiguration:preferences()
  local preferences = vim.deepcopy(self.snapshot.settings)
  return preferences
end

function BackgroundConfiguration:select_model(model, thinking, back)
  local preferences = self:preferences()
  local key = self.options.backend
  preferences.models[key] = preferences.models[key] or {}
  preferences.models[key].model = model
  preferences.models[key].thinking = thinking
  local speeds = model_options.speeds(model_options.find(self.model_list, model), self.options.capability())
  if not vim.tbl_contains(speeds, preferences.models[key].service_tier or "default") then
    preferences.models[key].service_tier = "default"
  end
  self:save(preferences, back)
end

function BackgroundConfiguration:change(id, direction)
  if not id:match("^background%-") then return false end
  if not self.snapshot or self.pending then return true end
  if id == "background-compact" then
    local preferences = self:preferences()
    preferences.auto_compaction.enabled = not preferences.auto_compaction.enabled
    self:save(preferences)
  elseif id == "background-threshold" then
    local preferences = self:preferences()
    preferences.auto_compaction.threshold_tokens = math.max(1, preferences.auto_compaction.threshold_tokens + direction)
    self:save(preferences)
  elseif id == "background-model" and self.model_list and #self.model_list > 0 then
    local position = 1
    for index, model in ipairs(self.model_list) do if model.id == self:model().model then position = index end end
    local model = self.model_list[(position - 1 + direction) % #self.model_list + 1]
    self:select_model(model.id, model.default_reasoning or model.reasoning and model.reasoning[1])
  elseif id == "background-thinking" then
    local model = self:descriptor()
    local values = model and model.reasoning or {}
    if #values == 0 then return true end
    local position = math.max(1, vim.fn.index(values, tostring(self:model().thinking)) + 1)
    self:select_model(model.id, values[(position - 1 + direction) % #values + 1])
  elseif id == "background-speed" then
    local values = self:speed_values()
    if #values == 1 then return true end
    local preferences = self:preferences()
    local choice = preferences.models[self.options.backend]
    local position = math.max(1, vim.fn.index(values, choice.service_tier or "default") + 1)
    choice.service_tier = values[(position - 1 + direction) % #values + 1]
    self:save(preferences)
  end
  return true
end

function BackgroundConfiguration:open(id)
  if not id:match("^background%-") then return false end
  if not self.snapshot or self.pending then return true end
  if id == "background-model" then
    if not self.model_list or #self.model_list == 0 then return true end
    local models = vim.deepcopy(self.model_list)
    for _, model in ipairs(models) do
      model.context_window = {}
      if model.id == self:model().model then model.selected_reasoning = tostring(self:model().thinking) end
    end
    picker.close(false)
    require("forge.views.harness.model_picker").open({ host = self.options.host, model_list = models,
      backend = self.options.backend,
      current_model = self:model().model, is_current = self.options.current, on_close = self.options.reopen,
      on_confirm = function(value)
        picker.close(false)
        self:select_model(value.model, value.effort, true)
      end })
    return true
  end
  if id == "background-compact" or id == "background-speed" then return self:change(id, 1) end
  if id == "background-threshold" then
    vim.ui.input({ prompt = "Compact at tokens: ", default = tostring(self.snapshot.settings.auto_compaction.threshold_tokens) }, function(value)
      if not value or not self.options.current() then return end
      local amount = tonumber(value)
      if not amount or amount <= 0 or amount % 1 ~= 0 then notifications.error("Enter a positive whole token count", "Background settings") return end
      local preferences = self:preferences()
      preferences.auto_compaction.threshold_tokens = amount
      self:save(preferences)
    end)
    return true
  end
  local options = {}
  local model = self:descriptor()
  for _, value in ipairs(model and model.reasoning or {}) do options[#options + 1] = { id = value, label = value } end
  if #options == 0 then return true end
  local selected_index = 1
  for index, option in ipairs(options) do if option.id == tostring(self:model().thinking) then selected_index = index end end
  picker.close(false)
  picker.open({ owner = "harness-" .. id, host = self.options.host,
    page_list = { { id = id, title = "Background thinking", option_list = options, selected_index = selected_index, footer = "Enter select  q back" } },
    on_close = self.options.reopen, on_confirm = function(result)
      picker.close(false)
      self:select_model(self:model().model, result.option.id, true)
    end })
  return true
end

return BackgroundConfiguration
