local CommitConfiguration = {}
CommitConfiguration.__index = CommitConfiguration
local store = require("forge.integrations.commit_settings")
local client = require("forge.client")
local picker = require("forge.views.picker")
local field = require("forge.views.picker.field")
local notifications = require("forge.infra.notifications")

---@class ForgeCommitConfigurationOptions
---@field backend string
---@field session_id string
---@field host table
---@field current fun(): boolean
---@field changed fun()
---@field reopen fun()

---@class ForgeCommitConfiguration
---@field options ForgeCommitConfigurationOptions
---@field snapshot? ForgeCommitSettings
---@field model_list? ForgeHarnessModel[]
---@field catalog_key? string
---@field pending boolean
---@field failure? string
---@field revision integer
---@field rows fun(self: ForgeCommitConfiguration): table[]
---@field change fun(self: ForgeCommitConfiguration, id: string, direction: integer): boolean
---@field open fun(self: ForgeCommitConfiguration, id: string): boolean
---@field load_models fun(self: ForgeCommitConfiguration)
---@field model fun(self: ForgeCommitConfiguration): ForgeCommitModel?
---@field descriptor fun(self: ForgeCommitConfiguration): table?
---@field save fun(self: ForgeCommitConfiguration, preferences: ForgeCommitPreferences, back?: boolean)
---@field preferences fun(self: ForgeCommitConfiguration): ForgeCommitPreferences
---@field select_model fun(self: ForgeCommitConfiguration, model: string, thinking?: string, back?: boolean)

---@param options ForgeCommitConfigurationOptions
---@return ForgeCommitConfiguration
function CommitConfiguration.new(options)
  local self = setmetatable({ options = options, pending = false, revision = 0 }, CommitConfiguration)
  store.request(nil, nil, function(snapshot, failure)
    if not options.current() then return end
    self.snapshot, self.failure = snapshot, failure
    if failure then notifications.error(failure, "Commit settings") end
    self:load_models()
    options.changed()
  end)
  return self
end

function CommitConfiguration:load_models()
  if not self.snapshot then return end
  local key = self.snapshot.effective_source == "gemini" and "gemini" or self.options.backend
  if self.catalog_key == key then return end
  self.catalog_key, self.model_list = key, nil
  self.revision = self.revision + 1
  local revision = self.revision
  local function accept(models, failure)
    if not self.options.current() or revision ~= self.revision then return end
    if failure then
      self.failure = failure
      notifications.error(failure, "Commit models")
    else
      self.failure, self.model_list = nil, models
    end
    self.options.changed()
  end
  if key ~= "gemini" then
    client.request_for(self.options.session_id, "backend.models", {}, accept)
    return
  end
  local models = {}
  for name, preset in pairs(require("ai.providers.gemini").presets) do
    local reasoning, seen = {}, {}
    for _, level in ipairs({ "minimal", "low", "medium", "high", "max" }) do
      local value = tostring(preset.thinking[level])
      if not seen[value] then reasoning[#reasoning + 1], seen[value] = value, true end
    end
    models[#models + 1] = { id = preset.model, description = name,
      reasoning = reasoning, default_reasoning = tostring(preset.thinking.minimal) }
  end
  table.sort(models, function(left, right) return left.id < right.id end)
  accept(models)
end

function CommitConfiguration:model()
  if not self.snapshot then return end
  return store.model(self.snapshot, self.options.backend)
end

function CommitConfiguration:descriptor()
  local selected = self:model()
  for _, model in ipairs(self.model_list or {}) do
    if selected and model.id == selected.model then return model end
  end
end

function CommitConfiguration:rows()
  local selected = self:model()
  local source = self.snapshot and self.snapshot.effective_source
  local disabled = self.pending and "Saving commit settings" or not self.snapshot and (self.failure or "Loading commit settings") or nil
  local catalog_disabled = disabled or not self.model_list and (self.failure or "Loading commit models") or nil
  return {
    { id = "commit-provider", label = "Commit provider", disabled_reason = disabled,
      columns = { "Commit provider", field.render(source == "gemini" and "Gemini" or "Harness", true,
        { selectable = self.snapshot and self.snapshot.gemini_available }),
        self.snapshot and not self.snapshot.gemini_available and "Gemini unavailable: GEMINI_API_KEY is not set" or "Independent, tool-free commit generation" } },
    { id = "commit-model", label = "Commit model", disabled_reason = catalog_disabled,
      columns = { "Commit model", field.render(selected and selected.model or "Loading…", true),
        source == "gemini" and "Gemini API model" or (self.options.backend .. " commit model · independent of chat") } },
    { id = "commit-thinking", label = "Commit thinking", disabled_reason = catalog_disabled,
      columns = { "Commit thinking", field.render(selected and selected.thinking and tostring(selected.thinking) or "Default", true),
        "Reasoning effort for commit generation" } },
  }
end

function CommitConfiguration:save(preferences, back)
  if self.pending then return end
  self.pending = true
  self.options.changed()
  store.request(self.snapshot, preferences, function(snapshot, failure)
    if not self.options.current() then return end
    self.pending = false
    if failure then
      notifications.error(failure, "Commit settings")
      store.request(nil, nil, function(current, read_failure)
        if not self.options.current() then return end
        if read_failure then notifications.error(read_failure, "Commit settings") end
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

function CommitConfiguration:preferences()
  local preferences = vim.deepcopy(self.snapshot.settings)
  preferences.source = self.snapshot.effective_source
  return preferences
end

function CommitConfiguration:select_model(model, thinking, back)
  local preferences = self:preferences()
  local key = preferences.source == "gemini" and "gemini" or self.options.backend
  preferences.models[key] = { model = model, thinking = key == "gemini" and tonumber(thinking) or thinking }
  self:save(preferences, back)
end

function CommitConfiguration:change(id, direction)
  if not id:match("^commit%-") then return false end
  if not self.snapshot or self.pending then return true end
  if id == "commit-provider" then
    if not self.snapshot.gemini_available then return true end
    local preferences = self:preferences()
    preferences.source = preferences.source == "gemini" and "harness" or "gemini"
    self:save(preferences)
  elseif id == "commit-model" and self.model_list and #self.model_list > 0 then
    local position = 1
    for index, model in ipairs(self.model_list) do if model.id == self:model().model then position = index end end
    local model = self.model_list[(position - 1 + direction) % #self.model_list + 1]
    self:select_model(model.id, model.default_reasoning or model.reasoning and model.reasoning[1])
  elseif id == "commit-thinking" then
    local model = self:descriptor()
    local values = model and model.reasoning or {}
    if #values == 0 then return true end
    local position = math.max(1, vim.fn.index(values, tostring(self:model().thinking)) + 1)
    self:select_model(model.id, values[(position - 1 + direction) % #values + 1])
  end
  return true
end

function CommitConfiguration:open(id)
  if not id:match("^commit%-") then return false end
  if not self.snapshot or self.pending then return true end
  if id == "commit-model" then
    if not self.model_list or #self.model_list == 0 then return true end
    local models = vim.deepcopy(self.model_list)
    for _, model in ipairs(models) do
      model.context_window = {}
      if model.id == self:model().model then model.selected_reasoning = tostring(self:model().thinking) end
    end
    picker.close(false)
    require("forge.views.harness.model_picker").open({ host = self.options.host, model_list = models,
      backend = self.snapshot.effective_source == "gemini" and "gemini" or self.options.backend,
      current_model = self:model().model, is_current = self.options.current, on_close = self.options.reopen,
      on_confirm = function(value)
        picker.close(false)
        self:select_model(value.model, value.effort, true)
      end })
    return true
  end
  local options = {}
  if id == "commit-provider" then
    options = { { id = "harness", label = "Harness" }, { id = "gemini", label = "Gemini",
      disabled_reason = not self.snapshot.gemini_available and "GEMINI_API_KEY is not set" or nil } }
  else
    local model = self:descriptor()
    for _, value in ipairs(model and model.reasoning or {}) do options[#options + 1] = { id = value, label = value } end
    if #options == 0 then return true end
  end
  local selected_index = 1
  local selected = id == "commit-provider" and self.snapshot.effective_source or tostring(self:model().thinking)
  for index, option in ipairs(options) do if option.id == selected then selected_index = index end end
  picker.close(false)
  picker.open({ owner = "harness-" .. id, host = self.options.host,
    page_list = { { id = id, title = id == "commit-provider" and "Commit provider" or "Commit thinking",
      option_list = options, selected_index = selected_index, footer = "Enter select  q back" } }, on_close = self.options.reopen,
    on_confirm = function(result)
      picker.close(false)
      if id == "commit-provider" then
        local preferences = self:preferences()
        preferences.source = result.option.id
        self:save(preferences, true)
      else self:select_model(self:model().model, result.option.id, true) end
    end })
  return true
end

return CommitConfiguration
