local M = {}
local client = require("forge.client")
local config = require("forge.infra.config")

---@class ForgeBackgroundModel
---@field model string
---@field thinking? string
---@field service_tier? 'default'|'fast'|'ultrafast'

---@class ForgeBackgroundPreferences
---@field models table<string, ForgeBackgroundModel>
---@field auto_compaction {enabled: boolean, threshold_tokens: integer}

---@class ForgeBackgroundSettings
---@field revision integer
---@field settings ForgeBackgroundPreferences

---@return ForgeBackgroundPreferences
local function defaults()
  local models = {}
  for provider, choice in pairs(config.options.background_generation or {}) do
    models[provider] = { model = choice.model, thinking = choice.reasoning_effort, service_tier = choice.service_tier or "default" }
  end
  return { models = models, auto_compaction = { enabled = true, threshold_tokens = 128000 } }
end

--- Read or save one revision of globally shared Background preferences.
---@param snapshot ForgeBackgroundSettings?
---@param preferences ForgeBackgroundPreferences?
---@param callback fun(snapshot: ForgeBackgroundSettings?, failure: string?)
function M.request(snapshot, preferences, callback)
  local generation, failure = client.generation_config()
  if not generation then callback(nil, failure) return end
  local baseline = defaults()
  baseline.models[generation.backend.kind] = baseline.models[generation.backend.kind]
    or { model = generation.model, thinking = generation.reasoning_effort, service_tier = generation.service_tier }
  client.request_host("background.settings", {
    data_root = generation.data_root, defaults = baseline,
    expected_revision = snapshot and snapshot.revision, settings = preferences,
  }, function(result, request_failure)
    if request_failure then callback(nil, request_failure)
    elseif type(result) ~= "table" or type(result.settings) ~= "table" then callback(nil, "Invalid Background settings response")
    else
      for _, choice in pairs(result.settings.models) do
        if choice.thinking == vim.NIL then choice.thinking = nil end
      end
      callback(result)
    end
  end)
end

---@param snapshot ForgeBackgroundSettings
---@param backend string
---@return ForgeBackgroundModel
function M.model(snapshot, backend)
  return assert(snapshot.settings.models[backend],
    "Background settings omitted the selected provider model")
end

return M
