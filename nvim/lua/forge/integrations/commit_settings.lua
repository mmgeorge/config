local M = {}
local client = require("forge.client")
local config = require("forge.infra.config")

---@class ForgeCommitModel
---@field model string
---@field thinking? string|integer

---@class ForgeCommitPreferences
---@field source "gemini"|"harness"
---@field models table<string, ForgeCommitModel>

---@class ForgeCommitSettings
---@field revision integer
---@field settings ForgeCommitPreferences
---@field effective_source "gemini"|"harness"
---@field gemini_available boolean

---@return ForgeCommitPreferences
local function defaults()
  local models = {}
  for provider, choice in pairs(config.options.commit_generation or {}) do
    models[provider] = { model = choice.model, thinking = choice.reasoning_effort }
  end
  local preset = require("ai.providers.gemini").presets["gemini3-lite"]
  models.gemini = { model = preset.model, thinking = preset.thinking.minimal }
  return { source = require("ai.adapters").use_gemini_for_commits and "gemini" or "harness", models = models }
end

--- Read or save one revision of globally shared commit preferences.
---@param snapshot ForgeCommitSettings?
---@param preferences ForgeCommitPreferences?
---@param callback fun(snapshot: ForgeCommitSettings?, failure: string?)
function M.request(snapshot, preferences, callback)
  local generation, failure = client.generation_config()
  if not generation then callback(nil, failure) return end
  local baseline = defaults()
  baseline.models[generation.backend.kind] = baseline.models[generation.backend.kind]
    or { model = generation.model, thinking = generation.reasoning_effort }
  client.request_host("repository.generation_settings", {
    data_root = generation.data_root, defaults = baseline,
    expected_revision = snapshot and snapshot.revision, settings = preferences,
  }, function(result, request_failure)
    if request_failure then callback(nil, request_failure)
    elseif type(result) ~= "table" or type(result.settings) ~= "table" then callback(nil, "Invalid commit settings response")
    else
      for _, choice in pairs(result.settings.models) do
        if choice.thinking == vim.NIL then choice.thinking = nil end
      end
      callback(result)
    end
  end)
end

---@param snapshot ForgeCommitSettings
---@param backend string
---@return ForgeCommitModel
function M.model(snapshot, backend)
  return assert(snapshot.settings.models[snapshot.effective_source == "gemini" and "gemini" or backend],
    "Commit settings omitted the selected provider model")
end

return M
