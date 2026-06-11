-- Generic one-shot prompt library: send a system + user prompt to an LLM
-- provider, get the response text back via an async callback. See AGENTS.md
-- in this directory for the model token grammar and provider details.

---@alias AIThinkingLevel "minimal"|"low"|"medium"|"high"|"max"

---@class AIPreset
---@field model string raw provider model id
---@field thinking? table<AIThinkingLevel, string|integer> thinking level -> provider request value; levels missing from the map (or the whole map) are accepted but send nothing

---@class AIProvider
---@field name string
---@field presets table<string, AIPreset>
---@field generate fun(ctx: AIRequestContext, spec: AIModelSpec, request: AIGenerateRequest, cb: fun(result: AIGenerateResult))
---@field reset? fun()

---@class AIModelSpec resolved from a model token
---@field provider AIProvider
---@field preset_name string
---@field model string
---@field thinking? string|integer provider-specific thinking value; nil when the preset has no mapping for the requested level

---@class AIRequestContext
---@field request fun(request: AIHttpRequest, cb: AIHttpCallback)
---@field read_file fun(path: string): string?
---@field now fun(): integer

---@class AIGenerateRequest
---@field system? string
---@field prompt string
---@field temperature? number
---@field max_output_tokens? integer

---@class AIGenerateOpts: AIGenerateRequest
---@field model string model token, e.g. "gemini3-lite,thinking=none"

---@class AIGenerateResult
---@field ok boolean
---@field content? string
---@field error? string

---@class AIBackend test seam; any omitted method falls back to the default
---@field request? fun(request: AIHttpRequest, cb: AIHttpCallback)
---@field read_file? fun(path: string): string?
---@field now? fun(): integer

---@class AIModule
---@field _backend AIBackend?
local M = {}

---@type AIProvider[]
local providers = {
  require("ai.providers.gemini"),
  require("ai.providers.openai"),
  require("ai.providers.copilot"),
}

---@type table<string, AIProvider>
local preset_index = {}
for _, provider in ipairs(providers) do
  for preset_name in pairs(provider.presets) do
    preset_index[preset_name] = provider
  end
end

local function reset_providers()
  for _, provider in ipairs(providers) do
    if provider.reset then provider.reset() end
  end
end

---@param backend AIBackend?
function M.set_backend(backend)
  M._backend = backend
  reset_providers()
end

function M.reset_backend()
  M._backend = nil
  reset_providers()
end

---@param path string
---@return string? contents
local function read_file(path)
  local file = io.open(path, "rb")
  if not file then return nil end
  local data = file:read("*a")
  file:close()
  return data
end

---@param map table<string, any>
---@return string
local function sorted_keys(map)
  local keys = {}
  for key in pairs(map) do keys[#keys + 1] = key end
  table.sort(keys)
  return table.concat(keys, ", ")
end

--- The end-user thinking vocabulary. Every preset accepts every level;
--- depending on the provider a level may send nothing or map to the same
--- request value as a neighboring level.
---@type table<AIThinkingLevel, true>
local thinking_levels = { minimal = true, low = true, medium = true, high = true, max = true }

--- Resolve a model token such as "gemini3,thinking=minimal" or
--- "copilot,model=gpt-5-mini" into a request spec.
---@param token string
---@return AIModelSpec? spec
---@return string? error
function M.resolve(token)
  local parts = vim.split(token, ",", { plain = true })
  local preset_name = vim.trim(parts[1] or "")
  local provider = preset_index[preset_name]
  if not provider then
    return nil, ("unknown model preset %q (available: %s)"):format(preset_name, sorted_keys(preset_index))
  end
  local preset = provider.presets[preset_name]

  ---@type AIModelSpec
  local spec = {
    provider = provider,
    preset_name = preset_name,
    model = preset.model,
  }

  for index = 2, #parts do
    local option = vim.trim(parts[index])
    local key, value = option:match("^([%w_]+)=(.+)$")
    if not key then
      return nil, ("invalid model option %q (expected key=value)"):format(option)
    end
    if key == "model" then
      spec.model = value
    elseif key == "thinking" then
      if not thinking_levels[value] then
        return nil, ("unknown thinking level %q (available: %s)"):format(value, sorted_keys(thinking_levels))
      end
      if preset.thinking then
        spec.thinking = preset.thinking[value]
      end
    else
      return nil, ("unknown model option %q (available: model, thinking)"):format(key)
    end
  end

  return spec
end

---@return AIRequestContext
local function request_context()
  local backend = M._backend or {}
  return {
    request = backend.request or require("ai.http").request,
    read_file = backend.read_file or read_file,
    now = backend.now or os.time,
  }
end

--- Send one prompt and receive the response text asynchronously. The callback
--- always runs on the main loop.
---@param opts AIGenerateOpts
---@param cb fun(result: AIGenerateResult)
function M.generate(opts, cb)
  local function finish(result)
    vim.schedule(function() cb(result) end)
  end

  if type(opts.prompt) ~= "string" or vim.trim(opts.prompt) == "" then
    finish({ ok = false, error = "prompt is required" })
    return
  end
  local spec, err = M.resolve(opts.model or "")
  if not spec then
    finish({ ok = false, error = err })
    return
  end

  spec.provider.generate(request_context(), spec, {
    system = opts.system,
    prompt = opts.prompt,
    temperature = opts.temperature,
    max_output_tokens = opts.max_output_tokens,
  }, finish)
end

return M
