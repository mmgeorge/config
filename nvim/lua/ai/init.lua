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

---@class AIToolSpec normalized tool definition sent to the provider
---@field name string
---@field description string
---@field parameters table JSON Schema for the arguments object

---@class AITool: AIToolSpec a callable tool offered to generate_with_tools
---@field run fun(args: table, done: fun(output: string)) async; must call done exactly once

---@class AIToolCall a call requested by the model
---@field id? string provider call id, echoed back with the result
---@field name string
---@field args table decoded arguments
---@field args_error? string set when the provider could not decode the arguments

---@class AIToolResult
---@field call AIToolCall
---@field output string

---@class AIGenerateRequest
---@field system? string
---@field prompt string
---@field temperature? number
---@field max_output_tokens? integer
---@field tools? AIToolSpec[]
---@field history? any provider-opaque conversation state from the previous round
---@field tool_results? AIToolResult[] results for the calls the provider returned last round

---@class AIGenerateOpts: AIGenerateRequest
---@field model string model token, e.g. "gemini3-lite,thinking=none"

---@class AIGenerateWithToolsOpts: AIGenerateOpts
---@field tools? (AITool|string)[] inline tool definitions and/or registry keys from ai/tools.lua
---@field mcps? string[] mcphub server names whose tools are offered to the model
---@field max_rounds? integer maximum model requests before aborting (default 8)

---@class AIGenerateResult
---@field ok boolean
---@field content? string
---@field error? string
---@field tool_calls? AIToolCall[] internal: set by providers when the model requested tool calls
---@field history? any internal: provider conversation state to continue from

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

--- Like generate, but lets the model call tools. Native tools come from
--- opts.tools; opts.mcps additionally offers the tools of the named mcphub
--- servers. The loop runs until the model answers with text, an error
--- occurs, or max_rounds model requests have been made. Tool failures
--- (unknown name, bad arguments, run errors) are fed back to the model as
--- the tool output so it can recover.
---@param opts AIGenerateWithToolsOpts
---@param cb fun(result: AIGenerateResult)
function M.generate_with_tools(opts, cb)
  local done = false
  local function finish(result)
    if done then return end
    done = true
    vim.schedule(function() cb(result) end)
  end

  if type(opts.prompt) ~= "string" or vim.trim(opts.prompt) == "" then
    finish({ ok = false, error = "prompt is required" })
    return
  end
  local spec, resolve_err = M.resolve(opts.model or "")
  if not spec then
    finish({ ok = false, error = resolve_err })
    return
  end

  ---@type AIToolSpec[]
  local tool_specs = {}
  ---@type table<string, AITool>
  local tools_by_name = {}

  ---@param tool AITool
  ---@return string? error
  local function add_tool(tool)
    if type(tool.name) ~= "string" or not tool.name:match("^[%w_-]+$") then
      return ("invalid tool name %q (use letters, digits, _ or -)"):format(tostring(tool.name))
    end
    if type(tool.run) ~= "function" then
      return ("tool %q is missing a run function"):format(tool.name)
    end
    if tools_by_name[tool.name] then
      return ("duplicate tool name %q"):format(tool.name)
    end
    tools_by_name[tool.name] = tool
    tool_specs[#tool_specs + 1] = {
      name = tool.name,
      description = tool.description or "",
      parameters = tool.parameters or { type = "object", properties = vim.empty_dict() },
    }
    return nil
  end

  for _, entry in ipairs(opts.tools or {}) do
    ---@type AITool
    local tool
    if type(entry) == "string" then
      local registered = require("ai.tools").get(entry)
      if not registered then
        finish({ ok = false, error = ("unknown registered tool %q (see ai/tools.lua)"):format(entry) })
        return
      end
      tool = registered
    else
      tool = entry --[[@as AITool]]
    end
    local add_err = add_tool(tool)
    if add_err then
      finish({ ok = false, error = add_err })
      return
    end
  end
  if opts.mcps and #opts.mcps > 0 then
    local mcp_tools, mcp_err = require("ai.mcp").tools(opts.mcps)
    if not mcp_tools then
      finish({ ok = false, error = mcp_err })
      return
    end
    for _, tool in ipairs(mcp_tools) do
      local add_err = add_tool(tool)
      if add_err then
        finish({ ok = false, error = add_err })
        return
      end
    end
  end

  local ctx = request_context()
  local max_rounds = opts.max_rounds or 8
  local rounds = 0
  local history = nil

  local run_round

  --- Execute the requested calls sequentially, then hand the results to the
  --- next round.
  ---@param calls AIToolCall[]
  ---@param index integer
  ---@param results AIToolResult[]
  local function run_calls(calls, index, results)
    if index > #calls then
      run_round(results)
      return
    end
    local call = calls[index]
    local completed = false
    local function complete(output)
      if completed then return end
      completed = true
      results[#results + 1] = { call = call, output = tostring(output) }
      run_calls(calls, index + 1, results)
    end

    if call.args_error then
      complete("Error: " .. call.args_error)
      return
    end
    local tool = tools_by_name[call.name]
    if not tool then
      complete(("Error: unknown tool %q"):format(call.name))
      return
    end
    local ok_run, run_err = pcall(tool.run, call.args, function(output)
      vim.schedule(function() complete(output) end)
    end)
    if not ok_run then
      complete("Error: " .. tostring(run_err))
    end
  end

  ---@param tool_results AIToolResult[]?
  run_round = function(tool_results)
    rounds = rounds + 1
    spec.provider.generate(ctx, spec, {
      system = opts.system,
      prompt = opts.prompt,
      temperature = opts.temperature,
      max_output_tokens = opts.max_output_tokens,
      tools = #tool_specs > 0 and tool_specs or nil,
      history = history,
      tool_results = tool_results,
    }, function(result)
      if not result.ok then
        finish(result)
        return
      end
      if result.tool_calls and #result.tool_calls > 0 then
        if rounds >= max_rounds then
          finish({ ok = false, error = ("tool rounds exhausted after %d model requests"):format(rounds) })
          return
        end
        history = result.history
        run_calls(result.tool_calls, 1, {})
        return
      end
      finish({ ok = true, content = result.content })
    end)
  end

  run_round(nil)
end

return M
