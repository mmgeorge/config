-- Google Gemini provider (Generative Language API).
--
-- Docs: https://ai.google.dev/api/generate-content
--       https://ai.google.dev/gemini-api/docs/thinking
-- Auth: GEMINI_API_KEY sent via the x-goog-api-key header.
--
-- Thinking control differs per model generation: Gemini 3 models take
-- generationConfig.thinkingConfig.thinkingLevel ("minimal".."high", where
-- "minimal" is the closest to off), Gemini 2.5 models take
-- thinkingConfig.thinkingBudget (token count, 0 disables). Presets encode the
-- distinction by value type: string values map to thinkingLevel, numbers to
-- thinkingBudget.

local http = require("ai.http")

---@class AIGeminiProvider: AIProvider
local M = {
  name = "gemini",
}

-- Gemini 3 has no thinking level above "high", so max aliases high. Gemini
-- 2.5 budgets: 0 disables thinking outright; 24576 is the documented ceiling
-- for the flash models, so high and max alias.
---@type table<string, AIPreset>
M.presets = {
  ["gemini3"] = {
    model = "gemini-3.5-flash",
    thinking = { minimal = "minimal", low = "low", medium = "medium", high = "high", max = "high" },
  },
  ["gemini3-lite"] = {
    model = "gemini-3.1-flash-lite",
    thinking = { minimal = "minimal", low = "low", medium = "medium", high = "high", max = "high" },
  },
  ["gemini2.5"] = {
    model = "gemini-2.5-flash",
    thinking = { minimal = 0, low = 1024, medium = 8192, high = 24576, max = 24576 },
  },
  ["gemini2.5-lite"] = {
    model = "gemini-2.5-flash-lite",
    thinking = { minimal = 0, low = 1024, medium = 8192, high = 24576, max = 24576 },
  },
}

---@param candidate table?
---@return string? content
local function candidate_text(candidate)
  local content = candidate and candidate.content
  local parts = content and content.parts or {}
  local texts = {}
  for _, part in ipairs(parts) do
    if type(part.text) == "string" and not part.thought then
      texts[#texts + 1] = part.text
    end
  end
  if #texts == 0 then return nil end
  return vim.trim(table.concat(texts, ""))
end

---@param candidate table?
---@return AIToolCall[] calls
local function candidate_tool_calls(candidate)
  local content = candidate and candidate.content
  local calls = {}
  for _, part in ipairs(content and content.parts or {}) do
    if part.functionCall and type(part.functionCall.name) == "string" then
      calls[#calls + 1] = {
        id = part.functionCall.id,
        name = part.functionCall.name,
        args = type(part.functionCall.args) == "table" and part.functionCall.args or {},
      }
    end
  end
  return calls
end

--- Gemini accepts an OpenAPI-style schema subset; strip JSON Schema fields
--- it rejects (MCP tool schemas commonly carry them).
---@param schema table
---@return table
local function sanitize_schema(schema)
  if type(schema) ~= "table" then return schema end
  local cleaned = {}
  for key, value in pairs(schema) do
    if key ~= "$schema" and key ~= "additionalProperties" then
      cleaned[key] = type(value) == "table" and sanitize_schema(value) or value
    end
  end
  return cleaned
end

--- Build the contents array: either a fresh single-turn prompt, or the prior
--- history (which already ends with the model's functionCall content,
--- thoughtSignature included) plus one user content carrying all
--- functionResponse parts. Gemini only knows the roles "user" and "model";
--- echoing functionCall.id in functionResponse.id maps parallel calls back.
---@param request AIGenerateRequest
---@return table[] contents
local function build_contents(request)
  if not request.history then
    return { { role = "user", parts = { { text = request.prompt } } } }
  end
  local contents = request.history
  if request.tool_results and #request.tool_results > 0 then
    local parts = {}
    for _, tool_result in ipairs(request.tool_results) do
      local response = { name = tool_result.call.name, response = { result = tool_result.output } }
      if tool_result.call.id then response.id = tool_result.call.id end
      parts[#parts + 1] = { functionResponse = response }
    end
    contents[#contents + 1] = { role = "user", parts = parts }
  end
  return contents
end

---@param ctx AIRequestContext
---@param spec AIModelSpec
---@param request AIGenerateRequest
---@param cb fun(result: AIGenerateResult)
function M.generate(ctx, spec, request, cb)
  local api_key = vim.env.GEMINI_API_KEY
  if not api_key or api_key == "" then
    cb({ ok = false, error = "gemini: GEMINI_API_KEY is not set" })
    return
  end

  local contents = build_contents(request)
  local body = {
    contents = contents,
  }
  if request.system then
    body.systemInstruction = { parts = { { text = request.system } } }
  end
  if request.tools and #request.tools > 0 then
    local declarations = {}
    for _, tool in ipairs(request.tools) do
      declarations[#declarations + 1] = {
        name = tool.name,
        description = tool.description,
        parameters = sanitize_schema(tool.parameters),
      }
    end
    body.tools = { { functionDeclarations = declarations } }
  end

  local generation_config = {}
  if request.temperature then generation_config.temperature = request.temperature end
  if request.max_output_tokens then generation_config.maxOutputTokens = request.max_output_tokens end
  if spec.thinking ~= nil then
    local field = type(spec.thinking) == "number" and "thinkingBudget" or "thinkingLevel"
    generation_config.thinkingConfig = { [field] = spec.thinking }
  end
  if next(generation_config) then
    body.generationConfig = generation_config
  end

  ctx.request({
    method = "POST",
    url = "https://generativelanguage.googleapis.com/v1beta/models/" .. spec.model .. ":generateContent",
    headers = {
      ["content-type"] = "application/json",
      ["x-goog-api-key"] = api_key,
    },
    body = vim.json.encode(body),
  }, function(err, response)
    if err then
      cb({ ok = false, error = "gemini: " .. err })
      return
    end
    if response.status < 200 or response.status >= 300 then
      cb({ ok = false, error = "gemini: " .. http.response_error(response) })
      return
    end

    local decoded = http.decode_json(response.body)
    local candidate = decoded and decoded.candidates and decoded.candidates[1]

    local calls = candidate_tool_calls(candidate)
    if candidate and #calls > 0 then
      -- Echo the model content verbatim into the history: Gemini 3 returns a
      -- 400 if the functionCall part's thoughtSignature is not sent back.
      local model_content = candidate.content
      model_content.role = model_content.role or "model"
      contents[#contents + 1] = model_content
      cb({ ok = true, tool_calls = calls, history = contents })
      return
    end

    local content = candidate_text(candidate)
    if not content or content == "" then
      local reason = candidate and candidate.finishReason or "no candidates"
      cb({ ok = false, error = "gemini: empty response (" .. tostring(reason) .. ")" })
      return
    end
    cb({ ok = true, content = content })
  end)
end

return M
