-- OpenAI provider (Responses API).
--
-- Docs: https://platform.openai.com/docs/api-reference/responses
--       https://platform.openai.com/docs/guides/reasoning
-- Auth: OPENAI_API_KEY sent as a bearer token.
--
-- The Responses API is OpenAI's recommended interface for new integrations
-- (Chat Completions remains supported but is the legacy surface). Reasoning
-- effort goes in `reasoning.effort`; "none" is supported on gpt-5.1 and later
-- and is the fastest setting. The output text is NOT guaranteed to be at
-- output[1] - reasoning models emit a "reasoning" item before the "message"
-- item, so parsing must scan for the message item.

local http = require("ai.http")

---@class AIOpenAIProvider: AIProvider
local M = {
  name = "openai",
}

-- minimal maps to effort "none" (supported on gpt-5.1 and later, the true
-- floor; OpenAI's own "minimal" effort is a higher legacy setting). max maps
-- to "xhigh" on gpt-5.5 where it is documented; the gpt-5.4 mini/nano model
-- pages do not confirm xhigh, so max aliases high there.
---@type table<string, AIPreset>
M.presets = {
  ["gpt"] = {
    model = "gpt-5.5",
    thinking = { minimal = "none", low = "low", medium = "medium", high = "high", max = "xhigh" },
  },
  ["gpt-mini"] = {
    model = "gpt-5.4-mini",
    thinking = { minimal = "none", low = "low", medium = "medium", high = "high", max = "high" },
  },
  ["gpt-nano"] = {
    model = "gpt-5.4-nano",
    thinking = { minimal = "none", low = "low", medium = "medium", high = "high", max = "high" },
  },
}

---@param decoded table?
---@return string? content
local function output_text(decoded)
  local texts = {}
  for _, item in ipairs(decoded and decoded.output or {}) do
    if item.type == "message" then
      for _, part in ipairs(item.content or {}) do
        if part.type == "output_text" and type(part.text) == "string" then
          texts[#texts + 1] = part.text
        end
      end
    end
  end
  if #texts == 0 then return nil end
  return vim.trim(table.concat(texts, ""))
end

---@param ctx AIRequestContext
---@param spec AIModelSpec
---@param request AIGenerateRequest
---@param cb fun(result: AIGenerateResult)
function M.generate(ctx, spec, request, cb)
  local api_key = vim.env.OPENAI_API_KEY
  if not api_key or api_key == "" then
    cb({ ok = false, error = "openai: OPENAI_API_KEY is not set" })
    return
  end

  local body = {
    model = spec.model,
    input = request.prompt,
  }
  if request.system then body.instructions = request.system end
  if request.temperature then body.temperature = request.temperature end
  if request.max_output_tokens then body.max_output_tokens = request.max_output_tokens end
  if spec.thinking ~= nil then
    body.reasoning = { effort = spec.thinking }
  end

  ctx.request({
    method = "POST",
    url = "https://api.openai.com/v1/responses",
    headers = {
      ["content-type"] = "application/json",
      ["authorization"] = "Bearer " .. api_key,
    },
    body = vim.json.encode(body),
  }, function(err, response)
    if err then
      cb({ ok = false, error = "openai: " .. err })
      return
    end
    if response.status < 200 or response.status >= 300 then
      cb({ ok = false, error = "openai: " .. http.response_error(response) })
      return
    end

    local decoded = http.decode_json(response.body)
    if decoded and decoded.status == "incomplete" then
      local reason = decoded.incomplete_details and decoded.incomplete_details.reason or "unknown"
      cb({ ok = false, error = "openai: incomplete response (" .. tostring(reason) .. ")" })
      return
    end
    local content = output_text(decoded)
    if not content or content == "" then
      cb({ ok = false, error = "openai: empty response" })
      return
    end
    cb({ ok = true, content = content })
  end)
end

return M
