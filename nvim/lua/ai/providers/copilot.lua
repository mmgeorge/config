-- GitHub Copilot provider (chat completions).
--
-- There is no officially documented public HTTP API for Copilot chat; this
-- speaks the same protocol as CopilotChat.nvim and codecompanion.nvim:
--   1. Read the long-lived oauth grant written by the Copilot sign-in flow
--      (copilot.lua / copilot.vim / copilot-language-server) from
--      github-copilot/{hosts,apps}.json under the user's config directory.
--   2. Exchange it for a short-lived session token at
--      GET https://api.github.com/copilot_internal/v2/token. The response
--      carries the session token, its expiry, and the API base URL
--      (business/enterprise accounts use a different host).
--   3. POST an OpenAI-style chat completion to <base>/chat/completions.
--
-- The session token is cached in this module and refreshed shortly before
-- expiry. Reference implementation: CopilotChat.nvim
-- lua/CopilotChat/config/providers.lua.

local http = require("ai.http")

---@class AICopilotSession
---@field token string
---@field expires_at integer
---@field base_url string

---@class AICopilotProvider: AIProvider
local M = {
  name = "copilot",
}

---@type AICopilotSession?
M._session = nil

---@type table<string, AIPreset>
M.presets = {
  -- The Copilot API exposes no verified reasoning parameter, so no preset has
  -- a thinking map: every thinking level is accepted and sends nothing. Pick
  -- a non-reasoning model (gpt-4.1) when you actually want no thinking.
  -- Model ids per https://docs.github.com/en/copilot/reference/ai-models/supported-models;
  -- gpt-5.4-nano is the cheapest current model but is flagged as unavailable
  -- in Agent/Ask modes, so it may be rejected on some plans.
  ["copilot"] = { model = "gpt-4.1" },
  ["copilot-mini"] = { model = "gpt-5-mini" },
  ["copilot-nano"] = { model = "gpt-5.4-nano" },
}

function M.reset()
  M._session = nil
end

---@return string[] directories
local function config_dirs()
  local dirs = {}
  if vim.env.XDG_CONFIG_HOME and vim.env.XDG_CONFIG_HOME ~= "" then
    dirs[#dirs + 1] = vim.env.XDG_CONFIG_HOME
  end
  if vim.env.LOCALAPPDATA and vim.env.LOCALAPPDATA ~= "" then
    dirs[#dirs + 1] = vim.env.LOCALAPPDATA
  end
  if vim.env.HOME and vim.env.HOME ~= "" then
    dirs[#dirs + 1] = vim.env.HOME .. "/.config"
  end
  return dirs
end

---@param ctx AIRequestContext
---@return string? oauth_token
local function oauth_token(ctx)
  for _, dir in ipairs(config_dirs()) do
    for _, file in ipairs({ "hosts.json", "apps.json" }) do
      local data = ctx.read_file(dir .. "/github-copilot/" .. file)
      local decoded = data and http.decode_json(data)
      for key, value in pairs(decoded or {}) do
        if key:find("github.com", 1, true) and type(value) == "table" and type(value.oauth_token) == "string" then
          return value.oauth_token
        end
      end
    end
  end
  return nil
end

---@param ctx AIRequestContext
---@param cb fun(err: string?, session: AICopilotSession?)
local function ensure_session(ctx, cb)
  local session = M._session
  if session and session.expires_at - 60 > ctx.now() then
    cb(nil, session)
    return
  end

  local oauth = oauth_token(ctx)
  if not oauth then
    cb("no Copilot oauth token found; sign in with :Copilot auth first")
    return
  end

  ctx.request({
    method = "GET",
    url = "https://api.github.com/copilot_internal/v2/token",
    headers = {
      ["authorization"] = "Token " .. oauth,
    },
  }, function(err, response)
    if err then
      cb(err)
      return
    end
    if response.status < 200 or response.status >= 300 then
      cb("token exchange failed: " .. http.response_error(response))
      return
    end
    local decoded = http.decode_json(response.body)
    if not decoded or type(decoded.token) ~= "string" then
      cb("token exchange returned no session token")
      return
    end
    local base_url = decoded.endpoints and decoded.endpoints.api or "https://api.githubcopilot.com"
    M._session = {
      token = decoded.token,
      expires_at = tonumber(decoded.expires_at) or 0,
      base_url = (base_url:gsub("/$", "")),
    }
    cb(nil, M._session)
  end)
end

---@param ctx AIRequestContext
---@param spec AIModelSpec
---@param request AIGenerateRequest
---@param cb fun(result: AIGenerateResult)
function M.generate(ctx, spec, request, cb)
  ensure_session(ctx, function(session_err, session)
    if not session then
      cb({ ok = false, error = "copilot: " .. session_err })
      return
    end

    local messages
    if request.history then
      messages = request.history
      for _, tool_result in ipairs(request.tool_results or {}) do
        messages[#messages + 1] = {
          role = "tool",
          tool_call_id = tool_result.call.id,
          content = tool_result.output,
        }
      end
    else
      messages = {}
      if request.system then
        messages[#messages + 1] = { role = "system", content = request.system }
      end
      messages[#messages + 1] = { role = "user", content = request.prompt }
    end

    local body = {
      model = spec.model,
      messages = messages,
      stream = false,
    }
    if request.temperature then body.temperature = request.temperature end
    if request.max_output_tokens then body.max_tokens = request.max_output_tokens end
    if request.tools and #request.tools > 0 then
      local tools = {}
      for _, tool in ipairs(request.tools) do
        tools[#tools + 1] = {
          type = "function",
          ["function"] = {
            name = tool.name,
            description = tool.description,
            parameters = tool.parameters,
          },
        }
      end
      body.tools = tools
    end

    local headers = {
      ["content-type"] = "application/json",
      ["authorization"] = "Bearer " .. session.token,
      ["editor-version"] = "Neovim/" .. tostring(vim.version()),
      ["editor-plugin-version"] = "ai.nvim/1.0",
      ["copilot-integration-id"] = "vscode-chat",
      ["x-github-api-version"] = "2025-10-01",
    }
    if request.tools then
      headers["x-initiator"] = "agent"
    end

    ctx.request({
      method = "POST",
      url = session.base_url .. "/chat/completions",
      headers = headers,
      body = vim.json.encode(body),
    }, function(err, response)
      if err then
        cb({ ok = false, error = "copilot: " .. err })
        return
      end
      if response.status < 200 or response.status >= 300 then
        cb({ ok = false, error = "copilot: " .. http.response_error(response) })
        return
      end

      local decoded = http.decode_json(response.body)
      local choice = decoded and decoded.choices and decoded.choices[1]
      local message = choice and choice.message

      if message and type(message.tool_calls) == "table" and #message.tool_calls > 0 then
        local calls = {}
        for _, tool_call in ipairs(message.tool_calls) do
          local fn = tool_call["function"] or {}
          local ok_args, args = pcall(vim.json.decode, fn.arguments or "", { luanil = { object = true, array = true } })
          calls[#calls + 1] = {
            id = tool_call.id,
            name = fn.name,
            args = (ok_args and type(args) == "table") and args or {},
            args_error = not (ok_args and type(args) == "table") and "function arguments were not valid JSON" or nil,
          }
        end
        messages[#messages + 1] = message
        cb({ ok = true, tool_calls = calls, history = messages })
        return
      end

      local content = message and message.content
      if type(content) ~= "string" or vim.trim(content) == "" then
        cb({ ok = false, error = "copilot: empty response" })
        return
      end
      cb({ ok = true, content = vim.trim(content) })
    end)
  end)
end

return M
