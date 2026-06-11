vim.loader.enable(false)

local ai = require("ai")

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function assert_eq(actual, expected, message)
  if actual ~= expected then
    error(("%s (expected %s, got %s)"):format(message, vim.inspect(expected), vim.inspect(actual)), 2)
  end
end

---@type AIHttpRequest[]
local requests = {}

---@alias AIMockRoute fun(request: AIHttpRequest): integer, table

---@type table<string, AIMockRoute> matched by plain substring of the url
local routes = {}

local mock_now = 1000

---@type AIBackend
local backend = {
  request = function(request, cb)
    requests[#requests + 1] = vim.deepcopy(request)
    for pattern, route in pairs(routes) do
      if request.url:find(pattern, 1, true) then
        local status, body = route(request)
        vim.defer_fn(function()
          cb(nil, { status = status, body = vim.json.encode(body) })
        end, 2)
        return
      end
    end
    vim.defer_fn(function()
      cb("no mock route for " .. request.url)
    end, 2)
  end,
  read_file = function(path)
    if path:find("github-copilot", 1, true) and path:find("apps.json", 1, true) then
      return vim.json.encode({
        ["github.com:Iv1.mock"] = { oauth_token = "ghu_mock_oauth" },
      })
    end
    return nil
  end,
  now = function()
    return mock_now
  end,
}

---@param opts AIGenerateOpts
---@return AIGenerateResult
local function generate_sync(opts)
  ---@type AIGenerateResult?
  local result
  ai.generate(opts, function(generate_result)
    result = generate_result
  end)
  assert_true(vim.wait(1000, function() return result ~= nil end, 10), "generate did not complete")
  return result --[[@as AIGenerateResult]]
end

---@param raw_body string
---@return table
local function decode_body(raw_body)
  return vim.json.decode(raw_body, { luanil = { object = true, array = true } })
end

local function test_resolve_errors()
  local _, err = ai.resolve("nonsense")
  assert_true(err and err:find("unknown model preset", 1, true), "missing unknown preset error: " .. tostring(err))

  _, err = ai.resolve("gemini3,thinking=bogus")
  assert_true(err and err:find("unknown thinking level", 1, true), "missing bad thinking error: " .. tostring(err))

  _, err = ai.resolve("gemini3,thinking=none")
  assert_true(err and err:find("unknown thinking level", 1, true), "thinking=none is not in the vocabulary: " .. tostring(err))

  local copilot_spec = ai.resolve("copilot,thinking=minimal")
  assert_true(copilot_spec ~= nil, "every thinking level should resolve on copilot")
  assert_eq(copilot_spec.thinking, nil, "copilot has no thinking mapping, level should resolve to nothing")

  local max_spec = ai.resolve("gemini3,thinking=max")
  assert_eq(max_spec and max_spec.thinking, "high", "gemini3 max should alias high")

  local xhigh_spec = ai.resolve("gpt,thinking=max")
  assert_eq(xhigh_spec and xhigh_spec.thinking, "xhigh", "gpt max should map to xhigh")

  _, err = ai.resolve("gemini3,bogus=1")
  assert_true(err and err:find("unknown model option", 1, true), "missing unknown option error: " .. tostring(err))

  _, err = ai.resolve("gemini3,thinking")
  assert_true(err and err:find("expected key=value", 1, true), "missing malformed option error: " .. tostring(err))

  local spec = ai.resolve("gemini3,model=gemini-3-flash-preview")
  assert_eq(spec and spec.model, "gemini-3-flash-preview", "model override not applied")
end

local function test_gemini_generate()
  vim.env.GEMINI_API_KEY = "gemini-test-key"
  requests = {}
  routes = {
    ["generativelanguage.googleapis.com"] = function()
      return 200, {
        candidates = {
          {
            content = {
              parts = {
                { text = "internal reasoning", thought = true },
                { text = "feat: add prompt module" },
              },
            },
            finishReason = "STOP",
          },
        },
      }
    end,
  }

  local result = generate_sync({
    model = "gemini3,thinking=minimal",
    system = "You write commit messages.",
    prompt = "diff content",
  })
  assert_true(result.ok, "gemini generate failed: " .. tostring(result.error))
  assert_eq(result.content, "feat: add prompt module", "gemini content mismatch (thought part not skipped?)")

  assert_eq(#requests, 1, "gemini should make exactly one request")
  local request = requests[1]
  assert_eq(request.method, "POST", "gemini method")
  assert_true(
    request.url:find("models/gemini-3.5-flash:generateContent", 1, true) ~= nil,
    "gemini url mismatch: " .. request.url
  )
  assert_eq(request.headers["x-goog-api-key"], "gemini-test-key", "gemini api key header")

  local body = decode_body(request.body)
  assert_eq(body.systemInstruction.parts[1].text, "You write commit messages.", "gemini system instruction")
  assert_eq(body.contents[1].role, "user", "gemini user role")
  assert_eq(body.contents[1].parts[1].text, "diff content", "gemini prompt text")
  assert_eq(body.generationConfig.thinkingConfig.thinkingLevel, "minimal", "gemini3 thinking=minimal should map to thinkingLevel minimal")
end

local function test_gemini_thinking_budget()
  requests = {}
  local result = generate_sync({ model = "gemini2.5,thinking=minimal", prompt = "diff" })
  assert_true(result.ok, "gemini2.5 generate failed: " .. tostring(result.error))
  local body = decode_body(requests[1].body)
  assert_eq(body.generationConfig.thinkingConfig.thinkingBudget, 0, "gemini2.5 thinking=minimal should map to thinkingBudget 0")
  assert_true(
    requests[1].url:find("models/gemini-2.5-flash:generateContent", 1, true) ~= nil,
    "gemini2.5 url mismatch: " .. requests[1].url
  )
end

local function test_gemini_default_thinking_omitted()
  requests = {}
  local result = generate_sync({ model = "gemini3", prompt = "diff" })
  assert_true(result.ok, "gemini default generate failed: " .. tostring(result.error))
  local body = decode_body(requests[1].body)
  assert_true(body.generationConfig == nil, "no thinking token should leave generationConfig unset")
end

local function test_gemini_http_error()
  requests = {}
  routes = {
    ["generativelanguage.googleapis.com"] = function()
      return 400, { error = { code = 400, message = "API key not valid", status = "INVALID_ARGUMENT" } }
    end,
  }
  local result = generate_sync({ model = "gemini3", prompt = "diff" })
  assert_true(not result.ok, "gemini http error should fail")
  assert_true(result.error:find("API key not valid", 1, true) ~= nil, "gemini error message lost: " .. result.error)
end

local function test_gemini_missing_key()
  vim.env.GEMINI_API_KEY = nil
  local result = generate_sync({ model = "gemini3", prompt = "diff" })
  assert_true(not result.ok, "missing gemini key should fail")
  assert_true(result.error:find("GEMINI_API_KEY", 1, true) ~= nil, "missing key error: " .. result.error)
end

local function test_openai_generate()
  vim.env.OPENAI_API_KEY = "openai-test-key"
  requests = {}
  routes = {
    ["api.openai.com/v1/responses"] = function()
      return 200, {
        status = "completed",
        output = {
          { type = "reasoning", summary = {} },
          {
            type = "message",
            role = "assistant",
            content = { { type = "output_text", text = "fix: handle empty diff" } },
          },
        },
      }
    end,
  }

  local result = generate_sync({
    model = "gpt-nano,thinking=minimal",
    system = "You write commit messages.",
    prompt = "diff content",
    max_output_tokens = 300,
  })
  assert_true(result.ok, "openai generate failed: " .. tostring(result.error))
  assert_eq(result.content, "fix: handle empty diff", "openai content mismatch (reasoning item not skipped?)")

  local request = requests[1]
  assert_eq(request.headers["authorization"], "Bearer openai-test-key", "openai auth header")
  local body = decode_body(request.body)
  assert_eq(body.model, "gpt-5.4-nano", "openai preset model")
  assert_eq(body.reasoning.effort, "none", "openai thinking=minimal should map to reasoning effort none")
  assert_eq(body.instructions, "You write commit messages.", "openai instructions")
  assert_eq(body.input, "diff content", "openai input")
  assert_eq(body.max_output_tokens, 300, "openai max_output_tokens")
end

local function test_openai_incomplete()
  requests = {}
  routes = {
    ["api.openai.com/v1/responses"] = function()
      return 200, {
        status = "incomplete",
        incomplete_details = { reason = "max_output_tokens" },
        output = {},
      }
    end,
  }
  local result = generate_sync({ model = "gpt-nano", prompt = "diff" })
  assert_true(not result.ok, "incomplete response should fail")
  assert_true(result.error:find("max_output_tokens", 1, true) ~= nil, "incomplete reason lost: " .. result.error)
end

local function test_copilot_generate_and_session_cache()
  requests = {}
  mock_now = 1000
  local token_exchanges = 0
  routes = {
    ["api.github.com/copilot_internal/v2/token"] = function()
      token_exchanges = token_exchanges + 1
      return 200, {
        token = "session-token-" .. token_exchanges,
        expires_at = 5000,
        endpoints = { api = "https://api.business.githubcopilot.com/" },
      }
    end,
    ["chat/completions"] = function()
      return 200, {
        choices = { { message = { role = "assistant", content = "chore: bump deps\n" } } },
      }
    end,
  }

  local result = generate_sync({
    model = "copilot,thinking=minimal",
    system = "You write commit messages.",
    prompt = "diff content",
  })
  assert_true(result.ok, "copilot generate failed: " .. tostring(result.error))
  assert_eq(result.content, "chore: bump deps", "copilot content should be trimmed")

  assert_eq(#requests, 2, "copilot first call should exchange token then complete")
  assert_eq(requests[1].method, "GET", "token exchange method")
  assert_eq(requests[1].headers["authorization"], "Token ghu_mock_oauth", "token exchange auth header")
  assert_eq(
    requests[2].url,
    "https://api.business.githubcopilot.com/chat/completions",
    "completion should use the base url from the token response"
  )
  assert_eq(requests[2].headers["authorization"], "Bearer session-token-1", "completion bearer token")
  assert_eq(requests[2].headers["copilot-integration-id"], "vscode-chat", "copilot integration id header")

  local body = decode_body(requests[2].body)
  assert_eq(body.model, "gpt-4.1", "copilot preset model")
  assert_eq(body.stream, false, "copilot should request non-streaming")
  assert_true(body.reasoning == nil and body.reasoning_effort == nil, "copilot thinking levels must not reach the wire")
  assert_eq(body.messages[1].role, "system", "copilot system message role")
  assert_eq(body.messages[2].content, "diff content", "copilot user message")

  -- Cached session: no second token exchange while it is fresh.
  result = generate_sync({ model = "copilot", prompt = "diff content" })
  assert_true(result.ok, "copilot cached generate failed: " .. tostring(result.error))
  assert_eq(#requests, 3, "cached session should make exactly one more request")
  assert_eq(requests[3].headers["authorization"], "Bearer session-token-1", "cached session token reused")

  -- Expired session: token exchange runs again.
  mock_now = 6000
  result = generate_sync({ model = "copilot,model=gpt-5-mini", prompt = "diff content" })
  assert_true(result.ok, "copilot refresh generate failed: " .. tostring(result.error))
  assert_eq(#requests, 5, "expired session should re-exchange the token")
  assert_eq(requests[5].headers["authorization"], "Bearer session-token-2", "refreshed session token used")
  assert_eq(decode_body(requests[5].body).model, "gpt-5-mini", "model override on copilot preset")
end

local function test_copilot_missing_oauth()
  ai.set_backend({
    request = backend.request,
    now = backend.now,
    read_file = function() return nil end,
  })
  local result = generate_sync({ model = "copilot", prompt = "diff" })
  assert_true(not result.ok, "missing oauth should fail")
  assert_true(result.error:find("oauth token", 1, true) ~= nil, "oauth error message: " .. result.error)
  ai.set_backend(backend)
end

local function test_transport_error()
  routes = {}
  local result = generate_sync({ model = "gemini3", prompt = "diff" })
  assert_true(not result.ok, "transport error should fail")
  assert_true(result.error:find("no mock route", 1, true) ~= nil, "transport error message: " .. result.error)
end

local function run()
  ai.set_backend(backend)

  test_resolve_errors()
  test_gemini_generate()
  test_gemini_thinking_budget()
  test_gemini_default_thinking_omitted()
  test_gemini_http_error()
  test_gemini_missing_key()
  test_openai_generate()
  test_openai_incomplete()
  test_copilot_generate_and_session_cache()
  test_copilot_missing_oauth()

  vim.env.GEMINI_API_KEY = "gemini-test-key"
  test_transport_error()
end

local ok, err = xpcall(run, debug.traceback)
ai.reset_backend()
vim.env.GEMINI_API_KEY = nil
vim.env.OPENAI_API_KEY = nil
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
