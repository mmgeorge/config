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

---@alias AIToolsMockRoute fun(request: AIHttpRequest, body: table): integer, table

---@type table<string, AIToolsMockRoute> matched by plain substring of the url
local routes = {}

---@type AIBackend
local backend = {
  request = function(request, cb)
    requests[#requests + 1] = vim.deepcopy(request)
    local body = request.body and vim.json.decode(request.body, { luanil = { object = true, array = true } }) or {}
    for pattern, route in pairs(routes) do
      if request.url:find(pattern, 1, true) then
        local status, response_body = route(request, body)
        vim.defer_fn(function()
          cb(nil, { status = status, body = vim.json.encode(response_body) })
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
      return vim.json.encode({ ["github.com:Iv1.mock"] = { oauth_token = "ghu_mock_oauth" } })
    end
    return nil
  end,
  now = function() return 1000 end,
}

---@param opts AIGenerateWithToolsOpts
---@return AIGenerateResult
local function generate_with_tools_sync(opts)
  ---@type AIGenerateResult?
  local result
  ai.generate_with_tools(opts, function(generate_result)
    result = generate_result
  end)
  assert_true(vim.wait(2000, function() return result ~= nil end, 10), "generate_with_tools did not complete")
  return result --[[@as AIGenerateResult]]
end

---@param raw_body string
---@return table
local function decode_body(raw_body)
  return vim.json.decode(raw_body, { luanil = { object = true, array = true } })
end

local function git_log_tool(log)
  return {
    name = "git_log",
    description = "Recent commits",
    parameters = {
      type = "object",
      properties = { count = { type = "number" } },
      required = { "count" },
    },
    run = function(args, done)
      log[#log + 1] = args
      vim.defer_fn(function()
        done("abc123 feat: earlier change (count=" .. tostring(args.count) .. ")")
      end, 2)
    end,
  }
end

local function test_openai_tool_round_trip()
  vim.env.OPENAI_API_KEY = "openai-test-key"
  requests = {}
  local tool_args = {}
  local openai_round = 0
  routes = {
    ["api.openai.com/v1/responses"] = function()
      openai_round = openai_round + 1
      if openai_round == 1 then
        return 200, {
          status = "completed",
          output = {
            { type = "reasoning", summary = {} },
            {
              type = "function_call",
              id = "fc_1",
              call_id = "call_1",
              name = "git_log",
              arguments = vim.json.encode({ count = 3 }),
            },
          },
        }
      end
      return 200, {
        status = "completed",
        output = {
          {
            type = "message",
            role = "assistant",
            content = { { type = "output_text", text = "feat: follow up on abc123" } },
          },
        },
      }
    end,
  }

  local result = generate_with_tools_sync({
    model = "gpt-nano,thinking=minimal",
    system = "You write commit messages.",
    prompt = "Summarize the change",
    tools = { git_log_tool(tool_args) },
  })
  assert_true(result.ok, "openai tool round trip failed: " .. tostring(result.error))
  assert_eq(result.content, "feat: follow up on abc123", "openai final content")
  assert_eq(#tool_args, 1, "tool should run exactly once")
  assert_eq(tool_args[1].count, 3, "tool received decoded arguments")

  assert_eq(#requests, 2, "openai should make two requests")
  local first_body = decode_body(requests[1].body)
  assert_eq(first_body.tools[1].type, "function", "openai tool type")
  assert_eq(first_body.tools[1].name, "git_log", "openai tools are flat (no nested function object)")
  assert_true(first_body.tools[1]["function"] == nil, "openai responses tools must not nest a function object")

  local second_body = decode_body(requests[2].body)
  assert_eq(second_body.instructions, "You write commit messages.", "instructions resent in stateless mode")
  local input = second_body.input
  assert_eq(input[1].role, "user", "round 2 replays the original user message")
  assert_eq(input[2].type, "reasoning", "round 2 replays reasoning items verbatim")
  assert_eq(input[3].type, "function_call", "round 2 replays the function_call item")
  assert_eq(input[3].call_id, "call_1", "function_call call_id preserved")
  assert_eq(input[4].type, "function_call_output", "round 2 appends the tool output")
  assert_eq(input[4].call_id, "call_1", "function_call_output call_id matches")
  assert_true(input[4].output:find("abc123", 1, true) ~= nil, "tool output text forwarded")
end

local function test_gemini_tool_round_trip_parallel()
  vim.env.GEMINI_API_KEY = "gemini-test-key"
  requests = {}
  local gemini_round = 0
  routes = {
    ["generativelanguage.googleapis.com"] = function()
      gemini_round = gemini_round + 1
      if gemini_round == 1 then
        return 200, {
          candidates = {
            {
              content = {
                role = "model",
                parts = {
                  {
                    functionCall = { id = "fc_a", name = "git_log", args = { count = 1 } },
                    thoughtSignature = "sig-A",
                  },
                  {
                    functionCall = { id = "fc_b", name = "git_diff", args = { staged = true } },
                  },
                },
              },
              finishReason = "STOP",
            },
          },
        }
      end
      return 200, {
        candidates = {
          {
            content = { parts = { { text = "fix: adjust parser" } } },
            finishReason = "STOP",
          },
        },
      }
    end,
  }

  local result = generate_with_tools_sync({
    model = "gemini3,thinking=minimal",
    system = "You write commit messages.",
    prompt = "Summarize",
    tools = {
      {
        name = "git_log",
        description = "Recent commits",
        parameters = { type = "object", properties = { count = { type = "number" } }, additionalProperties = false },
        run = function(_, done) done("log output") end,
      },
      {
        name = "git_diff",
        description = "Diff",
        parameters = { type = "object", properties = { staged = { type = "boolean" } } },
        run = function(_, done) done("diff output") end,
      },
    },
  })
  assert_true(result.ok, "gemini tool round trip failed: " .. tostring(result.error))
  assert_eq(result.content, "fix: adjust parser", "gemini final content")

  local first_body = decode_body(requests[1].body)
  local declarations = first_body.tools[1].functionDeclarations
  assert_eq(declarations[1].name, "git_log", "gemini functionDeclarations shape")
  assert_true(declarations[1].parameters.additionalProperties == nil, "gemini schema sanitizer strips additionalProperties")

  local second_body = decode_body(requests[2].body)
  local contents = second_body.contents
  assert_eq(#contents, 3, "round 2 contents: user, model, tool results")
  assert_eq(contents[2].role, "model", "model content echoed")
  assert_eq(contents[2].parts[1].thoughtSignature, "sig-A", "thoughtSignature echoed verbatim")
  assert_eq(contents[3].role, "user", "tool results use the user role")
  assert_eq(#contents[3].parts, 2, "parallel results in one content entry")
  assert_eq(contents[3].parts[1].functionResponse.id, "fc_a", "functionResponse echoes the call id")
  assert_eq(contents[3].parts[1].functionResponse.name, "git_log", "functionResponse name")
  assert_eq(contents[3].parts[1].functionResponse.response.result, "log output", "tool output wrapped as response.result")
  assert_eq(contents[3].parts[2].functionResponse.id, "fc_b", "second functionResponse id")
end

local function test_copilot_tool_round_trip()
  requests = {}
  local copilot_round = 0
  routes = {
    ["api.github.com/copilot_internal/v2/token"] = function()
      return 200, { token = "session-token", expires_at = 5000 }
    end,
    ["chat/completions"] = function()
      copilot_round = copilot_round + 1
      if copilot_round == 1 then
        return 200, {
          choices = {
            {
              message = {
                role = "assistant",
                content = vim.NIL,
                tool_calls = {
                  {
                    id = "call_9",
                    type = "function",
                    ["function"] = { name = "git_log", arguments = vim.json.encode({ count = 2 }) },
                  },
                },
              },
            },
          },
        }
      end
      return 200, { choices = { { message = { role = "assistant", content = "chore: tidy" } } } }
    end,
  }

  local tool_args = {}
  local result = generate_with_tools_sync({
    model = "copilot",
    system = "You write commit messages.",
    prompt = "Summarize",
    tools = { git_log_tool(tool_args) },
  })
  assert_true(result.ok, "copilot tool round trip failed: " .. tostring(result.error))
  assert_eq(result.content, "chore: tidy", "copilot final content")
  assert_eq(tool_args[1].count, 2, "copilot tool received decoded arguments")

  -- requests: token exchange, completion 1, completion 2
  assert_eq(#requests, 3, "copilot request count")
  assert_eq(requests[2].headers["x-initiator"], "agent", "tool requests send x-initiator agent")
  local first_body = decode_body(requests[2].body)
  assert_eq(first_body.tools[1]["function"].name, "git_log", "copilot tools nest a function object")

  local second_body = decode_body(requests[3].body)
  local messages = second_body.messages
  assert_eq(messages[3].role, "assistant", "assistant tool_calls message echoed")
  assert_eq(messages[3].tool_calls[1].id, "call_9", "tool_calls echoed verbatim")
  assert_eq(messages[4].role, "tool", "tool result role")
  assert_eq(messages[4].tool_call_id, "call_9", "tool_call_id matches")
end

local function test_unknown_tool_feeds_error_back()
  requests = {}
  local openai_round = 0
  routes = {
    ["api.openai.com/v1/responses"] = function()
      openai_round = openai_round + 1
      if openai_round == 1 then
        return 200, {
          status = "completed",
          output = {
            { type = "function_call", call_id = "call_x", name = "not_a_tool", arguments = "{}" },
          },
        }
      end
      return 200, {
        status = "completed",
        output = {
          { type = "message", content = { { type = "output_text", text = "recovered" } } },
        },
      }
    end,
  }

  local result = generate_with_tools_sync({
    model = "gpt-nano",
    prompt = "go",
    tools = { { name = "real_tool", description = "", parameters = { type = "object" }, run = function(_, done) done("x") end } },
  })
  assert_true(result.ok, "unknown tool should not abort the loop: " .. tostring(result.error))
  local second_body = decode_body(requests[2].body)
  local output_item = second_body.input[#second_body.input]
  assert_eq(output_item.type, "function_call_output", "error fed back as tool output")
  assert_true(output_item.output:find("unknown tool", 1, true) ~= nil, "unknown tool error message: " .. output_item.output)
end

local function test_max_rounds_exhausted()
  requests = {}
  routes = {
    ["api.openai.com/v1/responses"] = function()
      return 200, {
        status = "completed",
        output = {
          { type = "function_call", call_id = "call_loop", name = "spin", arguments = "{}" },
        },
      }
    end,
  }

  local result = generate_with_tools_sync({
    model = "gpt-nano",
    prompt = "go",
    max_rounds = 2,
    tools = { { name = "spin", description = "", parameters = { type = "object" }, run = function(_, done) done("again") end } },
  })
  assert_true(not result.ok, "endless tool calls must abort")
  assert_true(result.error:find("rounds exhausted", 1, true) ~= nil, "max rounds error: " .. tostring(result.error))
  assert_eq(#requests, 2, "no request beyond max_rounds")
end

local function test_tool_validation()
  local result = generate_with_tools_sync({
    model = "gpt-nano",
    prompt = "go",
    tools = { { name = "bad name!", description = "", parameters = {}, run = function() end } },
  })
  assert_true(not result.ok and result.error:find("invalid tool name", 1, true) ~= nil, "invalid tool name: " .. tostring(result.error))

  result = generate_with_tools_sync({
    model = "gpt-nano",
    prompt = "go",
    tools = {
      { name = "dup", description = "", parameters = {}, run = function(_, done) done("") end },
      { name = "dup", description = "", parameters = {}, run = function(_, done) done("") end },
    },
  })
  assert_true(not result.ok and result.error:find("duplicate tool name", 1, true) ~= nil, "duplicate tool name: " .. tostring(result.error))
end

local function test_registry_tools()
  local registry = require("ai.tools").registry
  local ran_args = {}
  registry.test_registry_tool = {
    description = "Test tool",
    parameters = { type = "object", properties = { value = { type = "string" } } },
    run = function(args, done)
      ran_args[#ran_args + 1] = args
      done("registry says: " .. tostring(args.value))
    end,
  }

  requests = {}
  local openai_round = 0
  routes = {
    ["api.openai.com/v1/responses"] = function()
      openai_round = openai_round + 1
      if openai_round == 1 then
        return 200, {
          status = "completed",
          output = {
            {
              type = "function_call",
              call_id = "call_reg",
              name = "test_registry_tool",
              arguments = vim.json.encode({ value = "hello" }),
            },
          },
        }
      end
      return 200, {
        status = "completed",
        output = {
          { type = "message", content = { { type = "output_text", text = "done" } } },
        },
      }
    end,
  }

  local result = generate_with_tools_sync({
    model = "gpt-nano",
    prompt = "go",
    tools = { "test_registry_tool" },
  })
  assert_true(result.ok, "registry tool flow failed: " .. tostring(result.error))
  assert_eq(#ran_args, 1, "registry tool should run")
  assert_eq(ran_args[1].value, "hello", "registry tool received arguments")
  local first_body = decode_body(requests[1].body)
  assert_eq(first_body.tools[1].name, "test_registry_tool", "registry tool offered under its key")

  registry.test_registry_tool = nil

  result = generate_with_tools_sync({ model = "gpt-nano", prompt = "go", tools = { "no_such_tool" } })
  assert_true(
    not result.ok and result.error:find("unknown registered tool", 1, true) ~= nil,
    "unknown registry tool: " .. tostring(result.error)
  )

  -- Shipped registry entries resolve to well-formed tools.
  local git_log = require("ai.tools").get("git_log")
  assert_true(git_log ~= nil and git_log.name == "git_log" and type(git_log.run) == "function", "git_log registry entry")
end

local function test_mcp_tools()
  local hub_calls = {}
  local fake_hub = {
    get_servers = function()
      return {
        {
          name = "files",
          capabilities = {
            tools = {
              {
                name = "read file",
                description = "Read a file",
                inputSchema = { type = "object", properties = { path = { type = "string" } }, required = { "path" } },
              },
            },
          },
        },
      }
    end,
    call_tool = function(_, server_name, tool_name, args, opts)
      hub_calls[#hub_calls + 1] = { server = server_name, tool = tool_name, args = args }
      opts.callback({ text = "contents of " .. tostring(args.path) }, nil)
    end,
  }
  package.loaded["mcphub"] = { get_hub_instance = function() return fake_hub end }

  requests = {}
  local openai_round = 0
  routes = {
    ["api.openai.com/v1/responses"] = function()
      openai_round = openai_round + 1
      if openai_round == 1 then
        return 200, {
          status = "completed",
          output = {
            {
              type = "function_call",
              call_id = "call_mcp",
              name = "files__read_file",
              arguments = vim.json.encode({ path = "init.lua" }),
            },
          },
        }
      end
      return 200, {
        status = "completed",
        output = {
          { type = "message", content = { { type = "output_text", text = "done" } } },
        },
      }
    end,
  }

  local result = generate_with_tools_sync({
    model = "gpt-nano",
    prompt = "read it",
    mcps = { "files" },
  })
  assert_true(result.ok, "mcp tool flow failed: " .. tostring(result.error))

  local first_body = decode_body(requests[1].body)
  assert_eq(first_body.tools[1].name, "files__read_file", "mcp tool offered with namespaced, sanitized name")
  assert_eq(first_body.tools[1].parameters.properties.path.type, "string", "mcp inputSchema passed through")

  assert_eq(#hub_calls, 1, "hub call count")
  assert_eq(hub_calls[1].server, "files", "hub called with the original server name")
  assert_eq(hub_calls[1].tool, "read file", "hub called with the original tool name")
  assert_eq(hub_calls[1].args.path, "init.lua", "hub received decoded arguments")

  local second_body = decode_body(requests[2].body)
  local output_item = second_body.input[#second_body.input]
  assert_true(output_item.output:find("contents of init.lua", 1, true) ~= nil, "mcp result forwarded: " .. output_item.output)

  -- Unknown server errors before any request.
  requests = {}
  result = generate_with_tools_sync({ model = "gpt-nano", prompt = "go", mcps = { "nope" } })
  assert_true(not result.ok and result.error:find("not active", 1, true) ~= nil, "unknown mcp server: " .. tostring(result.error))
  assert_eq(#requests, 0, "no request when mcp resolution fails")

  package.loaded["mcphub"] = nil
end

local function run()
  ai.set_backend(backend)

  test_openai_tool_round_trip()
  test_gemini_tool_round_trip_parallel()
  test_copilot_tool_round_trip()
  test_unknown_tool_feeds_error_back()
  test_max_rounds_exhausted()
  test_tool_validation()
  test_registry_tools()
  test_mcp_tools()
end

local ok, err = xpcall(run, debug.traceback)
ai.reset_backend()
package.loaded["mcphub"] = nil
vim.env.GEMINI_API_KEY = nil
vim.env.OPENAI_API_KEY = nil
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
