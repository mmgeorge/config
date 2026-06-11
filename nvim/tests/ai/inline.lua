vim.loader.enable(false)

local ai = require("ai")
local inline = require("ai.inline")

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

---@type fun(): integer, table response factory for the next requests
local respond = function()
  return 200, {}
end

---@type AIBackend
local backend = {
  request = function(request, cb)
    requests[#requests + 1] = vim.deepcopy(request)
    local status, body = respond()
    vim.defer_fn(function()
      cb(nil, { status = status, body = vim.json.encode(body) })
    end, 5)
  end,
}

---@type { message: string, level: integer }[]
local notifications = {}
local function capture_notify(message, level)
  notifications[#notifications + 1] = { message = message, level = level }
end

---@param text string
---@return fun(): integer, table
local function openai_text_response(text)
  return function()
    return 200, {
      status = "completed",
      output = {
        { type = "message", content = { { type = "output_text", text = text } } },
      },
    }
  end
end

---@param lines string[]
---@param filetype string?
---@return integer buf
local function make_buffer(lines, filetype)
  local buf = vim.api.nvim_create_buf(true, false)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  if filetype then vim.bo[buf].filetype = filetype end
  return buf
end

---@param opts AIInlineOpts
---@return AIGenerateResult
local function inline_sync(opts)
  ---@type AIGenerateResult?
  local result
  opts.notify = opts.notify or capture_notify
  opts.model = opts.model or "gpt-nano"
  inline.inline(opts, function(inline_result)
    result = inline_result
  end)
  assert_true(vim.wait(2000, function() return result ~= nil end, 10), "inline did not complete")
  return result --[[@as AIGenerateResult]]
end

---@param raw_body string
---@return table
local function decode_body(raw_body)
  return vim.json.decode(raw_body, { luanil = { object = true, array = true } })
end

local function buffer_lines(buf)
  return vim.api.nvim_buf_get_lines(buf, 0, -1, false)
end

local function test_replace()
  requests = {}
  respond = openai_text_response("```javascript\nasync function foo() {\n  return 1;\n}\n```")
  local buf = make_buffer({ "function foo() {", "", "}", "console.log(foo());" }, "javascript")

  local result = inline_sync({
    type = "replace",
    buf = buf,
    range = { start_line = 1, end_line = 3 },
    prompt = "make this function async and return 1",
  })
  assert_true(result.ok, "replace failed: " .. tostring(result.error))

  local lines = buffer_lines(buf)
  assert_eq(table.concat(lines, "\n"), "async function foo() {\n  return 1;\n}\nconsole.log(foo());", "replace splices the (fence-stripped) response over the selection")

  local body = decode_body(requests[1].body)
  assert_true(body.instructions:find("replaces the selected text", 1, true) ~= nil, "replace system prompt")
  assert_true(body.input[1].content:find("make this function async", 1, true) ~= nil, "instruction in user prompt")
  assert_true(body.input[1].content:find("function foo() {", 1, true) ~= nil, "selection in user prompt")
  assert_true(body.input[1].content:find("```javascript", 1, true) ~= nil, "selection fenced with the buffer filetype")
end

local function test_before_and_after()
  respond = openai_text_response("/// Adds two numbers.")
  local buf = make_buffer({ "fn add(a: i32, b: i32) -> i32 {", "    a + b", "}" }, "rust")

  local result = inline_sync({
    type = "before",
    buf = buf,
    range = { start_line = 1, end_line = 3 },
    prompt = function(selected_text)
      assert_true(selected_text:find("fn add", 1, true) ~= nil, "prompt function receives the selected text")
      return "Document this function:\n" .. selected_text
    end,
  })
  assert_true(result.ok, "before failed: " .. tostring(result.error))
  assert_eq(buffer_lines(buf)[1], "/// Adds two numbers.", "before inserts above the selection")
  assert_eq(buffer_lines(buf)[2], "fn add(a: i32, b: i32) -> i32 {", "selection itself is untouched")

  respond = openai_text_response("add(1, 2);")
  result = inline_sync({
    type = "after",
    buf = buf,
    range = { start_line = 2, end_line = 4 },
    prompt = "show a usage example",
  })
  assert_true(result.ok, "after failed: " .. tostring(result.error))
  local lines = buffer_lines(buf)
  assert_eq(lines[5], "add(1, 2);", "after inserts below the selection")
end

local function test_concurrent_edit_tracking()
  respond = openai_text_response("REPLACED")
  local buf = make_buffer({ "keep me", "target line", "keep me too" })

  ---@type AIGenerateResult?
  local result
  inline.inline({
    type = "replace",
    buf = buf,
    range = { start_line = 2, end_line = 2 },
    prompt = "x",
    model = "gpt-nano",
    notify = capture_notify,
  }, function(inline_result)
    result = inline_result
  end)
  -- Shift the target while the request is in flight.
  vim.api.nvim_buf_set_lines(buf, 0, 0, false, { "inserted 1", "inserted 2" })
  assert_true(vim.wait(2000, function() return result ~= nil end, 10), "tracked inline did not complete")
  assert_true(result.ok, "tracked replace failed: " .. tostring(result.error))

  local lines = buffer_lines(buf)
  assert_eq(lines[4], "REPLACED", "replacement followed the shifted selection")
  assert_eq(lines[3], "keep me", "earlier lines untouched")
  assert_eq(lines[5], "keep me too", "later lines untouched")
end

local function test_error_paths()
  local buf = make_buffer({ "line" })

  notifications = {}
  local result = inline_sync({ type = "sideways", buf = buf, range = { start_line = 1, end_line = 1 }, prompt = "x" })
  assert_true(not result.ok and result.error:find("invalid inline type", 1, true) ~= nil, "invalid type: " .. tostring(result.error))

  result = inline_sync({ type = "replace", buf = buf, range = { start_line = 1, end_line = 5 }, prompt = "x" })
  assert_true(not result.ok and result.error:find("invalid range", 1, true) ~= nil, "invalid range: " .. tostring(result.error))

  result = inline_sync({ type = "replace", buf = buf, range = { start_line = 1, end_line = 1 }, prompt = "" })
  assert_true(not result.ok and result.error:find("prompt is required", 1, true) ~= nil, "empty prompt: " .. tostring(result.error))

  respond = function()
    return 400, { error = { message = "bad request" } }
  end
  notifications = {}
  result = inline_sync({ type = "replace", buf = buf, range = { start_line = 1, end_line = 1 }, prompt = "x" })
  assert_true(not result.ok and result.error:find("bad request", 1, true) ~= nil, "api error surfaced: " .. tostring(result.error))
  assert_eq(buffer_lines(buf)[1], "line", "buffer untouched on error")
  local warned = false
  for _, notification in ipairs(notifications) do
    if notification.level == vim.log.levels.WARN and notification.message:find("bad request", 1, true) then
      warned = true
    end
  end
  assert_true(warned, "error notification emitted")

  -- A response that is only an empty code fence survives the provider's
  -- empty check but strips to nothing.
  respond = openai_text_response("```text\n\n```")
  result = inline_sync({ type = "replace", buf = buf, range = { start_line = 1, end_line = 1 }, prompt = "x" })
  assert_true(not result.ok and result.error:find("no text", 1, true) ~= nil, "empty model output: " .. tostring(result.error))
end

local function run()
  ai.set_backend(backend)
  vim.env.OPENAI_API_KEY = "openai-test-key"

  test_replace()
  test_before_and_after()
  test_concurrent_edit_tracking()
  test_error_paths()
end

local ok, err = xpcall(run, debug.traceback)
ai.reset_backend()
vim.env.OPENAI_API_KEY = nil
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
