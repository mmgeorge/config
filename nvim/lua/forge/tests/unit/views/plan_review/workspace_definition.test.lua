-- Unit tests for `forge.views.plan_review.workspace_definition` (workspace_definition.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["plan_workspace_definition"] = function()
  local definition = require("forge.views.plan_review.workspace_definition")
  local directory = temp.directory("workspace")
  local source_path, target_path = directory .. "/source.ts", directory .. "/target.ts"
  local contents = 'const note = "𝄞"; value.count;\r\n'
  local descriptor = { path = source_path, digest = vim.fn.sha256(contents), line = 1,
    column = assert(contents:find("count", 1, true)) - 1, filetype = "typescript" }
  local function write(path, text)
    local file = assert(io.open(path, "wb"))
    file:write(text)
    file:close()
  end
  write(source_path, contents)
  write(target_path, "export interface Value { count: number }\r\n")
  local request_count, attachment_count, cancelled, source_buffer = 0, 0, 0, nil
  local handler, submitted, delayed
  local client = { id = 951, name = "first", config = { root_dir = directory, filetypes = { "typescript" } }, offset_encoding = "utf-16" }
  function client:is_stopped() return false end
  function client:supports_method(method) return method == "textDocument/definition" end
  function client:cancel_request() cancelled = cancelled + 1 end
  function client:request(method, params, callback, buffer)
    eq(method, "textDocument/definition")
    assert(params.textDocument.uri == vim.uri_from_fname(source_path))
    assert(params.position.character == vim.str_utfindex(contents, "utf-16", descriptor.column, false))
    eq(vim.bo[buffer].fileformat, "dos")
    source_buffer, request_count, submitted = buffer, request_count + 1, callback
    if not delayed then vim.schedule(function() handler(callback) end) end
    return true, request_count
  end
  vim.lsp.get_clients = function() return { client } end
  vim.lsp.buf_is_attached = function() return false end
  vim.lsp.buf_attach_client = function(_, identity) assert(identity == client.id) attachment_count = attachment_count + 1 return true end
  local autostarts = 0
  local group = vim.api.nvim_create_augroup("PlanDefinitionAutostartTest", { clear = true })
  vim.api.nvim_create_autocmd("FileType", { group = group, callback = function() autostarts = autostarts + 1 end })
  local range = { start = { line = 0, character = 25 }, ["end"] = { line = 0, character = 30 } }
  -- Mock libuv timers let the fixture fire the lookup deadline, cancellation guard, and hidden-buffer
  -- release directly instead of waiting for them on the real clock.
  local mock_timer_list = {}
  vim.uv.new_timer = function()
    local timer = { active = false, closed = false }
    function timer:start(timeout, repeat_interval, callback)
      self.timeout, self.repeat_interval, self.callback, self.active = timeout, repeat_interval, callback, true
      return 0
    end
    function timer:stop() self.active = false return 0 end
    function timer:close() self.active, self.closed = false, true end
    function timer:is_closing() return self.closed end
    mock_timer_list[#mock_timer_list + 1] = timer
    return timer
  end
  local function fire_timers(timeout)
    local fired = 0
    for _, timer in ipairs(vim.list_slice(mock_timer_list)) do
      if timer.active and timer.timeout == timeout then
        if timer.repeat_interval == 0 then timer.active = false end
        fired = fired + 1
        timer.callback()
      end
    end
    return fired
  end
  local function request(current)
    local completed, target, failure, message = false
    local cancel = definition.request(descriptor, current or function() return true end, function(location, error, notice)
      target, failure, message, completed = location, error, notice, true
    end)
    return function()
      wait_until(function() return completed end, "definition callback did not settle", { timeout_ms = 4000 })
      return target, failure, message
    end, cancel
  end
  handler = function(callback) callback(nil, { uri = vim.uri_from_fname(target_path), range = range }) end
  local await = request()
  local target, error = await()
  assert(not error, error)
  assert(target and target.line == 1 and target.column == 25, vim.inspect(target))
  assert(vim.fs.abspath(target.path) == vim.fs.normalize(target.path), "definition path is not normalized")
  assert(vim.api.nvim_buf_is_valid(source_buffer) and #vim.fn.win_findbuf(source_buffer) == 0,
    "hidden source buffer was not retained for reuse")
  eq(fire_timers(60000), 1, { fail_reason = "released hidden source buffer has no expiry" })
  wait_until(function() return not vim.api.nvim_buf_is_valid(source_buffer) end, "request leaked its hidden source buffer")
  handler = function(callback) callback(nil, { { targetUri = vim.uri_from_fname(target_path), targetSelectionRange = range, targetRange = range } }) end
  await = request()
  assert(await())
  assert(request_count == 2 and attachment_count == 2, "a repeated jump reused a cached result")
  eq(autostarts, 0, { fail_reason = "loading hidden source triggered FileType autostart" })
  handler = function(callback) callback(nil, {}) end
  await = request()
  local empty, empty_error, notice = await()
  assert(not empty and not empty_error and notice:find("No definition", 1, true))
  handler = function(callback) callback({ message = "injected server failure" }) end
  await = request()
  local _, server_error = await()
  eq(server_error, "injected server failure")
  handler = function(callback) callback({ code = -32801, message = "content modified" }) end
  await = request()
  local retry, retry_error, retry_notice = await()
  assert(not retry and not retry_error and retry_notice:find("Retry the jump", 1, true))
  delayed = true
  local current = true
  local submitted_count = request_count
  await = request(function() return current end)
  wait_until(function() return request_count > submitted_count end, "definition request was not sent")
  current = false
  assert(fire_timers(50) >= 1, "definition lookup has no cancellation guard")
  local stale, stale_error, stale_message = await()
  assert(not stale and not stale_error and stale_message == "cancelled")
  submitted(nil, { uri = vim.uri_from_fname(target_path), range = range })
  submitted_count = request_count
  await = request()
  wait_until(function() return request_count > submitted_count end, "definition request was not sent")
  eq(fire_timers(5000), 1, { fail_reason = "definition lookup has no deadline" })
  local timed_out, timeout_error = await()
  assert(not timed_out and timeout_error:find("timed out", 1, true))
  assert(cancelled > 0, "timed out requests were not cancelled")
  delayed = false
  write(source_path, contents .. "// changed\n")
  await = request()
  local changed, changed_error, changed_notice = await()
  assert(not changed and not changed_error and changed_notice:find("Source changed", 1, true))
  write(source_path, contents)
  vim.lsp.get_clients = function() return {} end
  await = request()
  local unavailable, unavailable_error, unavailable_notice = await()
  assert(not unavailable and not unavailable_error and unavailable_notice:find("No running", 1, true))
  local other = setmetatable({ id = 952, name = "second" }, { __index = client })
  vim.lsp.get_clients = function() return { client, other } end
  await = request()
  local ambiguous, ambiguous_error, ambiguous_notice = await()
  assert(not ambiguous and not ambiguous_error and ambiguous_notice == "ambiguous_server: first, second")
  vim.lsp.get_clients = function() return { client } end
  contents = contents .. string.rep("// complete source padding\r\n", 50000)
  descriptor.digest = vim.fn.sha256(contents)
  write(source_path, contents)
  write(target_path, "export interface Value { count: number }\r\n/*"
    .. string.rep("complete target ", 1200000) .. "*/\r\n")
  handler = function(callback) callback(nil, { uri = vim.uri_from_fname(target_path), range = range }) end
  await = request()
  local large, large_error = await()
  assert(large and not large_error and large.column == 25,
    large_error or "large source or target rejected during definition lookup")
end

return T
