local M = {}

M.SERVER_PORT = 9090
M.MISMATCH_PATTERN = "expected:%s*([^%s%(]+)%s*%(([^)]*)%)%s*,%s*got:%s*([0-9a-f]+)%s*%(([^)]*)%)"
M._client_id = nil
M._heartbeat_timer = nil
M._server_job_id = nil
M._server_starting = false
M._server_waiters = {}

---@param message string
---@param level? integer
function M.notify_error(message, level)
  vim.notify(message, level or vim.log.levels.ERROR, { title = "Screenshot Review" })
end

---@return string?
function M.current_file()
  local path = vim.api.nvim_buf_get_name(0)
  if path == "" then
    M.notify_error("Current buffer has no file path", vim.log.levels.WARN)
    return nil
  end

  return path
end

---@param target string?
---@return boolean
local function is_screenshot_target(target)
  return target ~= nil and target:find("/screenshotTests", 1, true) ~= nil and target:match("%.spec%.tsx?$") ~= nil
end

---@param target string?
---@return boolean
function M.validate_screenshot_target(target)
  if is_screenshot_target(target) then
    return true
  end

  M.notify_error("Screenshot review commands must run from a screenshot spec file", vim.log.levels.WARN)
  return false
end

---@param args string[]?
---@param callback fun(result: vim.SystemCompleted)
local function git_async(args, callback)
  vim.system(vim.list_extend({ "git" }, args or {}), {
    text = true,
    stdout = true,
    stderr = true,
  }, function(result)
    vim.schedule(function()
      callback(result)
    end)
  end)
end

---@param value string
---@return string
function M.slugify(value)
  local slug = value:gsub("[^%w._-]+", "-"):gsub("^-+", ""):gsub("-+$", "")

  if slug == "" then
    return "detached"
  end

  return slug
end

---@return string
local function run_id()
  return tostring(os.time()) .. "-" .. tostring(vim.uv.hrtime())
end

---@param path string
---@return string
function M.server_url(path)
  return ("http://127.0.0.1:%d%s"):format(M.SERVER_PORT, path)
end

---@param method string
---@param path string
---@param body table?
---@param callback fun(result: table?)
function M.http_json_async(method, path, body, callback)
  local command = { "curl", "-fsS", "-X", method, M.server_url(path) }

  if body then
    vim.list_extend(command, {
      "-H",
      "content-type: application/json",
      "--data",
      vim.json.encode(body),
    })
  end

  vim.system(command, {
    text = true,
    stdout = true,
    stderr = true,
  }, function(result)
    vim.schedule(function()
      if result.code ~= 0 then
        callback(nil)
        return
      end

      local ok, decoded = pcall(vim.json.decode, result.stdout or "")
      callback(ok and decoded or nil)
    end)
  end)
end

---@return string
local function server_script_path()
  local source = debug.getinfo(1, "S").source:gsub("^@", "")
  local nvim_dir = vim.fs.dirname(vim.fs.dirname(vim.fs.dirname(source)))
  return vim.fs.joinpath(nvim_dir, "tools", "screenshot-review-server.mjs")
end

---@param ok boolean
local function finish_server_waiters(ok)
  local waiters = M._server_waiters
  M._server_waiters = {}
  M._server_starting = false

  for _, waiter in ipairs(waiters) do
    waiter(ok)
  end
end

---@param callback fun(ok: boolean)
function M.ensure_server(callback)
  M.http_json_async("GET", "/api/health", nil, function(result)
    if result and result.ok then
      callback(true)
      return
    end

    M._server_waiters[#M._server_waiters + 1] = callback
    if M._server_starting then
      return
    end

    M._server_starting = true
    local script = server_script_path()
    local stderr = {}

    M._server_job_id = vim.fn.jobstart({ "node", script }, {
      stdout_buffered = false,
      stderr_buffered = false,
      on_stdout = function(_, data)
        for _, line in ipairs(data or {}) do
          if line:find("SCREENSHOT_REVIEW_READY=", 1, true) then
            finish_server_waiters(true)
            return
          end
        end
      end,
      on_stderr = function(_, data)
        for _, line in ipairs(data or {}) do
          if line ~= "" then
            stderr[#stderr + 1] = line
          end
        end
      end,
      on_exit = function(_, code)
        if M._server_starting then
          M.notify_error(("Screenshot review server exited before ready (%s): %s"):format(code, table.concat(stderr, "\n")))
          finish_server_waiters(false)
        end
      end,
    })

    if M._server_job_id <= 0 then
      M.notify_error("Failed to start screenshot review server")
      finish_server_waiters(false)
    end
  end)
end

---@param context table
---@param callback fun(ok: boolean)
function M.post_run_reset(context, callback)
  M.http_json_async("POST", "/api/runs", {
    branch = context.branch,
    cwd = context.cwd,
    runId = context.runId,
    scope = context.scope,
    target = context.target,
    startedAt = context.started_at,
  }, function(result)
    callback(result ~= nil and result.ok == true)
  end)
end

---@param context table
---@param event table
function M.post_run_event(context, event)
  if not context.branch then
    return
  end

  M.http_json_async("POST", "/api/runs/" .. context.branch .. "/events", event, function() end)
end

---@param context table
function M.open_review_url(context)
  local url = M.server_url("/" .. context.branch)
  if vim.ui and vim.ui.open then
    vim.ui.open(url)
    return
  end

  vim.system({ "open", url }, { text = true }, function(result)
    if result.code ~= 0 then
      vim.schedule(function()
        M.notify_error("Failed to open screenshot review page: " .. tostring(result.stderr or ""))
      end)
    end
  end)
end

function M.start_heartbeat()
  if M._heartbeat_timer and not M._heartbeat_timer:is_closing() then
    return
  end

  M._heartbeat_timer = vim.uv.new_timer()
  M._heartbeat_timer:start(10000, 10000, vim.schedule_wrap(function()
    if not M._client_id then
      return
    end

    M.http_json_async("POST", "/api/clients/" .. M._client_id .. "/heartbeat", nil, function() end)
  end))
end

---@param callback fun(client_id: string?)
function M.ensure_client_lease(callback)
  if M._client_id then
    callback(M._client_id)
    return
  end

  M.http_json_async("POST", "/api/clients", nil, function(result)
    M._client_id = result and result.clientId or nil
    if M._client_id then
      M.start_heartbeat()
    end
    callback(M._client_id)
  end)
end

function M.release_client_lease()
  if M._heartbeat_timer and not M._heartbeat_timer:is_closing() then
    M._heartbeat_timer:stop()
    M._heartbeat_timer:close()
  end
  M._heartbeat_timer = nil

  if not M._client_id then
    return
  end

  local client_id = M._client_id
  M._client_id = nil
  M.http_json_async("DELETE", "/api/clients/" .. client_id, nil, function() end)
end

---@param cwd string
---@param url string
---@return table?
function M.resolve_screenshot_path(cwd, url)
  local namespace, hash = url:match("%.screenshots/([^/]+)/([0-9a-f]+)%.png")

  if not namespace or not hash then
    return nil
  end

  return {
    namespace = namespace,
    hash = hash,
    path = vim.fs.joinpath(cwd, ".screenshots", namespace, hash .. ".png"),
    url = url,
  }
end

---@param token string
---@return string?, string?
local function parse_expected_token(token)
  local tag, hash = token:match("^(.+):([0-9a-f]+)$")
  if hash then
    return hash, tag
  end

  return token:match("^[0-9a-f]+$") and token or nil, nil
end

---@param message string
---@param opts { cwd: string, test_id?: string, test_name?: string }?
---@return table[]
function M.parse_failure_message(message, opts)
  local failures = {}
  opts = opts or { cwd = vim.uv.cwd() }

  for expected_token, expected_url, actual_hash, actual_url in message:gmatch(M.MISMATCH_PATTERN) do
    local expected_hash, tag = parse_expected_token(expected_token)
    local expected = M.resolve_screenshot_path(opts.cwd, expected_url)
    local actual = M.resolve_screenshot_path(opts.cwd, actual_url)

    if expected_hash and expected and actual then
      failures[#failures + 1] = {
        id = table.concat({ opts.test_id or "unknown", expected_hash, actual_hash }, "::"),
        testId = opts.test_id,
        testName = opts.test_name,
        tag = tag,
        namespace = expected.namespace,
        expectedHash = expected_hash,
        actualHash = actual_hash,
        expectedPath = expected.path,
        actualPath = actual.path,
        expectedUrl = expected_url,
        actualUrl = actual_url,
        message = message,
      }
    end
  end

  return failures
end

---@param results table<string, table>
---@param opts { cwd: string }?
---@return table[]
function M.extract_failures_from_results(results, opts)
  local failures = {}
  opts = opts or { cwd = vim.uv.cwd() }

  for test_id, result in pairs(results or {}) do
    local messages = {}

    if type(result.short) == "string" then
      messages[#messages + 1] = result.short
    end

    if type(result.errors) == "table" then
      for _, error in ipairs(result.errors) do
        if type(error.message) == "string" then
          messages[#messages + 1] = error.message
        end
      end
    end

    for _, message in ipairs(messages) do
      vim.list_extend(failures, M.parse_failure_message(message, {
        cwd = opts.cwd,
        test_id = test_id,
        test_name = result.short,
      }))
    end
  end

  return failures
end

---@param context table
---@param event_type string
---@param payload table?
function M.emit_run_event(context, event_type, payload)
  if type(context) ~= "table" then
    return
  end

  context.events = context.events or {}
  local event = vim.tbl_extend("force", {
    type = event_type,
    runId = context.runId,
    branch = context.branch,
    at = os.time(),
  }, payload or {})
  context.events[#context.events + 1] = event

  if context.server_ready then
    M.post_run_event(context, event)
  end
end

---@param context table
function M.emit_run_start(context)
  M.emit_run_event(context, "run:start", {
    scope = context.scope,
    target = context.target,
  })
end

---@param context table
---@param failures table[]
function M.emit_run_update(context, failures)
  M.emit_run_event(context, "run:update", {
    failures = failures,
  })
end

---@param context table
---@param failures table[]
function M.emit_run_complete(context, failures)
  M.emit_run_event(context, "run:complete", {
    failures = failures,
  })
end

---@param context table
---@param message string
function M.emit_run_error(context, message)
  M.emit_run_event(context, "run:error", {
    message = message,
  })
end

---@param callback fun(slug: string)
function M.current_branch_slug(callback)
  git_async({ "branch", "--show-current" }, function(result)
    if result.code ~= 0 then
      M.notify_error("Failed to resolve git branch: " .. vim.trim(result.stderr or ""))
      callback("unknown")
      return
    end

    local branch = vim.trim(result.stdout or "")
    if branch == "" then
      git_async({ "rev-parse", "--short", "HEAD" }, function(head_result)
        local head = head_result.code == 0 and vim.trim(head_result.stdout or "") or "detached"
        callback(M.slugify(head))
      end)
      return
    end

    callback(M.slugify(branch))
  end)
end

---@param target string
---@param scope "file"|"nearest"
---@param callback fun(context: table)
function M.start_review_run(target, scope, callback)
  if not M.validate_screenshot_target(target) then
    return
  end

  M.current_branch_slug(function(branch_slug)
    local context = {
      branch = branch_slug,
      cwd = vim.uv.cwd(),
      runId = run_id(),
      scope = scope,
      target = target,
      started_at = os.time(),
    }
    vim.g.screenshot_review_active_run = context
    M.ensure_server(function(server_ok)
      if not server_ok then
        return
      end

      M.ensure_client_lease(function(client_id)
        if not client_id then
          M.notify_error("Failed to register screenshot review client lease")
          return
        end

        M.post_run_reset(context, function(reset_ok)
          if not reset_ok then
            M.notify_error("Failed to register screenshot review run")
            return
          end

          context.server_ready = true
          M.open_review_url(context)
          callback(context)
        end)
      end)
    end)
  end)
end

function M.review_file()
  local target = M.current_file()
  if not target then
    return
  end

  M.start_review_run(target, "file", function(context)
    require("neotest").run.run({ target, screenshot_review = context })
  end)
end

function M.review_nearest()
  local target = M.current_file()
  if not target then
    return
  end

  M.start_review_run(target, "nearest", function(context)
    require("neotest").run.run({ screenshot_review = context })
  end)
end

return M
