local M = {}

---@class GithubIssueSyncRequest
---@field database string
---@field directory string
---@field progress boolean
---@field request {repository: {hostname: string, owner: string, name: string}, scope: "open"|"all", manual: boolean, snapshot: string}
---@alias GithubIssueSyncResult {ok: true, refreshed: boolean, fetched: integer, pages: integer}|{ok: false, message: string}
---@class GithubIssueSyncOptions
---@field scope? "open"|"all"
---@field manual? boolean
---@field remember_cwd? boolean
---@field on_complete? fun(result: GithubIssueSyncResult)
---@class GithubIssueSyncContext
---@field repo string
---@field database string
---@field cwd string
---@field snapshot string
---@field progress table?
---@field on_complete? fun(result: GithubIssueSyncResult)
---@class GithubIssueDetailRequest
---@field database string
---@field directory string
---@field request {repository: {hostname: string, owner: string, name: string}, number: integer}

local detail_stale_after_seconds = 2 * 60
local progress_by_repo = {}
---@type table<string, GithubIssueSyncContext>
local in_flight = {}
local detail_in_flight = {}
local detail_prefetch_in_flight = {}
local detail_waiters = {}
local detail_memory_cache = {}
local issue_snapshot = require("github.issue_snapshot")
local notified = {}
local invalid_repos = {}
local spinner = { "⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏" }
local hostname_by_cwd = {}
---@type fun(params: GithubIssueStorageRequest, callback: fun(value: table?, failure: string?))?
local storage_runner_for_test = nil
---@type fun(params: GithubIssueSyncRequest, callback: fun(value: table?, failure: string?), progress: fun(value: table))?
local sync_runner_for_test = nil
---@type fun(params: GithubIssueDetailRequest, callback: fun(value: table?, failure: string?))?
local detail_runner_for_test = nil
local progress_enabled = true
local notifier_failed = false

---@param message string
---@param level any
---@param opts table
---@return boolean
local function safe_notify(message, level, opts)
  local function run()
    if notifier_failed then return false end
    local ok = pcall(vim.notify, message, level, opts)
    if not ok then notifier_failed = true end
    return ok
  end

  if vim.in_fast_event() then
    vim.schedule(run)
    return true
  end

  return run()
end

local function notify(message, level)
  safe_notify(message, level or vim.log.levels.ERROR, { title = "GitHub Issues" })
end

local function notify_once(key, message, level)
  if notified[key] then return end
  notified[key] = true
  notify(message, level)
end

---@param repo unknown
---@return string?
local function normalize_repo(repo)
  repo = vim.trim(tostring(repo or ""))
  repo = repo:gsub("^https://github.com/", ""):gsub("%.git$", "")
  local owner, name = repo:match("^([^/]+)/([^/]+)$")
  if not (owner and name and owner ~= "" and name ~= "") then return nil end
  return owner:lower() .. "/" .. name:lower()
end

---@param hostname string?
---@return string?
local function normalize_hostname(hostname)
  hostname = vim.trim(tostring(hostname or ""))
  hostname = hostname:gsub("^https?://", ""):gsub("/.*$", "")
  return hostname ~= "" and hostname:lower() or nil
end

---@param remote_url string?
---@return string?
local function remote_hostname(remote_url)
  local url = vim.trim(tostring(remote_url or "")):gsub("%.git$", "")
  local host = url:match("^git@([^:]+):")
    or url:match("^ssh://git@([^/]+)/")
    or url:match("^https?://([^/]+)/")
  return normalize_hostname(host)
end

---@param remote_text string?
---@return string?
local function first_remote_hostname(remote_text)
  for line in tostring(remote_text or ""):gmatch("[^\r\n]+") do
    local url = line:match("%s([^%s]+)%s+%(") or line:match("^%S+%s+([^%s]+)")
    local host = remote_hostname(url or line)
    if host then return host end
  end
  return nil
end

local function issue_dir(repo)
  local repo = normalize_repo(repo) or repo
  return vim.fs.joinpath(require("github.repo_cache").repo_dir(repo), "issues")
end

local function detail_key(repo, number)
  return M.db_path(normalize_repo(repo) or tostring(repo or "")) .. "#" .. tostring(number)
end

---@param repo string
---@param number integer|string
---@param item table
---@param fetched_at integer?
local function remember_detail(repo, number, item, fetched_at)
  local repo = normalize_repo(repo)
  local number = tonumber(number)
  if not (repo and number and type(item) == "table") then return end
  local cached_item = vim.deepcopy(item)
  cached_item.kind = cached_item.kind or "issue"
  cached_item.repo = normalize_repo(cached_item.repo or repo) or cached_item.repo or repo
  detail_memory_cache[detail_key(repo, number)] = {
    repo = repo,
    number = number,
    fetched_at = tonumber(fetched_at or os.time()) or os.time(),
    item = cached_item,
  }
end

---@param repo string
---@param number integer|string
---@param record table
---@param source "memory"|"redb"
---@return table?
local function detail_result_from_record(repo, number, record, source)
  local repo = normalize_repo(repo)
  local number = tonumber(number)
  if not (repo and number and type(record) == "table" and type(record.item) == "table") then return nil end
  local fetched_at = tonumber(record.fetched_at or 0) or 0
  local item = vim.deepcopy(record.item)
  item.kind = item.kind or "issue"
  item.repo = normalize_repo(item.repo or repo) or item.repo or repo
  return {
    ok = true,
    item = item,
    fetched_at = fetched_at,
    cached = true,
    memory = source == "memory",
    redb = source == "redb",
    stale = os.time() - fetched_at >= detail_stale_after_seconds,
  }
end

---@param output table?
---@param fallback_repo string
---@param fallback_number integer|string
---@return table?
local function remember_detail_output(output, fallback_repo, fallback_number)
  if not (type(output) == "table" and output.found == true and type(output.item) == "table") then return nil end
  local repo = normalize_repo(output.repo or fallback_repo)
  local number = tonumber(output.number or fallback_number)
  if not (repo and number) then return nil end
  remember_detail(repo, number, output.item, output.fetched_at)
  return detail_result_from_record(repo, number, detail_memory_cache[detail_key(repo, number)], "redb")
end

function M.db_path(repo)
  local repo = normalize_repo(repo) or repo
  return vim.fs.joinpath(issue_dir(repo), "issues.redb")
end

function M.snapshot_path(repo)
  local repo = normalize_repo(repo) or repo
  return vim.fs.joinpath(issue_dir(repo), "open-snapshot.json")
end

function M.log_path(repo)
  local repo = normalize_repo(repo) or repo
  return vim.fs.joinpath(issue_dir(repo), "sync.log")
end

---@param value any
---@return string
local function log_value(value)
  if value == vim.NIL then return "" end
  if type(value) == "boolean" then return value and "true" or "false" end
  if type(value) == "table" then value = vim.inspect(value) end
  return (tostring(value or ""):gsub("[\r\n\t]", " "))
end

---@param repo string?
---@param event string
---@param fields? table<string, any>
local function log_sync(repo, event, fields)
  local repo = normalize_repo(repo)
  if not repo or vim.g.github_issue_index_log == false then return end
  if vim.in_fast_event() then
    vim.schedule(function()
      log_sync(repo, event, fields)
    end)
    return
  end

  local parts = {
    os.date("!%Y-%m-%dT%H:%M:%SZ"),
    event,
  }
  local fields = fields or {}
  local keys = vim.tbl_keys(fields)
  table.sort(keys)
  for _, key in ipairs(keys) do
    parts[#parts + 1] = key .. "=" .. log_value(fields[key])
  end

  local path = M.log_path(repo)
  local ok_mkdir = pcall(vim.fn.mkdir, vim.fs.dirname(path), "p")
  local ok_write, write_result = pcall(vim.fn.writefile, { table.concat(parts, "\t") }, path, "a")
  if not (ok_mkdir and ok_write and write_result == 0) then
    notify_once("sync-log:" .. repo, "Could not write GitHub issue sync log: " .. path, vim.log.levels.WARN)
  end
end

---@param command string[]
---@param input string?
---@param cwd string?
---@param callback fun(result: table)
---@param opts? { timeout_ms?: integer, label?: string, log_repo?: string }
local function system_text_async(command, input, cwd, callback, opts)
  opts = opts or {}
  local timeout_ms = tonumber(opts.timeout_ms or 0) or 0
  local label = opts.label or table.concat(command, " ")
  local finished = false
  local timer = nil
  local process = nil
  local started_at = vim.uv.now()

  log_sync(opts.log_repo, "process:start", {
    command = table.concat(command, " "),
    cwd = cwd or "",
    label = label,
    stdin_bytes = #(input or ""),
    timeout_ms = timeout_ms,
  })

  local function finish(result)
    if finished then return end
    finished = true
    if timer then
      timer:stop()
      timer:close()
      timer = nil
    end
    vim.schedule(function()
      callback({
        code = result.code or 0,
        stdout = result.stdout or "",
        stderr = result.stderr or "",
        output = vim.trim((result.stdout or "") .. ((result.stdout or "") ~= "" and "\n" or "") .. (result.stderr or "")),
      })
    end)
    log_sync(opts.log_repo, "process:finish", {
      code = result.code or 0,
      duration_ms = vim.uv.now() - started_at,
      label = label,
      stderr_bytes = #(result.stderr or ""),
      stdout_bytes = #(result.stdout or ""),
    })
  end

  local ok, process_or_error = pcall(function()
    process = vim.system(command, {
      text = true,
      stdin = input,
      cwd = cwd,
      stdout = true,
      stderr = true,
    }, function(result)
      finish(result)
    end)
    log_sync(opts.log_repo, "process:spawned", {
      label = label,
      pid = process and process.pid or "",
    })
  end)
  if not ok then
    log_sync(opts.log_repo, "process:start-failed", {
      label = label,
      message = tostring(process_or_error),
    })
    vim.schedule(function()
      callback({ code = -1, stdout = "", stderr = tostring(process_or_error), output = tostring(process_or_error) })
    end)
    return
  end

  if timeout_ms > 0 then
    timer = vim.uv.new_timer()
    if timer then
      timer:start(timeout_ms, 0, vim.schedule_wrap(function()
        if finished then return end
        if process then pcall(function() process:kill(15) end) end
        log_sync(opts.log_repo, "process:timeout", {
          duration_ms = vim.uv.now() - started_at,
          label = label,
          timeout_ms = timeout_ms,
        })
        finish({
          code = -1,
          stdout = "",
          stderr = ("%s timed out after %ds"):format(label, math.floor(timeout_ms / 1000)),
        })
      end))
    end
  end
end

---@param cwd string?
---@param callback fun(hostname: string?, failure: string?)
local function resolve_hostname_async(cwd, callback)
  local configured = normalize_hostname(vim.g.github_issue_index_hostname)
  if configured then
    callback(configured)
    return
  end

  if sync_runner_for_test or detail_runner_for_test then
    callback(require("github.repo_cache").hostname())
    return
  end

  local key = cwd or vim.fn.getcwd()
  if hostname_by_cwd[key] ~= nil then
    callback(hostname_by_cwd[key] ~= false and hostname_by_cwd[key] or nil)
    return
  end

  system_text_async({ "git", "remote", "-v" }, nil, cwd, function(result)
    if result.code ~= 0 then
      callback(nil, "GitHub remote hostname lookup failed: " .. tostring(result.output))
      return
    end
    local host = first_remote_hostname(result.stdout)
    hostname_by_cwd[key] = host or false
    callback(host)
  end, {
    label = "GitHub remote hostname lookup",
    timeout_ms = 10000,
  })
end

---@alias GithubIssueStorageOperation
---| { operation: "detail", number: integer }
---| { operation: "details", number: integer[] }
---| { operation: "upsert_detail", number: integer, detail: table }
---| { operation: "reconcile_snapshot", state: "open", output: string }
---@class GithubIssueStorageRequest
---@field database string
---@field repo string
---@field request GithubIssueStorageOperation
---@alias GithubIssueStorageResult { ok: true, value: table }|{ ok: false, message: string }

---@param repo string
---@param operation GithubIssueStorageOperation
---@param callback fun(result: GithubIssueStorageResult)
local function request_storage(repo, operation, callback)
  local finished = false
  ---@param value unknown
  ---@param failure string?
  local function finish(value, failure)
    if finished then return end
    finished = true
    if failure then
      callback({ ok = false, message = tostring(failure) })
    elseif type(value) ~= "table" or value == vim.NIL then
      callback({ ok = false, message = "Forge issue storage returned an invalid result" })
    else
      callback({ ok = true, value = value })
    end
  end
  ---@type GithubIssueStorageRequest
  local params = { database = M.db_path(repo), repo = repo, request = operation }
  local ok, failure = pcall(function()
    if storage_runner_for_test then
      storage_runner_for_test(params, finish)
    else
      require("forge.client").request_host("github.issues", params, finish)
    end
  end)
  if not ok then
    if finished then error(failure) end
    finish(nil, "Forge issue storage request failed: " .. tostring(failure))
  end
end

---@param repo string
---@param callback? fun(result: {ok: boolean, message?: string})
---@param force? boolean
function M.reload_snapshot(repo, callback, force)
  local repo = normalize_repo(repo)
  if not repo then
    local failure = { ok = false, message = "Invalid issue snapshot repository" }
    if callback then callback(failure) else notify(failure.message) end
    return
  end
  local path = M.snapshot_path(repo)
  local database = M.db_path(repo)
  issue_snapshot.load(repo, path, function(failure)
    if callback then
      callback(failure and { ok = false, message = failure } or { ok = true })
    elseif failure and failure ~= "Snapshot load invalidated" then
      notify_once("snapshot-load:" .. repo, "Could not preload GitHub issue snapshot: " .. failure)
    end
  end, force, function(done)
    if M.snapshot_path(repo) ~= path or M.db_path(repo) ~= database then done("Snapshot load invalidated") return end
    request_storage(repo, { operation = "reconcile_snapshot", state = "open", output = path }, function(result)
      if M.snapshot_path(repo) ~= path or M.db_path(repo) ~= database then done("Snapshot load invalidated") return end
      if not result.ok then done(result.message or "Completion snapshot reconciliation failed") return end
      local recovery = result.value
      if type(recovery) ~= "table" or type(recovery.ready) ~= "boolean" or type(recovery.state) ~= "table"
        or recovery.state.repo ~= repo or type(recovery.state.revision) ~= "number"
        or recovery.state.revision < 0 or recovery.state.revision > 9007199254740991 or recovery.state.revision % 1 ~= 0 then
        done("Forge returned invalid completion snapshot recovery")
        return
      end
      done(nil, recovery.state.revision, recovery.ready)
    end)
  end)
end

local function token_terms(query)
  local terms = {}
  query = vim.trim(tostring(query or "")):lower()
  for term in query:gmatch("[%w_-]+") do
    terms[#terms + 1] = term
  end
  return terms
end

local function issue_label_text(issue)
  local parts = {}
  for _, label in ipairs(type(issue.labels) == "table" and issue.labels or {}) do
    if type(label) == "table" and type(label.name) == "string" then
      parts[#parts + 1] = label.name
    elseif type(label) == "string" then
      parts[#parts + 1] = label
    end
  end
  return table.concat(parts, " "):lower()
end

local function score_issue(issue, terms, raw_query)
  local number = tostring(issue.number or "")
  local title = tostring(issue.title or "")
  local haystack = (number .. " " .. title .. " " .. issue_label_text(issue)):lower()
  for _, term in ipairs(terms) do
    if not haystack:find(term, 1, true) then return nil end
  end
  local score = 1000
  if number == raw_query then
    score = score - 500
  elseif number:find(raw_query, 1, true) == 1 then
    score = score - 350
  elseif title:lower():find(raw_query, 1, true) == 1 then
    score = score - 250
  end
  score = score + math.min(tonumber(issue.number) or 0, 999999)
  return score
end

---@param repo string
---@param issue table
---@return table
local function issue_result(repo, issue)
  return {
    kind = "issue",
    repo = repo,
    number = tonumber(issue.number) or issue.number,
    title = tostring(issue.title or ""),
    state = tostring(issue.state or ""),
    url = tostring(issue.url or ""),
    author = tostring(issue.author or ""),
    comments_count = tonumber(issue.comments_count or issue.commentsCount or issue.comments) or 0,
    created_at = tostring(issue.created_at or issue.createdAt or ""),
    updated_at = tostring(issue.updated_at or issue.updatedAt or ""),
    is_draft = false,
    body = tostring(issue.body or ""),
    labels = vim.deepcopy(issue.labels or {}),
  }
end

---@param repo string
---@param opts? { limit?: integer }
---@return table[]
function M.list(repo, opts)
  local repo = normalize_repo(repo)
  if not repo then return {} end

  local items = {}
  local limit = opts and opts.limit or 100
  for _, issue in ipairs(issue_snapshot.records(repo)) do
    if type(issue) == "table" then
      items[#items + 1] = issue_result(repo, issue)
      if #items >= limit then break end
    end
  end
  return items
end

function M.search(repo, query, opts)
  local repo = normalize_repo(repo)
  if not repo then return {} end
  local raw_query = vim.trim(tostring(query or "")):lower()
  if raw_query == "" then return {} end
  local terms = token_terms(raw_query)
  if #terms == 0 then return {} end

  local scored = {}
  for _, issue in ipairs(issue_snapshot.records(repo)) do
    if type(issue) == "table" then
      local score = score_issue(issue, terms, raw_query)
      if score then
        scored[#scored + 1] = {
          score = score,
          issue = issue_result(repo, issue),
        }
      end
    end
  end
  table.sort(scored, function(left, right)
    if left.score ~= right.score then return left.score < right.score end
    return (tonumber(left.issue.number) or 0) < (tonumber(right.issue.number) or 0)
  end)

  local limit = opts and opts.limit or 20
  local items = {}
  for index = 1, math.min(limit, #scored) do
    items[#items + 1] = scored[index].issue
  end
  return items
end

---@param repo string
---@param number integer|string
---@param callback fun(result: table)
local function read_detail_async(repo, number, callback)
  local repo = normalize_repo(repo)
  local number = tonumber(number)
  if not (repo and number) then
    callback({ ok = false, message = "Invalid issue detail cache key" })
    return
  end
  request_storage(repo, { operation = "detail", number = number }, callback)
end

---@param repo string
---@param numbers integer[]
---@param callback fun(result: table)
local function read_details_async(repo, numbers, callback)
  local repo = normalize_repo(repo)
  if not repo then
    callback({ ok = false, message = "Invalid issue detail cache repo" })
    return
  end
  request_storage(repo, { operation = "details", number = numbers }, callback)
end

---@param repo string
---@param number integer|string
---@param item table
---@param callback fun(result: table)
local function write_detail_async(repo, number, item, callback)
  local repo = normalize_repo(repo)
  local number = tonumber(number)
  if not (repo and number) then
    callback({ ok = false, message = "Invalid issue detail cache key" })
    return
  end
  local payload = {
    repo = repo,
    number = number,
    fetched_at = os.time(),
    item = item,
  }
  request_storage(repo, { operation = "upsert_detail", number = number, detail = payload }, callback)
end

---@param key string
---@param result table
local function finish_detail_fetch(key, result)
  local waiters = detail_waiters[key] or {}
  detail_waiters[key] = nil
  detail_in_flight[key] = nil
  if not result.ok then notify("GitHub issue detail fetch failed:\n" .. tostring(result.message)) end
  for _, waiter in ipairs(waiters) do
    local accepted, failure = pcall(waiter, result)
    if not accepted then notify("GitHub issue detail callback failed:\n" .. tostring(failure)) end
  end
end

---@param cwd string?
---@param repo string
---@param number integer
local function fetch_detail_async(cwd, repo, number)
  local key = detail_key(repo, number)
  if detail_in_flight[key] then return end
  local pending = {}
  detail_in_flight[key] = pending
  local directory = cwd or vim.fn.getcwd()
  local database = M.db_path(repo)
  local completed = false
  ---@param value unknown
  ---@param failure string?
  local function complete(value, failure)
    if completed or detail_in_flight[key] ~= pending then return end
    completed = true
    if not failure and detail_key(repo, number) ~= key then failure = "GitHub cache context changed during detail fetch" end
    if not failure and (type(value) ~= "table" or type(value.item) ~= "table"
      or value.repo ~= repo or value.number ~= number or value.item.number ~= number
      or normalize_repo(value.item.repo) ~= repo or type(value.fetched_at) ~= "number") then
      failure = "Forge issue detail returned an invalid result"
    end
    if failure then
      finish_detail_fetch(key, { ok = false, message = tostring(failure) })
      return
    end
    remember_detail(repo, number, value.item, value.fetched_at)
    finish_detail_fetch(key, { ok = true, item = vim.deepcopy(value.item), fetched_at = value.fetched_at,
      cached = false, cache_updated = true })
  end
  resolve_hostname_async(directory, function(hostname, failure)
    if detail_in_flight[key] ~= pending then return end
    local cache_hostname = require("github.repo_cache").hostname()
    hostname = hostname or cache_hostname
    if failure or hostname ~= cache_hostname or M.db_path(repo) ~= database then
      complete(nil, failure or "GitHub detail hostname does not match the current cache context")
      return
    end
    local owner, name = repo:match("^([^/]+)/([^/]+)$")
    ---@type GithubIssueDetailRequest
    local params = { database = database, directory = directory,
      request = { repository = { hostname = hostname, owner = owner, name = name }, number = number } }
    local accepted, request_failure = pcall(function()
      if detail_runner_for_test then detail_runner_for_test(params, complete)
      else require("forge.client").request_host("github.detail", params, complete) end
    end)
    if not accepted then
      if completed then error(request_failure) end
      complete(nil, "Forge issue detail request failed: " .. tostring(request_failure))
    end
  end)
end

---@param cwd string?
---@param repo string
---@param number integer|string
---@param opts? { force?: boolean }
---@param callback fun(result: table)
function M.detail_async(cwd, repo, number, opts, callback)
  opts = opts or {}
  local repo = normalize_repo(repo)
  local number = tonumber(number)
  if not (repo and number and number > 0 and number <= 2147483647 and number == math.floor(number)) then
    local message = "Issue detail cache requires an owner/repo and a positive integer issue number"
    notify(message)
    callback({ ok = false, message = message })
    return
  end

  local key = detail_key(repo, number)
  if opts.force ~= true then
    local memory_result = detail_result_from_record(repo, number, detail_memory_cache[key], "memory")
    if memory_result then
      callback(memory_result)
      if not memory_result.stale then return end
      detail_waiters[key] = detail_waiters[key] or {}
      detail_waiters[key][#detail_waiters[key] + 1] = callback
      fetch_detail_async(cwd, repo, number)
      return
    end
  end

  read_detail_async(repo, number, function(cache_result)
    if detail_key(repo, number) ~= key then
      local message = "GitHub cache context changed during detail lookup"
      notify(message)
      callback({ ok = false, message = message })
      return
    end
    local should_fetch = opts.force == true
    if cache_result and cache_result.ok and cache_result.value and cache_result.value.found == true then
      local value = cache_result.value
      local fetched_at = tonumber(value.fetched_at or 0) or 0
      remember_detail(repo, number, value.item, fetched_at)
      local cached_result = detail_result_from_record(repo, number, detail_memory_cache[key], "redb")
      local age = os.time() - fetched_at
      if cached_result then
        callback(cached_result)
        should_fetch = should_fetch or age >= detail_stale_after_seconds
      else
        should_fetch = true
      end
    elseif cache_result and not cache_result.ok then
      notify("GitHub issue detail cache read failed:\n" .. tostring(cache_result.message or "unknown error"), vim.log.levels.WARN)
      should_fetch = true
    else
      should_fetch = true
    end

    if not should_fetch then return end
    detail_waiters[key] = detail_waiters[key] or {}
    detail_waiters[key][#detail_waiters[key] + 1] = callback
    fetch_detail_async(cwd, repo, number)
  end)
end

---@param repo string
---@param number integer|string
---@return table?
function M.cached_detail(repo, number)
  local repo = normalize_repo(repo)
  local number = tonumber(number)
  if not (repo and number) then return nil end
  return detail_result_from_record(repo, number, detail_memory_cache[detail_key(repo, number)], "memory")
end

---@param values table[]
---@param limit integer
---@return integer[]
local function detail_numbers(values, limit)
  local numbers = {}
  local seen = {}
  limit = math.max(0, math.floor(tonumber(limit) or 0))
  if limit == 0 then return numbers end
  for _, value in ipairs(type(values) == "table" and values or {}) do
    local number = tonumber(type(value) == "table" and value.number or value)
    if number and not seen[number] then
      seen[number] = true
      numbers[#numbers + 1] = number
      if #numbers >= limit then break end
    end
  end
  return numbers
end

---@param repo string
---@param values table[]
---@param opts? { limit?: integer, force?: boolean }
---@param callback? fun(result: table)
function M.prefetch_details(repo, values, opts, callback)
  opts = opts or {}
  local repo = normalize_repo(repo)
  if not repo then
    if callback then callback({ ok = false, message = "Invalid issue detail cache repo" }) end
    return
  end

  local numbers = {}
  for _, number in ipairs(detail_numbers(values, tonumber(opts.limit) or 100)) do
    if opts.force == true or not detail_memory_cache[detail_key(repo, number)] then numbers[#numbers + 1] = number end
  end
  if #numbers == 0 then
    if callback then callback({ ok = true, count = 0 }) end
    return
  end

  local prefetch_key = repo .. ":" .. table.concat(numbers, ",")
  if detail_prefetch_in_flight[prefetch_key] then
    if callback then callback({ ok = true, count = 0, in_flight = true }) end
    return
  end
  detail_prefetch_in_flight[prefetch_key] = true
  read_details_async(repo, numbers, function(result)
    detail_prefetch_in_flight[prefetch_key] = nil
    if not (result and result.ok) then
      notify("GitHub issue detail cache prefetch failed:\n" .. tostring(result and result.message or "unknown error"), vim.log.levels.WARN)
      if callback then callback(result or { ok = false, message = "GitHub issue detail cache prefetch failed" }) end
      return
    end

    local count = 0
    local details = result.value and result.value.details or {}
    for _, detail in ipairs(type(details) == "table" and details or {}) do
      if remember_detail_output(detail, repo, detail.number) then count = count + 1 end
    end
    if callback then callback({ ok = true, count = count }) end
  end)
end

---@param cwd string?
---@param repo string
---@param number integer|string
---@param item table
---@param callback? fun(result: table)
function M.store_detail_async(cwd, repo, number, item, callback)
  local fetched_at = os.time()
  write_detail_async(repo, number, item, function(result)
    if result and result.ok then remember_detail(repo, number, item, fetched_at) end
    if callback then
      callback(result)
      return
    end
    if not (result and result.ok) then
      notify("GitHub issue detail cache update failed:\n" .. tostring(result and result.message or "unknown error"), vim.log.levels.WARN)
    end
  end)
end

---@param progress table?
---@return string
local function progress_message(progress)
  if not progress then return "" end
  local total = tonumber(progress.total or 0) or 0
  local fetched = tonumber(progress.fetched or 0) or 0
  local count = total > 0 and ("%d/%d"):format(fetched, total) or tostring(fetched)
  local suffix = ""
  if not progress.done and progress.waiting_since then
    local elapsed = math.floor((vim.uv.now() - progress.waiting_since) / 1000)
    if elapsed >= 10 then suffix = (" (%s %ds)"):format(progress.waiting_label or "waiting", elapsed) end
  end
  local phase = progress.phase or "loading issues"
  if phase == "indexing issues" then return "Indexing issues " .. count .. suffix end
  if phase == "writing snapshot" then return "Writing issue snapshot " .. count .. suffix end
  if phase == "rate limited" then
    return "Syncing issues paused " .. count .. (progress.message and progress.message ~= "" and (" — " .. progress.message) or "")
  end
  if progress.done then
    return (progress.error and "Issue sync failed " or "Issues synced ") .. count
  end
  return "Syncing issues " .. count .. suffix
end

local stop_progress_timer

---@param progress table?
---@param level? integer
local function render_progress(progress, level)
  if not (progress and progress.active) then return end
  local ok = safe_notify(progress_message(progress), level or "info", {
    id = progress.id,
    title = "GitHub Issues",
    timeout = progress.done and progress.timeout or false,
    opts = function(notif)
      if progress.done then
        notif.icon = progress.error and "✗" or "✓"
        return
      end
      notif.icon = spinner[math.floor(vim.uv.hrtime() / (1e6 * 80)) % #spinner + 1]
    end,
  })
  if ok == false then
    log_sync(progress.repo, "notify:failed", {
      phase = progress.phase or "",
    })
    progress.active = false
    if stop_progress_timer then stop_progress_timer(progress) end
  end
end

local function start_progress_timer(progress)
  if not (progress and progress.active) or progress.timer then return end
  local timer = vim.uv.new_timer()
  if not timer then return end
  progress.timer = timer
  timer:start(0, 120, vim.schedule_wrap(function()
    render_progress(progress)
  end))
end

function stop_progress_timer(progress)
  if not (progress and progress.timer) then return end
  progress.timer:stop()
  progress.timer:close()
  progress.timer = nil
end

local function ensure_progress(repo, request_key)
  if not progress_enabled then return nil end
  if progress_by_repo[request_key] then return progress_by_repo[request_key] end
  local progress = {
    repo = repo,
    fetched = 0,
    total = nil,
    phase = "loading issues",
    id = "github_issue_index:" .. request_key,
    active = true,
  }
  progress_by_repo[request_key] = progress
  return progress
end

local function close_progress(request_key, message, level)
  local progress = progress_by_repo[request_key]
  progress_by_repo[request_key] = nil
  if not progress then return end
  stop_progress_timer(progress)
  progress.done = true
  progress.error = level == vim.log.levels.ERROR
  progress.phase = message or progress.phase
  progress.message = ""
  progress.timeout = level == vim.log.levels.ERROR and 4000 or 1200
  render_progress(progress, level)
  progress.active = false
end

local function update_progress(progress, values)
  if not progress then return end
  for key, value in pairs(values or {}) do
    progress[key] = value
  end
  start_progress_timer(progress)
  render_progress(progress)
end

---@param value unknown
---@return boolean
local function sync_count(value)
  return type(value) == "number" and value >= 0 and value <= 9007199254740991 and value == math.floor(value)
end

---@param repo string
function M.invalidate_repo(repo)
  local normalized = normalize_repo(repo)
  if not normalized then return end
  issue_snapshot.invalidate(normalized)
  local prefix = M.db_path(normalized) .. "#"
  for key in pairs(detail_memory_cache) do
    if key:sub(1, #prefix) == prefix then detail_memory_cache[key] = nil end
  end
  local pending = {}
  for key in pairs(detail_waiters) do
    if key:sub(1, #prefix) == prefix then pending[#pending + 1] = key end
  end
  for _, key in ipairs(pending) do
    finish_detail_fetch(key, { ok = false, message = "Issue detail cache invalidated by repository deletion" })
  end
end

---@param context GithubIssueSyncContext
---@param result GithubIssueSyncResult
local function finish_sync(context, result)
  if in_flight[context.database] ~= context then return end
  in_flight[context.database] = nil
  local level = result.ok and vim.log.levels.INFO or vim.log.levels.ERROR
  if not result.ok then
    local message = result.message
    if M.db_path(context.repo) == context.database and message:find("Could not resolve to a Repository", 1, true) then
      invalid_repos[context.database] = true
      local repo_cache = require("github.repo_cache")
      if repo_cache.clear_cwd_repo(context.cwd, context.repo) then
        message = message .. "\nCleared stale cached repo mapping for this cwd."
      end
      repo_cache.delete_repo(context.repo)
    end
    notify("GitHub issue sync failed for " .. context.repo .. ":\n" .. message, level)
  end
  close_progress(context.database, result.ok and "issues synced" or "sync failed", level)
  if context.on_complete then context.on_complete(result) end
end

---@param context GithubIssueSyncContext
---@param value table
local function sync_progress(context, value)
  if in_flight[context.database] ~= context then return end
  local phase = ({
    starting = "loading issues", reading = "loading issues", indexing = "indexing issues",
    publishing = "writing snapshot", rate_limited = "rate limited", complete = "issues synced", fresh = "issues synced",
  })[value.phase]
  if not phase or not sync_count(value.fetched) or not sync_count(value.pages)
    or (value.total ~= nil and value.total ~= vim.NIL and not sync_count(value.total))
    or (value.retry_after_ms ~= nil and value.retry_after_ms ~= vim.NIL and not sync_count(value.retry_after_ms)) then
    notify_once("sync-progress:" .. context.database, "Forge issue sync returned invalid progress for " .. context.repo)
    return
  end
  if not context.progress and value.phase ~= "starting" and value.phase ~= "fresh" and value.phase ~= "complete" then
    context.progress = ensure_progress(context.repo, context.database)
  end
  update_progress(context.progress, {
    phase = phase,
    fetched = value.fetched,
    total = type(value.total) == "number" and value.total or 0,
    message = type(value.retry_after_ms) == "number" and ("retry in %ds"):format(math.ceil(value.retry_after_ms / 1000)) or "",
  })
end

---@param cwd string?
---@param repo string
---@param opts? GithubIssueSyncOptions
function M.sync_repo(cwd, repo, opts)
  local normalized = normalize_repo(repo)
  opts = opts or {}
  if not normalized then
    local result = { ok = false, message = "Invalid GitHub issue sync repository: " .. tostring(repo) }
    notify(result.message)
    if opts.on_complete then opts.on_complete(result) end
    return
  end
  local database = M.db_path(normalized)
  if invalid_repos[database] and not opts.manual then
    if opts.on_complete then opts.on_complete({ ok = false, message = "Repository was not found during this session" }) end
    return
  end
  if in_flight[database] then
    local message = "GitHub issue sync is already running for " .. normalized
    if opts.manual then notify(message, vim.log.levels.INFO) end
    if opts.on_complete then opts.on_complete({ ok = false, message = message }) end
    return
  end
  if opts.manual then invalid_repos[database] = nil end
  ---@type GithubIssueSyncContext
  local context = {
    repo = normalized, database = database, cwd = cwd or vim.fn.getcwd(),
    snapshot = M.snapshot_path(normalized), on_complete = opts.on_complete,
    progress = opts.manual and ensure_progress(normalized, database) or nil,
  }
  in_flight[database] = context
  M.reload_snapshot(normalized)
  update_progress(context.progress, { phase = "loading issues", fetched = 0 })
  resolve_hostname_async(context.cwd, function(hostname, failure)
    if in_flight[database] ~= context then return end
    local cache_hostname = require("github.repo_cache").hostname()
    hostname = hostname or cache_hostname
    if failure or M.db_path(normalized) ~= database or hostname ~= cache_hostname then
      finish_sync(context, { ok = false, message = failure or
        ("GitHub sync host %s does not match cache context %s. Set GH_HOST to the intended host before syncing."):format(hostname, cache_hostname) })
      return
    end
    local owner, name = normalized:match("^([^/]+)/([^/]+)$")
    ---@type GithubIssueSyncRequest
    local params = {
      database = database, directory = context.cwd, progress = progress_enabled,
      request = {
        repository = { hostname = hostname, owner = owner, name = name },
        scope = opts.scope == "all" and "all" or "open", manual = opts.manual == true,
        snapshot = context.snapshot,
      },
    }
    local received = false
    ---@param value unknown
    ---@param request_failure string?
    local function complete(value, request_failure)
      if received or in_flight[database] ~= context then return end
      received = true
      if request_failure then
        finish_sync(context, { ok = false, message = tostring(request_failure) })
        return
      end
      if type(value) ~= "table" or type(value.refreshed) ~= "boolean" or not sync_count(value.fetched) or not sync_count(value.pages) then
        finish_sync(context, { ok = false, message = "Forge issue sync returned an invalid result" })
        return
      end
      if M.db_path(normalized) ~= database then
        finish_sync(context, { ok = false, message = "GitHub cache context changed during sync" })
        return
      end
      if value.refreshed and not context.progress then
        context.progress = ensure_progress(normalized, database)
      end
      update_progress(context.progress, { fetched = value.fetched, phase = "writing snapshot" })
      M.reload_snapshot(normalized, function(loaded)
        if M.db_path(normalized) ~= database then
          finish_sync(context, { ok = false, message = "GitHub cache context changed during snapshot refresh" })
        elseif not loaded.ok then
          finish_sync(context, { ok = false, message = "Issue sync completed but snapshot refresh failed: " .. tostring(loaded.message) })
        else
          finish_sync(context, { ok = true, refreshed = value.refreshed, fetched = value.fetched, pages = value.pages })
        end
      end, true)
    end
    local function progress(value)
      if not received then sync_progress(context, value) end
    end
    local accepted, request_failure = pcall(function()
      if sync_runner_for_test then
        sync_runner_for_test(params, complete, progress)
      else
        require("forge.client").request_host("github.sync", params, complete, progress)
      end
    end)
    if not accepted then
      if received then error(request_failure) end
      complete(nil, "Forge issue sync request failed: " .. tostring(request_failure))
    end
  end)
end

---@param cwd string?
---@param repo string
---@param opts? GithubIssueSyncOptions
function M.ensure_repo(cwd, repo, opts)
  local repo = normalize_repo(repo)
  if not repo then return end
  opts = opts or {}
  if opts.remember_cwd == true then require("github.repo_cache").remember_cwd_repo(cwd, repo) end
  M.sync_repo(cwd or vim.fn.getcwd(), repo, opts or {})
end

function M.ensure_for_buffer(buf, repo)
  buf = buf or vim.api.nvim_get_current_buf()
  local repo = normalize_repo(repo) or require("github.repo_cache").completion_repo(buf)
  if not repo then return end
  M.ensure_repo(vim.fn.getcwd(), repo, { manual = false })
end

function M.ensure_current(cwd, opts)
  cwd = cwd or vim.fn.getcwd()
  local repo_cache = require("github.repo_cache")
  local repo = repo_cache.repo_for_cwd(cwd)
  if repo then
    M.ensure_repo(cwd, repo, opts or {})
    return
  end
  require("github.gh").current_repo_async(cwd, function(result)
    if not (result and result.ok and result.repo) then
      if opts and opts.manual then notify("Could not resolve current GitHub repo:\n" .. tostring(result and result.message or "unknown error")) end
      return
    end
    repo_cache.remember_cwd_repo(cwd, result.repo)
    M.ensure_repo(cwd, result.repo, opts or {})
  end)
end

function M.sync_current(opts)
  M.ensure_current(vim.fn.getcwd(), vim.tbl_extend("force", opts or {}, { manual = true }))
end

---@param runner fun(params: GithubIssueStorageRequest, callback: fun(value: table?, failure: string?))?
function M._set_storage_runner_for_test(runner)
  storage_runner_for_test = runner
end

---@param runner fun(params: GithubIssueSyncRequest, callback: fun(value: table?, failure: string?), progress: fun(value: table))?
function M._set_sync_runner_for_test(runner)
  sync_runner_for_test = runner
end

---@param runner fun(params: GithubIssueDetailRequest, callback: fun(value: table?, failure: string?))?
function M._set_detail_runner_for_test(runner)
  detail_runner_for_test = runner
end

function M._set_progress_enabled_for_test(enabled)
  progress_enabled = enabled == true
end

function M._set_detail_stale_after_seconds_for_test(seconds)
  detail_stale_after_seconds = tonumber(seconds) or detail_stale_after_seconds
end

function M._clear_detail_memory_for_test()
  detail_prefetch_in_flight = {}
  detail_waiters = {}
  detail_memory_cache = {}
end

function M._reset_for_test()
  for _, progress in pairs(progress_by_repo) do
    stop_progress_timer(progress)
  end
  progress_by_repo = {}
  in_flight = {}
  detail_in_flight = {}
  detail_prefetch_in_flight = {}
  detail_waiters = {}
  detail_memory_cache = {}
  issue_snapshot.clear()
  notified = {}
  invalid_repos = {}
  hostname_by_cwd = {}
  storage_runner_for_test = nil
  sync_runner_for_test = nil
  detail_runner_for_test = nil
  progress_enabled = true
  detail_stale_after_seconds = 2 * 60
  notifier_failed = false
end

return M
