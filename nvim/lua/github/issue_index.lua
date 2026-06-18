local M = {}

local stale_after_seconds = 10 * 60
local progress_by_repo = {}
local in_flight = {}
local sync_locks = {}
local snapshot_cache = {}
local notified = {}
local invalid_repos = {}
local spinner = { "⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏" }
local hostname_by_cwd = {}
local runner_for_test = nil
local gh_runner_for_test = nil
local progress_enabled = true
local notifier_failed = false

local issue_query = table.concat({
  "query($owner:String!, $name:String!, $states:[IssueState!], $cursor:String) {",
  "  rateLimit { cost remaining resetAt }",
  "  repository(owner:$owner, name:$name) {",
  "    issues(first:100, after:$cursor, states:$states, orderBy:{field:UPDATED_AT, direction:DESC}) {",
  "      totalCount",
  "      pageInfo { hasNextPage endCursor }",
  "      nodes {",
  "          number title state url createdAt updatedAt",
  "        labels(first:50) { nodes { name color description } }",
  "      }",
  "    }",
  "  }",
  "}",
}, "\n")

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

local function normalize_repo(repo)
  repo = vim.trim(tostring(repo or ""))
  repo = repo:gsub("^https://github.com/", ""):gsub("%.git$", "")
  local owner, name = repo:match("^([^/]+)/([^/]+)$")
  if not (owner and name and owner ~= "" and name ~= "") then return nil end
  return owner:lower() .. "/" .. name:lower()
end

local function split_repo(repo)
  local normalized = normalize_repo(repo)
  if not normalized then return nil, nil end
  local owner, name = normalized:match("^([^/]+)/(.+)$")
  return owner, name
end

---@param value any
---@return string?
local function optional_string(value)
  if value == nil or value == vim.NIL then return nil end
  value = tostring(value)
  return value ~= "" and value or nil
end

---@param value any
---@return table
local function optional_table(value)
  return type(value) == "table" and value or {}
end

---@param hostname string?
---@return string?
local function normalize_hostname(hostname)
  hostname = vim.trim(tostring(hostname or ""))
  hostname = hostname:gsub("^https?://", ""):gsub("/.*$", "")
  return hostname ~= "" and hostname or nil
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
  repo = normalize_repo(repo) or repo
  return vim.fs.joinpath(require("github.repo_cache").repo_dir(repo), "issues")
end

function M.db_path(repo)
  repo = normalize_repo(repo) or repo
  return vim.fs.joinpath(issue_dir(repo), "issues.redb")
end

function M.snapshot_path(repo)
  repo = normalize_repo(repo) or repo
  return vim.fs.joinpath(issue_dir(repo), "open-snapshot.json")
end

function M.log_path(repo)
  repo = normalize_repo(repo) or repo
  return vim.fs.joinpath(issue_dir(repo), "sync.log")
end

function M.sync_lock_path(repo)
  repo = normalize_repo(repo) or repo
  return vim.fs.joinpath(issue_dir(repo), "sync.lock")
end

---@param value any
---@return string
local function log_value(value)
  if value == vim.NIL then return "" end
  if type(value) == "boolean" then return value and "true" or "false" end
  if type(value) == "table" then value = vim.inspect(value) end
  return tostring(value or ""):gsub("[\r\n\t]", " ")
end

---@param repo string?
---@param event string
---@param fields? table<string, any>
local function log_sync(repo, event, fields)
  repo = normalize_repo(repo)
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
  local keys = vim.tbl_keys(fields or {})
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

---@class GithubIssueIndexSyncLock
---@field repo string
---@field path string
---@field token string

---@return integer
local function lock_stale_after_seconds()
  return tonumber(vim.g.github_issue_index_lock_stale_seconds) or (30 * 60)
end

---@param stat table?
---@return integer?
local function stat_mtime_seconds(stat)
  return stat and stat.mtime and stat.mtime.sec or nil
end

---@param repo string
---@param path string
---@param stat table?
---@return boolean
local function remove_stale_sync_lock(repo, path, stat)
  local mtime = stat_mtime_seconds(stat)
  if not mtime then return false end
  local age_seconds = os.time() - mtime
  if age_seconds < lock_stale_after_seconds() then return false end

  log_sync(repo, "sync:lock:stale-remove", {
    age_seconds = age_seconds,
    path = path,
  })
  return vim.fn.delete(path, "rf") == 0
end

---@param repo string
---@param path string
---@param token string
local function write_sync_lock_owner(repo, path, token)
  local lines = {
    token,
    "pid=" .. tostring(vim.uv.os_getpid()),
    "started_at=" .. os.date("!%Y-%m-%dT%H:%M:%SZ"),
  }
  local ok, result = pcall(vim.fn.writefile, lines, vim.fs.joinpath(path, "owner"))
  if not (ok and result == 0) then
    log_sync(repo, "sync:lock:owner-write-failed", {
      path = path,
      result = result or "",
    })
  end
end

---@param repo string
---@return GithubIssueIndexSyncLock?
local function acquire_sync_lock(repo)
  local path = M.sync_lock_path(repo)
  vim.fn.mkdir(vim.fs.dirname(path), "p")
  local token = tostring(vim.uv.os_getpid()) .. ":" .. tostring(vim.uv.hrtime())

  for _ = 1, 2 do
    local ok, err, err_name = vim.uv.fs_mkdir(path, 448)
    if ok then
      write_sync_lock_owner(repo, path, token)
      log_sync(repo, "sync:lock:acquired", { path = path })
      return { repo = repo, path = path, token = token }
    end

    local stat = vim.uv.fs_stat(path)
    if stat and stat.type == "directory" and remove_stale_sync_lock(repo, path, stat) then
      -- Retry once after stale lock removal.
    else
      log_sync(repo, "sync:lock:busy", {
        error = err or err_name or "",
        path = path,
      })
      return nil
    end
  end

  log_sync(repo, "sync:lock:busy", { path = path })
  return nil
end

---@param path string
---@return string?
local function read_lock_token(path)
  local owner_path = vim.fs.joinpath(path, "owner")
  local ok, lines = pcall(vim.fn.readfile, owner_path, "", 1)
  if ok and type(lines) == "table" then return lines[1] end
  return nil
end

---@param lock GithubIssueIndexSyncLock?
local function release_sync_lock(lock)
  if not lock then return end
  local current = read_lock_token(lock.path)
  if current and current ~= lock.token then
    log_sync(lock.repo, "sync:lock:release-skipped", {
      path = lock.path,
    })
    return
  end

  if vim.fn.delete(lock.path, "rf") == 0 then
    log_sync(lock.repo, "sync:lock:released", { path = lock.path })
  else
    log_sync(lock.repo, "sync:lock:release-failed", { path = lock.path })
  end
end

---@param repo string
local function release_repo_sync_lock(repo)
  local lock = sync_locks[repo]
  sync_locks[repo] = nil
  release_sync_lock(lock)
end

local function executable_name()
  return vim.fn.has("win32") == 1 and "github-issue-index.exe" or "github-issue-index"
end

function M.sidecar_path()
  local configured = vim.g.github_issue_index_bin
  if type(configured) == "string" and configured ~= "" then return configured end
  local name = executable_name()
  local candidates = {
    vim.fs.joinpath(vim.fn.stdpath("data"), "gitstatus", "bin", name),
    vim.fs.joinpath(vim.fn.stdpath("config"), "rust", "github-issue-index", "target", "release", name),
    vim.fs.joinpath(vim.fn.stdpath("config"), "rust", "github-issue-index", "target", "debug", name),
  }
  for _, path in ipairs(candidates) do
    if vim.fn.executable(path) == 1 or vim.uv.fs_stat(path) ~= nil then return path end
  end
  return nil
end

---@param command string[]
---@param input string?
---@param cwd string?
---@param callback fun(result: table)
---@param opts? { timeout_ms?: integer, label?: string, log_repo?: string }
local function system_text_async(command, input, cwd, callback, opts)
  if runner_for_test then
    runner_for_test(command, input, callback, cwd)
    return
  end
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

local function gh_text_async(command, input, cwd, callback)
  if gh_runner_for_test then
    gh_runner_for_test(command, input, callback, cwd)
    return
  end
  system_text_async(command, input, cwd, callback, {
    label = "GitHub issue page request",
    timeout_ms = 120000,
  })
end

---@param cwd string?
---@param callback fun(hostname?: string)
local function resolve_hostname_async(cwd, callback)
  local configured = normalize_hostname(vim.g.github_issue_index_hostname)
  if configured then
    callback(configured)
    return
  end

  if gh_runner_for_test then
    callback(nil)
    return
  end

  local key = cwd or vim.fn.getcwd()
  if hostname_by_cwd[key] ~= nil then
    callback(hostname_by_cwd[key] ~= false and hostname_by_cwd[key] or nil)
    return
  end

  system_text_async({ "git", "remote", "-v" }, nil, cwd, function(result)
    local host = result.code == 0 and first_remote_hostname(result.stdout) or nil
    hostname_by_cwd[key] = host or false
    callback(host)
  end, {
    label = "GitHub remote hostname lookup",
    timeout_ms = 10000,
  })
end

local function run_sidecar_binary(binary, args, input, cwd, callback)
  local command = { binary }
  vim.list_extend(command, args)
  local repo = nil
  for index, arg in ipairs(args) do
    if arg == "--repo" then
      repo = args[index + 1]
      break
    end
  end
  system_text_async(command, input, cwd, function(result)
    if result.code ~= 0 then
      callback({ ok = false, message = result.output ~= "" and result.output or ("issue indexer exited " .. tostring(result.code)) })
      return
    end
    callback({ ok = true, stdout = result.stdout })
  end, {
    label = "GitHub issue indexer " .. tostring(args[1] or "request"),
    log_repo = repo,
    timeout_ms = 180000,
  })
end

local function run_sidecar(args, input, cwd, callback)
  local binary = runner_for_test and "github-issue-index" or M.sidecar_path()
  if binary then
    run_sidecar_binary(binary, args, input, cwd, callback)
    return
  end

  require("github.issue_index_builder").ensure(function(result)
    if not result.ok or not result.path then
      callback({
        ok = false,
        message = result.message
          or "GitHub issue indexer is not built and auto-build did not return a binary path",
      })
      return
    end
    run_sidecar_binary(result.path, args, input, cwd, callback)
  end)
end

local function run_sidecar_json(args, input, cwd, callback)
  run_sidecar(args, input, cwd, function(result)
    if not result.ok then
      callback(result)
      return
    end
    if result.stdout == "" then
      callback({ ok = true, value = {} })
      return
    end
    local ok, decoded = pcall(vim.json.decode, result.stdout)
    if not ok or type(decoded) ~= "table" then
      callback({ ok = false, message = "issue indexer returned invalid JSON" })
      return
    end
    callback({ ok = true, value = decoded })
  end)
end

local function snapshot_stat(repo)
  local path = M.snapshot_path(repo)
  local stat = vim.uv.fs_stat(path)
  if not stat then return nil end
  return {
    path = path,
    mtime = stat.mtime and stat.mtime.sec or 0,
    size = stat.size or 0,
  }
end

local function read_snapshot(repo)
  repo = normalize_repo(repo)
  if not repo then return nil end
  local stat = snapshot_stat(repo)
  if not stat then return nil end
  local cached = snapshot_cache[repo]
  if cached and cached.mtime == stat.mtime and cached.size == stat.size then return cached.snapshot end
  local ok, lines = pcall(vim.fn.readfile, stat.path)
  if not ok then
    notify_once("snapshot-read:" .. repo, "Could not read GitHub issue snapshot: " .. stat.path)
    return nil
  end
  local decoded_ok, decoded = pcall(vim.json.decode, table.concat(lines, "\n"))
  if not decoded_ok or type(decoded) ~= "table" then
    notify_once("snapshot-json:" .. repo, "GitHub issue snapshot contains invalid JSON: " .. stat.path)
    return nil
  end
  snapshot_cache[repo] = {
    mtime = stat.mtime,
    size = stat.size,
    snapshot = decoded,
  }
  return decoded
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
    labels = issue.labels or {},
  }
end

---@param repo string
---@return table[]
local function snapshot_issues(repo)
  local snapshot = read_snapshot(repo)
  local issues = snapshot and snapshot.issues or {}
  if type(issues) ~= "table" then return {} end
  return issues
end

---@param repo string
---@param opts? { limit?: integer }
---@return table[]
function M.list(repo, opts)
  repo = normalize_repo(repo)
  if not repo then return {} end

  local items = {}
  local limit = opts and opts.limit or 100
  for _, issue in ipairs(snapshot_issues(repo)) do
    if type(issue) == "table" then
      items[#items + 1] = issue_result(repo, issue)
      if #items >= limit then break end
    end
  end
  return items
end

function M.search(repo, query, opts)
  repo = normalize_repo(repo)
  if not repo then return {} end
  local raw_query = vim.trim(tostring(query or "")):lower()
  if raw_query == "" then return {} end
  local terms = token_terms(raw_query)
  if #terms == 0 then return {} end

  local scored = {}
  for _, issue in ipairs(snapshot_issues(repo)) do
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

local function ensure_progress(repo, manual)
  if not progress_enabled then return nil end
  if progress_by_repo[repo] then return progress_by_repo[repo] end
  local progress = {
    repo = repo,
    fetched = 0,
    total = nil,
    phase = "loading issues",
    id = "github_issue_index:" .. repo,
    active = true,
  }
  progress_by_repo[repo] = progress
  return progress
end

local function close_progress(repo, message, level)
  local progress = progress_by_repo[repo]
  progress_by_repo[repo] = nil
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

local function normalize_issue(raw, repo)
  local labels = {}
  local nodes = raw.labels and raw.labels.nodes or {}
  for _, label in ipairs(type(nodes) == "table" and nodes or {}) do
    if type(label) == "table" and type(label.name) == "string" then
      labels[#labels + 1] = {
        name = label.name,
        color = label.color,
        description = label.description,
      }
    end
  end
  return {
    repo = repo,
    number = raw.number,
    title = raw.title or "",
    state = raw.state or "",
    url = raw.url or "",
    created_at = raw.createdAt,
    updated_at = raw.updatedAt,
    body = raw.body,
    labels = labels,
  }
end

local function parse_graphql_issues(stdout, repo)
  local ok, decoded = pcall(vim.json.decode, stdout or "")
  if not ok or type(decoded) ~= "table" then return nil, "gh api graphql returned invalid JSON" end
  if type(decoded.errors) == "table" and #decoded.errors > 0 then
    return nil, vim.json.encode(decoded.errors)
  end
  local repository = decoded.data and decoded.data.repository
  local issue_connection = repository and repository.issues
  if type(issue_connection) ~= "table" then return nil, "gh api graphql returned no issue connection" end
  local issues = {}
  for _, node in ipairs(type(issue_connection.nodes) == "table" and issue_connection.nodes or {}) do
    if type(node) == "table" then issues[#issues + 1] = normalize_issue(node, repo) end
  end
  return {
    issues = issues,
    page_info = optional_table(issue_connection.pageInfo),
    total_count = issue_connection.totalCount,
    rate_limit = optional_table(decoded.data and decoded.data.rateLimit),
  }
end

local function fetch_issue_page(cwd, repo, states, cursor, callback)
  local owner, name = split_repo(repo)
  if not owner then
    callback({ ok = false, message = "Invalid GitHub repo: " .. tostring(repo) })
    return
  end
  cursor = optional_string(cursor)
  local variables = {
    owner = owner,
    name = name,
    states = states,
  }
  if cursor and cursor ~= "" then variables.cursor = cursor end
  local started_at = vim.uv.now()
  log_sync(repo, "gh:page:start", {
    cursor = cursor or "",
    states = table.concat(states or {}, ","),
  })
  resolve_hostname_async(cwd, function(hostname)
    local command = { "gh", "api" }
    if hostname then vim.list_extend(command, { "--hostname", hostname }) end
    vim.list_extend(command, { "graphql", "--input", "-" })
    gh_text_async(command, vim.json.encode({
      query = issue_query,
      variables = variables,
    }), cwd, function(result)
      log_sync(repo, "gh:page:finish", {
        code = result.code,
        duration_ms = vim.uv.now() - started_at,
        hostname = hostname or "",
        stderr_bytes = #(result.stderr or ""),
        stdout_bytes = #(result.stdout or ""),
      })
      if result.code ~= 0 then
        callback({ ok = false, message = result.output ~= "" and result.output or ("gh exited " .. tostring(result.code)) })
        return
      end
      local page, err = parse_graphql_issues(result.stdout, repo)
      if not page then
        log_sync(repo, "gh:page:parse-failed", {
          message = err or "GitHub issue sync failed",
        })
        callback({ ok = false, message = err or "GitHub issue sync failed" })
        return
      end
      local page_info = page.page_info or {}
      log_sync(repo, "gh:page:parsed", {
        end_cursor = page_info.endCursor or "",
        has_next_page = page_info.hasNextPage == true,
        issue_count = #page.issues,
        total_count = page.total_count or "",
      })
      callback({ ok = true, page = page })
    end)
  end)
end

local function is_rate_limit_message(message)
  message = tostring(message or ""):lower()
  return message:find("rate limit", 1, true) ~= nil or message:find("api rate limit exceeded", 1, true) ~= nil
end

local function is_repo_not_found_message(message)
  local text = tostring(message or "")
  return text:find("Could not resolve to a Repository", 1, true) ~= nil
    or (
      text:find('"type":"NOT_FOUND"', 1, true) ~= nil
      and text:find('"path":["repository"]', 1, true) ~= nil
    )
end

local function write_snapshot(cwd, repo, callback)
  local started_at = vim.uv.now()
  log_sync(repo, "sidecar:snapshot:start", {
    db = M.db_path(repo),
    output = M.snapshot_path(repo),
  })
  run_sidecar_json({
    "snapshot",
    "--db",
    M.db_path(repo),
    "--repo",
    repo,
    "--state",
    "open",
    "--output",
    M.snapshot_path(repo),
  }, nil, cwd, function(result)
    if result.ok then snapshot_cache[repo] = nil end
    local stat = snapshot_stat(repo)
    log_sync(repo, "sidecar:snapshot:finish", {
      duration_ms = vim.uv.now() - started_at,
      ok = result.ok == true,
      output_size = stat and stat.size or "",
      message = result.message or "",
    })
    callback(result)
  end)
end

local function upsert_page(cwd, repo, scope, payload, callback)
  local started_at = vim.uv.now()
  local payload_json = vim.json.encode(payload)
  log_sync(repo, "sidecar:upsert-page:start", {
    completed = payload.completed == true,
    cursor = payload.cursor or "",
    issue_count = #(payload.issues or {}),
    payload_bytes = #payload_json,
    scope = scope,
  })
  run_sidecar_json({
    "upsert-page",
    "--db",
    M.db_path(repo),
    "--repo",
    repo,
    "--scope",
    scope,
  }, payload_json, cwd, function(result)
    log_sync(repo, "sidecar:upsert-page:finish", {
      duration_ms = vim.uv.now() - started_at,
      ok = result.ok == true,
      message = result.message or "",
      upserted = result.value and result.value.upserted or "",
    })
    callback(result)
  end)
end

local function state_cursor(state, scope)
  if scope == "all" then return optional_string(state.all_cursor) end
  return optional_string(state.open_cursor)
end

local function historical_complete(state, scope)
  if scope == "all" then return state.all_historical_complete == true end
  return state.open_historical_complete == true
end

local function high_water(state, scope)
  if scope == "all" then return optional_string(state.all_high_water) end
  return optional_string(state.open_high_water)
end

local function checked_at(state, scope)
  if scope == "all" then return tonumber(state.last_all_checked_at or 0) or 0 end
  return tonumber(state.last_open_checked_at or 0) or 0
end

local function should_sync(repo, state, scope, manual)
  if manual then return true end
  if not snapshot_stat(repo) then return true end
  if not historical_complete(state, scope) then return true end
  return os.time() - checked_at(state, scope) >= stale_after_seconds
end

local function sync_finished(repo, progress, message)
  close_progress(repo, message or "issues synced")
  in_flight[repo] = nil
  release_repo_sync_lock(repo)
end

---@param context table
---@param message string
local function sync_failed(context, message)
  local repo = context.repo
  local progress = context.progress
  update_progress(progress, { phase = "sync failed", message = message })
  if is_repo_not_found_message(message) then
    invalid_repos[repo] = os.time()
    local repo_cache = require("github.repo_cache")
    local cleared_cwd = repo_cache.clear_cwd_repo(context.cwd, repo)
    release_repo_sync_lock(repo)
    repo_cache.delete_repo(repo)
    local suffix = cleared_cwd and "\nCleared stale cached repo mapping for this cwd." or ""
    notify_once("repo-not-found:" .. repo, "GitHub issue sync failed for " .. repo .. ":\n" .. tostring(message) .. suffix, vim.log.levels.ERROR)
  else
    notify("GitHub issue sync failed for " .. repo .. ":\n" .. tostring(message), vim.log.levels.ERROR)
  end
  close_progress(repo, "sync failed", vim.log.levels.ERROR)
  in_flight[repo] = nil
  release_repo_sync_lock(repo)
end

local function sync_pages(context)
  local repo = context.repo
  log_sync(repo, "sync:page:start", {
    cursor = context.cursor or "",
    fetched = context.fetched,
    incremental = context.incremental == true,
    total = context.total or "",
  })
  update_progress(context.progress, {
    phase = context.incremental and "refreshing issues" or "loading issues",
    waiting_label = "waiting for GitHub",
    waiting_since = vim.uv.now(),
  })
  fetch_issue_page(context.cwd, repo, context.states, context.cursor, function(result)
    if not result.ok then
      if is_rate_limit_message(result.message) then
        log_sync(repo, "sync:rate-limited", {
          message = result.message or "",
          retry_ms = 60000,
        })
        update_progress(context.progress, {
          phase = "rate limited",
          message = "Retrying in 60s: " .. tostring(result.message),
        })
        vim.defer_fn(function()
          if in_flight[repo] then sync_pages(context) end
        end, 60000)
        return
      end
      sync_failed(context, result.message or "GitHub issue fetch failed")
      return
    end

    local page = result.page
    local page_info = page.page_info or {}
    local issues = page.issues or {}
    context.fetched = context.fetched + #issues
    context.total = page.total_count or context.total
    update_progress(context.progress, {
      phase = context.incremental and "refreshing issues" or "loading issues",
      fetched = context.fetched,
      total = context.total,
      message = "",
      waiting_label = nil,
      waiting_since = nil,
    })

    local reached_high_water = false
    if context.incremental and context.high_water then
      for _, issue in ipairs(issues) do
        local updated_at = tostring(issue.updated_at or "")
        if updated_at ~= "" and updated_at <= context.high_water then
          reached_high_water = true
          break
        end
      end
    end
    local has_next_page = page_info.hasNextPage == true and not reached_high_water
    log_sync(repo, "sync:page:fetched", {
      fetched = context.fetched,
      has_next_page = has_next_page,
      issue_count = #issues,
      reached_high_water = reached_high_water,
      total = context.total or "",
    })
    local payload = {
      issues = issues,
      cursor = has_next_page and page_info.endCursor or nil,
      has_next_page = has_next_page,
      total_count = page.total_count,
      completed = not has_next_page,
      high_water = issues[1] and issues[1].updated_at or nil,
      checked_at = os.time(),
    }
    update_progress(context.progress, {
      phase = "indexing issues",
      message = ("Writing page with %d issues"):format(#issues),
      waiting_label = "indexing",
      waiting_since = vim.uv.now(),
    })
    upsert_page(context.cwd, repo, context.scope, payload, function(upsert_result)
      if not upsert_result.ok then
        sync_failed(context, upsert_result.message or "issue index upsert failed")
        return
      end
      update_progress(context.progress, {
        phase = "writing snapshot",
        message = "Refreshing local completion snapshot",
        waiting_label = "writing snapshot",
        waiting_since = vim.uv.now(),
      })
      write_snapshot(context.cwd, repo, function(snapshot_result)
        if not snapshot_result.ok then
          sync_failed(context, snapshot_result.message or "issue snapshot write failed")
          return
        end
        update_progress(context.progress, {
          phase = context.incremental and "refreshing issues" or "loading issues",
          message = "",
          waiting_label = nil,
          waiting_since = nil,
        })
        if has_next_page then
          context.cursor = optional_string(page_info.endCursor)
          local delay_ms = 150
          local rate_limit = optional_table(page.rate_limit)
          local remaining = tonumber(rate_limit.remaining)
          if remaining == 0 then
            delay_ms = 60000
            log_sync(repo, "sync:rate-budget-empty", {
              reset_at = rate_limit.resetAt or "",
              retry_ms = delay_ms,
            })
            update_progress(context.progress, {
              phase = "rate limited",
              message = "Retrying in 60s" .. (rate_limit.resetAt and ("; reset at " .. rate_limit.resetAt) or ""),
            })
          end
          vim.defer_fn(function()
            if in_flight[repo] then sync_pages(context) end
          end, delay_ms)
          return
        end
        sync_finished(repo, context.progress)
      end)
    end)
  end)
end

function M.sync_repo(cwd, repo, opts)
  repo = normalize_repo(repo)
  if not repo then return end
  opts = opts or {}
  if invalid_repos[repo] and opts.manual ~= true then return end
  if opts.manual == true then invalid_repos[repo] = nil end
  local scope = opts.scope == "all" and "all" or "open"
  if in_flight[repo] then
    if opts.manual then notify("GitHub issue sync is already running for " .. repo, vim.log.levels.INFO) end
    return
  end

  local lock = acquire_sync_lock(repo)
  if not lock then
    if opts.manual then
      notify(
        "GitHub issue sync is already running for " .. repo .. " in another Neovim instance. Using the latest local snapshot.",
        vim.log.levels.INFO
      )
    end
    return
  end
  sync_locks[repo] = lock

  run_sidecar_json({ "state", "--db", M.db_path(repo), "--repo", repo }, nil, cwd, function(state_result)
    if not state_result.ok then
      release_repo_sync_lock(repo)
      notify_once("state:" .. repo, "GitHub issue index state failed for " .. repo .. ":\n" .. tostring(state_result.message))
      return
    end
    local state = state_result.value or {}
    if not should_sync(repo, state, scope, opts.manual == true) then
      release_repo_sync_lock(repo)
      return
    end

    in_flight[repo] = true
    local progress = ensure_progress(repo, opts.manual == true)
    update_progress(progress, { phase = "loading issues", fetched = 0, total = state.open_total_count or state.all_total_count })
    local incremental = historical_complete(state, scope)
    local sync_states = incremental and { "OPEN", "CLOSED" } or (scope == "all" and { "OPEN", "CLOSED" } or { "OPEN" })
    sync_pages({
      cwd = cwd,
      repo = repo,
      scope = scope,
      states = sync_states,
      cursor = incremental and nil or state_cursor(state, scope),
      incremental = incremental,
      high_water = incremental and high_water(state, scope) or nil,
      fetched = 0,
      total = nil,
      progress = progress,
    })
  end)
end

function M.ensure_repo(cwd, repo, opts)
  repo = normalize_repo(repo)
  if not repo then return end
  opts = opts or {}
  if opts.remember_cwd == true then require("github.repo_cache").remember_cwd_repo(cwd, repo) end
  M.sync_repo(cwd or vim.fn.getcwd(), repo, opts or {})
end

function M.ensure_for_buffer(buf, repo)
  buf = buf or vim.api.nvim_get_current_buf()
  repo = normalize_repo(repo) or require("github.repo_cache").completion_repo(buf)
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

function M._set_runner_for_test(runner)
  runner_for_test = runner
end

function M._set_gh_runner_for_test(runner)
  gh_runner_for_test = runner
end

function M._set_progress_enabled_for_test(enabled)
  progress_enabled = enabled == true
end

function M._reset_for_test()
  for _, progress in pairs(progress_by_repo) do
    stop_progress_timer(progress)
  end
  for _, lock in pairs(sync_locks) do
    release_sync_lock(lock)
  end
  progress_by_repo = {}
  in_flight = {}
  sync_locks = {}
  snapshot_cache = {}
  notified = {}
  invalid_repos = {}
  hostname_by_cwd = {}
  runner_for_test = nil
  gh_runner_for_test = nil
  progress_enabled = true
  notifier_failed = false
end

return M
