local M = {}

local stale_after_seconds = 10 * 60
local progress_by_repo = {}
local in_flight = {}
local snapshot_cache = {}
local notified = {}
local invalid_repos = {}
local runner_for_test = nil
local gh_runner_for_test = nil
local progress_enabled = true

local issue_query = table.concat({
  "query($owner:String!, $name:String!, $states:[IssueState!], $cursor:String) {",
  "  rateLimit { cost remaining resetAt }",
  "  repository(owner:$owner, name:$name) {",
  "    issues(first:100, after:$cursor, states:$states, orderBy:{field:UPDATED_AT, direction:DESC}) {",
  "      totalCount",
  "      pageInfo { hasNextPage endCursor }",
  "      nodes {",
  "        number title state url createdAt updatedAt body",
  "        labels(first:50) { nodes { name color description } }",
  "      }",
  "    }",
  "  }",
  "}",
}, "\n")

local function notify(message, level)
  vim.notify(message, level or vim.log.levels.ERROR, { title = "GitHub Issues" })
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

local function system_text_async(command, input, cwd, callback)
  if runner_for_test then
    runner_for_test(command, input, callback, cwd)
    return
  end
  local ok, process_or_error = pcall(vim.system, command, { text = true, stdin = input, cwd = cwd }, function(result)
    vim.schedule(function()
      callback({
        code = result.code or 0,
        stdout = result.stdout or "",
        stderr = result.stderr or "",
        output = vim.trim((result.stdout or "") .. ((result.stdout or "") ~= "" and "\n" or "") .. (result.stderr or "")),
      })
    end)
  end)
  if not ok then
    vim.schedule(function()
      callback({ code = -1, stdout = "", stderr = tostring(process_or_error), output = tostring(process_or_error) })
    end)
  end
end

local function gh_text_async(command, input, cwd, callback)
  if gh_runner_for_test then
    gh_runner_for_test(command, input, callback, cwd)
    return
  end
  local ok, process_or_error = pcall(vim.system, command, { text = true, stdin = input, cwd = cwd }, function(result)
    vim.schedule(function()
      callback({
        code = result.code or 0,
        stdout = result.stdout or "",
        stderr = result.stderr or "",
        output = vim.trim((result.stdout or "") .. ((result.stdout or "") ~= "" and "\n" or "") .. (result.stderr or "")),
      })
    end)
  end)
  if not ok then
    vim.schedule(function()
      callback({ code = -1, stdout = "", stderr = tostring(process_or_error), output = tostring(process_or_error) })
    end)
  end
end

local function run_sidecar(args, input, cwd, callback)
  local binary = runner_for_test and "github-issue-index" or M.sidecar_path()
  if not binary then
    callback({
      ok = false,
      message = "GitHub issue indexer is not built. Run: cargo build --manifest-path nvim/rust/github-issue-index/Cargo.toml --release",
    })
    return
  end
  local command = { binary }
  vim.list_extend(command, args)
  system_text_async(command, input, cwd, function(result)
    if result.code ~= 0 then
      callback({ ok = false, message = result.output ~= "" and result.output or ("issue indexer exited " .. tostring(result.code)) })
      return
    end
    callback({ ok = true, stdout = result.stdout })
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

local function progress_window_config(width, height)
  return {
    relative = "editor",
    width = width,
    height = height,
    row = math.max(0, math.floor((vim.o.lines - height) / 2)),
    col = math.max(0, math.floor((vim.o.columns - width) / 2)),
    style = "minimal",
    border = "rounded",
    title = " GitHub Issues ",
    title_pos = "center",
  }
end

local function ensure_progress(repo, manual)
  if not progress_enabled then return nil end
  if progress_by_repo[repo] then return progress_by_repo[repo] end
  if not manual and snapshot_stat(repo) then return nil end
  local progress = { repo = repo, fetched = 0, total = nil, phase = "loading issues" }
  local ok, buf = pcall(vim.api.nvim_create_buf, false, true)
  if not ok then return nil end
  progress.buf = buf
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].bufhidden = "wipe"
  vim.bo[buf].swapfile = false
  local width = 54
  local height = 4
  local win_ok, win = pcall(vim.api.nvim_open_win, buf, false, progress_window_config(width, height))
  if win_ok then progress.win = win end
  progress_by_repo[repo] = progress
  return progress
end

local function render_progress(progress)
  if not (progress and progress.buf and vim.api.nvim_buf_is_valid(progress.buf)) then return end
  local width = 24
  local total = tonumber(progress.total or 0) or 0
  local fetched = tonumber(progress.fetched or 0) or 0
  local filled = total > 0 and math.min(width, math.floor((fetched / total) * width)) or 0
  local bar = string.rep("=", filled) .. string.rep("-", width - filled)
  local count = total > 0 and ("%d/%d"):format(fetched, total) or tostring(fetched)
  local lines = {
    progress.repo,
    ("%s [%s] %s"):format(progress.phase or "loading issues", bar, count),
    progress.message or "",
    "Completion uses the latest local snapshot while this runs.",
  }
  pcall(vim.api.nvim_buf_set_lines, progress.buf, 0, -1, false, lines)
end

local function close_progress(repo, message, level)
  local progress = progress_by_repo[repo]
  progress_by_repo[repo] = nil
  if not progress then return end
  if message then
    progress.phase = message
    progress.message = ""
    render_progress(progress)
  end
  vim.defer_fn(function()
    if progress.win and vim.api.nvim_win_is_valid(progress.win) then pcall(vim.api.nvim_win_close, progress.win, true) end
    if progress.buf and vim.api.nvim_buf_is_valid(progress.buf) then pcall(vim.api.nvim_buf_delete, progress.buf, { force = true }) end
  end, level == vim.log.levels.ERROR and 4000 or 1200)
end

local function update_progress(progress, values)
  if not progress then return end
  for key, value in pairs(values or {}) do
    progress[key] = value
  end
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
    page_info = issue_connection.pageInfo or {},
    total_count = issue_connection.totalCount,
    rate_limit = decoded.data and decoded.data.rateLimit or {},
  }
end

local function fetch_issue_page(cwd, repo, states, cursor, callback)
  local owner, name = split_repo(repo)
  if not owner then
    callback({ ok = false, message = "Invalid GitHub repo: " .. tostring(repo) })
    return
  end
  local variables = {
    owner = owner,
    name = name,
    states = states,
  }
  if cursor and cursor ~= "" then variables.cursor = cursor end
  gh_text_async({ "gh", "api", "graphql", "--input", "-" }, vim.json.encode({
    query = issue_query,
    variables = variables,
  }), cwd, function(result)
    if result.code ~= 0 then
      callback({ ok = false, message = result.output ~= "" and result.output or ("gh exited " .. tostring(result.code)) })
      return
    end
    local page, err = parse_graphql_issues(result.stdout, repo)
    if not page then
      callback({ ok = false, message = err or "GitHub issue sync failed" })
      return
    end
    callback({ ok = true, page = page })
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
    callback(result)
  end)
end

local function upsert_page(cwd, repo, scope, payload, callback)
  run_sidecar_json({
    "upsert-page",
    "--db",
    M.db_path(repo),
    "--repo",
    repo,
    "--scope",
    scope,
  }, vim.json.encode(payload), cwd, callback)
end

local function state_cursor(state, scope)
  if scope == "all" then return state.all_cursor end
  return state.open_cursor
end

local function historical_complete(state, scope)
  if scope == "all" then return state.all_historical_complete == true end
  return state.open_historical_complete == true
end

local function high_water(state, scope)
  if scope == "all" then return state.all_high_water end
  return state.open_high_water
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
    repo_cache.delete_repo(repo)
    local suffix = cleared_cwd and "\nCleared stale cached repo mapping for this cwd." or ""
    notify_once("repo-not-found:" .. repo, "GitHub issue sync failed for " .. repo .. ":\n" .. tostring(message) .. suffix, vim.log.levels.ERROR)
  else
    notify("GitHub issue sync failed for " .. repo .. ":\n" .. tostring(message), vim.log.levels.ERROR)
  end
  close_progress(repo, "sync failed", vim.log.levels.ERROR)
  in_flight[repo] = nil
end

local function sync_pages(context)
  local repo = context.repo
  fetch_issue_page(context.cwd, repo, context.states, context.cursor, function(result)
    if not result.ok then
      if is_rate_limit_message(result.message) then
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
    local payload = {
      issues = issues,
      cursor = has_next_page and page_info.endCursor or nil,
      has_next_page = has_next_page,
      total_count = page.total_count,
      completed = not has_next_page,
      high_water = issues[1] and issues[1].updated_at or nil,
      checked_at = os.time(),
    }
    upsert_page(context.cwd, repo, context.scope, payload, function(upsert_result)
      if not upsert_result.ok then
        sync_failed(context, upsert_result.message or "issue index upsert failed")
        return
      end
      write_snapshot(context.cwd, repo, function(snapshot_result)
        if not snapshot_result.ok then
          sync_failed(context, snapshot_result.message or "issue snapshot write failed")
          return
        end
        if has_next_page then
          context.cursor = page_info.endCursor
          local delay_ms = 150
          local remaining = tonumber(page.rate_limit and page.rate_limit.remaining or nil)
          if remaining == 0 then
            delay_ms = 60000
            update_progress(context.progress, {
              phase = "rate limited",
              message = "Retrying in 60s"
                .. (page.rate_limit and page.rate_limit.resetAt and ("; reset at " .. page.rate_limit.resetAt) or ""),
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

  run_sidecar_json({ "state", "--db", M.db_path(repo), "--repo", repo }, nil, cwd, function(state_result)
    if not state_result.ok then
      notify_once("state:" .. repo, "GitHub issue index state failed for " .. repo .. ":\n" .. tostring(state_result.message))
      return
    end
    local state = state_result.value or {}
    if not should_sync(repo, state, scope, opts.manual == true) then return end

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
  progress_by_repo = {}
  in_flight = {}
  snapshot_cache = {}
  notified = {}
  invalid_repos = {}
  runner_for_test = nil
  gh_runner_for_test = nil
  progress_enabled = true
end

function M._progress_window_config_for_test(width, height)
  return progress_window_config(width, height)
end

return M
