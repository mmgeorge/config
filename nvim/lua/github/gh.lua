---@alias GithubGhCommand string[]

---@class GithubGhAsyncResult
---@field code integer
---@field stdout string
---@field stderr string
---@field output string

---@alias GithubGhTextCallback fun(result: GithubGhAsyncResult)

---@class GithubGhBackend
---@field system_async? fun(command: GithubGhCommand, input: string?, callback: GithubGhTextCallback, cwd?: string)
---@field open_url? fun(url: string): boolean

---@class GithubGhRepository
---@field name string
---@field name_with_owner string

---@class GithubGhComment
---@field body string
---@field author string
---@field created_at string
---@field updated_at string
---@field url string

---@class GithubGhItem
---@field kind "issue"|"pr"
---@field number integer
---@field title string
---@field url string
---@field repo string
---@field author string
---@field comments_count integer
---@field updated_at string
---@field state string
---@field is_draft boolean

---@class GithubGhDetail: GithubGhItem
---@field body string
---@field created_at string
---@field labels string[]
---@field assignees string[]
---@field comments GithubGhComment[]
---@field head_ref_name? string
---@field base_ref_name? string

---@class GithubGhNotificationSubject
---@field title string
---@field type string
---@field url string
---@field latest_comment_url string

---@class GithubGhNotification
---@field id string
---@field unread boolean
---@field reason string
---@field updated_at string
---@field last_read_at string
---@field repo string
---@field subject GithubGhNotificationSubject
---@field comments_count? integer

---@class GithubGhListResult
---@field ok boolean
---@field items? GithubGhItem[]
---@field notifications? GithubGhNotification[]
---@field message? string
---@field code? integer

---@class GithubGhDetailResult
---@field ok boolean
---@field item? GithubGhDetail
---@field raw? table
---@field message? string
---@field code? integer

---@class GithubGhRepoResult
---@field ok boolean
---@field repo? string
---@field message? string
---@field code? integer

---@class GithubGhRepoContributorsResult
---@field ok boolean
---@field contributors? table[]
---@field message? string
---@field code? integer

---@class GithubGhCreateIssueResult
---@field ok boolean
---@field url? string
---@field message? string
---@field code? integer

---@class GithubGhModule
---@field _backend GithubGhBackend?

---@type GithubGhModule
local M = {}

local repo_users = require("github.repo_users")

local issue_search_fields = table.concat({
  "number",
  "title",
  "url",
  "repository",
  "author",
  "commentsCount",
  "updatedAt",
  "state",
}, ",")

local pr_search_fields = table.concat({
  "number",
  "title",
  "url",
  "repository",
  "author",
  "commentsCount",
  "updatedAt",
  "state",
  "isDraft",
}, ",")

local issue_fields = table.concat({
  "number",
  "title",
  "body",
  "url",
  "state",
  "author",
  "assignees",
  "labels",
  "comments",
  "createdAt",
  "updatedAt",
}, ",")

local pr_fields = table.concat({
  "number",
  "title",
  "body",
  "url",
  "state",
  "author",
  "assignees",
  "labels",
  "comments",
  "createdAt",
  "updatedAt",
  "headRefName",
  "baseRefName",
  "isDraft",
}, ",")

---@param backend GithubGhBackend?
function M.set_backend(backend)
  M._backend = backend
end

function M.reset_backend()
  M._backend = nil
end

---@param stdout string?
---@param stderr string?
---@return string
local function system_output(stdout, stderr)
  stdout = tostring(stdout or "")
  stderr = tostring(stderr or "")
  if stdout == "" then return stderr end
  if stderr == "" then return stdout end
  local separator = stdout:sub(-1) == "\n" and "" or "\n"
  return stdout .. separator .. stderr
end

---@param command GithubGhCommand
---@param input string?
---@param cwd string?
---@param callback GithubGhTextCallback
local function system_text_async(command, input, cwd, callback)
  local backend = M._backend
  if backend and backend.system_async then
    backend.system_async(command, input, callback, cwd)
    return
  end

  local ok, process_or_error = pcall(vim.system, command, {
    text = true,
    stdin = input,
    cwd = cwd,
    stdout = true,
    stderr = true,
  }, function(result)
    vim.schedule(function()
      local stdout = result.stdout or ""
      local stderr = result.stderr or ""
      callback({
        code = result.code or 0,
        stdout = stdout,
        stderr = stderr,
        output = system_output(stdout, stderr),
      })
    end)
  end)

  if not ok then
    vim.schedule(function()
      local message = tostring(process_or_error)
      callback({ code = -1, stdout = "", stderr = message, output = message })
    end)
  end
end

---@param value any
---@return string
local function as_string(value)
  return type(value) == "string" and value or ""
end

---@param value any
---@return integer
local function as_integer(value)
  return tonumber(value) or 0
end

---@param value any
---@return string
local function login(value)
  if type(value) == "table" then
    return as_string(value.login or value.name)
  end
  return as_string(value)
end

---@param value any
---@return string
local function repository_name(value)
  if type(value) ~= "table" then return as_string(value) end
  if type(value.nameWithOwner) == "string" then return value.nameWithOwner end
  if type(value.name_with_owner) == "string" then return value.name_with_owner end
  local owner = value.owner
  if type(owner) == "table" then owner = owner.login or owner.name end
  if type(owner) == "string" and type(value.name) == "string" then
    return owner .. "/" .. value.name
  end
  return as_string(value.full_name or value.name)
end

---@param values any
---@param key string
---@return string[]
local function names(values, key)
  local result = {}
  for _, value in ipairs(type(values) == "table" and values or {}) do
    if type(value) == "table" then
      local name = value[key] or value.name or value.login
      if type(name) == "string" and name ~= "" then result[#result + 1] = name end
    elseif type(value) == "string" and value ~= "" then
      result[#result + 1] = value
    end
  end
  return result
end

---@param raw table
---@param kind "issue"|"pr"
---@return GithubGhItem
local function normalize_search_item(raw, kind)
  return {
    kind = kind,
    number = as_integer(raw.number),
    title = as_string(raw.title),
    url = as_string(raw.url),
    repo = repository_name(raw.repository),
    author = login(raw.author),
    comments_count = as_integer(raw.commentsCount or raw.comments_count or raw.comments),
    updated_at = as_string(raw.updatedAt or raw.updated_at),
    state = as_string(raw.state),
    is_draft = raw.isDraft == true or raw.is_draft == true,
  }
end

---@param raw table
---@return GithubGhComment
local function normalize_comment(raw)
  return {
    body = as_string(raw.body),
    author = login(raw.author or raw.user),
    created_at = as_string(raw.createdAt or raw.created_at),
    updated_at = as_string(raw.updatedAt or raw.updated_at),
    url = as_string(raw.url or raw.html_url),
  }
end

---@param raw table
---@param kind "issue"|"pr"
---@param repo string?
---@return GithubGhDetail
local function normalize_detail(raw, kind, repo)
  local comments = {}
  for _, comment in ipairs(type(raw.comments) == "table" and raw.comments or {}) do
    comments[#comments + 1] = normalize_comment(comment)
  end

  return {
    kind = kind,
    number = as_integer(raw.number),
    title = as_string(raw.title),
    body = as_string(raw.body),
    url = as_string(raw.url or raw.html_url),
    repo = repo or repository_name(raw.repository),
    author = login(raw.author or raw.user),
    comments_count = #comments > 0 and #comments or as_integer(raw.comments),
    created_at = as_string(raw.createdAt or raw.created_at),
    updated_at = as_string(raw.updatedAt or raw.updated_at),
    state = as_string(raw.state),
    is_draft = raw.isDraft == true or raw.draft == true,
    labels = names(raw.labels, "name"),
    assignees = names(raw.assignees, "login"),
    comments = comments,
    head_ref_name = raw.headRefName or raw.head_ref_name,
    base_ref_name = raw.baseRefName or raw.base_ref_name,
  }
end

local result_error

---@param stdout string
---@return table?, string?
local function decode_json(stdout)
  local ok, decoded = pcall(vim.json.decode, stdout or "")
  if not ok or type(decoded) ~= "table" then
    return nil, "gh returned invalid JSON"
  end
  return decoded, nil
end

---@param cwd string?
---@param callback fun(result: GithubGhRepoResult)
function M.current_repo_async(cwd, callback)
  system_text_async({ "gh", "repo", "view", "--json", "nameWithOwner" }, nil, cwd, function(result)
    if result.code ~= 0 then
      callback({ ok = false, message = result_error(result), code = result.code })
      return
    end
    local decoded, err = decode_json(result.stdout)
    local repo = decoded and decoded.nameWithOwner or nil
    if type(repo) ~= "string" or repo == "" then
      callback({ ok = false, message = err or "gh repo view returned no nameWithOwner", code = result.code })
      return
    end
    callback({ ok = true, repo = repo })
  end)
end

---@param cwd string?
---@param repo string
---@param callback fun(result: GithubGhRepoContributorsResult)
function M.repo_contributors_async(cwd, repo, callback)
  repo_users.fetch_async({
    cwd = cwd,
    repo = repo,
    system_async = system_text_async,
    decode_json = decode_json,
    result_error = result_error,
    callback = callback,
  })
end

---@param stdout string
---@return table[], string?
local function decode_json_lines(stdout)
  local items = {}
  for _, line in ipairs(vim.split(stdout or "", "\n", { plain = true, trimempty = true })) do
    local decoded, err = decode_json(line)
    if not decoded then return {}, err end
    items[#items + 1] = decoded
  end
  return items, nil
end

---@param result GithubGhAsyncResult
---@return string
function result_error(result)
  local output = result.output or result.stderr or result.stdout or ""
  output = vim.trim(output)
  if output ~= "" then return output end
  return "gh exited with code " .. tostring(result.code)
end

---@param cwd string?
---@param command GithubGhCommand
---@param kind "issue"|"pr"
---@param callback fun(result: GithubGhListResult)
local function search_async(cwd, command, kind, callback)
  system_text_async(command, nil, cwd, function(result)
    if result.code ~= 0 then
      callback({ ok = false, message = result_error(result), code = result.code })
      return
    end

    local decoded, err = decode_json(result.stdout)
    if not decoded then
      callback({ ok = false, message = err, code = result.code })
      return
    end

    local items = {}
    for _, raw in ipairs(decoded) do
      if type(raw) == "table" then items[#items + 1] = normalize_search_item(raw, kind) end
    end
    callback({ ok = true, items = items })
  end)
end

---@param cwd string?
---@param callback fun(result: GithubGhListResult)
function M.search_issues_async(cwd, callback)
  search_async(cwd, {
    "gh",
    "search",
    "issues",
    "--involves",
    "@me",
    "--state",
    "open",
    "--json",
    issue_search_fields,
    "-L",
    "100",
  }, "issue", callback)
end

---@param repo string?
---@param query string?
---@param filter string[]?
---@param limit integer?
---@return GithubGhCommand
local function issue_search_command(repo, query, filter, limit)
  local command = { "gh", "search", "issues" }
  query = vim.trim(tostring(query or ""))
  if query ~= "" then command[#command + 1] = query end
  if repo and repo ~= "" then vim.list_extend(command, { "--repo", repo }) end
  vim.list_extend(command, filter or {})
  vim.list_extend(command, {
    "--state",
    "open",
    "--json",
    issue_search_fields,
    "-L",
    tostring(limit or 20),
  })
  return command
end

---@param items GithubGhItem[]
---@param incoming GithubGhItem[]?
local function append_unique_issue_items(items, incoming)
  local seen = {}
  for _, item in ipairs(items) do
    seen[(item.repo or ""):lower() .. "#" .. tostring(item.number)] = true
  end
  for _, item in ipairs(incoming or {}) do
    local key = (item.repo or ""):lower() .. "#" .. tostring(item.number)
    if not seen[key] then
      seen[key] = true
      items[#items + 1] = item
    end
  end
end

---@param cwd string?
---@param repo string
---@param query string
---@param callback fun(result: GithubGhListResult)
function M.search_issue_references_async(cwd, repo, query, callback)
  local searches = {
    issue_search_command(repo, query, { "--assignee", "@me" }, 10),
    issue_search_command(repo, query, { "--mentions", "@me" }, 10),
    issue_search_command(repo, query, {}, 20),
  }
  local pending = #searches
  local failures = {}
  local results_by_priority = {}

  for priority, command in ipairs(searches) do
    search_async(cwd, command, "issue", function(result)
      pending = pending - 1
      if result.ok then
        results_by_priority[priority] = result.items or {}
      else
        failures[#failures + 1] = result.message or "GitHub issue search failed"
      end
      if pending > 0 then return end

      local items = {}
      for priority_index = 1, #searches do
        append_unique_issue_items(items, results_by_priority[priority_index])
      end
      if #items > 0 or #failures < #searches then
        callback({ ok = true, items = items })
        return
      end
      callback({ ok = false, items = {}, message = table.concat(failures, "\n") })
    end)
  end
end

---@param cwd string?
---@param callback fun(result: GithubGhListResult)
function M.search_open_prs_async(cwd, callback)
  search_async(cwd, {
    "gh",
    "search",
    "prs",
    "--author",
    "@me",
    "--state",
    "open",
    "--json",
    pr_search_fields,
    "-L",
    "100",
  }, "pr", callback)
end

---@param cwd string?
---@param callback fun(result: GithubGhListResult)
function M.search_review_requests_async(cwd, callback)
  search_async(cwd, {
    "gh",
    "search",
    "prs",
    "--review-requested",
    "@me",
    "--state",
    "open",
    "--json",
    pr_search_fields,
    "-L",
    "100",
  }, "pr", callback)
end

---@param kind "issue"|"pr"
---@param number integer|string
---@param repo string?
---@return GithubGhCommand
local function detail_command(kind, number, repo)
  local fields = kind == "pr" and pr_fields or issue_fields
  local command = { "gh", kind, "view", tostring(number), "--json", fields }
  if repo and repo ~= "" then vim.list_extend(command, { "--repo", repo }) end
  return command
end

---@param cwd string?
---@param kind "issue"|"pr"
---@param number integer|string
---@param repo string?
---@param callback fun(result: GithubGhDetailResult)
local function detail_async(cwd, kind, number, repo, callback)
  system_text_async(detail_command(kind, number, repo), nil, cwd, function(result)
    if result.code ~= 0 then
      callback({ ok = false, message = result_error(result), code = result.code })
      return
    end

    local decoded, err = decode_json(result.stdout)
    if not decoded then
      callback({ ok = false, message = err, code = result.code })
      return
    end

    callback({ ok = true, item = normalize_detail(decoded, kind, repo), raw = decoded })
  end)
end

---@param cwd string?
---@param number integer|string
---@param repo string?
---@param callback fun(result: GithubGhDetailResult)
function M.issue_view_async(cwd, number, repo, callback)
  detail_async(cwd, "issue", number, repo, callback)
end

---@param cwd string?
---@param number integer|string
---@param repo string?
---@param callback fun(result: GithubGhDetailResult)
function M.pr_view_async(cwd, number, repo, callback)
  detail_async(cwd, "pr", number, repo, callback)
end

---@param cwd string?
---@param title string
---@param body string
---@param repo string?
---@param callback fun(result: GithubGhCreateIssueResult)
function M.create_issue_async(cwd, title, body, repo, callback)
  local command = { "gh", "issue", "create", "--title", title, "--body", body }
  if repo and repo ~= "" then vim.list_extend(command, { "--repo", repo }) end
  system_text_async(command, nil, cwd, function(result)
    if result.code ~= 0 then
      callback({ ok = false, message = result_error(result), code = result.code })
      return
    end

    local url = vim.trim(result.stdout or result.output or "")
    if url == "" then
      callback({ ok = false, message = "gh issue create returned no issue URL", code = result.code })
      return
    end
    callback({ ok = true, url = url })
  end)
end

---@param url string
---@return string
local function api_endpoint(url)
  return (url:gsub("^https://api%.github%.com", ""))
end

---@param raw table
---@return GithubGhNotification
local function normalize_notification(raw)
  local subject = type(raw.subject) == "table" and raw.subject or {}
  return {
    id = as_string(raw.id),
    unread = raw.unread == true,
    reason = as_string(raw.reason),
    updated_at = as_string(raw.updated_at or raw.updatedAt),
    last_read_at = as_string(raw.last_read_at or raw.lastReadAt),
    repo = repository_name(raw.repository),
    subject = {
      title = as_string(subject.title),
      type = as_string(subject.type),
      url = api_endpoint(as_string(subject.url)),
      latest_comment_url = api_endpoint(as_string(subject.latest_comment_url or subject.latestCommentUrl)),
    },
  }
end

---@param cwd string?
---@param callback fun(result: GithubGhListResult)
function M.notifications_async(cwd, callback)
  system_text_async({
    "gh",
    "api",
    "/notifications",
    "-F",
    "all=true",
    "-F",
    "participating=true",
    "--paginate",
    "--jq",
    ".[]",
  }, nil, cwd, function(result)
    if result.code ~= 0 then
      callback({ ok = false, message = result_error(result), code = result.code })
      return
    end

    local decoded, err = decode_json_lines(result.stdout)
    if err then
      callback({ ok = false, message = err, code = result.code })
      return
    end

    local notifications = {}
    for _, raw in ipairs(decoded) do
      notifications[#notifications + 1] = normalize_notification(raw)
    end
    callback({ ok = true, notifications = notifications })
  end)
end

---@param cwd string?
---@param endpoint string
---@param callback fun(result: GithubGhDetailResult)
function M.api_get_async(cwd, endpoint, callback)
  endpoint = api_endpoint(endpoint)
  system_text_async({ "gh", "api", endpoint }, nil, cwd, function(result)
    if result.code ~= 0 then
      callback({ ok = false, message = result_error(result), code = result.code })
      return
    end

    local decoded, err = decode_json(result.stdout)
    if not decoded then
      callback({ ok = false, message = err, code = result.code })
      return
    end
    callback({ ok = true, raw = decoded })
  end)
end

---@param cwd string?
---@param thread_id string
---@param callback GithubGhTextCallback
function M.notification_mark_read_async(cwd, thread_id, callback)
  system_text_async({ "gh", "api", "-X", "PATCH", "/notifications/threads/" .. thread_id }, nil, cwd, callback)
end

---@param cwd string?
---@param thread_id string
---@param callback GithubGhTextCallback
function M.notification_mark_done_async(cwd, thread_id, callback)
  system_text_async({ "gh", "api", "-X", "DELETE", "/notifications/threads/" .. thread_id }, nil, cwd, callback)
end

---@param url string
---@return boolean
function M.open_url(url)
  if url == "" then return false end
  local backend = M._backend
  if backend and backend.open_url then return backend.open_url(url) end
  if vim.ui and vim.ui.open then
    vim.ui.open(url)
    return true
  end
  return false
end

---@param subject GithubGhNotificationSubject
---@return "issue"|"pr"|nil, integer?, string?
function M.parse_subject(subject)
  local endpoint = subject and subject.url or ""
  local owner, repo, kind, number = endpoint:match("/repos/([^/]+)/([^/]+)/([^/]+)/(%d+)")
  if not owner then return nil, nil, nil end
  if kind == "issues" then return "issue", tonumber(number), owner .. "/" .. repo end
  if kind == "pulls" then return "pr", tonumber(number), owner .. "/" .. repo end
  return nil, nil, nil
end

return M
