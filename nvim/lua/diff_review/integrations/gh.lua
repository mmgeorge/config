---@alias DiffReviewGhCommand string[]

---@class DiffReviewGhAsyncResult
---@field code integer
---@field stdout string
---@field stderr string
---@field output string

---@alias DiffReviewGhTextCallback fun(result: DiffReviewGhAsyncResult)

---@class DiffReviewGhBackend
---@field system_async? fun(command: DiffReviewGhCommand, input: string?, cb: DiffReviewGhTextCallback, cwd?: string)
---@field open_url? fun(url: string): boolean

---@class DiffReviewGhPRFile
---@field path string
---@field additions integer
---@field deletions integer
---@field changeType? string
---@field status? string

---@class DiffReviewGhRequestedReviewer
---@field login string
---@field is_code_owner? boolean
---@field kind? string

---@class DiffReviewGhMilestone
---@field number integer
---@field title string
---@field state? string
---@field url? string

---@class DiffReviewGhPRCommit
---@field oid string
---@field messageHeadline? string
---@field committedDate? string
---@field authoredDate? string

---@class DiffReviewGhPR
---@field id? string
---@field number integer
---@field title string
---@field body string
---@field url string
---@field repo? string
---@field headRefName string
---@field headRefOid? string
---@field commits DiffReviewGhPRCommit[]
---@field files DiffReviewGhPRFile[]
---@field changedFiles integer
---@field additions integer
---@field deletions integer
---@field requestedReviewers? DiffReviewGhRequestedReviewer[]
---@field milestone? DiffReviewGhMilestone
---@field isDraft boolean
---@field state string
---@field createdAt? string
---@field updatedAt? string
---@field closedAt? string

---@class DiffReviewGhPRResult
---@field ok boolean
---@field pr? DiffReviewGhPR
---@field message? string
---@field code? integer
---@field unavailable? boolean

---@class DiffReviewGhPRListResult
---@field ok boolean
---@field prs? DiffReviewGhPR[]
---@field message? string
---@field code? integer
---@field unavailable? boolean

---@class DiffReviewGhPRCheck
---@field name string
---@field status string
---@field conclusion? string
---@field state string
---@field url? string
---@field workflow_name? string
---@field started_at? string
---@field completed_at? string

---@class DiffReviewGhPRChecksResult
---@field ok boolean
---@field checks? DiffReviewGhPRCheck[]
---@field message? string
---@field code? integer

---@class DiffReviewGhReviewCommentReply
---@field remote_id integer?
---@field remote_node_id string?
---@field review_id integer?
---@field review_node_id string?
---@field review_state string?
---@field body string
---@field viewer_did_author? boolean
---@field user? string
---@field created_at? string
---@field updated_at? string
---@field url? string

---@class DiffReviewGhPendingReviewComment
---@field remote_id integer?
---@field remote_node_id string?
---@field review_id integer?
---@field review_node_id string?
---@field review_state string?
---@field body string
---@field viewer_did_author? boolean
---@field path string
---@field line integer?
---@field start_line integer?
---@field position integer?
---@field side string?
---@field user? string
---@field created_at? string
---@field updated_at? string
---@field url? string
---@field resolved? boolean
---@field outdated? boolean
---@field replies? DiffReviewGhReviewCommentReply[]

---@class DiffReviewGhIssueComment
---@field remote_id integer?
---@field remote_node_id string?
---@field body string
---@field viewer_did_author? boolean
---@field user? string
---@field created_at? string
---@field updated_at? string
---@field url? string

---@class DiffReviewGhSubmittedReview
---@field id integer
---@field node_id string
---@field state string
---@field body string
---@field viewer_did_author? boolean
---@field user? string
---@field created_at? string
---@field updated_at? string
---@field submitted_at? string
---@field commit_id? string
---@field url? string
---@field comments? DiffReviewGhPendingReviewComment[]

---@class DiffReviewGhPRCommentsResult
---@field ok boolean
---@field reviews? DiffReviewGhSubmittedReview[]
---@field code_comments? DiffReviewGhPendingReviewComment[]
---@field issue_comments? DiffReviewGhIssueComment[]
---@field message? string
---@field code? integer

---@class DiffReviewGhReviewReplyResult
---@field ok boolean
---@field reply? DiffReviewGhReviewCommentReply
---@field message? string
---@field code? integer

---@class DiffReviewGhPendingReview
---@field id integer
---@field node_id string
---@field state string
---@field body string
---@field viewer_did_author? boolean
---@field commit_id string?
---@field user? string

---@class DiffReviewGhPendingReviewResult
---@field ok boolean
---@field review? DiffReviewGhPendingReview
---@field comments? DiffReviewGhPendingReviewComment[]
---@field message? string
---@field code? integer

---@class DiffReviewGhPrDraftResult
---@field ok boolean
---@field is_draft? boolean
---@field message? string
---@field code? integer

---@class DiffReviewGhPrStateResult
---@field ok boolean
---@field state? string
---@field is_draft? boolean
---@field message? string
---@field code? integer

---@class DiffReviewGhModule
---@field _backend DiffReviewGhBackend?

---@type DiffReviewGhModule
local M = {}

local repo_users = require("github.repo_users")

local default_timeout_ms = 5000
local timeout_ms = default_timeout_ms

local pr_json_fields = table.concat({
  "id",
  "number",
  "title",
  "body",
  "url",
  "headRefName",
  "headRefOid",
  "commits",
  "files",
  "changedFiles",
  "additions",
  "deletions",
  "reviewRequests",
  "milestone",
  "isDraft",
  "state",
  "createdAt",
  "updatedAt",
  "closedAt",
}, ",")

local pr_list_json_fields = table.concat({
  "id",
  "number",
  "title",
  "body",
  "url",
  "headRefName",
  "headRefOid",
  "state",
  "isDraft",
  "createdAt",
  "updatedAt",
  "closedAt",
}, ",")

---@param backend DiffReviewGhBackend?
function M.set_backend(backend)
  M._backend = backend
end

function M.reset_backend()
  M._backend = nil
end

---@param value integer
function M.set_timeout_ms(value)
  timeout_ms = math.max(1, tonumber(value) or default_timeout_ms)
end

function M.reset_timeout_ms()
  timeout_ms = default_timeout_ms
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

---@param command DiffReviewGhCommand
---@param input string?
---@param cwd string?
---@param cb DiffReviewGhTextCallback
local function system_text_async(command, input, cwd, cb)
  local backend = M._backend
  if backend and backend.system_async then
    backend.system_async(command, input, cb, cwd)
    return
  end

  local completed = false
  local process
  local command_timeout_ms = timeout_ms

  ---@param result DiffReviewGhAsyncResult
  local function finish(result)
    if completed then return end
    completed = true
    cb(result)
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
      finish({
        code = result.code or 0,
        stdout = stdout,
        stderr = stderr,
        output = system_output(stdout, stderr),
      })
    end)
  end)
  if ok then
    process = process_or_error
    vim.defer_fn(function()
      if completed then return end
      local message = "gh command timed out after " .. tostring(command_timeout_ms) .. "ms"
      if process and process.kill then pcall(process.kill, process, 15) end
      finish({
        code = -1,
        stdout = "",
        stderr = message,
        output = message,
      })
    end, command_timeout_ms)
  end
  if not ok then
    vim.schedule(function()
      local message = tostring(process_or_error)
      finish({
        code = -1,
        stdout = "",
        stderr = message,
        output = message,
      })
    end)
  end
end

---@param value any
---@return integer
local function as_integer(value)
  return tonumber(value) or 0
end

---@param url? string
---@return string?
function M.repo_from_pr_url(url)
  local owner, repo = tostring(url or ""):match("^https://[^/]+/([^/]+)/([^/]+)/pull/%d+")
  if owner and repo and owner ~= "" and repo ~= "" then return owner .. "/" .. repo end
  return nil
end

---@param text string
---@return table?
local function decode_json(text)
  local ok, decoded = pcall(vim.json.decode, tostring(text or ""))
  if ok and type(decoded) == "table" then return decoded end
  return nil
end

---@param raw table
---@param fallback? string[]
---@return DiffReviewGhRequestedReviewer[]
local function normalize_requested_reviewers(raw, fallback)
  local reviewers = {}
  local seen = {}

  ---@param login any
  ---@param metadata? table
  local function add_login(login, metadata)
    local username = tostring(login or ""):gsub("^@", "")
    if username == "" then return end
    local key = username:lower()
    local existing_index = seen[key]
    if existing_index then
      local existing = reviewers[existing_index]
      if metadata and metadata.is_code_owner then existing.is_code_owner = true end
      if metadata and metadata.kind and not existing.kind then existing.kind = metadata.kind end
      return
    end
    seen[key] = #reviewers + 1
    reviewers[#reviewers + 1] = {
      login = username,
      is_code_owner = metadata and metadata.is_code_owner or nil,
      kind = metadata and metadata.kind or nil,
    }
  end

  ---@param value any
  ---@param inherited? table
  ---@return table
  local function reviewer_metadata(value, inherited)
    local metadata = {
      is_code_owner = inherited and inherited.is_code_owner or nil,
      kind = inherited and inherited.kind or nil,
    }
    if type(value) ~= "table" then return metadata end
    if value.is_code_owner == true
      or value.isCodeOwner == true
      or value.asCodeOwner == true
      or value.codeOwner == true
      or tostring(value.reason or ""):upper() == "CODEOWNER"
      or tostring(value.source or ""):upper() == "CODEOWNER"
    then
      metadata.is_code_owner = true
    end
    local kind = value.kind or value.type or value.__typename
    if type(kind) == "string" and kind ~= "" then metadata.kind = kind end
    return metadata
  end

  ---@param value any
  ---@param inherited? table
  local function collect(value, inherited)
    local metadata = reviewer_metadata(value, inherited)
    if type(value) == "string" then
      add_login(value, metadata)
      return
    end
    if type(value) ~= "table" then return end
    if type(value.login) == "string" then
      add_login(value.login, metadata)
      return
    end
    if type(value.slug) == "string" then
      add_login(value.slug, metadata)
      return
    end
    if value.requestedReviewer then collect(value.requestedReviewer, metadata) end
    if value.requested_reviewer then collect(value.requested_reviewer, metadata) end
    if type(value.nodes) == "table" then collect(value.nodes, metadata) end
    if type(value.edges) == "table" then collect(value.edges, metadata) end
    if type(value.node) == "table" then collect(value.node, metadata) end
    for _, item in ipairs(value) do
      collect(item, metadata)
    end
  end

  if type(raw) == "table" then
    collect(raw.requested_reviewers)
    collect(raw.requestedReviewers)
    collect(raw.reviewRequests)
  end
  if #reviewers == 0 then collect(fallback or {}) end
  return reviewers
end

---@param raw table?
---@return DiffReviewGhMilestone?
local function normalize_milestone(raw)
  if type(raw) ~= "table" then return nil end
  local title = tostring(raw.title or "")
  if title == "" then return nil end
  return {
    number = as_integer(raw.number),
    title = title,
    state = raw.state,
    url = raw.html_url or raw.url,
  }
end

---@param decoded any
---@return DiffReviewGhMilestone[]
local function normalize_milestones(decoded)
  local milestones = {}
  local function collect(value)
    if type(value) ~= "table" then return end
    local milestone = normalize_milestone(value)
    if milestone then
      milestones[#milestones + 1] = milestone
      return
    end
    for _, item in ipairs(value) do
      collect(item)
    end
  end
  collect(decoded)
  return milestones
end

---@param value any
---@return string
local function normalize_body_text(value)
  return (tostring(value or ""):gsub("\r\n", "\n"):gsub("\r", "\n"))
end

---@param raw table
---@param repo? string
---@return DiffReviewGhPR
local function normalize_pr(raw, repo)
  local files = {}
  for _, file in ipairs(type(raw.files) == "table" and raw.files or {}) do
    files[#files + 1] = {
      path = tostring(file.path or file.filename or ""),
      additions = as_integer(file.additions),
      deletions = as_integer(file.deletions),
      changeType = file.changeType,
      status = file.status,
    }
  end

  local commits = {}
  for _, commit in ipairs(type(raw.commits) == "table" and raw.commits or {}) do
    commits[#commits + 1] = {
      oid = tostring(commit.oid or ""),
      messageHeadline = commit.messageHeadline,
      committedDate = commit.committedDate,
      authoredDate = commit.authoredDate,
    }
  end

  local normalized_repo = repo or M.repo_from_pr_url(raw.url)
  local state = tostring(raw.state or ""):upper()
  if state == "" then state = raw.closed == true and "CLOSED" or "OPEN" end
  return {
    id = type(raw.id) == "string" and raw.id or nil,
    number = as_integer(raw.number),
    title = tostring(raw.title or ""),
    body = normalize_body_text(raw.body),
    url = tostring(raw.url or ""),
    repo = normalized_repo,
    headRefName = tostring(raw.headRefName or ""),
    headRefOid = raw.headRefOid,
    commits = commits,
    files = files,
    changedFiles = as_integer(raw.changedFiles),
    additions = as_integer(raw.additions),
    deletions = as_integer(raw.deletions),
    requestedReviewers = normalize_requested_reviewers(raw),
    milestone = normalize_milestone(raw.milestone),
    isDraft = raw.isDraft == true or raw.is_draft == true,
    state = state,
    createdAt = raw.createdAt or raw.created_at,
    updatedAt = raw.updatedAt or raw.updated_at,
    closedAt = raw.closedAt or raw.closed_at,
  }
end

---@param number? integer|string
---@param repo? string
---@return DiffReviewGhCommand
local function pr_view_command(number, repo)
  local command = { "gh", "pr", "view" }
  if number and tostring(number) ~= "" then
    command[#command + 1] = tostring(number)
  end
  vim.list_extend(command, { "--json", pr_json_fields })
  if repo and repo ~= "" then vim.list_extend(command, { "--repo", repo }) end
  return command
end

---@param text string
---@return boolean
local function is_no_pr_output(text)
  local normalized = text:lower()
  return normalized:find("no pull request", 1, true) ~= nil
    or normalized:find("no pull requests", 1, true) ~= nil
    or normalized:find("could not find a pull request", 1, true) ~= nil
end

---@param text string
---@return boolean
local function is_pr_lookup_unavailable_output(text)
  local normalized = text:lower()
  return normalized:find("none of the git remotes configured for this repository correspond to the gh_host environment variable", 1, true) ~= nil
    or normalized:find("try adding a matching remote or unsetting the variable", 1, true) ~= nil
end

---@param cwd string
---@param cb fun(result: DiffReviewGhPRResult)
function M.current_pr_async(cwd, cb)
  system_text_async(pr_view_command(nil, nil), nil, cwd, function(result)
    if result.code ~= 0 then
      local output = result.output or ""
      if is_no_pr_output(output) then
        cb({ ok = true })
      elseif is_pr_lookup_unavailable_output(output) then
        cb({ ok = true, unavailable = true, message = output })
      else
        cb({ ok = false, message = output ~= "" and output or ("gh exited " .. result.code), code = result.code })
      end
      return
    end

    local ok, decoded = pcall(vim.json.decode, result.stdout or "")
    if not ok or type(decoded) ~= "table" then
      cb({ ok = false, message = "gh pr view returned invalid JSON", code = result.code })
      return
    end

    cb({ ok = true, pr = normalize_pr(decoded) })
  end)
end

---@param branch string
---@param repo? string
---@return DiffReviewGhCommand
local function pr_list_command(branch, repo)
  local command = {
    "gh", "pr", "list",
    "--head", branch,
    "--state", "all",
    "--limit", "100",
    "--json", pr_list_json_fields,
  }
  if repo and repo ~= "" then vim.list_extend(command, { "--repo", repo }) end
  return command
end

--- List every PR for one branch so callers can apply lifecycle selection policy.
---@param cwd string
---@param branch string
---@param repo? string
---@param cb fun(result: DiffReviewGhPRListResult)
function M.prs_for_branch_async(cwd, branch, repo, cb)
  if type(branch) ~= "string" or branch == "" or branch == "HEAD" then
    cb({ ok = false, message = "PR lookup requires a named branch", code = 1 })
    return
  end
  system_text_async(pr_list_command(branch, repo), nil, cwd, function(result)
    if result.code ~= 0 then
      local output = result.output or ""
      if is_no_pr_output(output) then
        cb({ ok = true, prs = {} })
      elseif is_pr_lookup_unavailable_output(output) then
        cb({ ok = true, unavailable = true, message = output })
      else
        cb({ ok = false, message = output ~= "" and output or ("gh exited " .. result.code), code = result.code })
      end
      return
    end

    local ok, decoded = pcall(vim.json.decode, result.stdout or "")
    if not ok or type(decoded) ~= "table" then
      cb({ ok = false, message = "gh pr list returned invalid JSON", code = result.code })
      return
    end
    local prs = {}
    for _, raw in ipairs(decoded) do
      if type(raw) == "table" then prs[#prs + 1] = normalize_pr(raw, repo) end
    end
    cb({ ok = true, prs = prs })
  end)
end

---@param cwd string
---@param number integer|string
---@param repo? string
---@param cb fun(result: DiffReviewGhPRResult)
function M.pr_async(cwd, number, repo, cb)
  system_text_async(pr_view_command(number, repo), nil, cwd, function(result)
    if result.code ~= 0 then
      cb({ ok = false, message = result.output ~= "" and result.output or ("gh exited " .. result.code), code = result.code })
      return
    end

    local ok, decoded = pcall(vim.json.decode, result.stdout or "")
    if not ok or type(decoded) ~= "table" then
      cb({ ok = false, message = "gh pr view returned invalid JSON", code = result.code })
      return
    end

    cb({ ok = true, pr = normalize_pr(decoded, repo) })
  end)
end

---@param cwd string
---@param number integer
---@param repo? string
---@param cb DiffReviewGhTextCallback
function M.pr_diff_async(cwd, number, repo, cb)
  if type(repo) == "function" and cb == nil then
    cb = repo
    repo = nil
  end
  local command = { "gh", "pr", "diff", tostring(number), "--patch", "--color", "never" }
  if repo and repo ~= "" then vim.list_extend(command, { "--repo", repo }) end
  system_text_async(command, nil, cwd, cb)
end

---@class DiffReviewGhPrEdit
---@field title? string
---@field body? string

---@class DiffReviewGhUser
---@field login string
---@field name? string

---@class DiffReviewGhUsersResult
---@field ok boolean
---@field users? table<string, DiffReviewGhUser>
---@field message? string
---@field code? integer

---@class DiffReviewGhRequestReviewersResult
---@field ok boolean
---@field reviewers? DiffReviewGhRequestedReviewer[]
---@field message? string
---@field code? integer

---@class DiffReviewGhMilestonesResult
---@field ok boolean
---@field milestones? DiffReviewGhMilestone[]
---@field milestone? DiffReviewGhMilestone
---@field message? string
---@field code? integer

---@class DiffReviewGhRepoResult
---@field ok boolean
---@field repo? string
---@field message? string
---@field code? integer

---@class DiffReviewGhRepoContributorsResult
---@field ok boolean
---@field contributors? table[]
---@field message? string
---@field code? integer

---Update a PR's title and/or body via `gh pr edit`. The body is passed on
---stdin (`--body-file -`) so it needs no quoting.
---@param cwd string
---@param number integer|string
---@param repo? string
---@param edit DiffReviewGhPrEdit
---@param cb DiffReviewGhTextCallback
function M.update_pr_async(cwd, number, repo, edit, cb)
  local command = { "gh", "pr", "edit", tostring(number) }
  if repo and repo ~= "" then vim.list_extend(command, { "--repo", repo }) end
  local input = nil
  if edit.title then vim.list_extend(command, { "--title", edit.title }) end
  if edit.body then
    vim.list_extend(command, { "--body-file", "-" })
    input = edit.body
  end
  system_text_async(command, input, cwd, cb)
end

---Build the `/repos/{owner}/{repo}/...` path for a PR sub-resource. When the
---repo is known it is embedded; otherwise gh's `{owner}`/`{repo}`
---placeholders resolve from the cwd's default repo.
---@param number integer|string
---@param repo? string
---@param resource string e.g. "comments" or "reviews"
---@return string
local function pr_api_path(number, repo, resource)
  local owner_repo = (repo and repo ~= "") and repo or "{owner}/{repo}"
  return ("/repos/%s/pulls/%s/%s"):format(owner_repo, tostring(number), resource)
end

---@param number integer|string
---@param repo? string
---@return string
local function issue_comments_api_path(number, repo)
  local owner_repo = (repo and repo ~= "") and repo or "{owner}/{repo}"
  return ("/repos/%s/issues/%s/comments"):format(owner_repo, tostring(number))
end

---@param repo? string
---@param resource string
---@return string
local function repo_api_path(repo, resource)
  local owner_repo = (repo and repo ~= "") and repo or "{owner}/{repo}"
  return ("/repos/%s/%s"):format(owner_repo, resource)
end

---@param number integer|string
---@param repo? string
---@return string
local function issue_api_path(number, repo)
  local owner_repo = (repo and repo ~= "") and repo or "{owner}/{repo}"
  return ("/repos/%s/issues/%s"):format(owner_repo, tostring(number))
end

---@param cwd string
---@param number integer|string
---@param repo? string
---@param reviewers string[]
---@param cb fun(result: DiffReviewGhRequestReviewersResult)
function M.request_reviewers_async(cwd, number, repo, reviewers, cb)
  local payload = { reviewers = reviewers or {} }
  system_text_async({ "gh", "api", "--method", "POST", pr_api_path(number, repo, "requested_reviewers"), "--input", "-" }, vim.json.encode(payload), cwd, function(result)
    if result.code ~= 0 then
      cb({ ok = false, message = result.output ~= "" and result.output or ("gh exited " .. tostring(result.code)), code = result.code })
      return
    end
    cb({ ok = true, reviewers = normalize_requested_reviewers(decode_json(result.stdout) or {}, reviewers) })
  end)
end

---@param cwd string
---@param number integer|string
---@param repo? string
---@param reviewers string[]
---@param cb fun(result: DiffReviewGhRequestReviewersResult)
function M.remove_reviewers_async(cwd, number, repo, reviewers, cb)
  local payload = { reviewers = reviewers or {} }
  system_text_async({ "gh", "api", "--method", "DELETE", pr_api_path(number, repo, "requested_reviewers"), "--input", "-" }, vim.json.encode(payload), cwd, function(result)
    if result.code ~= 0 then
      cb({ ok = false, message = result.output ~= "" and result.output or ("gh exited " .. tostring(result.code)), code = result.code })
      return
    end
    cb({ ok = true, reviewers = normalize_requested_reviewers(decode_json(result.stdout) or {}) })
  end)
end

---@param cwd string
---@param repo? string
---@param cb fun(result: DiffReviewGhMilestonesResult)
function M.repo_milestones_async(cwd, repo, cb)
  local command = { "gh", "api", repo_api_path(repo, "milestones?state=all&per_page=100"), "--paginate", "--slurp" }
  system_text_async(command, nil, cwd, function(result)
    if result.code ~= 0 then
      cb({ ok = false, message = result.output ~= "" and result.output or ("gh exited " .. tostring(result.code)), code = result.code })
      return
    end
    local decoded = decode_json(result.stdout)
    if not decoded then
      cb({ ok = false, message = "gh api returned invalid milestones JSON", code = result.code })
      return
    end
    cb({ ok = true, milestones = normalize_milestones(decoded) })
  end)
end

---@param cwd string
---@param repo? string
---@param title string
---@param cb fun(result: DiffReviewGhMilestonesResult)
function M.create_milestone_async(cwd, repo, title, cb)
  system_text_async({ "gh", "api", "--method", "POST", repo_api_path(repo, "milestones"), "--input", "-" }, vim.json.encode({ title = title }), cwd, function(result)
    if result.code ~= 0 then
      cb({ ok = false, message = result.output ~= "" and result.output or ("gh exited " .. tostring(result.code)), code = result.code })
      return
    end
    local milestone = normalize_milestone(decode_json(result.stdout))
    if not milestone then
      cb({ ok = false, message = "gh api returned invalid milestone JSON", code = result.code })
      return
    end
    cb({ ok = true, milestone = milestone })
  end)
end

---@param cwd string
---@param number integer|string
---@param repo? string
---@param milestone_number integer?
---@param cb fun(result: DiffReviewGhMilestonesResult)
function M.set_pr_milestone_async(cwd, number, repo, milestone_number, cb)
  local payload = { milestone = milestone_number or vim.NIL }
  system_text_async({ "gh", "api", "--method", "PATCH", issue_api_path(number, repo), "--input", "-" }, vim.json.encode(payload), cwd, function(result)
    if result.code ~= 0 then
      cb({ ok = false, message = result.output ~= "" and result.output or ("gh exited " .. tostring(result.code)), code = result.code })
      return
    end
    local decoded = decode_json(result.stdout)
    cb({ ok = true, milestone = normalize_milestone(decoded and decoded.milestone) })
  end)
end

---@param comment_id integer|string
---@param repo? string
---@return string
local function issue_comment_api_path(comment_id, repo)
  local owner_repo = (repo and repo ~= "") and repo or "{owner}/{repo}"
  return ("/repos/%s/issues/comments/%s"):format(owner_repo, tostring(comment_id))
end

---@param cwd string
---@param logins string[]
---@param cb fun(result: DiffReviewGhUsersResult)
function M.resolve_users_async(cwd, logins, cb)
  local users = {}
  local unique_logins = {}
  local seen = {}
  for _, login in ipairs(logins or {}) do
    local normalized = tostring(login or ""):gsub("^@", "")
    if normalized ~= "" and not seen[normalized:lower()] then
      seen[normalized:lower()] = true
      unique_logins[#unique_logins + 1] = normalized
    end
  end
  local function resolve_next(index)
    local login = unique_logins[index]
    if not login then
      cb({ ok = true, users = users })
      return
    end
    system_text_async({ "gh", "api", "/users/" .. login }, nil, cwd, function(result)
      local decoded = result.code == 0 and decode_json(result.stdout) or nil
      users[login:lower()] = {
        login = decoded and decoded.login or login,
        name = decoded and decoded.name or nil,
      }
      resolve_next(index + 1)
    end)
  end
  resolve_next(1)
end

---@param cwd string?
---@param cb fun(result: DiffReviewGhRepoResult)
function M.current_repo_async(cwd, cb)
  system_text_async({ "gh", "repo", "view", "--json", "nameWithOwner" }, nil, cwd, function(result)
    if result.code ~= 0 then
      cb({ ok = false, message = result.output ~= "" and result.output or ("gh exited " .. tostring(result.code)), code = result.code })
      return
    end
    local decoded = decode_json(result.stdout)
    local repo = decoded and decoded.nameWithOwner or nil
    if type(repo) ~= "string" or repo == "" then
      cb({ ok = false, message = "gh repo view returned no nameWithOwner", code = result.code })
      return
    end
    cb({ ok = true, repo = repo })
  end)
end

---@param cwd string?
---@param repo string
---@param cb fun(result: DiffReviewGhRepoContributorsResult)
function M.repo_contributors_async(cwd, repo, cb)
  repo_users.fetch_async({
    cwd = cwd,
    repo = repo,
    system_async = system_text_async,
    decode_json = function(stdout)
      local decoded = decode_json(stdout)
      if type(decoded) ~= "table" then return nil, "gh api returned invalid JSON" end
      return decoded, nil
    end,
    result_error = function(result)
      return result.output ~= "" and result.output or ("gh exited " .. tostring(result.code))
    end,
    callback = cb,
  })
end

---@param repo? string
---@return string? owner
---@return string? name
local function split_repo(repo)
  local owner, name = tostring(repo or ""):match("^([^/]+)/(.+)$")
  if owner and name and owner ~= "" and name ~= "" then return owner, name end
  return nil, nil
end

---@param value any
---@return integer?
local function maybe_integer(value)
  local number = tonumber(value)
  if number == nil then return nil end
  return math.floor(number)
end

---@param raw table
---@return string?
local function browser_url(raw)
  return raw.html_url or raw.url
end

---@param raw table
---@return boolean
local function viewer_did_author(raw)
  return raw.viewerDidAuthor == true or raw.viewer_did_author == true
end

---@param raw table
---@return DiffReviewGhReviewCommentReply
local function normalize_review_reply(raw)
  local author = raw.author or raw.user or {}
  local review = raw.pullRequestReview or {}
  return {
    remote_id = maybe_integer(raw.databaseId or raw.id),
    remote_node_id = raw.node_id or raw.id,
    review_id = maybe_integer(raw.pull_request_review_id or review.databaseId),
    review_node_id = review.id,
    review_state = review.state,
    body = normalize_body_text(raw.body),
    viewer_did_author = viewer_did_author(raw),
    user = author.login,
    created_at = raw.createdAt or raw.created_at,
    updated_at = raw.updatedAt or raw.updated_at,
    url = browser_url(raw),
  }
end

---@param raw table
---@param thread? table
---@return DiffReviewGhPendingReviewComment
local function normalize_review_comment(raw, thread)
  thread = thread or {}
  local author = raw.author or raw.user or {}
  local review = raw.pullRequestReview or {}
  return {
    remote_id = maybe_integer(raw.databaseId or raw.id),
    remote_node_id = raw.node_id or raw.id,
    review_id = maybe_integer(raw.pull_request_review_id or review.databaseId),
    review_node_id = review.id,
    review_state = review.state,
    body = normalize_body_text(raw.body),
    viewer_did_author = viewer_did_author(raw),
    path = tostring(raw.path or thread.path or ""),
    line = maybe_integer(raw.line or thread.line or thread.originalLine),
    start_line = maybe_integer(raw.startLine or raw.start_line or thread.startLine),
    position = maybe_integer(raw.position or raw.original_position),
    side = raw.side or raw.diffSide or thread.diffSide,
    user = author.login,
    created_at = raw.createdAt or raw.created_at,
    updated_at = raw.updatedAt or raw.updated_at,
    url = browser_url(raw),
    resolved = thread.isResolved,
    outdated = thread.isOutdated,
  }
end

---@param thread table
---@return DiffReviewGhPendingReviewComment?
local function normalize_review_thread(thread)
  local nodes = thread.comments and thread.comments.nodes or {}
  if type(nodes) ~= "table" or type(nodes[1]) ~= "table" then return nil end
  local comment = normalize_review_comment(nodes[1], thread)
  comment.replies = {}
  for node_index = 2, #nodes do
    local reply = nodes[node_index]
    if type(reply) == "table" then comment.replies[#comment.replies + 1] = normalize_review_reply(reply) end
  end
  return comment
end

---@param raw table
---@return DiffReviewGhSubmittedReview
local function normalize_submitted_review(raw)
  local author = raw.author or raw.user or {}
  local commit = raw.commit or {}
  return {
    id = maybe_integer(raw.databaseId or raw.id) or 0,
    node_id = tostring(raw.node_id or raw.id or ""),
    state = tostring(raw.state or ""),
    body = normalize_body_text(raw.body),
    viewer_did_author = viewer_did_author(raw),
    user = author.login,
    created_at = raw.createdAt or raw.created_at,
    updated_at = raw.updatedAt or raw.updated_at,
    submitted_at = raw.submittedAt or raw.submitted_at,
    commit_id = raw.commit_id or commit.oid,
    url = browser_url(raw),
    comments = {},
  }
end

---@param raw table
---@return DiffReviewGhIssueComment
local function normalize_issue_comment(raw)
  local author = raw.author or raw.user or {}
  return {
    remote_id = maybe_integer(raw.databaseId or raw.id),
    remote_node_id = raw.node_id or raw.id,
    body = normalize_body_text(raw.body),
    viewer_did_author = viewer_did_author(raw),
    user = author.login,
    created_at = raw.createdAt or raw.created_at,
    updated_at = raw.updatedAt or raw.updated_at,
    url = browser_url(raw),
  }
end

---@param raw table
---@return DiffReviewGhPendingReview
local function normalize_pending_review(raw)
  local author = raw.author or raw.user or {}
  local commit = raw.commit or {}
  return {
    id = maybe_integer(raw.databaseId or raw.id) or 0,
    node_id = tostring(raw.node_id or raw.id or ""),
    state = tostring(raw.state or ""),
    body = normalize_body_text(raw.body),
    viewer_did_author = viewer_did_author(raw),
    commit_id = raw.commit_id or commit.oid,
    user = author.login,
  }
end

---@param raw table
---@return DiffReviewGhPRCheck?
local function normalize_pr_check(raw)
  local kind = tostring(raw.__typename or "")
  if kind == "CheckRun" then
    local check_suite = raw.checkSuite or {}
    local workflow_run = check_suite.workflowRun or {}
    local workflow = workflow_run.workflow or {}
    local name = vim.trim(tostring(raw.name or ""))
    if name == "" then name = vim.trim(tostring(workflow.name or "")) end
    if name == "" then name = "Check run" end
    local status = vim.trim(tostring(raw.status or ""))
    local conclusion = vim.trim(tostring(raw.conclusion or ""))
    return {
      name = name,
      status = status,
      conclusion = conclusion ~= "" and conclusion or nil,
      state = conclusion ~= "" and conclusion or status,
      url = raw.detailsUrl,
      workflow_name = workflow.name,
      started_at = raw.startedAt,
      completed_at = raw.completedAt,
    }
  end
  if kind == "StatusContext" then
    local name = vim.trim(tostring(raw.context or ""))
    if name == "" then name = "Status context" end
    local state = vim.trim(tostring(raw.state or ""))
    return {
      name = name,
      status = state,
      conclusion = state ~= "" and state or nil,
      state = state,
      url = raw.targetUrl,
    }
  end
  return nil
end

---@param query string
---@param variables table
---@param cwd string?
---@param cb DiffReviewGhTextCallback
local function graphql_async(query, variables, cwd, cb)
  system_text_async({ "gh", "api", "graphql", "--input", "-" }, vim.json.encode({
    query = query,
    variables = variables,
  }), cwd, cb)
end

---@param cwd string
---@param pr_node_id string
---@param mutation string
---@param input_type string
---@param cb fun(result: DiffReviewGhPrStateResult)
local function mutate_pr_state_async(cwd, pr_node_id, mutation, input_type, cb)
  local query = table.concat({
    ("mutation($input:%s!) {"):format(input_type),
    ("  %s(input:$input) {"):format(mutation),
    "    pullRequest { id state isDraft }",
    "  }",
    "}",
  }, "\n")
  graphql_async(query, { input = { pullRequestId = pr_node_id } }, cwd, function(result)
    if result.code ~= 0 then
      cb({ ok = false, message = result.output ~= "" and result.output or ("gh exited " .. tostring(result.code)), code = result.code })
      return
    end
    local decoded = decode_json(result.stdout)
    local pull_request = decoded and decoded.data and decoded.data[mutation] and decoded.data[mutation].pullRequest
    if type(pull_request) ~= "table" then
      cb({ ok = false, message = "gh api returned invalid PR state JSON", code = result.code })
      return
    end
    cb({
      ok = true,
      state = tostring(pull_request.state or ""):upper(),
      is_draft = pull_request.isDraft == true or pull_request.is_draft == true,
    })
  end)
end

---@param cwd string
---@param pr_node_id string
---@param draft boolean
---@param cb fun(result: DiffReviewGhPrDraftResult)
function M.set_pr_draft_async(cwd, pr_node_id, draft, cb)
  if type(pr_node_id) ~= "string" or pr_node_id == "" then
    cb({ ok = false, message = "PR draft status update requires a pull request node id", code = 1 })
    return
  end

  local mutation = draft and "convertPullRequestToDraft" or "markPullRequestReadyForReview"
  local input_type = draft and "ConvertPullRequestToDraftInput" or "MarkPullRequestReadyForReviewInput"
  mutate_pr_state_async(cwd, pr_node_id, mutation, input_type, function(result)
    cb({ ok = result.ok, is_draft = result.is_draft, message = result.message, code = result.code })
  end)
end

--- Move a PR between DRAFT, OPEN, and CLOSED, composing reopen then draft when required.
---@param cwd string
---@param pr_node_id string
---@param current_state string
---@param desired_state string
---@param cb fun(result: DiffReviewGhPrStateResult)
function M.set_pr_state_async(cwd, pr_node_id, current_state, desired_state, cb)
  if type(pr_node_id) ~= "string" or pr_node_id == "" then
    cb({ ok = false, message = "PR state update requires a pull request node id", code = 1 })
    return
  end
  current_state = tostring(current_state or ""):upper()
  desired_state = tostring(desired_state or ""):upper()
  if desired_state ~= "DRAFT" and desired_state ~= "OPEN" and desired_state ~= "CLOSED" then
    cb({ ok = false, message = "Unsupported PR state: " .. desired_state, code = 1 })
    return
  end
  if current_state == desired_state then
    cb({ ok = true, state = desired_state == "DRAFT" and "OPEN" or desired_state, is_draft = desired_state == "DRAFT" })
    return
  end

  if desired_state == "CLOSED" then
    mutate_pr_state_async(cwd, pr_node_id, "closePullRequest", "ClosePullRequestInput", cb)
    return
  end
  if current_state == "CLOSED" then
    mutate_pr_state_async(cwd, pr_node_id, "reopenPullRequest", "ReopenPullRequestInput", function(reopen_result)
      if not reopen_result.ok then
        cb(reopen_result)
        return
      end
      local reopened_draft = reopen_result.is_draft == true
      if desired_state == "OPEN" and not reopened_draft then
        cb(reopen_result)
        return
      end
      if desired_state == "DRAFT" and reopened_draft then
        cb(reopen_result)
        return
      end
      local draft = desired_state == "DRAFT"
      local mutation = draft and "convertPullRequestToDraft" or "markPullRequestReadyForReview"
      local input_type = draft and "ConvertPullRequestToDraftInput" or "MarkPullRequestReadyForReviewInput"
      mutate_pr_state_async(cwd, pr_node_id, mutation, input_type, function(final_result)
        if not final_result.ok then
          final_result.state = reopen_result.state ~= "" and reopen_result.state or "OPEN"
          final_result.is_draft = reopen_result.is_draft
        end
        cb(final_result)
      end)
    end)
    return
  end

  local draft = desired_state == "DRAFT"
  local mutation = draft and "convertPullRequestToDraft" or "markPullRequestReadyForReview"
  local input_type = draft and "ConvertPullRequestToDraftInput" or "MarkPullRequestReadyForReviewInput"
  mutate_pr_state_async(cwd, pr_node_id, mutation, input_type, cb)
end

---@param cwd string
---@param number integer|string
---@param repo string
---@param cb fun(result: DiffReviewGhPRChecksResult)
function M.pr_checks_async(cwd, number, repo, cb)
  local owner, name = split_repo(repo)
  if not owner then
    cb({ ok = false, message = "PR checks require an owner/repo name", code = 1 })
    return
  end
  local query = table.concat({
    "query($owner:String!, $repo:String!, $number:Int!) {",
    "  repository(owner:$owner, name:$repo) {",
    "    pullRequest(number:$number) {",
    "      commits(last:1) { nodes {",
    "        commit {",
    "          statusCheckRollup {",
    "            contexts(first:100) { nodes {",
    "              __typename",
    "              ... on CheckRun {",
    "                name status conclusion detailsUrl startedAt completedAt",
    "                checkSuite { workflowRun { workflow { name } } }",
    "              }",
    "              ... on StatusContext {",
    "                context state targetUrl",
    "              }",
    "            } }",
    "          }",
    "        }",
    "      } }",
    "    }",
    "  }",
    "}",
  }, "\n")
  graphql_async(query, { owner = owner, repo = name, number = tonumber(number) }, cwd, function(result)
    if result.code ~= 0 then
      cb({ ok = false, message = result.output ~= "" and result.output or ("gh exited " .. result.code), code = result.code })
      return
    end

    local decoded = decode_json(result.stdout)
    if not decoded then
      cb({ ok = false, message = "gh api returned invalid PR checks JSON", code = result.code })
      return
    end
    if type(decoded.errors) == "table" and #decoded.errors > 0 then
      cb({ ok = false, message = vim.json.encode(decoded.errors), code = result.code })
      return
    end

    local pull_request = decoded
      and decoded.data
      and decoded.data.repository
      and decoded.data.repository.pullRequest
      or nil
    if type(pull_request) ~= "table" then
      cb({ ok = false, message = "gh api returned no pull request for checks", code = result.code })
      return
    end

    local commit_node = pull_request.commits
      and pull_request.commits.nodes
      and pull_request.commits.nodes[1]
      or {}
    local contexts = commit_node.commit
      and commit_node.commit.statusCheckRollup
      and commit_node.commit.statusCheckRollup.contexts
      and commit_node.commit.statusCheckRollup.contexts.nodes
      or {}
    local checks = {}
    for _, raw in ipairs(type(contexts) == "table" and contexts or {}) do
      local check = type(raw) == "table" and normalize_pr_check(raw) or nil
      if check then checks[#checks + 1] = check end
    end
    cb({ ok = true, checks = checks })
  end)
end

---@param cwd string
---@param number integer|string
---@param repo string
---@param cb fun(result: DiffReviewGhPRCommentsResult)
function M.pr_comments_async(cwd, number, repo, cb)
  local owner, name = split_repo(repo)
  if not owner then
    cb({ ok = false, message = "PR comments require an owner/repo name", code = 1 })
    return
  end
  local query = table.concat({
    "query($owner:String!, $repo:String!, $number:Int!) {",
    "  repository(owner:$owner, name:$repo) {",
    "    pullRequest(number:$number) {",
    "      reviews(first:100) { nodes {",
    "        id databaseId state body viewerDidAuthor createdAt updatedAt submittedAt url",
    "        author { login }",
    "        commit { oid }",
    "      } }",
    "      comments(first:100) { nodes {",
    "        id databaseId body viewerDidAuthor createdAt updatedAt url author { login }",
    "      } }",
    "      reviewThreads(first:100) { nodes {",
    "        isResolved isOutdated path line startLine originalLine diffSide",
    "        comments(first:100) { nodes {",
    "          id databaseId body viewerDidAuthor path line position createdAt updatedAt url author { login }",
    "          pullRequestReview { id databaseId state }",
    "        } }",
    "      } }",
    "    }",
    "  }",
    "}",
  }, "\n")
  graphql_async(query, { owner = owner, repo = name, number = tonumber(number) }, cwd, function(result)
    if result.code ~= 0 then
      cb({ ok = false, message = result.output ~= "" and result.output or ("gh exited " .. result.code), code = result.code })
      return
    end

    local decoded = decode_json(result.stdout)
    local pull_request = decoded
      and decoded.data
      and decoded.data.repository
      and decoded.data.repository.pullRequest
      or {}

    local reviews = {}
    local reviews_by_id = {}
    local review_nodes = pull_request.reviews and pull_request.reviews.nodes or {}
    for _, review_raw in ipairs(type(review_nodes) == "table" and review_nodes or {}) do
      local review = normalize_submitted_review(review_raw)
      if review.state ~= "" and review.state ~= "PENDING" then
        reviews[#reviews + 1] = review
        if review.id ~= 0 then reviews_by_id["db:" .. tostring(review.id)] = review end
        if review.node_id ~= "" then reviews_by_id["node:" .. review.node_id] = review end
      end
    end

    local issue_comments = {}
    local issue_nodes = pull_request.comments and pull_request.comments.nodes or {}
    for _, comment in ipairs(type(issue_nodes) == "table" and issue_nodes or {}) do
      issue_comments[#issue_comments + 1] = normalize_issue_comment(comment)
    end

    local code_comments = {}
    local threads = pull_request.reviewThreads and pull_request.reviewThreads.nodes or {}
    for _, thread in ipairs(type(threads) == "table" and threads or {}) do
      local normalized = normalize_review_thread(thread)
      if normalized then
        code_comments[#code_comments + 1] = normalized
        local review = normalized.review_id and reviews_by_id["db:" .. tostring(normalized.review_id)]
          or (normalized.review_node_id and reviews_by_id["node:" .. tostring(normalized.review_node_id)])
        if review then
          review.comments = review.comments or {}
          review.comments[#review.comments + 1] = normalized
        end
      end
    end

    for _, review in ipairs(reviews) do
      table.sort(review.comments or {}, function(left_comment, right_comment)
        local left_path = tostring(left_comment.path or "")
        local right_path = tostring(right_comment.path or "")
        if left_path ~= right_path then return left_path < right_path end
        local left_line = tonumber(left_comment.line or left_comment.start_line) or math.huge
        local right_line = tonumber(right_comment.line or right_comment.start_line) or math.huge
        if left_line ~= right_line then return left_line < right_line end
        return tostring(left_comment.created_at or "") < tostring(right_comment.created_at or "")
      end)
    end
    table.sort(reviews, function(left_review, right_review)
      return tostring(left_review.submitted_at or left_review.updated_at or left_review.created_at or "")
        < tostring(right_review.submitted_at or right_review.updated_at or right_review.created_at or "")
    end)

    cb({ ok = true, reviews = reviews, code_comments = code_comments, issue_comments = issue_comments })
  end)
end

---@param cwd string
---@param number integer|string
---@param repo string
---@param cb fun(result: DiffReviewGhPendingReviewResult)
function M.pending_review_async(cwd, number, repo, cb)
  local owner, name = split_repo(repo)
  if not owner then
    cb({ ok = false, message = "PR review sync requires an owner/repo name", code = 1 })
    return
  end
  local query = table.concat({
    "query($owner:String!, $repo:String!, $number:Int!) {",
    "  repository(owner:$owner, name:$repo) {",
    "    pullRequest(number:$number) {",
    "      reviews(first:100) { nodes {",
    "        id databaseId state body viewerDidAuthor",
    "        author { login }",
    "        commit { oid }",
    "      } }",
    "      reviewThreads(first:100) { nodes {",
    "        comments(first:100) { nodes {",
    "          id databaseId body viewerDidAuthor path line position createdAt updatedAt author { login }",
    "          pullRequestReview { id databaseId state }",
    "        } }",
    "      } }",
    "    }",
    "  }",
    "}",
  }, "\n")
  graphql_async(query, { owner = owner, repo = name, number = tonumber(number) }, cwd, function(result)
    if result.code ~= 0 then
      cb({ ok = false, message = result.output ~= "" and result.output or ("gh exited " .. result.code), code = result.code })
      return
    end

    local decoded = decode_json(result.stdout)
    local pull_request = decoded
      and decoded.data
      and decoded.data.repository
      and decoded.data.repository.pullRequest
      or {}
    local reviews = pull_request.reviews and pull_request.reviews.nodes or {}
    local selected = nil
    for _, review in ipairs(type(reviews) == "table" and reviews or {}) do
      if tostring(review.state or "") == "PENDING" and review.viewerDidAuthor then
        selected = review
        break
      end
    end
    if not selected then
      cb({ ok = true, comments = {} })
      return
    end

    local review = normalize_pending_review(selected)
    local comments = {}
    local threads = pull_request.reviewThreads and pull_request.reviewThreads.nodes or {}
    for _, thread in ipairs(type(threads) == "table" and threads or {}) do
      local comment = normalize_review_thread(thread)
      if comment then
        local comment_review = {
          databaseId = comment.review_id,
          id = comment.review_node_id,
        }
        local comment_review_id = maybe_integer(comment_review.databaseId)
        if comment_review_id == review.id or tostring(comment_review.id or "") == review.node_id then
          comments[#comments + 1] = comment
        end
      end
    end
    cb({ ok = true, review = review, comments = comments })
  end)
end

---@param cwd string
---@param number integer|string
---@param repo? string
---@param opts { commit_id?: string }
---@param cb fun(result: DiffReviewGhPendingReviewResult)
function M.create_pending_review_async(cwd, number, repo, opts, cb)
  local payload = {}
  if opts and opts.commit_id and opts.commit_id ~= "" then payload.commit_id = opts.commit_id end
  system_text_async({ "gh", "api", "--method", "POST", pr_api_path(number, repo, "reviews"), "--input", "-" }, vim.json.encode(payload), cwd, function(result)
    if result.code ~= 0 then
      cb({ ok = false, message = result.output ~= "" and result.output or ("gh exited " .. result.code), code = result.code })
      return
    end
    local decoded = decode_json(result.stdout)
    if not decoded then
      cb({ ok = false, message = "gh api returned invalid pending review JSON", code = result.code })
      return
    end
    cb({ ok = true, review = normalize_pending_review(decoded), comments = {} })
  end)
end

---@param cwd string
---@param review_node_id string
---@param opts { body: string, path: string, position: integer, commit_id?: string }
---@param cb fun(result: DiffReviewGhPendingReviewResult)
function M.add_pending_review_comment_async(cwd, review_node_id, opts, cb)
  if not review_node_id or tostring(review_node_id) == "" then
    cb({ ok = false, message = "pending review comment requires a review node id", code = 1 })
    return
  end
  local query = table.concat({
    "mutation($input:AddPullRequestReviewCommentInput!) {",
    "  addPullRequestReviewComment(input:$input) {",
    "    comment { id databaseId body viewerDidAuthor path line position createdAt updatedAt author { login } }",
    "  }",
    "}",
  }, "\n")
  local input = {
    pullRequestReviewId = review_node_id,
    body = opts.body,
    path = opts.path,
    position = opts.position,
  }
  if opts.commit_id and opts.commit_id ~= "" then input.commitOID = opts.commit_id end
  graphql_async(query, { input = input }, cwd, function(result)
    if result.code ~= 0 then
      cb({ ok = false, message = result.output ~= "" and result.output or ("gh exited " .. result.code), code = result.code })
      return
    end
    local decoded = decode_json(result.stdout)
    local comment = decoded
      and decoded.data
      and decoded.data.addPullRequestReviewComment
      and decoded.data.addPullRequestReviewComment.comment
    if type(comment) ~= "table" then
      cb({ ok = false, message = "gh api returned invalid review comment JSON", code = result.code })
      return
    end
    cb({ ok = true, comments = { normalize_review_comment(comment) } })
  end)
end

---@param cwd string
---@param number integer|string
---@param repo? string
---@param opts { body: string, commit_id?: string, path: string, line?: integer, side?: string, start_line?: integer, start_side?: string, position?: integer }
---@param cb fun(result: DiffReviewGhPendingReviewResult)
function M.create_pr_review_comment_async(cwd, number, repo, opts, cb)
  local payload = {
    body = opts.body,
    commit_id = opts.commit_id,
    path = opts.path,
  }
  if opts.line then
    payload.line = opts.line
    payload.side = opts.side or "RIGHT"
    if opts.start_line then
      payload.start_line = opts.start_line
      payload.start_side = opts.start_side or payload.side
    end
  elseif opts.position then
    payload.position = opts.position
  end
  system_text_async({ "gh", "api", "--method", "POST", pr_api_path(number, repo, "comments"), "--input", "-" }, vim.json.encode(payload), cwd, function(result)
    if result.code ~= 0 then
      cb({ ok = false, message = result.output ~= "" and result.output or ("gh exited " .. result.code), code = result.code })
      return
    end
    local decoded = decode_json(result.stdout)
    if not decoded then
      cb({ ok = false, message = "gh api returned invalid standalone review comment JSON", code = result.code })
      return
    end
    cb({ ok = true, comments = { normalize_review_comment(decoded) } })
  end)
end

--- Create a reply under a top-level pull-request review comment.
---@param cwd string
---@param number integer|string
---@param repo string?
---@param comment_id integer|string
---@param body string
---@param cb fun(result: DiffReviewGhReviewReplyResult)
function M.create_review_comment_reply_async(cwd, number, repo, comment_id, body, cb)
  if not comment_id or tostring(comment_id) == "" then
    cb({ ok = false, message = "review comment reply requires a top-level comment id", code = 1 })
    return
  end
  local suffix = ("comments/%s/replies"):format(tostring(comment_id))
  system_text_async(
    { "gh", "api", "--method", "POST", pr_api_path(number, repo, suffix), "--input", "-" },
    vim.json.encode({ body = body }),
    cwd,
    function(result)
      if result.code ~= 0 then
        cb({ ok = false, message = result.output ~= "" and result.output or ("gh exited " .. result.code), code = result.code })
        return
      end
      local decoded = decode_json(result.stdout)
      if not decoded then
        cb({ ok = false, message = "gh api returned invalid review reply JSON", code = result.code })
        return
      end
      cb({ ok = true, reply = normalize_review_reply(decoded), code = result.code })
    end
  )
end

---@param cwd string
---@param number integer|string
---@param repo? string
---@param body string
---@param cb fun(result: DiffReviewGhPRCommentsResult)
function M.create_issue_comment_async(cwd, number, repo, body, cb)
  system_text_async({ "gh", "api", "--method", "POST", issue_comments_api_path(number, repo), "--input", "-" }, vim.json.encode({ body = body }), cwd, function(result)
    if result.code ~= 0 then
      cb({ ok = false, message = result.output ~= "" and result.output or ("gh exited " .. result.code), code = result.code })
      return
    end
    local decoded = decode_json(result.stdout)
    if not decoded then
      cb({ ok = false, message = "gh api returned invalid issue comment JSON", code = result.code })
      return
    end
    cb({ ok = true, issue_comments = { normalize_issue_comment(decoded) } })
  end)
end

---@param cwd string
---@param repo? string
---@param comment_id integer|string
---@param body string
---@param cb fun(result: DiffReviewGhPRCommentsResult)
function M.update_issue_comment_async(cwd, repo, comment_id, body, cb)
  system_text_async({ "gh", "api", "--method", "PATCH", issue_comment_api_path(comment_id, repo), "--input", "-" }, vim.json.encode({ body = body }), cwd, function(result)
    if result.code ~= 0 then
      cb({ ok = false, message = result.output ~= "" and result.output or ("gh exited " .. result.code), code = result.code })
      return
    end
    local decoded = decode_json(result.stdout)
    if not decoded then
      cb({ ok = false, message = "gh api returned invalid issue comment JSON", code = result.code })
      return
    end
    cb({ ok = true, issue_comments = { normalize_issue_comment(decoded) } })
  end)
end

---@param cwd string
---@param comment_node_id string
---@param body string
---@param cb fun(result: DiffReviewGhPendingReviewResult)
function M.update_review_comment_async(cwd, comment_node_id, body, cb)
  local query = table.concat({
    "mutation($input:UpdatePullRequestReviewCommentInput!) {",
    "  updatePullRequestReviewComment(input:$input) {",
    "    pullRequestReviewComment { id databaseId body viewerDidAuthor path line position createdAt updatedAt author { login } }",
    "  }",
    "}",
  }, "\n")
  graphql_async(query, { input = { pullRequestReviewCommentId = comment_node_id, body = body } }, cwd, function(result)
    if result.code ~= 0 then
      cb({ ok = false, message = result.output ~= "" and result.output or ("gh exited " .. result.code), code = result.code })
      return
    end
    local decoded = decode_json(result.stdout)
    local comment = decoded
      and decoded.data
      and decoded.data.updatePullRequestReviewComment
      and decoded.data.updatePullRequestReviewComment.pullRequestReviewComment
    cb({ ok = true, comments = type(comment) == "table" and { normalize_review_comment(comment) } or {} })
  end)
end

---@param cwd string
---@param comment_node_id string
---@param cb fun(result: DiffReviewGhPendingReviewResult)
function M.delete_review_comment_async(cwd, comment_node_id, cb)
  local query = "mutation($input:DeletePullRequestReviewCommentInput!) { deletePullRequestReviewComment(input:$input) { clientMutationId } }"
  graphql_async(query, { input = { id = comment_node_id } }, cwd, function(result)
    if result.code ~= 0 then
      cb({ ok = false, message = result.output ~= "" and result.output or ("gh exited " .. result.code), code = result.code })
      return
    end
    cb({ ok = true, comments = {} })
  end)
end

---@param cwd string
---@param number integer|string
---@param repo? string
---@param review_id integer|string
---@param opts { body: string, event: "APPROVE"|"REQUEST_CHANGES"|"COMMENT" }
---@param cb DiffReviewGhTextCallback
function M.submit_pending_review_async(cwd, number, repo, review_id, opts, cb)
  local command = { "gh", "api", "--method", "POST", pr_api_path(number, repo, ("reviews/%s/events"):format(tostring(review_id))), "--input", "-" }
  system_text_async(command, vim.json.encode({ body = opts.body, event = opts.event }), cwd, cb)
end

---@class DiffReviewGhReviewComment
---@field body string
---@field path string repo-relative file path
---@field line integer last line of the range (new-file line for RIGHT)
---@field side "LEFT"|"RIGHT"
---@field start_line? integer first line when the comment spans a range
---@field start_side? "LEFT"|"RIGHT"

---Submit a complete review in one request: the summary, a verdict, and all
---inline comments at once (`POST .../pulls/{n}/reviews`). This is the normal
---GitHub review flow — comments are drafted locally and posted together, not
---one standalone comment at a time. The JSON body goes on stdin via
---`gh api --input -` so multi-line text needs no shell escaping.
---@param cwd string
---@param number integer|string
---@param repo? string
---@param opts { body: string, event: "APPROVE"|"REQUEST_CHANGES"|"COMMENT", commit_id?: string, comments?: DiffReviewGhReviewComment[] }
---@param cb DiffReviewGhTextCallback
function M.submit_pr_review_async(cwd, number, repo, opts, cb)
  local command = { "gh", "api", "--method", "POST", pr_api_path(number, repo, "reviews"), "--input", "-" }
  local payload = { body = opts.body, event = opts.event }
  if opts.commit_id and opts.commit_id ~= "" then payload.commit_id = opts.commit_id end
  if opts.comments and #opts.comments > 0 then payload.comments = opts.comments end
  system_text_async(command, vim.json.encode(payload), cwd, cb)
end

---@param url string?
---@return boolean
function M.browse_url(url)
  if not (url and url ~= "") then return false end
  local backend = M._backend
  if backend and backend.open_url then
    return backend.open_url(url)
  end
  if vim.ui and vim.ui.open then
    vim.ui.open(url)
    return true
  end
  return false
end

---@param pr DiffReviewGhPR
---@return boolean
function M.browse_pr(pr)
  if not (pr and pr.url and pr.url ~= "") then return false end
  return M.browse_url(pr.url)
end

---@param pr DiffReviewGhPR
---@param fragment? string
---@return boolean
function M.browse_pr_changes(pr, fragment)
  if not (pr and pr.url and pr.url ~= "") then return false end
  local url = pr.url:gsub("/$", "") .. "/changes"
  if fragment and fragment ~= "" then url = url .. "#" .. fragment end
  return M.browse_url(url)
end

return M
