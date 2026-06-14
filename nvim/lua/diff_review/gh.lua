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

---@class DiffReviewGhPRCommit
---@field oid string
---@field messageHeadline? string

---@class DiffReviewGhPR
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

---@class DiffReviewGhPRResult
---@field ok boolean
---@field pr? DiffReviewGhPR
---@field message? string
---@field code? integer
---@field unavailable? boolean

---@class DiffReviewGhReviewCommentReply
---@field remote_id integer?
---@field remote_node_id string?
---@field review_id integer?
---@field review_node_id string?
---@field review_state string?
---@field body string
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
---@field user? string
---@field created_at? string
---@field updated_at? string
---@field url? string

---@class DiffReviewGhSubmittedReview
---@field id integer
---@field node_id string
---@field state string
---@field body string
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

---@class DiffReviewGhPendingReview
---@field id integer
---@field node_id string
---@field state string
---@field body string
---@field commit_id string?
---@field user? string

---@class DiffReviewGhPendingReviewResult
---@field ok boolean
---@field review? DiffReviewGhPendingReview
---@field comments? DiffReviewGhPendingReviewComment[]
---@field message? string
---@field code? integer

---@class DiffReviewGhModule
---@field _backend DiffReviewGhBackend?

---@type DiffReviewGhModule
local M = {}

local default_timeout_ms = 5000
local timeout_ms = default_timeout_ms

local pr_json_fields = table.concat({
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
    }
  end

  local normalized_repo = repo or M.repo_from_pr_url(raw.url)
  return {
    number = as_integer(raw.number),
    title = tostring(raw.title or ""),
    body = tostring(raw.body or ""),
    url = tostring(raw.url or ""),
    repo = normalized_repo,
    headRefName = tostring(raw.headRefName or ""),
    headRefOid = raw.headRefOid,
    commits = commits,
    files = files,
    changedFiles = as_integer(raw.changedFiles),
    additions = as_integer(raw.additions),
    deletions = as_integer(raw.deletions),
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

---@param text string
---@return table?
local function decode_json(text)
  local ok, decoded = pcall(vim.json.decode, tostring(text or ""))
  if ok and type(decoded) == "table" then return decoded end
  return nil
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
    body = tostring(raw.body or ""),
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
    body = tostring(raw.body or ""),
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
    replies = {},
  }
end

---@param thread table
---@return DiffReviewGhPendingReviewComment?
local function normalize_review_thread(thread)
  local nodes = thread.comments and thread.comments.nodes or {}
  if type(nodes) ~= "table" or type(nodes[1]) ~= "table" then return nil end
  local comment = normalize_review_comment(nodes[1], thread)
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
    body = tostring(raw.body or ""),
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
    body = tostring(raw.body or ""),
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
    body = tostring(raw.body or ""),
    commit_id = raw.commit_id or commit.oid,
    user = author.login,
  }
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
    "        id databaseId state body createdAt updatedAt submittedAt url",
    "        author { login }",
    "        commit { oid }",
    "      } }",
    "      comments(first:100) { nodes {",
    "        id databaseId body createdAt updatedAt url author { login }",
    "      } }",
    "      reviewThreads(first:100) { nodes {",
    "        isResolved isOutdated path line startLine originalLine diffSide",
    "        comments(first:100) { nodes {",
    "          id databaseId body path line position createdAt updatedAt url author { login }",
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
    "          id databaseId body path line position createdAt updatedAt author { login }",
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

---@param opts { body: string, commit_id?: string, path: string, line?: integer, side?: string, start_line?: integer, start_side?: string, position?: integer }
---@param include_commit boolean
---@return table
local function review_comment_payload(opts, include_commit)
  local payload = {
    body = opts.body,
    path = opts.path,
  }
  if include_commit then payload.commit_id = opts.commit_id end
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
  return payload
end

---@param cwd string
---@param number integer|string
---@param repo? string
---@param review_id integer|string
---@param opts { body: string, path: string, line?: integer, side?: string, start_line?: integer, start_side?: string, position?: integer }
---@param cb fun(result: DiffReviewGhPendingReviewResult)
function M.add_pending_review_comment_async(cwd, number, repo, review_id, opts, cb)
  if not review_id or tostring(review_id) == "" or tostring(review_id) == "0" then
    cb({ ok = false, message = "pending review comment requires a review id", code = 1 })
    return
  end
  local resource = ("reviews/%s/comments"):format(tostring(review_id))
  system_text_async({ "gh", "api", "--method", "POST", pr_api_path(number, repo, resource), "--input", "-" }, vim.json.encode(review_comment_payload(opts, false)), cwd, function(result)
    if result.code ~= 0 then
      cb({ ok = false, message = result.output ~= "" and result.output or ("gh exited " .. result.code), code = result.code })
      return
    end
    local decoded = decode_json(result.stdout)
    if not decoded then
      cb({ ok = false, message = "gh api returned invalid review comment JSON", code = result.code })
      return
    end
    cb({ ok = true, comments = { normalize_review_comment(decoded) } })
  end)
end

---@param cwd string
---@param number integer|string
---@param repo? string
---@param opts { body: string, commit_id?: string, path: string, line?: integer, side?: string, start_line?: integer, start_side?: string, position?: integer }
---@param cb fun(result: DiffReviewGhPendingReviewResult)
function M.create_pr_review_comment_async(cwd, number, repo, opts, cb)
  system_text_async({ "gh", "api", "--method", "POST", pr_api_path(number, repo, "comments"), "--input", "-" }, vim.json.encode(review_comment_payload(opts, true)), cwd, function(result)
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

---@param cwd string
---@param comment_node_id string
---@param body string
---@param cb fun(result: DiffReviewGhPendingReviewResult)
function M.update_review_comment_async(cwd, comment_node_id, body, cb)
  local query = table.concat({
    "mutation($input:UpdatePullRequestReviewCommentInput!) {",
    "  updatePullRequestReviewComment(input:$input) {",
    "    pullRequestReviewComment { id databaseId body path line position createdAt updatedAt author { login } }",
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
