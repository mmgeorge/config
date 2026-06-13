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

  return {
    number = as_integer(raw.number),
    title = tostring(raw.title or ""),
    body = tostring(raw.body or ""),
    url = tostring(raw.url or ""),
    repo = repo,
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

---@param pr DiffReviewGhPR
---@return boolean
function M.browse_pr(pr)
  if not (pr and pr.url and pr.url ~= "") then return false end
  local backend = M._backend
  if backend and backend.open_url then
    return backend.open_url(pr.url)
  end
  if vim.ui and vim.ui.open then
    vim.ui.open(pr.url)
    return true
  end
  return false
end

return M
