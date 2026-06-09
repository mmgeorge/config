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

  local ok, process_or_error = pcall(vim.system, command, { text = true, stdin = input, cwd = cwd }, function(result)
    vim.schedule(function()
      local stdout = result.stdout or ""
      local stderr = result.stderr or ""
      cb({
        code = result.code or 0,
        stdout = stdout,
        stderr = stderr,
        output = stdout ~= "" and stdout or stderr,
      })
    end)
  end)
  if not ok then
    vim.schedule(function()
      local message = tostring(process_or_error)
      cb({
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
