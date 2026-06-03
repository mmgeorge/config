vim.loader.enable(false)

local diff_review = require("diff_review")
local gh = require("diff_review.gh")
local original_notify = vim.notify

local root = "D:/mock/github-project"
local calls = {}
local opened_urls = {}
local pr_mode = "ready"
local pr_title = "Improve DiffReview"

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function command_key(command)
  return table.concat(command, "\t")
end

local function record(kind, command, input)
  calls[#calls + 1] = {
    kind = kind,
    command = vim.deepcopy(command),
    key = command_key(command),
    input = input,
  }
end

---@type DiffReviewGitBackend
local git_backend = {}

function git_backend.systemlist(command)
  record("systemlist", command)
  local key = command_key(command)
  if key == "git\trev-parse\t--show-toplevel" then return { root }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--short\tHEAD" then return { "abc1234" }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--abbrev-ref\tHEAD" then return { "feature/pr-view" }, 0 end
  if key == "git\t-C\t" .. root .. "\tlog\t-1\t--format=%s" then return { "mock subject" }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--abbrev-ref\t@{upstream}" then return { "origin/feature/pr-view" }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--short\t@{upstream}" then return { "def5678" }, 0 end
  if key == "git\t-C\t" .. root .. "\tlog\t-1\t--format=%s\t@{upstream}" then return { "upstream subject" }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--abbrev-ref\t@{push}" then return { "origin/feature/pr-view" }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--short\t@{push}" then return { "def5678" }, 0 end
  if key == "git\t-C\t" .. root .. "\tlog\t-1\t--format=%s\t@{push}" then return { "push subject" }, 0 end
  if key == "git\t-C\t" .. root .. "\tls-files\t--others\t--exclude-standard" then return {}, 0 end
  if key == "git\t-C\t" .. root .. "\tdiff\t--cached\t--name-status" then return {}, 0 end
  if key == "git\t-C\t" .. root .. "\tdiff\t--name-status" then return {}, 0 end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff" then return {}, 0 end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--cached" then return {}, 0 end
  return {}, 1
end

function git_backend.systemlist_async(command, cb)
  vim.defer_fn(function()
    local output, code = git_backend.systemlist(command)
    cb(output, code)
  end, 5)
end

function git_backend.system(command, input)
  record("system", command, input)
  local key = command_key(command)
  if key == "git\t-C\t" .. root .. "\tpush" then return "", 0 end
  return "unexpected command: " .. key, 1
end

function git_backend.system_async(command, input, cb)
  vim.defer_fn(function()
    local output, code = git_backend.system(command, input)
    cb({
      code = code,
      stdout = output,
      stderr = "",
      output = output,
    })
  end, 5)
end

---@type DiffReviewGhBackend
local gh_backend = {}

local function pr_json()
  return vim.json.encode({
    number = 42,
    title = pr_title,
    body = "This PR adds GitHub integration.\n\n- status row\n- PR view",
    url = "https://github.example.test/org/repo/pull/42",
    headRefName = "feature/pr-view",
    headRefOid = "abc123456789",
    commits = {
      {
        oid = "abc123456789",
        messageHeadline = "mock subject",
      },
    },
    files = {
      {
        path = "lua/diff_review/init.lua",
        additions = 8,
        deletions = 2,
        changeType = "MODIFIED",
      },
      {
        path = "lua/diff_review/gh.lua",
        additions = 100,
        deletions = 0,
        changeType = "ADDED",
      },
    },
    changedFiles = 2,
    additions = 108,
    deletions = 2,
  })
end

function gh_backend.system_async(command, _, cb, cwd)
  record("gh_system_async", command, cwd)
  vim.defer_fn(function()
    local key = command_key(command)
    if key == "gh\tpr\tview\t--json\tnumber,title,body,url,headRefName,headRefOid,commits,files,changedFiles,additions,deletions" then
      if pr_mode == "none" then
        cb({ code = 1, stdout = "", stderr = "no pull requests found", output = "no pull requests found" })
      else
        cb({ code = 0, stdout = pr_json(), stderr = "", output = pr_json() })
      end
      return
    end
    if key == "gh\tpr\tdiff\t42\t--patch\t--color\tnever" then
      cb({ code = 0, stdout = "diff --git a/a b/a\n", stderr = "", output = "diff --git a/a b/a\n" })
      return
    end
    cb({ code = 1, stdout = "", stderr = "unexpected gh command: " .. key, output = "unexpected gh command: " .. key })
  end, 40)
end

function gh_backend.open_url(url)
  opened_urls[#opened_urls + 1] = url
  return true
end

local function status_lines(buf)
  return vim.api.nvim_buf_get_lines(buf, 0, -1, false)
end

local function buffer_contains(buf, needle)
  for _, line in ipairs(status_lines(buf)) do
    if line:find(needle, 1, true) then return true end
  end
  return false
end

local function find_row(buf, needle)
  for index, line in ipairs(status_lines(buf)) do
    if line:find(needle, 1, true) then return index end
  end
  error("missing row: " .. needle .. "\n" .. table.concat(status_lines(buf), "\n"), 2)
end

local function wait_for(condition, message)
  assert_true(vim.wait(3000, condition, 10), message)
end

local function trigger_normal_mapping(key, row)
  vim.api.nvim_win_set_cursor(0, { row, 0 })
  local mapping = vim.fn.maparg(key, "n", false, true)
  assert_true(type(mapping.callback) == "function", "missing normal mapping for " .. key)
  mapping.callback()
end

local function run()
  diff_review.set_git_backend(git_backend)
  gh.set_backend(gh_backend)
  vim.notify = function() end
  diff_review.setup({ pr_buffer_name = "DiffReviewPRTest" })
  diff_review.open()
  local status_buf = vim.api.nvim_get_current_buf()

  wait_for(function() return buffer_contains(status_buf, "PR:") and buffer_contains(status_buf, "...fetching...") end, "PR row did not show fetching state")
  wait_for(function() return buffer_contains(status_buf, "Improve DiffReview") end, "PR row did not show fetched PR title")

  trigger_normal_mapping("<CR>", find_row(status_buf, "PR:"))
  local pr_buf = vim.api.nvim_get_current_buf()
  assert_true(pr_buf ~= status_buf, "PRView did not open a new buffer")
  assert_true(vim.bo[pr_buf].filetype == "markdown", "PRView is not a markdown buffer")
  assert_true(buffer_contains(pr_buf, "Hint: b browse"), "PRView missing hint")
  assert_true(buffer_contains(pr_buf, "Title: Improve DiffReview"), "PRView missing title")
  assert_true(buffer_contains(pr_buf, "Description:"), "PRView missing description heading")
  assert_true(buffer_contains(pr_buf, "- status row"), "PRView missing markdown body")
  assert_true(buffer_contains(pr_buf, "Head:"), "PRView missing head row")
  assert_true(buffer_contains(pr_buf, "Changes (2)"), "PRView missing changes heading")
  assert_true(buffer_contains(pr_buf, "lua\\diff_review\\gh.lua +100 -0"), "PRView missing file change row")

  trigger_normal_mapping("b", 1)
  assert_true(opened_urls[#opened_urls] == "https://github.example.test/org/repo/pull/42", "browse did not open PR URL")

  vim.api.nvim_win_set_buf(0, status_buf)
  trigger_normal_mapping("gp", find_row(status_buf, "Head:"))
  assert_true(vim.api.nvim_get_current_buf() ~= status_buf, "gp did not open PRView")

  vim.api.nvim_win_set_buf(0, status_buf)
  pr_mode = "none"
  diff_review.render_status(status_buf, nil, nil, { refresh_pr = true })
  wait_for(function() return buffer_contains(status_buf, "PR:     none") end, "PR row did not render none state")

  pr_mode = "ready"
  pr_title = "PR after push"
  trigger_normal_mapping("pP", find_row(status_buf, "Head:"))
  wait_for(function() return buffer_contains(status_buf, "PR after push") end, "push did not refresh PR state")
  wait_for(function()
    for _, call in ipairs(calls) do
      if call.kind == "system" and call.key == "git\t-C\t" .. root .. "\tpush" then return true end
    end
    return false
  end, "push command did not run")
end

local ok, err = xpcall(run, debug.traceback)
diff_review.reset_git_backend()
gh.reset_backend()
vim.notify = original_notify
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
