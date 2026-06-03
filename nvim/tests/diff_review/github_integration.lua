vim.loader.enable(false)

local diff_review = require("diff_review")
local ai_commit = require("diff_review.ai_commit")
local commit = require("diff_review.commit")
local gh = require("diff_review.gh")
local original_notify = vim.notify

local root = "D:/mock/github-project"
local calls = {}
local opened_urls = {}
local pr_mode = "ready"
local pr_title = "Improve DiffReview"
local generated_commit = "feat: add diff review ai summary\n\nAdd generated commit metadata."
local has_changes = true
local generate_count = 0

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
  if key == "git\t-C\t" .. root .. "\tdiff\t--name-status" then return has_changes and { "M\tlua/diff_review/init.lua" } or {}, 0 end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff" then
    if not has_changes then return {}, 0 end
    return {
      "diff --git a/lua/diff_review/init.lua b/lua/diff_review/init.lua",
      "--- a/lua/diff_review/init.lua",
      "+++ b/lua/diff_review/init.lua",
      "@@ -1 +1 @@",
      "-old",
      "+new",
    }, 0
  end
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
  if key == "git\t-C\t" .. root .. "\tpull" then return "", 0 end
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

---@type DiffReviewAICommitBackend
local ai_backend = {}

function ai_backend.systemlist_async(command, cb)
  record("ai_systemlist_async", command)
  local key = command_key(command)
  vim.defer_fn(function()
    if key == "git\t-C\t" .. root .. "\tdiff\t--no-ext-diff\t--no-color\tHEAD\t--stat\t--summary" then
      if not has_changes then
        cb({}, 0)
        return
      end
      cb({ " lua/diff_review/init.lua | 2 +-" }, 0)
      return
    end
    if key == "git\t-C\t" .. root .. "\tdiff\t--no-ext-diff\t--no-color\tHEAD" then
      if not has_changes then
        cb({}, 0)
        return
      end
      cb({
        "diff --git a/lua/diff_review/init.lua b/lua/diff_review/init.lua",
        "--- a/lua/diff_review/init.lua",
        "+++ b/lua/diff_review/init.lua",
        "@@ -1 +1 @@",
        "-old",
        "+new",
      }, 0)
      return
    end
    if key == "git\t-C\t" .. root .. "\tdiff\t--no-ext-diff\t--no-color\t--staged\t--stat\t--summary" then
      cb({ " lua/diff_review/init.lua | 2 +-" }, 0)
      return
    end
    cb({}, 1)
  end, 5)
end

function ai_backend.generate_async(_, cb)
  generate_count = generate_count + 1
  vim.defer_fn(function()
    cb({ ok = true, message = generated_commit })
  end, 80)
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

---@param buf integer
---@param keys string[]
local function assert_buffer_contains_all(buf, keys)
  for _, key in ipairs(keys) do
    assert_true(buffer_contains(buf, key), "buffer missing " .. key)
  end
end

local function run()
  diff_review.set_git_backend(git_backend)
  ai_commit.set_backend(ai_backend)
  gh.set_backend(gh_backend)
  vim.notify = function() end
  diff_review.setup({ pr_buffer_name = "DiffReviewPRTest" })
  diff_review.open()
  local status_buf = vim.api.nvim_get_current_buf()

  wait_for(function() return buffer_contains(status_buf, "PR:") and buffer_contains(status_buf, "...fetching...") end, "PR row did not show fetching state")
  wait_for(function() return buffer_contains(status_buf, "About:  ...generating...") end, "About row did not show generating state")

  local commit_buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(commit_buf, 0, -1, false, { "" })
  vim.bo[commit_buf].modifiable = true
  ai_commit.populate_commit_buffer_when_ready(commit_buf, root)

  wait_for(function() return buffer_contains(status_buf, "Improve DiffReview") end, "PR row did not show fetched PR title")
  wait_for(function() return buffer_contains(status_buf, "feat: add diff review ai summary") end, "About row did not show generated commit subject")
  wait_for(function()
    local lines = vim.api.nvim_buf_get_lines(commit_buf, 0, -1, false)
    return lines[1] == "feat: add diff review ai summary"
  end, "generated commit was not applied to commit buffer after waiting")
  assert_true(vim.bo[commit_buf].modifiable, "commit buffer modifiable state was not preserved")

  local commit_host_buf = vim.api.nvim_get_current_buf()
  local commit_console = vim.api.nvim_create_buf(false, true)
  commit._active = {
    win = vim.api.nvim_get_current_win(),
    prev_buf = commit_host_buf,
    prev_winbar = "",
    console = commit_console,
    root = root,
  }
  commit.editor(root .. "/.git/COMMIT_EDITMSG", "")
  local editmsg_buf = vim.api.nvim_get_current_buf()
  assert_true(vim.b[editmsg_buf].ai_commit_generated == true, "DiffReview commit buffer did not suppress global AI commit autocmd")
  commit._active = nil
  vim.api.nvim_win_set_buf(0, commit_host_buf)
  if vim.api.nvim_buf_is_valid(editmsg_buf) then
    vim.api.nvim_buf_delete(editmsg_buf, { force = true })
  end
  if vim.api.nvim_buf_is_valid(commit_console) then
    vim.api.nvim_buf_delete(commit_console, { force = true })
  end

  trigger_normal_mapping("<CR>", find_row(status_buf, "About:"))
  local about_buf = vim.api.nvim_get_current_buf()
  assert_true(about_buf ~= status_buf, "About view did not open a new buffer")
  assert_true(buffer_contains(about_buf, "feat: add diff review ai summary"), "About view missing commit subject")
  assert_true(buffer_contains(about_buf, "Add generated commit metadata."), "About view missing commit body")
  vim.api.nvim_win_set_buf(0, status_buf)

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
  trigger_normal_mapping("?", find_row(status_buf, "Head:"))
  local help_buf = vim.api.nvim_get_current_buf()
  assert_true(help_buf ~= status_buf, "help did not open a popup buffer")
  assert_buffer_contains_all(help_buf, { "<Tab>", "S", "U", "j", "oc", "opP", "opp", "ogp", "<CR>", "r", "q", "?" })
  pcall(vim.api.nvim_win_close, 0, true)

  vim.api.nvim_win_set_buf(0, status_buf)
  trigger_normal_mapping("ogp", find_row(status_buf, "Head:"))
  assert_true(vim.api.nvim_get_current_buf() ~= status_buf, "ogp did not open PRView")

  vim.api.nvim_win_set_buf(0, status_buf)
  pr_mode = "none"
  diff_review.render_status(status_buf, nil, nil, { refresh_pr = true })
  wait_for(function() return buffer_contains(status_buf, "PR:     none") end, "PR row did not render none state")

  pr_mode = "ready"
  pr_title = "PR after push"
  trigger_normal_mapping("opP", find_row(status_buf, "Head:"))
  wait_for(function() return buffer_contains(status_buf, "PR after push") end, "push did not refresh PR state")
  wait_for(function()
    for _, call in ipairs(calls) do
      if call.kind == "system" and call.key == "git\t-C\t" .. root .. "\tpush" then return true end
    end
    return false
  end, "push command did not run")

  trigger_normal_mapping("opp", find_row(status_buf, "Head:"))
  wait_for(function()
    for _, call in ipairs(calls) do
      if call.kind == "system" and call.key == "git\t-C\t" .. root .. "\tpull" then return true end
    end
    return false
  end, "pull command did not run")

  if vim.api.nvim_buf_is_valid(status_buf) then
    vim.api.nvim_buf_delete(status_buf, { force = true })
  end
  diff_review._status = nil
  diff_review.setup({
    pr_buffer_name = "DiffReviewPRTest",
    keymaps = {
      status = {
        commit = "zc",
        push = "zpP",
        pull = "zpp",
        pr = "zgp",
        help = "zh",
      },
    },
  })
  diff_review.open()
  local override_buf = vim.api.nvim_get_current_buf()
  wait_for(function() return buffer_contains(override_buf, "PR:") end, "override status buffer did not render")
  assert_true(type(vim.fn.maparg("zc", "n", false, true).callback) == "function", "custom commit mapping missing")
  assert_true(type(vim.fn.maparg("zpP", "n", false, true).callback) == "function", "custom push mapping missing")
  assert_true(type(vim.fn.maparg("zpp", "n", false, true).callback) == "function", "custom pull mapping missing")
  assert_true(type(vim.fn.maparg("zgp", "n", false, true).callback) == "function", "custom PR mapping missing")
  trigger_normal_mapping("zh", find_row(override_buf, "Head:"))
  local override_help_buf = vim.api.nvim_get_current_buf()
  assert_buffer_contains_all(override_help_buf, { "zc", "zpP", "zpp", "zgp", "zh" })

  if vim.api.nvim_win_is_valid(0) then
    pcall(vim.api.nvim_win_close, 0, true)
  end
  if vim.api.nvim_buf_is_valid(override_buf) then
    vim.api.nvim_buf_delete(override_buf, { force = true })
  end
  has_changes = false
  local count_before_no_changes = generate_count
  diff_review._status = nil
  diff_review.setup({ pr_buffer_name = "DiffReviewPRTest" })
  diff_review.open()
  local no_changes_buf = vim.api.nvim_get_current_buf()
  wait_for(function() return buffer_contains(no_changes_buf, "No changes") end, "no changes status did not render")
  assert_true(buffer_contains(no_changes_buf, "About:  none"), "About row should show none with no changes")
  vim.wait(120)
  assert_true(generate_count == count_before_no_changes, "AI generation started despite no changes")
end

local ok, err = xpcall(run, debug.traceback)
diff_review.reset_git_backend()
ai_commit.reset_backend()
gh.reset_backend()
vim.notify = original_notify
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
