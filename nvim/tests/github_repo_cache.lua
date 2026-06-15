vim.loader.enable(false)

local cache = require("github.repo_cache")
local github_gh = require("github.gh")
local issue_index = require("github.issue_index")

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local original_notify = vim.notify
local captured_notifications = {}
local cache_root = vim.fn.tempname()
local cwd = "D:/github-cache-test-root"
local repo = "Owner/Repo"
local normalized_repo = "Owner/Repo"
local issue_search_calls = {}
local issue_search_should_fail = false
local issue_search_empty = false

local function capture_notify(message, level, opts)
  captured_notifications[#captured_notifications + 1] = {
    message = tostring(message),
    level = level,
    opts = opts,
  }
end

local function saw_notification_containing(needle)
  for _, notification in ipairs(captured_notifications) do
    if notification.message:find(needle, 1, true) then return true end
  end
  return false
end

local function assert_issue_searches_do_not_request_pr_fields()
  for _, call in ipairs(issue_search_calls) do
    assert_true(not call:find("isDraft", 1, true), "issue search requested PR-only isDraft field: " .. call)
  end
end

---@type GithubGhBackend
local github_backend = {}

local function issue_item(number, title)
  return {
    number = number,
    title = title,
    url = "https://github.com/Owner/Repo/issues/" .. tostring(number),
    repository = { nameWithOwner = normalized_repo },
    author = { login = "me" },
    commentsCount = 0,
    updatedAt = "2026-06-14T20:00:00Z",
    state = "open",
  }
end

function github_backend.system_async(command, _, callback)
  local key = table.concat(command, " ")
  issue_search_calls[#issue_search_calls + 1] = key
  local stdout
  if issue_search_should_fail then
    vim.defer_fn(function()
      callback({ code = 1, stdout = "", stderr = "mock issue search failure", output = "mock issue search failure" })
    end, 5)
    return
  elseif issue_search_empty then
    stdout = "[]"
  elseif key:find(" --assignee @me ", 1, true) then
    stdout = vim.json.encode({
      issue_item(42, "Assigned issue match"),
      issue_item(50, "Duplicate general issue"),
    })
  elseif key:find(" --mentions @me ", 1, true) then
    stdout = vim.json.encode({
      issue_item(43, "Mentioned issue match"),
      issue_item(42, "Assigned issue match"),
    })
  else
    stdout = vim.json.encode({
      issue_item(50, "Duplicate general issue"),
      issue_item(44, "General issue match"),
    })
  end
  vim.defer_fn(function()
    callback({ code = 0, stdout = stdout, stderr = "", output = stdout })
  end, 5)
end

---@param buf integer
---@param text string
---@return table<string, boolean>
local function completion_labels(buf, text)
  vim.api.nvim_set_current_buf(buf)
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, { text })
  vim.api.nvim_win_set_cursor(0, { 1, #text })
  local source = require("diff_review.reviewer_source").new({})
  assert_true(source:enabled(), "completion source did not enable for " .. text)
  local result
  source:get_completions({}, function(completions) result = completions end)
  local labels = {}
  for _, item in ipairs(result.items or {}) do
    labels[item.label] = true
  end
  return labels
end

---@param buf integer
---@param text string
---@return table result
local function issue_completion_result(buf, text)
  vim.api.nvim_set_current_buf(buf)
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, { text })
  local old_virtualedit = vim.o.virtualedit
  vim.o.virtualedit = "onemore"
  vim.api.nvim_win_set_cursor(0, { 1, #text })
  local source = require("github.issue_source").new({})
  assert_true(source:enabled(), "issue completion source did not enable for " .. text)
  local result
  source:get_completions({}, function(completions) result = completions end)
  assert_true(vim.wait(1000, function() return result ~= nil end, 10), "issue completions did not return")
  vim.o.virtualedit = old_virtualedit
  return result
end

local function write_issue_snapshot(repo_name)
  local path = issue_index.snapshot_path(repo_name)
  vim.fn.mkdir(vim.fs.dirname(path), "p")
  local result = vim.fn.writefile({ vim.json.encode({
    repo = repo_name,
    state = "open",
    issue_count = 4,
    issues = {
      {
        repo = repo_name,
        number = 42,
        title = "The name of issue assigned",
        state = "OPEN",
        url = "https://github.com/Owner/Repo/issues/42",
        labels = { { name = "assigned" } },
      },
      {
        repo = repo_name,
        number = 50,
        title = "The name of issue duplicate",
        state = "OPEN",
        url = "https://github.com/Owner/Repo/issues/50",
        labels = { { name = "general" } },
      },
      {
        repo = repo_name,
        number = 43,
        title = "Mentioned issue match",
        state = "OPEN",
        url = "https://github.com/Owner/Repo/issues/43",
        labels = { { name = "mentioned" } },
      },
      {
        repo = repo_name,
        number = 44,
        title = "General test issue match",
        state = "OPEN",
        url = "https://github.com/Owner/Repo/issues/44",
        labels = { { name = "general" } },
      },
    },
  }) }, path)
  assert_true(result == 0, "issue snapshot write failed")
end

local function run()
  vim.notify = capture_notify
  cache.set_data_dir_for_test(cache_root)
  issue_index._reset_for_test()
  issue_index._set_progress_enabled_for_test(false)
  issue_index._set_runner_for_test(function(command, _, callback)
    local key = table.concat(command, " ")
    if key:find(" state ", 1, true) then
      local stdout = vim.json.encode({
        repo = normalized_repo,
        open_historical_complete = true,
        last_open_checked_at = os.time(),
      })
      callback({ code = 0, stdout = stdout, stderr = "", output = stdout })
      return
    end
    callback({ code = 0, stdout = "{}", stderr = "", output = "{}" })
  end)
  github_gh.set_backend(github_backend)
  cache.remember_cwd_repo(cwd, normalized_repo)
  cache.set_base_branch(cwd, "master")
  write_issue_snapshot(normalized_repo)

  local metadata_error = cache.write_metadata(normalized_repo, {
    { login = "bobtown" },
    { login = "alice-dev", name = "Alice Developer" },
    { login = "Alice-Dev", name = "Duplicate Alice" },
    { login = "mgeorge-esri" },
  })
  assert_true(metadata_error == nil, "metadata write failed: " .. tostring(metadata_error))

  local contributors = cache.contributors(normalized_repo)
  assert_true(#contributors == 3, "contributors were not deduplicated: " .. vim.inspect(contributors))
  assert_true(contributors[1].login == "alice-dev", "contributors were not sorted by login: " .. vim.inspect(contributors))
  assert_true(contributors[2].login == "bobtown", "contributors were not sorted by login: " .. vim.inspect(contributors))
  assert_true(contributors[3].login == "mgeorge-esri", "contributors were not sorted by login: " .. vim.inspect(contributors))
  assert_true(cache.metadata_fresh(normalized_repo), "metadata should be fresh immediately after write")
  assert_true(cache.repo_for_cwd(cwd) == normalized_repo, "cwd repo mapping was not stored")
  assert_true(cache.get_base_branch(cwd) == "master", "base branch was not stored in cwd cache")

  issue_search_calls = {}
  local listed_issues
  github_gh.search_issues_async(cwd, function(result) listed_issues = result end)
  assert_true(vim.wait(1000, function() return listed_issues ~= nil end, 10), "issue list search did not return")
  assert_true(listed_issues.ok, "issue list search failed: " .. vim.inspect(listed_issues))
  assert_true(#issue_search_calls == 1, "issue list search should run exactly one command")
  assert_issue_searches_do_not_request_pr_fields()

  local disabled_buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_set_current_buf(disabled_buf)
  vim.api.nvim_buf_set_lines(disabled_buf, 0, -1, false, { "Comment @a" })
  vim.api.nvim_win_set_cursor(0, { 1, #"Comment @a" })
  assert_true(not require("diff_review.reviewer_source").new({}):enabled(), "completion source enabled without buffer whitelist")
  vim.api.nvim_buf_set_lines(disabled_buf, 0, -1, false, { "Comment #issue title" })
  vim.api.nvim_win_set_cursor(0, { 1, #"Comment #issue title" })
  assert_true(not require("github.issue_source").new({}):enabled(), "issue completion source enabled without buffer whitelist")

  local enabled_buf = vim.api.nvim_create_buf(false, true)
  cache.enable_user_completion(enabled_buf, normalized_repo)
  local labels = completion_labels(enabled_buf, "Comment @a")
  assert_true(labels["@alice-dev"], "completion did not include cached contributor @alice-dev")
  assert_true(labels["@bobtown"], "completion did not include cached contributor @bobtown")
  assert_true(labels["@mgeorge-esri"], "completion did not include cached collaborator @mgeorge-esri")

  local one_char_issue_result = issue_completion_result(enabled_buf, "Comment #t")
  assert_true(#one_char_issue_result.items == 4, "one-character issue completion did not search local snapshot: " .. vim.inspect(one_char_issue_result.items))
  assert_true(one_char_issue_result.items[1].label == "#42 The name of issue assigned", "local issue completion did not rank first match: " .. vim.inspect(one_char_issue_result.items))

  issue_search_calls = {}
  local issue_result = issue_completion_result(enabled_buf, "Comment #the name of issue")
  assert_true(#issue_result.items == 2, "issue completion did not filter local snapshot: " .. vim.inspect(issue_result.items))
  assert_true(issue_result.items[1].label == "#42 The name of issue assigned", "first local issue result was wrong: " .. vim.inspect(issue_result.items))
  assert_true(issue_result.items[2].label == "#50 The name of issue duplicate", "second local issue result was wrong: " .. vim.inspect(issue_result.items))
  assert_true(issue_result.items[1].textEdit.newText == "#42", "issue completion must insert only the issue id")
  assert_true(issue_result.items[1].textEdit.range.start.character == #"Comment ", "issue completion range did not start at #")
  assert_true(issue_result.items[1].textEdit.range["end"].character == #"Comment #the name of issue", "issue completion range did not end at cursor")
  assert_true(#issue_search_calls == 0, "issue completion should not call gh search: " .. vim.inspect(issue_search_calls))

  local no_repo_buf = vim.api.nvim_create_buf(false, true)
  cache.delete_cwd(vim.fn.getcwd())
  cache.enable_user_completion(no_repo_buf)
  captured_notifications = {}
  local no_repo_result = issue_completion_result(no_repo_buf, "Comment #missing")
  assert_true(#no_repo_result.items == 0, "missing repo issue completion should return no items")
  assert_true(
    vim.wait(1000, function() return saw_notification_containing("no repo context") end, 10),
    "missing repo issue completion did not notify"
  )

  local empty_result = issue_completion_result(enabled_buf, "Comment #1")
  assert_true(#empty_result.items == 0, "empty issue search should return no items")

  local review_path = cache.review_path(normalized_repo, 2)
  vim.fn.mkdir(vim.fs.dirname(review_path), "p")
  vim.fn.writefile({ "{}" }, review_path)
  assert_true(vim.uv.fs_stat(review_path) ~= nil, "review file was not created")

  local deleted = cache.delete_current(nil, cwd)
  assert_true(deleted >= 2, "delete_current did not remove mapped repo and cwd cache")
  assert_true(#cache.contributors(normalized_repo) == 0, "contributors were not deleted with repo cache")
  assert_true(cache.repo_for_cwd(cwd) == nil, "cwd repo mapping was not deleted")
  assert_true(cache.get_base_branch(cwd) == nil, "base branch was not deleted with cwd cache")
  assert_true(vim.uv.fs_stat(review_path) == nil, "review draft was not deleted with repo cache")
end

local ok, err = xpcall(run, debug.traceback)
vim.notify = original_notify
github_gh.reset_backend()
issue_index._reset_for_test()
cache.set_data_dir_for_test(nil)
vim.fn.delete(cache_root, "rf")
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
