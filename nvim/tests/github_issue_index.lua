local repo_cache = require("github.repo_cache")
local issue_index = require("github.issue_index")

local function assert_true(value, message)
  if not value then error(message or "assertion failed", 2) end
end

local function assert_equals(actual, expected, message)
  if actual ~= expected then
    error((message or "values differ") .. "\nexpected: " .. vim.inspect(expected) .. "\nactual: " .. vim.inspect(actual), 2)
  end
end

local function write_snapshot(repo, issues)
  local path = issue_index.snapshot_path(repo)
  vim.fn.mkdir(vim.fs.dirname(path), "p")
  local result = vim.fn.writefile({ vim.json.encode({
    repo = repo,
    state = "open",
    issue_count = #issues,
    issues = issues,
  }) }, path)
  assert_equals(result, 0, "snapshot write failed")
end

local function buffer_contains(buf, needle)
  for _, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    if line:find(needle, 1, true) then return true end
  end
  return false
end

local function find_issue_progress_float()
  for _, win in ipairs(vim.api.nvim_list_wins()) do
    local config = vim.api.nvim_win_get_config(win)
    if config.relative ~= "" then
      local buf = vim.api.nvim_win_get_buf(win)
      if buffer_contains(buf, "Completion uses the latest local snapshot while this runs.") then return win, config end
    end
  end
  return nil, nil
end

local function close_issue_progress_float()
  local win = find_issue_progress_float()
  if win and vim.api.nvim_win_is_valid(win) then pcall(vim.api.nvim_win_close, win, true) end
end

local test_root = vim.fs.joinpath(vim.fn.getcwd(), ".tmp-github-issue-index-test")
vim.fn.delete(test_root, "rf")
vim.fn.mkdir(test_root, "p")
repo_cache.set_data_dir_for_test(test_root)
issue_index._reset_for_test()
issue_index._set_progress_enabled_for_test(false)

local ok, err = xpcall(function()
  local repo = "mmgeorge/test-repo"
  write_snapshot(repo, {
    {
      repo = repo,
      number = 42,
      title = "Index all open issues",
      state = "OPEN",
      url = "https://github.com/mmgeorge/test-repo/issues/42",
      labels = {
        { name = "enhancement" },
      },
    },
    {
      repo = repo,
      number = 77,
      title = "Unrelated bug",
      state = "OPEN",
      url = "https://github.com/mmgeorge/test-repo/issues/77",
      labels = {
        { name = "bug" },
      },
    },
  })

  local matches = issue_index.search(repo, "enhance", { limit = 10 })
  assert_equals(#matches, 1, "label search should find one issue")
  assert_equals(matches[1].number, 42, "label search returned wrong issue")

  local buf = vim.api.nvim_create_buf(true, true)
  vim.api.nvim_set_current_buf(buf)
  vim.b[buf].github_repo = repo
  vim.b[buf].github_user_completion = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, { "Fix #ind" })
  vim.api.nvim_win_set_cursor(0, { 1, 8 })

  local completion_source = require("github.issue_source").new({})
  local completion_result
  completion_source:get_completions({}, function(result)
    completion_result = result
  end)
  assert_true(vim.wait(1000, function() return completion_result ~= nil end), "completion callback did not run")
  assert_equals(#completion_result.items, 1, "completion should return one item")
  local item = completion_result.items[1]
  assert_equals(item.label, "#42 Index all open issues", "completion label mismatch")
  assert_equals(item.textEdit.newText, "#42", "completion edit text mismatch")
  assert_equals(item.textEdit.range.start.character, 4, "completion edit start mismatch")
  assert_equals(item.textEdit.range["end"].character, 7, "completion edit end mismatch")

  issue_index._set_progress_enabled_for_test(true)
  issue_index._set_runner_for_test(function(command, _, callback)
    local action = command[2]
    if action == "state" then
      callback({ code = 0, stdout = vim.json.encode({}), stderr = "", output = "{}" })
      return
    end
    if action == "upsert-page" or action == "snapshot" then
      callback({ code = 0, stdout = vim.json.encode({}), stderr = "", output = "{}" })
      return
    end
    callback({ code = 1, stdout = "", stderr = "unexpected action: " .. tostring(action), output = "unexpected action: " .. tostring(action) })
  end)
  issue_index._set_gh_runner_for_test(function(_, _, callback)
    local stdout = vim.json.encode({
      data = {
        rateLimit = { remaining = 5000 },
        repository = {
          issues = {
            totalCount = 0,
            pageInfo = { hasNextPage = false, endCursor = vim.NIL },
            nodes = {},
          },
        },
      },
    })
    callback({ code = 0, stdout = stdout, stderr = "", output = stdout })
  end)

  local expected_config = issue_index._progress_window_config_for_test(54, 4)
  issue_index.sync_repo(vim.fn.getcwd(), repo, { manual = true })
  local found = vim.wait(1000, function()
    local win = find_issue_progress_float()
    return win ~= nil
  end, 10)
  assert_true(found, "issue sync progress float did not open")
  local _, progress_config = find_issue_progress_float()
  assert_equals(progress_config.relative, "editor", "progress float should be editor-relative")
  assert_equals(progress_config.width, expected_config.width, "progress float width mismatch")
  assert_equals(progress_config.height, expected_config.height, "progress float height mismatch")
  assert_equals(progress_config.row, expected_config.row, "progress float row should be centered")
  assert_equals(progress_config.col, expected_config.col, "progress float column should be centered")
  close_issue_progress_float()
end, debug.traceback)

repo_cache.set_data_dir_for_test(nil)
issue_index._reset_for_test()
close_issue_progress_float()
vim.fn.delete(test_root, "rf")
if not ok then
  print(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
