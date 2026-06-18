local repo_cache = require("github.repo_cache")
local issue_index = require("github.issue_index")
local issue_index_builder = require("github.issue_index_builder")

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

local test_root = vim.fs.joinpath(vim.fn.getcwd(), ".tmp-github-issue-index-test")
vim.fn.delete(test_root, "rf")
vim.fn.mkdir(test_root, "p")
repo_cache.set_data_dir_for_test(test_root)
issue_index._reset_for_test()
issue_index_builder._reset_for_test()
issue_index._set_progress_enabled_for_test(false)
local original_notify = vim.notify
local original_issue_index_hostname = vim.g.github_issue_index_hostname
local original_issue_index_log = vim.g.github_issue_index_log

local ok, err = xpcall(function()
  local repo = "mmgeorge/test-repo"
  write_snapshot(repo, {
    {
      repo = repo,
      number = 42,
      title = "Index all open issues",
      state = "OPEN",
      url = "https://github.com/mmgeorge/test-repo/issues/42",
      updated_at = "2026-06-14T20:00:00Z",
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
      updated_at = "2026-06-13T20:00:00Z",
      labels = {
        { name = "bug" },
      },
    },
  })

  local matches = issue_index.search(repo, "enhance", { limit = 10 })
  assert_equals(#matches, 1, "label search should find one issue")
  assert_equals(matches[1].number, 42, "label search returned wrong issue")
  assert_equals(matches[1].body, "", "search should not require synced issue body")

  local listed = issue_index.list(repo, { limit = 10 })
  assert_equals(#listed, 2, "issue list should return synced issues")
  assert_equals(listed[1].number, 42, "issue list should preserve snapshot order")
  assert_equals(listed[1].body, "", "issue list should not require synced issue body")

  local build_root = vim.fs.joinpath(test_root, "builder")
  vim.fn.mkdir(vim.fs.joinpath(build_root, "src"), "p")
  assert_equals(vim.fn.writefile({ '[package]', 'name = "github-issue-index"', 'version = "0.1.0"', 'edition = "2021"' }, vim.fs.joinpath(build_root, "Cargo.toml")), 0, "test Cargo.toml write failed")
  assert_equals(vim.fn.writefile({ "fn main() {}" }, vim.fs.joinpath(build_root, "src", "main.rs")), 0, "test main.rs write failed")
  issue_index_builder._set_crate_dir_for_test(build_root)
  local build_count = 0
  issue_index_builder._set_runner_for_test(function(command, opts, callback)
    build_count = build_count + 1
    assert_equals(command[1], "cargo", "builder should invoke cargo")
    assert_equals(command[2], "build", "builder should invoke cargo build")
    assert_equals(command[3], "--manifest-path", "builder should pass explicit manifest path")
    assert_equals(command[4], issue_index_builder.manifest_path(), "builder manifest path mismatch")
    assert_equals(command[5], "--release", "builder should build release binary")
    assert_equals(opts.cwd, build_root, "builder cwd mismatch")
    vim.fn.mkdir(vim.fs.dirname(issue_index_builder.release_binary_path()), "p")
    assert_equals(vim.fn.writefile({ "fake binary" }, issue_index_builder.release_binary_path()), 0, "fake binary write failed")
    callback({ code = 0, stdout = "", stderr = "" })
  end)
  local build_result
  issue_index_builder.ensure(function(result)
    build_result = result
  end)
  assert_true(vim.wait(1000, function() return build_result ~= nil end, 10), "builder callback did not run")
  assert_true(build_result.ok, build_result.message)
  assert_equals(build_result.path, issue_index_builder.release_binary_path(), "builder returned wrong binary path")
  assert_equals(build_count, 1, "builder should run cargo once for missing binary")
  local cached_build_result
  issue_index_builder.ensure(function(result)
    cached_build_result = result
  end)
  assert_true(vim.wait(1000, function() return cached_build_result ~= nil end, 10), "cached builder callback did not run")
  assert_true(cached_build_result.ok, cached_build_result.message)
  assert_equals(build_count, 1, "builder should skip cargo when release binary is current")
  issue_index_builder._reset_for_test()

  local stale_cwd = vim.fs.joinpath(test_root, "stale-cwd")
  local stale_repo = "org/repo"
  repo_cache.remember_cwd_repo(stale_cwd, stale_repo)
  vim.fn.mkdir(repo_cache.repo_dir(stale_repo), "p")
  local stale_fetch_count = 0
  issue_index._set_runner_for_test(function(command, _, callback)
    local action = command[2]
    if action == "state" then
      callback({ code = 0, stdout = vim.json.encode({}), stderr = "", output = "{}" })
      return
    end
    callback({ code = 1, stdout = "", stderr = "unexpected action: " .. tostring(action), output = "unexpected action: " .. tostring(action) })
  end)
  issue_index._set_gh_runner_for_test(function(_, _, callback)
    stale_fetch_count = stale_fetch_count + 1
    local message = [[{"errors":[{"type":"NOT_FOUND","path":["repository"],"message":"Could not resolve to a Repository with the name 'org/repo'."}]}]]
    callback({ code = 1, stdout = "", stderr = message, output = message })
  end)
  issue_index.ensure_current(stale_cwd, { manual = false })
  assert_true(vim.wait(1000, function() return repo_cache.repo_for_cwd(stale_cwd) == nil end, 10), "stale repo mapping was not cleared")
  assert_true(vim.uv.fs_stat(repo_cache.repo_dir(stale_repo)) == nil, "stale repo cache directory was not cleared")
  issue_index.ensure_repo(stale_cwd, stale_repo, { manual = false })
  assert_equals(stale_fetch_count, 1, "invalid repo should not retry automatically in the same session")

  issue_index._reset_for_test()
  issue_index._set_progress_enabled_for_test(false)
  local busy_repo = "mmgeorge/busy-repo"
  local busy_lock = issue_index.sync_lock_path(busy_repo)
  vim.fn.mkdir(busy_lock, "p")
  local busy_sidecar_count = 0
  local busy_notified = false
  vim.notify = function(message)
    if tostring(message):find("another Neovim instance", 1, true) then busy_notified = true end
  end
  issue_index._set_runner_for_test(function(_, _, callback)
    busy_sidecar_count = busy_sidecar_count + 1
    callback({ code = 0, stdout = "{}", stderr = "", output = "{}" })
  end)
  issue_index.sync_repo(vim.fn.getcwd(), busy_repo, { manual = true })
  assert_equals(busy_sidecar_count, 0, "busy cross-instance lock should skip sidecar state")
  assert_true(busy_notified, "manual busy lock should notify instead of failing state")
  vim.notify = original_notify
  vim.fn.delete(busy_lock, "rf")

  issue_index._reset_for_test()
  issue_index._set_progress_enabled_for_test(false)
  local fresh_repo = "mmgeorge/fresh-repo"
  write_snapshot(fresh_repo, {})
  local fresh_state_count = 0
  issue_index._set_runner_for_test(function(command, _, callback)
    assert_equals(command[2], "state", "fresh repo should only read state")
    fresh_state_count = fresh_state_count + 1
    callback({
      code = 0,
      stdout = vim.json.encode({
        repo = fresh_repo,
        open_historical_complete = true,
        last_open_checked_at = os.time(),
      }),
      stderr = "",
      output = "{}",
    })
  end)
  issue_index.sync_repo(vim.fn.getcwd(), fresh_repo, { manual = false })
  assert_equals(fresh_state_count, 1, "fresh repo should check state once")
  assert_true(vim.uv.fs_stat(issue_index.sync_lock_path(fresh_repo)) == nil, "no-op state check should release sync lock")

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
  vim.g.github_issue_index_hostname = "devtopia.esri.com"
  local notifications = {}
  local gh_commands = {}
  vim.notify = function(message, level, opts)
    notifications[#notifications + 1] = {
      message = tostring(message),
      level = level,
      opts = opts or {},
    }
    return #notifications
  end
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
  issue_index._set_gh_runner_for_test(function(command, _, callback)
    gh_commands[#gh_commands + 1] = command
    local stdout = vim.json.encode({
      data = {
        rateLimit = cursor == "cursor-1" and vim.NIL or { remaining = 5000 },
        repository = {
          issues = {
            totalCount = 2000,
            pageInfo = { hasNextPage = false, endCursor = vim.NIL },
            nodes = {
              {
                number = 100,
                title = "First synced issue",
                state = "OPEN",
                url = "https://github.com/mmgeorge/test-repo/issues/100",
                createdAt = "2026-06-17T20:00:00Z",
                updatedAt = "2026-06-17T20:00:00Z",
                body = "First body",
              },
              {
                number = 101,
                title = "Second synced issue",
                state = "OPEN",
                url = "https://github.com/mmgeorge/test-repo/issues/101",
                createdAt = "2026-06-17T20:01:00Z",
                updatedAt = "2026-06-17T20:01:00Z",
                body = "Second body",
              },
            },
          },
        },
      },
    })
    callback({ code = 0, stdout = stdout, stderr = "", output = stdout })
  end)

  issue_index.sync_repo(vim.fn.getcwd(), repo, { manual = true })
  vim.notify = original_notify
  local progress_id = "github_issue_index:" .. repo
  assert_equals(table.concat(gh_commands[1] or {}, " "), "gh api --hostname devtopia.esri.com graphql --input -", "enterprise GraphQL command mismatch")
  local saw_active_progress = false
  local saw_download_count = false
  local saw_indexing = false
  local saw_snapshot = false
  local saw_done = false
  local saw_spinner_callback = false
  for _, notification in ipairs(notifications) do
    if notification.opts.id == progress_id then
      assert_equals(notification.opts.title, "GitHub Issues", "progress notification title mismatch")
      if notification.opts.timeout == false then
        saw_active_progress = true
        if type(notification.opts.opts) == "function" then saw_spinner_callback = true end
      end
      if notification.message == "Syncing issues 2/2000" then saw_download_count = true end
      if notification.message == "Indexing issues 2/2000" then saw_indexing = true end
      if notification.message == "Writing issue snapshot 2/2000" then saw_snapshot = true end
      if notification.message == "Issues synced 2/2000" and notification.opts.timeout == 1200 then saw_done = true end
    end
  end
  assert_true(saw_active_progress, "issue sync progress notification did not stay active")
  assert_true(saw_spinner_callback, "issue sync progress notification did not provide a spinner callback")
  assert_true(saw_download_count, "issue sync progress did not report download count")
  assert_true(saw_indexing, "issue sync progress did not report indexing phase")
  assert_true(saw_snapshot, "issue sync progress did not report snapshot phase")
  assert_true(saw_done, "issue sync progress did not auto-dismiss on success")

  issue_index._reset_for_test()
  issue_index._set_progress_enabled_for_test(true)
  vim.notify = function()
    error("simulated notifier failure")
  end
  issue_index._set_runner_for_test(function(command, _, callback)
    local action = command[2]
    if action == "state" or action == "upsert-page" or action == "snapshot" then
      callback({ code = 0, stdout = vim.json.encode(action == "state" and {} or { upserted = 1 }), stderr = "", output = "{}" })
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
            totalCount = 1,
            pageInfo = { hasNextPage = false, endCursor = vim.NIL },
            nodes = {
              {
                number = 301,
                title = "Notify failure should not break sync",
                state = "OPEN",
                url = "https://github.com/mmgeorge/test-repo/issues/301",
                createdAt = "2026-06-17T20:00:00Z",
                updatedAt = "2026-06-17T20:00:00Z",
              },
            },
          },
        },
      },
    })
    callback({ code = 0, stdout = stdout, stderr = "", output = stdout })
  end)
  issue_index.sync_repo(vim.fn.getcwd(), repo, { manual = true })
  vim.notify = original_notify

  issue_index._reset_for_test()
  issue_index_builder._reset_for_test()
  issue_index._set_progress_enabled_for_test(false)
  vim.g.github_issue_index_hostname = nil
  vim.g.github_issue_index_log = true
  local sidecar_ready
  issue_index_builder.ensure(function(result)
    sidecar_ready = result
  end)
  assert_true(vim.wait(10000, function() return sidecar_ready ~= nil end, 10), "sidecar builder did not finish")
  assert_true(sidecar_ready.ok, sidecar_ready.message)

  local e2e_repo = "mmgeorge/e2e-repo"
  local gh_page_count = 0
  local large_body = string.rep("large enterprise issue body ", 1000)
  issue_index._set_gh_runner_for_test(function(command, input, callback)
    assert_equals(table.concat(command, " "), "gh api graphql --input -", "mocked e2e GraphQL command mismatch")
    local request = vim.json.decode(input)
    assert_true(not request.query:find(" body", 1, true), "issue sync GraphQL query should not request issue bodies")
    local cursor = request.variables and request.variables.cursor or nil
    gh_page_count = gh_page_count + 1
    local nodes
    local page_info
    if not cursor then
      nodes = {
        {
          number = 201,
          title = "First synced page issue",
          state = "OPEN",
          url = "https://github.com/mmgeorge/e2e-repo/issues/201",
          createdAt = "2026-06-17T20:00:00Z",
          updatedAt = "2026-06-17T20:03:00Z",
          body = large_body,
          labels = { nodes = { { name = "enhancement", color = "00ff00" } } },
        },
        {
          number = 202,
          title = "Second synced page issue",
          state = "OPEN",
          url = "https://github.com/mmgeorge/e2e-repo/issues/202",
          createdAt = "2026-06-17T20:01:00Z",
          updatedAt = "2026-06-17T20:02:00Z",
          body = large_body,
          labels = { nodes = { { name = "bug", color = "ff0000" } } },
        },
      }
      page_info = { hasNextPage = true, endCursor = "cursor-1" }
    elseif cursor == "cursor-1" then
      nodes = {
        {
          number = 203,
          title = "Final synced page issue",
          state = "OPEN",
          url = "https://github.com/mmgeorge/e2e-repo/issues/203",
          createdAt = "2026-06-17T20:02:00Z",
          updatedAt = "2026-06-17T20:01:00Z",
          body = large_body,
          labels = { nodes = { { name = "docs", color = "0000ff" } } },
        },
      }
      page_info = { hasNextPage = false, endCursor = vim.NIL }
    else
      callback({ code = 1, stdout = "", stderr = "unexpected cursor: " .. tostring(cursor), output = "unexpected cursor: " .. tostring(cursor) })
      return
    end

    local stdout = vim.json.encode({
      data = {
        rateLimit = { remaining = 5000 },
        repository = {
          issues = {
            totalCount = 3,
            pageInfo = page_info,
            nodes = nodes,
          },
        },
      },
    })
    callback({ code = 0, stdout = stdout, stderr = "", output = stdout })
  end)

  issue_index.sync_repo(vim.fn.getcwd(), e2e_repo, { manual = true })
  local e2e_snapshot
  local e2e_done = vim.wait(10000, function()
    local path = issue_index.snapshot_path(e2e_repo)
    if not vim.uv.fs_stat(path) then return false end
    local ok_decode, decoded = pcall(vim.json.decode, table.concat(vim.fn.readfile(path), "\n"))
    if not ok_decode or type(decoded) ~= "table" or decoded.issue_count ~= 3 then return false end
    e2e_snapshot = decoded
    return true
  end, 10)
  if not e2e_done then
    local snapshot_path = issue_index.snapshot_path(e2e_repo)
    local snapshot_text = vim.uv.fs_stat(snapshot_path) and table.concat(vim.fn.readfile(snapshot_path), "\n") or "<missing>"
    local log_path = issue_index.log_path(e2e_repo)
    local log_text = vim.uv.fs_stat(log_path) and table.concat(vim.fn.readfile(log_path), "\n") or "<missing>"
    error(
      "mocked GitHub sync did not write complete real-sidecar snapshot"
        .. "\ngh_page_count="
        .. tostring(gh_page_count)
        .. "\nsnapshot="
        .. snapshot_text
        .. "\nlog="
        .. log_text,
      2
    )
  end
  assert_equals(gh_page_count, 2, "mocked GitHub sync should fetch both pages")
  assert_equals(#e2e_snapshot.issues, 3, "snapshot issue count mismatch")
  assert_true(e2e_snapshot.issues[1].body == nil, "snapshot must omit issue bodies")
  assert_true((vim.uv.fs_stat(issue_index.snapshot_path(e2e_repo)).size or 0) < #large_body, "snapshot should stay compact even when mocked GitHub returns large bodies")
  local e2e_items = issue_index.search(e2e_repo, "enhancement", { limit = 10 })
  assert_equals(#e2e_items, 1, "real-sidecar snapshot search should find synced label")
  assert_equals(e2e_items[1].number, 201, "real-sidecar snapshot search returned wrong issue")
  local log_text = table.concat(vim.fn.readfile(issue_index.log_path(e2e_repo)), "\n")
  assert_true(log_text:find("sidecar:upsert-page:start", 1, true) ~= nil, "sync log should include upsert start")
  assert_true(log_text:find("sidecar:snapshot:finish", 1, true) ~= nil, "sync log should include snapshot finish")
  assert_true(log_text:find("output_size=", 1, true) ~= nil, "sync log should include snapshot output size")
end, debug.traceback)

repo_cache.set_data_dir_for_test(nil)
issue_index._reset_for_test()
issue_index_builder._reset_for_test()
vim.notify = original_notify
vim.g.github_issue_index_hostname = original_issue_index_hostname
vim.g.github_issue_index_log = original_issue_index_log
vim.fn.delete(test_root, "rf")
if not ok then
  print(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
