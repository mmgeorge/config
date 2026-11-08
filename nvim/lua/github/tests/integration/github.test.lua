-- Integration tests for the `github` area. Each scenario row runs in its own headless Neovim:
-- `.forge/checks/lua-tests.nu` passes the row's name as `arg[1]`. Tests that launch the Forge host
-- assume `rust-build` has produced a current executable.
local scenarios = {
  { name = "issue_index", run = function()
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
    revision = 0,
    issue_count = #issues,
    issues = issues,
  }) }, path)
  assert_equals(result, 0, "snapshot write failed")
  local loaded
  issue_index.reload_snapshot(repo, function(result) loaded = result end, true)
  assert_true(vim.wait(2000, function() return loaded ~= nil end, 5), "snapshot preload did not finish")
  assert_true(loaded.ok, loaded.message)
end

local test_root = vim.fs.joinpath(vim.fn.getcwd(), ".tmp-github-issue-index-test")
vim.fn.delete(test_root, "rf")
vim.fn.mkdir(test_root, "p")
repo_cache.set_data_dir_for_test(test_root)
issue_index._reset_for_test()
issue_index._set_progress_enabled_for_test(false)
local function recover_fixture(params, callback)
  assert_equals(params.request.operation, "reconcile_snapshot", "unexpected fixture storage operation")
  callback({ ready = true, republished = false, state = { repo = params.repo, revision = 0 } }, nil)
end
issue_index._set_storage_runner_for_test(recover_fixture)
local host_client = require("forge.client")
local original_request = host_client.request
local original_hostname = repo_cache.hostname
local original_system = vim.system
local host_builder = require("forge.builder")
local original_host_ensure = host_builder.ensure
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

  vim.g.github_issue_index_hostname = repo_cache.hostname()
  local sync_result
  local sync_callback
  local sync_params
  local request_count = 0
  local notice = {}
  vim.notify = function(message, level, options)
    notice[#notice + 1] = { message = tostring(message), level = level, options = options or {} }
  end
  issue_index._set_sync_runner_for_test(function(params, callback, progress)
    request_count = request_count + 1
    sync_params, sync_callback = params, callback
  end)
  issue_index.sync_repo(test_root, repo, { manual = true, scope = "all", on_complete = function(result) sync_result = result end })
  assert_equals(sync_params.request.repository.hostname, repo_cache.hostname(), "host identity mismatch")
  assert_equals(sync_params.request.repository.owner, "mmgeorge", "repository owner mismatch")
  assert_equals(sync_params.request.scope, "all", "all scope was not forwarded")
  assert_equals(sync_params.request.manual, true, "manual option was not forwarded")
  assert_equals(sync_params.database, issue_index.db_path(repo), "database path changed")
  assert_equals(sync_params.request.snapshot, issue_index.snapshot_path(repo), "snapshot path changed")
  assert_equals(sync_params.directory, test_root, "checkout context changed")
  assert_equals(sync_result, nil, "sync completed before its host response")
  local busy_result
  issue_index.sync_repo(test_root, repo, { manual = true, on_complete = function(result) busy_result = result end })
  assert_true(busy_result and not busy_result.ok, "duplicate sync was not rejected")
  assert_equals(request_count, 1, "duplicate sync reached the host")
  sync_callback({ refreshed = false, fetched = 0, pages = 0 }, nil)
  assert_true(vim.wait(2000, function() return sync_result ~= nil end, 5), "freshness response did not preload snapshot")
  assert_true(sync_result.ok and not sync_result.refreshed, "freshness response was lost")
  assert_equals(#issue_index.search(repo, "enhance"), 1, "freshness response cleared valid snapshot")

  for _, failure_case in ipairs({ "host failure", "malformed result", "request exception" }) do
    notice = {}
    sync_result = nil
    issue_index._set_sync_runner_for_test(function(_, callback)
      if failure_case == "host failure" then
        callback(nil, "host disconnected fixture")
        callback({ refreshed = true, fetched = 1, pages = 1 }, nil)
      elseif failure_case == "malformed result" then
        callback({ refreshed = true, fetched = -1, pages = 1 }, nil)
      else
        error("sync request exception fixture")
      end
    end)
    issue_index.sync_repo(test_root, repo, { manual = true, on_complete = function(result) sync_result = result end })
    assert_true(sync_result and not sync_result.ok, "sync failure did not complete")
    assert_equals(#notice, 1, "sync failure should notify exactly once")
    assert_true(notice[1].message:find("sync failed", 1, true) ~= nil, "sync failure lost request context")
  end

  local stale_repo = "org/repo"
  local stale_cwd = vim.fs.joinpath(test_root, "stale-cwd")
  repo_cache.remember_cwd_repo(stale_cwd, stale_repo)
  local stale_delete_callback
  host_client.request = function(request, callback)
    assert_equals(request.method, "github.issue.list", "stale cache deletion must use the shared host")
    assert_equals(request.params.request.operation, "delete_cache", "stale cache deletion operation")
    stale_delete_callback = callback
    return 1
  end
  local stale_count = 0
  issue_index._set_sync_runner_for_test(function(_, callback)
    stale_count = stale_count + 1
    callback(nil, "Could not resolve to a Repository with the name 'org/repo'.")
  end)
  issue_index.sync_repo(stale_cwd, stale_repo)
  assert_equals(repo_cache.repo_for_cwd(stale_cwd), nil, "stale mapping was not cleared")
  assert_true(stale_delete_callback ~= nil, "stale cache deletion was not requested")
  stale_delete_callback({ deleted = false }, nil)
  issue_index.sync_repo(stale_cwd, stale_repo)
  assert_equals(stale_count, 1, "missing repository retried automatically")
  host_client.request = original_request

  issue_index._set_sync_runner_for_test(function() error("mismatched host reached the service") end)
  vim.g.github_issue_index_hostname = "mismatched.example"
  sync_result = nil
  issue_index.sync_repo(test_root, repo, { manual = true, on_complete = function(result) sync_result = result end })
  assert_true(sync_result and not sync_result.ok and sync_result.message:find("does not match cache context", 1, true),
    "host/cache identity mismatch was not rejected")
  vim.g.github_issue_index_hostname = repo_cache.hostname()

  sync_result = nil
  issue_index._set_sync_runner_for_test(function(_, callback) sync_callback = callback end)
  issue_index.sync_repo(test_root, repo, { manual = true, on_complete = function(result) sync_result = result end })
  repo_cache.hostname = function() return "changed.example" end
  sync_callback({ refreshed = true, fetched = 1, pages = 1 }, nil)
  assert_true(sync_result and not sync_result.ok and sync_result.message:find("context changed", 1, true),
    "late sync response did not reject a changed cache namespace")
  repo_cache.hostname = original_hostname

  sync_result = nil
  issue_index._set_sync_runner_for_test(nil)
  vim.g.github_issue_index_hostname = nil
  vim.system = function(command, _, callback)
    assert_equals(command[1], "git", "hostname lookup ran an unexpected process")
    callback({ code = 1, stdout = "", stderr = "remote lookup failed fixture" })
    return { pid = 123, kill = function() end }
  end
  issue_index.sync_repo(test_root, repo, { manual = true, on_complete = function(result) sync_result = result end })
  assert_true(vim.wait(2000, function() return sync_result ~= nil end, 5), "hostname lookup failure did not complete")
  assert_true(not sync_result.ok and sync_result.message:find("remote lookup failed fixture", 1, true),
    "hostname lookup failure became a default host")
  vim.system = original_system
  vim.g.github_issue_index_hostname = repo_cache.hostname()

  issue_index._set_progress_enabled_for_test(true)
  notice = {}
  sync_result = nil
  issue_index._set_sync_runner_for_test(function(_, callback, progress)
    progress({ phase = "fresh", fetched = 0, total = vim.NIL, pages = 0 })
    callback({ refreshed = false, fetched = 0, pages = 0 }, nil)
  end)
  issue_index.sync_repo(test_root, repo, { on_complete = function(result) sync_result = result end })
  assert_true(vim.wait(2000, function() return sync_result ~= nil end, 5), "automatic fresh sync did not complete")
  assert_equals(#notice, 0, "automatic freshness skip displayed progress")
  notice = {}
  sync_result = nil
  issue_index._set_sync_runner_for_test(function(params, callback, progress)
    assert_true(params.progress, "progress was not requested")
    progress({ phase = "reading", fetched = 2, total = 2000, pages = 0 })
    progress({ phase = "indexing", fetched = 2, total = 2000, pages = 0 })
    progress({ phase = "publishing", fetched = 2, total = 2000, pages = 1 })
    progress({ phase = "rate_limited", fetched = 2, total = 2000, pages = 1, retry_after_ms = 60000 })
    callback({ refreshed = true, fetched = 2, pages = 1 }, nil)
    progress({ phase = "reading", fetched = 9999, total = 9999, pages = 1 })
  end)
  issue_index.sync_repo(test_root, repo, { manual = true, on_complete = function(result) sync_result = result end })
  assert_true(vim.wait(2000, function() return sync_result ~= nil end, 5), "progress sync did not finish")
  local seen = {}
  for _, notification in ipairs(notice) do seen[notification.message] = notification.options end
  assert_true(seen["Syncing issues 2/2000"], "reading progress lost its count")
  assert_true(seen["Indexing issues 2/2000"], "indexing progress was lost")
  assert_true(seen["Writing issue snapshot 2/2000"], "publication progress was lost")
  assert_equals(seen["Issues synced 2/2000"].timeout, 1200, "success progress did not dismiss")
  assert_true(not seen["Syncing issues 9999/9999"], "late progress was rendered")
  vim.notify = function() error("simulated notifier failure") end
  sync_result = nil
  issue_index.sync_repo(test_root, repo, { manual = true, on_complete = function(result) sync_result = result end })
  assert_true(vim.wait(2000, function() return sync_result ~= nil end, 5), "notification failure interrupted sync")
  assert_true(sync_result.ok, "notification failure changed successful sync")
  issue_index._reset_for_test()
  issue_index._set_progress_enabled_for_test(false)
  vim.notify = original_notify
  local prefetch_result
  local prefetch_notice
  vim.notify = function(message) prefetch_notice = tostring(message) end
  issue_index._set_storage_runner_for_test(function(params, callback)
    assert_equals(params.request.operation, "details", "prefetch should use one typed batch")
    callback(nil, "storage unavailable fixture")
  end)
  issue_index.prefetch_details(repo, { { number = 42 } }, {}, function(result) prefetch_result = result end)
  assert_true(prefetch_result and not prefetch_result.ok, "prefetch failure became an empty successful result")
  assert_true(prefetch_notice and prefetch_notice:find("storage unavailable fixture", 1, true) ~= nil, "prefetch failure was not notified")
  local detail_callback
  local detail_request_count = 0
  local detail_result
  issue_index._set_storage_runner_for_test(function(params, callback)
    assert_equals(params.request.operation, "detail", "detail lookup did not use typed storage")
    callback({ found = false }, nil)
  end)
  issue_index._set_detail_runner_for_test(function(params, callback)
    assert_equals(params.request.repository.hostname, repo_cache.hostname(), "detail hostname mismatch")
    assert_equals(params.request.number, 42, "detail issue number mismatch")
    detail_request_count = detail_request_count + 1
    detail_callback = callback
  end)
  issue_index.detail_async(test_root, repo, 42, {}, function() error("detail waiter fixture") end)
  issue_index.detail_async(test_root, repo, 42, {}, function(result) detail_result = result end)
  assert_equals(detail_request_count, 1, "duplicate detail request reached the host")
  local detail_record = { repo = repo, number = 42, fetched_at = os.time(), item = {
    repo = repo, number = 42, title = "Shared detail", body = "Owned body", kind = "issue",
  } }
  detail_callback(detail_record, nil)
  assert_true(detail_result and detail_result.ok and detail_result.cache_updated, "detail waiter failure prevented other completion")
  assert_equals(issue_index.cached_detail(repo, 42).item.body, "Owned body", "host detail was not cached")
  detail_callback(nil, "late failure")
  assert_true(detail_result.ok, "late callback replaced successful detail")
  issue_index.invalidate_repo(repo)
  assert_equals(issue_index.cached_detail(repo, 42), nil, "repository invalidation retained detail memory")
  for _, failure_case in ipairs({ "host failure", "invalid result", "request exception" }) do
    detail_result = nil
    prefetch_notice = nil
    issue_index._set_detail_runner_for_test(function(_, callback)
      if failure_case == "host failure" then callback(nil, "detail host disconnected")
      elseif failure_case == "invalid result" then callback({ item = {} }, nil)
      else error("detail request exception") end
    end)
    issue_index.detail_async(test_root, repo, 42, { force = true }, function(result) detail_result = result end)
    assert_true(detail_result and not detail_result.ok, "detail failure became success")
    assert_true(prefetch_notice and prefetch_notice:find("detail fetch failed", 1, true), "detail failure was not notified")
  end
  issue_index._set_detail_runner_for_test(nil)
  issue_index._set_storage_runner_for_test(recover_fixture)
  vim.notify = original_notify

  local reloaded
  issue_index.reload_snapshot(repo, function(result) reloaded = result end)
  assert_true(vim.wait(2000, function() return reloaded ~= nil end, 5), "completion preload did not finish")
  assert_true(reloaded.ok, reloaded.message)
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

  local binary = vim.fs.joinpath(vim.fn.getcwd(), "nvim", "rust", "forge", "target", "debug", "forge")
  issue_index._set_storage_runner_for_test(nil)
  local fixture = vim.fs.joinpath(test_root, "gh")
  if vim.fn.has("win32") == 1 then binary, fixture = binary .. ".exe", fixture .. ".exe" end
  assert_true(vim.fn.executable(binary) == 1, "build the Forge debug executable before native issue tests")
  local compiled
  vim.system({ "rustup", "run", "1.94.0", "rustc", "--edition=2024",
    "nvim/rust/forge/crates/forge-github/tests/fixtures/gh.rs", "-o", fixture },
    { text = true, stdout = true, stderr = true, timeout = 30000 }, function(result) compiled = result end)
  assert_true(vim.wait(31000, function() return compiled ~= nil end, 5), "native gh fixture compilation did not finish")
  assert_equals(compiled.code, 0, "native gh fixture compilation failed: " .. tostring(compiled.stderr))
  host_builder.ensure = function(callback) callback({ ok = true, path = binary }) end
  host_client._host_set_launcher_for_test(function(command, options, on_exit)
    options.env = { PATH = test_root }
    return vim.system(command, options, on_exit)
  end)
  local native_repo = "mmgeorge/native-repo"
  local native_cwd = vim.fs.joinpath(test_root, "native")
  vim.fn.mkdir(native_cwd, "p")
  vim.fn.writefile({ repo_cache.hostname() }, vim.fs.joinpath(native_cwd, "host"), "b")
  vim.fn.writefile({ "success" }, vim.fs.joinpath(native_cwd, "mode"), "b")
  vim.fn.writefile({ vim.json.encode({ data = {
    rateLimit = { remaining = 5000 }, repository = { issues = {
      totalCount = 1, pageInfo = { hasNextPage = false, endCursor = vim.NIL }, nodes = { {
        number = 201, title = "Native sync issue", state = "OPEN",
        url = "https://" .. repo_cache.hostname() .. "/" .. native_repo .. "/issues/201",
        createdAt = "2026-06-17T20:00:00Z", updatedAt = "2026-06-17T20:00:00Z",
        labels = { nodes = { { name = "enhancement" } } },
      } },
    } },
  } }) }, vim.fs.joinpath(native_cwd, "response.json"))
  sync_result = nil
  issue_index.sync_repo(native_cwd, native_repo, { manual = true, on_complete = function(result) sync_result = result end })
  assert_true(vim.wait(10000, function() return sync_result ~= nil end, 5), "native sync did not complete")
  assert_true(sync_result.ok, "native sync failed: " .. vim.inspect(sync_result))
  assert_equals(sync_result.fetched, 1, "native sync count mismatch")
  assert_equals(#issue_index.search(native_repo, "enhancement"), 1, "native sync did not preload the published snapshot")
  assert_true(host_client._client.ready and not host_client._client.harness_ready, "issue sync initialized Harness")
  sync_result = nil
  issue_index.sync_repo(native_cwd, native_repo, { on_complete = function(result) sync_result = result end })
  assert_true(vim.wait(3000, function() return sync_result ~= nil end, 5), "native freshness check did not finish")
  assert_true(sync_result.ok and not sync_result.refreshed, "native freshness check fetched again")
  local old_snapshot = vim.json.decode(table.concat(vim.fn.readfile(issue_index.snapshot_path(native_repo)), "\n"))
  issue_index.invalidate_repo(native_repo)
  local native_reads_before_recovery = #vim.fn.globpath(native_cwd, "started.*", false, true)
  local committed
  host_client.request({ method = "github.issue.list", params = {
    database = issue_index.db_path(native_repo), repo = native_repo,
    request = { operation = "upsert_page", scope = "open", page = { completed = true, issues = {
      { repo = native_repo, number = 202, title = "Recovered after interruption", state = "OPEN",
        url = "https://" .. repo_cache.hostname() .. "/" .. native_repo .. "/issues/202", labels = {} },
    } } },
  } }, function(result, request_error) assert_true(not request_error, request_error) committed = result end)
  assert_true(vim.wait(3000, function() return committed ~= nil end, 5), "interrupted-publication fixture did not commit")
  assert_true(committed.state.revision > old_snapshot.revision, "database revision did not advance")
  local stale_snapshot = vim.json.decode(table.concat(vim.fn.readfile(issue_index.snapshot_path(native_repo)), "\n"))
  assert_equals(stale_snapshot.revision, old_snapshot.revision, "page commit unexpectedly published a snapshot")
  local recovered
  issue_index.reload_snapshot(native_repo, function(result) recovered = result end, true)
  assert_true(vim.wait(3000, function() return recovered ~= nil end, 5), "native snapshot recovery did not complete")
  assert_true(recovered.ok, recovered.message)
  assert_equals(#issue_index.search(native_repo, "Recovered"), 1, "native recovery did not preload the committed issue")
  local repaired = vim.json.decode(table.concat(vim.fn.readfile(issue_index.snapshot_path(native_repo)), "\n"))
  assert_equals(repaired.revision, committed.state.revision, "snapshot revision did not reconcile with the database")
  assert_equals(#vim.fn.globpath(native_cwd, "started.*", false, true), native_reads_before_recovery, "snapshot recovery performed a remote read")
  local native_detail = vim.json.decode(table.concat(vim.fn.readfile("nvim/rust/forge/crates/forge-github/tests/fixtures/detail.json"), "\n"))
  native_detail.number = 201
  native_detail.body = string.rep("日本語 detail \"quote\"\n", 40000)
  native_detail.url = "https://" .. repo_cache.hostname() .. "/" .. native_repo .. "/issues/201"
  native_detail.comments[1].url = native_detail.url .. "#issuecomment-123"
  vim.fn.writefile({ vim.json.encode(native_detail) }, vim.fs.joinpath(native_cwd, "response.json"))
  vim.fn.writefile({ native_repo }, vim.fs.joinpath(native_cwd, "repository"), "b")
  detail_result = nil
  issue_index.detail_async(native_cwd, native_repo, 201, { force = true }, function(result) detail_result = result end)
  assert_true(vim.wait(3000, function() return detail_result ~= nil end, 5), "native detail fetch did not complete")
  assert_true(detail_result.ok and detail_result.cache_updated, "native detail fetch failed: " .. vim.inspect(detail_result))
  assert_equals(detail_result.item.comments_count, 1, "native detail comment normalization failed")
  assert_true(detail_result.item.body == native_detail.body, "segmented native detail body was truncated")
  issue_index._clear_detail_memory_for_test()
  detail_result = nil
  issue_index.detail_async(native_cwd, native_repo, 201, {}, function(result) detail_result = result end)
  assert_true(vim.wait(3000, function() return detail_result ~= nil end, 5), "persisted native detail did not load")
  assert_true(detail_result.ok and detail_result.redb, "native detail did not persist before completion")
  assert_true(detail_result.item.body == native_detail.body, "segmented persisted detail body was truncated")
  vim.fn.writefile({ '[[{"login":"carol"},{"login":"alice"}]]' }, vim.fs.joinpath(native_cwd, "contributors.json"))
  vim.fn.writefile({ '[[{"login":"Alice","name":"Alice Developer"},{"login":"bob"}]]' }, vim.fs.joinpath(native_cwd, "collaborators.json"))
  local metadata_result
  require("github.repo_users").fetch_async({ cwd = native_cwd, repo = native_repo, callback = function(result) metadata_result = result end })
  assert_true(vim.wait(3000, function() return metadata_result ~= nil end, 5), "native metadata fetch did not complete")
  assert_true(metadata_result.ok, "native metadata fetch failed: " .. vim.inspect(metadata_result))
  assert_equals(#metadata_result.contributors, 3, "native metadata did not merge and deduplicate users")
  assert_equals(metadata_result.contributors[1].name, "Alice Developer", "native metadata lost collaborator name")
  assert_equals(#repo_cache.contributors(native_repo), 3, "native metadata callback preceded publication")
  assert_equals(repo_cache.contributors(native_repo)[2].name, nil, "missing metadata names must not become Lua null values")
  assert_true(repo_cache.metadata_fresh(native_repo), "native metadata freshness was not persisted")
  repo_cache.remember_cwd_repo(native_cwd, native_repo)
  local deletion_count
  local deletion_failure
  local native_lock = vim.fs.joinpath(repo_cache.repo_dir(native_repo), "issues", "sync.lock")
  vim.fn.mkdir(native_lock, "p")
  repo_cache.delete_current(native_repo, native_cwd, function(count, failure)
    deletion_count, deletion_failure = count, failure
  end)
  assert_true(vim.wait(3000, function() return deletion_count ~= nil end, 5), "busy deletion did not complete")
  assert_true(deletion_failure ~= nil, "deletion ignored another process's sync lock")
  assert_equals(#issue_index.search(native_repo, "enhancement"), 1, "busy deletion invalidated snapshot")
  vim.fn.delete(native_lock, "rf")
  deletion_count = nil
  repo_cache.delete_current(native_repo, native_cwd, function(count, failure)
    deletion_count, deletion_failure = count, failure
  end)
  assert_true(vim.wait(3000, function() return deletion_count ~= nil end, 5), "native deletion did not complete")
  assert_equals(deletion_failure, nil, "native deletion failed")
  assert_equals(deletion_count, 2, "native deletion did not remove repo and cwd cache")
  assert_equals(#issue_index.search(native_repo, "enhancement"), 0, "native deletion retained snapshot")
  assert_equals(issue_index.cached_detail(native_repo, 201), nil, "native deletion retained cached detail")
  assert_equals(#repo_cache.contributors(native_repo), 0, "native deletion retained repository metadata")
  local process = host_client._client.process
  host_client.host_stop()
  assert_equals(process:wait(2000).code, 0, "issue host failed to stop")
end, debug.traceback)

host_client._host_reset_for_test()
host_client.request = original_request
repo_cache.hostname = original_hostname
vim.system = original_system
host_builder.ensure = original_host_ensure
repo_cache.set_data_dir_for_test(nil)
issue_index._reset_for_test()
vim.notify = original_notify
vim.g.github_issue_index_hostname = original_issue_index_hostname
vim.g.github_issue_index_log = original_issue_index_log
vim.fn.delete(test_root, "rf")
if not ok then
  print(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
  end },
  { name = "repo_cache", run = function()
vim.loader.enable(false)

local cache = require("github.repo_cache")
local github_gh = require("github.gh")
local issue_index = require("github.issue_index")
local host_client = require("forge.client")
local original_request = host_client.request

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
  local source = require("forge.views.pr.reviewer_source").new({})
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
  repo_name = repo_name:lower()
  local path = issue_index.snapshot_path(repo_name)
  vim.fn.mkdir(vim.fs.dirname(path), "p")
  local result = vim.fn.writefile({ vim.json.encode({
    repo = repo_name,
    state = "open",
    issue_count = 4,
    revision = 0,
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
  local loaded
  issue_index.reload_snapshot(repo_name, function(result) loaded = result end, true)
  assert_true(vim.wait(2000, function() return loaded ~= nil end, 5), "issue snapshot preload did not finish")
  assert_true(loaded.ok, loaded.message)
end

local function run()
  vim.notify = capture_notify
  cache.set_data_dir_for_test(cache_root)
  issue_index._reset_for_test()
  issue_index._set_progress_enabled_for_test(false)
  issue_index._set_sync_runner_for_test(function(_, callback)
    callback({ refreshed = false, fetched = 0, pages = 0 }, nil)
  end)
  issue_index._set_storage_runner_for_test(function(params, callback)
    if params.request.operation == "reconcile_snapshot" then
      callback({ ready = true, republished = false, state = { repo = params.repo, revision = 0 } }, nil)
      return
    end
    local action = params.request.operation
    if action == "state" then
      local stdout = vim.json.encode({
        repo = normalized_repo,
        open_historical_complete = true,
        last_open_checked_at = os.time(),
      })
      callback(vim.json.decode(stdout), nil)
      return
    end
    callback({}, nil)
  end)
  github_gh.set_backend(github_backend)
  cache.remember_cwd_repo(cwd, normalized_repo)
  cache.set_base_branch(cwd, "master")
  write_issue_snapshot(normalized_repo)

  vim.fn.mkdir(cache.repo_dir(normalized_repo), "p")
  vim.fn.writefile({ vim.json.encode({ repo = normalized_repo, fetched_at = os.time(), contributors = {
    { login = "alice-dev", name = "Alice Developer" }, { login = "bobtown" }, { login = "mgeorge-esri" },
  } }) }, cache.metadata_path(normalized_repo))

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
  assert_true(not require("forge.views.pr.reviewer_source").new({}):enabled(), "completion source enabled without buffer whitelist")
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

  local deletion_callback
  local deletion_params
  host_client.request = function(message, callback)
    local params = message.params
    assert_true(message.method == "github.issue.list", "cache deletion bypassed the issue service")
    assert_true(params.request.operation == "delete_cache", "cache deletion sent the wrong operation")
    deletion_callback = callback
    deletion_params = params
    return 1
  end
  local deleted
  local deletion_failure
  cache.delete_current(nil, cwd, function(count, failure)
    deleted = count
    deletion_failure = failure
  end)
  assert_true(deleted == nil, "deletion completed before the host response")
  assert_true(cache.repo_for_cwd(cwd) == normalized_repo, "pending deletion cleared the cwd mapping")
  assert_true(#issue_index.list(normalized_repo:lower()) == 4, "pending deletion invalidated the snapshot")
  deletion_callback(nil, "repository cache is busy")
  assert_true(deleted == 0 and deletion_failure ~= nil, "busy deletion did not fail")
  assert_true(saw_notification_containing("repository cache is busy"), "busy deletion did not notify")
  assert_true(cache.repo_for_cwd(cwd) == normalized_repo, "failed deletion cleared the cwd mapping")
  assert_true(#issue_index.list(normalized_repo:lower()) == 4, "failed deletion invalidated the snapshot")
  deleted = nil
  cache.delete_current(nil, cwd, function(count, failure)
    deleted = count
    deletion_failure = failure
  end)
  assert_true(deletion_params.database == vim.fs.joinpath(cache.repo_dir(normalized_repo), "issues", "issues.redb"),
    "deletion changed the cache path contract")
  vim.fn.delete(cache.repo_dir(normalized_repo), "rf")
  deletion_callback({ deleted = true }, nil)
  assert_true(deleted == 2 and deletion_failure == nil, "delete_current did not remove mapped repo and cwd cache")
  assert_true(#issue_index.list(normalized_repo:lower()) == 0, "successful deletion retained the snapshot")
  assert_true(#cache.contributors(normalized_repo) == 0, "contributors were not deleted with repo cache")
  assert_true(cache.repo_for_cwd(cwd) == nil, "cwd repo mapping was not deleted")
  assert_true(cache.get_base_branch(cwd) == nil, "base branch was not deleted with cwd cache")
  assert_true(vim.uv.fs_stat(review_path) == nil, "review draft was not deleted with repo cache")
  cache.remember_cwd_repo(cwd, normalized_repo)
  deleted = nil
  cache.delete_current(normalized_repo, cwd, function(count, failure)
    deleted = count
    deletion_failure = failure
  end)
  cache.remember_cwd_repo(cwd, "Owner/Changed")
  deletion_callback({ deleted = false }, nil)
  assert_true(deleted == 0 and deletion_failure ~= nil, "changed cwd context was not rejected")
  assert_true(cache.repo_for_cwd(cwd) == "Owner/Changed", "late deletion cleared a newer cwd mapping")
end

local ok, err = xpcall(run, debug.traceback)
vim.notify = original_notify
host_client.request = original_request
github_gh.reset_backend()
issue_index._reset_for_test()
cache.set_data_dir_for_test(nil)
vim.fn.delete(cache_root, "rf")
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
  end },
}

local selected = arg[1]
for _, scenario in ipairs(scenarios) do
  if scenario.name == selected then return scenario.run() end
end
error(("unknown scenario %q"):format(tostring(selected)))
