vim.loader.enable(false)
local index = require("github.issue_index")
local cache = require("github.repo_cache")
local directory = vim.fn.tempname()
local repo = "owner/repo"
cache.set_data_dir_for_test(directory)
index._reset_for_test()
local original_notify = vim.notify
vim.notify = function() end

local function write_snapshot(revision, title)
  local path = index.snapshot_path(repo)
  vim.fn.mkdir(vim.fs.dirname(path), "p")
  vim.fn.writefile({ vim.json.encode({ repo = repo, revision = revision, state = "open", issue_count = 1,
    issues = { { repo = repo, number = 1, title = title, state = "OPEN", url = "https://github.com/owner/repo/issues/1" } },
  }) }, path)
end

local ok, failure = xpcall(function()
  write_snapshot(1, "first")
  local pending, calls, completed = nil, 0, 0
  index._set_storage_runner_for_test(function(params, callback)
    assert(params.request.operation == "reconcile_snapshot")
    assert(params.request.output == index.snapshot_path(repo) and params.database == index.db_path(repo))
    calls, pending = calls + 1, callback
  end)
  for _ = 1, 2 do index.reload_snapshot(repo, function(result) assert(result.ok, result.message) completed = completed + 1 end) end
  assert(calls == 1 and completed == 0 and #index.list(repo) == 0)
  for _ = 1, 20 do index.search(repo, "first") end
  assert(calls == 1, "completion performed storage work while preparation was pending")
  pending({ ready = true, state = { repo = repo, revision = 1 } }, nil)
  assert(vim.wait(1000, function() return completed == 2 end, 1))
  assert(index.list(repo)[1].title == "first")

  local attempts = 0
  index._set_storage_runner_for_test(function(_, callback)
    attempts = attempts + 1
    if attempts == 2 then write_snapshot(2, "second") end
    callback({ ready = true, state = { repo = repo, revision = 2 } }, nil)
  end)
  local result
  index.reload_snapshot(repo, function(value) result = value end, true)
  assert(vim.wait(1000, function() return result ~= nil end, 1))
  assert(result.ok and attempts >= 2 and attempts <= 3, "revision mismatch did not retry within its bound")
  assert(index.list(repo)[1].title == "second")

  for _, response in ipairs({ {}, { ready = true, state = { repo = "foreign/repo", revision = 2 } },
    { ready = true, state = { repo = repo, revision = -1 } } }) do
    result = nil
    index._set_storage_runner_for_test(function(_, callback) callback(response, nil) end)
    index.reload_snapshot(repo, function(value) result = value end, true)
    assert(vim.wait(1000, function() return result ~= nil end, 1))
    assert(not result.ok and index.list(repo)[1].title == "second")
  end
  result = nil
  index._set_storage_runner_for_test(function(_, callback) callback(nil, "database busy") end)
  index.reload_snapshot(repo, function(value) result = value end)
  assert(result and not result.ok and result.message == "database busy")
  assert(index.list(repo)[1].title == "second")
  result = nil
  index._set_storage_runner_for_test(function(_, callback) pending = callback end)
  index.reload_snapshot(repo, function(value) result = value end)
  index.invalidate_repo(repo)
  pending({ ready = true, state = { repo = repo, revision = 2 } }, nil)
  assert(result and not result.ok and #index.list(repo) == 0)
end, debug.traceback)

index._reset_for_test()
cache.set_data_dir_for_test(nil)
vim.notify = original_notify
vim.fn.delete(directory, "rf")
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") else vim.cmd("qa!") end
