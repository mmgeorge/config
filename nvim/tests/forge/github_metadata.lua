vim.loader.enable(false)
local users = require("github.repo_users")
local cache = require("github.repo_cache")
local directory = vim.fn.tempname()
local original_notify = vim.notify
local notifications = 0
vim.notify = function() notifications = notifications + 1 end
cache.set_data_dir_for_test(directory)

local ok, failure = xpcall(function()
  local pending, observed
  users._set_runner_for_test(function(params, callback) observed, pending = params, callback end)
  local calls, result = 0, nil
  users.fetch_async({ repo = "Owner/Repo", cwd = directory, ttl_seconds = 7, callback = function(value)
    calls, result = calls + 1, value
  end })
  assert(observed.cache_directory == cache.repo_dir("Owner/Repo") and observed.directory == directory)
  assert(observed.request.repository.hostname == cache.hostname() and observed.request.ttl_seconds == 7)
  local metadata = { repo = "owner/repo", hostname = cache.hostname(), fetched_at = os.time(), contributors = {} }
  pending(metadata, nil)
  pending(nil, "late failure")
  assert(calls == 1 and result.ok and result.metadata == metadata)
  assert(vim.uv.fs_stat(cache.metadata_path("Owner/Repo")) == nil, "Lua wrote host-owned metadata")

  for _, invalid in ipairs({ {}, { repo = "foreign/repo", fetched_at = os.time(), contributors = {} },
    { repo = "Owner/Repo", hostname = "foreign.example", fetched_at = os.time(), contributors = {} },
    { repo = "Owner/Repo", fetched_at = -1, contributors = {} } }) do
    users.fetch_async({ repo = "Owner/Repo", callback = function(value) result = value end })
    pending(invalid, nil)
    assert(not result.ok and result.message:find("invalid repository metadata", 1, true))
  end
  users.fetch_async({ repo = "Owner/Repo", callback = function(value) result = value end })
  pending(nil, "permission denied")
  assert(not result.ok and result.message == "permission denied")
  users.fetch_async({ repo = "Owner/Repo", callback = function(value) result = value end })
  cache.set_data_dir_for_test(directory .. "-changed")
  pending(metadata, nil)
  assert(not result.ok and result.message:find("context changed", 1, true))
  cache.set_data_dir_for_test(directory)

  users._set_runner_for_test(function() error("request failed") end)
  users.fetch_async({ repo = "Owner/Repo", callback = function(value) result = value end })
  assert(not result.ok and result.message:find("request failed", 1, true))
  local dispatched = 0
  users._set_runner_for_test(function(_, callback) dispatched, pending = dispatched + 1, callback end)
  cache.ensure_metadata(directory, "Owner/Repo", { remember_cwd = false })
  cache.ensure_metadata(directory, "Owner/Repo", { remember_cwd = false })
  assert(dispatched == 1, "metadata requests were not coalesced")
  pending(nil, "remote metadata failed")
  assert(vim.wait(1000, function() return notifications == 1 end, 1))
  cache.ensure_metadata(directory, "Owner/Repo", { remember_cwd = false })
  assert(dispatched == 2, "failed metadata request retained admission")
  pending(metadata, nil)
  assert(cache.repo_for_cwd(directory) == nil, "explicit metadata request overwrote cwd mapping")
end, debug.traceback)

users._set_runner_for_test(nil)
cache.set_data_dir_for_test(nil)
vim.notify = original_notify
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") else vim.cmd("qa!") end
