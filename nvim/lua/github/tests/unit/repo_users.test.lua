-- Unit tests for `github.repo_users` (repo_users.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
local notify = dofile("nvim/lua/forge/tests/support/notify.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({
  hooks = isolation.hooks({
    post_case = function()
      local cache = package.loaded["github.repo_cache"]
      if cache then cache.set_data_dir_for_test(nil) end
    end,
  }),
})

T["metadata"] = function()
  local users = require("github.repo_users")
  local cache = require("github.repo_cache")
  local directory = temp.directory("cache")
  local changed_directory = temp.directory("cache-changed")
  local notifications = notify.capture()
  cache.set_data_dir_for_test(directory)

  local pending, observed
  users._set_runner_for_test(function(params, callback) observed, pending = params, callback end)
  local calls, result = 0, nil
  users.fetch_async({ repo = "Owner/Repo", ttl_seconds = 7, callback = function(value)
    calls, result = calls + 1, value
  end })
  assert(observed.cache_directory == cache.repo_dir("Owner/Repo") and observed.directory == nil)
  assert(observed.request.repository.hostname == cache.hostname() and observed.request.ttl_seconds == 7)
  local metadata = { repo = "owner/repo", hostname = cache.hostname(), fetched_at = os.time(), contributors = {} }
  pending(metadata, nil)
  pending(nil, "late failure")
  assert(calls == 1 and result.ok and result.metadata == metadata)
  eq(vim.uv.fs_stat(cache.metadata_path("Owner/Repo")), nil, { fail_reason = "Lua wrote host-owned metadata" })

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
  cache.set_data_dir_for_test(changed_directory)
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
  eq(dispatched, 1, { fail_reason = "metadata requests were not coalesced" })
  pending(nil, "remote metadata failed")
  wait_until(function() return #notifications == 1 end, "failed metadata request did not notify")
  cache.ensure_metadata(directory, "Owner/Repo", { remember_cwd = false })
  eq(dispatched, 2, { fail_reason = "failed metadata request retained admission" })
  pending(metadata, nil)
  eq(cache.repo_for_cwd(directory), nil, { fail_reason = "explicit metadata request overwrote cwd mapping" })
end

return T
