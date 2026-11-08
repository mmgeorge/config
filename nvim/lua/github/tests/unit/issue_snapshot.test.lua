-- Unit tests for `github.issue_snapshot` (issue_snapshot.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
local notify = dofile("nvim/lua/forge/tests/support/notify.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({
  hooks = isolation.hooks({
    post_case = function()
      local snapshot, index = package.loaded["github.issue_snapshot"], package.loaded["github.issue_index"]
      local cache = package.loaded["github.repo_cache"]
      if snapshot then snapshot.clear() end
      if index then index._reset_for_test() end
      if cache then cache.set_data_dir_for_test(nil) end
    end,
  }),
})

T["issue_snapshot"] = function()
  local snapshot = require("github.issue_snapshot")
  local index = require("github.issue_index")
  local cache = require("github.repo_cache")
  local root = temp.directory("cache")
  local repo = "owner/repo"
  local original_read = vim.uv.fs_read
  local original_close = vim.uv.fs_close
  notify.capture()
  cache.set_data_dir_for_test(root)

  ---@param path string
  ---@param title string
  ---@param repository? string
  local function publish(path, title, repository)
    repository = repository or repo
    vim.fn.mkdir(vim.fs.dirname(path), "p")
    local bytes = vim.json.encode({ repo = repository, state = "open", issue_count = 1, issues = {
      { repo = repository, number = 7, title = title, state = "OPEN", url = "https://github.com/owner/repo/issues/7", labels = { { name = "original" } } },
    } })
    local temporary = path .. ".next"
    vim.fn.writefile({ bytes }, temporary)
    assert(vim.uv.fs_rename(temporary, path))
  end

  ---@param path string
  ---@return string?
  local function load(path)
    local done, failure = false, nil
    snapshot.load(repo, path, function(result) failure = result done = true end, true)
    wait_until(function() return done end, "snapshot preload did not finish", { timeout_ms = 4000 })
    return failure
  end

  local path = index.snapshot_path(repo)
  eq(#index.search(repo, "first"), 0)
  publish(path, "First record")
  eq(load(path), nil)

  local original_open, original_stat, original_decode = vim.uv.fs_open, vim.uv.fs_stat, vim.json.decode
  vim.uv.fs_open = function() error("completion opened a file") end
  vim.uv.fs_stat = function() error("completion performed a filesystem stat") end
  vim.json.decode = function() error("completion decoded JSON") end
  local query_ok, query_failure = pcall(function()
    local items = index.search(repo, "first")
    assert(#items == 1 and items[1].number == 7)
    local listed = index.list(repo)
    listed[1].labels[1].name = "mutated"
    eq(#index.search(repo, "original"), 1, { fail_reason = "consumer mutated cached labels" })
  end)
  vim.uv.fs_open, vim.uv.fs_stat, vim.json.decode = original_open, original_stat, original_decode
  assert(query_ok, query_failure)

  publish(path, "Watcher replacement")
  wait_until(function() return #index.search(repo, "watcher") == 1 end, "atomic publication did not reload completion", { timeout_ms = 2000 })
  local temporary = path .. ".next"
  vim.fn.writefile({ "invalid JSON" }, temporary)
  assert(vim.uv.fs_rename(temporary, path))
  assert(load(path) ~= nil)
  eq(#index.search(repo, "watcher"), 1, { fail_reason = "failed preload discarded the last valid snapshot" })

  local long_title = string.rep("multibyte-\195\169 ", 20000)
  publish(path, long_title)
  eq(load(path), nil)
  assert(snapshot.records(repo)[1].title == long_title, "chunked reading changed UTF-8 bytes")

  snapshot.invalidate(repo)
  local held
  local closed = 0
  vim.uv.fs_read = function(descriptor, length, offset, callback)
    return original_read(descriptor, length, offset, function(read_failure, bytes)
      held = function() callback(read_failure, bytes) end
    end)
  end
  vim.uv.fs_close = function(descriptor, callback)
    closed = closed + 1
    return original_close(descriptor, callback)
  end
  local cancelled, completed = nil, false
  snapshot.load(repo, path, function(result) cancelled = result completed = true end, true)
  wait_until(function() return held ~= nil end, "held snapshot read did not start", { timeout_ms = 2000 })
  snapshot.invalidate(repo)
  eq(closed, 0, { fail_reason = "descriptor was released before its read callback completed" })
  held()
  wait_until(function() return completed end, "invalidated snapshot load did not settle", { timeout_ms = 2000 })
  assert(cancelled == "Snapshot load invalidated" and closed == 1)
  eq(#snapshot.records(repo), 0, { fail_reason = "invalidated read republished cached records" })
  vim.uv.fs_read, vim.uv.fs_close = original_read, original_close

  local oversized = vim.fs.joinpath(root, "oversized.json")
  local full_title = string.rep("complete-", 4 * 1024 * 1024)
  publish(oversized, full_title)
  eq(load(oversized), nil)
  assert(snapshot.records(repo)[1].title == full_title, "large snapshot was rejected by cache admission")
  publish(oversized, "small replacement")
  eq(load(oversized), nil)
  eq(snapshot.records(repo)[1].title, "small replacement")

  local cached_title = string.rep("cached-", 3 * 1024 * 1024)
  publish(path, cached_title)
  eq(load(path), nil)
  local held_refresh, refresh_done, refresh_failure
  vim.uv.fs_read = function(descriptor, length, offset, callback)
    return original_read(descriptor, length, offset, function(read_failure, bytes)
      held_refresh = function() callback(read_failure, bytes) end
    end)
  end
  snapshot.load(repo, path, function(result)
    refresh_failure, refresh_done = result, true
  end, true)
  wait_until(function() return held_refresh ~= nil end, "snapshot refresh read did not start", { timeout_ms = 2000 })
  vim.uv.fs_read = original_read
  local other_repo, other_done, other_failure = "other/repo", false, nil
  local other_path = vim.fs.joinpath(root, "other.json")
  local other_title = string.rep("other-", 3 * 1024 * 1024)
  publish(other_path, other_title, other_repo)
  snapshot.load(other_repo, other_path, function(result)
    other_failure, other_done = result, true
  end, true)
  wait_until(function() return other_done end, "second repository snapshot did not load", { timeout_ms = 4000 })
  eq(other_failure, nil, { fail_reason = other_failure })
  assert(snapshot.records(other_repo)[1].title == other_title, "pinned cache rejected complete content")
  assert(snapshot.records(repo)[1].title == cached_title, "pending refresh lost its valid snapshot")
  held_refresh()
  wait_until(function() return refresh_done end, "held snapshot refresh did not complete", { timeout_ms = 4000 })
  eq(refresh_failure, nil, { fail_reason = refresh_failure })

  snapshot.clear()
  local pending_open = {}
  local started, finished = 0, 0
  vim.uv.fs_open = function(_, _, _, callback)
    started = started + 1
    pending_open[#pending_open + 1] = callback
  end
  for number = 1, 8 do
    snapshot.load("queue/repo" .. number, vim.fs.joinpath(root, "missing" .. number), function(result)
      eq(result, nil, { fail_reason = result })
      finished = finished + 1
    end)
  end
  assert(started == 4 and #pending_open == 4, "preload exceeded four active reads")
  for number = 1, 8 do
    wait_until(function() return pending_open[number] ~= nil end, "queued preload did not start", { timeout_ms = 2000 })
    pending_open[number]("ENOENT")
    wait_until(function() return finished == number end, "queued preload did not complete", { timeout_ms = 2000 })
    assert(started - finished <= 4, "queue exceeded the active read bound")
  end
  assert(started == 8 and finished == 8)
end

return T
