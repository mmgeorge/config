vim.loader.enable(false)
local snapshot = require("github.issue_snapshot")
local index = require("github.issue_index")
local cache = require("github.repo_cache")
local root = vim.fn.tempname()
vim.fn.mkdir(root, "p")
local owned_root = vim.uv.fs_realpath(root)
local repo = "owner/repo"
local original_notify = vim.notify
local original_read = vim.uv.fs_read
local original_open = vim.uv.fs_open
local original_close = vim.uv.fs_close
local notices = {}
vim.notify = function(message) notices[#notices + 1] = tostring(message) end
cache.set_data_dir_for_test(root)

---@param path string
---@param title string
local function publish(path, title)
  vim.fn.mkdir(vim.fs.dirname(path), "p")
  local bytes = vim.json.encode({ repo = repo, state = "open", issue_count = 1, issues = {
    { repo = repo, number = 7, title = title, state = "OPEN", url = "https://github.com/owner/repo/issues/7", labels = { { name = "original" } } },
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
  assert(vim.wait(2000, function() return done end, 5), "snapshot preload did not finish")
  return failure
end

local ok, failure = xpcall(function()
  local path = index.snapshot_path(repo)
  assert(#index.search(repo, "first") == 0)
  publish(path, "First record")
  assert(load(path) == nil)

  local original_open, original_stat, original_decode = vim.uv.fs_open, vim.uv.fs_stat, vim.json.decode
  vim.uv.fs_open = function() error("completion opened a file") end
  vim.uv.fs_stat = function() error("completion performed a filesystem stat") end
  vim.json.decode = function() error("completion decoded JSON") end
  local query_ok, query_failure = pcall(function()
    local items = index.search(repo, "first")
    assert(#items == 1 and items[1].number == 7)
    local listed = index.list(repo)
    listed[1].labels[1].name = "mutated"
    assert(#index.search(repo, "original") == 1, "consumer mutated cached labels")
  end)
  vim.uv.fs_open, vim.uv.fs_stat, vim.json.decode = original_open, original_stat, original_decode
  assert(query_ok, query_failure)

  publish(path, "Watcher replacement")
  assert(vim.wait(2000, function() return #index.search(repo, "watcher") == 1 end, 5), "atomic publication did not reload completion")
  local temporary = path .. ".next"
  vim.fn.writefile({ "invalid JSON" }, temporary)
  assert(vim.uv.fs_rename(temporary, path))
  assert(load(path) ~= nil)
  assert(#index.search(repo, "watcher") == 1, "failed preload discarded the last valid snapshot")

  local long_title = string.rep("multibyte-\195\169 ", 20000)
  publish(path, long_title)
  assert(load(path) == nil)
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
  assert(vim.wait(2000, function() return held ~= nil end, 5))
  snapshot.invalidate(repo)
  assert(closed == 0, "descriptor was released before its read callback completed")
  held()
  assert(vim.wait(2000, function() return completed end, 5))
  assert(cancelled == "Snapshot load invalidated" and closed == 1)
  assert(#snapshot.records(repo) == 0, "invalidated read republished cached records")
  vim.uv.fs_read, vim.uv.fs_close = original_read, original_close

  local oversized = vim.fs.joinpath(root, "oversized.json")
  local descriptor = assert(vim.uv.fs_open(oversized, "w", 438))
  assert(vim.uv.fs_ftruncate(descriptor, 16 * 1024 * 1024 + 1))
  assert(vim.uv.fs_close(descriptor))
  local limit_failure = load(oversized)
  assert(limit_failure and limit_failure:find("16 MiB", 1, true))

  snapshot.clear()
  local pending_open = {}
  local started, finished = 0, 0
  vim.uv.fs_open = function(_, _, _, callback)
    started = started + 1
    pending_open[#pending_open + 1] = callback
  end
  for number = 1, 8 do
    snapshot.load("queue/repo" .. number, vim.fs.joinpath(root, "missing" .. number), function(result)
      assert(result == nil, result)
      finished = finished + 1
    end)
  end
  assert(started == 4 and #pending_open == 4, "preload exceeded four active reads")
  for number = 1, 8 do
    assert(vim.wait(2000, function() return pending_open[number] ~= nil end, 5), "queued preload did not start")
    pending_open[number]("ENOENT")
    assert(vim.wait(2000, function() return finished == number end, 5), "queued preload did not complete")
    assert(started - finished <= 4, "queue exceeded the active read bound")
  end
  assert(started == 8 and finished == 8)
  vim.uv.fs_open = original_open
end, debug.traceback)

vim.uv.fs_open = original_open
vim.uv.fs_read, vim.uv.fs_close = original_read, original_close
snapshot.clear()
index._reset_for_test()
cache.set_data_dir_for_test(nil)
vim.notify = original_notify
if owned_root and vim.uv.fs_realpath(root) == owned_root then vim.fn.delete(root, "rf") end
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") else vim.cmd("qa!") end
