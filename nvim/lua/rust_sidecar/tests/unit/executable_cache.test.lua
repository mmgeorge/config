-- Unit tests for `rust_sidecar.executable_cache` (executable_cache.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["sidecar_cache"] = function()
  local leases = {}
  MiniTest.finally(function()
    for _, lease in ipairs(leases) do pcall(lease.release) end
  end)
  local root = temp.directory("cache")
  local source = root .. "/source.exe"
  vim.fn.writefile({ "first executable version" }, source)
  local cache = require("rust_sidecar.executable_cache")
  local image = require("rust_sidecar.windows_image")
  local original_copy = vim.uv.fs_copyfile
  -- The cache prunes superseded images from a deferred callback; capture it so cleanup runs on demand.
  local deferred_prunes = {}
  vim.defer_fn = function(callback) deferred_prunes[#deferred_prunes + 1] = callback end
  local function run_deferred_prunes()
    local ready = deferred_prunes
    deferred_prunes = {}
    for _, callback in ipairs(ready) do callback() end
  end
  -- Pruning visits one cache entry per scheduled step, so drain enough steps to cover every entry.
  local function drain_scheduled_steps()
    for _ = 1, 8 do
      local reached = false
      vim.schedule(function() reached = true end)
      wait_until(function() return reached end, "scheduler did not advance")
    end
  end
  if vim.fn.has("win32") ~= 1 then image.pin = function() return function() end end end
  local function acquire()
    local result, failure, done
    cache.acquire(source, root, "fixture.exe", function(value, error)
      result, failure, done = value, error, true
      if value then leases[#leases + 1] = value end
    end)
    wait_until(function() return done end, "cache acquisition did not complete", { timeout_ms = 3000 })
    assert(result, failure)
    return result
  end
  local copied = 0
  vim.uv.fs_copyfile = function(...)
    copied = copied + 1
    return original_copy(...)
  end
  local first = acquire()
  local second = acquire()
  assert(not first.cache_hit and second.cache_hit and copied == 1, "unchanged executable was recopied")
  assert(first.path == second.path and first.path ~= source)
  first.release()
  first.release()
  assert(vim.uv.fs_stat(first.path), "release deleted the reusable executable")
  vim.fn.writefile({ "next executable version" }, source)
  local stat = assert(vim.uv.fs_stat(source))
  assert(vim.uv.fs_utime(source, stat.atime.sec, stat.mtime.sec + 1))
  local third = acquire()
  assert(third.path ~= first.path and copied == 2, "rebuild reused an old image")
  eq(vim.fn.readfile(second.path)[1], "first executable version", { fail_reason = "rebuild changed the active image" })
  if vim.fn.has("win32") == 1 then
    run_deferred_prunes()
    drain_scheduled_steps()
    assert(vim.uv.fs_stat(second.path), "cleanup removed a pinned image")
    second.release()
    local fourth = acquire()
    assert(fourth.cache_hit)
    run_deferred_prunes()
    wait_until(function() return not vim.uv.fs_stat(second.path) end, "released old image was not collected", { timeout_ms = 3000 })
  end
  third.release()
  assert(vim.uv.fs_stat(third.path), "current cached image was removed")
  vim.fn.writefile({ "concurrent executable version" }, source)
  local results = {}
  local previous_copies = copied
  for _ = 1, 2 do
    cache.acquire(source, root, "fixture.exe", function(value, acquire_error)
      assert(value, acquire_error)
      results[#results + 1] = value
      leases[#leases + 1] = value
    end)
  end
  wait_until(function() return #results == 2 end, "concurrent acquisitions did not both complete", { timeout_ms = 3000 })
  assert(copied == previous_copies + 1 and results[1].path == results[2].path, "concurrent acquisition duplicated publication")
  local mutate = true
  vim.fn.writefile({ "intermediate executable" }, source)
  vim.uv.fs_copyfile = function(from, to, flags, callback)
    return original_copy(from, to, flags, function(copy_error)
      vim.schedule(function()
        if mutate then mutate = false vim.fn.writefile({ "final executable version" }, source) end
        callback(copy_error)
      end)
    end)
  end
  local current = acquire()
  eq(vim.fn.readfile(current.path)[1], "final executable version", { fail_reason = "source change published a stale executable" })
end

return T
