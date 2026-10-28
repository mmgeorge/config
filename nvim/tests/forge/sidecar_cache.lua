vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local root = vim.fn.tempname()
vim.fn.mkdir(root, "p")
local source = root .. "/source.exe"
vim.fn.writefile({ "first executable version" }, source)
local cache = require("rust_sidecar.executable_cache")
local image = require("rust_sidecar.windows_image")
local original_copy, original_pin = vim.uv.fs_copyfile, image.pin
local leases = {}
if vim.fn.has("win32") ~= 1 then image.pin = function() return function() end end end
local function acquire()
  local result, failure, done
  cache.acquire(source, root, "fixture.exe", function(value, error)
    result, failure, done = value, error, true
    if value then leases[#leases + 1] = value end
  end)
  assert(vim.wait(3000, function() return done end, 5), "cache acquisition did not complete")
  assert(result, failure)
  return result
end
local ok, failure = xpcall(function()
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
  assert(vim.fn.readfile(second.path)[1] == "first executable version", "rebuild changed the active image")
  if vim.fn.has("win32") == 1 then
    vim.wait(1200, function() return false end, 10)
    assert(vim.uv.fs_stat(second.path), "cleanup removed a pinned image")
    second.release()
    local fourth = acquire()
    assert(fourth.cache_hit)
    assert(vim.wait(3000, function() return not vim.uv.fs_stat(second.path) end, 10), "released old image was not collected")
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
  assert(vim.wait(3000, function() return #results == 2 end, 5))
  assert(copied == previous_copies + 1 and results[1].path == results[2].path, "concurrent acquisition duplicated publication")
  local mutate = true
  vim.fn.writefile({ "intermediate executable" }, source)
  vim.uv.fs_copyfile = function(from, to, callback)
    return original_copy(from, to, function(copy_error)
      vim.schedule(function()
        if mutate then mutate = false vim.fn.writefile({ "final executable version" }, source) end
        callback(copy_error)
      end)
    end)
  end
  local current = acquire()
  assert(vim.fn.readfile(current.path)[1] == "final executable version", "source change published a stale executable")
end, debug.traceback)
for _, lease in ipairs(leases) do pcall(lease.release) end
vim.uv.fs_copyfile, image.pin = original_copy, original_pin
vim.fn.delete(root, "rf")
assert(ok, failure)
print("sidecar_cache: reuse, image pinning, rebuild isolation, and cleanup passed")
