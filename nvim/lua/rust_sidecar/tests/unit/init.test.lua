-- Unit tests for `rust_sidecar` (init.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["sidecar_manual"] = function()
  ---@type RustSidecarLease?
  local lease
  MiniTest.finally(function()
    if lease then pcall(lease.release) end
  end)
  local root = temp.directory("sidecar")
  local builder = require("rust_sidecar").new({ crate_name = "fixture", binary_target = "fixture", locked = true,
    crate_dir = function() return root end,
    artifact_root = function() return root end,
  })
  local original_hash = vim.fn.sha256
  local original_has, original_copyfile = vim.fn.has, vim.uv.fs_copyfile
  local image = require("rust_sidecar.windows_image")
  if original_has("win32") ~= 1 then image.pin = function() return function() end end end
  vim.fn.has = function(feature) return feature == "win32" and 0 or original_has(feature) end
  vim.uv.fs_copyfile = function() error("Unix startup copied the executable") end
  vim.fn.mkdir(vim.fs.dirname(builder.binary_path()), "p")
  vim.fn.writefile({ "Unix build output" }, builder.binary_path())
  builder.acquire(builder.binary_path(), function(result, acquire_error)
    assert(not acquire_error, acquire_error)
    lease = result
  end)
  assert(lease and lease.path == builder.binary_path(), "Unix startup did not reuse the build output")
  assert(not vim.uv.fs_stat(vim.fs.joinpath(root, "leases")), "Unix startup created an executable lease")
  lease.release()
  lease.release()
  eq(vim.fn.readfile(builder.binary_path())[1], "Unix build output", { fail_reason = "Unix release removed the build output" })
  lease = nil
  vim.uv.fs_copyfile = original_copyfile
  builder._host_reset_for_test()
  vim.fn.has = function(feature) return feature == "win32" and 1 or original_has(feature) end
  vim.system = function(command, _, callback)
    if command[1] ~= "tasklist" then error("startup invoked a compiler") end
    callback({ code = 0, signal = 0, stdout = ('"nvim.exe","%s","Console","1","1 K"\n')
      :format(vim.fn.getpid()), stderr = "" })
  end
  vim.fn.sha256 = function(value) assert(value:sub(1,1) == "[" or value == vim.uv.fs_realpath(builder.binary_path()), "startup hashed file contents") return original_hash(value) end
  vim.fn.mkdir(vim.fs.dirname(builder.binary_path()), "p")
  vim.fn.writefile({ "manually built executable" }, builder.binary_path())
  local leases_root = vim.fs.joinpath(root, "leases")
  local stale_root = vim.fs.joinpath(leases_root, "999999-1")
  local live_root = vim.fs.joinpath(leases_root, ("%s-1"):format(vim.fn.getpid()))
  local reused_root = vim.fs.joinpath(leases_root, ("%s-1"):format(vim.uv.os_getppid()))
  vim.fn.mkdir(stale_root, "p")
  vim.fn.mkdir(live_root, "p")
  vim.fn.mkdir(reused_root, "p")
  vim.fn.writefile({ "stale" }, vim.fs.joinpath(stale_root, "fixture.exe"))
  vim.fn.writefile({ "live" }, vim.fs.joinpath(live_root, "fixture.exe"))
  vim.fn.writefile({ "reused" }, vim.fs.joinpath(reused_root, "fixture.exe"))
  ---@type RustSidecarExecutableResult?
  local available
  builder.ensure(function(result) available = result end)
  assert(available and available.ok and available.path == builder.binary_path())
  builder.acquire(available.path, function(result, copy_error)
    assert(not copy_error, copy_error)
    lease = result
  end)
  wait_until(function() return lease ~= nil end, "copy did not finish")
  wait_until(function() return not vim.uv.fs_stat(stale_root) end, "stale lease was retained", { timeout_ms = 3000 })
  wait_until(function() return not vim.uv.fs_stat(reused_root) end, "reused PID lease was retained", { timeout_ms = 3000 })
  assert(vim.uv.fs_stat(live_root), "live editor lease was removed")
  assert(lease.path ~= available.path)
  vim.fn.writefile({ "rebuilt executable" }, builder.binary_path())
  eq(vim.fn.readfile(lease.path)[1], "manually built executable", { fail_reason = "rebuild changed a running host's copy" })
  lease.release()
  lease.release()
  assert(vim.uv.fs_stat(lease.path), "process exit discarded the reusable image")
  local copy_failure
  builder.acquire(root .. "/missing", function(result, copy_error)
    assert(not result)
    copy_failure = copy_error
  end)
  wait_until(function() return copy_failure ~= nil end, "failed copy did not report its error")
  assert(copy_failure:find("Failed to copy", 1, true))
  builder.crate_dir = function() return root end
  vim.fn.delete(builder.binary_path())
  vim.fn.executable = function() return 1 end
  ---@type { message: string, level: integer, options: table }[]
  local notification = {}
  vim.notify = function(message, level, options)
    notification[#notification + 1] = { message = message, level = level, options = options }
  end
  local spawned = 0
  ---@type fun(failure: string?, data: string?)
  local stderr
  ---@type fun(result: vim.SystemCompleted)?
  local complete_build
  vim.system = function(command, options, callback)
    spawned = spawned + 1
    eq(command, builder.build_command())
    assert(vim.tbl_contains(command, "--release") and vim.tbl_contains(command, "--locked"))
    assert(options.text and options.stdout and options.stderr)
    assert(options.cwd == root, "Cargo did not use the crate's toolchain directory")
    stderr = options.stderr
    complete_build = callback
  end
  available = nil
  ---@type RustSidecarExecutableResult?
  local concurrent
  builder.ensure(function(result) available = result end)
  builder.ensure(function(result) concurrent = result end)
  assert(spawned == 1 and not available and not concurrent)
  assert(#notification == 1 and notification[1].options.timeout == 10000)
  stderr(nil, "   Compil")
  stderr(nil, "ing forge v0.1.0\n")
  wait_until(function() return #notification >= 3 end, "build progress was not published")
  assert(notification[#notification].message:find("Compiling forge v0.1.0", 1, true))
  assert(notification[1].options.id == notification[#notification].options.id)
  vim.fn.writefile({ "automatically built executable" }, builder.binary_path())
  complete_build({ code = 0, signal = 0, stdout = "", stderr = "" })
  wait_until(function() return available ~= nil and concurrent ~= nil end, "completed build did not settle both waiters")
  assert(available.ok and concurrent.ok and available.path == builder.binary_path())
  assert(notification[#notification].message:find("Build completed", 1, true))
  eq(notification[#notification].options.timeout, 3000)
  builder.ensure(function(result) assert(result.ok) end)
  eq(spawned, 1, { fail_reason = "existing executable triggered another build" })

  available, concurrent = nil, nil
  ---@type RustSidecarExecutableResult?
  local startup_result
  builder.build(function(result) available = result end)
  builder.build(function(result) concurrent = result end)
  builder.ensure(function(result) startup_result = result end)
  assert(spawned == 2 and not available and not concurrent and not startup_result,
    "explicit builds and startup did not share the active Cargo process")
  complete_build({ code = 0, signal = 0, stdout = "", stderr = "" })
  wait_until(function() return available ~= nil and concurrent ~= nil and startup_result ~= nil end, "shared build did not settle every waiter")
  assert(available.ok and concurrent.ok and startup_result.ok)

  vim.fn.delete(builder.binary_path())
  available = nil
  builder.ensure(function(result) available = result end)
  stderr(nil, "fixture compiler error\n")
  complete_build({ code = 101, signal = 0, stdout = "", stderr = nil })
  wait_until(function() return available ~= nil end, "failed build did not settle")
  assert(not available.ok and available.message:find("fixture compiler error", 1, true))
  eq(notification[#notification].level, vim.log.levels.ERROR)
  assert(notification[#notification].message:find("Build failed", 1, true))
  local finished_count = #notification
  stderr(nil, "late output\n")
  vim.wait(1100, function() return false end, 10)
  assert(#notification == finished_count, "completed build retained progress updates")

  available = nil
  builder.ensure(function(result) available = result end)
  eq(spawned, 4, { fail_reason = "failed build could not be retried" })
  complete_build({ code = 0, signal = 0, stdout = "", stderr = "" })
  wait_until(function() return available ~= nil end, "build without an executable did not settle")
  assert(not available.ok and available.message:find("produced no executable", 1, true))
  assert(available.message:find(builder.binary_path(), 1, true))

  vim.system = function() error("fixture spawn failure") end
  available = nil
  builder.ensure(function(result) available = result end)
  wait_until(function() return available ~= nil end, "spawn failure did not settle")
  assert(not available.ok and available.message:find("fixture spawn failure", 1, true))
  vim.fn.executable = function() return 0 end
  builder.ensure(function(result) available = result end)
  assert(not available.ok and available.message:find("cargo is not executable", 1, true))
end

return T
