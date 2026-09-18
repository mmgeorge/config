vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local root = vim.fn.tempname()
vim.fn.mkdir(root, "p")
local builder = require("rust_sidecar").new({ crate_name = "fixture", binary_target = "fixture", locked = true,
  crate_dir = function() error("startup inspected compiler inputs") end,
  artifact_root = function() return root end,
})
local original_system, original_hash = vim.system, vim.fn.sha256
local original_executable, original_notify = vim.fn.executable, vim.notify
---@type RustSidecarLease?
local lease
local succeeded, failure = xpcall(function()
  vim.system = function(command, _, callback)
    if command[1] ~= "tasklist" then error("startup invoked a compiler") end
    callback({ code = 0, signal = 0, stdout = ('"nvim.exe","%s","Console","1","1 K"\n')
      :format(vim.fn.getpid()), stderr = "" })
  end
  vim.fn.sha256 = function() error("startup hashed contents") end
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
  assert(vim.wait(1000, function() return lease ~= nil end, 5), "copy did not finish")
  assert(vim.wait(1000, function() return not vim.uv.fs_stat(stale_root) end, 5), "stale lease was retained")
  assert(vim.wait(1000, function() return not vim.uv.fs_stat(reused_root) end, 5), "reused PID lease was retained")
  assert(vim.uv.fs_stat(live_root), "live editor lease was removed")
  assert(lease.path ~= available.path)
  vim.fn.writefile({ "rebuilt executable" }, builder.binary_path())
  assert(vim.fn.readfile(lease.path)[1] == "manually built executable", "rebuild changed a running host's copy")
  lease.release()
  lease.release()
  assert(not vim.uv.fs_stat(lease.path), "process exit retained its copy")
  local copy_failure
  builder.acquire(root .. "/missing", function(result, copy_error)
    assert(not result)
    copy_failure = copy_error
  end)
  assert(vim.wait(1000, function() return copy_failure ~= nil end, 5))
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
    assert(vim.deep_equal(command, builder.build_command()))
    assert(vim.tbl_contains(command, "--release") and vim.tbl_contains(command, "--locked"))
    assert(options.text and options.stdout and options.stderr)
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
  assert(vim.wait(1000, function() return #notification >= 3 end, 5))
  assert(notification[#notification].message:find("Compiling forge v0.1.0", 1, true))
  assert(notification[1].options.id == notification[#notification].options.id)
  vim.fn.writefile({ "automatically built executable" }, builder.binary_path())
  complete_build({ code = 0, signal = 0, stdout = "", stderr = "" })
  assert(vim.wait(1000, function() return available ~= nil and concurrent ~= nil end, 5))
  assert(available.ok and concurrent.ok and available.path == builder.binary_path())
  assert(notification[#notification].message:find("Build completed", 1, true))
  assert(notification[#notification].options.timeout == 3000)
  builder.ensure(function(result) assert(result.ok) end)
  assert(spawned == 1, "existing executable triggered another build")

  vim.fn.delete(builder.binary_path())
  available = nil
  builder.ensure(function(result) available = result end)
  stderr(nil, "fixture compiler error\n")
  complete_build({ code = 101, signal = 0, stdout = "", stderr = nil })
  assert(vim.wait(1000, function() return available ~= nil end, 5))
  assert(not available.ok and available.message:find("fixture compiler error", 1, true))
  assert(notification[#notification].level == vim.log.levels.ERROR)
  assert(notification[#notification].message:find("Build failed", 1, true))
  local finished_count = #notification
  stderr(nil, "late output\n")
  vim.wait(1100, function() return false end, 10)
  assert(#notification == finished_count, "completed build retained progress updates")

  available = nil
  builder.ensure(function(result) available = result end)
  assert(spawned == 3, "failed build could not be retried")
  complete_build({ code = 0, signal = 0, stdout = "", stderr = "" })
  assert(vim.wait(1000, function() return available ~= nil end, 5))
  assert(not available.ok and available.message:find("produced no executable", 1, true))
  assert(available.message:find(builder.binary_path(), 1, true))

  vim.system = function() error("fixture spawn failure") end
  available = nil
  builder.ensure(function(result) available = result end)
  assert(vim.wait(1000, function() return available ~= nil end, 5))
  assert(not available.ok and available.message:find("fixture spawn failure", 1, true))
  vim.fn.executable = function() return 0 end
  builder.ensure(function(result) available = result end)
  assert(not available.ok and available.message:find("cargo is not executable", 1, true))
end, debug.traceback)
vim.system, vim.fn.sha256 = original_system, original_hash
vim.fn.executable, vim.notify = original_executable, original_notify
if lease then pcall(lease.release) end
vim.fn.delete(root, "rf")
if not succeeded then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("automatic sidecar build and executable lease lifecycle passed")
vim.cmd("qa!")
