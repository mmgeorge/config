vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local root = vim.fn.tempname()
vim.fn.mkdir(root, "p")
local builder = require("rust_sidecar").new({ crate_name = "fixture", profile = "dev",
  crate_dir = function() error("startup inspected compiler inputs") end,
  artifact_root = function() return root end,
})
local original_system, original_hash, original_scan = vim.system, vim.fn.sha256, vim.uv.fs_scandir
---@type RustSidecarLease?
local lease
local succeeded, failure = xpcall(function()
  vim.system = function() error("startup invoked a compiler") end
  vim.fn.sha256 = function() error("startup hashed contents") end
  vim.uv.fs_scandir = function() error("startup scanned sources or deployments") end
  vim.fn.mkdir(vim.fs.dirname(builder.binary_path()), "p")
  vim.fn.writefile({ "manually built executable" }, builder.binary_path())
  ---@type RustSidecarExecutableResult?
  local available
  builder.ensure(function(result) available = result end)
  assert(available and available.ok and available.path == builder.binary_path())
  builder.acquire(available.path, function(result, copy_error)
    assert(not copy_error, copy_error)
    lease = result
  end)
  assert(vim.wait(1000, function() return lease ~= nil end, 5), "copy did not finish")
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
  builder.ensure(function(result) available = result end)
  assert(not available.ok and available.message:find("Build it manually", 1, true))
  assert(available.message:find(builder.binary_path(), 1, true))
end, debug.traceback)
vim.system, vim.fn.sha256, vim.uv.fs_scandir = original_system, original_hash, original_scan
if lease then pcall(lease.release) end
vim.fn.delete(root, "rf")
if not succeeded then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("manual sidecar startup passed")
vim.cmd("qa!")
