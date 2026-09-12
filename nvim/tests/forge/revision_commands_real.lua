vim.loader.enable(false)
local client = require("forge.client")
local builder = require("forge.builder")
local original_ensure = builder.ensure
local original_notify = vim.notify
local original_directory = vim.fn.getcwd()
local root = vim.fn.tempname()
vim.fn.mkdir(root, "p")
local owned_root = vim.uv.fs_realpath(root)
local binary = vim.fs.joinpath(original_directory, "nvim", "rust", "forge", "target", "debug", "forge.exe")
if vim.fn.has("win32") == 0 then binary = binary:sub(1, -5) end
local notifications = {}

---@param arguments string[]
local function git(arguments)
  local command = { "git", "-C", root }
  vim.list_extend(command, arguments)
  local result = vim.system(command, { text = true, stdout = true, stderr = true }):wait(5000)
  assert(result.code == 0, result.stderr)
end

local ok, failure = xpcall(function()
  assert(vim.fn.executable(binary) == 1, "build the Forge debug executable before native host tests")
  git({ "init", "--quiet", "--initial-branch=main" })
  vim.fn.writefile({ "content" }, vim.fs.joinpath(root, "file"))
  git({ "add", "file" })
  git({ "-c", "user.name=Forge Test", "-c", "user.email=forge@example.test", "commit", "--quiet", "-m", "base" })
  git({ "branch", "feature-é" })
  local plugin = dofile("nvim/lua/plugins/forge.lua")[1]
  vim.cmd.cd(vim.fn.fnameescape(root))
  plugin.config(nil, { harness = { backend = "mock" }, harness_logging = false, diff_logging = false })
  builder._set_artifact_root_for_test(vim.fs.joinpath(root, "forge-sidecar"))
  builder.ensure = function(done) done({ ok = true, path = binary }) end
  vim.notify = function(message, level, options)
    notifications[#notifications + 1] = { message = tostring(message), level = level, options = options }
  end
  package.loaded["forge.harness.backends.mock"] = { descriptor = function() error("completion initialized a provider") end }
  assert(#vim.fn.getcompletion("ForgeBranchDiff feature-", "cmdline") == 0)
  assert(vim.wait(10000, function()
    local values = vim.fn.getcompletion("ForgeBranchDiff feature-", "cmdline")
    return #values == 1 and values[1] == "feature-é"
  end, 5), "native host did not publish branch candidates: " .. vim.inspect({
    notifications = notifications,
    client = client._client and {
      ready = client._client.ready,
      starting = client._client.starting,
      draining = client._client.draining,
      stderr = client._client.stderr,
      pending = vim.tbl_keys(client._client.pending),
    },
    values = vim.fn.getcompletion("ForgeBranchDiff feature-", "cmdline"),
  }))
  assert(client._client.ready and not client._client.harness_ready)
  local process = client._client.process
  client.stop()
  local exit = process:wait(2000)
  assert(exit.code == 0, "shared host did not shut down cleanly")
end, debug.traceback)

builder.ensure = original_ensure
builder._reset_for_test()
vim.notify = original_notify
client._reset_for_test()
vim.cmd.cd(vim.fn.fnameescape(original_directory))
if owned_root and vim.uv.fs_realpath(root) == owned_root then vim.fn.delete(root, "rf") end
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") else vim.cmd("qa!") end
