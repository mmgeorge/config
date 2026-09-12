vim.loader.enable(false)

local root = vim.fs.normalize(vim.fn.getcwd())
local executable = require("forge.builder").binary_path()
assert(vim.fn.executable(executable) == 1, "build the Forge host before running this fixture")

local protocol = require("forge.protocol")
local data_root = vim.fn.tempname()
assert(vim.fn.mkdir(data_root, "p") == 1)
local workspace = vim.fn.tempname()
assert(vim.fn.mkdir(workspace, "p") == 1)
local initialized = vim.system({ "git", "-C", workspace, "init", "--quiet" }, { text = true }):wait()
assert(initialized.code == 0, initialized.stderr)
local original_cwd = vim.fn.getcwd()
vim.fn.chdir(workspace)
local failure

local function initialize_host(attempt)
  local partial, stderr, initialized, exited = "", "", nil, nil
  local received_bytes, received_frames = 0, 0
  local process = vim.system({ executable }, {
    text = true,
    stdin = true,
    stdout = function(_, chunk)
      if not chunk then return end
      partial = partial .. chunk
      while true do
        local newline = partial:find("\n", 1, true)
        if not newline then break end
        local line = partial:sub(1, newline - 1)
        partial = partial:sub(newline + 1)
        received_bytes, received_frames = received_bytes + #line + 1, received_frames + 1
        local message = vim.json.decode(line)
        if message.id == 1 then initialized = message end
      end
    end,
    stderr = function(_, chunk)
      if chunk then stderr = stderr .. chunk end
    end,
  }, function(result) exited = result end)
  process:write(protocol.encode_request(1, "initialize", {
    protocol_version = protocol.VERSION,
    recovery_directory = vim.fs.joinpath(data_root, "forge", "recovery", "github", "v1"),
    status_ignored_directory = vim.fs.joinpath(data_root, "forge", "status-ignored"),
  }))
  assert(vim.wait(2000, function() return initialized ~= nil or exited ~= nil end, 200),
    "host initialization timed out on attempt " .. attempt .. ": " .. vim.inspect({ stderr = stderr }))
  assert(initialized and not initialized.error and initialized.result.protocol_version == protocol.VERSION,
    "host initialization failed on attempt " .. attempt .. ": " .. vim.inspect({ initialized = initialized, exited = exited, stderr = stderr }))
  process:write(protocol.encode_request(0, "transport.consumed", { bytes = received_bytes, frames = received_frames }))
  process:write(protocol.encode_request(2, "shutdown", {}))
  assert(vim.wait(3000, function() return exited ~= nil end, 200),
    "host shutdown timed out on attempt " .. attempt .. ": " .. vim.inspect({ stderr = stderr }))
  assert(process:wait(0).code == 0, "host process was not collected on attempt " .. attempt)
  assert(exited.code == 0, "host shutdown failed on attempt " .. attempt .. ": " .. vim.inspect(exited))
  process = nil
end

local ok, err = xpcall(function()
  initialize_host(1)
end, debug.traceback)
if not ok then
  vim.fn.chdir(original_cwd)
  vim.fn.delete(data_root, "rf")
  vim.fn.delete(workspace, "rf")
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit 1")
end
vim.fn.chdir(original_cwd)
vim.fn.delete(data_root, "rf")
vim.fn.delete(workspace, "rf")
print("host_initialize: direct stdio initialize and collected shutdown passed")
vim.cmd("qa!")
