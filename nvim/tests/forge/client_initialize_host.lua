vim.loader.enable(false)

local root = vim.fs.normalize(vim.fn.getcwd())
local workspace = vim.fn.tempname()
assert(vim.fn.mkdir(workspace, "p") == 1)
local executable = vim.g.forge_test_binary or require("forge.builder").binary_path()
assert(vim.fn.executable(executable) == 1, "build the Forge host before running this fixture")

local function git(arguments)
  local command = { "git", "-C", workspace }
  vim.list_extend(command, arguments)
  local result = vim.system(command, { text = true, timeout = 10000 }):wait()
  assert(result.code == 0, result.stderr)
end

git({ "init", "--quiet" })
local original_cwd = vim.fn.getcwd()
vim.fn.chdir(workspace)
package.loaded["forge.builder"] = {
  ensure = function(callback)
    callback({ ok = true, path = executable })
    return function() end
  end,
}
package.loaded["forge.client"] = nil
local client = require("forge.client")
local launch_trace = {}

client._set_launcher_for_test(function(command, options, on_exit)
  local stdout = options.stdout
  options.stdout = function(error, chunk)
    launch_trace[#launch_trace + 1] = { stream = "stdout", error = error, bytes = chunk and #chunk or 0 }
    stdout(error, chunk)
  end
  local stderr = options.stderr
  options.stderr = function(error, chunk)
    launch_trace[#launch_trace + 1] = { stream = "stderr", error = error, bytes = chunk and #chunk or 0 }
    stderr(error, chunk)
  end
  return vim.system(command, options, function(result)
    launch_trace[#launch_trace + 1] = { stream = "exit", code = result.code, signal = result.signal }
    on_exit(result)
  end)
end)

local ok, failure = xpcall(function()
  local started, startup_failure = false, nil
  client.start(function(_, error)
    startup_failure = error
    started = true
  end)
  assert(vim.wait(10000, function() return started and vim.tbl_isempty(client._client.pending) end, 200),
    "client initialization timed out: " .. vim.inspect({ state = client._client, launch_trace = launch_trace }))
  assert(not startup_failure, "client initialization failed: " .. tostring(startup_failure))
  local result, request_failure
  client.request_host("status", { operation = "open", document = "client-lifecycle", workspace = workspace }, function(response, error)
    result, request_failure = response, error
  end)
  assert(vim.wait(10000, function() return result ~= nil or request_failure ~= nil end, 200), "client application request timed out")
  assert(result and not request_failure, "client application request failed: " .. tostring(request_failure))
  local stopped_process = client._client.process
  assert(stopped_process and stopped_process.wait, "client did not retain a process")
  client.stop()
  assert(vim.wait(6000, function() return client._client.process == nil end, 200),
    "client shutdown timed out: " .. vim.inspect({ state = client._client, launch_trace = launch_trace }))
  assert(stopped_process:wait(0), "client process was not collected")
  assert(client._client.receiver == nil, "client receiver survived process collection")
end, debug.traceback)

client._reset_for_test()
vim.fn.chdir(original_cwd)
vim.fn.delete(workspace, "rf")
if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
end
print("client_initialize_host: client initialize, Status request, and collection passed")
vim.cmd("qa!")
