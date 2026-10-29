vim.loader.enable(false)
local executable = require("forge.builder").binary_path()
assert(vim.fn.executable(executable) == 1, "build the selected host before running this fixture")
local workspace = vim.fn.tempname()
assert(vim.fn.mkdir(workspace, "p") == 1)
local initialized = vim.system({ "git", "-C", workspace, "init", "--quiet" },
  { text = true, timeout = 10000 }):wait()
assert(initialized.code == 0, initialized.stderr)
local original_directory = vim.fn.getcwd()
vim.fn.chdir(workspace)
package.loaded["forge.client"] = nil
local client = require("forge.client")
client._host_set_launcher_for_test(function(command, options, on_exit)
  return vim.system(command, options, on_exit)
end)
local success, failure = xpcall(function()
  local ready, startup_failure
  client.host_start(function(_, message) ready, startup_failure = true, message end)
  assert(vim.wait(10000, function() return ready end, 50), "host startup timed out")
  assert(not startup_failure, startup_failure)
  for _, request in ipairs({
    { method = "github.issue.document", params = { operation = "edit", edit = {} }, expected = "unknown variant" },
    { method = "review.region_edit", params = {}, expected = "Unknown Forge method" },
  }) do
    local delivered, rejected
    client.request({ method = request.method, params = request.params }, function(result, message)
      assert(result == nil)
      delivered, rejected = true, message
    end)
    assert(vim.wait(10000, function() return delivered end, 50), "route response timed out")
    assert(type(rejected) == "string" and rejected:find(request.expected, 1, true), tostring(rejected))
  end
end, debug.traceback)
client.host_stop()
assert(vim.wait(6000, function() return client._client.process == nil end, 50), "host collection timed out")
client._host_reset_for_test()
vim.fn.chdir(original_directory)
vim.fn.delete(workspace, "rf")
assert(success, failure)
print("per_edit_routes_host: client and host reject retired per-edit transports")
