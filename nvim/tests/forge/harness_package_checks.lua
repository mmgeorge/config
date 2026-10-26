vim.loader.enable(false)
local root = vim.fs.normalize(vim.fn.getcwd())
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
for _, name in ipairs({ "first", "second" }) do
  vim.fn.mkdir(workspace .. "/packages/" .. name .. "/src", "p")
  vim.fn.writefile({ vim.json.encode({ name = name }) }, workspace .. "/packages/" .. name .. "/package.json")
end
vim.fn.writefile({ vim.json.encode({ name = "root", private = true, workspaces = { "packages/*" } }) }, workspace .. "/package.json")
vim.fn.writefile({ vim.json.encode({ checks = {
  ["package-tests"] = { trigger = { auto = true, paths = { "packages/**" } }, action = { command = { "nu",  "--no-config-file", "-c", "print '{package}'"  }, label = "Package tests", directory = ".", category = "test", package_manager = "npm", after = {} } }
} }) }, workspace .. "/.forge.json")
local binary = data .. "/forge-host" .. (vim.fn.has("win32") == 1 and ".exe" or "")
assert(vim.uv.fs_copyfile(require("forge.builder").binary_path(), binary))
local original_stdpath, original_notify = vim.fn.stdpath, vim.notify
vim.fn.stdpath = function(kind) return kind == "data" and data or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = binary }) end)
  return function() end
end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local errors = {}
vim.notify = function(message, level)
  if level == vim.log.levels.ERROR then errors[#errors + 1] = tostring(message) end
end

---@param method string
---@param params? table
---@return table
local function request(method, params)
  local settled, value, failure
  client.request(method, params or {}, function(result, error) value, failure, settled = result, error, true end)
  assert(vim.wait(10000, function() return settled end, 10), method .. " timeout")
  assert(not failure, method .. ": " .. tostring(failure))
  return value
end

local succeeded, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = false,
    harness = { backend = "mock", backends = { mock = { command = { "retained-control" }, selectable = true } } } })
  require("forge.views.harness").open()
  assert(vim.wait(10000, function() return state.session ~= nil end, 10), table.concat(errors, "\n"))
  assert(request("checks.list").configured)
  local id = request("checks.run", { check = "package-tests", files = {
      "packages/first/src/one.ts", "packages/first/src/two.ts", "packages/second/src/one.ts",
  } }).run_id
  local run
  assert(vim.wait(10000, function()
    run = request("checks.status", { run_id = id })
    return run.state ~= "running"
  end, 20), "package checks did not settle")
  assert(run.state == "passed", vim.inspect(run))
  assert(#run.commands == 2, "expected one invocation per package: " .. vim.inspect(run))
  assert(#run.commands[1].context.matched_files == 2)
  assert(run.commands[1].context.invocation.package.name == "first")
  assert(run.commands[2].context.invocation.package.name == "second")
  assert(table.concat(vim.fn.readfile(run.commands[1].stdout), "\n") == "first")
  assert(#errors == 0, table.concat(errors, "\n"))
end, debug.traceback)
require("forge.views.harness.workspace").release(state)
if state.presentation then state.presentation.close() end
client.stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(root)
vim.fn.stdpath, vim.notify = original_stdpath, original_notify
assert(succeeded and collected, failure or "check host was not collected")
print("harness_package_checks: passed")
