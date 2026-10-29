vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
vim.loader.enable(false)
local root = vim.fn.tempname()
local workspace = root .. "/repository"
vim.fn.mkdir(workspace, "p")
local function git(arguments)
  local command = { "git", "-C", workspace }
  vim.list_extend(command, arguments)
  local result = vim.system(command, { text = true, timeout = 10000 }):wait()
  assert(result.code == 0, result.stderr)
end
git({ "init", "-q", "-b", "main" })
git({ "config", "user.name", "Fixture" })
git({ "config", "user.email", "fixture@example.test" })
git({ "commit", "-q", "--allow-empty", "-m", "feat: initial" })
vim.fn.writefile({ "local value = true" }, workspace .. "/example.lua")
local executable = require("forge.builder").binary_path()
local stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return (kind == "data" or kind == "config") and root or stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback) callback({ ok = true, path = executable }) return function() end end }
require("forge").setup({ harness = { backend = "mock" }, background_generation = { mock = { model = "mock-model", reasoning_effort = "low" } } })
vim.fn.chdir(workspace)
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local errors = {}
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
local function call(method, params)
  local result, failure, done
  client.request({ method = method, params = params }, function(value, error) result, failure, done = value, error, true end)
  assert(vim.wait(20000, function() return done end, 10), method .. " timed out")
  assert(not failure, tostring(failure) .. "\n" .. tostring(client._client.stderr) .. "\n" .. vim.inspect(errors))
  return result
end
local generation = assert(client.generation_get_config())
local preview = call("repository.generation", { operation = "preview", workspace = workspace, comparison = "head" })
assert(preview.prompt.user:find("Current Background task: commit_message", 1, true))
assert(preview.prompt.user:find("Conventional Commit", 1, true))
assert(not preview.prompt.system:find("Conventional Commit", 1, true))
assert(preview.prompt.input_bytes == #preview.prompt.user + #preview.prompt.system)
local params = { operation = "generate", workspace = workspace, comparison = "head", generation = generation }
local first = call("repository.generation", params)
assert(first.session_id and first.message and first.message ~= "")
local second = call("repository.generation", params)
assert(first.session_id == second.session_id, "HEAD-stable generation lost persistence")
assert(state.session == nil, "Background generation initialized interactive Harness")
local query = { workspace = workspace, data_root = generation.data_root }
local snapshot = call("background.session.document", query)
assert(snapshot.session.id == first.session_id)
query.auto_compaction = { enabled = false, threshold_tokens = 64000 }
query.expected_revision = snapshot.settings.revision
snapshot = call("background.session.document", query)
assert(snapshot.settings.desired.auto_compaction.threshold_tokens == 64000)
query.auto_compaction, query.expected_revision = nil, nil
require("forge").open_background_timeline()
assert(vim.wait(10000, function()
  return vim.api.nvim_buf_get_name(0):match("ForgeBackground")
    and table.concat(vim.api.nvim_buf_get_lines(0,0,-1,false), "\n"):find("Mock response",1,true)
end, 10), "Background timeline did not render")
assert(vim.bo.modifiable == false, "Background timeline is editable")
assert(state.session == nil, "Debug timeline selected interactive Harness")
git({ "switch", "-q", "-c", "feature" })
local branch = call("repository.generation", params)
assert(branch.session_id ~= first.session_id, "branch switch reused obsolete context")
assert(#errors == 0, table.concat(errors, "\n"))
client.host_stop()
assert(vim.wait(10000, function() return not client._client.process end, 10), "Host did not stop")
print("background host: persistence, branch rotation, overrides, independent timeline and read-only rendering passed")
vim.cmd("qa!")
