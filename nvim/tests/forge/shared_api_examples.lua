vim.loader.enable(false)
local original_directory = vim.fn.getcwd()
local original_stdpath = vim.fn.stdpath
local directory = vim.fn.tempname()
local workspace, data = directory .. "/workspace", directory .. "/data"
assert(vim.fn.mkdir(workspace, "p") == 1 and vim.fn.mkdir(data, "p") == 1)
local initialized = vim.system({ "git", "-C", workspace, "init", "--quiet" }, { text = true, timeout = 10000 }):wait()
assert(initialized.code == 0, initialized.stderr)
local executable = require("forge.builder").binary_path()
package.loaded["forge.builder"] = { ensure = function(callback)
  callback({ ok = true, path = executable })
  return function() end
end }
vim.fn.stdpath = function(kind)
  return (kind == "data" or kind == "config") and data or original_stdpath(kind)
end
vim.fn.chdir(workspace)
require("forge").setup({ harness_logging = false, harness = { backend = "mock", config_dir = data } })
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local capture = dofile(original_directory .. "/nvim/tests/forge/support/wire_capture.lua").start()
local examples = {}
local function request(method, params)
  local result, failure, completed
  local specification = { method = method, params = params or vim.empty_dict() }
  client.request(specification, function(value, problem)
    result, failure, completed = value, problem, true
  end)
  assert(vim.wait(10000, function() return completed end, 5), method .. " timed out")
  assert(not failure, method .. ": " .. tostring(failure))
  examples[#examples + 1] = capture.completed(method)
  return result
end
local success, failure = xpcall(function()
  local initialized, problem
  client.session_initialize(function(_, error) initialized, problem = true, error end, { services_only = true })
  assert(vim.wait(10000, function() return initialized end, 5), "shared runtime did not initialize")
  assert(not problem, problem)
  local about = request("repository.status.context", { operation = "about_message", document = "example-about", text = "Local fixture description" })
  assert(type(about) == "table")
  assert(type(request("repository.write", { operation = "trace" }).host_id) == "string")
  local profile = request("agent.profile.get")
  request("agent.profile.save", { expected_revision = profile.revision, document = profile.document })
  local permission = request("permissions.document.open")
  assert(request("permissions.document.save", { source = permission.source }).source == permission.source)
  assert(#request("provider.model.pin.list", { backend = "codex" }) == 0)
  local pinned = request("provider.model.pin.set", { backend = "codex", model = "example-model", pinned = true })
  assert(pinned[1] == "example-model")
  local settings = request("settings.get", { workspace = workspace })
  local changed = request("settings.update", { workspace = workspace, scope = "workspace",
    expected_revision = settings.revision, desired = { plan_compact = true }, operation_id = "example-settings" })
  assert(changed.state == "saved")
  local native = directory .. "/mcp.json"
  vim.fn.writefile({ vim.json.encode({ mcpServers = { example = { command = "node", args = { "example.js" } } } }) }, native)
  local preview = request("settings.import.preview", { workspace = workspace, source = native, kind = "mcp", scope = "workspace" })
  local candidate = assert(preview.candidate[1])
  local imported = request("settings.import.apply", { workspace = workspace, source = native, kind = "mcp", scope = "workspace",
    fingerprint = preview.fingerprint, selected = { candidate.id }, expected_revision = preview.revision, operation_id = "example-import" })
  assert(imported.state == "saved")
  request("settings.mcp.set_enabled", { workspace = workspace, name = candidate.name, resource_id = candidate.id,
    scope = "workspace", enabled = true, expected_revision = imported.revision, operation_id = "example-mcp" })
  local skill = directory .. "/native-skill"
  vim.fn.mkdir(skill, "p")
  vim.fn.writefile({ "---", "name: example-skill", "description: Example API fixture", "---", "Use only within this fixture." }, skill .. "/SKILL.md")
  local skill_preview = request("settings.import.preview", { workspace = workspace, source = skill, kind = "skills", scope = "workspace" })
  local skill_candidate = assert(skill_preview.candidate[1])
  local skill_import = request("settings.import.apply", { workspace = workspace, source = skill, kind = "skills", scope = "workspace",
    fingerprint = skill_preview.fingerprint, selected = { skill_candidate.id }, expected_revision = skill_preview.revision,
    operation_id = "example-skill-import" })
  request("settings.skill.set_enabled", { workspace = workspace, name = skill_candidate.name, resource_id = skill_candidate.id,
    scope = "workspace", enabled = true, expected_revision = skill_import.revision, operation_id = "example-skill" })
  assert(not client._client.harness_ready, "shared calls initialized an interactive conversation")
  if arg[1] then
    vim.fn.writefile({ vim.json.encode({ root = directory, examples = examples }) }, arg[1])
  end
end, debug.traceback)
client.host_stop()
local stopped = vim.wait(6000, function() return client._client.process == nil end, 10)
capture.close()
vim.fn.chdir(original_directory)
vim.fn.stdpath = original_stdpath
assert(success and stopped, failure or "shared API fixture host did not stop")
print("shared_api_examples: all shared methods passed without interactive session creation")
