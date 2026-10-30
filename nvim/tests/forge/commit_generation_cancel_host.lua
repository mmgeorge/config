vim.loader.enable(false)
local client = require("forge.client")
local config = require("forge.infra.config")
local fixture, data = vim.fn.tempname(), vim.fn.tempname()
assert(vim.fn.mkdir(fixture, "p") == 1 and vim.fn.mkdir(data, "p") == 1)
local builder = require("forge.builder")
local binary = data .. "/forge-cancellation-host" .. (vim.fn.has("win32") == 1 and ".exe" or "")
assert(vim.uv.fs_copyfile(builder.binary_path(), binary))
builder.ensure = function(callback) callback({ ok = true, path = binary }) return function() end end
client._host_set_launcher_for_test(vim.system)
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return kind == "data" and data or original_stdpath(kind) end
local function await(predicate, message)
  assert(vim.wait(10000, predicate, 10), message)
end
local function git(arguments)
  local result = vim.system(vim.list_extend({ "git", "-C", fixture }, arguments), { text = true }):wait()
  assert(result.code == 0, result.stderr)
end
local success, failure = xpcall(function()
  git({ "init", "--quiet" })
  git({ "config", "user.name", "Forge Fixture" })
  git({ "config", "user.email", "forge@example.invalid" })
  vim.fn.writefile({ "before" }, fixture .. "/tracked.txt")
  git({ "add", "." })
  git({ "commit", "--quiet", "-m", "initial" })
  vim.fn.writefile({ "after" }, fixture .. "/tracked.txt")
  git({ "add", "." })
  config.setup({ harness = { backend = "mock", backends = { mock = { command = { "writing-blocking" } } } } })
  local started, completed, diagnostic = false, false, nil
  local cancel = client.generation_request({ operation = "generate", workspace = fixture, comparison = "staged" },
    function(_, error) completed, diagnostic = true, error end,
    function(label) if label:find("mock-model", 1, true) then started = true end end,
    { model = "mock-model", auto_compaction = { enabled = false, threshold_tokens = 128000 } })
  await(function() return started end, "generation did not start")
  local marker = fixture .. "/mock-provider-change.txt"
  await(function() return vim.fn.filereadable(marker) == 1 or completed end, "provider did not reach its blocking response")
  assert(not completed and vim.fn.filereadable(marker) == 1, diagnostic)
  cancel()
  cancel()
  await(function() return completed end, "cancelled provider request did not settle")
  assert(diagnostic and diagnostic:find("cancelled", 1, true), tostring(diagnostic))
  config.setup({ harness = { backend = "mock", backends = { mock = { command = {} } } } })
  local replacement, replacement_error
  client.generation_request({ operation = "generate", workspace = fixture, comparison = "staged" },
    function(result, error) replacement, replacement_error = result, error end, nil,
    { model = "mock-model", auto_compaction = { enabled = false, threshold_tokens = 128000 } })
  await(function() return replacement or replacement_error end, "replacement generation did not settle")
  assert(not replacement_error, replacement_error)
  assert(replacement.state == "ready" and replacement.session_id, vim.inspect(replacement))
  assert(replacement.message:find("Mock response: Current Background task: commit_message", 1, true), vim.inspect(replacement))
  assert(replacement.message:find("- before\n+ after", 1, true), "replacement did not use the current staged diff")
end, debug.traceback)
client.host_stop()
await(function() return client._client.process == nil end, "generation test host was not collected")
vim.fn.stdpath = original_stdpath
assert(success, failure)
print("commit_generation_cancel_host: cancellation and replacement passed")
