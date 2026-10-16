vim.loader.enable(false)

local client = require("forge.client")
local builder = require("forge.builder")
local config = require("forge.infra.config")
local preference = require("forge.harness.backend_preference")
local session = require("forge.session")
local original_ensure, original_load = builder.ensure, preference.load
local original_options, original_explicit = config.options, config.harness_backend_explicit
local original_state = session.harness
local generation, generated_session
local preference_reads = 0

builder.ensure = function(callback) callback({ ok = true, path = "generation-preference-fixture" }) end
preference.load = function(backend_map, fallback)
  assert(backend_map.copilot and fallback == "codex")
  preference_reads = preference_reads + 1
  return "copilot"
end

---@param command string[]
---@param options table
---@return table
local function launch_fixture(command, options)
  return {
    write = function(_, encoded)
      local request = vim.json.decode(encoded)
      if request.method == "transport.consumed" or request.method == "shutdown" then return end
      local result = {}
      if request.method == "initialize" then
        result.protocol_version = require("forge.protocol").VERSION
      elseif request.method == "harness.initialize" then
        error("Standalone generation initialized a conversation")
      elseif request.method == "repository.generate" then
        generation = request.params.generation
        generated_session = request.session_id
        result = { state = "ready", message = "fix: use selected provider" }
      elseif request.method == "trace.configure" then
        result.enabled = false
      end
      options.stdout(nil, vim.json.encode({ id = request.id, result = result }) .. "\n")
    end,
    kill = function() end,
  }
end

local succeeded, failure = xpcall(function()
  for _, explicit in ipairs({ false, true }) do
    client._reset_for_test()
    client._set_launcher_for_test(launch_fixture)
    generation, generated_session = nil, nil
    config.setup(explicit and { harness_logging = false, harness = { backend = "codex" } }
      or { harness_logging = false })
    session.activate_harness(session.new_harness_state())
    local completed = false
    client.request_generation({ operation = "generate", workspace = "fixture", comparison = "staged" },
      function(result, request_error)
        assert(not request_error, request_error)
        assert(result.message == "fix: use selected provider")
        completed = true
      end)
    assert(vim.wait(1000, function() return completed end, 5), "commit generation startup timed out")
    assert(generation.backend.kind == (explicit and "codex" or "copilot"),
      "commit startup ignored the saved preference or explicit configuration")
    assert(vim.deep_equal(generation.backend.command,
      config.options.harness.backends[generation.backend.kind].command or {}),
      "generation lost the configured provider command")
    assert(preference_reads == 1, "explicit backend configuration read the saved preference")

    assert(generated_session == nil, "cold generation attached a conversation")
    assert(not client._client.harness_ready and session.harness.session == nil,
      "cold generation created Harness session state")
    session.harness.session = { id = "selected-session", backend = generation.backend.kind, model = "selected-model" }
    completed = false
    client.request_generation({ operation = "generate", workspace = "fixture", comparison = "staged" },
      function(_, request_error)
        assert(not request_error, request_error)
        completed = true
      end)
    assert(vim.wait(1000, function() return completed end, 5), "selected session generation timed out")
    assert(generated_session == nil, "generation attached the active conversation")
    assert(generation.model == "selected-model", "generation lost the selected model")
    assert(preference_reads == 1, "ready Harness reloaded the provider preference")

    client._reset_for_test()
    client._set_launcher_for_test(launch_fixture)
    config.options.harness.backend = "codex"
    session.harness.session.backend = "codex"
    completed = false
    client.request_generation({ operation = "generate", workspace = "fixture", comparison = "staged" },
      function(_, request_error)
        assert(not request_error, request_error)
        completed = true
      end)
    assert(vim.wait(1000, function() return completed end, 5), "selected backend restart timed out")
    assert(generation.backend.kind == "codex" and preference_reads == 1,
      "host restart replaced the selected backend with its older saved preference")
  end
  session.harness.session = { id = "foreign-session", backend = "missing-provider" }
  local request_error
  client.request_generation({ operation = "generate", workspace = "fixture", comparison = "staged" },
    function(_, failure) request_error = failure end)
  assert(request_error == "Unknown Harness backend: missing-provider",
    "invalid provider configuration did not reach the caller")
end, debug.traceback)

client._reset_for_test()
builder.ensure, preference.load = original_ensure, original_load
config.options, config.harness_backend_explicit = original_options, original_explicit
session.activate_harness(original_state)
if not succeeded then io.stderr:write(tostring(failure), "\n") vim.cmd("cquit 1") end
print("commit backend preference passed")
vim.cmd("qa!")
