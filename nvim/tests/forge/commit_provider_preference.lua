vim.loader.enable(false)

local client = require("forge.client")
local builder = require("forge.builder")
local config = require("forge.infra.config")
local preference = require("forge.harness.provider_preference")
local session = require("forge.session")
local original_ensure, original_load = builder.ensure, preference.load
local original_options, original_explicit = config.options, config.harness_provider_explicit
local original_state = session.harness
local generation, generated_session
local preference_reads = 0
local registry_fixture = dofile("nvim/tests/forge/support/provider_catalog.lua")

builder.ensure = function(callback) callback({ ok = true, path = "generation-preference-fixture" }) end
preference.load = function(provider_map, fallback)
  assert(provider_map.fixture_beta and fallback == "fixture_alpha")
  preference_reads = preference_reads + 1
  return "fixture_beta"
end

---@param command string[]
---@param options table
---@return table
local function launch_fixture(command, options)
  return {
    write = function(_, encoded)
      local request = vim.json.decode(encoded)
      if request.method == "host.transport.consumed" or request.method == "host.shutdown" then return end
      local result = {}
      if request.method == "host.initialize" then
        result.protocol_version = require("forge.protocol").VERSION
      elseif request.method == "provider.registry.list" then
        result = registry_fixture.registry()
      elseif request.method == "session.initialize" then
        error("Standalone generation initialized a conversation")
      elseif request.method == "repository.generation" then
        generation = request.params.generation
        generated_session = request.session_id
        result = { state = "ready", message = "fix: use selected provider" }
      elseif request.method == "host.trace.configure" then
        result.enabled = false
      end
      options.stdout(nil, vim.json.encode({ id = request.id, result = result }) .. "\n")
    end,
    kill = function() end,
  }
end

local succeeded, failure = xpcall(function()
  for _, explicit in ipairs({ false, true }) do
    client._host_reset_for_test()
    client._host_set_launcher_for_test(launch_fixture)
    generation, generated_session = nil, nil
    config.setup(explicit and { harness_logging = false, harness = { provider = "fixture_alpha" } }
      or { harness_logging = false })
    session.activate_harness(session.new_harness_state())
    local completed = false
    client.generation_request({ operation = "generate", workspace = "fixture", comparison = "staged" },
      function(result, request_error)
        assert(not request_error, request_error)
        assert(result.message == "fix: use selected provider")
        completed = true
      end)
    assert(vim.wait(1000, function() return completed end, 5), "commit generation startup timed out")
    assert(generation.provider.kind == (explicit and "fixture_alpha" or "fixture_beta"),
      "commit startup ignored the saved preference or explicit configuration")
    assert(vim.deep_equal(generation.provider.command,
      config.options.harness.providers[generation.provider.kind].command or {}),
      "generation lost the configured provider command")
    assert(preference_reads == 1, "explicit provider configuration read the saved preference")
    assert(generation.model == "fixture-model" and generation.reasoning_effort == "low",
      "commit generation did not use registry model defaults")

    assert(generated_session == nil, "cold generation attached a conversation")
    assert(not client._client.harness_ready and session.harness.session == nil,
      "cold generation created Harness session state")
    session.harness.session = { id = "selected-session", provider = generation.provider.kind, model = "selected-model" }
    completed = false
    client.generation_request({ operation = "generate", workspace = "fixture", comparison = "staged" },
      function(_, request_error)
        assert(not request_error, request_error)
        completed = true
      end)
    assert(vim.wait(1000, function() return completed end, 5), "selected session generation timed out")
    assert(generated_session == nil, "generation attached the active conversation")
    assert(generation.model == "fixture-model" and generation.reasoning_effort == "low",
      "commit generation inherited the active conversation model")
    assert(preference_reads == 1, "ready Harness reloaded the provider preference")

    config.options.background_generation[generation.provider.kind] = { model = "custom-commit-model", reasoning_effort = "medium" }
    local label
    completed = false
    client.generation_request({ operation = "generate", workspace = "fixture", comparison = "staged" },
      function(_, request_error)
        assert(not request_error, request_error)
        completed = true
      end, function(model) label = model end)
    assert(vim.wait(1000, function() return completed end, 5), "configured model generation timed out")
    assert(generation.model == "custom-commit-model" and generation.reasoning_effort == "medium",
      "generation ignored its model and effort configuration")
    assert(label:find("custom-commit-model · thinking medium", 1, true), "notification reported a different model")
    assert(session.harness.session.model == "selected-model", "generation changed the active conversation model")

    client._host_reset_for_test()
    client._host_set_launcher_for_test(launch_fixture)
    config.options.harness.provider = "fixture_alpha"
    session.harness.session.provider = "fixture_alpha"
    completed = false
    client.generation_request({ operation = "generate", workspace = "fixture", comparison = "staged" },
      function(_, request_error)
        assert(not request_error, request_error)
        completed = true
      end)
    assert(vim.wait(1000, function() return completed end, 5), "selected provider restart timed out")
    assert(generation.provider.kind == "fixture_alpha" and preference_reads == 1,
      "host restart replaced the selected provider with its older saved preference")
  end
  session.harness.session = { id = "selected-session", provider = "fixture_alpha", model = "selected-model" }
  config.options.background_generation.fixture_alpha = { model = "" }
  local invalid_model
  client.generation_request({ operation = "generate", workspace = "fixture", comparison = "staged" },
    function(_, failure) invalid_model = failure end)
  assert(vim.wait(1000, function() return invalid_model ~= nil end, 5), "configuration failure was not delivered")
  assert(invalid_model == "Invalid Background-generation model for provider: fixture_alpha",
    "invalid model silently inherited the active conversation model")
  session.harness.session = { id = "foreign-session", provider = "missing-provider" }
  local request_error
  client.generation_request({ operation = "generate", workspace = "fixture", comparison = "staged" },
    function(_, failure) request_error = failure end)
  assert(vim.wait(1000, function() return request_error ~= nil end, 5), "unknown provider failure was not delivered")
  assert(request_error == "Unknown Harness provider: missing-provider",
    "invalid provider configuration did not reach the caller")
end, debug.traceback)

client._host_reset_for_test()
builder.ensure, preference.load = original_ensure, original_load
config.options, config.harness_provider_explicit = original_options, original_explicit
session.activate_harness(original_state)
if not succeeded then io.stderr:write(tostring(failure), "\n") vim.cmd("cquit 1") end
print("commit provider preference passed")
vim.cmd("qa!")
