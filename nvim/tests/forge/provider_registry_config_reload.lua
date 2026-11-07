vim.loader.enable(false)

-- Replacing config.options through a repeated setup must not discard the provider registry the
-- running host already returned; provider lookups must keep resolving registry descriptors.
local client = require("forge.client")
local builder = require("forge.builder")
local config = require("forge.infra.config")
local preference = require("forge.harness.provider_preference")
local session = require("forge.session")
local original_ensure, original_load = builder.ensure, preference.load
local original_options, original_explicit = config.options, config.harness_provider_explicit
local original_state = session.harness
local registry_fixture = dofile("nvim/tests/forge/support/provider_catalog.lua")
local registry_request_count = 0

builder.ensure = function(callback) callback({ ok = true, path = "registry-reload-fixture" }) end
preference.load = function(_, fallback) return fallback end

---@param _ string[]
---@param options table
---@return table
local function launch_fixture(_, options)
  return {
    write = function(_, encoded)
      local request = vim.json.decode(encoded)
      if request.method == "host.transport.consumed" or request.method == "host.shutdown" then return end
      local result = {}
      if request.method == "host.initialize" then
        result.protocol_version = require("forge.protocol").VERSION
      elseif request.method == "provider.registry.list" then
        registry_request_count = registry_request_count + 1
        result = registry_fixture.registry()
      elseif request.method == "host.trace.configure" then
        result.enabled = false
      end
      options.stdout(nil, vim.json.encode({ id = request.id, result = result }) .. "\n")
    end,
    kill = function() end,
  }
end

---@return ForgeTextGenerationConfig?, string?
local function resolve_generation()
  local settled, generation, failure = false, nil, nil
  client.generation_config(function(value, request_failure)
    settled, generation, failure = true, value, request_failure
  end)
  assert(vim.wait(1000, function() return settled end, 5), "generation configuration never settled")
  return generation, failure
end

local succeeded, failure = xpcall(function()
  client._host_reset_for_test()
  client._host_set_launcher_for_test(launch_fixture)
  config.setup({ harness_logging = false })
  session.activate_harness(session.new_harness_state())
  local generation, first_failure = resolve_generation()
  assert(generation and not first_failure, first_failure)
  assert(registry_request_count == 1)

  config.setup({ harness_logging = false })
  session.harness.session = { id = "reload-session", provider = "fixture_beta", model = "chat-model" }
  generation, first_failure = resolve_generation()
  assert(not first_failure, "replaced configuration lost the host registry: " .. tostring(first_failure))
  assert(generation and generation.provider.kind == "fixture_beta")
  assert(vim.deep_equal(generation.provider.command, { "fixture-provider", "fixture_beta" }),
    "generation lost the registry provider command")
  assert(registry_request_count == 1, "re-adopting a retained registry must not repeat the host request")
  assert(client.provider_map().fixture_gamma, "synchronous provider lookup lost the registry")

  config.setup({ harness_logging = false, harness = { providers = { fixture_beta = { label = "Configured Beta" } } } })
  assert(client.provider_map().fixture_beta.label == "Configured Beta",
    "user provider overrides must win over registry descriptors")
  assert(client.provider_map().fixture_beta.command[2] == "fixture_beta",
    "user provider overrides must merge with registry descriptors")
end, debug.traceback)

client._host_reset_for_test()
builder.ensure, preference.load = original_ensure, original_load
config.options, config.harness_provider_explicit = original_options, original_explicit
session.activate_harness(original_state)
if not succeeded then io.stderr:write(tostring(failure), "\n") vim.cmd("cquit 1") end
print("provider registry survives configuration replacement passed")
vim.cmd("qa!")
