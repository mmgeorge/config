local M = {}

--- Supply a neutral registry for tests of shared provider selection and presentation.
function M.registry()
  local providers = {}
  for index, id in ipairs({ "fixture_alpha", "fixture_beta", "fixture_gamma", "fixture_delta", "mock" }) do
    providers[#providers + 1] = {
      id = id, label = "Fixture " .. index, detail = "Test provider", selectable = id ~= "mock",
      command = { "fixture-provider", id }, capability = { os_sandbox = index == 1, integration = { isolated_generation = true } },
      background_generation = { model = "fixture-model", reasoning_effort = "low" },
    }
  end
  return { default_provider = "fixture_alpha", providers = providers }
end

--- Install fixture-owned metadata without requiring native-provider knowledge in UI tests.
function M.install()
  local config = require("forge.infra.config")
  local registry = M.registry()
  local providers, background = {}, {}
  for _, entry in ipairs(registry.providers) do
    providers[entry.id] = vim.deepcopy(entry)
    background[entry.id] = vim.deepcopy(entry.background_generation)
  end
  config.options.harness.providers = providers
  config.options.background_generation = background
  if not config.options.harness.provider then config.options.harness.provider = registry.default_provider end
  return registry
end

return M
