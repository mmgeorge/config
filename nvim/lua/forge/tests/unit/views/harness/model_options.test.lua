-- Unit tests for `forge.views.harness.model_options` (model_options.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["label_names_exact_matches_by_provider_label"] = function()
  local model_options = require("forge.views.harness.model_options")
  local models = {
    { id = "opus", label = "Opus 5.5" },
    { id = "sonnet", label = "Sonnet 5.5", is_default = true },
    { id = "custom-model" },
  }

  MiniTest.expect.equality(model_options.label(models, "opus"), "Opus 5.5")
  MiniTest.expect.equality(model_options.label(models, "custom-model"), "custom-model")
  MiniTest.expect.equality(model_options.label(models, "unlisted"), "unlisted")
  MiniTest.expect.equality(model_options.label(nil, "opus"), "opus")
end

T["label_keeps_default_selection_name"] = function()
  local model_options = require("forge.views.harness.model_options")
  local models = { { id = "sonnet", label = "Sonnet 5.5", is_default = true } }

  MiniTest.expect.equality(model_options.label(models, "default"), "default")
  MiniTest.expect.equality(
    model_options.label({ { id = "default", label = "Default (recommended)" } }, "default"),
    "Default (recommended)"
  )
end

return T
