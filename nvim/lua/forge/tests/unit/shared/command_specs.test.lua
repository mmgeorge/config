-- Unit tests for `forge.shared.command_specs` (shared/command_specs.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["every_default_plan_review_key_has_a_command_spec"] = function()
  local command_specs = require("forge.shared.command_specs")
  local config = require("forge.infra.config")
  local spec_by_id = command_specs.view_spec_by_id.plan_review
  local missing = {}
  for command_id in pairs(config.defaults.keymaps.plan_review) do
    if not spec_by_id[command_id] then missing[#missing + 1] = command_id end
  end
  table.sort(missing)
  -- A key without a spec is neither mapped nor listed in help, so the command it names is unreachable.
  MiniTest.expect.equality(missing, {})
end

return T
