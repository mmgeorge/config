local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["module_returns_a_provided_submodule"] = function()
  local blocks = { normalize = function(text) return text end }
  package.loaded["markdown_math.blocks"] = blocks
  MiniTest.finally(function() package.loaded["markdown_math.blocks"] = nil end)
  eq(require("forge.integrations.markdown_math").module("blocks"), blocks)
end

T["module_returns_nil_when_markdown_math_is_absent"] = function()
  package.preload["markdown_math.display_handler"] = function() error("module 'markdown_math.display_handler' not found") end
  MiniTest.finally(function() package.preload["markdown_math.display_handler"] = nil end)
  local integration = require("forge.integrations.markdown_math")
  eq(integration.module("display_handler"), nil)
  eq(integration.available(), false)
end

return T
