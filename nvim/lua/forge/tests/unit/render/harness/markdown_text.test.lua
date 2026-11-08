local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["normalizes_math_through_markdown_math"] = function()
  stub(require("forge.integrations.markdown_math"), "module", function(name)
    eq(name, "blocks")
    return { normalize = function(text) return "normalized:" .. text end }
  end)
  eq(require("forge.render.harness.markdown_text").normalize_math("\\(x\\)"), "normalized:\\(x\\)")
end

T["passes_math_through_without_markdown_math"] = function()
  stub(require("forge.integrations.markdown_math"), "module", function() return nil end)
  eq(require("forge.render.harness.markdown_text").normalize_math("\\(x\\)"), "\\(x\\)")
end

return T
