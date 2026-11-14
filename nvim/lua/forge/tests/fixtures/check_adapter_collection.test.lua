local MiniTest = require("mini.test")
local T = MiniTest.new_set({ hooks = { pre_case = function()
  assert(not vim.env.FORGE_TEST_COLLECT, "collection invoked an execution hook")
end } })

if false then
T["excluded"] = function() error("excluded case executed") end
end

T["computed_" .. "entry"] = function()
  assert(not vim.env.FORGE_TEST_COLLECT, "collection invoked a test body")
end

T["nested"] = MiniTest.new_set({ parametrize = { { 1 }, { 2 } } })
T["nested"]["variant"] = function(value)
  assert(not vim.env.FORGE_TEST_COLLECT, "collection invoked a nested test body")
  MiniTest.expect.equality(value > 0, true)
end

return T
