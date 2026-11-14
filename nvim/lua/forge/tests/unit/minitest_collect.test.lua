local MiniTest = require("mini.test")
local collector = dofile("nvim/lua/forge/tests/support/minitest_collect.lua")
local eq = MiniTest.expect.equality
local T = MiniTest.new_set()

T["collects_the_requested_top_level_entry"] = function()
  local cases = { { desc = { "file", "entry", "nested" } } }
  local mini = { collect = function(options)
    eq(options.find_files(), { "test.lua" })
    eq(options.filter_cases(cases[1]), true)
    eq(options.filter_cases({ desc = { "file", "other" } }), false)
    return cases
  end }
  eq(collector.collect(mini, "test.lua", "entry"), cases)
end

T["groups_nested_and_parametrized_cases_without_running_them"] = function()
  local cases = {
    { desc = { "file", "computed", "one" } },
    { desc = { "file", "computed", "two" } },
    { desc = { "file", "other" } },
  }
  local items = collector.items(cases, "test.lua")
  eq(#items, 2)
  eq(items[1], { id = "test.lua::computed", parent = "test.lua", label = "computed", case = true, name = "computed", path = "test.lua" })
  eq(items[2].name, "other")
end

return T
