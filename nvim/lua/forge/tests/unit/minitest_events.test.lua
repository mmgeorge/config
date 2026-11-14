local MiniTest = require("mini.test")
local events = dofile("nvim/lua/forge/tests/support/minitest_events.lua")
local eq = MiniTest.expect.equality
local T = MiniTest.new_set()

T["groups_parametrized_entries_and_keeps_failure_locations"] = function()
  local cases = {
    { desc = { "file", "entry", "variant one" }, exec = { state = "Executing test", fails = {} } },
    { desc = { "file", "entry", "variant two" }, exec = { state = "Executing test", fails = {} } },
  }
  local output, now = {}, 0
  local reporter = events.new(cases, "nvim/lua/forge/tests/unit/example.test.lua", function(event) output[#output + 1] = event end, function() return now end)
  reporter:update(1)
  cases[1].exec = { state = "Fail", fails = { "assertion failed\nTraceback:\n nvim/lua/forge/client.lua:12" } }
  reporter:update(1)
  reporter:update(1)
  reporter:update(2)
  now = 2500000
  cases[2].exec.state = "Pass"
  reporter:update(2)
  eq(#output, 2)
  eq(output[1].event, "started")
  eq(output[2].status, "failed")
  eq(output[2].duration_ms, 2)
  eq(output[2].failures, {{ message = "entry | variant one: assertion failed", path = "nvim/lua/forge/client.lua", line = 12 }})
end

T["timeout_finishes_only_the_running_entry_once"] = function()
  local output = {}
  local cases = {{ desc = { "file", "entry" }, exec = { state = "Executing test", fails = {} } }}
  local reporter = events.new(cases, "test.lua", function(event) output[#output + 1] = event end, function() return 0 end)
  reporter:timeout()
  eq(#output, 0)
  reporter:update(1)
  reporter:timeout()
  reporter:timeout()
  eq(#output, 2)
  eq(output[2].status, "timed_out")
end

T["passing_entry_emits_one_terminal_result"] = function()
  local output = {}
  local cases = {{ desc = { "file", "entry" }, exec = { state = "Executing test", fails = {} } }}
  local reporter = events.new(cases, "test.lua", function(event) output[#output + 1] = event end, function() return 0 end)
  reporter:update(1)
  cases[1].exec.state = "Pass with notes"
  reporter:update(1)
  reporter:timeout()
  eq(#output, 2)
  eq(output[2].status, "passed")
end

return T
