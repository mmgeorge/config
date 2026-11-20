-- Unit tests for `forge.render.harness.agent_summary` (agent_summary.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

-- The agents picker shows the duration and tool tally of the child's own exchange, the numbers its
-- status row, transcript row, and heading show. It once measured the agent record from admission
-- to its last update, so a child read `Ran 10s` in its row and `Completed in 14s` in the picker,
-- and it counted `0 tools called` beside a status row's `2/2 tools`.
T["picker_status_reads_the_child_exchange_clock_and_tally"] = function()
  local agent_summary = require("forge.render.harness.agent_summary")
  local run = { id = "child", task_name = "code-explorer-4", created_at_ms = 0, updated_at_ms = 14000 }
  eq(agent_summary.status_detail(run, { state = "running", elapsed_ms = 2500, running_since_ms = 100000,
    tool_label = "2/2 tools" }, 104600), "Running for 7s · 2/2 tools")
  eq(agent_summary.status_detail(run, { state = "complete", elapsed_ms = 10987, running_since_ms = vim.NIL,
    tool_label = "5/5 tools" }, 200000), "Completed in 10s · 5/5 tools")
  eq(agent_summary.status_detail(run, { state = "queued", elapsed_ms = 0, tool_label = vim.NIL }, 200000),
    "Queued for 0s")
end

return T
