-- Unit tests for `forge.views.harness.task` (views/harness/task.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["review_phase_shows_only_after_the_submitting_turn_settles"] = function()
  local task = require("forge.views.harness.task")
  local reviewed = { kind = "plan", phase = "review", status = "waiting" }
  -- A turn may resubmit its plan several times, so a submission it made is not yet the one to review.
  eq(task.label(reviewed, true), "plan · draft")
  eq(task.label(reviewed, false), "plan · review")
  eq(task.label({ kind = "plan", phase = "draft", status = "running" }, true), "plan · draft")
  eq(task.label({ kind = "execute", phase = "implement", status = "paused" }, true), "exec · implement · paused")
end

return T
