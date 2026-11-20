-- Unit tests for `forge.views.harness.activity` (views/harness/activity.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

-- A hand-off predicts the next queued turn or an accepted plan's execution, so the submission it
-- follows is not yet the one to review: the winbar keeps the drafting phase instead of flashing
-- `plan · review` for the instant before the successor starts.
T["turn_pending_counts_the_hand_off_to_the_next_turn"] = function()
  local activity = require("forge.views.harness.activity")
  local working = { status = { kind = "working", activity = "working" } }
  local handing_off = { status = { kind = "working", activity = "handing_off" } }
  local reviewing = { status = { kind = "awaiting_plan_review", plan_id = "plan", revision = 2 } }
  eq({ activity.turn_running(handing_off), activity.turn_pending(handing_off) }, { false, true })
  eq({ activity.turn_running(working), activity.turn_pending(working) }, { true, true })
  eq({ activity.turn_running(reviewing), activity.turn_pending(reviewing) }, { false, false })
  eq(activity.turn_pending({ host_error = "gone", status = handing_off.status }), false)
end

return T
