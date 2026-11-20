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

-- A compaction occupies the session as a turn does, but only one that stopped a turn, or runs
-- inside one, leaves a turn to follow it. A compaction while a plan waits in review leaves the
-- submission the one to review, so it must not read as a pending planning turn.
T["turn_awaited_counts_a_compaction_only_when_a_turn_waits_for_it"] = function()
  local activity = require("forge.views.harness.activity")
  local between_turns = { status = { kind = "compacting", started_at_ms = 0, turn_waiting = false } }
  local holding_a_turn = { status = { kind = "compacting", started_at_ms = 0, turn_waiting = true } }
  local handing_off = { status = { kind = "working", activity = "handing_off" } }
  eq({ activity.turn_pending(between_turns), activity.turn_awaited(between_turns) }, { true, false })
  eq({ activity.turn_pending(holding_a_turn), activity.turn_awaited(holding_a_turn) }, { true, true })
  eq(activity.turn_awaited(handing_off), true)
  eq(activity.turn_awaited({ host_error = "gone", status = holding_a_turn.status }), false)
end

return T
