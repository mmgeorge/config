-- Unit tests for `forge.views.harness.agent_picker` (agent_picker.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

---@param state string
---@return table
local function picker_state(state)
  return {
    agent = {
      run = { { id = "explorer", definition = "code-explorer", task_name = "code-explorer-11" } },
      summary = { explorer = { state = state, elapsed_ms = 1000 } },
    },
  }
end

--- Open the picker over `state` with the shared picker replaced by a recorder.
---@param state table
---@return table recorder
local function open_picker(state)
  local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
  local shared_picker = require("forge.views.picker")
  local recorder = { spec = nil, action_list = {} }
  local function record(spec) recorder.spec = spec end
  stub(shared_picker, "open", record)
  stub(shared_picker, "update", record)
  stub(shared_picker, "is_open", function(owner) return owner == "agent" end)
  stub(shared_picker, "close", function() end)
  local agent_picker = require("forge.views.harness.agent_picker")
  MiniTest.finally(function() pcall(agent_picker.close) end)
  agent_picker.open({
    host = {},
    state_provider = function() return state end,
    on_select = function() end,
    on_action = function(action, run_id, message, on_result)
      recorder.action_list[#recorder.action_list + 1] = { action = action, run_id = run_id, on_result = on_result }
    end,
  })
  return recorder
end

---@param recorder table
---@return string
local function child_detail(recorder)
  for _, option in ipairs(recorder.spec.page_list[1].option_list) do
    if option.value.run then return option.detail end
  end
  error("the picker lists no child")
end

---@param recorder table
local function press_interrupt(recorder)
  for _, action in ipairs(recorder.spec.action_list) do
    if action.id == "interrupt" then
      action.callback({ option = recorder.spec.page_list[1].option_list[2] })
      return
    end
  end
  error("the picker has no interrupt action")
end

-- `Ctrl-X` on a running child looked ignored: the host applied the child's earlier events for
-- seconds after the interrupt, so the row kept counting tools as if nothing had been asked.
T["interrupt_marks_the_child_cancelling_until_it_settles"] = function()
  local state = picker_state("running")
  local recorder = open_picker(state)
  press_interrupt(recorder)
  eq(#recorder.action_list, 1)
  eq(recorder.action_list[1].action, "interrupt")
  eq(child_detail(recorder):match("^Cancelling · ") ~= nil, true)
  state.agent.summary.explorer.state = "cancelled"
  require("forge.views.harness.agent_picker").refresh()
  eq(child_detail(recorder):match("Cancelling") == nil, true)
  eq(child_detail(recorder):lower():match("cancelled") ~= nil, true)
end

-- A child that finished on its own before the interrupt reached it says so instead of reading as
-- a completed child whose interrupt silently did nothing.
T["interrupt_names_a_child_that_finished_first"] = function()
  local state = picker_state("running")
  local recorder = open_picker(state)
  press_interrupt(recorder)
  recorder.action_list[1].on_result(nil)
  state.agent.summary.explorer.state = "complete"
  require("forge.views.harness.agent_picker").refresh()
  eq(child_detail(recorder):match("^Finished before the interrupt reached it · ") ~= nil, true)
end

-- An interrupt the host could not carry out shows the reason in the row.
T["interrupt_failure_is_shown_in_the_row"] = function()
  local state = picker_state("running")
  local recorder = open_picker(state)
  press_interrupt(recorder)
  recorder.action_list[1].on_result("the child's task changed before the interrupt applied")
  eq(child_detail(recorder):match("^Interrupt failed: the child's task changed") ~= nil, true)
end

--- Replace the loop clock the picker reads for its notice period with a value the case sets.
---@return table clock `clock.now_ms` is the time the picker reads.
local function stub_clock()
  local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
  local clock = { now_ms = 1000 }
  stub(vim.uv, "now", function() return clock.now_ms end)
  return clock
end

-- A refused interrupt's reason stayed in the row through the child's next Queued, Running, and
-- Completed states, describing a run that no longer existed.
T["a_refused_interrupt_leaves_the_row_when_the_child_state_changes"] = function()
  stub_clock()
  local state = picker_state("complete")
  local recorder = open_picker(state)
  press_interrupt(recorder)
  eq(child_detail(recorder):match("^Interrupt failed: ") ~= nil, true)
  state.agent.summary.explorer.state = "queued"
  require("forge.views.harness.agent_picker").refresh()
  eq(child_detail(recorder):match("Interrupt failed") == nil, true)
  state.agent.summary.explorer.state = "complete"
  require("forge.views.harness.agent_picker").refresh()
  eq(child_detail(recorder):match("Interrupt failed") == nil, true)
end

-- A refusal also leaves the row once the notice period has passed while the child stays put.
T["a_refused_interrupt_leaves_the_row_after_the_notice_period"] = function()
  local clock = stub_clock()
  local state = picker_state("complete")
  local recorder = open_picker(state)
  local agent_picker = require("forge.views.harness.agent_picker")
  press_interrupt(recorder)
  clock.now_ms = clock.now_ms + agent_picker.interrupt_notice_ms - 1
  agent_picker.refresh()
  eq(child_detail(recorder):match("^Interrupt failed: ") ~= nil, true)
  clock.now_ms = clock.now_ms + 1
  agent_picker.refresh()
  eq(child_detail(recorder):match("Interrupt failed") == nil, true)
end

-- A child that finished first and then runs a follow-up is running again, not cancelling.
T["a_lost_interrupt_race_leaves_the_row_when_the_child_runs_again"] = function()
  stub_clock()
  local state = picker_state("running")
  local recorder = open_picker(state)
  press_interrupt(recorder)
  recorder.action_list[1].on_result(nil)
  state.agent.summary.explorer.state = "complete"
  require("forge.views.harness.agent_picker").refresh()
  eq(child_detail(recorder):match("^Finished before the interrupt reached it · ") ~= nil, true)
  state.agent.summary.explorer.state = "running"
  require("forge.views.harness.agent_picker").refresh()
  eq(child_detail(recorder):match("^Running") ~= nil, true)
end

-- A follow-up's run counts only its own tools; the row once dropped the child's earlier tally.
T["a_child_run_again_keeps_its_earlier_tool_tally"] = function()
  local state = picker_state("complete")
  state.agent.summary.explorer.run_tool_label = "14/14 tools"
  state.agent.summary.explorer.exchange_count = 2
  local recorder = open_picker(state)
  eq(child_detail(recorder):match(" · 14/14 tools across 2 runs$") ~= nil, true)
end

-- `Ctrl-X` on a child that is not running sends nothing and says why.
T["interrupt_of_a_finished_child_is_refused_in_the_row"] = function()
  local state = picker_state("complete")
  local recorder = open_picker(state)
  press_interrupt(recorder)
  eq(#recorder.action_list, 0)
  eq(child_detail(recorder):match("^Interrupt failed: code%-explorer%-11 is not running") ~= nil, true)
end

return T
