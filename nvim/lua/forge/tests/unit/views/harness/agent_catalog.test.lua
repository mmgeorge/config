-- Unit tests for `forge.views.harness.agent_catalog` (agent_catalog.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

---@param name string
---@param state string
---@return table run
---@return table summary
local function child(name, state)
  return { id = name, definition = "code-explorer", task_name = name }, { state = state }
end

---@param entries table[]
---@return table state
local function catalog_state(entries)
  local state = { agent = { run = {}, summary = {} } }
  for _, entry in ipairs(entries) do
    local run, summary = child(entry[1], entry[2])
    state.agent.run[#state.agent.run + 1] = run
    state.agent.summary[run.id] = summary
  end
  return state
end

---@param run_list table[]
---@return string[]
local function names(run_list)
  return vim.tbl_map(function(run) return run.task_name end, run_list)
end

-- Children are listed by their number: string order put `-10` and `-11` before `-2`.
T["children_are_ordered_by_their_number"] = function()
  local catalog = require("forge.views.harness.agent_catalog")
  local state = catalog_state({
    { "code-explorer-10", "complete" }, { "code-explorer-2", "complete" }, { "code-explorer-1", "cancelled" },
    { "code-explorer-11", "complete" }, { "code-explorer-14", "running" }, { "code-explorer-3", "running" },
  })
  eq(names(catalog.run_list(state, false)), { "code-explorer-1", "code-explorer-2", "code-explorer-10", "code-explorer-11" })
  eq(names(catalog.run_list(state, true)), { "code-explorer-3", "code-explorer-14" })
  eq(catalog.resolve(state, "1").task_name, "code-explorer-14")
end

-- A child list read before the current one never replaces it within one host run. Applying such
-- a list showed a completed child as running again, so `Ctrl-X` in the agents picker addressed a
-- child that had already finished and nothing was interrupted.
T["an_earlier_child_list_never_replaces_a_later_one"] = function()
  local catalog = require("forge.views.harness.agent_catalog")
  local state = catalog_state({ { "code-explorer-13", "complete" } })
  state.agent.revision, state.agent.runtime_epoch = 9, "epoch"
  local stale = catalog_state({ { "code-explorer-13", "running" } }).agent
  stale.revision, stale.runtime_epoch = 8, "epoch"
  eq(catalog.replace(state, stale), false)
  eq(names(catalog.run_list(state, false)), { "code-explorer-13" })
  local restarted = vim.deepcopy(stale)
  restarted.runtime_epoch = "restarted"
  eq(catalog.replace(state, restarted), true)
  eq(names(catalog.run_list(state, true)), { "code-explorer-13" })
  local newer = catalog_state({ { "code-explorer-13", "complete" } }).agent
  newer.revision, newer.runtime_epoch = 8, "restarted"
  eq(catalog.replace(state, newer), true)
  eq(names(catalog.run_list(state, false)), { "code-explorer-13" })
end

-- An interrupt request succeeds as a whole while one child's interrupt fails; that failure was
-- dropped, so `Ctrl-X` on such a child appeared to do nothing.
T["interrupt_failure_names_the_child_outcome"] = function()
  local catalog = require("forge.views.harness.agent_catalog")
  eq(catalog.interrupt_failure({ interrupted = true, targets = { { result = { Ok = vim.NIL } } } }), nil)
  eq(catalog.interrupt_failure({ interrupted = true, targets = {
    { result = { Ok = vim.NIL } }, { result = { Err = { failure = "stale_target" } } },
  } }), "the child's task changed before the interrupt applied")
  eq(catalog.interrupt_failure({ targets = { { result = { Err = { failure = "new_code" } } } } }),
    "the interrupt failed: new code")
  eq(catalog.interrupt_failure(nil), nil)
end

return T
