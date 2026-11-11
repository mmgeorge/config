-- Unit tests for `forge.views.harness.plan_question` (plan_question/init.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

---@param answered boolean
local function elicitation(answered)
  return {
    revision = 1, answer_revision = 0, current_index = answered and 1 or 0,
    answer = answered and { { question_id = "scope", response = { kind = "selected", option = "Small" } } } or {},
    question_set = { id = "set", questions = { { id = "scope", question = "Scope?", options = { { label = "Small" } } } } },
  }
end

---@param overrides table
local function host(overrides)
  local window = vim.api.nvim_get_current_win()
  return vim.tbl_extend("force", {
    window_list = { window }, control_win = window, allow_ask = true,
    answer = function() end, continue = function() end, closed = function() end, pending = function() end,
  }, overrides)
end

T["submit_reports_the_close"] = function()
  local plan_question = require("forge.views.harness.plan_question")
  local picker = require("forge.views.picker")
  local closed, continued = 0, 0
  plan_question.open(elicitation(true), host({
    closed = function() closed = closed + 1 end,
    continue = function() continued = continued + 1 end,
  }))
  local spec = picker._state_for_test().spec
  spec.on_confirm({ page = { id = "review" }, option = { value = "submit" } })
  assert(not picker.is_open(), "submitting answers left the picker open")
  eq(closed, 1)
  eq(continued, 1)
end

T["late_answer_leaves_other_pickers"] = function()
  local plan_question = require("forge.views.harness.plan_question")
  local picker = require("forge.views.picker")
  local answered
  local window = vim.api.nvim_get_current_win()
  plan_question.open(elicitation(false), host({ answer = function(_, callback) answered = callback end }))
  local spec = picker._state_for_test().spec
  spec.on_confirm({ page = { id = "scope" }, option = { value = "Small", entry_kind = "choice" } })
  assert(answered, "the answer was not sent")
  picker.open({ owner = "approval", host = { window_list = { window }, control_win = window },
    page_list = { { id = "decision", option_list = { { label = "Allow once" } } } } })
  answered(elicitation(true))
  eq(picker._state_for_test().spec.owner, "approval")
  eq(picker._state_for_test().spec.page_list[1].id, "decision")
end

return T
