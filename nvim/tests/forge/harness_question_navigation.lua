vim.loader.enable(false)
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local folds = require("forge.folds")
local present, toggle = controller.present_plan_question, folds.toggle_heading
local opened, expanded = 0, 0
local action = { kind = "question", question_set_id = "pending" }
state.transcript_buf = vim.api.nvim_get_current_buf()
state.presentation = {
  transcript = {},
  activate = function(callback) callback(action, {}) end,
  open_output = function() return false end,
}
controller.present_plan_question = function(force)
  assert(force)
  opened = opened + 1
end
folds.toggle_heading = function() expanded = expanded + 1 return true end
state.active_elicitation = { elicitation = { question_set = { id = "pending" } } }
controller.open_timeline_entry()
assert(opened == 1 and expanded == 0, "Enter did not reopen the selected pending question")
action.question_set_id = "historical"
controller.open_timeline_entry()
assert(opened == 1 and expanded == 1, "historical question opened the unrelated pending picker")
state.active_elicitation = nil
action.question_set_id = "pending"
controller.open_timeline_entry()
assert(opened == 1 and expanded == 2, "answered or withdrawn question reopened a picker")
controller.present_plan_question, folds.toggle_heading = present, toggle
state.presentation = nil
print("harness_question_navigation: passed")
