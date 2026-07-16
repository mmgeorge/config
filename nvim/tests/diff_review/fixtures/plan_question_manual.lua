local elicitation = {
  question_set = {
    id = "manual-question-set",
    questions = {
      {
        id = "manual-migration",
        header = "Migration",
        question = "Which migration strategy should the plan use?",
        options = {
          { label = "Staged migration", description = "Support both formats temporarily." },
          { label = "Immediate replacement", description = "Remove the old format now." },
          { label = "Compatibility layer", description = "Preserve an explicit adapter." },
        },
        allow_freeform = true,
      },
      {
        id = "manual-testing",
        header = "Testing",
        question = "Which verification depth should the plan require?",
        options = {
          { label = "Focused", description = "Run focused integration coverage." },
          { label = "Full", description = "Run the complete DiffReview suite." },
        },
        allow_freeform = true,
      },
      {
        id = "manual-rollout",
        header = "Rollout",
        question = "How should the rollout proceed?",
        options = {
          { label = "Incremental", description = "Land the state machine before UI polish." },
          { label = "Atomic", description = "Land the complete workflow together." },
        },
        allow_freeform = true,
      },
    },
  },
  answer = {},
  current_index = 0,
  clarification_active = false,
}

local function commit(params, callback)
  elicitation.answer[#elicitation.answer + 1] = {
    question_id = params.question_id,
    response = params.response or { kind = "skipped" },
  }
  elicitation.current_index = elicitation.current_index + 1
  callback(vim.deepcopy(elicitation))
end

local transcript_win = vim.api.nvim_get_current_win()
vim.bo.swapfile = false
vim.api.nvim_buf_set_name(0, "HarnessPickerManual")
vim.cmd("belowright 3split")
local composer_win = vim.api.nvim_get_current_win()
vim.bo.swapfile = false
vim.api.nvim_buf_set_name(0, "HarnessInputPickerManual")
require("diff_review.views.harness.plan_question").open(elicitation, {
  transcript_win = transcript_win,
  window_list = { transcript_win, composer_win },
  control_win = composer_win,
  answer = commit,
  skip = commit,
  ask = function(params) vim.notify("Ask: " .. params.text) end,
  continue = function() vim.notify("Continue planning") end,
})
