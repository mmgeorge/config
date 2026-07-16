vim.loader.enable(false)

local diff_review = require("diff_review")

local function assert_true(value, message)
  if not value then error(message or "expected truthy value", 2) end
end

local function assert_equals(actual, expected, message)
  if actual ~= expected then
    error((message or "values differ") .. ": expected " .. vim.inspect(expected) .. ", got " .. vim.inspect(actual), 2)
  end
end

local function invoke(key, mode)
  local mapping = vim.fn.maparg(key, mode or "n", false, true)
  assert_true(mapping.callback ~= nil, "missing mapping for " .. key)
  mapping.callback()
end

local question_set = {
  id = "set",
  questions = {
    {
      id = "migration",
      header = "Migration strategy",
      question = "Which migration strategy should the plan use?",
      options = {
        { label = "Staged", description = "Support both formats temporarily." },
        { label = "Immediate", description = "Replace immediately." },
      },
      allow_freeform = true,
    },
    {
      id = "storage",
      header = "Storage boundary",
      question = "Which storage boundary should own state?",
      options = {
        { label = "SQLite", description = "Persist through the broker." },
        { label = "Memory", description = "Keep state editor-local." },
      },
      allow_freeform = true,
    },
  },
}

local elicitation = { question_set = question_set, answer = {}, current_index = 0 }

local ok, failure = pcall(function()
  diff_review.setup({ walkthrough_inventory = "sem", harness = { backend = "mock" } })
  local view = require("diff_review.views.harness.plan_question")
  local transcript_win = vim.api.nvim_get_current_win()
  local ask_params = nil
  local continued = false
  local function commit(params, callback)
    elicitation.answer = vim.tbl_filter(function(answer) return answer.question_id ~= params.question_id end, elicitation.answer)
    elicitation.answer[#elicitation.answer + 1] = { question_id = params.question_id, response = params.response }
    elicitation.current_index = #question_set.questions
    for index, question in ipairs(question_set.questions) do
      if not vim.iter(elicitation.answer):any(function(answer) return answer.question_id == question.id end) then
        elicitation.current_index = index - 1
        break
      end
    end
    callback(vim.deepcopy(elicitation))
  end
  local host = {
    transcript_win = transcript_win,
    window_list = { transcript_win },
    control_win = transcript_win,
    answer = commit,
    ask = function(params) ask_params = params end,
    continue = function() continued = true end,
  }
  view.open(elicitation, host)
  local active = view._state_for_test()
  assert_true(view.is_open(), "planning picker should open")
  assert_equals(vim.api.nvim_get_mode().mode:sub(1, 1), "n", "planning picker should leave Insert mode")
  assert_true(vim.inspect(vim.api.nvim_win_get_config(active.win).title):find("Migration strategy", 1, true) ~= nil,
    "planning picker should title the current page")
  assert_true(vim.inspect(vim.api.nvim_win_get_config(active.win).title):find("1/2", 1, true) ~= nil,
    "planning picker should show page progress")

  invoke("e")
  assert_equals(active.state.selected_index_by_page.migration, 2, "quick key should select without submitting")
  assert_equals(elicitation.current_index, 0, "quick key should not advance planning")
  invoke("<Tab>")
  vim.api.nvim_buf_set_lines(active.input_buf, 0, -1, false, { "Keep one compatibility release" })
  invoke("<C-s>", "i")
  assert_equals(elicitation.answer[1].response.option, "Immediate", "feedback should preserve the selected choice")
  assert_equals(elicitation.answer[1].response.feedback, "Keep one compatibility release",
    "feedback should preserve multiline input content")
  assert_equals(active.state.page_index, 2, "answering should advance to the provider-selected question")

  assert_true(vim.fn.maparg("a", "n", false, true).callback ~= nil,
    "Ask mapping missing in " .. vim.bo.filetype .. " with " .. vim.inspect(active.mapping_list))
  invoke("a")
  assert_equals(active.state.selected_index_by_page.storage, 4, "Ask should select the reserved final option")
  invoke("<CR>")
  vim.api.nvim_buf_set_lines(active.input_buf, 0, -1, false, { "Why does storage matter?" })
  invoke("<C-s>", "i")
  assert_equals(ask_params.text, "Why does storage matter?", "Ask should submit clarification text")
  assert_true(not view.is_open(), "Ask should close while clarification runs")

  view.open(elicitation, host)
  active = view._state_for_test()
  invoke("n")
  invoke("<CR>")
  assert_equals(elicitation.current_index, 2, "final answer should advance to review")
  local review_lines = vim.api.nvim_buf_get_lines(active.buf, 0, -1, false)
  assert_true(vim.iter(review_lines):any(function(line) return line:find("Immediate", 1, true) end),
    "review should show the first answer")
  assert_true(vim.iter(review_lines):any(function(line) return line:find("SQLite", 1, true) end),
    "review should show the second answer")
  local answer_highlight = vim.iter(vim.api.nvim_buf_get_extmarks(active.buf, -1, 0, -1, { details = true })):any(
    function(extmark) return extmark[4].hl_group == "DiffReviewPickerAnswer" end
  )
  assert_true(answer_highlight, "review answers should use the shared answer highlight")
  invoke("y")
  assert_true(continued, "review confirmation should continue planning")
  assert_true(not view.is_open(), "review confirmation should close the picker")
end)

if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
