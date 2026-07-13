vim.loader.enable(false)

local diff_review = require("diff_review")

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function assert_equals(actual, expected, message)
  if actual ~= expected then
    error((message or "values differ") .. ": expected " .. vim.inspect(expected) .. ", got " .. vim.inspect(actual), 2)
  end
end

local question_set = {
  id = "set",
  questions = {
    {
      id = "migration",
      header = "Migration",
      question = "Which migration strategy should the plan use?",
      options = {
        { label = "Staged", description = "Support both formats temporarily." },
        { label = "Immediate", description = "Replace immediately." },
        { label = "Compatible", description = "Keep a compatibility layer." },
      },
      allow_freeform = true,
    },
    {
      id = "storage",
      header = "Storage",
      question = "Which storage boundary?",
      options = {
        { label = "SQLite", description = "Persist through the broker." },
        { label = "Memory", description = "Keep state editor-local." },
      },
      allow_freeform = true,
    },
  },
}

local elicitation = {
  question_set = question_set,
  answer = {},
  current_index = 0,
  clarification_active = false,
}

local ok, failure = pcall(function()
  diff_review.setup({
    walkthrough_inventory = "sem",
    harness = { backend = "mock" },
  })
  local view = require("diff_review.views.harness.plan_question")
  local transcript_win = vim.api.nvim_get_current_win()
  local ask_params = nil
  local continued = false
  local function commit(params, callback)
    elicitation.answer = vim.tbl_filter(function(answer)
      return answer.question_id ~= params.question_id
    end, elicitation.answer)
    elicitation.answer[#elicitation.answer + 1] = {
      question_id = params.question_id,
      response = params.response or { kind = "skipped" },
    }
    elicitation.current_index = #question_set.questions
    for index, question in ipairs(question_set.questions) do
      if not vim.iter(elicitation.answer):any(function(answer) return answer.question_id == question.id end) then
        elicitation.current_index = index - 1
        break
      end
    end
    callback(vim.deepcopy(elicitation))
  end
  view.open(elicitation, {
    transcript_win = transcript_win,
    answer = commit,
    skip = commit,
    ask = function(params) ask_params = params end,
    continue = function() continued = true end,
  })

  local state = view._state_for_test()
  assert_true(view.is_open(), "question float should open")
  assert_equals(vim.api.nvim_get_mode().mode:sub(1, 1), "n",
    "opening a question float should leave Neovim in Normal mode")
  assert_true(vim.inspect(vim.api.nvim_win_get_config(state.win).title):find(
    "Harness question 1 of 2", 1, true
  ) ~= nil, "question float should show progress")
  assert_equals(vim.api.nvim_win_get_config(state.win).focusable, false,
    "question selection should remain a non-focusable presentation float")
  assert_equals(vim.api.nvim_get_current_win(), transcript_win,
    "question selection should route modal keys from the transcript window")
  assert_true(vim.fn.maparg("a", "n", false, true).callback ~= nil, "Ask should map a")
  assert_equals(vim.fn.maparg("<C-s>", "n", false, true).callback, nil,
    "Ctrl-s should not submit from the question float")
  assert_true(vim.tbl_contains(vim.api.nvim_buf_get_lines(state.buf, 0, -1, false),
    "  <-> previous/next question, Tab add feedback"),
    "question help should show only navigation and feedback hints")
  vim.fn.maparg("s", "n", false, true).callback()
  assert_equals(state.selected_index, #state.entry_list, "s should move to the previous option")
  vim.fn.maparg("t", "n", false, true).callback()
  assert_equals(state.selected_index, 1, "t should move to the next option")
  local selected_highlight = vim.iter(vim.api.nvim_buf_get_extmarks(state.buf, -1, 0, -1, { details = true })):any(
    function(extmark) return extmark[4].hl_group == "DiffReviewHarnessQuestionSelected" end
  )
  assert_true(selected_highlight, "the highlighted option should use the light-blue selection group")
  local question_config_before_input = vim.api.nvim_win_get_config(state.win)
  local question_height_before_input = vim.api.nvim_win_get_height(state.win)
  vim.fn.maparg("<Tab>", "n", false, true).callback()
  assert_equals(state.input_kind, "choice", "Tab should open selected-option feedback")
  assert_true(vim.api.nvim_win_is_valid(state.input_win), "feedback should open an attached input window")
  local input_border = vim.api.nvim_win_get_config(state.input_win).border
  local input_corner = type(input_border[1]) == "table" and input_border[1][1] or input_border[1]
  assert_equals(input_corner, "╭", "feedback should use an independent rounded border")
  local input_has_padding = vim.iter(vim.api.nvim_buf_get_extmarks(
    state.input_buf, -1, 0, -1, { details = true }
  )):any(function(extmark)
    local details = extmark[4]
    return details.virt_text_pos == "inline"
      and details.virt_text and details.virt_text[1] and details.virt_text[1][1] == " "
  end)
  assert_true(input_has_padding, "feedback text should have presentation-only left padding")
  assert_equals(vim.api.nvim_win_get_config(state.win).row, question_config_before_input.row,
    "opening feedback should preserve the question float's starting row")
  assert_equals(vim.api.nvim_win_get_height(state.win), question_height_before_input,
    "opening feedback should preserve the question float's height")
  assert_equals(vim.api.nvim_win_get_config(state.input_win).row,
    question_config_before_input.row + question_height_before_input + 2,
    "feedback should attach directly below the question float")
  vim.api.nvim_buf_set_lines(state.input_buf, 0, -1, false, { "Keep one compatibility release" })
  vim.fn.maparg("go", "n", false, true).callback()
  assert_equals(vim.api.nvim_get_current_win(), transcript_win,
    "go should return focus to the transcript-backed question controls")
  vim.fn.maparg("e", "n", false, true).callback()
  assert_equals(state.selected_index, 2, "options should remain selectable while feedback stays open")
  assert_equals(vim.api.nvim_buf_get_lines(state.input_buf, 0, -1, false)[1],
    "Keep one compatibility release", "switching options should preserve the feedback draft")
  vim.fn.maparg("go", "n", false, true).callback()
  assert_equals(vim.api.nvim_get_current_win(), state.input_win, "go should return focus to feedback")
  assert_equals(vim.api.nvim_get_mode().mode:sub(1, 1), "n",
    "go should return to feedback without entering Insert mode")
  vim.fn.maparg("<C-s>", "i", false, true).callback()
  assert_equals(elicitation.current_index, 1, "feedback should advance the question")
  assert_equals(elicitation.answer[1].response.feedback, "Keep one compatibility release",
    "feedback should preserve the attached editor content")
  assert_equals(elicitation.answer[1].response.option, "Immediate",
    "feedback should apply to the option selected before submission")

  vim.fn.maparg("o", "n", false, true).callback()
  assert_equals(state.selected_index, 3, "o should highlight Other for question two")
  assert_equals(state.input_kind, nil, "quick keys should not submit or open an input")
  assert_equals(elicitation.current_index, 1, "quick keys should not advance the question")
  vim.fn.maparg("<CR>", "n", false, true).callback()
  assert_equals(state.input_kind, "other", "Enter should open the selected Other input")
  vim.fn.maparg("go", "n", false, true).callback()
  vim.fn.maparg("a", "n", false, true).callback()
  assert_equals(state.input_kind, "ask", "Ask quick key should retarget the open editor without submitting")
  vim.fn.maparg("<CR>", "n", false, true).callback()
  vim.api.nvim_buf_set_lines(state.input_buf, 0, -1, false, { "Why does SQLite matter?" })
  vim.fn.maparg("<C-s>", "i", false, true).callback()
  assert_equals(ask_params.text, "Why does SQLite matter?", "Ask should submit its attached input")
  assert_true(not view.is_open(), "Ask should close the float while the clarification runs")

  view.open(elicitation, {
    transcript_win = transcript_win,
    answer = commit,
    skip = commit,
    ask = function(params) ask_params = params end,
    continue = function() continued = true end,
  })
  vim.fn.maparg("<Left>", "n", false, true).callback()
  assert_equals(state.elicitation.current_index, 0, "Left should show the previous question without answering")
  assert_equals(elicitation.current_index, 1, "question navigation should not mutate the host elicitation")
  vim.fn.maparg("<Right>", "n", false, true).callback()
  assert_equals(state.elicitation.current_index, 1, "Right should show the next question")
  assert_equals(vim.fn.maparg("<S-Tab>", "n", false, true).callback, nil,
    "Shift-Tab should not own a question action")
  vim.api.nvim_win_call(state.win, function() vim.fn.winrestview({ topline = 999 }) end)
  vim.api.nvim_exec_autocmds("WinScrolled", { buffer = state.buf })
  local maximum_topline = math.max(1,
    vim.api.nvim_buf_line_count(state.buf) - vim.api.nvim_win_get_height(state.win) + 1)
  assert_true(vim.api.nvim_win_call(state.win, vim.fn.winsaveview).topline <= maximum_topline,
    "question popup should not scroll beyond rendered content")
  vim.fn.maparg("n", "n", false, true).callback()
  vim.fn.maparg("<CR>", "n", false, true).callback()
  assert_equals(elicitation.current_index, 2, "answering the final question should open review")
  local review_lines = vim.api.nvim_buf_get_lines(state.buf, 0, -1, false)
  assert_true(not vim.tbl_contains(review_lines, "  Review answers"),
    "final page should not repeat its title inside the content")
  assert_true(vim.tbl_contains(review_lines, "     Immediate"),
    "review should show the selected answer for each question")
  assert_true(vim.tbl_contains(review_lines, "     Keep one compatibility release"),
    "review should show additional option feedback")
  assert_true(vim.tbl_contains(review_lines, "     SQLite"),
    "review should show answers in question order")
  local answer_row = vim.iter(review_lines):enumerate():find(function(_, line)
    return line == "     SQLite"
  end)
  assert_true(answer_row ~= nil, "review should expose the final answer row")
  local answer_is_dimmed = vim.iter(vim.api.nvim_buf_get_extmarks(
    state.buf, -1, { answer_row - 1, 0 }, { answer_row - 1, -1 }, { details = true }
  )):any(function(extmark)
    return extmark[4].hl_group == "DiffReviewHarnessQuestionHint"
  end)
  assert_true(not answer_is_dimmed, "the final answer should not inherit the footer hint highlight")
  local answer_is_blue = vim.iter(vim.api.nvim_buf_get_extmarks(
    state.buf, -1, { answer_row - 1, 0 }, { answer_row - 1, -1 }, { details = true }
  )):any(function(extmark)
    return extmark[4].hl_group == "DiffReviewHarnessQuestionReviewAnswer"
  end)
  assert_true(answer_is_blue, "review answers should use the light-blue answer highlight")
  local input_row = vim.iter(review_lines):enumerate():find(function(_, line)
    return line == "     Keep one compatibility release"
  end)
  local input_uses_normal_text = vim.iter(vim.api.nvim_buf_get_extmarks(
    state.buf, -1, { input_row - 1, 0 }, { input_row - 1, -1 }, { details = true }
  )):any(function(extmark)
    return extmark[4].hl_group == "DiffReviewHarnessQuestionReviewInput"
  end)
  assert_true(input_uses_normal_text, "additional review input should use the normal text highlight")
  local question_row = vim.iter(review_lines):enumerate():find(function(_, line)
    return line == "  2. Which storage boundary?"
  end)
  local question_is_white = vim.iter(vim.api.nvim_buf_get_extmarks(
    state.buf, -1, { question_row - 1, 0 }, { question_row - 1, -1 }, { details = true }
  )):any(function(extmark)
    return extmark[4].hl_group == "DiffReviewHarnessQuestionReviewQuestion"
  end)
  assert_true(question_is_white, "review questions should use the white question highlight")
  assert_equals(vim.wo[state.win].winhighlight, "Cursor:DiffReviewHarnessHiddenCursor",
    "the review page should hide its inactive cursor")
  assert_equals(vim.api.nvim_win_get_config(state.win).focusable, false,
    "the review page should not retain terminal cursor focus")
  assert_equals(vim.api.nvim_get_current_win(), transcript_win,
    "the review page should route modal keys from the transcript window")
  assert_true(vim.api.nvim_win_get_width(state.win) <= 78,
    "the question float should use the compact horizontal width")
  assert_equals(vim.fn.maparg("<C-s>", "n", false, true).callback, nil,
    "Ctrl-s should not bypass final answer review")

  vim.fn.maparg("n", "n", false, true).callback()
  assert_equals(vim.wo[state.win].winhighlight, "Cursor:DiffReviewHarnessHiddenCursor",
    "returning to question selection should keep its cursor hidden")
  assert_equals(vim.api.nvim_win_get_config(state.win).focusable, false,
    "returning to question selection should keep the float non-focusable")
  assert_equals(vim.api.nvim_get_current_win(), transcript_win,
    "returning to question selection should retain transcript focus")
  assert_equals(state.elicitation.current_index, 0, "n should return to the first question")
  assert_equals(elicitation.current_index, 2, "review navigation should not mutate durable answer progress")
  assert_equals(state.selected_index, 2, "returning to a question should restore its selected answer")
  vim.fn.maparg("<Tab>", "n", false, true).callback()
  assert_equals(vim.api.nvim_buf_get_lines(state.input_buf, 0, -1, false)[1],
    "Keep one compatibility release", "returning to a question should restore its additional input")
  vim.fn.maparg("<C-s>", "i", false, true).callback()
  assert_equals(state.elicitation.current_index, 2,
    "Ctrl-s in the attached input should save the answer and return to review")
  vim.fn.maparg("y", "n", false, true).callback()
  assert_true(continued, "y should submit reviewed answers and continue planning")
  assert_true(not view.is_open(), "submitting reviewed answers should close the question float")
end)

if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
