local M = {}

local namespace = vim.api.nvim_create_namespace("DiffReviewHarnessPlanQuestion")
local choice_window = require("diff_review.views.harness.choice_flow.window")

---@param transcript_win integer
---@param width integer
---@param height integer
---@param title string
---@return integer, integer
function M.open(transcript_win, width, height, title)
  return choice_window.open(transcript_win, width, height, title, "DiffReviewPlanQuestion", 80)
end

---@param win integer
---@param transcript_win integer
---@param width integer
---@param height integer
function M.resize(win, transcript_win, width, height)
  choice_window.resize(win, transcript_win, width, height)
end

---@param buf integer
---@param frame DiffReviewPlanQuestionFrame
---@param selected_index integer
---@param entry_list DiffReviewPlanQuestionEntry[]
function M.render(buf, frame, selected_index, entry_list)
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, frame.lines)
  vim.api.nvim_buf_clear_namespace(buf, namespace, 0, -1)
  for index, line in ipairs(frame.option_line) do
    local entry = entry_list[index]
    if entry and entry.key then
      vim.api.nvim_buf_add_highlight(
        buf,
        namespace,
        "DiffReviewHarnessQuestionKey",
        line - 1,
        2,
        2 + #entry.key
      )
    end
  end
  local selected_line = frame.option_line[selected_index]
  if selected_line then
    vim.api.nvim_buf_add_highlight(buf, namespace, "DiffReviewHarnessQuestionSelected", selected_line - 1, 0, -1)
  end
  for _, line in ipairs(frame.review_question_line) do
    vim.api.nvim_buf_add_highlight(buf, namespace, "DiffReviewHarnessQuestionReviewQuestion", line - 1, 0, -1)
  end
  for _, line in ipairs(frame.review_answer_line) do
    vim.api.nvim_buf_add_highlight(buf, namespace, "DiffReviewHarnessQuestionReviewAnswer", line - 1, 0, -1)
  end
  for _, line in ipairs(frame.review_input_line) do
    vim.api.nvim_buf_add_highlight(buf, namespace, "DiffReviewHarnessQuestionReviewInput", line - 1, 0, -1)
  end
  vim.api.nvim_buf_add_highlight(
    buf,
    namespace,
    "DiffReviewHarnessQuestionHint",
    frame.footer_line - 1,
    0,
    -1
  )
  vim.bo[buf].modifiable = false
  choice_window.clamp_view(vim.fn.bufwinid(buf), buf)
end

---@param win integer
---@param hidden boolean
function M.set_cursor_hidden(win, hidden)
  choice_window.set_cursor_hidden(win, hidden)
end

---@param win integer
---@param focusable boolean
function M.set_focusable(win, focusable)
  choice_window.set_focusable(win, focusable)
end

---@param win integer
---@param title string
function M.set_title(win, title)
  choice_window.set_title(win, title)
end

---@param win integer?
function M.close(win)
  choice_window.close(win)
end

return M
