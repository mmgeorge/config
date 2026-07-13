local M = {}

---@class DiffReviewPlanQuestionFrame
---@field lines string[]
---@field option_line integer[]
---@field review_question_line integer[]
---@field review_answer_line integer[]
---@field review_input_line integer[]
---@field footer_line integer
---@field reviewing boolean

local function append_wrapped(line_list, text, width, prefix)
  local first_line = #line_list + 1
  prefix = prefix or ""
  local current = prefix
  for word in tostring(text or ""):gmatch("%S+") do
    local separator = current == prefix and "" or " "
    if vim.fn.strdisplaywidth(current .. separator .. word) > width and current ~= prefix then
      line_list[#line_list + 1] = current
      current = string.rep(" ", vim.fn.strdisplaywidth(prefix)) .. word
    else
      current = current .. separator .. word
    end
  end
  line_list[#line_list + 1] = current ~= prefix and current or prefix
  return first_line, #line_list
end

---@param question table?
---@param entry_list DiffReviewPlanQuestionEntry[]
---@param width integer
---@param review_item_list DiffReviewPlanQuestionReviewItem[]?
---@return DiffReviewPlanQuestionFrame
function M.build(question, entry_list, width, review_item_list)
  local line_list = { "" }
  local option_line = {}
  local review_question_line = {}
  local review_answer_line = {}
  local review_input_line = {}
  if review_item_list then
    for index, item in ipairs(review_item_list) do
      local question_first, question_last = append_wrapped(
        line_list,
        ("%d. %s"):format(index, item.question),
        width,
        "  "
      )
      for line = question_first, question_last do review_question_line[#review_question_line + 1] = line end
      local answer_first, answer_last = append_wrapped(line_list, item.answer, width, "     ")
      for line = answer_first, answer_last do review_answer_line[#review_answer_line + 1] = line end
      if item.additional_input then
        local input_first, input_last = append_wrapped(line_list, item.additional_input, width, "     ")
        for line = input_first, input_last do review_input_line[#review_input_line + 1] = line end
      end
      if index < #review_item_list then line_list[#line_list + 1] = "" end
    end
  else
    append_wrapped(line_list, question and question.question or "Planning feedback", width, "  ")
    line_list[#line_list + 1] = ""
    for index, entry in ipairs(entry_list) do
      option_line[index] = #line_list + 1
      local key = entry.key and (entry.key .. "  ") or "   "
      local description = entry.description ~= "" and (" — " .. entry.description) or ""
      append_wrapped(line_list, entry.label .. description, width, "  " .. key)
    end
  end
  line_list[#line_list + 1] = ""
  local footer_line = #line_list + 1
  if review_item_list then
    line_list[#line_list + 1] = "  y submit  n revise  q close"
  else
    line_list[#line_list + 1] = "  <-> previous/next question, Tab add feedback"
  end
  return {
    lines = line_list,
    option_line = option_line,
    review_question_line = review_question_line,
    review_answer_line = review_answer_line,
    review_input_line = review_input_line,
    footer_line = footer_line,
    reviewing = review_item_list ~= nil,
  }
end

return M
