local M = {}

---@class DiffReviewPlanQuestionEntry
---@field kind "choice"|"other"|"ask"
---@field label string
---@field description string
---@field key string?
---@field option string?

---@class DiffReviewPlanQuestionReviewItem
---@field question string
---@field answer string
---@field additional_input string?

---@param question table
---@param choice_keys string[]
---@return DiffReviewPlanQuestionEntry[]
function M.entries(question, choice_keys)
  local entry_list = {}
  for index, option in ipairs(question.options or {}) do
    entry_list[#entry_list + 1] = {
      kind = "choice",
      label = option.label or "[unnamed option]",
      description = option.description or "",
      key = choice_keys[index],
      option = option.label,
    }
  end
  if question.allow_freeform ~= false then
    entry_list[#entry_list + 1] = {
      kind = "other",
      label = "Other…",
      description = "Enter a custom answer.",
      key = "o",
    }
  end
  entry_list[#entry_list + 1] = {
    kind = "ask",
    label = "Ask…",
    description = "Ask a question before deciding.",
    key = "a",
  }
  return entry_list
end

---@param elicitation table
---@return table?
function M.current_question(elicitation)
  local question_list = elicitation.question_set and elicitation.question_set.questions or {}
  return question_list[(elicitation.current_index or 0) + 1]
end

---@param elicitation table
---@return integer
function M.question_count(elicitation)
  return #(elicitation.question_set and elicitation.question_set.questions or {})
end

---@param elicitation table
---@param question_id string
---@return table?
function M.answer_for(elicitation, question_id)
  for _, answer in ipairs(elicitation.answer or {}) do
    if answer.question_id == question_id then return answer end
  end
end

---@param entry_list DiffReviewPlanQuestionEntry[]
---@param answer table?
---@return integer
function M.selected_entry_index(entry_list, answer)
  local response = answer and answer.response or nil
  if not response then return 1 end
  for index, entry in ipairs(entry_list) do
    if response.kind == "selected" and entry.kind == "choice" and entry.option == response.option then
      return index
    end
    if response.kind == "other" and entry.kind == "other" then return index end
  end
  return 1
end

---@param entry DiffReviewPlanQuestionEntry
---@param answer table?
---@return string
function M.input_text(entry, answer)
  local response = answer and answer.response or nil
  if not response then return "" end
  if entry.kind == "choice" and response.kind == "selected" and entry.option == response.option then
    return response.feedback or ""
  end
  if entry.kind == "other" and response.kind == "other" then return response.text or "" end
  return ""
end

---@param elicitation table
---@return DiffReviewPlanQuestionReviewItem[]
function M.review_item_list(elicitation)
  local item_list = {}
  local question_list = elicitation.question_set and elicitation.question_set.questions or {}
  for _, question in ipairs(question_list) do
    local answer = M.answer_for(elicitation, question.id)
    local response = answer and answer.response or nil
    local answer_text = "Unanswered"
    local additional_input = nil
    if response and response.kind == "selected" then
      answer_text = response.option or "[unnamed option]"
      additional_input = response.feedback
    elseif response and response.kind == "other" then
      answer_text = "Other"
      additional_input = response.text
    elseif response and response.kind == "skipped" then
      answer_text = "Unanswered"
    end
    item_list[#item_list + 1] = {
      question = question.question or question.header or "Planning question",
      answer = answer_text,
      additional_input = additional_input ~= "" and additional_input or nil,
    }
  end
  return item_list
end

return M
