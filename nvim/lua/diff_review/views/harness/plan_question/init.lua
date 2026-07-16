local PlanQuestion = {}

local config = require("diff_review.infra.config")
local model = require("diff_review.views.harness.plan_question.model")
local picker = require("diff_review.views.picker")

local active_elicitation = nil
local active_host = nil

local function option_list(question, elicitation)
  local entries = model.entries(question, config.options.picker.choice_keys)
  local answer = model.answer_for(elicitation, question.id)
  local selected_index = model.selected_entry_index(entries, answer)
  local result = {}
  for _, entry in ipairs(entries) do
    result[#result + 1] = {
      key = entry.key,
      label = entry.label,
      detail = entry.description,
      value = entry.option,
      entry_kind = entry.kind,
      input_kind = entry.kind == "ask" and "ask" or entry.kind == "other" and "other" or nil,
    }
  end
  return result, selected_index, entries
end

local function question_page_list(elicitation)
  local question_set = elicitation.question_set or {}
  local page_list = {}
  for index, question in ipairs(question_set.questions or {}) do
    local options, selected_index, entries = option_list(question, elicitation)
    local answer = model.answer_for(elicitation, question.id)
    local selected_entry = entries[selected_index]
    page_list[#page_list + 1] = {
      id = question.id,
      title = question.header or question.question or "Planning question",
      subtitle = question.question ~= question.header and question.question or question.detail,
      option_list = options,
      selected_index = selected_index,
      initial_draft = selected_entry and model.input_text(selected_entry, answer) or "",
      allow_input = true,
      input_height = config.options.picker.input_height,
      footer = "←→ page  ↑↓ select  Enter confirm  Tab feedback",
    }
  end
  return page_list
end

local function review_page(elicitation)
  local content_list = {}
  for index, item in ipairs(model.review_item_list(elicitation)) do
    content_list[#content_list + 1] = {
      text = ("%d. %s"):format(index, item.question),
      group = "DiffReviewPickerQuestion",
    }
    content_list[#content_list + 1] = { text = item.answer, group = "DiffReviewPickerAnswer" }
    if item.additional_input then
      content_list[#content_list + 1] = { text = item.additional_input, group = "DiffReviewPickerText" }
    end
  end
  return {
    id = "review",
    title = ("Review Harness answers (%d)"):format(model.question_count(elicitation)),
    subtitle = "Confirm the answers that will continue planning.",
    content_list = content_list,
    option_list = {
      { key = "y", label = "Submit answers", value = "submit", confirm_on_key = true },
      { key = "n", label = "Revise answers", value = "revise", confirm_on_key = true },
    },
    footer = "y submit  n revise  q close",
  }
end

local function build_spec(elicitation, host)
  local question = model.current_question(elicitation)
  local pages = question and question_page_list(elicitation) or { review_page(elicitation) }
  local initial_page = question and math.min((elicitation.current_index or 0) + 1, #pages) or 1
  return {
    owner = "plan_question",
    host = {
      window_list = host.window_list or { host.transcript_win },
      control_win = host.control_win or host.transcript_win,
    },
    page_list = pages,
    initial_page = initial_page,
    on_confirm = function(result)
      if result.page.id == "review" then
        if result.option.value == "submit" then
          picker.close(false)
          host.continue()
        else
          active_elicitation.current_index = 0
          picker.update(build_spec(active_elicitation, host))
        end
        return false
      end
      local question_id = result.page.id
      local entry_kind = result.option.entry_kind
      if result.input_kind == "ask" then
        picker.close(false)
        host.ask({ question_id = question_id, text = result.text })
        return false
      end
      local response = entry_kind == "other"
          and { kind = "other", text = result.text }
        or {
          kind = "selected",
          option = result.option.value,
          feedback = result.text and result.text ~= "" and result.text or nil,
        }
      host.answer({ question_id = question_id, response = response }, function(next_elicitation)
        if not next_elicitation then
          picker.close(false)
          return
        end
        active_elicitation = vim.deepcopy(next_elicitation)
        picker.update(build_spec(active_elicitation, host))
      end)
      return false
    end,
    on_close = host.closed,
  }
end

---@param elicitation table
---@param host table
function PlanQuestion.open(elicitation, host)
  active_elicitation = vim.deepcopy(elicitation)
  active_host = host
  picker.open(build_spec(active_elicitation, active_host))
end

function PlanQuestion.is_open()
  return picker.is_open("plan_question")
end

function PlanQuestion.close()
  picker.close()
end

function PlanQuestion._state_for_test()
  return picker._state_for_test()
end

return PlanQuestion
