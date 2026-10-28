local PlanQuestion = {}

local config = require("forge.infra.config")
local model = require("forge.views.harness.plan_question.model")
local picker = require("forge.views.picker")

local active_elicitation = nil
local active_host = nil
local active_submission = nil

local function continue_if_ready(submission, host)
  if submission.continue_requested and not submission.continued
    and submission.replica.pending() == 0 and not next(submission.failed) then
    submission.continued = true
    host.continue()
  end
end

local function project_answer(elicitation, intent)
  elicitation.answer = vim.tbl_filter(function(answer) return answer.question_id ~= intent.question_id end, elicitation.answer or {})
  elicitation.answer[#elicitation.answer + 1] = { question_id = intent.question_id, response = vim.deepcopy(intent.response), source = "user" }
  elicitation.current_index = #(elicitation.question_set.questions or {})
  for index, question in ipairs(elicitation.question_set.questions or {}) do
    if not model.answer_for(elicitation, question.id) then elicitation.current_index = index - 1 break end
  end
end

local function option_list(question, elicitation, allow_ask)
  local entries = model.entries(question, config.options.picker.choice_keys, allow_ask)
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

local function question_page_list(elicitation, host)
  local question_set = elicitation.question_set or {}
  local page_list = {}
  for index, question in ipairs(question_set.questions or {}) do
    local options, selected_index, entries = option_list(question, elicitation, host.allow_ask)
    local draft = host.clarification_draft
    if draft and draft.question_id == question.id then
      for entry_index, entry in ipairs(entries) do
        if entry.kind == "ask" then selected_index = entry_index break end
      end
    end
    local answer = model.answer_for(elicitation, question.id)
    local selected_entry = entries[selected_index]
    local operation = active_submission and active_submission.by_question[question.id]
    local failure = operation and active_submission.failed[operation]
    page_list[#page_list + 1] = {
      id = question.id,
      title = question.header or question.question or "Answer Question",
      subtitle = question.question ~= question.header and question.question or question.detail,
      column_headers = { "Answer", "Description" },
      flexible_column = 2,
      wrap_column = 2,
      option_list = options,
      selected_index = selected_index,
      initial_draft = draft and draft.question_id == question.id and draft.text
        or selected_entry and model.input_text(selected_entry, answer) or "",
      allow_input = true,
      input_height = config.options.picker.input_height,
      footer = failure and ("Answer not saved: " .. failure .. " · Enter retry")
        or "←→ page  ↑↓ select  Enter confirm  Tab feedback",
    }
  end
  return page_list
end

local function review_page(elicitation)
  local content_list = {}
  for index, item in ipairs(model.review_item_list(elicitation)) do
    content_list[#content_list + 1] = {
      text = ("%d. %s"):format(index, item.question),
      group = "ForgePickerQuestion",
    }
    content_list[#content_list + 1] = { text = item.answer, group = "ForgePickerAnswer" }
    if item.additional_input then
      content_list[#content_list + 1] = { text = item.additional_input, group = "ForgePickerText" }
    end
  end
  return {
    id = "review",
    title = ("Review Answers (%d)"):format(model.question_count(elicitation)),
    content_list = content_list,
    option_list = {
      { key = "y", label = "Submit answers", value = "submit", confirm_on_key = true },
      { key = "n", label = "Revise answers", value = "revise", confirm_on_key = true },
    },
    column_headers = { "Action" },
    footer = "y submit  n revise  q close",
  }
end

local function build_spec(elicitation, host)
  local submission = active_submission
  local question = model.current_question(elicitation)
  local pages = question and question_page_list(elicitation, host) or { review_page(elicitation) }
  local initial_page = question and math.min((elicitation.current_index or 0) + 1, #pages) or 1
  if host.clarification_draft then
    for index, page in ipairs(pages) do
      if page.id == host.clarification_draft.question_id then initial_page = index break end
    end
  end
  return {
    owner = "plan_question",
    deadline = elicitation.deadline,
    host = host.picker_host or {
      window_list = host.window_list or { host.transcript_win },
      control_win = host.control_win or host.transcript_win,
    },
    page_list = pages,
    initial_page = initial_page,
    initial_input_kind = host.clarification_draft and "ask" or nil,
    on_confirm = function(result)
      if not active_elicitation then return false end
      if result.page.id == "review" then
        if result.option.value == "submit" then
          if next(submission.failed) then
            active_elicitation.current_index = 0
            picker.update(build_spec(active_elicitation, host))
            return false
          end
          picker.close(false)
          submission.continue_requested = true
          if submission.replica.pending() == 0 then continue_if_ready(submission, host)
          elseif host.pending then host.pending() end
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
      local identity = require("forge.command").identity()
      local previous = submission.by_question[question_id]
      if previous then
        submission.replica.remove(previous)
        submission.failed[previous] = nil
      end
      local intent = { question_id = question_id, response = response, operation_id = identity }
      submission.by_question[question_id] = identity
      submission.replica.begin(identity, intent)
      active_elicitation = submission.replica.view()
      picker.update(build_spec(active_elicitation, host))
      local completed = false
      host.answer(intent, function(next_elicitation, failure)
        if active_submission ~= submission or completed then return end
        completed = true
        if failure and submission.by_question[question_id] ~= identity then
          submission.replica.remove(identity)
          continue_if_ready(submission, host)
          return
        elseif failure then
          submission.failed[identity] = failure
          if submission.continue_requested then
            submission.continue_requested = false
            active_elicitation.current_index = 0
            picker.open(build_spec(active_elicitation, host))
          elseif picker.is_open("plan_question") then
            picker.update(build_spec(active_elicitation, host))
          end
          return
        end
        submission.replica.remove(identity)
        if not next_elicitation then
          picker.close(false)
          return
        end
        submission.revision = math.max(submission.revision + 1, next_elicitation.answer_revision or 0)
        submission.replica.replace(next_elicitation, next_elicitation.answer_revision or submission.revision)
        active_elicitation = submission.replica.view()
        if submission.continue_requested then
          continue_if_ready(submission, host)
        elseif picker.is_open() then picker.update(build_spec(active_elicitation, host)) end
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
  active_submission = { replica = require("forge.replica").new(elicitation, elicitation.answer_revision or 0, project_answer),
    revision = elicitation.answer_revision or 0, by_question = {}, failed = {} }
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
