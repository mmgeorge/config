local M = {}

local picker = require("forge.views.picker")
local config = require("forge.infra.config")
local client = require("forge.client")

---@class ForgeQuestionView
---@field revision integer
---@field questions table[]

local active_state
local active_host
local build_spec

---@param state ForgeHarnessPresentationState
---@return integer, integer
function M.count(state)
  local pending, required = 0, 0
  for _, question in ipairs((state.questions or {}).questions or {}) do
    if question.resolution.kind == "pending" then
      pending = pending + 1
      if question.requirement == "required" then required = required + 1 end
    end
  end
  return pending, required
end

---True while the host's reply to the user's own answer is applied. A picker closed by that reply
---follows the user's decision, so the keys typed after it are not discarded.
local applying_answer_reply = false

---Refresh the owning question picker, closing completed answering sessions.
---@param state ForgeHarnessPresentationState
local function refresh_picker(state, question_id)
  if not picker.is_open("questions") or active_state ~= state then return end
  if M.count(state) == 0 and not state.question_show_history then
    picker.close(true, applying_answer_reply and "user" or nil)
  else
    local spec = build_spec(state, active_host)
    if question_id then
      for index, entry in ipairs(spec.page_list) do
        if entry.id == question_id then spec.initial_page = index break end
      end
    end
    picker.update(spec)
  end
end

---Apply the host's reply to the user's own answer, which may close the picker.
---@param state ForgeHarnessPresentationState
---@param result ForgeQuestionView
local function apply_answer_reply(state, result)
  applying_answer_reply = true
  local applied, failure = pcall(function()
    if not M.receive(state, result) then refresh_picker(state) end
  end)
  applying_answer_reply = false
  if not applied then error(failure, 0) end
end

local function submit(state, question, action)
  state.question_saving = state.question_saving or {}
  if state.question_saving[question.id] then return end
  local session_id = state.session.id
  local operation = require("forge.command").identity()
  state.question_saving[question.id] = operation
  state.question_failure = nil
  local function receive(result, failure)
    if not state.session or state.session.id ~= session_id then return end
    if state.question_saving[question.id] ~= operation then return end
    state.question_saving[question.id] = nil
    if failure then
      state.question_failure = failure
      require("forge.infra.notifications").error(failure, "Harness question")
      refresh_picker(state)
    else
      apply_answer_reply(state, result)
    end
  end
  if action.kind == "clarify" then
    require("forge.views.harness.input_state").submit(state, action.text, {}, "auto", nil,
      function(failure) receive(nil, failure) end, nil,
      { owner = "question", question_id = question.id, revision = question.content.revision })
  else
    client.request({ session_id = session_id, method = "session.question.answer", params = {
      command = { operation_id = operation, question_id = question.id, revision = question.content.revision, action = action },
    } }, receive)
  end
end

---@class ForgeQuestionAnswerState
---@field answers table<string, {revision: integer, action: table}>
---@field request? table
---@field saving? boolean
---@field failure? string

---Keep one session's unsent decisions separate from authoritative resolutions.
---@param state ForgeHarnessPresentationState
---@return ForgeQuestionAnswerState
local function answer_state(state)
  state.question_answer_state = state.question_answer_state or { answers = {} }
  return state.question_answer_state
end

---Submit exactly once when an explicit confirmation completes the pending set.
local function stage_answer(state, question, action)
  local drafts = answer_state(state)
  if drafts.saving then return end
  local entry = { revision = question.content.revision, action = action }
  if not vim.deep_equal(drafts.answers[question.id], entry) then drafts.request = nil end
  drafts.answers[question.id] = entry
  drafts.failure = nil
  local answers, pending = {}, {}
  for _, candidate in ipairs((state.questions or {}).questions or {}) do
    if candidate.resolution.kind == "pending" then
      pending[#pending + 1] = candidate
      local draft = drafts.answers[candidate.id]
      if draft and draft.revision == candidate.content.revision then
        answers[#answers + 1] = { question_id = candidate.id, revision = draft.revision, action = draft.action }
      end
    end
  end
  if #answers ~= #pending then
    local current_index = 1
    for index, candidate in ipairs(pending) do if candidate.id == question.id then current_index = index break end end
    for offset = 1, #pending do
      local candidate = pending[(current_index + offset - 1) % #pending + 1]
      if not drafts.answers[candidate.id] then refresh_picker(state, candidate.id) return end
    end
    return
  end
  drafts.request = drafts.request or { operation_id = require("forge.command").identity(), answers = answers }
  local request = drafts.request
  local session_id = state.session.id
  drafts.saving = true
  -- The last answer is the user's decision, so the picker closes with it and takes no further
  -- keys: those typed right after the submit key reach the window and mode it returns to. A
  -- failed save keeps the drafts, and reopening the questions shows the failure beside them.
  if picker.is_open("questions") and active_state == state then picker.close(true, "user") end
  client.request({ session_id = session_id, method = "session.question.answer_batch", params = request }, function(result, failure)
    drafts.saving = false
    if failure then drafts.failure = failure end
    if not state.session or state.session.id ~= session_id or state.question_answer_state ~= drafts then return end
    if failure then
      local reopen_key = require("forge.shared.keymaps").view_keys_for("harness", "reopen_question")[1]
      require("forge.infra.notifications").error(failure .. (reopen_key and (" · " .. reopen_key .. " reopen") or ""),
        "Harness questions")
      refresh_picker(state)
    else
      drafts.request = nil
      apply_answer_reply(state, result)
    end
  end)
end

local function page(state, question)
  local required = question.requirement == "required"
  local pending = question.resolution.kind == "pending"
  local content = question.content
  local drafts = answer_state(state)
  local draft = drafts.answers[question.id]
  local title = content.header .. " · " .. (required and "Required" or "Optional")
  local options = {}
  if pending then
    for index, option in ipairs(content.options or {}) do
      options[#options + 1] = { id = option.id, key = config.options.picker.choice_keys[index],
        label = option.label, detail = option.description, value = option.id }
    end
    options[#options + 1] = { id = "other", key = "o", label = "Other…", detail = "Enter a custom answer.", input_kind = "other" }
    options[#options + 1] = { id = "ask", key = "a", label = "Ask…", detail = "Ask for clarification without answering.", input_kind = "ask" }
  elseif question.resolution.kind == "answered" then
    options[#options + 1] = { id = "correct", key = "c", label = "Correct answer…", detail = "Send a correction; preserve the earlier answer.", input_kind = "other" }
  end
  local details = { { text = title, group = "ForgePickerQuestion" } }
  if required and pending then
    local _, required_count = M.count(state)
    details[#details + 1] = { text = required_count > 1
      and "Work is paused until all required questions are answered or withdrawn."
      or "Work is paused until this question is answered or withdrawn.", group = "ForgePickerHint" }
  end
  details[#details + 1] = { text = content.question, group = "ForgePickerText" }
  for _, message in ipairs(question.discussion or {}) do
    local label = ({ clarification = "You asked", reply = "Explanation", answer = "You answered", correction = "You corrected" })[message.kind]
    details[#details + 1] = { text = (label or message.kind) .. ": " .. message.text, group = "ForgePickerText" }
  end
  if pending then
    details[#details + 1] = { text = "Answers stay editable until every pending question is answered or ignored.", group = "ForgePickerHint" }
  end
  if draft then
    local label = "Ignored"
    if draft.action.kind == "answer" then
      local answer = draft.action.answer
      label = answer.text or answer.option_id
      for _, option in ipairs(content.options or {}) do if option.id == answer.option_id then label = option.label break end end
      if answer.feedback then label = label .. " — " .. answer.feedback end
    end
    details[#details + 1] = { text = "Draft answer: " .. label, group = "ForgePickerHint" }
  elseif pending then
    -- Text typed into "Other…" and not yet confirmed is still this question's answer in progress.
    local typed = vim.trim(((state.question_drafts or {})[question.id] or {}).other or "")
    if typed ~= "" then
      details[#details + 1] = { text = "Draft answer: Other… — " .. typed, group = "ForgePickerHint" }
    end
  end
  if drafts.failure then details[#details + 1] = { text = "Not saved: " .. drafts.failure, group = "ForgeHarnessToolFailure" } end
  if drafts.saving then details[#details + 1] = { text = "Saving answers…", group = "ForgePickerHint" } end
  if state.question_failure then details[#details + 1] = { text = "Not saved: " .. state.question_failure, group = "ForgeHarnessToolFailure" } end
  if (state.question_saving or {})[question.id] then details[#details + 1] = { text = "Saving…", group = "ForgePickerHint" } end
  return { id = question.id, title = title,
    confirm_description = pending and "Save draft and advance; submit all when every question is answered or ignored" or nil,
    content_list = details, option_list = options, column_headers = { "Answer", "Description" },
    flexible_column = 2, wrap_column = 2, allow_input = true,
    input_height = config.options.picker.input_height,
    action_list = pending and not required and {
      { id = "dismiss", key = "d", label = "ignore", desc = "Ignore optional question", show_in_help_footer = true,
        callback = function() stage_answer(state, question, { kind = "dismiss" }) end },
    } or {},
  }
end

build_spec = function(state, host)
  local pages, by_id = {}, {}
  for _, question in ipairs((state.questions or {}).questions or {}) do
    if question.resolution.kind == "pending" or state.question_show_history then
      pages[#pages + 1] = page(state, question)
      by_id[question.id] = question
    end
  end
  if #pages == 0 then pages = { { id = "empty", title = "Questions", content_list = { { text = "No questions need answers.", group = "ForgePickerHint" } }, option_list = {} } } end
  return { owner = "questions", title = "Questions", host = host, page_list = pages,
    page_navigation = "tabs", wrap_pages = true, hint_mode = "help", preserve_input = true,
    input_settle_ms = picker.focus_taking_settle_ms,
    keymaps = { page_next = { "<Tab>", "<Right>", "]" }, page_previous = { "<S-Tab>", "<Left>", "[" }, feedback = "<C-f>" },
    resume_state = state.question_picker_resume,
    read_input_draft = function(id, kind) return ((state.question_drafts or {})[id] or {})[kind] or "" end,
    save_input_draft = function(id, kind, text)
      state.question_drafts = state.question_drafts or {}
      state.question_drafts[id] = state.question_drafts[id] or {}
      state.question_drafts[id][kind] = text
    end,
    action_list = { { id = "history", key = "H", desc = "Toggle question history", callback = function()
      state.question_show_history = not state.question_show_history
      refresh_picker(state)
    end } },
    on_confirm = function(result)
      local question = by_id[result.page.id]
      if not question or answer_state(state).saving then return false end
      result.text = result.text or ""
      local action
      if result.option.id == "correct" then action = { kind = "correct", text = result.text }
      elseif result.input_kind == "ask" then action = { kind = "clarify", text = result.text }
      elseif result.option.id == "other" then action = { kind = "answer", answer = { kind = "other", text = result.text } }
      else action = { kind = "answer", answer = { kind = "selected", option_id = result.option.value, feedback = result.text ~= "" and result.text or nil } } end
      if action.kind == "answer" then stage_answer(state, question, action)
      else submit(state, question, action) end
      return false
    end,
    on_close = function(resume) state.question_picker_resume = resume end,
  }
end

---@param state ForgeHarnessPresentationState
---@param host table
function M.open(state, host, question_id)
  if M.count(state) == 0 and not state.question_show_history then return end
  active_state, active_host = state, host
  local spec = build_spec(state, host)
  if question_id then
    for index, entry in ipairs(spec.page_list) do
      if entry.id == question_id then spec.initial_page = index break end
    end
  end
  picker.open(spec)
end

---@param state ForgeHarnessPresentationState
---@param snapshot ForgeQuestionView
---@return boolean content_changed
function M.receive(state, snapshot)
  if not snapshot or snapshot == vim.NIL then return false end
  if state.questions and (state.questions.revision or 0) >= (snapshot.revision or 0) then return false end
  local content_changed = not state.questions or not vim.deep_equal(state.questions.questions, snapshot.questions)
  state.questions = snapshot
  local drafts = answer_state(state)
  local current = {}
  for _, question in ipairs(snapshot.questions or {}) do current[question.id] = question end
  for id, draft in pairs(drafts.answers) do
    local question = current[id]
    if not question or question.resolution.kind ~= "pending" or question.content.revision ~= draft.revision then
      drafts.answers[id] = nil
      drafts.request = nil
    end
  end
  if content_changed then refresh_picker(state) end
  return content_changed
end

return M
