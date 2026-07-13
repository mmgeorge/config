local M = {}

local command_set = require("diff_review.shared.view_command_set")
local config = require("diff_review.infra.config")
local input_window = require("diff_review.views.harness.plan_question.input_window")
local keymaps = require("diff_review.shared.keymaps")
local model = require("diff_review.views.harness.plan_question.model")
local notifications = require("diff_review.infra.notifications")
local render = require("diff_review.views.harness.plan_question.render")
local window = require("diff_review.views.harness.plan_question.window")

---@class DiffReviewPlanQuestionHost
---@field transcript_win integer
---@field answer fun(params: table, callback: fun(elicitation?: table))
---@field skip fun(params: table, callback: fun(elicitation?: table))
---@field ask fun(params: table)
---@field continue fun()
---@field closed? fun()

---@class DiffReviewPlanQuestionViewState
---@field buf integer?
---@field win integer?
---@field elicitation table?
---@field host DiffReviewPlanQuestionHost?
---@field entry_list DiffReviewPlanQuestionEntry[]
---@field selected_index integer
---@field input_entry integer?
---@field input_kind "choice"|"other"|"ask"?
---@field input_buf integer?
---@field input_win integer?
---@field input_draft string
---@field input_draft_by_question table<string, string>
---@field frame DiffReviewPlanQuestionFrame?
---@field modal_keymap_list table[]

---@type DiffReviewPlanQuestionViewState
local state = {
  buf = nil,
  win = nil,
  elicitation = nil,
  host = nil,
  entry_list = {},
  selected_index = 1,
  input_entry = nil,
  input_kind = nil,
  input_buf = nil,
  input_win = nil,
  input_draft = "",
  input_draft_by_question = {},
  frame = nil,
  modal_keymap_list = {},
}

local function valid_window() return state.win and vim.api.nvim_win_is_valid(state.win) end

local function valid_input_window()
  return state.input_win and vim.api.nvim_win_is_valid(state.input_win)
end

local function current_question()
  return state.elicitation and model.current_question(state.elicitation) or nil
end

local function select_answered_entry()
  local question = current_question()
  if not (question and state.elicitation) then
    state.selected_index = 1
    return
  end
  state.selected_index = model.selected_entry_index(
    state.entry_list,
    model.answer_for(state.elicitation, question.id)
  )
end

local function title()
  local count = state.elicitation and model.question_count(state.elicitation) or 0
  local index = math.min((state.elicitation and state.elicitation.current_index or 0) + 1, count)
  if not current_question() then return ("Review Harness answers (%d)"):format(count) end
  return ("Harness question %d of %d"):format(index, count)
end

local close
local confirm_answers
local revise_answers

local function clear_modal_keymaps()
  for _, mapping in ipairs(state.modal_keymap_list) do
    if vim.api.nvim_buf_is_valid(mapping.buf) then
      pcall(vim.keymap.del, "n", mapping.key, { buffer = mapping.buf })
      if mapping.previous and next(mapping.previous) then
        vim.api.nvim_buf_call(mapping.buf, function() vim.fn.mapset("n", false, mapping.previous) end)
      end
    end
  end
  state.modal_keymap_list = {}
end

local function preserve_modal_keymap(key)
  if vim.iter(state.modal_keymap_list):any(function(mapping) return mapping.key == key end) then return end
  local transcript_buf = vim.api.nvim_win_get_buf(state.host.transcript_win)
  local previous = vim.api.nvim_buf_call(transcript_buf, function()
    return vim.fn.maparg(key, "n", false, true)
  end)
  state.modal_keymap_list[#state.modal_keymap_list + 1] = {
    buf = transcript_buf,
    key = key,
    previous = previous,
  }
end

local function render_view()
  if not (state.buf and vim.api.nvim_buf_is_valid(state.buf) and valid_window()) then return end
  local question = current_question()
  local width = vim.api.nvim_win_get_width(state.win)
  state.entry_list = question and model.entries(question, config.options.harness.question_choice_keys) or {}
  state.selected_index = math.max(1, math.min(state.selected_index, math.max(1, #state.entry_list)))
  local review_item_list = not question and state.elicitation and model.review_item_list(state.elicitation) or nil
  state.frame = render.build(question, state.entry_list, width, review_item_list)
  local parent_height = vim.api.nvim_win_get_height(state.host.transcript_win)
  local height = math.min(#state.frame.lines, math.max(6, parent_height - 10))
  window.resize(state.win, state.host.transcript_win, width, height)
  window.render(state.buf, state.frame, state.selected_index, state.entry_list)
  window.set_cursor_hidden(state.win, true)
  window.set_focusable(state.win, false)
  window.set_title(state.win, title())
  if vim.api.nvim_get_current_win() == state.win then
    vim.api.nvim_set_current_win(state.host.transcript_win)
  end
end

local function input_title()
  local entry = state.entry_list[state.input_entry or state.selected_index]
  if state.input_kind == "ask" then return "Ask" end
  if state.input_kind == "other" then return "Other answer" end
  return entry and ("Feedback for " .. entry.label) or "Feedback"
end

local function update_input_target()
  if not valid_input_window() then return end
  local entry = state.entry_list[state.selected_index]
  if not entry then return end
  state.input_entry = state.selected_index
  state.input_kind = entry.kind == "ask" and "ask" or entry.kind == "other" and "other" or "choice"
  input_window.retarget(state.input_win, state.win, input_title())
end

local function restore_current_input_draft()
  if not (valid_input_window() and state.input_buf) then return end
  local question = current_question()
  local entry = state.entry_list[state.selected_index]
  if not (question and entry and state.elicitation) then return end
  local answer = model.answer_for(state.elicitation, question.id)
  state.input_draft = state.input_draft_by_question[question.id] or model.input_text(entry, answer)
  input_window.set_text(state.input_buf, state.input_draft)
  update_input_target()
end

close = function(notify_host)
  local host = state.host
  clear_modal_keymaps()
  input_window.close(state.input_win)
  if valid_window() then vim.api.nvim_win_close(state.win, true) end
  state.buf = nil
  state.win = nil
  state.elicitation = nil
  state.host = nil
  state.entry_list = {}
  state.input_entry = nil
  state.input_kind = nil
  state.input_buf = nil
  state.input_win = nil
  state.input_draft = ""
  state.input_draft_by_question = {}
  state.frame = nil
  state.modal_keymap_list = {}
  if notify_host and host and host.closed then host.closed() end
end

local function apply_elicitation(elicitation)
  if not elicitation then
    close(false)
    return
  end
  local answered_question = current_question()
  if answered_question then state.input_draft_by_question[answered_question.id] = nil end
  input_window.close(state.input_win)
  state.elicitation = vim.deepcopy(elicitation)
  state.input_entry = nil
  state.input_kind = nil
  state.input_buf = nil
  state.input_win = nil
  state.input_draft = ""
  local question = current_question()
  state.entry_list = question and model.entries(question, config.options.harness.question_choice_keys) or {}
  select_answered_entry()
  render_view()
end

local function move(delta)
  if #state.entry_list == 0 then return end
  state.selected_index = ((state.selected_index - 1 + delta) % #state.entry_list) + 1
  render_view()
  update_input_target()
end

local function move_question(delta)
  if not state.elicitation then return end
  local previous_question = current_question()
  if valid_input_window() and previous_question then
    state.input_draft_by_question[previous_question.id] = input_window.text(state.input_buf)
  end
  local count = model.question_count(state.elicitation)
  if count == 0 or not previous_question then return end
  local current_index = state.elicitation.current_index or 0
  state.elicitation.current_index = math.max(0, math.min(count - 1, current_index + delta))
  local question = current_question()
  state.entry_list = question and model.entries(question, config.options.harness.question_choice_keys) or {}
  select_answered_entry()
  render_view()
  restore_current_input_draft()
end

local function focus_options()
  if not valid_window() then return end
  if valid_input_window() then
    state.input_draft = input_window.text(state.input_buf)
    local question = current_question()
    if question then state.input_draft_by_question[question.id] = state.input_draft end
  end
  vim.cmd("stopinsert")
  vim.api.nvim_set_current_win(state.host.transcript_win)
end

local function focus_input()
  if not valid_input_window() then return end
  vim.cmd("stopinsert")
  vim.api.nvim_set_current_win(state.input_win)
end

local function edit_input()
  if not valid_input_window() then return end
  vim.api.nvim_set_current_win(state.input_win)
  vim.cmd("startinsert!")
end

local function commit_input()
  local text = state.input_buf and input_window.text(state.input_buf) or ""
  local entry = state.entry_list[state.input_entry or 0]
  local question = current_question()
  local host = state.host
  if not (entry and question and host) then return end
  if (state.input_kind == "other" or state.input_kind == "ask") and text == "" then
    notifications.warn("Enter a value before submitting this Harness option.", "Harness")
    return
  end
  vim.cmd("stopinsert")
  if state.input_kind == "ask" then
    local params = { question_id = question.id, text = text }
    close(false)
    host.ask(params)
    return
  end
  local response = state.input_kind == "other"
      and { kind = "other", text = text }
      or { kind = "selected", option = entry.option, feedback = text ~= "" and text or nil }
  host.answer({ question_id = question.id, response = response }, apply_elicitation)
end

local function install_input_keymaps()
  if not state.input_buf then return end
  for _, key in ipairs(keymaps.view_keys_for("plan_question", "submit_input")) do
    vim.keymap.set({ "n", "i" }, key, commit_input, {
      buffer = state.input_buf,
      silent = true,
      nowait = true,
      desc = "Submit Harness question input",
    })
  end
  for _, key in ipairs(keymaps.view_keys_for("plan_question", "focus_input")) do
    vim.keymap.set("n", key, focus_options, {
      buffer = state.input_buf,
      silent = true,
      nowait = true,
      desc = "Return to Harness question options",
    })
  end
end

local function begin_input(entry_index, kind)
  if not (state.buf and valid_window() and state.host) then return end
  state.input_entry = entry_index
  state.input_kind = kind
  if not valid_input_window() then
    local question = current_question()
    local entry = state.entry_list[entry_index]
    local answer = question and state.elicitation and model.answer_for(state.elicitation, question.id) or nil
    state.input_draft = question and state.input_draft_by_question[question.id]
      or (entry and model.input_text(entry, answer) or "")
    state.input_buf, state.input_win = input_window.open(
      state.host.transcript_win,
      state.win,
      input_title(),
      state.input_draft
    )
    install_input_keymaps()
  else
    input_window.retarget(state.input_win, state.win, input_title())
  end
  edit_input()
end

local function select(with_feedback)
  local entry = state.entry_list[state.selected_index]
  local question = current_question()
  local host = state.host
  if not (entry and question and host) then return end
  if valid_input_window() then
    update_input_target()
    edit_input()
    return
  end
  if entry.kind == "ask" then
    begin_input(state.selected_index, "ask")
  elseif entry.kind == "other" then
    begin_input(state.selected_index, "other")
  elseif with_feedback then
    begin_input(state.selected_index, "choice")
  else
    host.answer({
      question_id = question.id,
      response = { kind = "selected", option = entry.option, feedback = nil },
    }, apply_elicitation)
  end
end

confirm_answers = function()
  if current_question() then return end
  local host = state.host
  if not host then return end
  close(false)
  host.continue()
end

revise_answers = function()
  if current_question() or not state.elicitation then return end
  state.elicitation.current_index = 0
  local question = current_question()
  if not question then return end
  state.entry_list = model.entries(question, config.options.harness.question_choice_keys)
  select_answered_entry()
  render_view()
end

---@param key string
---@return boolean
local function run_review_action(key)
  if current_question() then return false end
  if vim.tbl_contains(keymaps.view_keys_for("plan_question", "confirm"), key) then
    confirm_answers()
    return true
  end
  if vim.tbl_contains(keymaps.view_keys_for("plan_question", "revise"), key) then
    revise_answers()
    return true
  end
  return false
end

local function install_keymaps()
  local set = command_set.new()
  command_set.register(set, "previous", function() move(-1) end)
  command_set.register(set, "next", function() move(1) end)
  command_set.register(set, "select", function() select(false) end)
  command_set.register(set, "feedback", function() select(true) end)
  command_set.register(set, "question_previous", function() move_question(-1) end)
  command_set.register(set, "question_next", function() move_question(1) end)
  command_set.register(set, "focus_input", focus_input)
  command_set.register(set, "confirm", confirm_answers)
  command_set.register(set, "revise", revise_answers)
  command_set.register(set, "close", function() close(true) end)
  local transcript_buf = vim.api.nvim_win_get_buf(state.host.transcript_win)
  for _, command_id in ipairs(set.order) do
    for _, key in ipairs(keymaps.view_keys_for("plan_question", command_id)) do preserve_modal_keymap(key) end
  end
  keymaps.setup_view_keymaps(transcript_buf, "plan_question", set)
  for index, key in ipairs(config.options.harness.question_choice_keys) do
    preserve_modal_keymap(key)
    vim.keymap.set("n", key, function()
      if run_review_action(key) then return end
      if state.entry_list[index] and state.entry_list[index].kind ~= "ask" then
        state.selected_index = index
        render_view()
        update_input_target()
      end
    end, {
      buffer = transcript_buf,
      silent = true,
      nowait = true,
      desc = "Select planning option " .. index,
    })
  end
  preserve_modal_keymap("o")
  vim.keymap.set("n", "o", function()
    for index, entry in ipairs(state.entry_list) do
      if entry.kind == "other" then
        state.selected_index = index
        render_view()
        update_input_target()
        return
      end
    end
  end, {
    buffer = transcript_buf,
    silent = true,
    nowait = true,
    desc = "Select Other planning option",
  })
  preserve_modal_keymap("a")
  vim.keymap.set("n", "a", function()
    if run_review_action("a") then return end
    for index, entry in ipairs(state.entry_list) do
      if entry.kind == "ask" then
        state.selected_index = index
        render_view()
        update_input_target()
        return
      end
    end
  end, {
    buffer = transcript_buf,
    silent = true,
    nowait = true,
    desc = "Ask about this planning question",
  })
end

---@param elicitation table
---@param host DiffReviewPlanQuestionHost
function M.open(elicitation, host)
  close(false)
  state.elicitation = vim.deepcopy(elicitation)
  state.host = host
  state.input_draft_by_question = {}
  local parent_width = vim.api.nvim_win_get_width(host.transcript_win)
  local width = math.max(48, math.min(78, parent_width - 8))
  local question = current_question()
  state.entry_list = question and model.entries(question, config.options.harness.question_choice_keys) or {}
  select_answered_entry()
  local review_item_list = not question and model.review_item_list(state.elicitation) or nil
  local initial_frame = render.build(question, state.entry_list, width, review_item_list)
  local parent_height = vim.api.nvim_win_get_height(host.transcript_win)
  local height = math.min(#initial_frame.lines, math.max(6, parent_height - 10))
  state.buf, state.win = window.open(host.transcript_win, width, height, title())
  vim.cmd("stopinsert")
  render_view()
  install_keymaps()
end

---@return boolean
function M.is_open() return valid_window() == true end

---@return table?
function M._state_for_test() return state end

return M
