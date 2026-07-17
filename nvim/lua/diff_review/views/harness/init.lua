local M = {}

local client = require("diff_review.harness.client")
local controller = require("diff_review.views.harness.controller")
local layout = require("diff_review.views.harness.layout")
local notifications = require("diff_review.infra.notifications")
local session = require("diff_review.session")
local snapshot = require("diff_review.views.harness.snapshot")
local picker = require("diff_review.views.picker")

local function valid_window(win) return win and vim.api.nvim_win_is_valid(win) end

local function apply_snapshot(state, result)
  snapshot.apply(state, result, "reconcile")
  controller.render(true)
  controller.resolve_runtime_model()
  if state.active_elicitation and state.active_elicitation.elicitation then
    state.presented_question_key = nil
    vim.schedule(controller.present_plan_question)
  end
  if #state.approval > 0 then vim.schedule(controller.present_approval) end
  if state.goal and state.goal.state == "active" then
    vim.schedule(controller.drain)
  end
end

---@param conflict table
---@return DiffReviewChoicePopupOption[]
function M.lease_conflict_options(conflict)
  local option_list = {
    { key = "n", value = "new", label = "Start new session" },
    { key = "r", value = "retry", label = "Retry session" },
  }
  if conflict.native_fork == true then
    table.insert(option_list, 1, { key = "f", value = "fork", label = "Fork session" })
  end
  return option_list
end

local function finish_start(state, result, start_error, error_detail)
  local conflict = error_detail and error_detail.code == "session_lease_conflict" and error_detail.data or nil
  if conflict then
    local option_list = M.lease_conflict_options(conflict)
    for _, option in ipairs(option_list) do option.detail = option.desc end
    picker.open({
      host = {
        window_list = { state.transcript_win, state.composer_win },
        control_win = state.composer_win,
      },
      page_list = {
        {
          id = "lease-conflict",
          title = "Session in use",
          subtitle = "Another Neovim instance controls this Harness session.",
          option_list = option_list,
          footer = "↑↓ select  Enter confirm  q close",
        },
      },
      on_confirm = function(result)
        local action = result.option.value
        client.resolve_lease_conflict(action, conflict, function(next_result, next_error, next_detail)
          finish_start(state, next_result, next_error, next_detail)
        end)
      end,
    })
    return
  end
  if start_error then
    notifications.error(start_error, "Harness")
    controller.render()
    return
  end
  apply_snapshot(state, result)
end

function M.open()
  local state = session.harness
  if valid_window(state.composer_win) then
    vim.api.nvim_set_current_win(state.composer_win)
    if state.goal and state.goal.state == "active" then
      vim.schedule(controller.drain)
    end
    return
  end
  state.transcript_buf, state.transcript_win, state.composer_buf, state.composer_win = layout.open()
  layout.attach_auto_height(state.composer_buf, state.composer_win)
  layout.attach_scroll_boundary(state.transcript_buf, state.transcript_win)
  controller.attach()
  controller.render(true)
  client.start(function(result, start_error, error_detail) finish_start(state, result, start_error, error_detail) end)
  vim.api.nvim_set_current_win(state.composer_win)
end

function M.new_session()
  M.open()
  client.request("session.new", {}, function(result, request_error)
    if request_error then notifications.error(request_error, "Harness") return end
    local state = session.harness
    snapshot.apply(state, result)
    state.queue = {}
    controller.render(true)
  end)
end

return M
