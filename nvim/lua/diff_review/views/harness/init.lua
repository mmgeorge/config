local M = {}

local client = require("diff_review.harness.client")
local controller = require("diff_review.views.harness.controller")
local layout = require("diff_review.views.harness.layout")
local notifications = require("diff_review.infra.notifications")
local session = require("diff_review.session")
local transcript_state = require("diff_review.views.harness.transcript_state")

local function valid_window(win) return win and vim.api.nvim_win_is_valid(win) end

function M.open()
  local state = session.harness
  if valid_window(state.composer_win) then
    vim.api.nvim_set_current_win(state.composer_win)
    if state.active_plan and state.active_plan.state == "awaiting_review" then
      require("diff_review.views.plan_review").open(state.active_plan)
    elseif state.goal and state.goal.state == "active" then
      vim.schedule(controller.drain)
    end
    return
  end
  state.transcript_buf, state.transcript_win, state.composer_buf, state.composer_win = layout.open()
  layout.attach_auto_height(state.composer_buf, state.composer_win)
  layout.attach_scroll_boundary(state.transcript_buf, state.transcript_win)
  controller.attach()
  controller.render()
  local opening_transcript_revision = transcript_state.revision(state)
  local opening_transcript_count = #state.transcript
  client.start(function(result, start_error)
    if start_error then
      notifications.error(start_error, "Harness")
      transcript_state.append(state, { kind = "error", text = start_error })
      controller.render()
      return
    end
    state.session = result.session
    state.capability = result.capability or {}
    transcript_state.reconcile_snapshot(
      state,
      result.transcript or {},
      opening_transcript_revision,
      opening_transcript_count
    )
    state.no_checkpoint = result.no_checkpoint == true
    state.goal = result.goal
    state.active_plan = result.active_plan
    controller.render()
    controller.resolve_runtime_model()
    if state.active_plan and state.active_plan.state == "awaiting_review" then
      require("diff_review.views.plan_review").open(state.active_plan)
    elseif state.goal and state.goal.state == "active" then
      vim.schedule(controller.drain)
    end
  end)
  vim.api.nvim_set_current_win(state.composer_win)
  vim.cmd("startinsert")
end

function M.new_session()
  M.open()
  client.request("session.new", {}, function(result, request_error)
    if request_error then notifications.error(request_error, "Harness") return end
    local state = session.harness
    state.session = result.session
    state.capability = result.capability or {}
    transcript_state.replace(state, result.transcript or {})
    state.queue = {}
    state.goal = nil
    state.active_plan = nil
    controller.render()
  end)
end

return M
