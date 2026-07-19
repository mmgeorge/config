local M = {}

local client = require("diff_review.harness.client")
local backend_preference = require("diff_review.harness.backend_preference")
local config = require("diff_review.infra.config")
local controller = require("diff_review.views.harness.controller")
local layout = require("diff_review.views.harness.layout")
local notifications = require("diff_review.infra.notifications")
local session = require("diff_review.session")
local session_navigation = require("diff_review.views.harness.session_navigation")
local picker = require("diff_review.views.picker")

local function valid_window(win) return win and vim.api.nvim_win_is_valid(win) end

local function apply_snapshot(state, result)
  session_navigation.activate(result, {
    state = state,
    interaction_mode = "reconcile",
  })
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

local function finish_start(state, result, start_error, error_detail, callback)
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
          finish_start(state, next_result, next_error, next_detail, callback)
        end)
      end,
    })
    return
  end
  if start_error then
    if callback and callback.on_error then
      callback.on_error(start_error)
    else
      notifications.error(start_error, "Harness")
      controller.render()
    end
    return
  end
  if callback and callback.on_ready then callback.on_ready(result) end
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
  if not config.harness_backend_explicit then
    config.options.harness.backend = backend_preference.load(
      config.options.harness.backends,
      config.options.harness.backend
    )
  end
  state.transcript_buf, state.transcript_win, state.composer_buf, state.composer_win, state.timeline_tab =
    layout.open("initial-pending-" .. tostring(vim.uv.hrtime()))
  layout.attach_auto_height(state.composer_buf, state.composer_win)
  layout.attach_scroll_boundary(state.transcript_buf, state.transcript_win)
  controller.attach()
  controller.render(true)
  client.start(function(result, start_error, error_detail) finish_start(state, result, start_error, error_detail) end)
  vim.api.nvim_set_current_win(state.composer_win)
end

---@param backend string
function M.switch_backend(backend)
  local state = session.harness
  local backend_config = config.options.harness.backends[backend]
  if not backend_config or backend_config.selectable == false then
    notifications.error("Unknown Harness backend: " .. tostring(backend), "Harness backend")
    return
  end
  if state.busy then
    notifications.warn("Cancel or finish the active turn before switching backends", "Harness backend")
    return
  end
  local previous_backend = config.options.harness.backend
  if backend == previous_backend then return end
  state.queue = {}
  state.pending_steer = {}
  state.pending_config = nil
  state.pending_mode = nil
  client.stop()
  config.options.harness.backend = backend
  client.start(function(result, start_error, error_detail)
    finish_start(state, result, start_error, error_detail, {
      on_ready = function()
        local saved, save_error = backend_preference.save(backend)
        if not saved then
          notifications.error(save_error or "Failed to save backend preference", "Harness backend")
        end
      end,
      on_error = function(switch_error)
        notifications.error("Failed to switch Harness backend: " .. switch_error, "Harness backend")
        config.options.harness.backend = previous_backend
        client.stop()
        client.start(function(previous_result, previous_error, previous_detail)
          finish_start(state, previous_result, previous_error, previous_detail)
        end)
      end,
    })
  end)
end

---@param name? string
function M.new_session(name)
  M.open()
  local source_session_id = session.harness.session and session.harness.session.id or nil
  local pending = session_navigation.begin_new(name)
  client.request_for(source_session_id, "session.new", { name = name }, function(result, request_error)
    if request_error then
      pending.error = request_error
      session_navigation.render_pending(pending)
      notifications.error(request_error, "Harness")
      return
    end
    session_navigation.complete_pending(pending, result)
  end)
end

return M
