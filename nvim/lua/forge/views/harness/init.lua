local M = {}

local client = require("forge.client")
local backend_preference = require("forge.harness.backend_preference")
local config = require("forge.infra.config")
local controller = require("forge.views.harness.controller")
local layout = require("forge.views.harness.layout")
local notifications = require("forge.infra.notifications")
local session = require("forge.session")
local session_navigation = require("forge.views.harness.session_navigation")
local picker = require("forge.views.picker")

local function valid_window(win) return win and vim.api.nvim_win_is_valid(win) end

local function apply_snapshot(state, result, open_mode)
  session_navigation.activate(result, {
    state = state,
    interaction_mode = "reconcile",
    open_mode = open_mode,
  })
end

---@param conflict table
---@return ForgeChoicePopupOption[]
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
  local startup = state.startup_trace
  local conflict = error_detail and error_detail.code == "session_lease_conflict" and error_detail.data or nil
  if conflict then
    if startup then startup.mark("lease_conflict") end
    local confirmed = false
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
          title = "Session in Use",
          subtitle = "Another Neovim instance controls this Harness session.",
          column_headers = { "Action" },
          option_list = option_list,
          footer = "↑↓ select  Enter confirm  q close",
        },
      },
      on_confirm = function(result)
        confirmed = true
        local action = result.option.value
        client.session_resolve_lease_conflict(action, conflict, function(next_result, next_error, next_detail)
          finish_start(state, next_result, next_error, next_detail, callback)
        end)
      end,
      on_close = function()
        if not confirmed then
          if startup then startup.finish("cancelled") end
          state.initial_snapshot_pending = nil
          controller.render()
          if callback and callback.on_error then callback.on_error("Session selection cancelled") end
        end
      end,
    })
    return
  end
  if start_error then
    if startup then startup.finish("failed", { error = start_error }) end
    state.initial_snapshot_pending = nil
    if callback and callback.on_error then
      callback.on_error(start_error)
    else
      notifications.error(start_error, "ForgeHarness")
      controller.render()
    end
    return
  end
  if startup then startup.mark("snapshot.apply.begin") end
  apply_snapshot(state, result, callback and callback.open_mode)
  if startup then startup.mark("snapshot.apply.end") end
  if callback and callback.on_ready then callback.on_ready(result) end
end

function M.open()
  local state = session.harness
  if valid_window(state.composer_win) then
    vim.api.nvim_set_current_win(state.composer_win)
    return
  end
  if state.startup_trace then state.startup_trace.finish("superseded") end
  local startup = require("forge.harness_startup_log").begin({ workspace = vim.fn.getcwd() })
  state.startup_trace = startup
  state.initial_snapshot_pending = true
  state.transcript_buf, state.transcript_win, state.composer_buf, state.composer_win, state.timeline_tab =
    layout.open("initial-pending-" .. tostring(vim.uv.hrtime()))
  layout.attach_auto_height(state.composer_buf, state.composer_win)
  layout.attach_scroll_boundary(state.transcript_buf, state.transcript_win)
  controller.attach()
  controller.render()
  startup.mark("layout.ready")
  local transcript_buffer = assert(state.transcript_buf)
  client.session_initialize(function(result, start_error, error_detail)
    if state.transcript_buf ~= transcript_buffer or not vim.api.nvim_buf_is_valid(transcript_buffer)
      or not valid_window(state.composer_win) then startup.finish("closed_before_ready") return end
    finish_start(state, result, start_error, error_detail)
  end, nil, startup)
  vim.api.nvim_set_current_win(state.composer_win)
end

---@return boolean
function M.backend_switch_available()
  local state = session.harness
  return state.session ~= nil and not state.busy and not state.switching_backend
    and not state.configuring and not state.state_sync_pending
    and not state.aborting_plan
    and not (state.status and state.status.kind == "finalizing")
    and #(state.queue or {}) == 0 and #(state.pending_steer or {}) == 0
    and #(state.pending_submit or {}) == 0
end

---@class ForgeHarnessBackendDestination
---@field kind "new"|"resume"
---@field session_id? string

---@param backend string
---@param destination ForgeHarnessBackendDestination
function M.switch_backend(backend, destination)
  local state = session.harness
  local backend_config = config.options.harness.backends[backend]
  if not backend_config or backend_config.selectable == false then
    notifications.error("Unknown Harness backend: " .. tostring(backend), "Harness backend")
    return
  end
  if not M.backend_switch_available() then
    notifications.warn("Finish or cancel pending work before switching providers", "Harness backend")
    return
  end
  if not destination or (destination.kind ~= "new" and destination.kind ~= "resume")
    or (destination.kind == "resume" and not destination.session_id) then
    notifications.error("Select a new chat or a session to resume", "Harness backend")
    return
  end
  local previous_backend = state.session.backend
  local previous_session_id = state.session.id
  local previous_mode = state.session.approval_mode
  local previous_access = vim.deepcopy(state.session.access)
  if backend == previous_backend then return end
  local initialize_options = destination.kind == "new" and { new_session_name = "" }
    or { session_id = destination.session_id }
  state.switching_backend = true
  client.host_stop(nil, function()
    config.options.harness.backend = backend
    client.session_initialize(function(result, start_error, error_detail)
      finish_start(state, result, start_error, error_detail, {
        open_mode = "current",
        on_ready = function()
          state.switching_backend = false
          if destination.kind == "new" then
            controller.configure({ access = previous_access, approval_mode = previous_mode })
          end
          controller.render()
          local saved, save_error = backend_preference.save(backend)
          if not saved then
            notifications.error(save_error or "Failed to save backend preference", "Harness backend")
          end
        end,
        on_error = function(switch_error)
          notifications.error("Failed to switch Harness backend: " .. switch_error, "Harness backend")
          config.options.harness.backend = previous_backend
          client.host_stop(nil, function()
            client.session_initialize(function(previous_result, previous_error, previous_detail)
              state.switching_backend = false
              finish_start(state, previous_result, previous_error, previous_detail)
            end, { session_id = previous_session_id })
          end)
        end,
      })
    end, initialize_options)
  end)
end

---@param name? string
function M.new_session(name)
  local source_session_id = session.harness.session and session.harness.session.id or nil
  local pending = session_navigation.begin_new(name)
  client.session_create({ source_session_id = source_session_id, name = name }, function(result, request_error)
    if request_error then
      pending.error = request_error
      session_navigation.render_pending(pending)
      notifications.error(request_error, "ForgeHarness")
      return
    end
    session_navigation.complete_pending(pending, result)
  end)
end

return M
