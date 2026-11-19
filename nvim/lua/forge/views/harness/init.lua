local activity = require("forge.views.harness.activity")
local M = {}

local client = require("forge.client")
local provider_preference = require("forge.harness.provider_preference")
local config = require("forge.infra.config")
local controller = require("forge.views.harness.controller")
local layout = require("forge.views.harness.layout")
local notifications = require("forge.infra.notifications")
local session = require("forge.session")
local session_navigation = require("forge.views.harness.session_navigation")
local picker = require("forge.views.picker")

local function valid_window(win) return win and vim.api.nvim_win_is_valid(win) end

local function apply_snapshot(state, result, open_mode, insert)
  -- `state` issued the request, so it is the activation's source even when the user has selected
  -- another tab meanwhile; focus follows only when it is still selected.
  session_navigation.activate(result, {
    state = state,
    source_state = state,
    interaction_mode = "reconcile",
    open_mode = open_mode,
    focus = session.harness == state,
    insert = insert,
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
          controller.with_state(state, controller.render)
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
      -- Keep the failure as state: a transient notification alone leaves every later command
      -- reporting that Harness is still initializing.
      state.start_failure = start_error
      notifications.error("Harness failed to start: " .. start_error, "ForgeHarness")
      controller.with_state(state, controller.render)
    end
    return
  end
  state.start_failure = nil
  if startup then startup.mark("snapshot.apply.begin") end
  apply_snapshot(state, result, callback and callback.open_mode, callback and callback.insert)
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
  local retained_session = state.session and state.transcript_buf and state.composer_buf
    and vim.api.nvim_buf_is_valid(state.transcript_buf) and vim.api.nvim_buf_is_valid(state.composer_buf)
  local timeline_key = retained_session and state.session.id or ("initial-pending-" .. tostring(vim.uv.hrtime()))
  state.transcript_buf, state.transcript_win, state.composer_buf, state.composer_win, state.timeline_tab =
    layout.open(timeline_key, "tab")
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
  end, retained_session and { session_id = state.session.id } or nil, startup)
  vim.api.nvim_set_current_win(state.composer_win)
end

---Return whether `state` holds work that stopping the shared host would interrupt or lose.
---@param state ForgeHarnessPresentationState
---@return boolean
function M.session_has_pending_work(state)
  return activity.busy(state) or state.switching_provider == true
    or (state.status and state.status.kind == "finalizing") == true
    or #(state.queue or {}) > 0 or #(state.pending_steer or {}) > 0
    or #(state.pending_submit or {}) > 0 or #(state.pending_handoff or {}) > 0
end

---Switching provider stops the one host that serves every Harness tab, so it waits until no
---session in any tab has a turn running, queued or unsent input, or a finalization in progress.
---@return boolean
function M.provider_switch_available()
  local state = session.harness
  if state.session == nil or state.state_sync_pending or M.session_has_pending_work(state) then return false end
  for _, other_state in pairs(session.harness_by_id) do
    if other_state ~= state and other_state.session and not other_state.host_error
      and M.session_has_pending_work(other_state) then
      return false
    end
  end
  return true
end

---@class ForgeHarnessProviderDestination
---@field kind "new"|"resume"
---@field session_id? string
---@field persist_default? boolean Save `provider` as the default provider once the switch succeeds.
---  Defaults to true, for an explicit provider choice. A caller that only follows a chosen
---  session to its provider passes false, so resuming a session never changes the default.

---Replace the host with one that serves `provider`, then open `destination` in the current view.
---@param provider string
---@param destination ForgeHarnessProviderDestination
---@return boolean started False when no switch started: a refused switch has reported its reason,
---  and a provider that already serves the view needs none. A caller dismisses its own picker
---  only for a started switch.
function M.switch_provider(provider, destination)
  local state = session.harness
  local provider_config = client.provider_map()[provider]
  if not provider_config or provider_config.selectable == false then
    notifications.error("Unknown Harness provider: " .. tostring(provider), "Harness provider")
    return false
  end
  if not M.provider_switch_available() then
    notifications.warn("Finish or cancel pending work before switching providers", "Harness provider")
    return false
  end
  if not destination or (destination.kind ~= "new" and destination.kind ~= "resume")
    or (destination.kind == "resume" and not destination.session_id) then
    notifications.error("Select a new chat or a session to resume", "Harness provider")
    return false
  end
  local previous_provider = state.session.provider
  local previous_session_id = state.session.id
  local previous_mode = state.session.approval_mode
  local previous_access = vim.deepcopy(state.session.access)
  if provider == previous_provider then return false end
  local initialize_options = destination.kind == "new" and { new_session_name = "" }
    or { session_id = destination.session_id }
  state.switching_provider = true
  client.host_stop(nil, function()
    config.options.harness.provider = provider
    -- The pickers that chose this switch have returned focus to the composer by now. Replacing the
    -- session swaps its buffers or focuses the chosen session's own tab, either of which leaves
    -- Insert mode, so the activation restores the mode the composer had when the switch began.
    local composer_insert = state.composer_win ~= nil and vim.api.nvim_get_current_win() == state.composer_win
      and vim.fn.mode(1):sub(1, 1) == "i"
    client.session_initialize(function(result, start_error, error_detail)
      finish_start(state, result, start_error, error_detail, {
        open_mode = "current",
        insert = composer_insert,
        on_ready = function()
          state.switching_provider = false
          -- The reply can arrive after the user selected another tab, so the configuration and
          -- render act on the switching tab rather than on the selected one.
          controller.with_state(state, function()
            if destination.kind == "new" then
              controller.configure({ access = previous_access, approval_mode = previous_mode })
            end
            controller.render()
          end)
          if destination.persist_default ~= false then
            provider_preference.save(provider, function(saved, save_error)
              if not saved then notifications.error(save_error or "Failed to save provider preference", "Harness provider") end
            end)
          end
        end,
        on_error = function(switch_error)
          notifications.error("Failed to switch Harness provider: " .. switch_error, "Harness provider")
          config.options.harness.provider = previous_provider
          client.host_stop(nil, function()
            client.session_initialize(function(previous_result, previous_error, previous_detail)
              state.switching_provider = false
              finish_start(state, previous_result, previous_error, previous_detail)
            end, { session_id = previous_session_id })
          end)
        end,
      })
    end, initialize_options)
  end)
  return true
end

---@param name? string
---@param options? {open_mode?: "current"|"tab"}
function M.new_session(name, options)
  local source_state = session.harness
  local startup = require("forge.harness_startup_log").begin({ workspace = vim.fn.getcwd(), operation = "new" })
  local source_session_id = source_state.session and source_state.session.id or nil
  local replace_current = options and options.open_mode == "current"
    and source_state.timeline_tab and vim.api.nvim_tabpage_is_valid(source_state.timeline_tab)
    and valid_window(source_state.transcript_win) and valid_window(source_state.composer_win)
  local pending = not replace_current and session_navigation.begin_new(name, options and options.open_mode) or nil
  startup.mark("layout.ready")
  startup.mark("session.new.dispatch")
  client.session_create({ source_session_id = source_session_id, name = name }, function(result, request_error)
    startup.mark("session.new.received", { session_id = result and result.session and result.session.id,
      success = request_error == nil })
    if request_error then
      startup.finish("failed")
      if pending then
        pending.error = request_error
        session_navigation.render_pending(pending)
      end
      notifications.error(request_error, "ForgeHarness")
      return
    end
    local target_state = pending and pending.state or source_state
    local target_buffer = target_state.transcript_buf
    if not target_buffer or not vim.api.nvim_buf_is_valid(target_buffer)
      or not target_state.timeline_tab or not vim.api.nvim_tabpage_is_valid(target_state.timeline_tab) then
      startup.finish("closed_before_ready")
      return
    end
    startup.mark("snapshot.apply.begin")
    if pending then
      session_navigation.complete_pending(pending, result)
    elseif source_state.timeline_tab and vim.api.nvim_tabpage_is_valid(source_state.timeline_tab) then
      session_navigation.activate(result, {
        state = source_state, open_mode = "current", focus = session.harness == source_state,
      })
    end
    startup.mark("snapshot.apply.end")
    startup.watch(target_state.transcript_buf)
  end)
end

return M
