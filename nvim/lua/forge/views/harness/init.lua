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

---@class ForgeHarnessStartActivation
---@field open_mode? "current"|"tab"
---@field insert? boolean
---@field build_hidden? boolean Build a session other than the one `state` shows behind its panes
---  and swap it in once rendered, as a same-provider session switch does, so the view keeps the
---  session being left on screen instead of drawing a blank transcript.
---@field on_settled? fun() Runs once the activated session is on screen.
---@field on_ready? fun(result: table)
---@field on_error? fun(failure: string)

---@param state ForgeHarnessPresentationState
---@param result table
---@param activation? ForgeHarnessStartActivation
local function apply_snapshot(state, result, activation)
  activation = activation or {}
  -- `state` issued the request, so it is the activation's source even when the user has selected
  -- another tab meanwhile; focus follows only when it is still selected.
  session_navigation.activate(result, {
    state = not activation.build_hidden and state or nil,
    source_state = state,
    interaction_mode = "reconcile",
    open_mode = activation.open_mode,
    focus = session.harness == state,
    insert = activation.insert,
    on_settled = activation.on_settled,
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
  apply_snapshot(state, result, callback)
  if startup then startup.mark("snapshot.apply.end") end
  if callback and callback.on_ready then callback.on_ready(result) end
end

function M.open()
  local state = session.harness
  if valid_window(state.composer_win) then
    -- A view whose host stopped or now serves another provider comes back in place.
    M.reconnect(state)
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

---@param provider string?
---@return string
function M.provider_label(provider)
  local provider_config = provider and client.provider_map()[provider]
  return provider_config and provider_config.label or tostring(provider)
end

---Text a view shows while the one host does not serve its session.
---@param state ForgeHarnessPresentationState
---@param host_provider string The provider the host serves, or is starting for.
---@return string
function M.detached_notice(state, host_provider)
  local provider = state.session and state.session.provider
  if provider == nil or provider == host_provider then
    return "Harness restarted. Submit or run :ForgeHarness to reconnect."
  end
  return ("Harness now serves %s. Submit or run :ForgeHarness to return to %s."):format(
    M.provider_label(host_provider), M.provider_label(provider))
end

---Tell every view other than `served_state` that the host no longer serves its session. A host
---restart unloads every session, and a view that still drew its transcript and winbar would look
---alive while every submit failed, so the notice stays in the view's status rows until a submit or
---`:ForgeHarness` there brings it back (`reconnect`).
---@param served_state? ForgeHarnessPresentationState The view whose session the host now serves.
---@param failure? string Why the last return failed, put before each notice.
function M.mark_detached_views(served_state, failure)
  local diagnostic = require("forge.views.harness.diagnostic")
  local host_provider = config.options.harness.provider
  local marked = {}
  for _, other_state in pairs(session.harness_by_id) do
    if other_state ~= served_state and not marked[other_state] and other_state.session
      and other_state.session.id and not other_state.switching_provider then
      marked[other_state] = true
      local text = M.detached_notice(other_state, host_provider)
      if failure then text = failure .. ". " .. text end
      diagnostic.set(other_state, "host_error", text, { owner = tostring(client.host_get_generation()),
        category = "host", severity = "warning", recovery = "reconnect", clear_when = "session resumed" })
      -- The status rows show this notice; the controller's own stop message would repeat it.
      other_state.execution_notice = nil
      if valid_window(other_state.transcript_win) then controller.with_state(other_state, controller.render) end
    end
  end
end

---Bring back a view whose host stopped or now serves another provider: a host on another provider
---switches back to the view's provider under `switch_provider`'s rules, and a host on the same
---provider resumes the view's session in place. The composer keeps its draft unsent.
---@param state ForgeHarnessPresentationState
---@return boolean handled False when the view is not waiting for its host; true when the return
---  started, is already running, or was refused with its reason reported.
function M.reconnect(state)
  local saved = state.diagnostic and state.diagnostic.host_error
  if not (state.host_error and saved and saved.text == state.host_error and saved.recovery == "reconnect"
    and state.session and state.session.id) then
    return false
  end
  if state.switching_provider then return true end
  local session_id, provider = state.session.id, state.session.provider
  if provider and provider ~= config.options.harness.provider then
    controller.with_state(state, function()
      M.switch_provider(provider, { kind = "resume", session_id = session_id, persist_default = false })
    end)
    return true
  end
  state.switching_provider, state.switch_notice = true, "Reconnecting Harness"
  controller.with_state(state, controller.render)
  local composer_insert = state.composer_win ~= nil and vim.api.nvim_get_current_win() == state.composer_win
    and vim.fn.mode(1):sub(1, 1) == "i"
  local function settle() state.switching_provider, state.switch_notice = false, nil end
  client.session_initialize(function(result, start_error, error_detail)
    finish_start(state, result, start_error, error_detail, {
      open_mode = "current",
      insert = composer_insert,
      on_settled = settle,
      on_ready = function()
        settle()
        controller.with_state(state, controller.render)
      end,
      on_error = function(reconnect_error)
        settle()
        notifications.error("Failed to reconnect Harness: " .. reconnect_error, "ForgeHarness")
        M.mark_detached_views(nil, "Reconnect failed: " .. reconnect_error)
      end,
    })
  end, { session_id = session_id })
  return true
end

---@class ForgeHarnessProviderDestination
---@field kind "new"|"resume"
---@field session_id? string
---@field persist_default? boolean Save `provider` as the default provider once the switch succeeds.
---  Defaults to true, for an explicit provider choice. A caller that only follows a chosen
---  session to its provider passes false, so resuming a session never changes the default.

---Replace the host with one that serves `provider`, then open `destination` in the current view.
---The view says that the switch is running until the destination replaces it, and every other view
---keeps a notice naming the provider the host now serves (`mark_detached_views`). A view whose own
---session the host no longer serves returns through this switch too (`reconnect`).
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
  -- A live view's own provider is the host's. A view the host no longer serves is returning to
  -- its provider from whichever one the host serves now.
  local returning = state.host_error ~= nil
  local previous_provider = not returning and state.session.provider or config.options.harness.provider
  local previous_session_id = state.session.id
  local previous_mode = state.session.approval_mode
  local previous_access = vim.deepcopy(state.session.access)
  if provider == previous_provider then return false end
  local initialize_options = destination.kind == "new" and { new_session_name = "" }
    or { session_id = destination.session_id }
  state.switching_provider, state.switch_notice = true, "Switching Harness to " .. provider_config.label
  controller.with_state(state, controller.render)
  client.host_stop(nil, function()
    config.options.harness.provider = provider
    M.mark_detached_views(state)
    -- The pickers that chose this switch have returned focus to the composer by now. Replacing the
    -- session swaps its buffers or focuses the chosen session's own tab, either of which leaves
    -- Insert mode, so the activation restores the mode the composer had when the switch began.
    local composer_insert = state.composer_win ~= nil and vim.api.nvim_get_current_win() == state.composer_win
      and vim.fn.mode(1):sub(1, 1) == "i"
    -- The destination is built behind this view and replaces it once rendered; the view it leaves
    -- then joins the views the host no longer serves.
    local served_session_id, settled = nil, false
    local function finish_switch()
      if not settled then
        settled = true
        state.switching_provider, state.switch_notice = false, nil
      end
      if served_session_id then M.mark_detached_views(session.harness_by_id[served_session_id] or state) end
    end
    client.session_initialize(function(result, start_error, error_detail)
      finish_start(state, result, start_error, error_detail, {
        open_mode = "current",
        insert = composer_insert,
        build_hidden = true,
        on_settled = finish_switch,
        on_ready = function(ready_result)
          served_session_id = ready_result and ready_result.session and ready_result.session.id
          local served_state = served_session_id and session.harness_by_id[served_session_id] or state
          -- A destination that took over this view's panes directly is on screen already.
          if served_state == state then finish_switch() end
          -- The reply can arrive after the user selected another tab, so the configuration and
          -- render act on the destination's view rather than on the selected one.
          controller.with_state(served_state, function()
            if destination.kind == "new" then
              controller.configure({ access = previous_access, approval_mode = previous_mode })
            end
            controller.render()
          end)
          if settled then finish_switch() end
          if destination.persist_default ~= false then
            provider_preference.save(provider, function(saved, save_error)
              if not saved then notifications.error(save_error or "Failed to save provider preference", "Harness provider") end
            end)
          end
        end,
        on_error = function(switch_error)
          notifications.error("Failed to switch Harness provider: " .. switch_error, "Harness provider")
          config.options.harness.provider = previous_provider
          if returning then
            -- The view was already detached, and every other view returns on its own submit, so
            -- no host is restarted on its behalf.
            state.switching_provider, state.switch_notice = false, nil
            M.mark_detached_views(nil, "Switch failed: " .. switch_error)
            return
          end
          client.host_stop(nil, function()
            client.session_initialize(function(previous_result, previous_error, previous_detail)
              state.switching_provider, state.switch_notice = false, nil
              finish_start(state, previous_result, previous_error, previous_detail)
              if not previous_error then M.mark_detached_views(state) end
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
      -- The host created no session, so the tab opened for it closes and the refusal is reported
      -- where the command was typed.
      if pending then session_navigation.fail_pending(pending, request_error, "ForgeHarness")
      else notifications.error(request_error, "ForgeHarness") end
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
