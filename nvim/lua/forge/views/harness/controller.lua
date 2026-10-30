local M = {}

local client = require("forge.client")
local command_set = require("forge.shared.view_command_set")
local config = require("forge.infra.config")
local keymaps = require("forge.shared.keymaps")
local notifications = require("forge.infra.notifications")
local diagnostic = require("forge.views.harness.diagnostic")
local perf = require("forge.infra.perf")
local layout = require("forge.views.harness.layout")
local session = require("forge.session")
local prompt_history = require("forge.views.harness.prompt_history")
local provider_picker = require("forge.views.harness.provider_picker")
local context_status = require("forge.views.harness.context_status")
local model_picker = require("forge.views.harness.model_picker")
local model_pins = require("forge.views.harness.model_pins")
local snapshot = require("forge.views.harness.snapshot")
local picker = require("forge.views.picker")
local question_presentation = require("forge.views.harness.question_presentation")
local recap = require("forge.views.harness.recap")
local session_navigation = require("forge.views.harness.session_navigation")
local task_control = require("forge.views.harness.task")
local queue_editor = require("forge.views.harness.queue_editor")
local attachment = require("forge.views.harness.attachment")
local prompt_submission = require("forge.views.harness.prompt_submission")
local tabline = require("forge.views.harness.tabline")
local input_state = require("forge.views.harness.input_state")

local queue_namespace = vim.api.nvim_create_namespace("ForgeHarnessQueue")
local render_observer_for_test = nil
local begin_request
local submit_immediate
local configure_now
local finish_execution
local render_status_hint
local queue_only_commands = { ["/plan"] = true }
local workflow_action = {
  plan = true, execute = true, goal = true, fork = true, attach_plan = true,
  resume = true, pause = true, clear = true, undo = true,
}

---@param state table
---@param field "model"|"effort"|"service_tier"
---@return string
local function applied_setting(state, field)
  if field == "service_tier" then return (state.session and state.session.service_tier) or "default" end
  return (state.session and state.session[field]) or config.options.harness[field]
end

---@param state table
---@param field "model"|"effort"|"service_tier"
---@return string
local function selected_setting(state, field)
  if state.task_config and state.task_config[field] ~= nil then return state.task_config[field] end
  if state.configuring_config and state.configuring_config[field] ~= nil then return state.configuring_config[field] end
  return applied_setting(state, field)
end

---Parse model configuration separately from its optional queued prompt.
local function model_command(text)
  if not text:match("^/model%s") then return nil end
  local model, remainder = text:match("^/model%s+(%S+)%s*(.*)$")
  if not model then return nil end
  local effort, prompt = remainder:match("^(%S+)%s*(.*)$")
  if effort then
    local known = vim.tbl_contains({ "default", "none", "minimal", "low", "medium", "high", "xhigh", "max", "ultra" }, effort)
    for _, candidate in ipairs(session.harness.model_list or {}) do
      known = known or vim.tbl_contains(candidate.reasoning or {}, effort)
    end
    if not known then return nil, nil, "Unknown reasoning effort: " .. effort end
  end
  return { model = model, effort = effort }, prompt or ""
end

---@return table
local function harness_state() return session.harness end

---@param message string
local function report_configuration_error(message)
  notifications.error("Configuration rejected: " .. message, "ForgeHarness")
end

local function picker_host(state)
  local window_list = {}
  for _, win in pairs({ state.transcript_win, state.composer_win }) do
    if win and vim.api.nvim_win_is_valid(win) then window_list[#window_list + 1] = win end
  end
  local current = vim.api.nvim_get_current_win()
  local control_win = vim.tbl_contains(window_list, current) and current or state.composer_win or state.transcript_win
  local actions = {}
  for _, key in ipairs(keymaps.view_keys_for("harness", "cancel")) do
    actions[#actions + 1] = {
      key = key, modes = { "n", "i" }, desc = "Interrupt Harness task", input = true, label = "interrupt",
      callback = function()
        picker.close(true)
        if session.harness == state then M.task_transition({ action = "pause" }) end
      end,
    }
  end
  return {
    action_list = actions,
    status_view = state.presentation and state.presentation.status_view,
    window_list = window_list,
    control_win = control_win,
    transcript_win = state.transcript_win,
    composer_win = state.composer_win,
  }
end

local function open_choice_picker(state, title, subtitle, option_list, callback, empty_text)
  for index, option in ipairs(option_list) do
    option.key = option.key or config.options.picker.choice_keys[index]
  end
  picker.open({
    host = picker_host(state),
    page_list = {
      {
        id = title,
        title = title,
        subtitle = subtitle,
        column_headers = { "Choice", "Details" },
        option_list = option_list,
        empty_text = empty_text,
        footer = "↑↓ select  Enter confirm  q close",
      },
    },
    on_confirm = function(result) callback(result.option.value) end,
  })
end

local function selected_agent_run(state)
  if not state.selected_agent_run_id then return nil end
  for _, run in ipairs((state.agent and state.agent.run) or {}) do
    if run.id == state.selected_agent_run_id then return run end
  end
  return nil
end

---@param buf integer
---@return string
local function composer_text(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return "" end
  return vim.trim(table.concat(vim.api.nvim_buf_get_lines(buf, 0, -1, false), "\n"))
end

---@param buffer integer?
---@return boolean
local function attachments_ready(buffer)
  if not attachment.pending(buffer) then return true end
  notifications.warn("Wait for the attachment import to finish", "ForgeHarness")
  return false
end

---@param buf integer
local function dismiss_composer_completion(buf)
  local completion = package.loaded["blink.cmp"]
  if completion and vim.api.nvim_get_current_buf() == buf then completion.hide() end
end

---@param buf integer
---@param text string
local function set_composer_text(buf, text, attachments)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  dismiss_composer_completion(buf)
  vim.bo[buf].modifiable = true
  local last_row = vim.api.nvim_buf_line_count(buf) - 1
  local last = vim.api.nvim_buf_get_lines(buf, last_row, last_row + 1, false)[1]
  vim.api.nvim_buf_set_text(buf, 0, 0, last_row, #last, vim.split(text, "\n", { plain = true }))
  attachment.restore(buf, attachments)
end

local function restore_retracted_prompt(text, attachments)
  local state = harness_state()
  if state.queue_edit or composer_text(state.composer_buf) ~= "" then
    input_state.submit(state, text, attachments or {}, "queue")
    notifications.warn("The retracted prompt was queued because the composer already contains a newer draft", "ForgeHarness")
    render_status_hint(state)
    return
  end
  set_composer_text(state.composer_buf, text, attachments)
  layout.resize_composer(state.composer_buf, state.composer_win)
  vim.schedule(function()
    if state.composer_win and vim.api.nvim_win_is_valid(state.composer_win) then
      vim.api.nvim_set_current_win(state.composer_win)
    end
  end)
end

---@return { text: string, group: string }[]
local function status_text()
  local state = harness_state()
  if state.initial_snapshot_pending or not state.session then return {} end
  local active_session = state.session
  local current_task = task_control.current(state)
  local approval = active_session.approval_mode or "ask"
  local abbreviation = state.access_abbreviation or "rxw"
  local policy_label = abbreviation .. (({ deny = "dx", allow = "ax" })[approval] or "")
    .. (state.check_editing and "ce" or "")
  local yolo = abbreviation == "rxwx" and approval == "allow"
    and (active_session.access or {}).sandbox == false
  local configured_model = active_session.model or config.options.harness.model
  local model = active_session.resolved_model or (configured_model == "default" and "resolving model" or configured_model)
  local effort = selected_setting(state, "effort")
  local selected_model = selected_setting(state, "model")
  if selected_model ~= configured_model then model = selected_model end
  local busy = state.host_error and "host stopped"
    or state.cancel_requested and "cancelling"
    or #(state.pending_submit or {}) > 0 and "submitting"
    or state.busy and ""
    or (#state.queue > 0 and ("queued " .. #state.queue) or "")
  local tier = selected_setting(state, "service_tier")
  local speed_icon = ({ fast = "󱐌", ultrafast = "󱐋" })[tier]
  local deferred = state.configuration_completion and state.configuration_completion.deferred
  if deferred == nil and state.task_operation and state.task_operation.action == "configure" then
    deferred = state.task_operation.configuration_deferred
  end
  local model_pending = deferred == true and (selected_model ~= configured_model
    or effort ~= (active_session.effort or config.options.harness.effort)
    or tier ~= (active_session.service_tier or "default"))
  local model_text = ("%s %s%s"):format(model, effort, model_pending and "*" or "")
  local segment_list = {}
  local function append_status(text, group)
    if not text or text == "" then return end
    if #segment_list > 0 then segment_list[#segment_list + 1] = keymaps.winbar_separator end
    segment_list[#segment_list + 1] = { text = text, group = group }
  end
  if yolo then append_status("YOLO", "ForgeHarnessYoloIndicator") end
  append_status(policy_label, "ForgeStatusLabel")
  if (active_session.access or {}).sandbox ~= false then
    append_status("sandbox", "ForgeStatusLabel")
  end
  if speed_icon then
    append_status(speed_icon, tier == "ultrafast" and "ForgeHarnessUltrafast" or "ForgeStatusLabel")
    segment_list[#segment_list + 1] = { text = " " .. model_text, group = "ForgeStatusLabel" }
  else
    append_status(model_text, "ForgeStatusLabel")
  end
  local selected_run = selected_agent_run(state)
  append_status(selected_run and (selected_run.nickname or selected_run.definition) or "Main", "ForgeStatusLabel")
  if state.connection_error then
    append_status(state.connection_error, "ForgeHarnessGoal")
  elseif state.sync_error then
    append_status("State unavailable: " .. state.sync_error, "ForgeHarnessGoal")
  elseif state.task_operation and workflow_action[state.task_operation.action]
    and state.task_operation.state ~= "running" then
    append_status("Task " .. state.task_operation.state, "ForgeHarnessGoal")
  elseif current_task then
    append_status(current_task.kind, "ForgeHarnessGoal")
    append_status(task_control.phase(current_task), "ForgeHarnessGoal")
    if task_control.status(current_task) ~= "running" then append_status(task_control.status(current_task), "ForgeHarnessGoal") end
  end
  if current_task and (current_task.kind == "plan" or current_task.kind == "execute") then
    append_status("Auto-accept: " .. (current_task.auto_accept_plan and "on" or "off"), "ForgeHarnessGoal")
  end
  local artifact_count = #(state.artifact or {})
  if artifact_count > 0 then
    append_status(("%d %s"):format(artifact_count, artifact_count == 1 and "artifact" or "artifacts"), "ForgeStatusLabel")
  end
  append_status(busy, "ForgeStatusLabel")
  if state.configuration_error then
    append_status("Settings rejected: " .. state.configuration_error:gsub("%s+", " "), "ForgeHarnessToolFailure")
  end
  return segment_list
end

function M.refresh_winbar()
  local state = harness_state()
  if not state.command_set then return end
  local active_session = not state.initial_snapshot_pending and state.session or nil
  tabline.set_session_name(state.timeline_tab, active_session and active_session.name)
  local right_status = context_status.segment(active_session and active_session.context_usage) or {}
  for _, hint in ipairs(keymaps.view_hint_entries("harness", state.command_set, state, "queue_edit")) do
    if #right_status > 0 then right_status[#right_status + 1] = keymaps.winbar_separator end
    right_status[#right_status + 1] = { text = hint.key, group = "ForgeStatusHintKey" }
    right_status[#right_status + 1] = { text = " " .. hint.label, group = "ForgeStatusHint" }
  end
  local window = {}
  if state.transcript_win then window[state.transcript_win] = true end
  for attached in pairs(state.presentation and state.presentation.views or {}) do window[attached] = true end
  local status = status_text()
  for attached in pairs(window) do
    keymaps.apply_view_winbar(attached, "", "harness", state.command_set, status,
      { diff_view = state.presentation and state.presentation.diff_view }, right_status)
  end
  if state.composer_win and vim.api.nvim_win_is_valid(state.composer_win) then
    vim.wo[state.composer_win].winbar = ""
  end
  render_status_hint(state)
end

---@return integer?
local function working_seconds()
  local state = harness_state()
  if not state.busy or not state.working_started_ns then return nil end
  return math.floor((vim.uv.hrtime() - state.working_started_ns) / 1000000000)
end

render_status_hint = function(state)
  if state.initial_snapshot_pending then return end
  if state.composer_buf and vim.api.nvim_buf_is_valid(state.composer_buf)
    and vim.b[state.composer_buf].forge_queue_rows ~= nil then
    vim.api.nvim_buf_clear_namespace(state.composer_buf, queue_namespace, 0, -1)
    vim.b[state.composer_buf].forge_queue_rows = nil
    if state.composer_win and vim.api.nvim_win_is_valid(state.composer_win) then
      layout.resize_composer(state.composer_buf, state.composer_win)
    end
  end
  if state.presentation and state.presentation.transcript then
    local status_context = {
      restore_recovery = state.restore_recovery, recap = state.recap, rename_status = state.rename_status,
      message_notice = diagnostic.get(state, "execution_notice", "warning"),
      status_notice = require("forge.views.harness.health").notice(state),
    }
    if state.presentation.set_staging then
      state.presentation.set_staging(queue_editor.staging(state, composer_text(state.composer_buf), state.queue or {}),
        queue_editor.staging(state, nil, state.pending_steer or {}))
    end
    if state.transcript_win and vim.api.nvim_win_is_valid(state.transcript_win) then
      state.presentation.status_view:render(status_context, M.command_set())
    end
  end
end

function M.render()
  local state = harness_state()
  if state.initial_snapshot_pending then M.refresh_winbar() return end
  render_status_hint(state)
  if state.host_error then M.refresh_winbar() return end
  if state.switching_backend then M.refresh_winbar() return end
  if not (state.transcript_buf and vim.api.nvim_buf_is_valid(state.transcript_buf)
      and state.composer_buf and vim.api.nvim_buf_is_valid(state.composer_buf)) then return end
  if not (state.session and state.session.id) then
    M.refresh_winbar()
    return
  end
  if require("forge.views.harness.session_preview").is_open(state.transcript_win) then
    M.refresh_winbar()
    return
  end
  ---@type HarnessPresentation?
  local owner = state.presentation
  if owner and (owner.session_id ~= state.session.id or owner.host_generation ~= client.host_get_generation()) then
    if not owner.close({ preserve_buffer = true }) then return end
    state.presentation = nil
    owner = nil
  end
  if owner and not owner.closed then owner.sync() M.refresh_winbar() return end
  if not (state.transcript_win and vim.api.nvim_win_is_valid(state.transcript_win)) then
    state.transcript_win = vim.fn.win_findbuf(state.transcript_buf)[1]
    if not state.transcript_win then return end
  end
  local opening_session_id, opening_generation = state.session.id, client.host_get_generation()
  local function owns_presentation()
    return not state.switching_backend and state.session and state.session.id == opening_session_id
      and client.host_get_generation() == opening_generation
  end
  state.presentation = require("forge.views.harness.presentation").open({
    startup_trace = state.startup_trace,
    session_id = state.session.id, transcript_buffer = state.transcript_buf, composer_buffer = state.composer_buf,
    transcript_window = state.transcript_win, composer_window = state.composer_win,
    is_alive = function() return vim.api.nvim_buf_is_valid(state.transcript_buf) and vim.api.nvim_buf_is_valid(state.composer_buf) end,
    notice = function(message) if owns_presentation() then notifications.error(message, "ForgeHarness") end end,
    on_update = function()
      render_status_hint(state)
      if render_observer_for_test then render_observer_for_test(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), { native = true }) end
    end,
  }, function(opened, failure)
    if not owns_presentation() then return end
    if failure then notifications.error(failure, "ForgeHarness") return end
    if state.selected_agent_run_id then opened.select_agent(state.selected_agent_run_id) end
    opened.sync()
  end)
  M.refresh_winbar()
end

local function schedule_render()
  local state = harness_state()
  if not vim.in_fast_event() then
    M.render()
    return
  end
  if state.render_pending then return end
  state.render_pending = true
  vim.schedule(function()
    local active_state = session.harness
    session.activate_harness(state)
    state.render_pending = false
    M.render()
    if active_state ~= state then session.activate_harness(active_state) end
  end)
end

---@param busy boolean
---@return table? owner
local function set_busy(busy)
  local state = harness_state()
  state.request_busy = busy
  state.busy = busy or state.execution_busy == true
  state.execution_request_owner = busy and {} or nil
  if busy then
    state.execution_notice = nil
    state.execution_notice_operation = nil
    if state.working_started_ns then return state.execution_request_owner end
    state.working_started_ns = vim.uv.hrtime()
    state.working_timer = vim.uv.new_timer()
    state.working_timer:start(1000, 1000, function()
      vim.schedule(function()
        local active_state = session.harness
        session.activate_harness(state)
        if state.busy then schedule_render() end
        if active_state ~= state then session.activate_harness(active_state) end
      end)
    end)
  else
    state.working_started_ns = nil
    state.cancel_requested = false
    if state.working_timer then
      state.working_timer:stop()
      state.working_timer:close()
      state.working_timer = nil
    end
  end
  schedule_render()
  return state.execution_request_owner
end

---@param state table
---@param approval_id string
---@return boolean
local function remove_approval(state, approval_id)
  for index, approval in ipairs(state.approval or {}) do
    if approval.id == approval_id then
      table.remove(state.approval, index)
      return true
    end
  end
  return false
end

---@param state table
local function reconcile_approval_presentation(state)
  if state.dismissed_approval_id and not vim.iter(state.approval or {}):any(function(approval)
    return approval.id == state.dismissed_approval_id
  end) then state.dismissed_approval_id = nil end
  local presented_id = state.presented_approval_id
  if not presented_id then return end
  local still_pending = vim.iter(state.approval or {}):any(function(approval)
    return approval.id == presented_id
  end)
  if still_pending then return end
  require("forge.views.harness.approval").close()
  state.approval_open = false
  state.presented_approval_id = nil
end

---@param callback? fun(result?: table, failure?: string)
---@param target_state? table
local function synchronize_state(callback, target_state)
  local synchronized_state = target_state or harness_state()
  local synchronized_session = synchronized_state.session and synchronized_state.session.id
  local synchronized_generation = client.host_get_generation()
  if synchronized_state.host_error then M.render() return end
  if callback then
    synchronized_state.state_sync_callback = synchronized_state.state_sync_callback or {}
    table.insert(synchronized_state.state_sync_callback, callback)
  end
  if synchronized_state.state_sync_pending then
    synchronized_state.state_sync_again = true
    return
  end
  synchronized_state.state_sync_pending = true
  local invalidation = synchronized_state.state_invalidation or 0
  client.request({ session_id = synchronized_session, method = "session.state.get", params = {} }, function(result, request_error, error_detail)
    synchronized_state.state_sync_pending = false
    if client.host_get_generation() ~= synchronized_generation or not synchronized_state.session
      or synchronized_state.session.id ~= synchronized_session then return end
    if result and invalidation ~= (synchronized_state.state_invalidation or 0) then
      vim.schedule(function() synchronize_state(nil, synchronized_state) end)
      return
    end
    if result and result.session and result.session.id ~= synchronized_session then
      local failure = "Snapshot belongs to another conversation"
      diagnostic.set(synchronized_state, "sync_error", failure,
        { owner = synchronized_session, category = "protocol", severity = "error", recovery = "reconnect" })
      local callbacks = synchronized_state.state_sync_callback or {}
      synchronized_state.state_sync_callback = nil
      for _, completed in ipairs(callbacks) do completed(nil, failure) end
      if harness_state() == synchronized_state then M.render() end
      return
    end
    if result and result.runtime_epoch == synchronized_state.runtime_epoch
      and (result.timeline_revision or 0) < (synchronized_state.timeline_revision or 0) then
      synchronized_state.state_sync_again = nil
      vim.schedule(function() synchronize_state(nil, synchronized_state) end)
      return
    end
    if result and result.runtime_epoch == synchronized_state.runtime_epoch
      and (result.snapshot_revision or 0) < (synchronized_state.snapshot_revision or 0) then
      synchronized_state.state_sync_again = nil
      vim.schedule(function() synchronize_state(nil, synchronized_state) end)
      return
    end
    if request_error or not result then
      local failure = request_error or "Harness broker returned an empty state snapshot"
      local code = error_detail and error_detail.code
      diagnostic.set(synchronized_state, "sync_error", failure,
        { owner = synchronized_session, category = "snapshot", recovery = "retry",
          severity = (not request_error or error_detail and code ~= "not_admitted" and code ~= "outcome_unknown") and "error" or "info" })
      synchronized_state.state_sync_again = nil
      if harness_state() == synchronized_state then M.render() end
      local retry = (synchronized_state.state_sync_retry or 0) + 1
      synchronized_state.state_sync_retry = retry
      vim.defer_fn(function()
        if harness_state() == synchronized_state and client.host_get_generation() == synchronized_generation then synchronize_state(nil, synchronized_state) end
      end, math.min(8000, 1000 * 2 ^ math.min(retry - 1, 3)))
      return
    end
    diagnostic.set(synchronized_state, "sync_error", nil)
    synchronized_state.state_sync_retry = nil
    if synchronized_state.state_sync_again then
      synchronized_state.state_sync_again = nil
      synchronize_state(nil, synchronized_state)
      return
    end
    local state = synchronized_state
    snapshot.apply(state, result)
    input_state.refresh(state)
    if harness_state() ~= state then state.state_sync_callback = nil return end
    M.attach_transcript(state.transcript_buf)
    require("forge.views.harness.agent_picker").refresh()
    reconcile_approval_presentation(state)
    local elicitation = state.active_elicitation and state.active_elicitation.elicitation
    if elicitation and question_presentation.should_present(state) then
      if state.plan_question_open then
        require("forge.views.harness.plan_question").close()
        state.plan_question_open = false
      end
      vim.schedule(function() M.present_plan_question(false) end)
    end
    if #state.approval > 0 then vim.schedule(M.present_approval) end
    M.render()
    local callbacks = synchronized_state.state_sync_callback or {}
    synchronized_state.state_sync_callback = nil
  for _, completed in ipairs(callbacks) do completed(result, nil) end
  end)
end



local function on_event(event, payload)
  local state = harness_state()
  if event ~= "backend_event" and event ~= "input_changed" and event ~= "task_operation" then
    state.state_invalidation = (state.state_invalidation or 0) + 1
  end
  if event == "task_operation" then task_control.receive(state, payload) return end
  if event == "input_changed" then input_state.receive(state, payload) return end
  if event == "host_stopped" then
    diagnostic.set(state, "host_error", payload.message,
      { owner = tostring(client.host_get_generation()), category = "host", severity = payload.expected and "info" or "error",
        recovery = "reconnect", clear_when = "new host generation" })
    state.execution_notice = payload.message .. ". Reopen Harness to reconnect."
    state.cancel_requested = false
    state.state_sync_pending, state.state_sync_again, state.state_sync_callback = nil, nil, nil
    state.configuring = false
    state.configuring_config = nil
    if state.configuration_completion then state.configuration_completion.complete(false) end
    state.configuration_completion, state.task_config = nil, nil
    state.approval, state.active_wait, state.dismissed_approval_id = {}, nil, nil
    state.ready = false
    state.execution_busy = false
    if state.presentation and state.presentation.terminals then state.presentation.terminals.close() end
    set_busy(false)
    M.refresh_winbar()
    return
  end
  if state.host_error then return end
  if event ~= "state_invalidated" then
    local route = require("forge.event_contract").route("session", event)
    if route == "ignore" then return end
    if route == "refresh" then synchronize_state() return end
  end
  if event == "document_changed" then
    if payload.session_id ~= (state.session and state.session.id) then return end
    if payload.revision < (state.timeline_revision or 0) then return end
    state.timeline_revision = payload.revision
    if payload.status then state.status = payload.status end
    if type(payload.checks_running) == "boolean" then
      local checks_finished = state.checks_running and not payload.checks_running
      state.checks_running = payload.checks_running
    end
    if payload.active_check_run ~= nil then
      if payload.active_check_run == vim.NIL then state.active_check_run = nil
      else state.active_check_run = payload.active_check_run end
    end
    if state.status.kind ~= "awaiting_input" and state.plan_question_open then
      require("forge.views.harness.plan_question").close()
      state.plan_question_open = false
    end
    if state.status.kind == "awaiting_plan_review" then
      state.active_elicitation = nil
      question_presentation.reset(state)
    end
    schedule_render()
  elseif event == "backend_event" then
    if require("forge.event_contract").route("backend", payload.kind) == "ignore" then return end
    if payload.kind == "checks_configuration_error" then
      notifications.error((payload.data or {}).error or "Fix the project check configuration", "Harness checks")
    elseif payload.kind == "turn_started" then
      recap.clear(state)
    elseif payload.kind == "execution_state" then
      local execution = payload.data or {}
      if type(execution.session) ~= "table" or not state.session or execution.session.id ~= state.session.id then return end
      state.session = execution.session
      state.execution_busy = true
      state.busy = true
      M.refresh_winbar()
    elseif payload.kind == "prompt_submission" then
      schedule_render()
    elseif payload.kind == "approval_requested" then
      if state.state_sync_pending then state.state_sync_again = true end
      local request = payload.data or payload
      state.approval = state.approval or {}
      if not vim.iter(state.approval):any(function(approval) return approval.id == request.id end) then
        state.approval[#state.approval + 1] = request
      end
      M.refresh_winbar()
      render_status_hint(state)
      vim.schedule(M.present_approval)
    elseif payload.kind == "approval_resolved" or payload.kind == "approval_cancelled" then
      if state.state_sync_pending then state.state_sync_again = true end
      local request = payload.data or payload
      remove_approval(state, request.id)
      reconcile_approval_presentation(state)
      M.refresh_winbar()
      render_status_hint(state)
      if #state.approval > 0 then vim.schedule(M.present_approval) end
    elseif payload.kind == "agent_updated" then
      state.agent = vim.deepcopy(payload.data or payload)
      require("forge.views.harness.agent_picker").refresh()
    elseif payload.kind == "runtime_resolved" then
      local runtime = payload.data or {}
      if not state.session or runtime.session_id ~= state.session.id then return end
      state.session.provider_label = runtime.provider
      state.session.resolved_model = runtime.model
      M.refresh_winbar()
    elseif payload.kind == "context_usage" then
      if state.session then state.session.context_usage = payload.data or payload end
      M.refresh_winbar()
    elseif payload.kind == "timeline_node_updated" then
      M.refresh_winbar()
    elseif payload.kind == "error" then
      if type(payload.text) == "string" then notifications.warn(payload.text, "ForgeHarness") end
    else
      error("Unhandled Harness backend event: " .. tostring(payload.kind))
    end
  elseif event == "question" then
    if not question_presentation.receive(state, payload, true) then return end
    if question_presentation.should_present(state) then
      if state.plan_question_open then
        require("forge.views.harness.plan_question").close()
        state.plan_question_open = false
      end
      vim.schedule(function() M.present_plan_question(false) end)
    end
    schedule_render()
  elseif event == "question_updated" then
    if not question_presentation.receive(state, payload, false) then return end
    if question_presentation.should_present(state) then
      require("forge.views.harness.plan_question").close()
      state.plan_question_open = false
      vim.schedule(function() M.present_plan_question(false) end)
    end
    schedule_render()
  elseif event == "question_answered" then
    if state.plan_question_open then
      require("forge.views.harness.plan_question").close()
      state.plan_question_open = false
    end
    state.active_elicitation = nil
    question_presentation.reset(state)
    synchronize_state()
  elseif event == "question_withdrawn" then
    state.active_elicitation = nil
    question_presentation.reset(state)
    synchronize_state()
  elseif event == "plan_question" then
    state.active_plan = payload.plan or state.active_plan
    synchronize_state()
  elseif event == "plan_question_updated" then
    state.active_plan = payload.plan or state.active_plan
    M.refresh_winbar()
    schedule_render()
  elseif event == "goal_changed" or event == "goal_continue_requested" then
    synchronize_state()
  elseif event == "context_compacted" then
    synchronize_state()
  elseif event == "session_fork_ready" or event == "session_fork_failed" then
    state.session = payload.session or state.session
    M.refresh_winbar()
    if event == "session_fork_failed" then
      local provider_fork_state = state.session and state.session.provider_fork_state or {}
      notifications.error(provider_fork_state.message or "Provider fork preparation failed", "Harness fork")
    end
  elseif event == "session_changed" or event == "session_configured"
  then
    local next_session = payload.session or payload
    if event == "session_changed" and state.session and next_session.id ~= state.session.id then
      recap.clear(state)
      state.queue = {}
      state.queue_edit = nil
      state.active_plan = nil
      state.active_elicitation = nil
      state.active_wait = nil
      state.timeline = {}
      state.artifact = {}
      state.plan_question_open = false
      question_presentation.reset(state)
      prompt_history.reset_navigation()
    end
    state.session = next_session
    if payload.access_abbreviation then state.access_abbreviation = payload.access_abbreviation end
    local configuration = state.configuration_completion
    if configuration and state.task_operation and payload.operation_id == state.task_operation.id then
      if state.task_config and type(state.task_config.lsp_tools) == "boolean" then state.lsp_tools = state.task_config.lsp_tools end
      if state.task_config and type(state.task_config.subagent_tools) == "boolean" then
        state.subagent_tools = state.task_config.subagent_tools
      end
      state.configuration_completion = nil
      state.task_config = nil
      configuration.complete(true)
    end
    M.refresh_winbar()
  elseif event == "agent_updated" then
    state.agent = vim.deepcopy(payload)
    require("forge.views.harness.agent_picker").refresh()
    schedule_render()
  elseif event == "state_invalidated" then
    synchronize_state()
  else
    error("Unhandled Harness session event: " .. tostring(event))
  end
end

---@param reopen? boolean
function M.present_approval(reopen)
  local state = harness_state()
  local request = state.approval and state.approval[1]
  if state.approval_open or not request then return end
  if not reopen and state.dismissed_approval_id == request.id then return end
  state.dismissed_approval_id = nil
  state.approval_open = true
  state.presented_approval_id = request.id
  local owner = type(request.execution_owner) == "table" and request.execution_owner or nil
  local child_id = owner and owner.agent_id ~= (state.session.primary_agent_id or (state.session.id .. ":agent:primary"))
    and owner.agent_id or nil
  local owner_label
  if child_id then
    local run = vim.iter((state.agent and state.agent.run) or {}):find(function(candidate) return candidate.id == child_id end)
    local name = run and (run.task_name or run.nickname or run.definition) or child_id
    local provider = request.provider
    owner_label = "Agent " .. name .. (provider and (" · " .. provider) or "")
  end
  require("forge.views.harness.approval").open(request, {
    owner_label = owner_label,
    interrupt = function()
      if child_id then M.agent_action("interrupt", child_id)
      else M.task_transition({ action = "pause" }) end
    end,
    transcript_win = state.transcript_win,
    window_list = picker_host(state).window_list,
    control_win = picker_host(state).control_win,
    resolve = function(approval_id, answer_list, callback)
      client.request({ session_id = state.session.id, method = "session.approval.resolve", params = {
        approval_id = approval_id,
        answer_list = answer_list,
      } }, function(_, request_error)
        if request_error then
          notifications.error(request_error, "Harness approval")
          synchronize_state()
          callback(false)
          return
        end
        remove_approval(state, approval_id)
        state.approval_open = false
        state.presented_approval_id = nil
        callback(true)
        M.refresh_winbar()
        if #state.approval > 0 then vim.schedule(M.present_approval) end
      end)
    end,
    closed = function()
      state.approval_open = false
      state.presented_approval_id = nil
      state.dismissed_approval_id = vim.iter(state.approval or {}):any(function(approval)
        return approval.id == request.id
      end) and request.id or nil
      M.refresh_winbar()
    end,
  })
end

---@param force? boolean
function M.present_plan_question(force)
  local state = harness_state()
  local elicitation = state.active_elicitation and state.active_elicitation.elicitation
  if state.question_clarification or (state.busy and not force) or state.plan_question_open or not elicitation then return end
  local owner = state.active_elicitation.owner
  local owner_id = state.active_elicitation.exchange_id or state.active_elicitation.plan_id
  local session_id = state.session.id
  local question_target = { owner = owner, id = owner_id, set_id = elicitation.question_set.id, revision = elicitation.revision }
  local draft = state.question_draft
  if draft and (draft.owner_id ~= owner_id or draft.question_set_id ~= elicitation.question_set.id
    or not vim.iter(elicitation.question_set.questions or {}):any(function(item) return item.id == draft.question_id end)) then
    state.question_draft = nil
    draft = nil
  end
  if not force and not question_presentation.should_present(state) then return end
  question_presentation.mark_presented(state)
  state.plan_question_open = true
  local host = picker_host(state)
  require("forge.views.harness.plan_question").open(elicitation, {
    picker_host = host,
    transcript_win = state.transcript_win,
    window_list = host.window_list,
    control_win = host.control_win,
    allow_ask = owner ~= "plan_acceptance",
    clarification_draft = draft,
    answer = function(params, callback)
      params.target = question_target
      client.request({ session_id = session_id, method = "session.question.answer", params = params }, function(result, request_error, detail)
        if request_error then
          notifications.error(request_error, "Harness question")
          callback(nil, request_error, detail)
          return
        end
        state.active_plan = result.active_plan or state.active_plan
        question_presentation.receive(state, result.active_elicitation, false)
        callback(result.active_elicitation and result.active_elicitation.elicitation)
      end)
    end,
    skip = function(params, callback)
      params.target = question_target
      client.request({ session_id = session_id, method = "session.question.skip", params = params }, function(result, request_error, detail)
        if request_error then
          notifications.error(request_error, "Harness question")
          callback(nil, request_error, detail)
          return
        end
        state.active_plan = result.active_plan or state.active_plan
        question_presentation.receive(state, result.active_elicitation, false)
        callback(result.active_elicitation and result.active_elicitation.elicitation)
      end)
    end,
    ask = function(params)
      state.plan_question_open = false
      prompt_history.record(params.text)
      local pending_draft = { owner_id = owner_id, question_set_id = elicitation.question_set.id,
        question_id = params.question_id, text = params.text }
      state.question_draft = pending_draft
      state.question_clarification = pending_draft
      input_state.submit(state, params.text, {}, "auto", nil, function(failure)
        if not failure and state.question_draft == pending_draft then state.question_draft = nil end
        question_presentation.reset(state)
        synchronize_state(function(_, sync_failure)
          if not sync_failure and state.question_clarification == pending_draft then
            state.question_clarification = nil
            M.present_plan_question(false)
          end
        end)
      end)
    end,
    pending = function()
      state.execution_notice = "Submitting answers"
      M.render()
    end,
    continue = function()
      state.plan_question_open = false
      state.execution_notice = nil
      local request_owner = set_busy(true)
      client.request({ session_id = session_id, method = "session.question.continue", params = { target = question_target } }, function(_, request_error, error_detail)
        if request_error and error_detail and error_detail.code == "not_admitted" then
          set_busy(false)
          synchronize_state()
          return
        end
        if finish_execution(request_error, error_detail, request_owner) then return end
        if request_error then
          if not error_detail or error_detail.code ~= "not_admitted" then
            notifications.error(request_error, "Planning continuation")
          end
          synchronize_state()
          return
        end
        synchronize_state()
      end)
    end,
    closed = function()
      state.plan_question_open = false
      if owner == "plan_acceptance" then
        client.request({ session_id = state.session.id, method = "session.plan.acceptance.cancel", params = {} }, function(result, request_error)
          if request_error then
            notifications.error(request_error, "Plan acceptance")
            synchronize_state()
            return
          end
          state.active_plan = result.active_plan or state.active_plan
          state.active_elicitation = result.active_elicitation
          M.render()
          M.refresh_winbar()
        end)
        return
      end
      M.render()
    end,
  })
end

function M.reopen_question()
  local state = harness_state()
  if #(state.approval or {}) > 0 then
    M.present_approval(true)
    return
  end
  if not (state.active_elicitation and state.active_elicitation.elicitation) then
    notifications.warn("No Harness approval or question awaits feedback", "ForgeHarness")
    return
  end
  M.present_plan_question(true)
end

---@param request_error string?
---@param error_detail {code: string}?
---@param request_owner table?
---@return boolean
finish_execution = function(request_error, error_detail, request_owner)
  local state = harness_state()
  if request_owner and state.execution_request_owner ~= request_owner then return true end
  if state.host_error then set_busy(false) return true end
  if state.task_operation then return true end
  if request_error and error_detail and error_detail.code == "turn_cancelled" then
    diagnostic.set(state, "execution_notice", "Paused", { owner = state.session.id, category = "cancelled", severity = "info", recovery = "resume" })
    set_busy(false)
    synchronize_state()
    return true
  end
  if request_error and not (error_detail and error_detail.code == "turn_retracted") then
    local code = error_detail and error_detail.code
    local message = code == "not_admitted" and ("Not submitted · " .. request_error)
      or code == "outcome_unknown" and ("Checking submission · " .. request_error)
      or ("Stopped · " .. request_error)
    diagnostic.set(state, "execution_notice", message, { owner = state.session.id, category = code or "execution",
      severity = code == "not_admitted" and "info" or code == "outcome_unknown" and "warning" or "error",
      recovery = code == "not_admitted" and "resubmit" or "inspect" })
  end
  set_busy(false)
  return false
end

---@param text string
begin_request = function(text, from_composer, attachments, captured_prompt, queue_entry)
  local state = harness_state()
  local context
  if from_composer then
    local failure
    if captured_prompt then context = captured_prompt.context
    elseif state.presentation then context, failure = state.presentation.capture_prompt()
    else failure = "Harness composer is not ready" end
    if not context then notifications.error(failure, "ForgeHarness") return end
  end
  recap.clear(state)
  if from_composer then dismiss_composer_completion(state.composer_buf) end
  state.cancel_requested = false
  if from_composer then state.queue_paused = false end
  prompt_submission.start(state, {
    text = text, attachments = attachments, context = context,
    source = captured_prompt and captured_prompt.source,
    changedtick = captured_prompt and captured_prompt.changedtick,
    queue_entry = queue_entry,
  }, function() M.refresh_winbar() end)
end

---@return nil
function M.cancel_turn()
  local state = harness_state()
  if state.host_error then M.render() return end
  if state.selected_agent_run_id then
    M.agent_action("interrupt", state.selected_agent_run_id)
    return
  end
  local operation = state.task_operation
  if state.cancel_requested or operation and operation.action == "pause" then return end
  local session_id = state.session and state.session.id
  M.task_transition({ action = "pause" }, nil, function(success)
    if not success or not session_id or harness_state() ~= state or state.session.id ~= session_id then return end
    state.cancel_requested = true
    client.request({ session_id = session_id, method = "session.turn.cancel",
      params = { restore_prompt_if_no_output = true } }, function(_, failure)
      if harness_state() ~= state or state.session.id ~= session_id then return end
      state.cancel_requested = false
      if failure then notifications.error(failure, "Harness cancellation") end
      M.render()
    end)
  end)
end
function M.compact(completed)
  if not (harness_state().capability and harness_state().capability.native_compact) then
    notifications.error("The current backend does not support manual context compaction", "ForgeHarness")
    return
  end
  M.task_transition({ action = "compact" }, nil, completed)
end


---@param name string
function M.rename_session(name)
  local state = harness_state()
  local model = selected_setting(state, "model")
  require("forge.views.harness.session_name").rename(state, name, model, function()
    render_status_hint(state)
    M.refresh_winbar()
  end)
end

---@param name? string
function M.fork_session(name)
  local state = harness_state()
  local active_session = state.session
  if not (active_session and active_session.id) then
    notifications.error("No active Harness session to fork", "ForgeHarness")
    return
  end
  if state.capability.native_fork ~= true then
    notifications.warn("The current backend does not support session fork", "ForgeHarness")
    return
  end
  local params = { session_id = active_session.id }
  if name and name ~= "" then params.name = name end
  local fork_started = perf.now()
  local pending = session_navigation.begin_fork(active_session, name)
  perf.event("harness", "harness.fork.ui_opened", {
    ms = perf.elapsed_ms(fork_started),
    source_session_id = active_session.id,
  })
  client.request({ session_id = active_session.id, method = "session.fork", params = params }, function(result, request_error)
    if request_error then
      perf.event("harness", "harness.fork.failed", {
        ms = perf.elapsed_ms(fork_started),
        source_session_id = active_session.id,
        error = tostring(request_error),
      })
      session_navigation.fail_fork(pending, request_error)
      return
    end
    local performance = result and result.fork_performance or {}
    perf.event("harness", "harness.fork.complete", {
      ms = perf.elapsed_ms(fork_started),
      broker_ms = performance.total_ms,
      source_session_id = active_session.id,
      child_session_id = result.session and result.session.id,
    })
    for _, timing in ipairs(performance.timing or {}) do
      perf.event("harness", "harness.fork.phase", {
        phase = timing.phase,
        ms = timing.duration_ms,
        source_session_id = active_session.id,
      })
    end
    session_navigation.complete_fork(pending, result)
  end)
end

function M.open_timeline_entry()
  local state = harness_state()
  if vim.api.nvim_get_current_buf() ~= state.transcript_buf then return end
  if not state.presentation then return end
  state.presentation.activate(function(action, captured)
    if state.presentation.open_output(action, captured) then return end
    if action.kind == "question" then
      local elicitation = state.active_elicitation and state.active_elicitation.elicitation
      if elicitation and elicitation.question_set and elicitation.question_set.id == action.question_set_id then
        M.present_plan_question(true)
      else
        state.presentation.toggle_heading(vim.api.nvim_get_current_win())
      end
    elseif action.kind == "session" then session_navigation.open_parent(action.session_id)
    elseif action.kind == "agent" then M.select_agent(action.run_id)
    elseif action.kind == "url" then vim.ui.open(action.url)
    elseif action.kind == "declaration" then
      client.request({ session_id = state.session.id, method = "session.plan.declaration.get", params = action }, function(snapshot, failure)
        if failure then notifications.error(failure, "Plan declaration") return end
        vim.cmd("tabnew")
        local declaration_buffer = vim.api.nvim_get_current_buf()
        vim.api.nvim_buf_set_name(declaration_buffer, ("PlanDeclaration://%s/%d/%s/%s"):format(
          action.plan_id, action.revision, action.baseline and "baseline" or "proposed", action.path)
          .. "#" .. declaration_buffer)
        vim.api.nvim_buf_set_lines(declaration_buffer, 0, -1, false, vim.split(snapshot.text, "\n", { plain = true }))
        vim.bo[declaration_buffer].buftype = "nofile"
        vim.bo[declaration_buffer].bufhidden = "wipe"
        vim.bo[declaration_buffer].swapfile = false
        vim.bo[declaration_buffer].filetype = vim.filetype.match({ filename = action.path }) or ""
        vim.bo[declaration_buffer].modifiable = false
        vim.bo[declaration_buffer].readonly = true
        vim.wo.winbar = ("Plan declaration • revision %d • %s"):format(action.revision, action.path)
        vim.api.nvim_win_set_cursor(0, { math.max(1, math.min(action.line or 1, vim.api.nvim_buf_line_count(0))), 0 })
        vim.keymap.set("n", "q", "<Cmd>tabclose<CR>", { buffer = declaration_buffer, silent = true })
      end)
    elseif action.kind == "file" then
      local path = vim.fs.joinpath(state.session.workspace, action.path)
      if vim.fn.filereadable(path) ~= 1 then
        notifications.warn("Changed file is no longer available: " .. path, "ForgeHarness")
        return
      end
      local opened, failure = pcall(vim.api.nvim_cmd, { cmd = "tabedit", args = { path } }, {})
      if not opened then notifications.error(failure, "ForgeHarness") return end
      vim.api.nvim_win_set_cursor(0, { math.max(1, math.min(action.line or 1, vim.api.nvim_buf_line_count(0))), 0 })
    elseif action.kind == "plan" then
      local session_id, generation = state.session.id, client.host_get_generation()
      client.request({ session_id = session_id, method = "session.plan.get", params = { plan_id = action.plan_id, revision = action.revision } }, function(plan, failure)
        if not state.session or state.session.id ~= session_id or client.host_get_generation() ~= generation then return end
        if failure then notifications.error(failure, "Harness artifact") return end
        require("forge.views.plan_review").open(plan)
      end)
    end
  end)
end

function M.abort_plan()
  M.task_transition({ action = "clear" })
end

function M.open_replan_picker()
  local state = harness_state()
  task_control.plans(state, picker_host(state), false, M.task_transition)
end

function M.open_artifact_picker()
  local state = harness_state()
  local session_id, generation = state.session.id, client.host_get_generation()
  if state.presentation and state.status and state.status.kind == "awaiting_plan_review" then
    state.presentation.status_action(function(action)
      client.request({ session_id = session_id, method = "session.plan.get", params = { plan_id = action.plan_id, revision = action.revision } }, function(plan, failure)
        if not state.session or state.session.id ~= session_id or client.host_get_generation() ~= generation then return end
        if failure then notifications.error(failure, "Harness artifact") return end
        require("forge.views.plan_review").open(plan)
      end)
    end)
    return
  end
  local function current()
    return state.session and state.session.id == session_id and client.host_get_generation() == generation
  end
  local artifact_list = vim.deepcopy(state.artifact or {})
  local current_id = state.active_plan and state.active_plan.id
  table.sort(artifact_list, function(left, right)
    if (left.id == current_id) ~= (right.id == current_id) then return left.id == current_id end
    return (left.updated_at_ms or 0) > (right.updated_at_ms or 0)
  end)
  local option_list = vim.tbl_map(function(artifact)
      local title = artifact.title and artifact.title ~= "" and artifact.title or "[unnamed plan]"
      return { label = title, detail = artifact.state or "unknown", value = artifact }
    end, artifact_list)
  open_choice_picker(state, "Select Artifact", nil, option_list, function(artifact)
    if not artifact or not current() then return end
    client.request({ session_id = session_id, method = "session.plan.get", params = { plan_id = artifact.id } }, function(plan, request_error)
      if not current() then return end
      if request_error then
        notifications.error(request_error, "Harness artifact")
        return
      end
      require("forge.views.plan_review").open(plan)
    end)
  end, "This session has no artifacts.")
end

---@class ForgeRollbackInteraction
---@field id string
---@field ordinal integer
---@field prompt string
---@field state "complete"|"failed"|"cancelled"
---@field checkpoint_before string

---@param text string
local function restore_composer_prompt(text)
  local state = harness_state()
  set_composer_text(state.composer_buf, text)
  layout.resize_composer(state.composer_buf, state.composer_win)
  vim.schedule(function()
    if not (state.composer_buf and vim.api.nvim_buf_is_valid(state.composer_buf)
      and state.composer_win and vim.api.nvim_win_is_valid(state.composer_win))
    then
      return
    end
    local line_list = vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)
    local last_line = line_list[#line_list] or ""
    vim.api.nvim_set_current_win(state.composer_win)
    vim.api.nvim_win_set_cursor(state.composer_win, { math.max(1, #line_list), #last_line })
    vim.cmd("startinsert!")
  end)
end

---@param preview table
---@param interaction table?
local function confirm_restore_preview(preview, interaction)
  local state = harness_state()
  local session_id, generation = state.session.id, client.host_get_generation()
  local function current()
    return harness_state() == state and state.session and state.session.id == session_id
      and client.host_get_generation() == generation
  end
  local description = { tostring(preview.files) .. " files will be restored.", "Git’s index will not change." }
  for _, warning in ipairs(preview.warnings or {}) do description[#description + 1] = warning end
  local choices = {
    { label = "Cancel", value = "cancel" },
    { label = (preview.warning_count or 0) > 0 and "Restore anyway" or "Restore", value = "restore" },
  }
  if preview.next_offset then
    choices[#choices + 1] = { label = "More warnings", value = "more" }
  end
  open_choice_picker(state, "Restore checkpoint", table.concat(description, "\n"), choices, function(choice)
    if not current() or choice == "cancel" or not choice then return end
    if choice == "more" then
      client.request({ session_id = session_id, method = "session.exchange.rollback.preview", params = { preview_id = preview.preview_id, offset = preview.next_offset } }, function(page, err)
        if not current() then return end
        if err then notifications.error(err, "Harness undo") return end
        confirm_restore_preview(page, interaction)
      end)
      return
    end
    if state.restore_applying or state.busy then return end
    local operation = {}
    state.restore_applying = operation
    client.request({ session_id = session_id, method = "session.exchange.rollback.apply", params = { preview_id = preview.preview_id } }, function(_, err)
      if not current() or state.restore_applying ~= operation then return end
      state.restore_applying = nil
      synchronize_state()
      if err then
        notifications.error(err, "Harness undo")
        if err:find("preview is stale", 1, true) and interaction then
          client.request({ session_id = session_id, method = "session.exchange.rollback.prepare", params = { exchange_id = interaction.id } }, function(refreshed, failure)
            if not current() then return end
            if failure then notifications.error(failure, "Harness undo") return end
            confirm_restore_preview(refreshed, interaction)
          end)
        end
        return
      end
      if interaction then restore_composer_prompt(interaction.prompt or "") end
    end)
  end)
end

---@param interaction ForgeRollbackInteraction
---@param latest_exchange_id string
local function confirm_rollback(interaction, latest_exchange_id)
  local state = harness_state()
  local session_id, generation = state.session.id, client.host_get_generation()
  if state.busy or state.restore_applying then return end
  local choices = {
    { label = "Cancel", value = "cancel" },
    { label = "Conversation only", detail = "Keep all current files", value = "conversation" },
  }
  if interaction.checkpoint_before and not state.no_checkpoint then
    choices[#choices + 1] = { label = "Conversation and files", detail = "Restore this interaction's checkpoint", value = "workspace" }
  end
  open_choice_picker(state, "Rollback interaction", "Choose what to roll back", choices, function(choice)
    if harness_state() ~= state or not state.session or state.session.id ~= session_id
      or client.host_get_generation() ~= generation then return end
    if not choice or choice == "cancel" or state.busy or state.restore_applying then return end
    if choice == "workspace" then
      client.request({ session_id = session_id, method = "session.exchange.rollback.prepare", params = { exchange_id = interaction.id } }, function(preview, err)
        if harness_state() ~= state or not state.session or state.session.id ~= session_id
          or client.host_get_generation() ~= generation then return end
        if err then notifications.error(err, "Harness undo") return end
        confirm_restore_preview(preview, interaction)
      end)
      return
    end
    if choice ~= "conversation" then return end
    local operation = {}
    state.restore_applying = operation
    M.task_transition({ action = "undo", rollback = {
      scope = "conversation", exchange_id = interaction.id, latest_exchange_id = latest_exchange_id,
    } }, nil, function(applied)
      if harness_state() ~= state or not state.session or state.session.id ~= session_id
        or client.host_get_generation() ~= generation or state.restore_applying ~= operation then return end
      state.restore_applying = nil
      if not applied then return end
      restore_composer_prompt(interaction.prompt or "")
    end)
  end)
end

---Open settled interaction history before choosing a rollback scope.
local function open_exchange_undo_picker(recovery)
  local state = harness_state()
  if state.busy then
    notifications.warn("Cancel or finish the active turn before undoing an exchange", "Harness undo")
    return
  end
  local session_id, generation = state.session and state.session.id, client.host_get_generation()
  if not session_id then return end
  client.request({ session_id = session_id, method = "session.exchange.list", params = {} }, function(interaction_list, request_error)
    if harness_state() ~= state or not state.session or state.session.id ~= session_id
      or client.host_get_generation() ~= generation then return end
    if request_error then
      notifications.error(request_error, "Harness undo")
      return
    end
    local rollback_state = { complete = true, failed = true, cancelled = true, interrupted = true }
    local option_list = {}
    for index = #(interaction_list or {}), 1, -1 do
      local interaction = interaction_list[index]
      if rollback_state[interaction.state]
        and interaction.disposition == "current"
      then
        local prompt = vim.trim(tostring(interaction.prompt or ""):gsub("%s+", " "))
        option_list[#option_list + 1] = {
          id = interaction.id,
          label = "Exchange " .. tostring(interaction.ordinal),
          detail = tostring(interaction.state) .. " · " .. (prompt ~= "" and prompt or "[empty prompt]"),
          value = interaction,
        }
      end
    end
    if recovery and not recovery.pending then
      option_list[#option_list + 1] = { id = "undo-file-restore", label = "Undo last file restore",
        detail = "Preview restoring files from before the last rollback", value = { undo_file_restore = true } }
    end
    open_choice_picker(
      state,
      "Select Exchange",
      nil,
      option_list,
      function(interaction)
        if interaction.undo_file_restore then
          client.request({ session_id = session_id, method = "session.exchange.recovery", params = { action = "undo" } }, function(preview, err)
            if harness_state() ~= state or not state.session or state.session.id ~= session_id
              or client.host_get_generation() ~= generation then return end
            if err then notifications.error(err, "Harness recovery") return end
            confirm_restore_preview(preview)
          end)
        else
          confirm_rollback(interaction, interaction_list[#interaction_list].id)
        end
      end,
      "This session has no exchanges available to undo."
    )
  end)
end

---Open interaction history directly unless an unfinished file restore needs recovery.
function M.open_undo_picker()
  local state = harness_state()
  if state.busy or state.restore_applying then
    notifications.warn("Cancel or finish the active turn before restoring files", "Harness undo")
    return
  end
  if not state.session then return end
  local session_id, generation = state.session.id, client.host_get_generation()
  client.request({ session_id = session_id, method = "session.exchange.recovery", params = {} }, function(recovery, err)
    if harness_state() ~= state or not state.session or state.session.id ~= session_id
      or client.host_get_generation() ~= generation then return end
    if err then notifications.error(err, "Harness recovery") return end
    if not recovery or recovery == vim.NIL then open_exchange_undo_picker() return end
    if not recovery.pending then open_exchange_undo_picker(recovery) return end
    local choices = { { label = "Cancel", value = "cancel" } }
    choices[#choices + 1] = { label = "Continue interrupted restore", value = "continue" }
    choices[#choices + 1] = { label = "Restore pre-restore files", value = "undo" }
    open_choice_picker(state, "Harness restore", "An interrupted restore requires recovery.", choices, function(choice)
      if harness_state() ~= state or not state.session or state.session.id ~= session_id
        or client.host_get_generation() ~= generation then return end
      if choice ~= "undo" and choice ~= "continue" then return end
      if harness_state() ~= state or not state.session or state.session.id ~= session_id then return end
      client.request({ session_id = session_id, method = "session.exchange.recovery", params = { action = choice } }, function(result, failure)
        if harness_state() ~= state or not state.session or state.session.id ~= session_id
          or client.host_get_generation() ~= generation then return end
        if failure then notifications.error(failure, "Harness recovery") synchronize_state() return end
        if choice == "undo" then confirm_restore_preview(result) else synchronize_state() end
      end)
    end)
  end)
end

---Switch the transcript and composer target to one child-agent timeline or Main.
---@param selector string
function M.select_agent(selector)
  local state = harness_state()
  if selector:lower() == "main" then
    state.selected_agent_run_id = nil
  else
    local run, resolve_error = require("forge.views.harness.agent_catalog").resolve(state, selector)
    if not run then
      notifications.warn(resolve_error or ("No child agent matches " .. selector), "Harness agent")
      return
    end
    state.selected_agent_run_id = run.id
  end
  if state.presentation then state.presentation.select_agent(state.selected_agent_run_id) end
  M.render()
end

--- Admit a fresh child using a global profile and explicit task text.
---@param profile string|table
---@param text? string
function M.spawn_agent(profile, text)
  local state = harness_state()
  local request = type(profile) == "table" and profile or { profile = profile, text = text }
  if state.presentation then state.presentation.follow_tail() end
  client.request({ session_id = state.session.id, method = "session.agent.start", params = request }, function(result, request_error)
    if request_error then notifications.error(request_error, "Harness agent") end
    if result and result.warning then notifications.warn(result.warning, "Subagent profiles") end
    synchronize_state()
  end)
end

---Open the provider-backed child-agent timeline selector.
---@param action string
---@param run_id string?
---@param message string?
function M.agent_action(action, run_id, message)
  local state = harness_state()
  local params = { target = run_id, message = message }
  if action == "tools" then M.configure({ subagent_tools = state.subagent_tools ~= true }, true) return end
  if action == "message" or action == "followup" then
    if not message or message:match("^%s*$") then return end
  end
  local method = action == "message" and "session.agent.message.send" or "session.agent." .. action
  client.request({ session_id = state.session.id, method = method, params = params }, function(_, failure)
    if failure then notifications.error(failure, "Harness child " .. action) return end
    if message and not state.queue_edit and composer_text(state.composer_buf) == message then
      set_composer_text(state.composer_buf, "")
    end
    synchronize_state()
  end)
end

function M.open_agent_picker()
  local state = harness_state()
  require("forge.views.harness.agent_picker").open({
    host = picker_host(state),
    state_provider = harness_state,
    on_select = function(run_id)
      M.select_agent(run_id and run_id or "main")
    end,
    on_action = M.agent_action,
  })
end

--- Select an enabled profile and compose its task text.
---@param profile? string
function M.open_spawn_picker(profile)
  local state = harness_state()
  local session_id = state.session and state.session.id
  client.request({ method = "agent.profile.get", params = {} }, function(snapshot, failure)
    if not state.session or state.session.id ~= session_id then return end
    if failure then notifications.error(failure, "Subagent profiles") return end
    if snapshot.error then notifications.warn(snapshot.error, "Subagent profiles") end
    require("forge.views.harness.spawn_picker").open({
      host = picker_host(state), profile = profile, document = snapshot.document, on_spawn = M.spawn_agent,
    })
  end)
end

function M.open_session_picker()
  require("forge.views.harness.session_picker").open(picker_host(harness_state()))
end

---List provider-owned background shells and terminate only the selected shell.
function M.open_background_picker()
  local state = harness_state()
  local session_id, generation = state.session.id, client.host_get_generation()
  local function current()
    return state.session and state.session.id == session_id and client.host_get_generation() == generation
  end
  client.request({ session_id = session_id, method = "session.document", params = { operation = "background_terminals" } }, function(inventory, failure)
    if not current() then return end
    if failure then notifications.error(failure, "Select Background Terminal") return end
    if not inventory.supported then
      notifications.warn("The current backend does not support background terminals", "ForgeHarness")
      return
    end
    local options = {}
    for _, terminal in ipairs(inventory.terminal or {}) do
      options[#options + 1] = { label = require("forge.render.harness.command").display(terminal.command):gsub("%s+", " "), detail = "Terminal " .. terminal.id, value = terminal.id }
    end
    open_choice_picker(state, "Terminate Background Terminal", nil, options, function(id)
      if not current() then return end
      client.request({ session_id = session_id, method = "session.document", params = { operation = "terminate_terminal", id = id } }, function(_, terminate_failure)
        if not current() then return end
        if terminate_failure then notifications.error(terminate_failure, "Select Background Terminal") end
        if state.presentation and state.presentation.terminals then state.presentation.terminals.refresh() end
      end)
    end, "No background terminals are running.")
  end)
end

local function finish_queue_edit(state, text, unqueue)
  local edit = state.queue_edit
  queue_editor.remember(state, text)
  edit.commit_text, edit.commit_config = {}, {}
  local source = require("forge.views.harness.completion.command_source")
  for index, entry in ipairs(state.queue) do
    local draft = edit.drafts[entry]
    if draft and draft ~= "" and not (unqueue and entry == edit.entry) then
      local next_config, prompt, failure = model_command(draft)
      if not source.accepts_prompt(draft) or not source.queues_execution(draft) then
        failure = "Use a prompt, or clear the text to delete this queued message"
      end
      if failure then
        edit.entry = entry
        set_composer_text(state.composer_buf, draft)
        notifications.warn("Queued message " .. index .. ": " .. failure, "ForgeHarness")
        M.refresh_winbar()
        return
      end
      if next_config then edit.commit_text[entry], edit.commit_config[entry] = prompt, next_config end
    end
  end
  local selected_attachments = edit.attachments[edit.entry]
  local original, failure, attachments = queue_editor.finish(state, unqueue and "" or text)
  if failure then notifications.warn(failure, "ForgeHarness") return end
  if unqueue then original, attachments = text, selected_attachments
  elseif text ~= "" then prompt_history.record(text) end
  set_composer_text(state.composer_buf, original, attachments)
  if state.composer_win and vim.api.nvim_win_is_valid(state.composer_win) then
    layout.resize_composer(state.composer_buf, state.composer_win)
    if unqueue then vim.api.nvim_set_current_win(state.composer_win) end
  end
  M.refresh_winbar()
  if state.presentation then state.presentation.follow_tail() end
end

function M.submit()
  require("forge.text").invalidate(harness_state().composer_buf)
  if not attachments_ready(harness_state().composer_buf) then return end
  if harness_state().host_error then notifications.error(harness_state().execution_notice, "ForgeHarness") return end
  harness_state().execution_notice = nil
  local state = harness_state()
  if state.switching_backend then return end
  local text = composer_text(state.composer_buf)
  if state.queue_edit then finish_queue_edit(state, text) return end
  if text == "/task refresh" then
    state.state_sync_retry = nil
    synchronize_state()
    return
  end
  if text == "" then return end
  if text == "/capabilities" then
    set_composer_text(state.composer_buf, "")
    require("forge.views.harness.capability_picker").open(picker_host(state), state.session.id)
    return
  end
  local accepted, admission_failure = require("forge.views.harness.completion.command_source").accepts_prompt(text)
  if not accepted then notifications.warn(assert(admission_failure), "ForgeHarness") return end
  if text == "/task" or text == "/plan" or text == "/execute" then
    set_composer_text(state.composer_buf, "")
    if text == "/task" then task_control.open(state, picker_host(state), M.task_transition)
    else task_control.plans(state, picker_host(state), text == "/execute", M.task_transition) end
    return
  end
  if text == "/task new" then
    set_composer_text(state.composer_buf, "")
    task_control.new(picker_host(state), function(kind)
      if kind == "execute" then task_control.plans(state, picker_host(state), true, M.task_transition)
      else restore_composer_prompt("/" .. kind .. " ") end
    end)
    return
  end
  if text == "/plan accept on" or text == "/plan accept off" then
    M.task_transition({ action = "plan_accept", enabled = text == "/plan accept on" }, text)
    return
  end
  local task_action = ({ ["/task resume"] = "resume", ["/task pause"] = "pause", ["/task clear"] = "clear",
    ["/goal resume"] = "resume", ["/goal pause"] = "pause", ["/goal clear"] = "clear",
    ["/plan cancel"] = "clear", ["/plan retry"] = "resume" })[text]
  local task_kind, task_text = text:match("^/(%a+)%s+(.+)$")
  if task_action or (task_kind == "plan" and task_text ~= "cancel" and task_text ~= "retry") or task_kind == "goal" or text == "/execute last" then
    local action = task_action and { action = task_action }
      or text == "/execute last" and { action = "execute", plan_id = "last" }
      or { action = task_kind, text = task_text }
    prompt_history.record(text)
    M.task_transition(action, text)
    return
  end
  if text == "/plan cancel" then
    set_composer_text(state.composer_buf, "")
    M.abort_plan()
    return
  end
  if text == "/recap" then
    set_composer_text(state.composer_buf, "")
    recap.request(state, function()
      render_status_hint(state)
    end)
    return
  end
  if text == "/bg" then
    set_composer_text(state.composer_buf, "")
    M.open_background_picker()
    return
  end
  local _, model_prompt, model_failure = model_command(text)
  if model_failure then report_configuration_error(model_failure) return end
  if model_prompt and model_prompt ~= "" then M.queue_submit() return end
  prompt_history.record(text)
  local goal_control = ({ ["/goal pause"] = "session.goal.pause", ["/goal clear"] = "session.goal.clear" })[text]
  if goal_control then
    local session_id = state.session.id
    set_composer_text(state.composer_buf, "")
    client.request({ session_id = session_id, method = goal_control, params = {} }, function(result, request_error)
      if request_error then
        notifications.error(request_error, "Harness goal")
        return
      end
      if state.session and state.session.id == session_id then
        synchronize_state()
      end
    end)
    return
  end
  local permission_change = {
    ["/read full"] = { kind = "toggle_read_scope" },
    ["/write"] = { kind = "toggle_write_enabled" },
    ["/write full"] = { kind = "toggle_write_scope" },
    ["/ask"] = { kind = "set_approval", mode = "ask" },
    ["/deny"] = { kind = "set_approval", mode = "deny" },
    ["/allow"] = { kind = "set_approval", mode = "allow" },
    ["/yolo"] = { kind = "enable_yolo" },
  }
  if permission_change[text] then
    set_composer_text(state.composer_buf, "")
    M.task_transition({ action = "permission", change = permission_change[text] })
    return
  end
  local command = text:match("^(/%S+)")
  if vim.tbl_contains({ "/read", "/write", "/mode", "/ask", "/deny", "/allow", "/yolo" }, command) then
    report_configuration_error("Use /read full, /write, /write full, /ask, /deny, /allow, or /yolo")
    return
  end
  if text == "/effort" then
    set_composer_text(state.composer_buf, "")
    M.select_effort()
    return
  end
  if text == "/model" then
    set_composer_text(state.composer_buf, "")
    M.select_model()
    return
  end
  if text == "/backend" then
    set_composer_text(state.composer_buf, "")
    M.select_backend()
    return
  end
  if text == "/checks" then
    set_composer_text(state.composer_buf, "")
    require("forge.views.harness.checks").open(state, picker_host(state))
    return
  end
  local check_editing = text:match("^/checks%s+edit%s+(%S+)$")
  if check_editing == "on" or check_editing == "off" then
    set_composer_text(state.composer_buf, "")
    local session_id = state.session.id
    client.request({ session_id = session_id, method = "session.check.editing.set", params = { enabled = check_editing == "on" } }, function(result, failure)
      if not state.session or state.session.id ~= session_id then return end
      if failure then
        notifications.error(failure, "Harness checks")
      else
        state.check_editing = result.check_editing == true
        M.refresh_winbar()
      end
      synchronize_state()
    end)
    return
  end
  if text == "/skills" then
    set_composer_text(state.composer_buf, "")
    M.open_skill_picker()
    return
  end
  if text == "/mcp" then
    set_composer_text(state.composer_buf, "")
    M.open_mcp_picker()
    return
  end
  if text == "/rename" then
    set_composer_text(state.composer_buf, "")
    M.rename_session("")
    return
  end
  if text == "/fork" then
    set_composer_text(state.composer_buf, "")
    M.fork_session()
    return
  end
  if text == "/new" or text == "/clear" then
    set_composer_text(state.composer_buf, "")
    require("forge.views.harness").new_session()
    return
  end
  if text == "/questions" then
    set_composer_text(state.composer_buf, "")
    M.present_plan_question(true)
    return
  end
  local agent_tools_action = text:match("^/agents%s+(%S+)%s*$")
  if agent_tools_action == "on" or agent_tools_action == "off" then
    set_composer_text(state.composer_buf, "")
    M.configure({ subagent_tools = agent_tools_action == "on" }, true)
    return
  end
  local edit_profile = text:match("^/agents%s+edit%s+(%S+)%s*$")
  if text:match("^/agents%s+edit%s*$") or edit_profile then
    set_composer_text(state.composer_buf, "")
    require("forge.views.harness.subagent_profiles").open(state, picker_host(state), edit_profile)
    return
  elseif text:match("^/agents%s+edit%s+") then
    set_composer_text(state.composer_buf, "")
    notifications.warn("Use /agents edit or /agents edit <profile>", "ForgeHarness")
    return
  end
  if text == "/agents" then
    set_composer_text(state.composer_buf, "")
    M.open_agent_picker()
    return
  end
  if text:match("^/agents%s+spawn%s*$") then
    set_composer_text(state.composer_buf, "")
    M.open_spawn_picker()
    return
  end
  if text == "/sessions" then
    set_composer_text(state.composer_buf, "")
    M.open_session_picker()
    return
  end
  if text == "/undo" then
    set_composer_text(state.composer_buf, "")
    M.open_undo_picker()
    return
  end
  local spawn_profile, spawn_task = text:match("^/agents%s+spawn%s+(%S+)%s+(.+)$")
  if spawn_profile and spawn_task then
    set_composer_text(state.composer_buf, "")
    M.spawn_agent(spawn_profile, spawn_task)
    return
  end
  local spawn_profile_only = text:match("^/agents%s+spawn%s+(%S+)%s*$")
  if spawn_profile_only then
    set_composer_text(state.composer_buf, "")
    M.open_spawn_picker(spawn_profile_only)
    return
  end
  local agent_selector = text:match("^/agents%s+(%S+)%s*$")
  if agent_selector then
    set_composer_text(state.composer_buf, "")
    M.select_agent(agent_selector)
    return
  elseif text:match("^/agents%s+") then
    set_composer_text(state.composer_buf, "")
    notifications.warn("Use /agents, /agents on|off, /agents edit [profile], or /agents spawn <profile> <text>", "ForgeHarness")
    return
  end
  if text == "/compact" then
    set_composer_text(state.composer_buf, "")
    if state.capability.native_compact ~= true then
      notifications.warn("The current backend does not support manual context compaction", "ForgeHarness")
      return
    end
    M.compact()
    return
  end
  local session_name = text:match("^/rename%s+(.+)$")
  if session_name then
    set_composer_text(state.composer_buf, "")
    M.rename_session(vim.trim(session_name))
    return
  end
  local fork_name = text:match("^/fork%s+(.+)$")
  if fork_name then
    set_composer_text(state.composer_buf, "")
    M.fork_session(vim.trim(fork_name))
    return
  end
  local new_name = text:match("^/new%s+(.+)$")
  if new_name then
    set_composer_text(state.composer_buf, "")
    require("forge.views.harness").new_session(vim.trim(new_name))
    return
  end
  local model, model_effort = text:match("^/model%s+(%S+)%s+(%S+)$")
  if not model then model = text:match("^/model%s+(%S+)$") end
  if model then
    set_composer_text(state.composer_buf, "")
    if state.capability.model_selection ~= true then
      notifications.warn("The current backend does not support model selection", "ForgeHarness")
      return
    end
    if model_effort and state.capability.effort_selection ~= true then
      notifications.warn("The current backend does not support reasoning effort selection", "ForgeHarness")
      return
    end
    M.configure({ model = model, effort = model_effort }, true)
    return
  end
  local effort = text:match("^/effort%s+(%S+)$")
  if effort then
    set_composer_text(state.composer_buf, "")
    if state.capability.effort_selection ~= true then
      notifications.warn("The current backend does not support reasoning effort selection", "ForgeHarness")
      return
    end
    M.configure({ effort = effort }, true)
    return
  end
  if text == "/fast" or text == "/ultrafast" then
    set_composer_text(state.composer_buf, "")
    M.toggle_service_tier(text:sub(2))
    return
  end
  if text:match("^/fast%s") or text:match("^/ultrafast%s") then
    set_composer_text(state.composer_buf, "")
    state.configuration_error = "Use /fast or /ultrafast without arguments to toggle the service tier"
    M.refresh_winbar()
    return
  end
  if state.busy and not state.checks_running and queue_only_commands[text:match("^%s*(%S+)")] then
    M.queue_submit()
    return
  end
  if text == "/config" or text == "/log" or text:match("^/log%s") then
    set_composer_text(state.composer_buf, "")
    local settings = require("forge.views.harness.settings")
    if text == "/config" then settings.open(state, picker_host(state))
    else settings.log(state.session.id, text:match("^/log%s+(.+)$")) end
    return
  end
  if state.selected_agent_run_id then
    prompt_history.record(text)
    M.agent_action("followup", state.selected_agent_run_id, text)
    return
  end
  begin_request(text, true)
end

submit_immediate = function(state, text, attachments)
  attachments = attachments or attachment.capture(state.composer_buf).attachments
  if state.selected_agent_run_id then
    M.agent_action("message", state.selected_agent_run_id, text)
    return
  end
  recap.clear(state)
  begin_request(text, true, attachments)
end

---Send the composer into the active provider turn without creating a new interaction.
function M.steer_submit()
  local state = harness_state()
  if not attachments_ready(state.composer_buf) then return end
  if state.queue_edit then M.submit() return end
  local text = composer_text(state.composer_buf)
  if text == "" then return end
  if state.selected_agent_run_id then
    prompt_history.record(text)
    M.agent_action("message", state.selected_agent_run_id, text)
    return
  end
  prompt_history.record(text)
  submit_immediate(state, text)
end

---Queue a follow-up independently of active waits and the selected child timeline.
function M.queue_submit()
  if not attachments_ready(harness_state().composer_buf) then return end
  if harness_state().host_error then notifications.error(harness_state().execution_notice, "ForgeHarness") return end
  harness_state().execution_notice = nil
  local state = harness_state()
  if state.queue_edit then finish_queue_edit(state, composer_text(state.composer_buf), true) return end
  if state.switching_backend then return end
  local text = composer_text(state.composer_buf)
  if text == "" then return end
  local accepted, admission_failure = require("forge.views.harness.completion.command_source").accepts_prompt(text)
  if not accepted then notifications.warn(assert(admission_failure), "ForgeHarness") return end
  if not require("forge.views.harness.completion.command_source").queues_execution(text) then
    M.submit()
    return
  end
  local captured_config, prompt, failure = model_command(text)
  if failure then report_configuration_error(failure) return end
  prompt_history.record(text)
  local attachments = attachment.capture(state.composer_buf).attachments
  set_composer_text(state.composer_buf, "")
  input_state.submit(state, prompt or text, attachments, "queue", captured_config)
  M.refresh_winbar()
end

function M.edit_queued(direction)
  local state = harness_state()
  if not attachments_ready(state.composer_buf) then return end
  local draft = composer_text(state.composer_buf)
  local text, failure, attachments = queue_editor.move(state, draft, direction)
  if failure then notifications.warn(failure, "ForgeHarness") return end
  if not text then return end
  vim.cmd.stopinsert()
  set_composer_text(state.composer_buf, text, attachments)
  if state.composer_win and vim.api.nvim_win_is_valid(state.composer_win) then
    layout.resize_composer(state.composer_buf, state.composer_win)
    vim.api.nvim_set_current_win(state.composer_win)
  end
  M.refresh_winbar()
  if state.presentation then state.presentation.follow_tail() end
end

---@param delta integer
function M.jump_prompt(delta)
  local state = harness_state()
  if vim.api.nvim_get_current_buf() ~= state.transcript_buf then
    if not (state.transcript_win and vim.api.nvim_win_is_valid(state.transcript_win)) then return end
    vim.api.nvim_set_current_win(state.transcript_win)
  end
  if state.presentation then state.presentation.navigate_prompt(delta < 0) end
end

function M.toggle_activity()
  local state = harness_state()
  if vim.api.nvim_get_current_buf() ~= state.transcript_buf then
    if not (state.transcript_win and vim.api.nvim_win_is_valid(state.transcript_win)) then return end
    vim.api.nvim_set_current_win(state.transcript_win)
  end
  if state.presentation and state.presentation.toggle_heading(vim.api.nvim_get_current_win()) then return end
end

---@param direction integer
function M.change_effort(direction)
  local state = harness_state()
  local current = selected_setting(state, "effort")
  local session_id = state.session and state.session.id
  require("forge.views.harness.provider_catalog").models(false, function(model_list, failure)
    if failure then notifications.error(failure, "Harness effort") return end
    if harness_state() ~= state or not state.session or state.session.id ~= session_id then return end
    local model_id = selected_setting(state, "model")
    for _, model in ipairs(model_list or {}) do
      if model.id == model_id or (model_id == "default" and model.is_default) then
        local choice = model.reasoning or {}
        if #choice == 0 then return end
        local index = 1
        for candidate_index, candidate in ipairs(choice) do if candidate == current then index = candidate_index end end
        index = math.max(1, math.min(#choice, index + direction))
        M.configure({ effort = choice[index] }, true)
        return
      end
    end
  end)
end

function M.select_effort()
  if harness_state().capability.effort_selection ~= true then
    harness_state().configuration_error = "The current backend does not support reasoning effort selection"
    M.refresh_winbar()
    return
  end
  local detail_list = {
    minimal = "Fastest reasoning for straightforward work.",
    low = "Light reasoning for routine changes.",
    medium = "Balanced reasoning for everyday work.",
    high = "Deeper reasoning for complex changes.",
    xhigh = "Maximum reasoning for the hardest work.",
  }
  local state = harness_state()
  local session_id = state.session and state.session.id
  local loading_owner = "harness-effort-loading-" .. tostring(session_id)
  local loading = state.model_backend ~= (state.session and state.session.backend) or type(state.model_list) ~= "table"
  if loading then
    picker.open({ owner = loading_owner, host = picker_host(state), page_list = { {
      id = "effort-loading", title = "Select Reasoning Effort", option_list = {}, empty_text = "Loading reasoning options...", footer = "q close",
    } }, on_confirm = function() return false end })
  end
  require("forge.views.harness.provider_catalog").models(false, function(model_list, failure)
    if failure then
      if loading and picker.is_open(loading_owner) then picker.close() end
      notifications.error(failure, "Harness effort")
      return
    end
    if harness_state() ~= state or not state.session or state.session.id ~= session_id then return end
    if loading and not picker.is_open(loading_owner) then return end
    local model_id = selected_setting(state, "model")
    local choice = {}
    for _, model in ipairs(model_list or {}) do
      if model.id == model_id or (model_id == "default" and model.is_default) then choice = model.reasoning or {} break end
    end
    local options = vim.tbl_map(function(effort)
      return { label = effort, detail = detail_list[effort] or "Provider-advertised reasoning choice", value = effort }
    end, choice)
    open_choice_picker(state, "Select Reasoning Effort", nil, options, function(effort)
      M.configure({ effort = effort }, true)
    end, "The selected model advertises no reasoning choices.")
  end)
end

---@param tier "default"|"fast"|"ultrafast"
function M.configure_service_tier(tier)
  local state = harness_state()
  if (tier == "fast" and state.capability.fast_mode ~= true)
    or (tier == "ultrafast" and state.capability.ultrafast_mode ~= true)
  then
    state.configuration_error = "The current backend does not support " .. tier .. " mode"
    M.refresh_winbar()
    return
  end
  if tier == "default" then M.configure({ service_tier = tier }) return end
  local session_id = state.session and state.session.id
  local selected_model = selected_setting(state, "model")
  local function apply(models, failure)
    if not state.session or state.session.id ~= session_id or selected_setting(state, "model") ~= selected_model then return end
    local options = require("forge.views.harness.model_options")
    local model = options.find(models, selected_model)
    if failure or not vim.tbl_contains(options.speeds(model, state.capability), tier) then
      state.configuration_error = failure or ("Model " .. tostring(selected_model) .. " does not support " .. tier .. " mode")
      notifications.error(state.configuration_error, "Harness speed")
      M.refresh_winbar()
      return
    end
    M.configure({ service_tier = tier })
  end
  if state.model_backend == state.session.backend and state.model_list then apply(state.model_list)
  else require("forge.views.harness.provider_catalog").models(false, apply) end
end

---@param tier "fast"|"ultrafast"
function M.toggle_service_tier(tier)
  local state = harness_state()
  M.configure_service_tier(selected_setting(state, "service_tier") == tier and "default" or tier)
end

function M.select_model(on_confirm)
  local state = harness_state()
  on_confirm = type(on_confirm) == "function" and on_confirm or function(value)
    local options = require("forge.views.harness.model_options")
    local speeds = options.speeds(options.find(state.model_list, value.model), state.capability or {})
    value.service_tier = vim.tbl_contains(speeds, selected_setting(state, "service_tier"))
      and selected_setting(state, "service_tier") or "default"
    M.configure(value)
  end
  local backend = state.session and state.session.backend or config.options.harness.backend
  local session_id = state.session and state.session.id
  local loading_owner = "harness-model-loading-" .. tostring(session_id)
  local loading = state.model_backend ~= backend or type(state.model_list) ~= "table"
  local function is_current()
    return harness_state() == state and state.session and state.session.id == session_id
      and state.session.backend == backend
      and (not loading or picker.is_open(loading_owner))
  end
  if state.capability.model_selection ~= true then
    notifications.warn("The current backend does not support model selection", "ForgeHarness")
    return
  end
  if loading then
    picker.open({ owner = loading_owner, host = picker_host(state), page_list = { {
      id = "model-loading", title = "Select Model", option_list = {}, empty_text = "Loading models...", footer = "q close",
    } }, on_confirm = function() return false end })
  end
  local function open_model_picker(model_list)
    if not is_current() then return end
    loading = false
    if type(model_list) ~= "table" or #model_list == 0 then
      local current = harness_state().session and harness_state().session.model or config.options.harness.model
      picker.open({
        host = picker_host(harness_state()),
        initial_input_kind = "other",
        page_list = {
          {
            id = "custom-model",
            title = "Enter Model",
            column_headers = { "Model", "Details" },
            option_list = { { label = "Model", value = current, input_kind = "other" } },
            allow_input = true,
            input_height = 3,
            footer = "C-s apply  go options  q close",
          },
        },
        on_confirm = function(result) on_confirm({ model = result.text }) end,
      })
      return
    end
    local state = harness_state()
    model_picker.open({
      host = picker_host(state),
      model_list = model_list,
      backend = backend,
      is_current = is_current,
      current_model = selected_setting(state, "model") == "default" and state.session.resolved_model
        or selected_setting(state, "model"),
      on_confirm = on_confirm,
    })
  end
  model_pins.refresh(backend, function(_, pin_error)
    if pin_error then
      if loading and picker.is_open(loading_owner) then picker.close() end
      notifications.error("Failed to load Harness model pins: " .. pin_error, "Harness model")
      return
    end
    if not is_current() then return end
    require("forge.views.harness.provider_catalog").models(false, function(model_list, request_error)
      if request_error then
        if loading and picker.is_open(loading_owner) then picker.close() end
        notifications.error(request_error, "Harness model")
        return
      end
      if not is_current() then return end
      state.model_backend = backend
      state.model_list = vim.deepcopy(model_list or {})
      open_model_picker(state.model_list)
    end)
  end)
end

function M.open_skill_picker()
  local state = harness_state()
  if not (state.capability.catalog and state.capability.catalog.skill) then
    notifications.warn("The current backend does not advertise provider skills", "Harness skills")
    return
  end
  provider_picker.open_skills({
    host = picker_host(state),
    is_current = function() return harness_state() == state end,
    on_insert = function(text) set_composer_text(harness_state().composer_buf, text) end,
  })
end

function M.open_mcp_picker()
  local state = harness_state()
  local session_id = state.session and state.session.id
  local function owns_session()
    local current = harness_state()
    return current == state and (current.session and current.session.id) == session_id
  end
  if not (state.capability.catalog and state.capability.catalog.mcp) then
    notifications.warn("The current backend does not advertise MCP management", "Harness MCP")
    return
  end
  provider_picker.open_mcp({
    host = picker_host(state),
    is_current = owns_session,

  })
end

function M.select_backend()
  local state = harness_state()
  local harness = require("forge.views.harness")
  if not harness.backend_switch_available() then
    notifications.warn("Finish or cancel pending work before switching providers", "Harness backend")
    return
  end
  local source_session_id = state.session.id
  local function current_source()
    return session.harness == state and state.session and state.session.id == source_session_id
  end
  local current = state.session and state.session.backend or config.options.harness.backend
  local option_list = {}
  for backend, backend_config in pairs(config.options.harness.backends) do
    if backend_config.selectable ~= false then
      option_list[#option_list + 1] = {
        id = backend,
        label = backend_config.label,
        detail = backend_config.detail .. (backend == current and " (current)" or ""),
        value = backend,
      }
    end
  end
  table.sort(option_list, function(left, right) return left.label < right.label end)
  open_choice_picker(state, "Select Harness", nil, option_list,
    function(backend)
      if backend == current or not current_source() then return end
      local function destination_picker()
        if not current_source() then return end
        open_choice_picker(state, "Select Chat", config.options.harness.backends[backend].label, {
          { id = "new", label = "New chat", value = "new" },
          { id = "resume", label = "Resume session…", value = "resume" },
        }, function(destination)
          if not current_source() then return end
          if destination == "new" then
            harness.switch_backend(backend, { kind = "new" })
          else
            require("forge.views.harness.session_picker").open(picker_host(state), {
              backend = backend,
              on_select = function(entry)
                if current_source() then harness.switch_backend(backend, { kind = "resume", session_id = entry.id }) end
              end,
              on_cancel = destination_picker,
            })
          end
        end)
      end
      destination_picker()
    end)
end

function M.resolve_runtime_model()
  local state = harness_state()
  local backend = state.session and state.session.backend
  require("forge.views.harness.provider_catalog").models(true, function(model_list, request_error)
    if request_error then notifications.error("Failed to resolve Harness model: " .. request_error, "ForgeHarness") end
    if request_error then return end
    state.model_backend = backend
    state.model_list = vim.deepcopy(model_list or {})
  end)
end

---@param action table
---@param submitted_text? string
---@param completed? fun(success: boolean)
---@param method? string
function M.task_transition(action, submitted_text, completed, method)
  local state = harness_state()
  local policy_change = action.action == "plan_accept"
  if submitted_text then action.attachments = attachment.capture(state.composer_buf).attachments end
  if state.host_error then notifications.error(state.execution_notice or state.host_error, "Harness task") return end
  if not state.session then notifications.warn("Harness is still initializing", "Harness task") return end
  if action.action == "pause" then
    state.queue_paused = true
  elseif vim.tbl_contains({ "resume", "clear", "chat", "plan", "goal", "execute" }, action.action) then
    state.queue_paused = false
  end
  if action.action ~= "configure" and not policy_change then
    state.task_config = nil
    if state.configuration_completion then
      state.configuration_completion.complete(false)
      state.configuration_completion = nil
    end
  end
  local current = task_control.current(state)
  if action.action == "resume" and current and task_control.status(current) == "running" then
    input_state.perform(state, { kind = "resume" })
    return
  end
  if not policy_change then set_busy(true) end
  task_control.transition(state, action, M.render, function(failure, background)
    if background then
      if completed then completed(not failure) end
      return
    end
    if harness_state() ~= state then
      if not policy_change then state.busy = false end
      if state.presentation and state.presentation.transcript then
        state.presentation.status_view:resize()
      end
      synchronize_state(nil, state)
      return
    end
    if not policy_change then
      set_busy(false)
      state.cancel_requested = false
    end
    if completed then completed(not failure) end
    synchronize_state(function(_, sync_failure)
      if sync_failure then return end
      M.render()
    end)
  end, function()
    if submitted_text and harness_state() == state and composer_text(state.composer_buf) == submitted_text then
      set_composer_text(state.composer_buf, "")
    end
  end, method)
  M.refresh_winbar()
end

function M.toggle_write()
  M.task_transition({ action = "permission", change = { kind = "toggle_write_enabled" } })
end

---@param next_config table
---@param validate_selection? boolean
---@param completed? fun(applied: boolean)
configure_now = function(next_config, validate_selection, completed)
  local state = harness_state()
  local deferred
  if state.configuration_completion then
    deferred = state.configuration_completion.deferred
  elseif state.task_operation and state.task_operation.action == "configure" then
    deferred = state.task_operation.configuration_deferred == true
  else
    deferred = state.busy == true
  end
  local request_config = vim.tbl_extend("force", state.task_config or {}, next_config, { validate = validate_selection == true })
  state.task_config = request_config
  state.configuring = true
  state.configuring_config = request_config
  state.configuration_error = nil
  local finished = false
  local configuration = { deferred = deferred, complete = function(applied)
    if finished then return end
    finished = true
    if state.configuring_config == request_config then
      state.configuring = false
      state.configuring_config = nil
    end
    if applied then
      if next_config.model and not state.session.resolved_model then M.resolve_runtime_model() end
    end
    M.refresh_winbar()
    if completed then completed(applied) end
  end }
  state.configuration_completion = configuration
  M.task_transition({ action = "configure", config = vim.tbl_extend("force", {}, next_config, { validate = validate_selection == true }) }, nil, function(applied)
    if state.task_config == request_config then state.task_config = nil end
    if state.configuration_completion == configuration then state.configuration_completion = nil end
    configuration.complete(applied)
  end)
end

---@param next_config table
---@param validate_selection? boolean
---@param completed? fun(applied: boolean)
function M.configure(next_config, validate_selection, completed)
  local state = harness_state()
  local only_field = next(next_config)
  if (only_field == "check_timing" or only_field == "automatic_checks") and next(next_config, only_field) == nil then
    local session_id, epoch = state.session.id, state.runtime_epoch
    state.check_settings_request = (state.check_settings_request or 0) + 1
    local revision = state.check_settings_request
    client.request({ session_id = session_id, method = "session.configure", params = next_config }, function(result, failure)
      if not state.session or state.session.id ~= session_id or state.runtime_epoch ~= epoch then return end
      if state.check_settings_request ~= revision then
        if completed then completed(false) end
        return
      end
      if failure then
        report_configuration_error(failure)
        if completed then completed(false) end
        return
      end
      state.check_timing = result.check_timing
      state.automatic_checks = result.automatic_checks == true
      if result.checks_deferred ~= nil then state.checks_deferred = result.checks_deferred end
      M.render()
      if completed then completed(true) end
    end)
    return
  end
  if only_field == "status_thoughts" and next(next_config, only_field) == nil then
    local session_id = state.session.id
    local generation = client.host_get_generation()
    state.status_thoughts_request = (state.status_thoughts_request or 0) + 1
    local request_revision = state.status_thoughts_request
    client.request({ session_id = session_id, method = "session.configure", params = next_config }, function(result, failure)
      if harness_state() ~= state or not state.session or state.session.id ~= session_id
        or client.host_get_generation() ~= generation then return end
      if state.status_thoughts_request ~= request_revision then
        if completed then completed(false) end
        return
      end
      if failure then
        report_configuration_error(failure)
        if completed then completed(false) end
        return
      end
      state.status_thoughts = result.status_thoughts == true
      M.render()
      if completed then completed(true) end
    end)
    return
  end
  local timeout_only = next(next_config) ~= nil
  for field in pairs(next_config) do
    if field ~= "approval_timeout" and field ~= "answer_timeout" then timeout_only = false end
  end
  if timeout_only then
    local session_id = state.session.id
    client.request({ session_id = session_id, method = "session.input.timeout.configure", params = next_config }, function(result, failure)
      if failure then
        report_configuration_error(failure)
        if completed then completed(false) end
        return
      end
      if state.session.id == session_id then
        state.session.approval_timeout = result.approval_timeout
        state.session.answer_timeout = result.answer_timeout
      end
      if completed then completed(true) end
    end)
    return
  end
  configure_now(next_config, validate_selection, completed)
end

local function close()
  local state = harness_state()
  queue_editor.remember(state, composer_text(state.composer_buf))
  local workspace = require("forge.views.harness.workspace")
  workspace.release(state)
  if state.presentation then
    local ok, closed = pcall(state.presentation.close)
    if not ok or not closed then
      workspace.attach(state)
      if not ok then error(closed, 0) end
      return
    end
  end
  state.presentation = nil
  recap.clear(state)
  local tab_count = vim.fn.tabpagenr("$")
  tabline.clear(state.timeline_tab)
  if tab_count > 1 then
    vim.cmd("tabclose")
  else
    if state.composer_win and vim.api.nvim_win_is_valid(state.composer_win) then
      vim.api.nvim_win_close(state.composer_win, true)
    end
    vim.cmd("enew")
  end
  state.transcript_win = nil
  state.composer_win = nil
end

---@param result table
function M.activate_snapshot(result)
  local state = harness_state()
  local previous_session = state.session and state.session.id
  if not snapshot.apply(state, result) then return end
  if state.input and state.input.session_id == state.session.id then input_state.refresh(state) end
  state.initial_snapshot_pending = nil
  require("forge.views.harness.health").watch(state, function()
    if harness_state() == state then M.render() end
  end)
  if previous_session ~= state.session.id then state.queue, state.queue_edit, state.queue_paused = {}, nil, false end
  M.render()
  M.resolve_runtime_model()
  if state.active_elicitation and state.active_elicitation.elicitation then
    state.presented_question_key = nil
    vim.schedule(M.present_plan_question)
  end
  if #state.approval > 0 then vim.schedule(M.present_approval) end
end

---@return ForgeViewCommandSet
function M.command_set()
  local set = command_set.new()
  command_set.register(set, "visual_line_with_gutter", function()
    local state = harness_state()
    local presentation = state.presentation
    if vim.api.nvim_get_current_buf() ~= state.transcript_buf or not presentation
      or not presentation.ready or presentation.closed or presentation.failure then return end
    presentation.selection.start()
  end)
  command_set.register(set, "edit_queued", function() M.edit_queued(-1) end)
  command_set.register(set, "next_queued", function() M.edit_queued(1) end)
  command_set.register(set, "submit", M.submit)
  command_set.register(set, "queue", M.queue_submit)
  command_set.register(set, "steer", M.steer_submit)
  command_set.register(set, "retry_input", function()
    local state = harness_state()
    if state.queue_edit then
      notifications.error("Finish editing the queued message before retrying", "Harness message")
      return
    end
    for _, entry in ipairs(state.queue or {}) do
      if entry.delivery == "failed" or entry.delivery == "unknown" then
        input_state.retry(state, entry)
        return
      end
    end
  end)
  command_set.register(set, "cancel", M.cancel_turn)
  command_set.register(set, "toggle_write", M.toggle_write)
  command_set.register(set, "previous_prompt", function() M.jump_prompt(-1) end)
  command_set.register(set, "next_prompt", function() M.jump_prompt(1) end)
  command_set.register(set, "toggle_activity", M.toggle_activity)
  command_set.register(set, "open_artifact", M.open_artifact_picker)
  command_set.register(set, "status_details", function()
    local presentation = harness_state().presentation
    if presentation then presentation.status_view:open_details() end
  end)
  command_set.register(set, "abort_plan", M.abort_plan)
  command_set.register(set, "agent", M.open_agent_picker)
  command_set.register(set, "sessions", M.open_session_picker)
  command_set.register(set, "background", M.open_background_picker)
  command_set.register(set, "open_timeline", M.open_timeline_entry)
  for _, action in ipairs({ "toggle_visibility", "toggle_presentation" }) do
    command_set.register(set, action, function()
      local presentation = harness_state().presentation
      if presentation then presentation.toggle_diff(action) end
    end)
  end
  command_set.register(set, "reopen_question", M.reopen_question)
  command_set.register(set, "model", M.select_model)
  command_set.register(set, "effort_down", function() M.change_effort(-1) end)
  command_set.register(set, "effort_up", function() M.change_effort(1) end)
  command_set.register(set, "close", close)
  command_set.register(set, "help", function() keymaps.show_view_help("harness", set, "ForgeHarness") end)
  return set
end

---@param buf integer
function M.attach_transcript(buf)
  if not buf or not vim.api.nvim_buf_is_valid(buf) then return end
  local state = harness_state()
  for _, key in ipairs(keymaps.view_keys_for("harness", "open_timeline")) do
    pcall(vim.keymap.del, "n", key, { buffer = buf })
  end
  state.command_set = M.command_set()
  keymaps.setup_view_keymaps(buf, "harness", state.command_set)
end

---Share session commands without overriding native editing with transcript-only actions.
---@return ForgeViewCommandSet
function M.composer_command_set()
  local set = M.command_set()
  command_set.unregister(set, "visual_line_with_gutter")
  command_set.unregister(set, "open_timeline")
  command_set.unregister(set, "toggle_visibility")
  command_set.unregister(set, "toggle_presentation")
  command_set.register(set, "history_previous", prompt_history.previous)
  command_set.register(set, "history_next", prompt_history.next)
  return set
end

function M.attach()
  local state = harness_state()
  input_state.attach(state, function()
    local active = session.harness
    session.activate_harness(state)
    M.render()
    if active ~= state then session.activate_harness(active) end
  end)
  local composer_buffer = state.composer_buf
  attachment.attach(composer_buffer, state.session and state.session.id)
  attachment.decorate(state.transcript_buf)
  local edit = state.queue_edit
  if edit and edit.buffer ~= composer_buffer then
    set_composer_text(composer_buffer, edit.drafts[edit.entry] or edit.entry.text)
    edit.buffer = composer_buffer
  end
  vim.api.nvim_create_autocmd({ "TextChanged", "TextChangedI", "BufLeave", "BufWipeout" }, {
    group = vim.api.nvim_create_augroup("ForgeQueueEdit" .. composer_buffer, { clear = true }),
    buffer = composer_buffer,
    callback = function(event)
      if state.composer_buf ~= composer_buffer or not state.queue_edit
        or not vim.api.nvim_buf_is_valid(composer_buffer) then return end
      queue_editor.remember(state, composer_text(composer_buffer))
      if event.event == "TextChanged" or event.event == "TextChangedI" then render_status_hint(state) end
    end,
  })
  require("forge.views.harness.health").watch(state, function()
    if harness_state() == state then M.render() end
  end)
  if client.host_is_accepting() then state.host_error = nil end
  state.command_set = M.command_set()
  M.attach_transcript(state.transcript_buf)
  keymaps.setup_view_keymaps(state.composer_buf, "harness", M.composer_command_set())
  require("forge.views.harness.workspace").attach(state)
  prompt_history.attach(state.composer_buf)
  if state.unsubscribe then state.unsubscribe() end
  state.unsubscribe = client.session_events_subscribe(function(event, payload, event_session_id)
    if state.switching_backend then return end
    if state.session and event_session_id and state.session.id ~= event_session_id then return end
    local active_state = session.harness
    session.activate_harness(state)
    local ok, failure = xpcall(function() on_event(event, payload) end, debug.traceback)
    if not ok then
      diagnostic.set(state, "sync_error", "Presentation failed: " .. tostring(failure),
        { owner = state.session and state.session.id or "presentation", category = "internal", severity = "error", recovery = "reconnect" })
      state.busy = false
      notifications.error(state.sync_error, "ForgeHarness")
      pcall(M.refresh_winbar)
    end
    if active_state ~= state then session.activate_harness(active_state) end
  end)
end

---@param observer? fun(lines: string[])
function M._set_render_observer_for_test(observer)
  render_observer_for_test = observer
end

return M
