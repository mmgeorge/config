local M = {}

local client = require("forge.client")
local command_set = require("forge.shared.view_command_set")
local config = require("forge.infra.config")
local keymaps = require("forge.shared.keymaps")
local notifications = require("forge.infra.notifications")
local perf = require("forge.infra.perf")
local queue_renderer = require("forge.render.harness.queue")
local layout = require("forge.views.harness.layout")
local session = require("forge.session")
local prompt_history = require("forge.views.harness.prompt_history")
local provider_picker = require("forge.views.harness.provider_picker")
local context_status = require("forge.views.harness.context_status")
local model_picker = require("forge.views.harness.model_picker")
local snapshot = require("forge.views.harness.snapshot")
local picker = require("forge.views.picker")
local timeline_status = require("forge.views.harness.timeline_status")
local question_presentation = require("forge.views.harness.question_presentation")
local recap = require("forge.views.harness.recap")
local session_navigation = require("forge.views.harness.session_navigation")
local task_control = require("forge.views.harness.task")
local tabline = require("forge.views.harness.tabline")

local queue_namespace = vim.api.nvim_create_namespace("ForgeHarnessQueue")
local render_observer_for_test = nil
local begin_request
local submit_immediate
local configure_now
local finish_execution
local effort_list = { "minimal", "low", "medium", "high", "xhigh" }
local queue_only_commands = { ["/plan"] = true }

---@param state table
---@param field string
---@return string|boolean
local function applied_setting(state, field)
  if field == "service_tier" then return (state.session and state.session.service_tier) or "default" end
  return (state.session and state.session[field]) or config.options.harness[field]
end

---@param state table
---@param field string
---@return string|boolean
local function selected_setting(state, field)
  if state.task_config and state.task_config[field] ~= nil then return state.task_config[field] end
  if state.pending_config and state.pending_config[field] ~= nil then return state.pending_config[field] end
  if state.configuring_config and state.configuring_config[field] ~= nil then return state.configuring_config[field] end
  return applied_setting(state, field)
end

---@param state table
local function prune_pending_settings(state)
  local pending = state.pending_config
  if not pending then return end
  for _, field in ipairs({ "effort", "service_tier" }) do
    local applied = applied_setting(state, field)
    local inflight = state.configuring_config and state.configuring_config[field]
    if pending[field] == applied and (inflight == nil or inflight == applied) then pending[field] = nil end
  end
  if not next(pending) then state.pending_config, state.pending_config_validate = nil, false end
end

---Parse model configuration separately from its optional queued prompt.
local function model_command(text)
  if not text:match("^/model%s") then return nil end
  local model, remainder = text:match("^/model%s+(%S+)%s*(.*)$")
  if not model then return nil end
  local effort, prompt = remainder:match("^(%S+)%s*(.*)$")
  if effort and not vim.tbl_contains(effort_list, effort) then
    return nil, nil, "Unknown reasoning effort: " .. effort
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
  for _, win in ipairs({ state.transcript_win, state.composer_win }) do
    if win and vim.api.nvim_win_is_valid(win) then window_list[#window_list + 1] = win end
  end
  local current = vim.api.nvim_get_current_win()
  local control_win = vim.tbl_contains(window_list, current) and current or state.composer_win or state.transcript_win
  return {
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

local function selected_agent_target(state)
  local run = selected_agent_run(state)
  if not run then return nil end
  local summary = state.agent and state.agent.summary and state.agent.summary[run.id]
  return summary and type(summary.target) == "table" and summary.target or nil
end

local function render_queue()
  local state = harness_state()
  local buf = state.composer_buf
  local win = state.composer_win
  if not (buf and vim.api.nvim_buf_is_valid(buf) and win and vim.api.nvim_win_is_valid(win)) then return end
  vim.api.nvim_buf_clear_namespace(buf, queue_namespace, 0, -1)
  local virtual_line_list, row_count = queue_renderer.build(
    state.queue,
    vim.api.nvim_win_get_width(win),
    state.pending_steer
  )
  if row_count > 0 then
    vim.api.nvim_buf_set_extmark(buf, queue_namespace, 0, 0, {
      virt_lines = virtual_line_list,
      virt_lines_above = true,
    })
  end
  vim.b[buf].forge_queue_rows = row_count
  layout.resize_composer(buf, win)
  vim.api.nvim_win_call(win, function()
    local view = vim.fn.winsaveview()
    view.topfill = row_count
    vim.fn.winrestview(view)
  end)
end

---@param buf integer
---@return string
local function composer_text(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return "" end
  return vim.trim(table.concat(vim.api.nvim_buf_get_lines(buf, 0, -1, false), "\n"))
end

---@param buf integer
local function dismiss_composer_completion(buf)
  local completion = package.loaded["blink.cmp"]
  if completion and vim.api.nvim_get_current_buf() == buf then completion.hide() end
end

---@param buf integer
---@param text string
local function set_composer_text(buf, text)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  dismiss_composer_completion(buf)
  vim.bo[buf].modifiable = true
  local last_row = vim.api.nvim_buf_line_count(buf) - 1
  local last = vim.api.nvim_buf_get_lines(buf, last_row, last_row + 1, false)[1]
  vim.api.nvim_buf_set_text(buf, 0, 0, last_row, #last, vim.split(text, "\n", { plain = true }))
end

local function restore_retracted_prompt(text)
  local state = harness_state()
  if composer_text(state.composer_buf) ~= "" then
    table.insert(state.queue, 1, text)
    notifications.warn("The retracted prompt was queued because the composer already contains a newer draft", "ForgeHarness")
    render_queue()
    return
  end
  set_composer_text(state.composer_buf, text)
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
  local active_session = state.session or {}
  local current_task = task_control.current(state)
  local pending_marker = state.busy and (not current_task or current_task.status == "running") and "*" or ""
  local raw_mode = state.pending_mode or active_session.execution_mode or "read"
  local mode = raw_mode:lower() == "yolo" and "YOLO" or (raw_mode:sub(1, 1):upper() .. raw_mode:sub(2))
  if state.pending_mode then mode = mode .. pending_marker end
  local configured_model = active_session.model or config.options.harness.model
  local model = active_session.resolved_model or (configured_model == "default" and "resolving model" or configured_model)
  local effort = selected_setting(state, "effort")
  local selected_model = selected_setting(state, "model")
  if selected_model ~= configured_model then model = selected_model .. pending_marker end
  if effort ~= (active_session.effort or config.options.harness.effort) then effort = effort .. pending_marker end
  local busy = state.host_error and " • host stopped"
    or state.execution_notice and " • stopped"
    or state.cancel_requested and " • cancelling"
    or state.busy and ""
    or (#state.queue > 0 and (" • queued " .. #state.queue) or "")
  local goal = current_task and (" • " .. current_task.kind .. " · " .. current_task.phase
    .. (current_task.status == "running" and "" or " · " .. current_task.status)) or nil
  if state.task_operation and state.task_operation.state ~= "running" then
    goal = " • Task " .. state.task_operation.state
  end
  if state.sync_error then goal = " • State unavailable: " .. state.sync_error end
  if state.connection_error then goal = " • " .. state.connection_error end
  local tier = selected_setting(state, "service_tier")
  local tier_label = ({ fast = " +", ultrafast = " ++" })[tier] or ""
  if tier ~= (active_session.service_tier or "default") and pending_marker ~= "" then
    tier_label = (tier == "default" and " standard" or tier_label) .. "*"
  end
  local segment_list = {
    {
      text = mode,
      group = require("forge.infra.highlights").harness_mode(raw_mode),
    },
    {
      text = (" • %s %s%s"):format(model, effort, tier_label),
      group = "ForgeStatusLabel",
    },
  }
  if (active_session.access or {}).sandbox ~= false then
    segment_list[#segment_list + 1] = { text = " • sandbox", group = "ForgeStatusLabel" }
  end
  local selected_run = selected_agent_run(state)
  segment_list[#segment_list + 1] = {
    text = selected_run and (" • " .. (selected_run.nickname or selected_run.definition)) or " • Main",
    group = "ForgeStatusLabel",
  }
  if goal then
    segment_list[#segment_list + 1] = { text = goal, group = "ForgeHarnessGoal" }
  end
  local artifact_count = #(state.artifact or {})
  if artifact_count > 0 then
    segment_list[#segment_list + 1] = {
      text = (" • %d %s"):format(artifact_count, artifact_count == 1 and "artifact" or "artifacts"),
      group = "ForgeStatusLabel",
    }
  end
  if #(state.approval or {}) > 0 then
    local reopen_key = keymaps.view_keys_for("harness", "reopen_question")[1]
    local reopen_hint = reopen_key and (" (press " .. reopen_key .. ")") or ""
    segment_list[#segment_list + 1] = {
      text = " • Approval requested" .. reopen_hint,
      group = "ForgeHarnessWrite",
    }
  end
  if busy ~= "" then
    segment_list[#segment_list + 1] = { text = busy, group = "ForgeStatusLabel" }
  end
  if state.configuration_error then
    segment_list[#segment_list + 1] = {
      text = " • Settings rejected: " .. state.configuration_error:gsub("%s+", " "),
      group = "ForgeHarnessToolFailure",
    }
  end
  return segment_list
end

function M.refresh_winbar()
  local state = harness_state()
  if not state.command_set then return end
  tabline.set_session_name(state.timeline_tab, state.session and state.session.name)
  keymaps.apply_view_winbar(
    state.transcript_win,
    "",
    "harness",
    state.command_set,
    status_text(),
    nil,
    context_status.segment(state.session and state.session.context_usage)
  )
  if state.composer_win and vim.api.nvim_win_is_valid(state.composer_win) then
    vim.wo[state.composer_win].winbar = keymaps.render_hintbar(
      keymaps.view_hint_entries("harness", state.command_set, state, "composer"),
      vim.api.nvim_win_get_width(state.composer_win))
  end
  render_queue()
end

---@return integer?
local function working_seconds()
  local state = harness_state()
  if not state.busy or not state.working_started_ns then return nil end
  return math.floor((vim.uv.hrtime() - state.working_started_ns) / 1000000000)
end

local function render_status_hint(state)
  if state.presentation and state.presentation.transcript then
    state.presentation.transcript.restore_recovery = state.restore_recovery
    state.presentation.transcript.recap = state.recap
    state.presentation.transcript.rename_status = state.rename_status
    state.presentation.transcript.execution_notice = state.presentation.failure or state.execution_notice
      or state.connection_error or state.sync_error or state.presentation.section_error
    state.presentation.transcript.wait_notice = require("forge.views.harness.health").wait_notice(state)
    if state.transcript_win and vim.api.nvim_win_is_valid(state.transcript_win) then
      require("forge.views.harness.status_hint").render(state.presentation.transcript,
        M.command_set(), vim.api.nvim_win_get_width(state.transcript_win))
    end
  end
end

function M.render()
  local state = harness_state()
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
  local owner = state.presentation
  if owner and (owner.session_id ~= state.session.id or owner.host_generation ~= client.host_generation()) then
    if not owner.close({ preserve_buffer = true }) then return end
    state.presentation = nil
    owner = nil
  end
  if owner and not owner.closed then owner.sync() M.refresh_winbar() return end
  if not (state.transcript_win and vim.api.nvim_win_is_valid(state.transcript_win)) then
    state.transcript_win = vim.fn.win_findbuf(state.transcript_buf)[1]
    if not state.transcript_win then return end
  end
  local opening_session_id, opening_generation = state.session.id, client.host_generation()
  local function owns_presentation()
    return not state.switching_backend and state.session and state.session.id == opening_session_id
      and client.host_generation() == opening_generation
  end
  state.presentation = require("forge.views.harness.presentation").open({
    session_id = state.session.id, transcript_buffer = state.transcript_buf, composer_buffer = state.composer_buf,
    transcript_window = state.transcript_win,
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
local function set_busy(busy)
  local state = harness_state()
  state.busy = busy
  if busy then
    state.execution_notice = nil
    state.execution_notice_operation = nil
    if state.working_started_ns then return end
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

---@param callback? fun(result: table)
local function synchronize_state(callback, target_state)
  local synchronized_state = target_state or harness_state()
  local synchronized_session = synchronized_state.session and synchronized_state.session.id
  local synchronized_generation = client.host_generation()
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
  client.request_for(synchronized_session, "state.get", {}, function(result, request_error)
    synchronized_state.state_sync_pending = false
    if client.host_generation() ~= synchronized_generation or not synchronized_state.session
      or synchronized_state.session.id ~= synchronized_session then return end
    if result and result.session and result.session.id ~= synchronized_session then
      request_error = "Snapshot belongs to another conversation"
    end
    if result and result.runtime_epoch == synchronized_state.runtime_epoch
      and (result.timeline_revision or 0) < (synchronized_state.timeline_revision or 0) then
      request_error = "Snapshot predates the displayed transcript"
    end
    if result and result.runtime_epoch == synchronized_state.runtime_epoch
      and (result.snapshot_revision or 0) < (synchronized_state.snapshot_revision or 0) then
      request_error = "Snapshot predates the displayed task state"
    end
    if request_error or not result then
      local failure = request_error or "Harness broker returned an empty state snapshot"
      synchronized_state.sync_error = failure
      synchronized_state.state_sync_again = nil
      local callbacks = synchronized_state.state_sync_callback or {}
      synchronized_state.state_sync_callback = nil
      for _, completed in ipairs(callbacks) do completed(nil, failure) end
      notifications.error(failure, "Harness state")
      if harness_state() == synchronized_state then M.render() end
      local retry = (synchronized_state.state_sync_retry or 0) + 1
      synchronized_state.state_sync_retry = retry
      if retry <= 3 then
        vim.defer_fn(function()
          if harness_state() == synchronized_state and client.host_generation() == synchronized_generation then synchronize_state() end
        end, 1000 * 2 ^ (retry - 1))
      end
      return
    end
    synchronized_state.sync_error, synchronized_state.state_sync_retry = nil, nil
    if synchronized_state.state_sync_again then
      synchronized_state.state_sync_again = nil
      synchronize_state(nil, synchronized_state)
      return
    end
    local state = synchronized_state
    snapshot.apply(state, result)
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
  if state.pending_config then vim.schedule(M.drain) end
  end)
end

local function resolve_scope_deviation(deviation, approved)
  client.request("plan.deviation.resolve", {
    deviation_id = deviation.id,
    approved = approved,
  }, function(_, request_error)
    if request_error then notifications.error(request_error, "Plan deviation") end
  end)
end

local function present_scope_deviation(deviation)
  local policy = config.options.harness.plan and config.options.harness.plan.scope_deviation_review or "auto"
  if policy == "auto" then
    resolve_scope_deviation(deviation, true)
    return
  end
  open_choice_picker(harness_state(), "Review Scope Deviation", deviation.reason or deviation.summary, {
    { label = "Approve", detail = deviation.summary, value = true },
    { label = "Reject", detail = "Block execution without changing accepted intent.", value = false },
  }, function(approved) resolve_scope_deviation(deviation, approved) end)
end

local function on_event(event, payload)
  local state = harness_state()
  if event == "task_operation" then task_control.receive(state, payload) return end
  if event == "host_stopped" then
    state.host_error = payload.message
    state.execution_notice = payload.message .. ". Reopen Harness to reconnect."
    state.pending_mode = nil
    state.cancel_requested = false
    state.state_sync_pending, state.state_sync_again, state.state_sync_callback = nil, nil, nil
    state.configuring, state.configuration_debounce = false, false
    if state.configuration_completion then state.configuration_completion.complete(false) end
    state.configuration_completion, state.task_config = nil, nil
    state.approval, state.active_wait = {}, nil
    state.ready = false
    if state.presentation and state.presentation.terminals then state.presentation.terminals.close() end
    set_busy(false)
    M.refresh_winbar()
    return
  end
  if state.host_error then return end
  if event == "document_changed" then
    if payload.session_id ~= (state.session and state.session.id) then return end
    if payload.revision < (state.timeline_revision or 0) then return end
    state.last_provider_progress, state.wait_notice = vim.uv.now(), nil
    state.timeline_revision = payload.revision
    if payload.status then state.status = payload.status end
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
    state.last_provider_progress, state.wait_notice = vim.uv.now(), nil
    if payload.kind == "document_changed" then on_event("document_changed", payload.data) return end
    if payload.kind == "turn_started" then recap.clear(state) end
    if payload.kind == "execution_state" then
      local execution = payload.data or {}
      if type(execution.session) ~= "table" or not state.session or execution.session.id ~= state.session.id then return end
      state.session = execution.session
      state.task = execution.task or state.task
      state.goal = type(execution.goal) == "table" and execution.goal.state ~= "cleared" and execution.goal or nil
      state.goal_execution = type(execution.goal_execution) == "table" and execution.goal_execution or nil
      M.refresh_winbar()
    elseif payload.kind == "prompt_submission" then
      if state.presentation then state.presentation.receive(payload) end
    elseif payload.kind == "approval_requested" then
      local request = payload.data or payload
      state.approval = state.approval or {}
      if not vim.iter(state.approval):any(function(approval) return approval.id == request.id end) then
        state.approval[#state.approval + 1] = request
      end
      M.refresh_winbar()
      vim.schedule(M.present_approval)
    elseif payload.kind == "approval_resolved" or payload.kind == "approval_cancelled" then
      local request = payload.data or payload
      remove_approval(state, request.id)
      reconcile_approval_presentation(state)
      M.refresh_winbar()
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
      local update = payload.data or payload
      local acknowledged = update.node and update.node.prompt
      for index, pending in ipairs(state.pending_steer or {}) do
        if acknowledged and pending.text == acknowledged.text then
          table.remove(state.pending_steer, index)
          break
        end
      end
      M.refresh_winbar()
    elseif payload.kind == "error" and type(payload.text) == "string" then
      notifications.warn(payload.text, "ForgeHarness")
    end
  elseif event == "question" then
    state.active_elicitation = payload
    if question_presentation.should_present(state) then
      if state.plan_question_open then
        require("forge.views.harness.plan_question").close()
        state.plan_question_open = false
      end
      vim.schedule(function() M.present_plan_question(false) end)
    end
    schedule_render()
  elseif event == "question_updated" then
    state.active_elicitation = payload
    if question_presentation.should_present(state) then
      require("forge.views.harness.plan_question").close()
      state.plan_question_open = false
      vim.schedule(function() M.present_plan_question(false) end)
    end
    schedule_render()
  elseif event == "question_answered" then
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
  elseif event == "plan_created" or event == "plan_revision_created" or event == "plan_entity_renamed" or event == "plan_tests_deleted"
    or event == "plan_changes_requested"
    or event == "plan_acceptance_started" or event == "plan_acceptance_updated"
    or event == "plan_acceptance_cancelled"
    or event == "plan_question_answered" or event == "plan_question_withdrawn"
    or event == "plan_accepted" or event == "plan_cancelled"
    or event == "plan_activated"
  then
    synchronize_state()
  elseif event == "plan_deviation_review" then
    vim.schedule(function() present_scope_deviation(payload) end)
    schedule_render()
  elseif event == "plan_deviation_recorded" or event == "plan_deviation_resolved"
    or event == "plan_task_updated" or event == "plan_resolution"
  then
    synchronize_state()
  elseif event == "goal_changed" or event == "goal_continue_requested" then
    state.goal = payload.state ~= "cleared" and payload or nil
    if event == "goal_continue_requested" then
      synchronize_state(M.drain)
    elseif state.goal_execution then
      synchronize_state()
    else
      M.refresh_winbar()
    end
  elseif event == "context_compacted" then
    synchronize_state()
  elseif event == "session_fork_ready" or event == "session_fork_failed" then
    state.session = payload.session or state.session
    M.refresh_winbar()
    if event == "session_fork_failed" then
      local provider_fork_state = state.session and state.session.provider_fork_state or {}
      notifications.error(provider_fork_state.message or "Provider fork preparation failed", "Harness fork")
    end
  elseif event == "session_changed" or event == "session_configured" or event == "mode_changed"
    or event == "execution_mode_changed"
  then
    local next_session = payload.session or payload
    if event == "session_changed" and state.session and next_session.id ~= state.session.id then
      recap.clear(state)
      state.queue = {}
      state.goal = nil
      state.goal_execution = nil
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
    if payload.operation_id then state.pending_mode = nil end
    local configuration = state.configuration_completion
    if configuration and state.task_operation and payload.operation_id == state.task_operation.id then
      state.configuration_completion = nil
      state.task_config = nil
      configuration.complete(true)
    end
    if (event == "execution_mode_changed" or event == "mode_changed") then
      state.pending_mode = nil
    end
    M.refresh_winbar()
  elseif event == "interaction_rolled_back" then
    synchronize_state()
  elseif event == "agent_updated" then
    state.agent = vim.deepcopy(payload)
    require("forge.views.harness.agent_picker").refresh()
    schedule_render()
  elseif event == "exchange_complete" or event == "exchange_updated" then
    synchronize_state()
  elseif event == "state_invalidated" then
    synchronize_state()
  end
end

function M.present_approval()
  local state = harness_state()
  local request = state.approval and state.approval[1]
  if state.approval_open or not request then return end
  state.approval_open = true
  state.presented_approval_id = request.id
  require("forge.views.harness.approval").open(request, {
    interrupt = M.cancel_turn,
    transcript_win = state.transcript_win,
    window_list = picker_host(state).window_list,
    control_win = picker_host(state).control_win,
    resolve = function(approval_id, choice_id, callback)
      client.request("approval.resolve", {
        approval_id = approval_id,
        choice_id = choice_id,
      }, function(_, request_error)
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
      M.refresh_winbar()
    end,
  })
end

---@param force? boolean
function M.present_plan_question(force)
  local state = harness_state()
  local elicitation = state.active_elicitation and state.active_elicitation.elicitation
  if state.busy or state.plan_question_open or not elicitation then return end
  local owner = state.active_elicitation.owner
  if not force and not question_presentation.should_present(state) then return end
  question_presentation.mark_presented(state)
  state.plan_question_open = true
  require("forge.views.harness.plan_question").open(elicitation, {
    transcript_win = state.transcript_win,
    window_list = picker_host(state).window_list,
    control_win = picker_host(state).control_win,
    allow_ask = owner ~= "plan_acceptance",
    answer = function(params, callback)
      client.request("question.answer", params, function(result, request_error)
        if request_error then
          notifications.error(request_error, "Harness question")
          return
        end
        state.active_plan = result.active_plan or state.active_plan
        state.active_elicitation = result.active_elicitation
        callback(result.active_elicitation and result.active_elicitation.elicitation)
      end)
    end,
    skip = function(params, callback)
      client.request("question.skip", params, function(result, request_error)
        if request_error then
          notifications.error(request_error, "Harness question")
          return
        end
        state.active_plan = result.active_plan or state.active_plan
        state.active_elicitation = result.active_elicitation
        callback(result.active_elicitation and result.active_elicitation.elicitation)
      end)
    end,
    ask = function(params)
      state.plan_question_open = false
      prompt_history.record(params.text)
      set_busy(true)
      client.request("question.ask", params, function(_, request_error, error_detail)
        if finish_execution(request_error, error_detail) then return end
        question_presentation.reset(state)
        if request_error then
          notifications.error(request_error, "Planning clarification")
          synchronize_state()
          return
        end
        synchronize_state()
      end)
    end,
    continue = function()
      state.plan_question_open = false
      set_busy(true)
      client.request("question.continue", {}, function(_, request_error, error_detail)
        if finish_execution(request_error, error_detail) then return end
        if request_error then
          notifications.error(request_error, "Planning continuation")
          synchronize_state()
          return
        end
        synchronize_state(M.drain)
      end)
    end,
    closed = function()
      state.plan_question_open = false
      if owner == "plan_acceptance" then
        client.request("plan.acceptance.cancel", {}, function(result, request_error)
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
    M.present_approval()
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
---@return boolean
finish_execution = function(request_error, error_detail)
  local state = harness_state()
  if state.host_error then set_busy(false) return true end
  if state.task_operation then return true end
  if request_error and error_detail and error_detail.code == "turn_cancelled" then
    state.execution_notice = "Paused"
    set_busy(false)
    synchronize_state()
    return true
  end
  if request_error and not (error_detail and error_detail.code == "turn_retracted") then
    state.execution_notice = error_detail and error_detail.code == "turn_cancelled" and "Paused"
      or ("Stopped: " .. request_error)
  end
  set_busy(false)
  return false
end

---@param text string
begin_request = function(text, from_composer)
  local state = harness_state()
  recap.clear(state)
  if from_composer then dismiss_composer_completion(state.composer_buf) end
  state.cancel_requested = false
  set_busy(true)
  local goal_objective = text:match("^/goal%s+(.+)$")
  if goal_objective and goal_objective ~= "pause" and goal_objective ~= "resume" and goal_objective ~= "clear" then
    state.goal = { objective = goal_objective, state = "active" }
  end
  M.render()
  local method = "prompt.submit"
  local params = { text = text }
  local function receive(result, request_error, error_detail)
    if finish_execution(request_error, error_detail) then return end
    state.cancel_requested = false
    if request_error then
      if error_detail and error_detail.code == "turn_retracted" then
        local prompt = error_detail.data and error_detail.data.prompt or text
        if not from_composer then restore_retracted_prompt(prompt) end
        synchronize_state()
        return
      end
      if error_detail and error_detail.code == "turn_cancelled" then
        synchronize_state(M.drain)
        return
      end
      notifications.error(request_error, "ForgeHarness")
      synchronize_state()
      return
    elseif result then
      state.session = result.session or (result.id and result) or state.session
      state.capability = result.capability or state.capability
    end
    synchronize_state(M.drain)
  end
  if from_composer then
    if state.presentation then state.presentation.submit(receive)
    else receive(nil, "Harness composer is not ready") end
  else client.request(method, params, receive) end
end

function M.cancel_turn()
  local state = harness_state()
  if state.host_error then M.render() return end
  if not state.selected_agent_run_id then
    if state.task_operation and state.task_operation.action == "pause" then return end
    M.task_transition({ action = "pause" })
    return
  end
  if not state.busy and not state.host_error and state.status
      and (state.status.kind == "finalizing" or state.status.kind == "paused") then
    if state.cancel_requested then return end
    state.cancel_requested = true
    client.request("turn.cancel", {}, function(_, request_error)
      if state.host_error then return end
      state.cancel_requested = false
      if request_error then
        state.execution_notice = "Finalization failed: " .. request_error
        notifications.error(request_error, "Harness finalization")
      else
        state.execution_notice = "Paused"
      end
      M.render()
      synchronize_state()
    end)
    return
  end
  if not state.busy then
    state.execution_notice = state.execution_notice or "Paused"
    M.render()
    if not state.host_error then synchronize_state() end
    return
  end
  if state.cancel_requested then return end
  local target = selected_agent_target(state)
  if state.selected_agent_run_id and not target then
    notifications.warn("The selected child has no active turn to cancel", "ForgeHarness")
    return
  end
  state.pending_mode = nil
  state.cancel_requested = true
  M.refresh_winbar()
  local restore_prompt = state.capability.native_turn_rollback == true
    and #(state.queue or {}) == 0
    and #(state.pending_steer or {}) == 0
    and composer_text(state.composer_buf) == ""
  client.request("turn.cancel", { restore_prompt_if_no_output = restore_prompt, target = target }, function(result, request_error)
    if state.host_error then return end
    if request_error then
      state.cancel_requested = false
      notifications.error(request_error, "Harness cancel")
      M.refresh_winbar()
      return
    end
    if not (result and result.cancel_requested) then
      state.cancel_requested = false
      notifications.warn("The Harness turn already finished", "ForgeHarness")
      M.refresh_winbar()
    elseif target then
      state.cancel_requested = false
      M.refresh_winbar()
    end
  end)
end

function M.compact()
  if not (harness_state().capability and harness_state().capability.native_compact) then
    notifications.error("The current backend does not support manual context compaction", "ForgeHarness")
    return
  end
  M.task_transition({ action = "compact" })
end

function M.drain()
  local state = harness_state()
  if state.host_error or state.execution_notice or state.task_operation or state.sync_error or state.connection_error then return end
  if state.aborting_plan then return end
  if state.busy or state.switching_backend or state.state_sync_pending or state.configuring or state.configuration_debounce then return end
  if #(state.pending_steer or {}) > 0 then return end
  if state.status and state.status.kind == "finalizing" then return end
  if state.pending_mode then
    local pending_mode = state.pending_mode
    state.pending_mode = nil
    M.set_mode(pending_mode)
    return
  end
  if state.pending_config then
    local pending_config = state.pending_config
    local validate_selection = state.pending_config_validate == true
    state.pending_config = nil
    state.pending_config_validate = false
    configure_now(pending_config, validate_selection)
    return
  end
  local current_task_id = state.session and state.session.current_task_id
  local first_queued = state.queue[1]
  state.queue_suspended = type(first_queued) == "table" and first_queued.task_id ~= current_task_id
  local next_entry = state.queue[1]
  local next_text = type(next_entry) == "table" and next_entry.text or next_entry
  if state.active_elicitation and state.active_elicitation.elicitation
    and not (next_text and next_text:match("^/replan%s")) then
    return
  end
  local entry = state.queue[1]
  local text = type(entry) == "table" and entry.text or entry
  if text and not state.queue_suspended then
    local next_config, prompt, failure = model_command(text)
    if failure then report_configuration_error(failure) return end
    if next_config then
      if type(entry) == "table" then next_config = vim.tbl_extend("force", next_config, entry.config or {}) end
      M.configure(next_config, true, function(applied)
        if not applied then return end
        if state.queue[1] ~= entry then return end
        table.remove(state.queue, 1)
        if prompt ~= "" then begin_request(prompt) else M.drain() end
      end)
      return
    end
    table.remove(state.queue, 1)
    if text == "/compact" then M.compact() else begin_request(text) end
    return
  end

end

---@param name string
function M.rename_session(name)
  local state = harness_state()
  local model = selected_setting(state, "model")
  require("forge.views.harness.session_name").rename(state, name, model, function()
    if state.presentation and state.transcript_win and vim.api.nvim_win_is_valid(state.transcript_win) then
      state.presentation.transcript.rename_status = state.rename_status
      require("forge.views.harness.status_hint").render(state.presentation.transcript,
        state.command_set or M.command_set(), vim.api.nvim_win_get_width(state.transcript_win))
    end
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
  client.request_for(active_session.id, "session.fork", params, function(result, request_error)
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
      client.request_for(state.session.id, "plan.declaration", action, function(snapshot, failure)
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
      local opened, failure = pcall(vim.cmd, "tabedit " .. vim.fn.fnameescape(path))
      if not opened then notifications.error(failure, "ForgeHarness") return end
      vim.api.nvim_win_set_cursor(0, { math.max(1, math.min(action.line or 1, vim.api.nvim_buf_line_count(0))), 0 })
    elseif action.kind == "plan" then
      client.request_for(state.session.id, "plan.activate", { plan_id = action.plan_id, revision = action.revision }, function(plan, failure)
        if failure then notifications.error(failure, "Harness artifact") return end
        if not plan.historical_revision then state.active_plan = plan end
        require("forge.views.plan_review").open(plan)
        synchronize_state()
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
  local artifact_list = state.artifact or {}
  local option_list = vim.tbl_map(function(artifact)
      local title = artifact.title and artifact.title ~= "" and artifact.title or "[unnamed plan]"
      return { label = title, detail = artifact.state or "unknown", value = artifact }
    end, artifact_list)
  open_choice_picker(state, "Select Artifact", nil, option_list, function(artifact)
    if not artifact then return end
    client.request("plan.activate", { plan_id = artifact.id }, function(plan, request_error)
      if request_error then
        notifications.error(request_error, "Harness artifact")
        return
      end
      state.active_plan = plan
      require("forge.views.plan_review").open(plan)
      synchronize_state()
    end)
  end, "This session has no artifacts.")
end

---@class ForgeRollbackInteraction
---@field id string
---@field ordinal integer
---@field prompt string
---@field state "complete"|"failed"|"cancelled"
---@field checkpoint_before string

---@param interaction ForgeRollbackInteraction
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
  local session_id, generation = state.session.id, client.host_generation()
  local function current()
    return harness_state() == state and state.session and state.session.id == session_id
      and client.host_generation() == generation
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
      client.request_for(session_id, "exchange.rollback.preview", { preview_id = preview.preview_id, offset = preview.next_offset }, function(page, err)
        if not current() then return end
        if err then notifications.error(err, "Harness undo") return end
        confirm_restore_preview(page, interaction)
      end)
      return
    end
    if state.restore_applying or state.busy then return end
    local operation = {}
    state.restore_applying = operation
    client.request_for(session_id, "exchange.rollback.apply", { preview_id = preview.preview_id }, function(_, err)
      if not current() or state.restore_applying ~= operation then return end
      state.restore_applying = nil
      synchronize_state()
      if err then
        notifications.error(err, "Harness undo")
        if err:find("preview is stale", 1, true) and interaction then
          client.request_for(session_id, "exchange.rollback.prepare", { exchange_id = interaction.id }, function(refreshed, failure)
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
local function confirm_rollback(interaction)
  local state = harness_state()
  local session_id, generation = state.session.id, client.host_generation()
  if state.busy or state.restore_applying then return end
  client.request_for(session_id, "exchange.rollback.prepare", { exchange_id = interaction.id }, function(preview, err)
    if harness_state() ~= state or not state.session or state.session.id ~= session_id
      or client.host_generation() ~= generation then return end
    if err then notifications.error(err, "Harness undo") return end
    confirm_restore_preview(preview, interaction)
  end)
end

---Open the checkpointed interaction picker for an editable rollback.
local function open_exchange_undo_picker()
  local state = harness_state()
  if state.busy then
    notifications.warn("Cancel or finish the active turn before undoing an exchange", "Harness undo")
    return
  end
  if state.no_checkpoint then
    notifications.warn("Undo is unavailable because this session has NO CHECKPOINT", "Harness undo")
    return
  end
  local session_id, generation = state.session and state.session.id, client.host_generation()
  if not session_id then return end
  client.request_for(session_id, "exchange.list", {}, function(interaction_list, request_error)
    if harness_state() ~= state or not state.session or state.session.id ~= session_id
      or client.host_generation() ~= generation then return end
    if request_error then
      notifications.error(request_error, "Harness undo")
      return
    end
    local rollback_state = { complete = true, failed = true, cancelled = true, interrupted = true }
    local option_list = {}
    for index = #(interaction_list or {}), 1, -1 do
      local interaction = interaction_list[index]
      if interaction.checkpoint_before and rollback_state[interaction.state]
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
    open_choice_picker(
      state,
      "Select Exchange",
      nil,
      option_list,
      confirm_rollback,
      "This session has no exchanges available to undo."
    )
  end)
end

---Open saved restore recovery before offering exchange history.
function M.open_undo_picker()
  local state = harness_state()
  if state.busy or state.restore_applying then
    notifications.warn("Cancel or finish the active turn before restoring files", "Harness undo")
    return
  end
  if not state.session then return end
  local session_id, generation = state.session.id, client.host_generation()
  client.request_for(session_id, "exchange.recovery", {}, function(recovery, err)
    if harness_state() ~= state or not state.session or state.session.id ~= session_id
      or client.host_generation() ~= generation then return end
    if err then notifications.error(err, "Harness recovery") return end
    if not recovery or recovery == vim.NIL then open_exchange_undo_picker() return end
    local choices = { { label = "Cancel", value = "cancel" } }
    if recovery.pending then
      choices[#choices + 1] = { label = "Continue interrupted restore", value = "continue" }
      choices[#choices + 1] = { label = "Restore pre-restore files", value = "undo" }
    else
      choices[#choices + 1] = { label = "Select an earlier exchange", value = "history" }
      choices[#choices + 1] = { label = "Undo last restore", value = "undo" }
    end
    open_choice_picker(state, "Harness restore", recovery.pending and "An interrupted restore requires recovery." or nil, choices, function(choice)
      if harness_state() ~= state or not state.session or state.session.id ~= session_id
        or client.host_generation() ~= generation then return end
      if choice == "history" then open_exchange_undo_picker() return end
      if choice ~= "undo" and choice ~= "continue" then return end
      if harness_state() ~= state or not state.session or state.session.id ~= session_id then return end
      client.request_for(session_id, "exchange.recovery", { action = choice }, function(result, failure)
        if harness_state() ~= state or not state.session or state.session.id ~= session_id
          or client.host_generation() ~= generation then return end
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

---Request one provider-backed child agent with an explicit definition and task.
---@param definition string
---@param task string
function M.spawn_agent(definition, task)
  local state = harness_state()
  if not (state.capability.agent and state.capability.agent.catalog) then
    notifications.warn("The current backend does not expose spawnable child agents", "Harness agent")
    return
  end
  if state.presentation then state.presentation.follow_tail() end
  set_busy(true)
  client.request("agent.start", { definition = definition, task = task }, function(_, request_error, error_detail)
    if finish_execution(request_error, error_detail) then return end
    if request_error then notifications.error(request_error, "Harness agent") end
    synchronize_state(M.drain)
  end)
end

---Open the provider-backed child-agent timeline selector.
function M.open_agent_picker()
  local state = harness_state()
  require("forge.views.harness.agent_picker").open({
    host = picker_host(state),
    state_provider = harness_state,
    on_select = function(run_id)
      M.select_agent(run_id and run_id or "main")
    end,
  })
end

---Open the searchable agent-definition selector and attached task editor.
---@param definition_name? string
function M.open_spawn_picker(definition_name)
  local state = harness_state()
  if not (state.capability.agent and state.capability.agent.catalog) then
    notifications.warn("The current backend does not expose spawnable child agents", "Harness agent")
    return
  end
  local opened = require("forge.views.harness.spawn_picker").open({
    host = picker_host(state),
    definition_list = (state.agent and state.agent.definition) or {},
    definition_name = definition_name,
    on_spawn = M.spawn_agent,
  })
  if not opened then notifications.warn("Unknown agent definition: " .. definition_name, "Harness agent") end
end

function M.open_session_picker()
  require("forge.views.harness.session_picker").open(picker_host(harness_state()))
end

---List provider-owned background shells and terminate only the selected shell.
function M.open_background_picker()
  local state = harness_state()
  local session_id, generation = state.session.id, client.host_generation()
  local function current()
    return state.session and state.session.id == session_id and client.host_generation() == generation
  end
  client.request_for(session_id, "harness.document", { operation = "background_terminals" }, function(inventory, failure)
    if not current() then return end
    if failure then notifications.error(failure, "Select Background Terminal") return end
    if not inventory.supported then
      notifications.warn("The current backend does not support background terminals", "ForgeHarness")
      return
    end
    local options = {}
    for _, terminal in ipairs(inventory.terminal or {}) do
      options[#options + 1] = { label = terminal.command:gsub("%s+", " "), detail = "Terminal " .. terminal.id, value = terminal.id }
    end
    open_choice_picker(state, "Terminate Background Terminal", nil, options, function(id)
      if not current() then return end
      client.request_for(session_id, "harness.document", { operation = "terminate_terminal", id = id }, function(_, terminate_failure)
        if not current() then return end
        if terminate_failure then notifications.error(terminate_failure, "Select Background Terminal") end
        if state.presentation and state.presentation.terminals then state.presentation.terminals.refresh() end
      end)
    end, "No background terminals are running.")
  end)
end

function M.submit()
  if harness_state().host_error then notifications.error(harness_state().execution_notice, "ForgeHarness") return end
  harness_state().execution_notice = nil
  local state = harness_state()
  if state.switching_backend then return end
  local text = composer_text(state.composer_buf)
  if text == "/task refresh" then
    state.state_sync_retry = nil
    synchronize_state()
    return
  end
  if text == "" then return end
  if not require("forge.views.harness.completion.command_source").accepts_prompt(text) then return end
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
      if state.presentation and state.transcript_win and vim.api.nvim_win_is_valid(state.transcript_win) then
        state.presentation.transcript.restore_recovery = state.restore_recovery
        state.presentation.transcript.recap = state.recap
        require("forge.views.harness.status_hint").render(state.presentation.transcript,
          M.command_set(), vim.api.nvim_win_get_width(state.transcript_win))
      end
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
  local goal_control = ({ ["/goal pause"] = "goal.pause", ["/goal clear"] = "goal.clear" })[text]
  if goal_control then
    local session_id = state.session.id
    set_composer_text(state.composer_buf, "")
    client.request_for(session_id, goal_control, {}, function(result, request_error)
      if request_error then
        notifications.error(request_error, "Harness goal")
        return
      end
      if state.session and state.session.id == session_id then
        state.goal = result.state ~= "cleared" and result or nil
        M.refresh_winbar()
      end
    end)
    return
  end
  if vim.tbl_contains({ "/read", "/write", "/yolo" }, text) then
    set_composer_text(state.composer_buf, "")
    M.set_mode(text:sub(2))
    return
  end
  if text == "/mode" then
    set_composer_text(state.composer_buf, "")
    M.select_mode()
    return
  end
  local execution_mode = text:match("^/mode%s+(%S+)$")
  if execution_mode then
    set_composer_text(state.composer_buf, "")
    execution_mode = execution_mode:lower()
    if not vim.tbl_contains({ "read", "write", "yolo" }, execution_mode) then
      report_configuration_error("Unknown execution mode: " .. execution_mode)
      return
    end
    M.set_mode(execution_mode)
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
  if text == "/new" then
    set_composer_text(state.composer_buf, "")
    require("forge.views.harness").new_session()
    return
  end
  if text == "/questions" then
    set_composer_text(state.composer_buf, "")
    M.present_plan_question(true)
    return
  end
  if text == "/agent" then
    set_composer_text(state.composer_buf, "")
    M.open_agent_picker()
    return
  end
  if text == "/spawn" then
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
  local agent_selector = text:match("^/agent%s+(%S+)%s*$")
  if agent_selector then
    set_composer_text(state.composer_buf, "")
    M.select_agent(agent_selector)
    return
  elseif text:match("^/agent%s+") then
    set_composer_text(state.composer_buf, "")
    notifications.warn("Use /agent main, /agent <running alias>, or /agent", "ForgeHarness")
    return
  end
  local spawn_definition, spawn_task = text:match("^/spawn%s+(%S+)%s+(.+)$")
  if spawn_definition and spawn_task then
    set_composer_text(state.composer_buf, "")
    M.spawn_agent(spawn_definition, spawn_task)
    return
  end
  local spawn_definition_only = text:match("^/spawn%s+(%S+)%s*$")
  if spawn_definition_only then
    set_composer_text(state.composer_buf, "")
    M.open_spawn_picker(spawn_definition_only)
    return
  elseif text:match("^/spawn%s+") then
    set_composer_text(state.composer_buf, "")
    notifications.warn("Use /spawn <definition> <task>", "ForgeHarness")
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
    if model_effort and not vim.tbl_contains(effort_list, model_effort) then
      report_configuration_error("Unknown reasoning effort: " .. model_effort)
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
    if not vim.tbl_contains(effort_list, effort) then
      report_configuration_error("Unknown reasoning effort: " .. effort)
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
  if state.busy and queue_only_commands[text:match("^%s*(%S+)")] then
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
    M.steer_submit()
    return
  end
  if state.task_operation and state.task_operation.state ~= "running" then
    M.refresh_winbar()
    return
  end
  if state.busy or state.configuring then
    set_composer_text(state.composer_buf, "")
    if state.busy and not selected_agent_run(state)
      and state.capability and state.capability.native_steer
    then
      submit_immediate(state, text)
      return
    end
    state.queue[#state.queue + 1] = { text = text, task_id = state.session and state.session.current_task_id }
    M.refresh_winbar()
    return
  end
  begin_request(text, true)
end

local function remove_pending_steer(state, target)
  for index, entry in ipairs(state.pending_steer or {}) do
    if entry == target then
      table.remove(state.pending_steer, index)
      return
    end
  end
end

submit_immediate = function(state, text)
  local selected_run = selected_agent_run(state)
  local target = selected_agent_target(state)
  if state.selected_agent_run_id then
    if not selected_run or not target then
      notifications.warn("The selected child has no active turn to steer", "ForgeHarness")
      return
    end
  else
    set_composer_text(state.composer_buf, "")
  end
  local pending = { text = text }
  recap.clear(state)
  state.pending_steer = state.pending_steer or {}
  state.pending_steer[#state.pending_steer + 1] = pending
  M.refresh_winbar()
  client.request("turn.steer", { text = text, target = target }, function(_, request_error)
    if request_error then
      remove_pending_steer(state, pending)
      if selected_run then
        notifications.warn(request_error, "ForgeHarness")
      else
        state.queue[#state.queue + 1] = text
        notifications.warn("Steering missed the active turn; queued as a follow-up", "ForgeHarness")
      end
    else
      if selected_run and composer_text(state.composer_buf) == text then
        set_composer_text(state.composer_buf, "")
      end
    end
    M.refresh_winbar()
    vim.schedule(M.drain)
  end)
end

---Send the composer into the active provider turn without creating a new interaction.
function M.steer_submit()
  local state = harness_state()
  local text = composer_text(state.composer_buf)
  if text == "" then return end
  if not state.busy then
    notifications.warn("Harness has no active turn to steer", "ForgeHarness")
    return
  end
  if not (state.capability and state.capability.native_steer) then
    notifications.warn("The current backend does not support active-turn steering", "ForgeHarness")
    return
  end
  if state.task_operation and state.task_operation.state ~= "running" then
    M.refresh_winbar()
    return
  end
  prompt_history.record(text)
  submit_immediate(state, text)
end

---Queue a follow-up independently of active waits and the selected child timeline.
function M.queue_submit()
  if harness_state().host_error then notifications.error(harness_state().execution_notice, "ForgeHarness") return end
  harness_state().execution_notice = nil
  local state = harness_state()
  if state.switching_backend then return end
  local text = composer_text(state.composer_buf)
  if text == "" then return end
  if not require("forge.views.harness.completion.command_source").accepts_prompt(text) then return end
  if text:match("^/task") or text:match("^/execute") or text:match("^/plan") or text:match("^/goal") or text == "/config" or text == "/log" or text:match("^/log%s") then M.submit() return end
  if text == "/fast" or text == "/ultrafast" or text:match("^/fast%s") or text:match("^/ultrafast%s") or text == "/bg" or text == "/recap" or text == "/mcp" or text == "/replan" or text == "/plan cancel" then M.submit() return end
  if text == "/model" then
    M.select_model(function(next_config)
      local command = "/model " .. next_config.model .. (next_config.effort and (" " .. next_config.effort) or "")
      state.queue[#state.queue + 1] = { text = command, config = next_config, task_id = state.session and state.session.current_task_id }
      prompt_history.record(command)
      if composer_text(state.composer_buf) == text then set_composer_text(state.composer_buf, "") end
      M.refresh_winbar()
      M.drain()
    end)
    return
  end
  local _, _, failure = model_command(text)
  if failure then report_configuration_error(failure) return end
  prompt_history.record(text)
  state.queue[#state.queue + 1] = { text = text, task_id = state.session and state.session.current_task_id }
  set_composer_text(state.composer_buf, "")
  M.refresh_winbar()
  M.drain()
end

function M.edit_last_queued()
  local state = harness_state()
  if state.configuring and #state.queue == 1 then return end
  local queued = table.remove(state.queue)
  if not queued then
    notifications.warn("No queued prompt to edit", "ForgeHarness")
    return
  end
  local draft = composer_text(state.composer_buf)
  if draft ~= "" then state.queue[#state.queue + 1] = draft end
  set_composer_text(state.composer_buf, type(queued) == "table" and queued.text or queued)
  if state.composer_win and vim.api.nvim_win_is_valid(state.composer_win) then
    vim.api.nvim_set_current_win(state.composer_win)
  end
  M.refresh_winbar()
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
  if vim.fn.foldclosed(vim.fn.line(".")) == -1 and state.presentation
      and state.presentation.toggle_tool() then return end
  if state.presentation and state.presentation.toggle_heading(vim.api.nvim_get_current_win()) then return end
end

---@param direction integer
function M.change_effort(direction)
  local state = harness_state()
  local current = selected_setting(state, "effort")
  local index = 3
  for candidate_index, candidate in ipairs(effort_list) do if candidate == current then index = candidate_index end end
  index = math.max(1, math.min(#effort_list, index + direction))
  M.configure({ effort = effort_list[index] })
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
  local options = vim.tbl_map(function(effort)
    return { label = effort, detail = detail_list[effort], value = effort }
  end, effort_list)
  open_choice_picker(harness_state(), "Select Reasoning Effort", nil, options, function(effort)
    M.configure({ effort = effort })
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
  M.configure({ service_tier = tier })
end

---@param tier "fast"|"ultrafast"
function M.toggle_service_tier(tier)
  local state = harness_state()
  M.configure_service_tier(selected_setting(state, "service_tier") == tier and "default" or tier)
end

function M.select_model(on_confirm)
  on_confirm = type(on_confirm) == "function" and on_confirm or M.configure
  local state = harness_state()
  if state.capability.model_selection ~= true then
    notifications.warn("The current backend does not support model selection", "ForgeHarness")
    return
  end
  local function open_model_picker(model_list)
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
      current_model = state.session and (state.session.resolved_model or state.session.model),
      on_confirm = on_confirm,
    })
  end
  local backend = state.session and state.session.backend
  if state.model_backend == backend and type(state.model_list) == "table" then
    open_model_picker(state.model_list)
    return
  end
  client.request("backend.models", {}, function(model_list, request_error)
    if request_error then notifications.error(request_error, "Harness model") return end
    state.model_backend = backend
    state.model_list = vim.deepcopy(model_list or {})
    open_model_picker(state.model_list)
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
  client.request("backend.models", {}, function(model_list, request_error)
    if request_error then notifications.error("Failed to resolve Harness model: " .. request_error, "ForgeHarness") end
    if request_error then return end
    state.model_backend = backend
    state.model_list = vim.deepcopy(model_list or {})
  end)
end

---@param mode string
function M.task_transition(action, submitted_text, completed)
  local state = harness_state()
  if state.host_error then notifications.error(state.execution_notice or state.host_error, "Harness task") return end
  if not state.session then notifications.warn("Harness is still initializing", "Harness task") return end
  if action.action ~= "configure" then
    state.task_config = nil
    if state.configuration_completion then
      state.configuration_completion.complete(false)
      state.configuration_completion = nil
    end
  end
  state.last_provider_progress, state.wait_notice = vim.uv.now(), nil
  local current = task_control.current(state)
  if action.action == "resume" and current and current.status == "running"
    and (not action.task_id or action.task_id == current.id) then
    M.render()
    return
  end
  set_busy(true)
  task_control.transition(state, action, M.render, function(failure)
    if harness_state() ~= state then
      state.busy = false
      timeline_status.stop(state)
      synchronize_state(nil, state)
      return
    end
    set_busy(false)
    state.cancel_requested = false
    if completed then completed(not failure) end
    synchronize_state(function(_, sync_failure)
      if sync_failure then return end
      M.render()
      if not failure then M.drain() end
    end)
  end, function()
    if submitted_text and harness_state() == state and composer_text(state.composer_buf) == submitted_text then
      set_composer_text(state.composer_buf, "")
    end
  end)
  M.refresh_winbar()
end

function M.set_mode(mode)
  mode = mode:lower()
  if not vim.tbl_contains({ "read", "write", "yolo" }, mode) then
    report_configuration_error("Unknown permission: " .. mode)
    return
  end
  M.task_transition({ action = "permission", mode = mode })
end

function M.toggle_mode()
  local state = harness_state()
  local current_mode = state.pending_mode
    or (state.session and state.session.execution_mode) or "read"
  M.set_mode(current_mode == "read" and "write" or "read")
end

function M.select_mode()
  local state = harness_state()
  local current_mode = state.pending_mode
    or (state.session and state.session.execution_mode) or "read"
  local detail_list = {
    read = "Ask before edits and untrusted commands.",
    write = "Apply saved approval rules within configured access.",
    yolo = "Skip approval prompts within configured access.",
  }
  local option_list = vim.tbl_map(function(mode)
    local label = mode == "yolo" and "YOLO" or (mode:sub(1, 1):upper() .. mode:sub(2))
    if mode == current_mode then label = label .. " (current)" end
    return {
      label = label,
      detail = detail_list[mode],
      value = mode,
      highlight_group = require("forge.infra.highlights").harness_mode(mode),
      highlight_text = mode == "yolo" and "YOLO" or (mode:sub(1, 1):upper() .. mode:sub(2)),
    }
  end, { "read", "write", "yolo" })
  open_choice_picker(state, "Select Mode", nil, option_list,
    M.set_mode)
end

---@param next_config table
---@param validate_selection? boolean
---@param completed? fun(applied: boolean)
configure_now = function(next_config, validate_selection, completed)
  local state = harness_state()
  local request_config = vim.tbl_extend("force", {}, next_config, { validate = validate_selection == true })
  state.configuring = true
  state.configuring_config = request_config
  M.refresh_winbar()
  client.request("session.configure", request_config, function(result, request_error)
    if state.configuring_config ~= request_config then return end
    state.configuring = false
    state.configuring_config = nil
    if request_error then
      if next_config.model == nil and (next_config.effort ~= nil or next_config.service_tier ~= nil) then
        state.configuration_error = request_error
      else
        report_configuration_error(request_error)
      end
      M.refresh_winbar()
      if completed then completed(false) end
      if state.pending_config then vim.schedule(M.drain) end
      return
    end
    state.session = result
    state.configuration_error = nil
    prune_pending_settings(state)
    M.refresh_winbar()
    if next_config.model and not result.resolved_model then M.resolve_runtime_model() end
    if completed then completed(true) else vim.schedule(M.drain) end
  end)
end

---@param next_config table
---@param validate_selection? boolean
---@param completed? fun(applied: boolean)
function M.configure(next_config, validate_selection, completed)
  local state = harness_state()
  if state.busy or state.task_operation then
    state.pending_config, state.pending_config_validate = nil, false
    local requested = vim.tbl_extend("force", state.task_config or {}, next_config, { validate = validate_selection == true })
    state.task_config = requested
    if state.configuration_completion then state.configuration_completion.complete(false) end
    local finished = false
    local configuration = { complete = function(applied)
      if finished then return end
      finished = true
      if completed then completed(applied) end
    end }
    state.configuration_completion = configuration
    M.task_transition({ action = "configure", config = requested }, nil, function(applied)
      if state.task_config == requested then state.task_config = nil end
      if state.configuration_completion == configuration then state.configuration_completion = nil end
      configuration.complete(false)
    end)
    return
  end
  local tuning_only = not completed and next(next_config) ~= nil
  for field in pairs(next_config) do
    if field ~= "effort" and field ~= "service_tier" then tuning_only = false end
  end
  state.configuration_error = nil
  if state.busy or state.configuring or tuning_only then
    state.pending_config = vim.tbl_extend("force", state.pending_config or {}, next_config)
    state.pending_config_validate = state.pending_config_validate == true or validate_selection == true
    prune_pending_settings(state)
    if tuning_only then
      local revision = (state.configuration_revision or 0) + 1
      state.configuration_revision = revision
      state.configuration_debounce = true
      local generation = client.host_generation()
      local session_id = state.session and state.session.id
      vim.defer_fn(function()
        if state.configuration_revision ~= revision then return end
        state.configuration_debounce = nil
        if client.host_generation() ~= generation or (state.session and state.session.id) ~= session_id then return end
        local previous = session.harness
        session.activate_harness(state)
        M.drain()
        if previous ~= state then session.activate_harness(previous) end
      end, 100)
    end
    M.refresh_winbar()
    return
  end
  local merged = vim.tbl_extend("force", state.pending_config or {}, next_config)
  validate_selection = validate_selection == true or state.pending_config_validate == true
  state.pending_config, state.pending_config_validate = nil, false
  state.configuration_revision = (state.configuration_revision or 0) + 1
  state.configuration_debounce = nil
  configure_now(merged, validate_selection, completed)
end

local function close()
  local state = harness_state()
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
  timeline_status.stop(state)
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
  require("forge.views.harness.health").watch(state, function()
    if harness_state() == state then M.render() end
  end)
  if previous_session ~= state.session.id then state.queue = {} end
  M.render()
  M.resolve_runtime_model()
  if state.active_elicitation and state.active_elicitation.elicitation then
    state.presented_question_key = nil
    vim.schedule(M.present_plan_question)
  end
  if #state.approval > 0 then vim.schedule(M.present_approval) end
  if state.goal and state.goal.state == "active" then vim.schedule(M.drain) end
end

---@return ForgeViewCommandSet
function M.command_set()
  local set = command_set.new()
  command_set.register(set, "edit_queued", M.edit_last_queued)
  command_set.register(set, "submit", M.submit)
  command_set.register(set, "queue", M.queue_submit)
  command_set.register(set, "steer", M.steer_submit)
  command_set.register(set, "cancel", M.cancel_turn)
  command_set.register(set, "toggle_mode", M.toggle_mode)
  command_set.register(set, "previous_prompt", function() M.jump_prompt(-1) end)
  command_set.register(set, "next_prompt", function() M.jump_prompt(1) end)
  command_set.register(set, "toggle_activity", M.toggle_activity)
  command_set.register(set, "open_artifact", M.open_artifact_picker)
  command_set.register(set, "abort_plan", M.abort_plan)
  command_set.register(set, "agent", M.open_agent_picker)
  command_set.register(set, "sessions", M.open_session_picker)
  command_set.register(set, "background", M.open_background_picker)
  command_set.register(set, "open_timeline", M.open_timeline_entry)
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
  local state = harness_state()
  for _, key in ipairs(keymaps.view_keys_for("harness", "open_timeline")) do
    pcall(vim.keymap.del, "n", key, { buffer = buf })
  end
  state.command_set = M.command_set()
  keymaps.setup_view_keymaps(buf, "harness", state.command_set)
end

function M.attach()
  local state = harness_state()
  require("forge.views.harness.health").watch(state, function()
    if harness_state() == state then M.render() end
  end)
  if client.host_accepting() then state.host_error = nil end
  state.command_set = M.command_set()
  M.attach_transcript(state.transcript_buf)
  local composer_command_set = M.command_set()
  command_set.unregister(composer_command_set, "open_timeline")
  command_set.register(composer_command_set, "history_previous", prompt_history.previous)
  command_set.register(composer_command_set, "history_next", prompt_history.next)
  keymaps.setup_view_keymaps(state.composer_buf, "harness", composer_command_set)
  require("forge.views.harness.workspace").attach(state)
  prompt_history.attach(state.composer_buf)
  if state.unsubscribe then state.unsubscribe() end
  state.unsubscribe = client.subscribe(function(event, payload, event_session_id)
    if state.switching_backend then return end
    if state.session and event_session_id and state.session.id ~= event_session_id then return end
    local active_state = session.harness
    session.activate_harness(state)
    local ok, failure = xpcall(function() on_event(event, payload) end, debug.traceback)
    if not ok then
      state.sync_error = "Presentation failed: " .. tostring(failure)
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
