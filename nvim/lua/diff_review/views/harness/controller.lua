local M = {}

local client = require("diff_review.harness.client")
local command_set = require("diff_review.shared.view_command_set")
local config = require("diff_review.infra.config")
local keymaps = require("diff_review.shared.keymaps")
local notifications = require("diff_review.infra.notifications")
local perf = require("diff_review.infra.perf")
local renderer = require("diff_review.render.harness.interaction_tree")
local markdown = require("diff_review.render.harness.markdown")
local queue_renderer = require("diff_review.render.harness.queue")
local render_transaction = require("diff_review.render.harness.transaction")
local layout = require("diff_review.views.harness.layout")
local session = require("diff_review.session")
local interaction_state = require("diff_review.views.harness.interaction_state")
local prompt_history = require("diff_review.views.harness.prompt_history")
local provider_picker = require("diff_review.views.harness.provider_picker")
local context_status = require("diff_review.views.harness.context_status")
local main_timeline = require("diff_review.views.harness.timeline")
local model_picker = require("diff_review.views.harness.model_picker")
local snapshot = require("diff_review.views.harness.snapshot")
local picker = require("diff_review.views.picker")
local timeline_status = require("diff_review.views.harness.timeline_status")
local question_presentation = require("diff_review.views.harness.question_presentation")
local session_navigation = require("diff_review.views.harness.session_navigation")

local namespace = vim.api.nvim_create_namespace("DiffReviewHarnessTranscript")
local queue_namespace = vim.api.nvim_create_namespace("DiffReviewHarnessQueue")
local render_observer_for_test = nil
local begin_request
local submit_immediate
local effort_list = { "minimal", "low", "medium", "high", "xhigh" }

---@return table
local function harness_state() return session.harness end

---@param message string
---@param severity? "status"|"error"
local function append_session_status(message, severity)
  local state = harness_state()
  local previous_entry = state.timeline[#state.timeline]
  local entry = {
    kind = "session_event",
    id = "local-session-event-" .. tostring(vim.uv.hrtime()),
    created_at_ms = os.time() * 1000,
    event = { message = message, severity = severity or "status" },
    local_session_id = state.session and state.session.id,
    local_after_id = previous_entry and previous_entry.id,
  }
  state.local_session_event = state.local_session_event or {}
  state.local_session_event[#state.local_session_event + 1] = entry
  state.timeline[#state.timeline + 1] = entry
  M.render()
end

---@param message string
local function report_configuration_error(message)
  append_session_status("Configuration rejected: " .. message, "error")
  notifications.error(message, "Harness")
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

local function open_choice_picker(state, title, subtitle, option_list, callback)
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
        option_list = option_list,
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

local function selected_agent_timeline(state)
  if not state.selected_agent_run_id then return nil end
  local timeline = {}
  for _, turn in ipairs((state.agent and state.agent.turn) or {}) do
    if turn.agent_run_id == state.selected_agent_run_id then
      timeline[#timeline + 1] = { kind = "interaction", interaction = turn.interaction }
    end
  end
  local live = (state.agent_live or {})[state.selected_agent_run_id]
  if live and live.interaction then
    local interaction = vim.deepcopy(live.interaction)
    interaction.active = vim.deepcopy(live.active)
    timeline[#timeline + 1] = { kind = "interaction", interaction = interaction }
  end
  return timeline
end

local function discard_terminal_agent_live_state(state)
  state.agent_live = state.agent_live or {}
  for _, run in ipairs((state.agent and state.agent.run) or {}) do
    if run.status == "completed" or run.status == "failed" or run.status == "interrupted" or run.status == "closed" then
      state.agent_live[run.id] = nil
    end
  end
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
  vim.b[buf].diff_review_queue_rows = row_count
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
---@param text string
local function set_composer_text(buf, text)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.split(text, "\n", { plain = true }))
end

local function restore_retracted_prompt(text)
  local state = harness_state()
  if composer_text(state.composer_buf) ~= "" then
    table.insert(state.queue, 1, text)
    notifications.warn("The retracted prompt was queued because the composer already contains a newer draft", "Harness")
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
  local raw_mode = state.pending_mode or active_session.execution_mode or "read"
  local mode = raw_mode:lower() == "yolo" and "YOLO" or (raw_mode:sub(1, 1):upper() .. raw_mode:sub(2))
  if state.pending_mode then mode = mode .. "*" end
  local backend = active_session.backend or config.options.harness.backend
  local provider = active_session.provider_label
    or (backend == "codex" and "Codex CLI")
    or (backend == "copilot" and "Copilot CLI")
    or backend
  local configured_model = active_session.model or config.options.harness.model
  local model = active_session.resolved_model or (configured_model == "default" and "resolving model" or configured_model)
  local effort = active_session.effort or config.options.harness.effort
  if state.pending_config and state.pending_config.model then model = state.pending_config.model .. "*" end
  if state.pending_config and state.pending_config.effort then effort = state.pending_config.effort .. "*" end
  local busy = state.cancel_requested and " • cancelling"
    or state.mode_restart_requested and " • restarting"
    or state.busy and " • running"
    or (#state.queue > 0 and (" • queued " .. #state.queue) or "")
  local goal = nil
  if state.goal and state.goal.objective and state.goal.state ~= "cleared" then
    local goal_state = state.goal.state and state.goal.state ~= "active" and (" [" .. state.goal.state .. "]") or ""
    goal = " • Goal: " .. state.goal.objective .. goal_state
  end
  local fast = active_session.fast_mode and " fast" or ""
  local segment_list = {
    {
      text = mode,
      group = ({
        read = "DiffReviewHarnessRead",
        write = "DiffReviewHarnessWrite",
        full = "DiffReviewHarnessFull",
        yolo = "DiffReviewHarnessYolo",
      })[raw_mode:lower()] or "DiffReviewHarnessRead",
    },
    {
      text = (" • %s • %s %s%s"):format(provider, model, effort, fast),
      group = "DiffReviewStatusLabel",
    },
  }
  local selected_run = selected_agent_run(state)
  segment_list[#segment_list + 1] = {
    text = selected_run and (" • " .. (selected_run.nickname or selected_run.definition)) or " • Main",
    group = "DiffReviewStatusLabel",
  }
  if goal then
    segment_list[#segment_list + 1] = { text = goal, group = "DiffReviewHarnessGoal" }
  end
  local artifact_count = #(state.artifact or {})
  if artifact_count > 0 then
    segment_list[#segment_list + 1] = {
      text = (" • %d %s"):format(artifact_count, artifact_count == 1 and "artifact" or "artifacts"),
      group = "DiffReviewStatusLabel",
    }
  end
  if #(state.approval or {}) > 0 then
    local reopen_key = keymaps.view_keys_for("harness", "reopen_question")[1]
    local reopen_hint = reopen_key and (" (press " .. reopen_key .. ")") or ""
    segment_list[#segment_list + 1] = {
      text = " • Approval requested" .. reopen_hint,
      group = "DiffReviewHarnessWrite",
    }
  end
  if busy ~= "" then
    segment_list[#segment_list + 1] = { text = busy, group = "DiffReviewStatusLabel" }
  end
  return segment_list
end

function M.refresh_winbar()
  local state = harness_state()
  if not state.command_set then return end
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
    vim.wo[state.composer_win].winbar = ""
  end
  render_queue()
end

---@return integer?
local function working_seconds()
  local state = harness_state()
  if not state.busy or not state.working_started_ns then return nil end
  return math.floor((vim.uv.hrtime() - state.working_started_ns) / 1000000000)
end

---@param reset? boolean
function M.render(reset)
  local state = harness_state()
  local buf = state.transcript_buf
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  local transcript_win = state.transcript_win
  local transcript_visible = transcript_win and vim.api.nvim_win_is_valid(transcript_win)
  local follow_tail = false
  if transcript_visible then
    local active_win = vim.api.nvim_get_current_win()
    local previous_last_line = vim.api.nvim_buf_line_count(buf)
    vim.api.nvim_win_call(transcript_win, function()
      follow_tail = active_win ~= transcript_win or vim.api.nvim_win_get_cursor(transcript_win)[1] >= previous_last_line
    end)
  end
  local agent_timeline = selected_agent_timeline(state)
  local timeline = agent_timeline or main_timeline.project(state)
  local status = agent_timeline and nil or timeline_status.resolve(state)
  local render = renderer.build(timeline, {
    working_seconds = working_seconds(),
    content_width = transcript_visible and vim.api.nvim_win_get_width(transcript_win) or nil,
    cwd = vim.uv.cwd(),
    on_diff_update = function() vim.schedule(M.render) end,
    expanded = state.activity_expanded,
    now_ms = os.time() * 1000,
    timeline_status = status,
  })
  state.prompt_line = render.prompt_lines
  state.activity_range = render.folds
  state.timeline_row = render.rows
  state.render_namespace = namespace
  local transaction = render_transaction.apply(state, render, {
    reset = reset == true,
    follow_tail = follow_tail,
  })
  timeline_status.synchronize(state, render.timeline_status_line)
  markdown.render(buf, transcript_win, render.markdown_ranges)
  if render_observer_for_test then
    render_observer_for_test(vim.deepcopy(render.lines), vim.deepcopy(transaction))
  end
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
  require("diff_review.views.harness.approval").close()
  state.approval_open = false
  state.presented_approval_id = nil
end

---@param callback? function
local function synchronize_state(callback)
  local synchronized_state = harness_state()
  if synchronized_state.state_sync_pending then return end
  synchronized_state.state_sync_pending = true
  client.request("state.get", {}, function(result, request_error)
    synchronized_state.state_sync_pending = false
    if request_error then
      notifications.error(request_error, "Harness state")
      return
    end
    if not result then
      notifications.error("Harness broker returned an empty state snapshot", "Harness state")
      return
    end
    local state = harness_state()
    local next_session_id = result.session and result.session.id
    local local_session_event = vim.tbl_filter(function(entry)
      return entry.local_session_id == next_session_id
    end, state.local_session_event or {})
    snapshot.apply(state, result)
    state.local_session_event = local_session_event
    for _, entry in ipairs(local_session_event) do
      local insertion_index = #state.timeline + 1
      if entry.local_after_id then
        for index, candidate in ipairs(state.timeline) do
          if candidate.id == entry.local_after_id then
            insertion_index = index + 1
            break
          end
        end
      end
      table.insert(state.timeline, insertion_index, vim.deepcopy(entry))
    end
    M.attach_transcript(state.transcript_buf)
    require("diff_review.views.harness.agent_picker").refresh()
    reconcile_approval_presentation(state)
    local elicitation = state.active_elicitation and state.active_elicitation.elicitation
    if elicitation and question_presentation.should_present(state) then
      if state.plan_question_open then
        require("diff_review.views.harness.plan_question").close()
        state.plan_question_open = false
      end
      vim.schedule(function() M.present_plan_question(false) end)
    end
    if #state.approval > 0 then vim.schedule(M.present_approval) end
    M.render()
    if callback then callback(result) end
  end)
end

---@param state table
---@param entry table
local function upsert_timeline_entry(state, entry)
  for index, previous in ipairs(state.timeline or {}) do
    if previous.id == entry.id then
      state.timeline[index] = entry
      return
    end
  end
  state.timeline[#state.timeline + 1] = entry
end

---@param event string
---@param payload table
local function on_event(event, payload)
  local state = harness_state()
  if event == "backend_event" then
    if payload.kind == "approval_requested" then
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
      discard_terminal_agent_live_state(state)
      require("diff_review.views.harness.agent_picker").refresh()
      schedule_render()
    elseif payload.kind == "agent_timeline_updated" then
      local update = payload.data or payload
      state.agent_live = state.agent_live or {}
      state.agent_live[update.run_id] = update
      require("diff_review.views.harness.agent_picker").refresh()
      schedule_render()
    elseif payload.kind == "context_usage" then
      if state.session then state.session.context_usage = payload.data or payload end
      M.refresh_winbar()
    elseif payload.kind == "timeline_node_updated" then
      local update = payload.data or payload
      interaction_state.apply_node(state, update)
      local acknowledged = update.node and update.node.prompt
      for index, pending in ipairs(state.pending_steer or {}) do
        if acknowledged and pending.text == acknowledged.text then
          table.remove(state.pending_steer, index)
          break
        end
      end
      M.refresh_winbar()
      schedule_render()
    elseif payload.kind == "timeline_wait_updated" then
      interaction_state.apply_wait(state, payload.data or payload)
      schedule_render()
    elseif payload.kind == "timeline_task_updated" then
      interaction_state.apply_task(state, payload.data or payload)
      schedule_render()
    elseif payload.kind == "timeline_interaction_retracted" then
      interaction_state.retract(state, payload.data or payload)
      M.render()
    elseif payload.kind == "timeline_interaction_started" then
      interaction_state.start_interaction(state, payload.data or payload)
      schedule_render()
    elseif payload.kind == "timeline_interaction_cancelled" then
      interaction_state.complete_interaction(state, payload.data or payload)
      schedule_render()
    elseif payload.kind == "timeline_plan_lifecycle" then
      local entry = vim.deepcopy(payload.data or payload)
      upsert_timeline_entry(state, entry)
      schedule_render()
    elseif payload.kind == "timeline_session_event" then
      upsert_timeline_entry(state, vim.deepcopy(payload.data or payload))
      M.render()
    elseif payload.kind == "error" and type(payload.text) == "string" then
      notifications.warn(payload.text, "Harness")
    end
  elseif event == "question" then
    state.active_elicitation = payload
    if question_presentation.should_present(state) then
      if state.plan_question_open then
        require("diff_review.views.harness.plan_question").close()
        state.plan_question_open = false
      end
      vim.schedule(function() M.present_plan_question(false) end)
    end
    schedule_render()
  elseif event == "question_updated" then
    state.active_elicitation = payload
    if question_presentation.should_present(state) then
      require("diff_review.views.harness.plan_question").close()
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
  elseif event == "plan_created" or event == "plan_revision_created" or event == "plan_changes_requested"
    or event == "plan_question_answered" or event == "plan_question_withdrawn"
    or event == "plan_accepted" or event == "plan_cancelled"
    or event == "plan_activated"
  then
    synchronize_state()
  elseif event == "goal_changed" or event == "goal_continue_requested" then
    state.goal = payload.state == "cleared" and nil or payload
    M.refresh_winbar()
    if event == "goal_continue_requested" then vim.schedule(M.drain) end
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
      interaction_state.replace(state, payload.interaction or {})
      state.queue = {}
      state.goal = nil
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
    if event == "execution_mode_changed" and not state.mode_restart_requested then state.pending_mode = nil end
    M.refresh_winbar()
  elseif event == "interaction_rolled_back" then
    synchronize_state()
  elseif event == "agent_updated" then
    state.agent = vim.deepcopy(payload)
    require("diff_review.views.harness.agent_picker").refresh()
    schedule_render()
  elseif event == "interaction_complete" or event == "interaction_updated" then
    interaction_state.complete_interaction(state, payload.interaction or payload)
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
  require("diff_review.views.harness.approval").open(request, {
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
  if not force and not question_presentation.should_present(state) then return end
  question_presentation.mark_presented(state)
  state.plan_question_open = true
  require("diff_review.views.harness.plan_question").open(elicitation, {
    transcript_win = state.transcript_win,
    window_list = picker_host(state).window_list,
    control_win = picker_host(state).control_win,
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
      client.request("question.ask", params, function(_, request_error)
        set_busy(false)
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
      client.request("question.continue", {}, function(_, request_error)
        set_busy(false)
        if request_error then
          notifications.error(request_error, "Planning continuation")
          synchronize_state(function() vim.schedule(function() M.present_plan_question(true) end) end)
          return
        end
        synchronize_state()
      end)
    end,
    closed = function()
      state.plan_question_open = false
      local reopen_key = keymaps.view_keys_for("harness", "reopen_question")[1]
      local reopen_action = reopen_key and (reopen_key .. " or /questions") or "/questions"
      notifications.info(("Harness questions remain available with %s."):format(reopen_action), "Harness")
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
    notifications.warn("No Harness approval or question awaits feedback", "Harness")
    return
  end
  M.present_plan_question(true)
end

---@param mode string
local function resume_mode_restart(mode)
  local state = harness_state()
  local label = mode == "yolo" and "YOLO" or (mode:sub(1, 1):upper() .. mode:sub(2))
  set_busy(true)
  client.request("session.execution_mode", { mode = mode }, function(session_result, mode_error)
    if mode_error then
      state.mode_restart_requested = false
      state.pending_mode = nil
      set_busy(false)
      report_configuration_error("Could not change execution mode after interruption: " .. mode_error)
      synchronize_state()
      return
    end
    state.session = session_result or state.session
    append_session_status("Execution mode changed to " .. label)
    client.request("interaction.resume", {
      text = "Continue the active task from the interrupted turn. Execution mode is now " .. label
        .. ". Preserve completed work and do not repeat finished actions.",
    }, function(result, resume_error)
      state.mode_restart_requested = false
      state.pending_mode = nil
      set_busy(false)
      if resume_error then
        notifications.error("Harness could not resume after changing to " .. label .. ": " .. resume_error, "Harness")
        synchronize_state()
        return
      end
      if result then
        state.session = result.session or (result.id and result) or state.session
        state.capability = result.capability or state.capability
        if result.interaction then interaction_state.complete_interaction(state, result.interaction) end
      end
      synchronize_state()
      vim.schedule(M.drain)
    end)
  end)
end

local function clear_mcp_restart(state)
  state.mcp_restart = nil
end

local function maybe_resume_mcp_restart()
  local state = harness_state()
  local restart = state.mcp_restart
  if not (restart and restart.mutation_finished and restart.turn_cancelled) then return end
  picker.close(false)
  set_busy(true)
  local change_summary = restart.error and ("The MCP change failed: " .. restart.error)
    or ("MCP server %s is now %s."):format(restart.name, restart.enabled and "enabled" or "disabled")
  client.request("interaction.resume", {
    text = "Continue the active task from the interrupted turn. " .. change_summary
      .. " Preserve completed work and do not repeat finished actions.",
  }, function(result, resume_error)
    clear_mcp_restart(state)
    set_busy(false)
    if resume_error then
      notifications.error("Harness could not resume after changing MCP state: " .. resume_error, "Harness MCP")
      synchronize_state()
      return
    end
    if result then
      state.session = result.session or (result.id and result) or state.session
      state.capability = result.capability or state.capability
      if result.interaction then interaction_state.complete_interaction(state, result.interaction) end
    end
    synchronize_state()
    vim.schedule(M.drain)
  end)
end

---@param text string
begin_request = function(text)
  local state = harness_state()
  local selected_run = selected_agent_run(state)
  state.cancel_requested = false
  if not selected_run then interaction_state.begin(state, text) end
  set_busy(true)
  local goal_objective = text:match("^/goal%s+(.+)$")
  if goal_objective and goal_objective ~= "pause" and goal_objective ~= "resume" and goal_objective ~= "clear" then
    state.goal = { objective = goal_objective, state = "active" }
  end
  M.render()
  local method = selected_run and "agent.submit" or "prompt.submit"
  local params = selected_run and { run_id = selected_run.id, text = text } or { text = text }
  client.request(method, params, function(result, request_error, error_detail)
    set_busy(false)
    state.cancel_requested = false
    if request_error then
      if error_detail and error_detail.code == "turn_retracted" then
        local prompt = error_detail.data and error_detail.data.prompt or text
        restore_retracted_prompt(prompt)
        synchronize_state()
        return
      end
      if error_detail and error_detail.code == "turn_cancelled" then
        if state.mcp_restart then
          state.mcp_restart.turn_cancelled = true
          maybe_resume_mcp_restart()
          return
        end
        if state.mode_restart_requested and state.pending_mode then
          resume_mode_restart(state.pending_mode)
          return
        end
        synchronize_state()
        vim.schedule(M.drain)
        return
      end
      if not selected_run then interaction_state.fail_pending(state, request_error) end
      notifications.error(request_error, "Harness")
      M.render()
      return
    elseif result then
      state.session = result.session or (result.id and result) or state.session
      state.capability = result.capability or state.capability
      if result.interaction then interaction_state.complete_interaction(state, result.interaction) end
    end
    synchronize_state()
    vim.schedule(M.drain)
  end)
end

function M.cancel_turn()
  local state = harness_state()
  if not state.busy then
    notifications.warn("No Harness turn is running", "Harness")
    return
  end
  if state.cancel_requested then return end
  state.mode_restart_requested = false
  state.pending_mode = nil
  state.cancel_requested = true
  M.refresh_winbar()
  local restore_prompt = state.capability.native_turn_rollback == true
    and #(state.queue or {}) == 0
    and #(state.pending_steer or {}) == 0
    and composer_text(state.composer_buf) == ""
  local selected_run = selected_agent_run(state)
  local target = selected_run and selected_run.provider_thread_id and selected_run.active_turn_id and {
    thread_id = selected_run.provider_thread_id,
    turn_id = selected_run.active_turn_id,
  } or nil
  client.request("turn.cancel", { restore_prompt_if_no_output = restore_prompt, target = target }, function(result, request_error)
    if request_error then
      state.cancel_requested = false
      notifications.error(request_error, "Harness cancel")
      M.refresh_winbar()
      return
    end
    if not (result and result.cancel_requested) then
      state.cancel_requested = false
      notifications.warn("The Harness turn already finished", "Harness")
      M.refresh_winbar()
    end
  end)
end

function M.compact()
  local state = harness_state()
  if not (state.capability and state.capability.native_compact) then
    notifications.warn("The current backend does not support manual context compaction", "Harness")
    return
  end
  set_busy(true)
  M.refresh_winbar()
  client.request("session.compact", {}, function(result, request_error)
    set_busy(false)
    if request_error then
      notifications.error(request_error, "Harness compact")
      M.refresh_winbar()
      return
    end
    if result then
      state.session = result.session or state.session
      state.capability = result.capability or state.capability
    end
    synchronize_state()
    vim.schedule(M.drain)
  end)
end

function M.drain()
  local state = harness_state()
  if state.busy then return end
  if #(state.pending_steer or {}) > 0 then return end
  if state.pending_backend then
    local pending_backend = state.pending_backend
    state.pending_backend = nil
    require("diff_review.views.harness").switch_backend(pending_backend)
    return
  end
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
    M.configure(pending_config, validate_selection)
    return
  end
  if state.active_elicitation and state.active_elicitation.elicitation then
    return
  end
  local text = table.remove(state.queue, 1)
  if text then
    if text == "/compact" then M.compact() else begin_request(text) end
    return
  end
  if state.goal and state.goal.state == "active" then
    set_busy(true)
    M.refresh_winbar()
    client.request("goal.continue", {}, function(result, request_error)
      set_busy(false)
      if request_error then
        notifications.error(request_error, "Harness Goal")
        M.render()
        return
      end
      if result then state.session = result.session or state.session end
      M.render()
      vim.schedule(M.drain)
    end)
  end
end

---@param name string
function M.rename_session(name)
  local state = harness_state()
  local active_session = state.session
  if not active_session or not active_session.id then
    notifications.error("No active Harness session to rename", "Harness")
    return
  end
  client.request("session.rename", { session_id = active_session.id, name = name }, function(result, request_error)
    if request_error then
      notifications.error(request_error, "Harness")
      return
    end
    if result then state.session = result end
    M.refresh_winbar()
  end)
end

---@param name? string
function M.fork_session(name)
  local state = harness_state()
  local active_session = state.session
  if not (active_session and active_session.id) then
    notifications.error("No active Harness session to fork", "Harness")
    return
  end
  if state.capability.native_fork ~= true then
    notifications.warn("The current backend does not support session fork", "Harness")
    return
  end
  local params = { session_id = active_session.id }
  if name and name ~= "" then params.name = name end
  local fork_started = perf.now()
  local pending = session_navigation.begin_fork(active_session, name)
  perf.event("harness.fork.ui_opened", {
    ms = perf.elapsed_ms(fork_started),
    source_session_id = active_session.id,
  })
  client.request_for(active_session.id, "session.fork", params, function(result, request_error)
    if request_error then
      perf.event("harness.fork.failed", {
        ms = perf.elapsed_ms(fork_started),
        source_session_id = active_session.id,
        error = tostring(request_error),
      })
      session_navigation.fail_fork(pending, request_error)
      return
    end
    local performance = result and result.fork_performance or {}
    perf.event("harness.fork.complete", {
      ms = perf.elapsed_ms(fork_started),
      broker_ms = performance.total_ms,
      source_session_id = active_session.id,
      child_session_id = result.session and result.session.id,
    })
    for _, timing in ipairs(performance.timing or {}) do
      perf.event("harness.fork.phase", {
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
  local row = vim.api.nvim_win_get_cursor(0)[1]
  local timeline_row = state.timeline_row and state.timeline_row[row]
  if timeline_row and timeline_row.target_session_id then
    session_navigation.open_parent(timeline_row.target_session_id)
  end
end

function M.open_artifact_picker()
  local state = harness_state()
  local artifact_list = state.artifact or {}
  if #artifact_list == 0 then
    notifications.info("This Harness session has no artifacts", "Harness")
    return
  end
  local option_list = vim.tbl_map(function(artifact)
      local title = artifact.title and artifact.title ~= "" and artifact.title or "[unnamed plan]"
      return { label = title, detail = artifact.state or "unknown", value = artifact }
    end, artifact_list)
  open_choice_picker(state, "Harness artifacts", "Open a plan created by this session.", option_list, function(artifact)
    if not artifact then return end
    client.request("plan.activate", { plan_id = artifact.id }, function(plan, request_error)
      if request_error then
        notifications.error(request_error, "Harness artifact")
        return
      end
      state.active_plan = plan
      require("diff_review.views.plan_review").open(plan)
      synchronize_state()
    end)
  end)
end

---@class DiffReviewRollbackInteraction
---@field id string
---@field ordinal integer
---@field prompt string
---@field state "complete"|"failed"|"cancelled"
---@field checkpoint_before string

---@param interaction DiffReviewRollbackInteraction
local function restore_interaction_prompt(interaction)
  local state = harness_state()
  set_composer_text(state.composer_buf, interaction.prompt or "")
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

---@param interaction DiffReviewRollbackInteraction
local function rollback_interaction(interaction)
  local state = harness_state()
  if state.busy then
    notifications.warn("Cancel or finish the active turn before undoing an interaction", "Harness undo")
    return
  end
  client.request("interaction.rollback", { interaction_id = interaction.id }, function(_, request_error)
    if request_error then
      notifications.error(request_error, "Harness undo")
      return
    end
    notifications.info("Rolled back to before Interaction " .. tostring(interaction.ordinal), "Harness undo")
    synchronize_state()
    restore_interaction_prompt(interaction)
  end)
end

---@param interaction DiffReviewRollbackInteraction
local function confirm_rollback(interaction)
  open_choice_picker(
    harness_state(),
    "Confirm rollback",
    "Restore the worktree before Interaction " .. tostring(interaction.ordinal)
      .. " and supersede every later interaction?",
    {
      { label = "Cancel", detail = "Keep the current workspace.", value = false },
      {
        label = "Rollback to before Interaction " .. tostring(interaction.ordinal),
        detail = "Restore its checkpoint and return the prompt to the composer.",
        value = true,
      },
    },
    function(confirmed)
      if confirmed then rollback_interaction(interaction) end
    end
  )
end

---Open the checkpointed interaction picker for an editable rollback.
function M.open_undo_picker()
  local state = harness_state()
  if state.busy then
    notifications.warn("Cancel or finish the active turn before undoing an interaction", "Harness undo")
    return
  end
  if state.no_checkpoint then
    notifications.warn("Undo is unavailable because this session has NO CHECKPOINT", "Harness undo")
    return
  end
  client.request("interaction.list", {}, function(interaction_list, request_error)
    if request_error then
      notifications.error(request_error, "Harness undo")
      return
    end
    local rollback_state = { complete = true, failed = true, cancelled = true }
    local option_list = {}
    for index = #(interaction_list or {}), 1, -1 do
      local interaction = interaction_list[index]
      if interaction.checkpoint_before and rollback_state[interaction.state] then
        local prompt = vim.trim(tostring(interaction.prompt or ""):gsub("%s+", " "))
        option_list[#option_list + 1] = {
          id = interaction.id,
          label = "Interaction " .. tostring(interaction.ordinal),
          detail = tostring(interaction.state) .. " · " .. (prompt ~= "" and prompt or "[empty prompt]"),
          value = interaction,
        }
      end
    end
    if #option_list == 0 then
      notifications.info("This Harness session has no interactions available to undo", "Harness undo")
      return
    end
    open_choice_picker(
      state,
      "Undo interaction",
      "Select the interaction whose original prompt should be restored.",
      option_list,
      confirm_rollback
    )
  end)
end

---Switch the transcript and composer target to one child-agent timeline or Main.
---@param selector string
function M.select_agent(selector)
  local state = harness_state()
  if selector:lower() == "main" then
    state.selected_agent_run_id = nil
  else
    local run, resolve_error = require("diff_review.views.harness.agent_catalog").resolve(state.agent, selector)
    if not run then
      notifications.warn(resolve_error or ("No child agent matches " .. selector), "Harness agent")
      return
    end
    state.selected_agent_run_id = run.id
  end
  state.activity_expanded = {}
  M.render(true)
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
  set_busy(true)
  client.request("agent.start", { definition = definition, task = task }, function(_, request_error)
    set_busy(false)
    if request_error then notifications.error(request_error, "Harness agent") end
    synchronize_state()
    vim.schedule(M.drain)
  end)
end

---Open the provider-backed child-agent timeline selector.
function M.open_agent_picker()
  local state = harness_state()
  require("diff_review.views.harness.agent_picker").open({
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
  local opened = require("diff_review.views.harness.spawn_picker").open({
    host = picker_host(state),
    definition_list = (state.agent and state.agent.definition) or {},
    definition_name = definition_name,
    on_spawn = M.spawn_agent,
  })
  if not opened then notifications.warn("Unknown agent definition: " .. definition_name, "Harness agent") end
end

function M.open_session_picker()
  require("diff_review.views.harness.session_picker").open(picker_host(harness_state()))
end

function M.submit()
  local state = harness_state()
  local text = composer_text(state.composer_buf)
  if text == "" then return end
  prompt_history.record(text)
  if vim.tbl_contains({ "/read", "/write", "/full", "/yolo" }, text) then
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
    if not vim.tbl_contains({ "read", "write", "full", "yolo" }, execution_mode) then
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
    require("diff_review.views.harness").new_session()
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
    notifications.warn("Use /agent main, /agent <running alias>, or /agent", "Harness")
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
    notifications.warn("Use /spawn <definition> <task>", "Harness")
    return
  end
  if text == "/compact" then
    set_composer_text(state.composer_buf, "")
    if state.capability.native_compact ~= true then
      notifications.warn("The current backend does not support manual context compaction", "Harness")
      return
    end
    if state.busy then
      state.queue[#state.queue + 1] = text
      M.refresh_winbar()
    else
      M.compact()
    end
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
    require("diff_review.views.harness").new_session(vim.trim(new_name))
    return
  end
  local model, model_effort = text:match("^/model%s+(%S+)%s+(%S+)$")
  if not model then model = text:match("^/model%s+(%S+)$") end
  if model then
    set_composer_text(state.composer_buf, "")
    if state.capability.model_selection ~= true then
      notifications.warn("The current backend does not support model selection", "Harness")
      return
    end
    if model_effort and state.capability.effort_selection ~= true then
      notifications.warn("The current backend does not support reasoning effort selection", "Harness")
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
      notifications.warn("The current backend does not support reasoning effort selection", "Harness")
      return
    end
    if not vim.tbl_contains(effort_list, effort) then
      report_configuration_error("Unknown reasoning effort: " .. effort)
      return
    end
    M.configure({ effort = effort }, true)
    return
  end
  if text == "/fast" then
    set_composer_text(state.composer_buf, "")
    M.select_fast_mode()
    return
  end
  local fast_value = text:match("^/fast%s+(%S+)$")
  if fast_value == "on" or fast_value == "off" then
    set_composer_text(state.composer_buf, "")
    M.configure_fast_mode(fast_value == "on")
    return
  elseif fast_value then
    set_composer_text(state.composer_buf, "")
    notifications.warn("Use /fast on or /fast off", "Harness")
    return
  end
  set_composer_text(state.composer_buf, "")
  if state.busy then
    if state.active_wait and not selected_agent_run(state)
      and state.capability and state.capability.native_steer
    then
      submit_immediate(state, text, false)
      return
    end
    state.queue[#state.queue + 1] = text
    M.refresh_winbar()
    return
  end
  begin_request(text)
end

local function remove_pending_steer(state, target)
  for index, entry in ipairs(state.pending_steer or {}) do
    if entry == target then
      table.remove(state.pending_steer, index)
      return
    end
  end
end

submit_immediate = function(state, text, notify_success)
  set_composer_text(state.composer_buf, "")
  local pending = { text = text }
  state.pending_steer = state.pending_steer or {}
  state.pending_steer[#state.pending_steer + 1] = pending
  M.refresh_winbar()
  local selected_run = selected_agent_run(state)
  local target = selected_run and selected_run.provider_thread_id and selected_run.active_turn_id and {
    thread_id = selected_run.provider_thread_id,
    turn_id = selected_run.active_turn_id,
  } or nil
  client.request("turn.steer", { text = text, target = target }, function(_, request_error)
    if request_error then
      remove_pending_steer(state, pending)
      state.queue[#state.queue + 1] = text
      notifications.warn("Steering missed the active turn; queued as a follow-up", "Harness")
    elseif notify_success then
      notifications.info("Steered the active turn", "Harness")
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
    notifications.warn("Harness has no active turn to steer", "Harness")
    return
  end
  if not (state.capability and state.capability.native_steer) then
    notifications.warn("The current backend does not support active-turn steering", "Harness")
    return
  end
  prompt_history.record(text)
  submit_immediate(state, text, true)
end

function M.edit_last_queued()
  local state = harness_state()
  local queued = table.remove(state.queue)
  if not queued then
    notifications.warn("No queued prompt to edit", "Harness")
    return
  end
  local draft = composer_text(state.composer_buf)
  if draft ~= "" then state.queue[#state.queue + 1] = draft end
  set_composer_text(state.composer_buf, queued)
  if state.composer_win and vim.api.nvim_win_is_valid(state.composer_win) then
    vim.api.nvim_set_current_win(state.composer_win)
  end
  M.refresh_winbar()
end

---@param delta integer
function M.jump_prompt(delta)
  local state = harness_state()
  if #state.prompt_line == 0 then return end
  if state.transcript_win and vim.api.nvim_win_is_valid(state.transcript_win) then
    local current_line = vim.api.nvim_win_get_cursor(state.transcript_win)[1]
    local target = delta < 0 and state.prompt_line[1] or state.prompt_line[#state.prompt_line]
    if delta < 0 then
      for index = #state.prompt_line, 1, -1 do
        if state.prompt_line[index] < current_line then
          target = state.prompt_line[index]
          break
        end
      end
    else
      for _, line in ipairs(state.prompt_line) do
        if line > current_line then
          target = line
          break
        end
      end
    end
    vim.api.nvim_set_current_win(state.transcript_win)
    vim.api.nvim_win_set_cursor(state.transcript_win, { target, 0 })
  end
end

function M.toggle_activity()
  local state = harness_state()
  if not (state.transcript_win and vim.api.nvim_win_is_valid(state.transcript_win)) then return end
  vim.api.nvim_set_current_win(state.transcript_win)
  local cursor_line = vim.api.nvim_win_get_cursor(state.transcript_win)[1]
  local row = state.render_rows and state.render_rows[cursor_line] or nil
  local expand_key = row and row.expand_key or nil
  if not expand_key then
    notifications.warn("Only completed thoughts, tools, and changes can expand", "Harness")
    return
  end
  local expanded = state.activity_expanded[expand_key]
  if expanded == nil then expanded = row.expanded_by_default == true end
  state.activity_expanded[expand_key] = not expanded
  M.render()
end

---@param direction integer
function M.change_effort(direction)
  local state = harness_state()
  local current = state.session and state.session.effort or config.options.harness.effort
  local index = 3
  for candidate_index, candidate in ipairs(effort_list) do if candidate == current then index = candidate_index end end
  index = math.max(1, math.min(#effort_list, index + direction))
  M.configure({ effort = effort_list[index] })
end

function M.select_effort()
  if harness_state().capability.effort_selection ~= true then
    notifications.warn("The current backend does not support reasoning effort selection", "Harness")
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
  open_choice_picker(harness_state(), "Select reasoning effort", "Applied at the next safe turn boundary.", options, function(effort)
    M.configure({ effort = effort })
  end)
end

---@param enabled boolean
function M.configure_fast_mode(enabled)
  local state = harness_state()
  if state.capability.fast_mode ~= true then
    notifications.warn("The current backend does not support fast mode", "Harness")
    return
  end
  M.configure({ fast_mode = enabled })
end

function M.select_fast_mode()
  local state = harness_state()
  local enabled = state.session and state.session.fast_mode == true
  local next_enabled = not enabled
  open_choice_picker(state, "Codex fast mode", "Choose whether Codex prioritizes faster inference.", {
    {
      label = next_enabled and "Enable fast mode" or "Disable fast mode",
      detail = "Apply this setting at the next safe turn boundary.",
      value = next_enabled,
    },
    { label = "Cancel", detail = "Keep the current setting.", value = nil },
  }, function(choice) if choice ~= nil then M.configure_fast_mode(choice) end end)
end

function M.select_model()
  local state = harness_state()
  if state.capability.model_selection ~= true then
    notifications.warn("The current backend does not support model selection", "Harness")
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
            title = "Harness model",
            subtitle = "Enter a model identifier exposed by the current backend.",
            option_list = { { label = "Model", value = current, input_kind = "other" } },
            allow_input = true,
            input_height = 3,
            footer = "C-s apply  go options  q close",
          },
        },
        on_confirm = function(result) M.configure({ model = result.text }) end,
      })
      return
    end
    local state = harness_state()
    model_picker.open({
      host = picker_host(state),
      model_list = model_list,
      current_model = state.session and (state.session.resolved_model or state.session.model),
      on_confirm = M.configure,
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
  if not (state.capability.catalog and state.capability.catalog.mcp) then
    notifications.warn("The current backend does not advertise MCP management", "Harness MCP")
    return
  end
  provider_picker.open_mcp({
    host = picker_host(state),
    on_mutation_start = function(definition, enabled)
      local current = harness_state()
      if current.busy and not current.capability.catalog.live_mcp_mutation then
        current.mcp_restart = {
          name = definition.name,
          enabled = enabled,
          mutation_finished = false,
          turn_cancelled = false,
        }
      end
    end,
    on_mutation = function(result)
      local current = harness_state()
      append_session_status(("MCP server %s %s"):format(
        result.name or "server",
        result.enabled and "enabled" or "disabled"
      ))
      if result.restart_required then
        current.mcp_restart = current.mcp_restart or {
          name = result.name,
          enabled = result.enabled,
          turn_cancelled = false,
        }
        current.mcp_restart.mutation_finished = true
        maybe_resume_mcp_restart()
      else
        clear_mcp_restart(current)
      end
    end,
    on_mutation_error = function(mutation_error)
      local current = harness_state()
      if not current.mcp_restart then return end
      current.mcp_restart.error = mutation_error
      current.mcp_restart.mutation_finished = true
      maybe_resume_mcp_restart()
    end,
  })
end

function M.select_backend()
  local state = harness_state()
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
  open_choice_picker(state, "Select Harness backend", "Switch the CLI used for this workspace.", option_list,
    function(backend)
      if backend == current then return end
      if state.busy then
        state.pending_backend = backend
        M.refresh_winbar()
        notifications.info("Harness backend will switch after the active turn", "Harness backend")
        return
      end
      require("diff_review.views.harness").switch_backend(backend)
    end)
end

function M.resolve_runtime_model()
  local state = harness_state()
  local backend = state.session and state.session.backend
  client.request("backend.models", {}, function(model_list, request_error)
    if request_error then notifications.error("Failed to resolve Harness model: " .. request_error, "Harness") end
    if request_error then return end
    state.model_backend = backend
    state.model_list = vim.deepcopy(model_list or {})
  end)
end

---@param mode string
function M.set_mode(mode)
  local state = harness_state()
  mode = mode:lower()
  local function apply_mode()
    if state.busy then
      if state.mode_restart_requested then
        notifications.info("Harness is already restarting in the requested execution mode", "Harness")
        return
      end
      if selected_agent_run(state) then
        state.pending_mode = mode
        M.refresh_winbar()
        notifications.info("Harness execution mode will change after the active child turn", "Harness")
        return
      end
      state.pending_mode = mode
      state.mode_restart_requested = true
      M.refresh_winbar()
      client.request("turn.restart", { mode = mode }, function(result, request_error)
        if request_error or not (result and result.restart_requested) then
          state.mode_restart_requested = false
          state.pending_mode = nil
          report_configuration_error(
            "Failed to restart Harness in " .. mode .. " mode: " .. (request_error or "request rejected")
          )
          M.refresh_winbar()
        end
      end)
      return
    end
    client.request("session.execution_mode", { mode = mode }, function(result, request_error)
      if request_error then
        report_configuration_error("Failed to change execution mode: " .. request_error)
        M.refresh_winbar()
        return
      end
      state.session = result or state.session
      local label = mode == "yolo" and "YOLO" or (mode:sub(1, 1):upper() .. mode:sub(2))
      append_session_status("Execution mode changed to " .. label)
      M.refresh_winbar()
      vim.schedule(M.drain)
    end)
  end

  if mode ~= "read" and state.no_checkpoint and config.options.harness.non_git_write_confirm then
    local label = mode == "yolo" and "YOLO" or (mode:sub(1, 1):upper() .. mode:sub(2))
    open_choice_picker(state, "Enable " .. label .. " without checkpoints?", "This is not a Git worktree. Harness cannot diff or roll back changes.", {
      { label = "Cancel", detail = "Keep the current execution mode.", value = false },
      { label = "Enable " .. label, detail = "Proceed without Harness rollback support.", value = true },
    }, function(choice) if choice then apply_mode() end end)
    return
  end
  apply_mode()
end

function M.toggle_mode()
  local state = harness_state()
  local mode_list = (state.capability and state.capability.execution_mode_list) or {}
  if #mode_list == 0 then mode_list = { "read", "write", "full", "yolo" } end
  local current_mode = state.pending_mode or (state.session and state.session.execution_mode) or "read"
  local current_index = 0
  for index, candidate in ipairs(mode_list) do
    if candidate == current_mode then
      current_index = index
      break
    end
  end
  M.set_mode(mode_list[(current_index % #mode_list) + 1])
end

function M.select_mode()
  local state = harness_state()
  local current_mode = state.pending_mode or (state.session and state.session.execution_mode) or "read"
  local detail_list = {
    read = "Allow reads and network access while denying local writes.",
    write = "Allow workspace writes within the current repository.",
    full = "Allow machine-wide writes with approval prompts.",
    yolo = "Allow machine-wide writes without approval prompts.",
  }
  local option_list = vim.tbl_map(function(mode)
    local label = mode == "yolo" and "YOLO" or (mode:sub(1, 1):upper() .. mode:sub(2))
    if mode == current_mode then label = label .. " (current)" end
    return { label = label, detail = detail_list[mode], value = mode }
  end, { "read", "write", "full", "yolo" })
  open_choice_picker(state, "Select execution mode", "Changing mode during a main turn restarts it automatically.", option_list,
    M.set_mode)
end

---@param next_config table
---@param validate_selection? boolean
function M.configure(next_config, validate_selection)
  local state = harness_state()
  if state.busy then
    state.pending_config = vim.tbl_extend("force", state.pending_config or {}, next_config)
    state.pending_config_validate = state.pending_config_validate == true or validate_selection == true
    M.refresh_winbar()
    notifications.info("Harness configuration will apply at the next safe boundary", "Harness")
    return
  end
  local request_config = vim.tbl_extend("force", {}, next_config, { validate = validate_selection == true })
  client.request("session.configure", request_config, function(result, request_error)
    if request_error then report_configuration_error(request_error) return end
    state.session = result
    if next_config.model then append_session_status("Model changed to " .. next_config.model) end
    if next_config.effort then append_session_status("Reasoning effort changed to " .. next_config.effort) end
    M.refresh_winbar()
    if next_config.model and not result.resolved_model then M.resolve_runtime_model() end
    vim.schedule(M.drain)
  end)
end

local function close()
  local state = harness_state()
  timeline_status.stop(state)
  local tab_count = vim.fn.tabpagenr("$")
  if tab_count > 1 then vim.cmd("tabclose") else vim.cmd("enew") end
  state.transcript_win = nil
  state.composer_win = nil
end

---@param result table
---@param interaction_mode? "reconcile"
function M.activate_snapshot(result, interaction_mode)
  local state = harness_state()
  snapshot.apply(state, result, interaction_mode)
  state.queue = {}
  M.render(true)
  M.resolve_runtime_model()
  if state.active_elicitation and state.active_elicitation.elicitation then
    state.presented_question_key = nil
    vim.schedule(M.present_plan_question)
  end
  if #state.approval > 0 then vim.schedule(M.present_approval) end
  if state.goal and state.goal.state == "active" then vim.schedule(M.drain) end
end

---@return DiffReviewViewCommandSet
function M.command_set()
  local set = command_set.new()
  command_set.register(set, "submit", M.submit)
  command_set.register(set, "steer", M.steer_submit)
  command_set.register(set, "cancel", M.cancel_turn)
  command_set.register(set, "edit_queued", M.edit_last_queued)
  command_set.register(set, "toggle_mode", M.toggle_mode)
  command_set.register(set, "previous_prompt", function() M.jump_prompt(-1) end)
  command_set.register(set, "next_prompt", function() M.jump_prompt(1) end)
  command_set.register(set, "toggle_activity", M.toggle_activity)
  command_set.register(set, "open_artifact", M.open_artifact_picker)
  command_set.register(set, "agent", M.open_agent_picker)
  command_set.register(set, "sessions", M.open_session_picker)
  if harness_state().capability.native_fork == true then
    command_set.register(set, "open_timeline", M.open_timeline_entry)
  end
  command_set.register(set, "reopen_question", M.reopen_question)
  command_set.register(set, "model", M.select_model)
  command_set.register(set, "effort_down", function() M.change_effort(-1) end)
  command_set.register(set, "effort_up", function() M.change_effort(1) end)
  command_set.register(set, "close", close)
  command_set.register(set, "help", function() keymaps.show_view_help("harness", set, "Harness") end)
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
  state.command_set = M.command_set()
  M.attach_transcript(state.transcript_buf)
  local composer_command_set = M.command_set()
  command_set.unregister(composer_command_set, "close")
  command_set.unregister(composer_command_set, "open_timeline")
  command_set.register(composer_command_set, "history_previous", prompt_history.previous)
  command_set.register(composer_command_set, "history_next", prompt_history.next)
  keymaps.setup_view_keymaps(state.composer_buf, "harness", composer_command_set)
  prompt_history.attach(state.composer_buf)
  if state.unsubscribe then state.unsubscribe() end
  state.unsubscribe = client.subscribe(function(event, payload, event_session_id)
    if state.session and event_session_id and state.session.id ~= event_session_id then return end
    local active_state = session.harness
    session.activate_harness(state)
    on_event(event, payload)
    if active_state ~= state then session.activate_harness(active_state) end
  end)
end

---@param observer? fun(lines: string[])
function M._set_render_observer_for_test(observer)
  render_observer_for_test = observer
end

return M
