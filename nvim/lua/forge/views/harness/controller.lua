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
local activity = require("forge.views.harness.activity")
local cleanup_progress = require("forge.views.harness.cleanup_progress")

local queue_namespace = vim.api.nvim_create_namespace("ForgeHarnessQueue")
local render_observer_for_test = nil
local begin_request
local submit_immediate
local configure_now
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

---List the model, thinking, and speed changes the host has not applied yet, in the order it
---applies them, each marked `waiting` when existing work holds it back. Changes apply in the order
---they were sent. One sent while a turn runs waits for that turn to stop and restart, and so does
---every change sent after it. A queued `/model` entry applies only at its hand-off, after every
---change already sent. An idle change applies within the host's own validation and is listed
---without `waiting`. A change sent for another session or to a host that has since been replaced
---no longer applies here.
---@param state table
---@return { change: table, waiting: boolean }[]
local function pending_configuration_list(state)
  local pending_list = {}
  for _, request in ipairs(state.configuration_requests or {}) do
    if request.session_id == (state.session and state.session.id)
      and request.generation == client.host_get_generation() then
      pending_list[#pending_list + 1] = { change = request.change, waiting = request.waiting == true }
    end
  end
  for _, group in ipairs({ state.pending_handoff, state.pending_submit, state.queue }) do
    for _, entry in ipairs(group or {}) do
      if type(entry.config) == "table" then pending_list[#pending_list + 1] = { change = entry.config, waiting = true } end
    end
  end
  return pending_list
end

---Get the model, thinking, and speed the session will use once every pending change applies, and
---whether any of them waits for existing work. A model change without a thinking level takes that
---model's default, as the host applies it.
---@param state table
---@return { model: string, effort: string, service_tier: string, waiting: boolean }
local function selected_settings(state)
  local selected = {
    model = applied_setting(state, "model"),
    effort = applied_setting(state, "effort"),
    service_tier = applied_setting(state, "service_tier"),
    waiting = false,
  }
  local catalog = state.session and state.model_provider == state.session.provider and state.model_list or nil
  for _, pending in ipairs(pending_configuration_list(state)) do
    local change = pending.change
    if type(change.model) == "string" and change.model ~= selected.model then
      selected.model = change.model
      local model = require("forge.views.harness.model_options").find(catalog, change.model)
      selected.effort = model and model.default_reasoning or "default"
    end
    if type(change.effort) == "string" then selected.effort = change.effort end
    if type(change.service_tier) == "string" then selected.service_tier = change.service_tier end
    selected.waiting = selected.waiting or pending.waiting
  end
  return selected
end

---@param state table
---@param field "model"|"effort"|"service_tier"
---@return string
local function selected_setting(state, field)
  return selected_settings(state)[field]
end

---Get the model catalog loaded for the session's own provider, or nil while none is loaded.
---@param state table
---@return table[]?
local function loaded_model_catalog(state)
  if state.session and state.model_provider == state.session.provider and type(state.model_list) == "table" then
    return state.model_list
  end
end

---Parse model configuration separately from its optional queued prompt. With a catalog, the model
---is resolved by id or display label (see `model_options.resolve_command`); without one, the first
---word is taken as the model id, the next as its thinking level, and the host validates both.
---@param text string
---@param models? table[] The catalog to resolve against; defaults to the session's loaded catalog.
---@return { model: string, effort: string? }? next_config
---@return string? prompt
---@return string? failure
local function model_command(text, models)
  if not text:match("^/model%s") then return nil end
  local arguments = text:match("^/model%s+(.*)$")
  if not arguments or not arguments:match("%S") then return nil end
  models = models or loaded_model_catalog(session.harness)
  if models then return require("forge.views.harness.model_options").resolve_command(models, arguments, true) end
  local model, remainder = arguments:match("^(%S+)%s*(.*)$")
  local effort, prompt = remainder:match("^(%S+)%s*(.*)$")
  return { model = model, effort = effort }, prompt or ""
end

---@return table
local function harness_state() return session.harness end

---@param message string
local function report_configuration_error(message)
  notifications.error("Configuration rejected: " .. message, "ForgeHarness")
end

---Cancel the root exchange independently of the selected child timeline.
---@param state table
local function cancel_exchange(state)
  local session_id = state.session and state.session.id
  if not session_id or cleanup_progress.pending(state) then return end
  local epoch = state.runtime_epoch
  local operation_id = require("forge.command").identity()
  if state.input then state.input.pause_operation_id = operation_id end
  cleanup_progress.begin(state, operation_id)
  client.request({ session_id = session_id, method = "session.turn.cancel",
    params = { operation_id = operation_id, restore_prompt_if_no_output = true } }, function(_, failure, detail)
    if not state.session or state.session.id ~= session_id then return end
    if not cleanup_progress.respond(state, operation_id, epoch) then return end
    if failure then
      if detail and detail.code == "execution_collection_timeout" then cleanup_progress.timeout(state, operation_id) end
      notifications.error(failure, "Harness cancellation")
    else cleanup_progress.settled(state, operation_id) end
    if harness_state() == state then M.render() end
  end)
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
        picker.close(true, "user")
        if session.harness == state then cancel_exchange(state) end
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

---@class ForgeHarnessChoicePickerOptions
---@field empty_text? string
---@field selected_index? integer
---@field wrap_selection? boolean
---@field assign_choice_keys? boolean
---@field height_ratio? number
---@field column_headers? string[]
---@field flexible_column? integer
---@field preview? ForgeHarnessExchangePreview

---@param options? ForgeHarnessChoicePickerOptions
local function open_choice_picker(state, title, subtitle, option_list, callback, options)
  options = options or {}
  local preview = options.preview
  if options.assign_choice_keys ~= false then
    for index, option in ipairs(option_list) do
      option.key = option.key or config.options.picker.choice_keys[index]
    end
  end
  picker.open({
    host = picker_host(state),
    height_ratio = options.height_ratio,
    page_list = {
      {
        id = title,
        title = title,
        subtitle = subtitle,
        column_headers = options.column_headers or { "Choice", "Details" },
        flexible_column = options.flexible_column,
        option_list = option_list,
        empty_text = options.empty_text,
        selected_index = options.selected_index,
        wrap_selection = options.wrap_selection,
        footer = "↑↓ select  Enter confirm  q close",
      },
    },
    on_change = preview and function(context)
      local interaction = context.option and context.option.value
      preview.show(interaction and interaction.id, context.preview_height)
    end,
    on_dispose = preview and preview.close,
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
    .. (current_task and (current_task.kind == "plan" or current_task.kind == "execute")
      and current_task.auto_accept_plan and "pa" or "")
  local yolo = abbreviation == "rxwx" and approval == "allow"
    and (active_session.access or {}).sandbox == false
  local configured_model = active_session.model or config.options.harness.model
  local confirmed_model = type(active_session.resolved_model) == "string" and active_session.resolved_model or nil
  local model_catalog = state.model_provider == active_session.provider and state.model_list or nil
  local model_options = require("forge.views.harness.model_options")
  local displayed_model = confirmed_model or configured_model
  local saved_label = displayed_model == active_session.model and type(active_session.model_label) == "string"
    and active_session.model_label or nil
  local model = model_options.label(model_catalog, displayed_model, saved_label)
  local selected = selected_settings(state)
  if selected.model ~= configured_model then model = model_options.label(model_catalog, selected.model) end
  -- The thinking field names the level the provider uses, the name the model picker gives it.
  local thinking_model = selected.model == "default" and configured_model == "default" and confirmed_model or selected.model
  local effort = model_options.thinking(model_options.find(model_catalog, thinking_model), selected.effort)
  local tier = selected.service_tier
  local speed_icon = ({ fast = "󱐌", ultrafast = "󱐋" })[tier]
  -- Selected values that differ from the applied ones and wait for existing work carry the marker;
  -- an idle change shows its value at once, while the host validates and applies it.
  local waiting = selected.waiting and (selected.model ~= configured_model
    or selected.effort ~= applied_setting(state, "effort")
    or tier ~= applied_setting(state, "service_tier"))
  local model_text = ("%s %s%s"):format(model, effort, waiting and "*" or "")
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
  local selected_run = selected_agent_run(state)
  local child_selection = selected_run and selected_run.selection
  if type(child_selection) == "table" and type(child_selection.provider) == "string" then
    -- A child's view names the child's own provider, model, and thinking level, which its
    -- profile may choose independently of the root session's.
    local child_catalog = state.model_provider == child_selection.provider and state.model_list or nil
    local child_tier = child_selection.service_tier
    local child_speed_icon = ({ fast = "󱐌", ultrafast = "󱐋" })[child_tier]
    if child_speed_icon then
      append_status(child_speed_icon, child_tier == "ultrafast" and "ForgeHarnessUltrafast" or "ForgeStatusLabel")
    end
    local child_model = child_selection.model or "default"
    local child_model_text = ("%s · %s %s"):format(child_selection.provider,
      model_options.label(child_catalog, child_model),
      model_options.thinking(model_options.find(child_catalog, child_model), child_selection.reasoning_effort))
    if child_speed_icon then
      segment_list[#segment_list + 1] = { text = " " .. child_model_text, group = "ForgeStatusLabel" }
    else
      append_status(child_model_text, "ForgeStatusLabel")
    end
  elseif speed_icon then
    append_status(speed_icon, tier == "ultrafast" and "ForgeHarnessUltrafast" or "ForgeStatusLabel")
    segment_list[#segment_list + 1] = { text = " " .. model_text, group = "ForgeStatusLabel" }
  else
    append_status(model_text, "ForgeStatusLabel")
  end
  if selected_run then
    append_status(require("forge.render.harness.agent_summary").label(selected_run), "ForgeStatusLabel")
  end
  if state.connection_error then
    append_status(state.connection_error, "ForgeHarnessGoal")
  elseif state.sync_error then
    append_status("State unavailable: " .. state.sync_error, "ForgeHarnessGoal")
  elseif activity.unavailable_reason(state) then
    append_status("Session unavailable: " .. activity.unavailable_reason(state), "ForgeHarnessGoal")
  elseif current_task then
    append_status(task_control.label(current_task, activity.turn_awaited(state)), "ForgeHarnessGoal")
  end
  if state.configuration_error then
    append_status("Settings rejected: " .. state.configuration_error:gsub("%s+", " "), "ForgeHarnessToolFailure")
  end
  return segment_list
end

--- Write the winbar of every window that shows `state`'s transcript.
---@param state table
local function write_winbar(state)
  if not state.command_set then return end
  local active_session = not state.initial_snapshot_pending and state.session or nil
  tabline.refresh()
  local right_status = context_status.segment(active_session and active_session.context_usage) or {}
  local artifact_count = active_session and #(state.artifact or {}) or 0
  if artifact_count > 0 then
    if #right_status > 0 then table.insert(right_status, 1, keymaps.winbar_separator) end
    table.insert(right_status, 1, {
      text = ("%d %s"):format(artifact_count, artifact_count == 1 and "artefact" or "artefacts"),
      group = "ForgeStatusLabel",
    })
  end
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
end

function M.refresh_winbar()
  local state = harness_state()
  if not state.command_set then return end
  write_winbar(state)
  render_status_hint(state)
end

---Report whether the retry key sends a queued `entry` again.
---
---The key retries a message the host marked failed or uncertain and a draft the host rejected. A
---draft still being sent is not retried, because its submission is in flight.
---@param entry table
---@return boolean
function M.retries(entry)
  if entry.local_only then return entry.draft_state ~= "pending" end
  return entry.delivery == "failed" or entry.delivery == "unknown"
end

---Get the hint for the queued follow-up heading while nothing sends the queue's messages.
---
---An interrupt or a failed message pauses the queue, and nothing sends its messages until the user
---resumes it, so every frame says so, also while the stop, a compaction, or a rollback runs. A
---failed message also holds the follow-ups behind it after a newer prompt resumed delivery. The
---heading names the retry key, which retries the first failed message and otherwise resumes.
---@param state table
---@return string? hint Absent while delivery runs; empty when the retry key is unbound.
function M.paused_queue_hint(state)
  local queue = state.queue or {}
  local retry_index, held_by_failure
  for index, entry in ipairs(queue) do
    if M.retries(entry) then
      retry_index = retry_index or index
    elseif retry_index then
      held_by_failure = true
      break
    end
  end
  local suspended = state.input and state.input.suspended and #queue > 0
  if not (suspended or held_by_failure) then return nil end
  local key = require("forge.shared.keymaps").view_keys_for("harness", "retry_input")[1]
  if not key then return "" end
  return key .. (retry_index and " retry" or " resume")
end

---@return integer?
local function working_seconds()
  local state = harness_state()
  if not activity.busy(state) then return nil end
  return math.floor(((state.status or {}).duration_ms or 0) / 1000)
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
      questions = state.questions,
    }
    if state.presentation.set_staging then
      -- A claimed follow-up keeps its row until the transcript shows its prompt, where the host
      -- drops the staged copy in the same publication; hiding it at the claim closed the split
      -- and reopened it one frame later.
      local follow_ups = vim.list_extend(vim.list_slice(state.pending_handoff or {}), state.queue or {})
      state.presentation.set_staging(queue_editor.staging(state, composer_text(state.composer_buf), follow_ups),
        queue_editor.staging(state, nil, state.pending_steer or {}), M.paused_queue_hint(state))
    end
    if state.transcript_win and vim.api.nvim_win_is_valid(state.transcript_win) then
      state.presentation.status_view:render(status_context, M.command_set())
    end
  end
end

local function render()
  local state = harness_state()
  if state.initial_snapshot_pending then M.refresh_winbar() return end
  render_status_hint(state)
  if state.host_error then M.refresh_winbar() return end
  if state.switching_provider then M.refresh_winbar() return end
  if not (state.transcript_buf and vim.api.nvim_buf_is_valid(state.transcript_buf)
      and state.composer_buf and vim.api.nvim_buf_is_valid(state.composer_buf)) then return end
  if not (state.session and state.session.id) then
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
  -- The status hint was composed above, so this path writes the winbar alone.
  if owner and not owner.closed then owner.sync() write_winbar(state) return end
  if not (state.transcript_win and vim.api.nvim_win_is_valid(state.transcript_win)) then
    state.transcript_win = vim.fn.win_findbuf(state.transcript_buf)[1]
    if not state.transcript_win then return end
  end
  local opening_session_id, opening_generation = state.session.id, client.host_get_generation()
  local function owns_presentation()
    return not state.switching_provider and state.session and state.session.id == opening_session_id
      and client.host_get_generation() == opening_generation
  end
  state.presentation = require("forge.views.harness.presentation").open({
    startup_trace = state.startup_trace,
    capability = function() return state.capability end,
    session_id = state.session.id, transcript_buffer = state.transcript_buf, composer_buffer = state.composer_buf,
    -- A presentation rebuilt behind the view binds the composer once the retired one releases it.
    transcript_window = state.transcript_win, composer_window = not state.pending_rebuild and state.composer_win or nil,
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

function M.render()
  local started = perf.timing_enabled() and perf.now() or nil
  local state = harness_state()
  if state.transcript_buf and state.session then perf.watch_view(state.transcript_buf, state.session.id) end
  render()
  if started then
    local fields = { elapsed_ms = perf.elapsed_ms(started), session_id = state.session and state.session.id }
    if perf.enabled("harness") then perf.event("harness", "ui.render", fields)
    else perf.diagnostic("ui.render", fields) end
  end
end

local function schedule_render()
  local state = harness_state()
  -- Transport events normally arrive on the main loop already. Coalesce their
  -- presentation work too, so a burst does not render between every queue delta.
  if state.render_pending then return end
  state.render_pending = true
  vim.schedule(function()
    state.render_pending = false
    M.with_state(state, M.render)
  end)
end

local function remove_approval(state, approval_id)
  for index, approval in ipairs(state.approval or {}) do
    if approval.request_id == approval_id then
      table.remove(state.approval, index)
      return true
    end
  end
  return false
end

---@param state table
local function reconcile_approval_presentation(state)
  if #(state.approval or {}) == 0 then M.forget_held_popup(state, "approval") end
  if state.dismissed_approval_id and not vim.iter(state.approval or {}):any(function(approval)
    return approval.request_id == state.dismissed_approval_id
  end) then state.dismissed_approval_id = nil end
  local presented_id = state.presented_approval_id
  if not presented_id then return end
  local still_pending = vim.iter(state.approval or {}):any(function(approval)
    return approval.request_id == presented_id
  end)
  if still_pending then return end
  require("forge.views.harness.approval").close()
  state.approval_open = false
  state.presented_approval_id = nil
end

---Delays before the first, second, and third retry of a failed `session.state.get`. A read that
---still fails after the third retry leaves its failure visible until a later read succeeds.
M.state_sync_retry_delays_ms = { 1000, 2000, 4000 }

---Answer every caller waiting on a state read of `target`, exactly once.
---@param target table
---@param result? table
---@param failure? string
function M.settle_state_sync(target, result, failure)
  local callbacks = target.state_sync_callback or {}
  target.state_sync_callback = nil
  for _, completed in ipairs(callbacks) do completed(result, failure) end
end

---Run `action` with `target` as the active Harness state and restore the previous selection, even
---when `action` raises. Callbacks that complete for a background tab use this so that code reading
---the active state acts on the tab that issued the request. An action that selects a session for
---the user, as a session switch completing in this scope does, leaves that session active.
---@param target table
---@param action fun()
function M.with_state(target, action)
  local active_state, selection = session.harness, session.harness_selection()
  session.activate_harness(target)
  local ok, failure = xpcall(action, debug.traceback)
  if session.harness_selection() ~= selection then session.activate_harness(session.selected_harness())
  elseif session.harness == target and active_state ~= target then session.activate_harness(active_state) end
  if not ok then error(failure, 0) end
end

---Retry a failed state read of `target` after the next delay in `state_sync_retry_delays_ms`.
---@param target table
---@param session_id string
function M.schedule_state_sync_retry(target, session_id)
  local retry = target.state_sync_retry
  if not retry or retry.session_id ~= session_id then
    retry = { session_id = session_id, attempt = 0 }
    target.state_sync_retry = retry
  end
  retry.attempt = retry.attempt + 1
  local delay = M.state_sync_retry_delays_ms[retry.attempt]
  if not delay then return end
  local attempt = retry.attempt
  vim.defer_fn(function()
    if target.state_sync_retry ~= retry or retry.attempt ~= attempt or target.host_error
      or not target.session or target.session.id ~= session_id then return end
    M.synchronize_state(nil, target)
  end, delay)
end

---@param callback? fun(result?: table, failure?: string)
---@param target_state? table
local function synchronize_state(callback, target_state)
  local synchronized_state = target_state or harness_state()
  local synchronized_session = synchronized_state.session and synchronized_state.session.id
  local synchronized_generation = client.host_get_generation()
  if callback then
    synchronized_state.state_sync_callback = synchronized_state.state_sync_callback or {}
    table.insert(synchronized_state.state_sync_callback, callback)
  end
  if synchronized_state.host_error then
    M.settle_state_sync(synchronized_state, nil, synchronized_state.host_error)
    M.render()
    return
  end
  -- One read is in flight per session and host. A read issued for an earlier session of this view
  -- does not satisfy the current one, so an in-place session switch supersedes it.
  local pending = synchronized_state.state_sync_pending
  if pending and pending.session_id == synchronized_session and pending.host_generation == synchronized_generation then
    return
  end
  local observation = { session_id = synchronized_session, host_generation = synchronized_generation }
  synchronized_state.state_sync_pending = observation
  local invalidation = synchronized_state.state_invalidation or 0
  client.request({ session_id = synchronized_session, method = "session.state.get", params = {
    publication_cursor = synchronized_state.publication_cursor,
  } }, function(result, request_error, error_detail)
    if synchronized_state.state_sync_pending ~= observation then return end
    synchronized_state.state_sync_pending = nil
    if client.host_get_generation() ~= synchronized_generation or not synchronized_state.session
      or synchronized_state.session.id ~= synchronized_session then
      -- The view now shows another session or host, and no read was issued for it since. Read
      -- the current one so its reconciliation still runs and waiting callers are answered.
      if synchronized_state.session and not synchronized_state.host_error then
        vim.schedule(function() synchronize_state(nil, synchronized_state) end)
      else
        M.settle_state_sync(synchronized_state, nil, "Harness session changed before its state arrived")
      end
      return
    end
    if result and result.session and result.session.id ~= synchronized_session then
      local failure = "Snapshot belongs to another conversation"
      diagnostic.set(synchronized_state, "sync_error", failure,
        { owner = synchronized_session, category = "protocol", severity = "error", recovery = "reconnect" })
      M.settle_state_sync(synchronized_state, nil, failure)
      if harness_state() == synchronized_state then M.render() end
      return
    end
    if result and result.session then snapshot.receive_publication(synchronized_state, result) end
    if result and invalidation ~= (synchronized_state.state_invalidation or 0) then
      vim.schedule(function() synchronize_state(nil, synchronized_state) end)
      return
    end
    if result and result.runtime_epoch == synchronized_state.runtime_epoch
      and (result.timeline_revision or 0) < (synchronized_state.timeline_revision or 0) then
      vim.schedule(function() synchronize_state(nil, synchronized_state) end)
      return
    end
    if result and result.runtime_epoch == synchronized_state.runtime_epoch
      and (result.snapshot_revision or 0) < (synchronized_state.snapshot_revision or 0) then
      vim.schedule(function() synchronize_state(nil, synchronized_state) end)
      return
    end
    if request_error or not result then
      local failure = request_error or "Harness broker returned an empty state snapshot"
      local code = error_detail and error_detail.code
      if code == "reconnect_invalid_cursor" then
        synchronized_state.publication_cursor, synchronized_state.publication_received, synchronized_state.publication_more = nil, nil, nil
      end
      diagnostic.set(synchronized_state, "sync_error", failure,
        { owner = synchronized_session, category = "snapshot", recovery = "retry",
          severity = (not request_error or error_detail and code ~= "not_admitted" and code ~= "outcome_unknown") and "error" or "info" })
      if harness_state() == synchronized_state then M.render() end
      M.schedule_state_sync_retry(synchronized_state, synchronized_session)
      M.settle_state_sync(synchronized_state, nil, failure)
      return
    end
    diagnostic.set(synchronized_state, "sync_error", nil)
    synchronized_state.state_sync_retry = nil
    local state = synchronized_state
    snapshot.apply(state, result)
    if state.input and (state.input.runtime_epoch ~= state.runtime_epoch
      or state.input.host_generation ~= client.host_get_generation()) then input_state.refresh(state) end
    if harness_state() ~= state then
      -- A background tab adopts the snapshot but leaves its presentation to its next activation.
      M.settle_state_sync(state, result, nil)
      return
    end
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
    if #state.approval > 0 then vim.schedule(function() M.present_approval(false, state) end) end
    M.render()
    if result.owner_publication then
      if result.owner_publication.more then return end
    end
    M.settle_state_sync(synchronized_state, result, nil)
  end)
end



---Refresh canonical session state without interpreting command receipts as snapshots.
M.synchronize_state = synchronize_state

local function on_event(event, payload)
  local state = harness_state()
  if event == "publication_confirmed" then
    if snapshot.confirm_publication(state, payload) and state.publication_more then
      vim.schedule(function() synchronize_state(nil, state) end)
    end
    return
  end
  -- Rollback discards the provider context that produced the retained usage figure.
  -- Snapshot adoption keeps prior usage whenever a snapshot omits it, so the stale
  -- figure must be dropped here or it would outlive the rollback.
  if event == "interaction_rolled_back" and state.session then state.session.context_usage = nil end
  -- A `document_changed` patch does not invalidate a state read: the read's `timeline_revision`
  -- comparison already rejects a snapshot older than the patch. Counting it would discard and
  -- reissue every read that overlaps a streamed patch.
  local changes_session = event ~= "provider_event" and event ~= "input_changed"
    and event ~= "activity_changed" and event ~= "command_completed" and event ~= "document_changed"
  if event == "provider_event" then
    changes_session = payload.kind == "runtime_resolved" or payload.kind == "context_usage"
      or payload.kind == "approval_requested" or payload.kind == "approval_resolved"
      or payload.kind == "approval_cancelled" or payload.kind == "agent_updated"
  end
  if changes_session then
    state.state_invalidation = (state.state_invalidation or 0) + 1
  end
  if event == "input_changed" then input_state.receive(state, payload) return end
  if event == "activity_changed" then
    local was_busy = activity.busy(state)
    if activity.receive(state, payload.activity, payload.runtime_epoch) then
      schedule_render()
      -- A question raised while the session read busy, such as the execution settings an
      -- automatic plan acceptance asks for while its command applies, could not open then.
      if was_busy and not activity.busy(state) and question_presentation.should_present(state) then
        vim.schedule(function() M.with_state(state, function() M.present_plan_question(false) end) end)
      end
    end
    return
  end
  if event == "host_stopped" then
    diagnostic.set(state, "host_error", payload.message,
      { owner = tostring(client.host_get_generation()), category = "host", severity = payload.expected and "info" or "error",
        recovery = "reconnect", clear_when = "new host generation" })
    state.execution_notice = payload.message .. ". Reopen Harness to reconnect."
    state.state_sync_pending, state.state_sync_retry = nil, nil
    M.settle_state_sync(state, nil, payload.message)
    state.approval, state.active_wait, state.dismissed_approval_id = {}, nil, nil
    state.ready = false
    if state.presentation and state.presentation.terminals then state.presentation.terminals.close() end
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
    -- Timeline revisions restart with each runtime epoch, so a patch published by an earlier
    -- epoch or host would raise the revision floor above every read of the current one. The
    -- host does not stamp these fields on `document_changed` yet; the check applies once it does.
    if payload.runtime_epoch ~= nil and payload.runtime_epoch ~= state.runtime_epoch then return end
    if payload.host_generation ~= nil and payload.host_generation ~= client.host_get_generation() then return end
    if payload.revision < (state.timeline_revision or 0) then return end
    state.timeline_revision = payload.revision
    if payload.status then
      state.status = payload.status
      require("forge.views.harness.input_state").status_changed(state)
    end
    if type(payload.checks_running) == "boolean" then
      local checks_finished = state.checks_running and not payload.checks_running
      state.checks_running = payload.checks_running
    end
    if payload.active_check_run ~= nil then
      if payload.active_check_run == vim.NIL then state.active_check_run = nil
      else state.active_check_run = payload.active_check_run end
    end
    local review_questions = state.active_elicitation and
      (state.active_elicitation.owner == "plan_review" or state.active_elicitation.owner == "plan_acceptance")
    if state.status.kind ~= "awaiting_input" and not (state.status.kind == "awaiting_plan_review" and review_questions) and state.plan_question_open then
      require("forge.views.harness.plan_question").close()
      state.plan_question_open = false
    end
    if state.status.kind == "awaiting_plan_review" and not review_questions then
      state.active_elicitation = nil
      question_presentation.reset(state)
    end
    -- A question the host raises on its own, such as automatic acceptance asking for execution
    -- settings, arrives only in the snapshot, so a status that awaits an answer the editor does
    -- not hold reads it.
    if state.status.kind == "awaiting_input" and not M.holds_question(state) then M.synchronize_state() end
    schedule_render()
  elseif event == "provider_event" then
    if require("forge.event_contract").route("provider", payload.kind) == "ignore" then return end
    if payload.kind == "cleanup_progress" then
      cleanup_progress.receive(state, payload.data)
      schedule_render()
    elseif payload.kind == "checks_configuration_error" then
      notifications.error((payload.data or {}).error or "Fix the project check configuration", "Harness checks")
    elseif payload.kind == "turn_started" then
      recap.clear(state)
    elseif payload.kind == "prompt_submission" then
      schedule_render()
    elseif payload.kind == "approval_requested" then
      local request = payload.data or payload
      state.approval = state.approval or {}
      if not vim.iter(state.approval):any(function(approval) return approval.request_id == request.request_id end) then
        state.approval[#state.approval + 1] = request
      end
      M.refresh_winbar()
      render_status_hint(state)
      vim.schedule(function() M.present_approval(false, state) end)
    elseif payload.kind == "approval_resolved" or payload.kind == "approval_cancelled" then
      local request = payload.data or payload
      remove_approval(state, request.request_id)
      reconcile_approval_presentation(state)
      M.refresh_winbar()
      render_status_hint(state)
      if #state.approval > 0 then vim.schedule(function() M.present_approval(false, state) end) end
    elseif payload.kind == "agent_updated" then
      if require("forge.views.harness.agent_catalog").replace(state, payload.data or payload) then
        require("forge.views.harness.agent_picker").refresh()
      end
    elseif payload.kind == "runtime_resolved" then
      local runtime = payload.data or {}
      if not state.session or runtime.session_id ~= state.session.id then return end
      local provider_catalog = require("forge.views.harness.provider_catalog")
      local resolved_model = type(runtime.model) == "string" and runtime.model or nil
      if not state.model_catalog_bound or state.session.resolved_model ~= resolved_model then
        state.model_catalog_bound = true
        provider_catalog.invalidate_models(state)
      end
      state.session.provider_label = runtime.provider
      state.session.resolved_model = resolved_model
      M.refresh_winbar()
      -- A resolved runtime has a live provider session, so its catalog can now name the model.
      -- A catalog failure leaves the id in the status line; the model picker reports such failures.
      provider_catalog.models(false, function(model_list)
        if model_list then M.refresh_winbar() end
      end)
    elseif payload.kind == "context_usage" then
      if state.session then state.session.context_usage = payload.data or payload end
      M.refresh_winbar()
    elseif payload.kind == "timeline_node_updated" then
      M.refresh_winbar()
    elseif payload.kind == "error" then
      if type(payload.text) == "string" then
        local severity = (payload.data or {}).severity
        if severity == "error" then notifications.error(payload.text, "ForgeHarness")
        else notifications.warn(payload.text, "ForgeHarness") end
      end
    else
      error("Unhandled Harness provider event: " .. tostring(payload.kind))
    end
  elseif event == "questions_changed" then
    local questions = require("forge.views.harness.questions")
    local _, previous_required = questions.count(state)
    questions.receive(state, payload)
    local _, required_count = questions.count(state)
    local current_win = vim.api.nvim_get_current_win()
    -- A new question opens by itself, like an approval, when focus is in this session's view.
    if required_count > previous_required and not require("forge.views.picker").is_open()
      and (current_win == state.transcript_win or current_win == state.composer_win)
      and not M.hold_while_typing(state, "questions", function() questions.open(state, picker_host(state)) end) then
      questions.open(state, picker_host(state))
    end
    M.refresh_winbar()
    synchronize_state()
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
    if snapshot.apply(state, payload) then M.refresh_winbar() schedule_render() end
  elseif event == "session_fork_ready" or event == "session_fork_failed" then
    if not snapshot.apply(state, payload.snapshot) then return end
    M.refresh_winbar()
    if event == "session_fork_failed" then
      local provider_fork_state = state.session and state.session.provider_fork_state or {}
      notifications.error(provider_fork_state.message or "Provider fork preparation failed", "Harness fork")
    end
  elseif event == "session_configured" or event == "session_changed" then
    if snapshot.apply(state, payload.snapshot) then
      if state.input and state.input.runtime_epoch ~= state.runtime_epoch then input_state.refresh(state) end
      if state.model_catalog_bound and state.model_list_provisional then
        require("forge.views.harness.provider_catalog").models(false, function() end)
      end
      M.refresh_winbar()
      schedule_render()
    end
  elseif event == "agent_updated" then
    if require("forge.views.harness.agent_catalog").replace(state, payload) then
      require("forge.views.harness.agent_picker").refresh()
      schedule_render()
    end
  elseif event == "state_invalidated" then
    synchronize_state()
  else
    error("Unhandled Harness session event: " .. tostring(event))
  end
end

-- A popup that takes focus while the user types receives the next letters and Enter as its
-- decisions; one such run saved an "always allow" rule after the user paused for a second, so no
-- key timing decides. An approval, plan question, or question never opens by itself while the
-- editor is in Insert, Replace, or Command-line mode in any buffer. It waits in this list, as if
-- the user had dismissed it, and one global hook opens the held popups in order when that mode
-- ends, provided the session's view is in the current tab; otherwise they wait for the reopen key.
-- Each entry keeps the window and mode the request interrupted, and a held popup returns there
-- when it is answered or closed, so a user who was typing in the composer continues typing.
---@type {state: table, kind: "approval"|"plan_question"|"questions", present: fun(), origin: ForgePopupOrigin}[]
local held_popup_list = {}
local held_popup_order = { approval = 1, plan_question = 2, questions = 3 }
local held_popup_label = { approval = "Approval", plan_question = "Question", questions = "Question" }

---Identify the notice that announces a held popup, so it leaves when the popup opens or its
---request ends instead of at the notifier's timeout.
---@param entry {state: table, kind: string}
---@return string
local function held_notice_id(entry)
  return ("forge-harness-held-%s-%s"):format(entry.kind, tostring(entry.state))
end

---@return boolean typing Whether the editor is in a mode that takes typed text.
function M.typing_mode()
  local mode = vim.api.nvim_get_mode().mode:sub(1, 1)
  return mode == "i" or mode == "R" or mode == "c"
end

---@param entry {state: table}
---@return boolean
local function held_view_in_current_tab(entry)
  local window = entry.state.transcript_win
  return window ~= nil and vim.api.nvim_win_is_valid(window)
    and vim.api.nvim_win_get_tabpage(window) == vim.api.nvim_get_current_tabpage()
end

---Open the first held popup whose view is in the current tab, and the next ones as each closes.
function M.release_held_popups()
  if #held_popup_list == 0 or M.typing_mode() or picker.is_open() then return end
  -- Popups held for a view in another tab wait for the reopen key.
  held_popup_list = vim.tbl_filter(held_view_in_current_tab, held_popup_list)
  local entry = table.remove(held_popup_list, 1)
  if not entry then return end
  notifications.withdraw(held_notice_id(entry))
  picker.present_from(entry.origin, entry.present)
  if #held_popup_list > 0 then
    vim.schedule(function() picker.when_idle(M.release_held_popups) end)
  end
end

vim.api.nvim_create_autocmd({ "InsertLeave", "CmdlineLeave" }, {
  group = vim.api.nvim_create_augroup("ForgeHarnessHeldPopup", { clear = true }),
  callback = function() if #held_popup_list > 0 then vim.schedule(M.release_held_popups) end end,
  desc = "Open Harness popups held while the user typed",
})

---Hold `present` when the user is typing; the caller then leaves the request pending.
---@param state table
---@param kind "approval"|"plan_question"|"questions"
---@param present fun()
---@return boolean held
function M.hold_while_typing(state, kind, present)
  if not M.typing_mode() then return false end
  for _, entry in ipairs(held_popup_list) do
    if entry.state == state and entry.kind == kind then entry.present = present return true end
  end
  local entry = { state = state, kind = kind, present = present,
    origin = { win = vim.api.nvim_get_current_win(), mode = vim.fn.mode(1) } }
  held_popup_list[#held_popup_list + 1] = entry
  table.sort(held_popup_list, function(left, right) return held_popup_order[left.kind] < held_popup_order[right.kind] end)
  local key = keymaps.view_keys_for("harness", "reopen_question")[1]
  notifications.info(held_popup_label[kind] .. " waiting" .. (key and (" · " .. key) or ""), "ForgeHarness",
    held_notice_id(entry))
  return true
end

---Forget the `kind` popup held for `state` and withdraw its notice, because its request was
---resolved or cancelled while the user typed.
---@param state table
---@param kind "approval"|"plan_question"|"questions"
function M.forget_held_popup(state, kind)
  held_popup_list = vim.tbl_filter(function(entry)
    if entry.state ~= state or entry.kind ~= kind then return true end
    notifications.withdraw(held_notice_id(entry))
    return false
  end, held_popup_list)
end

---Forget the popups held for `state`, which the user opened another way.
---@param state table
---@return ForgePopupOrigin? origin Where the earliest of them interrupted the user.
local function release_held_for(state)
  local origin
  held_popup_list = vim.tbl_filter(function(entry)
    if entry.state ~= state then return true end
    origin = origin or entry.origin
    notifications.withdraw(held_notice_id(entry))
    return false
  end, held_popup_list)
  return origin
end

---Show the first pending approval of `target_state`, or of the active state.
---
---The view's windows may be gone, for example after `q` closed the tab while the session kept its
---event subscription. Presentation then waits: the flags stay clear, so reopening the view
---presents the approval through `activate_snapshot`.
---@param reopen? boolean
---@param target_state? table
function M.present_approval(reopen, target_state)
  local state = target_state or harness_state()
  local request = state.approval and state.approval[1]
  if state.approval_open or not request then return end
  if not reopen and state.dismissed_approval_id == request.request_id then return end
  local host = picker_host(state)
  if #host.window_list == 0 then return end
  if not reopen and M.hold_while_typing(state, "approval", function()
    if not state.approval_open then M.present_approval(true, state) end
  end) then
    state.dismissed_approval_id = request.request_id
    if harness_state() == state then M.refresh_winbar() end
    return
  end
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
  local approval_host = {
    owner_label = owner_label,
    interrupt = function()
      if child_id then M.with_state(state, function() M.agent_action("interrupt", child_id) end)
      else cancel_exchange(state) end
    end,
    transcript_win = state.transcript_win,
    window_list = host.window_list,
    control_win = host.control_win,
    resolve = function(approval_id, answer_list, callback)
      client.request({ session_id = state.session.id, method = "session.approval.resolve", params = {
        approval_id = approval_id,
        answer_list = answer_list,
      } }, function(_, request_error)
        if request_error then
          notifications.error(request_error, "Harness approval")
          synchronize_state(nil, state)
          callback(false)
          return
        end
        callback(true)
      end)
    end,
    ---@param reason? "user"|"programmatic"
    closed = function(reason)
      state.approval_open = false
      state.presented_approval_id = nil
      local still_pending = vim.iter(state.approval or {}):any(function(approval)
        return approval.request_id == request.request_id
      end)
      if reason == "user" then
        -- Only the user's own close hides the approval until they reopen it.
        state.dismissed_approval_id = still_pending and request.request_id or nil
      elseif still_pending then
        -- Another picker replaced the popup, so the approval returns once no picker is open.
        vim.schedule(function()
          picker.when_idle(function() M.present_approval(false, state) end)
        end)
      end
      if harness_state() == state then M.refresh_winbar() end
    end,
  }
  local opened, failure = pcall(require("forge.views.harness.approval").open, request, approval_host)
  if not opened then
    notifications.error("Cannot show the Harness approval: " .. tostring(failure), "Harness approval")
    return
  end
  -- The flags follow a successful open, so a failed presentation never blocks a later one.
  state.dismissed_approval_id = nil
  state.approval_open = true
  state.presented_approval_id = request.request_id
end

---@param force? boolean
function M.present_plan_question(force)
  local state = harness_state()
  local elicitation = state.active_elicitation and state.active_elicitation.elicitation
  if state.question_clarification or (activity.busy(state) and not force) or state.plan_question_open or not elicitation then return end
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
  local host = picker_host(state)
  -- A view closed with `q` keeps its subscription but has no window to anchor the picker; the
  -- question stays unpresented until the view reopens.
  if #host.window_list == 0 then return end
  if not force and M.hold_while_typing(state, "plan_question", function()
    M.with_state(state, function() M.present_plan_question(false) end)
  end) then return end
  ---Send one answer or skip, and adopt the question set that remains open after it.
  local function settle_question(method, params, callback)
    params.target = question_target
    client.request({ session_id = session_id, method = method, params = params }, function(result, request_error, detail)
      if request_error then
        notifications.error(request_error, "Harness question")
        callback(nil, request_error, detail)
        return
      end
      state.active_plan = result.active_plan or state.active_plan
      local remaining = result.active_elicitation ~= vim.NIL and result.active_elicitation or nil
      question_presentation.receive(state, remaining, false)
      -- The reply that settles the last review question reports an acceptance the paused task
      -- only recorded; the review's decision reply stayed silent while these questions waited.
      if not remaining and (owner == "plan_review" or owner == "plan_acceptance") then
        task_control.report_recorded_acceptance(state, nil)
      end
      callback(remaining and remaining.elicitation)
    end)
  end
  question_presentation.mark_presented(state)
  state.plan_question_open = true
  require("forge.views.harness.plan_question").open(elicitation, {
    picker_host = host,
    transcript_win = state.transcript_win,
    window_list = host.window_list,
    control_win = host.control_win,
    allow_ask = true,
    clarification_draft = draft,
    answer = function(params, callback) settle_question("session.question.answer", params, callback) end,
    skip = function(params, callback) settle_question("session.question.skip", params, callback) end,
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
        -- The clarification suppresses plan-question popups only until this read settles, whether
        -- it succeeds or not; a failed read must not disable the popups for the session's life.
        synchronize_state(function(_, sync_failure)
          if state.question_clarification ~= pending_draft then return end
          state.question_clarification = nil
          if not sync_failure then M.with_state(state, function() M.present_plan_question(false) end) end
        end, state)
      end, nil, vim.tbl_extend("force", question_target, { question_id = params.question_id }))
    end,
    pending = function()
      state.execution_notice = "Submitting answers"
      M.render()
    end,
    continue = function()
      state.plan_question_open = false
      state.execution_notice = nil
      client.request({ session_id = session_id, method = "session.question.continue", params = { target = question_target } }, function(_, request_error, error_detail)
        if harness_state() ~= state or not state.session or state.session.id ~= session_id then return end
        if request_error and not (error_detail and error_detail.code == "turn_cancelled") then
          notifications.error(request_error, "Planning continuation")
        end
        M.render()
      end)
    end,
    closed = function()
      state.plan_question_open = false
      M.render()
    end,
  })
end

---Report whether the editor holds an approval or question it can present.
---@param state table
---@return boolean
function M.holds_question(state)
  -- The questions snapshot also keeps answered and withdrawn questions as history; only a pending
  -- one awaits the user.
  return #(state.approval or {}) > 0
    or require("forge.views.harness.questions").count(state) > 0
    or (state.active_elicitation ~= nil and state.active_elicitation.elicitation ~= nil)
end

---@param synchronized? boolean the state was just read, so a missing question is final
function M.reopen_question(synchronized)
  local state = harness_state()
  local held_origin = release_held_for(state)
  -- A popup the user opens while it is held returns, like a released one, to where it interrupted.
  if held_origin then
    picker.present_from(held_origin, function() M.reopen_question(synchronized) end)
    return
  end
  if #(state.approval or {}) > 0 then
    M.present_approval(true)
    return
  end
  -- Only a pending question takes this branch: the questions picker opens nothing for history
  -- alone, so a resolved question must not hide the plan question below.
  if require("forge.views.harness.questions").count(state) > 0 then
    require("forge.views.harness.questions").open(state, picker_host(state))
    return
  end
  if not (state.active_elicitation and state.active_elicitation.elicitation) then
    if not synchronized and state.status and state.status.kind == "awaiting_input" then
      -- The status says a question waits, but this editor never received it: read the host's.
      M.synchronize_state(function(_, failure)
        if failure then notifications.warn(failure, "ForgeHarness") return end
        if harness_state() == state then M.reopen_question(true) end
      end, state)
      return
    end
    notifications.warn("No Harness approval or question awaits feedback", "ForgeHarness")
    return
  end
  M.present_plan_question(true)
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
  cancel_exchange(state)
end
function M.compact(completed)
  if not (harness_state().capability and harness_state().capability.native_compact) then
    notifications.error("The current provider does not support manual context compaction", "ForgeHarness")
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

---Send the question that opened an aside as the aside's first input.
---
---The host holds the input until the aside's provider fork is ready, so the question is
---submitted as soon as the aside's view is bound to its session.
---@param aside_state table
---@param aside_session_id string?
---@param prompt string
local function submit_aside_prompt(aside_state, aside_session_id, prompt)
  local input = aside_state.input
  if not (aside_session_id and input and input.session_id == aside_session_id) then
    -- The aside's view did not bind to its session, so no view exists to queue the question in.
    notifications.warn("The aside's tab could not be opened, so its question was not sent. "
      .. "Resume the aside from /sessions and ask again: " .. prompt, "Harness aside")
    return
  end
  input_state.submit(aside_state, prompt, {}, "auto")
end

---@class ForgeHarnessForkOptions
---@field aside_prompt string? Opens the fork as an aside of the current session and sends this question to it.
---@field restore_draft? fun(): boolean Returns the submitted `/fork` or `/btw` command to the
---  composer when the fork is refused, from `composer_draft.take`.

---Clear a submitted fork command from the composer and return its restoration for a refusal.
---@param state ForgeHarnessPresentationState
---@return fun(): boolean
local function take_fork_command(state)
  local composer = state.composer_buf
  return require("forge.views.harness.composer_draft").take(composer,
    vim.api.nvim_buf_get_lines(composer, 0, -1, false), attachment.capture(composer).attachments,
    function() return harness_state() == state and state.composer_buf == composer end)
end

---Ask the host to fork the current session, and open the fork's tab once the host has created it.
---A refusal opens nothing, so no frame shows a tab for a session that never existed; the command
---returns to the composer, whose focus and mode never changed.
---@param name? string
---@param options? ForgeHarnessForkOptions
function M.fork_session(name, options)
  local aside_prompt = options and options.aside_prompt
  local restore_draft = options and options.restore_draft or function() return false end
  local state = harness_state()
  local active_session = state.session
  if not (active_session and active_session.id) then
    restore_draft()
    notifications.error("No active Harness session to fork", "ForgeHarness")
    return
  end
  if state.capability.native_fork ~= true then
    restore_draft()
    notifications.warn("The current provider does not support session fork", "ForgeHarness")
    return
  end
  local params = { session_id = active_session.id }
  if name and name ~= "" then params.name = name end
  if aside_prompt then params.aside = true end
  local fork_started = perf.now()
  client.request({ session_id = active_session.id, method = "session.fork", params = params }, function(result, request_error)
    if request_error then
      perf.event("harness", "harness.fork.failed", {
        ms = perf.elapsed_ms(fork_started),
        source_session_id = active_session.id,
        error = tostring(request_error),
      })
      restore_draft()
      notifications.error(request_error, "Harness fork")
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
    local fork_state = session_navigation.open_fork(result, state)
    if aside_prompt then
      submit_aside_prompt(fork_state or {}, result.session and result.session.id, aside_prompt)
    end
  end)
end

function M.open_timeline_entry()
  local state = harness_state()
  if vim.api.nvim_get_current_buf() ~= state.transcript_buf then return end
  if not state.presentation then return end
  state.presentation.activate(function(action, captured)
    if state.presentation.open_output(action, captured) then return end
    if action.kind == "question" then
      for _, question in ipairs((state.questions or {}).questions or {}) do
        if question.id == action.question_set_id then
          state.question_show_history = question.resolution.kind ~= "pending"
          state.question_picker_resume = nil
          require("forge.views.harness.questions").open(state, picker_host(state), question.id)
          return
        end
      end
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
      require("forge.views.harness.source_actions").open_file(state.session.workspace, action)
    elseif action.kind == "quickfix" then
      require("forge.views.harness.source_actions").quickfix(state.session.workspace, action)
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

---Open one artifact's review, or explain why a plan still being drafted has none yet.
---@param state table
---@param artifact { id: string, state?: string }
---@param revision? integer
function M.open_artifact(state, artifact, revision)
  if artifact.state == "generating" or artifact.state == "configuring_checks" then
    notifications.info("This plan is still being drafted; it opens once a revision is submitted", "Harness artifact")
    return
  end
  local session_id, generation = state.session.id, client.host_get_generation()
  client.request({ session_id = session_id, method = "session.plan.get",
    params = { plan_id = artifact.id, revision = revision } }, function(plan, failure)
    if not state.session or state.session.id ~= session_id or client.host_get_generation() ~= generation then return end
    if failure then notifications.error(failure, "Harness artifact") return end
    require("forge.views.plan_review").open(plan)
  end)
end

function M.open_artifact_picker()
  local state = harness_state()
  local session_id, generation = state.session.id, client.host_get_generation()
  -- A plan awaiting review opens directly, also while its task is paused after a restart.
  local active = state.active_plan
  if active and active.state == "awaiting_review"
    and not (state.status and state.status.kind == "awaiting_plan_review") then
    M.open_artifact(state, active)
    return
  end
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
    M.open_artifact(state, artifact)
  end, { empty_text = "This session has no artifacts." })
end

---@class ForgeRollbackInteraction
---@field id string
---@field ordinal integer
---@field prompt string
---@field opening? ForgeExchangeOpening
---@field state "queued"|"running"|"complete"|"failed"|"cancelled"|"interrupted"
---@field checkpoint_before? string
---@field elapsed_ms? integer
---@field edits? { file_count: integer, additions: integer, deletions: integer }
---@field rollback_unavailable? string Why the host cannot roll back starting with this interaction.
---@field task_command? string The `/goal` or `/plan` command a rollback of this task start returns.

---Put `text` in the composer and focus it for an immediate follow-up.
---
---`composer_free`, from `composer_draft.observe`, marks a restoration that completes long after the
---user acted, such as a rollback: it then writes only into a composer that holds no newer work,
---and moves the cursor only when the composer is already the current window, so a user who moved
---on keeps both their text and their window.
---@param text string
---@param composer_free? fun(): boolean
---@return boolean restored
local function restore_composer_prompt(text, composer_free)
  local state = harness_state()
  if composer_free then
    if not composer_free() then return false end
    set_composer_text(state.composer_buf, text)
    if state.composer_win and vim.api.nvim_win_is_valid(state.composer_win) then
      layout.resize_composer(state.composer_buf, state.composer_win)
      if vim.api.nvim_get_current_win() == state.composer_win then
        local line_list = vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)
        vim.api.nvim_win_set_cursor(state.composer_win, { math.max(1, #line_list), #(line_list[#line_list] or "") })
      end
    end
    return true
  end
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
  return true
end

---Report a rollback failure and keep it in `:messages`.
---
---The notice fades, and a rollback the user confirmed must still explain afterwards why nothing
---changed, so the failure also enters message history.
---@param message string
---@param title? string
local function report_undo_failure(message, title)
  notifications.error(message, title or "Harness undo")
  pcall(vim.api.nvim_echo, { { (title or "Harness undo") .. ": " .. tostring(message), "ErrorMsg" } }, true, {})
end

---Confirm a files restore from its preview.
---
---Preparing the preview collects a running turn, because the preview compares the workspace with
---the closing checkpoint that only a settled exchange writes, so `stopped_turn` marks a preview
---prepared over a running turn and its Cancel says the turn already stopped.
---@param preview table
---@param interaction? ForgeRollbackInteraction
---@param stopped_turn? boolean
---@param selection? { exchange_id: string, latest_exchange_id: string }
local function confirm_restore_preview(preview, interaction, stopped_turn, selection)
  local state = harness_state()
  local session_id, generation = state.session.id, client.host_get_generation()
  local function current()
    return harness_state() == state and state.session and state.session.id == session_id
      and client.host_get_generation() == generation
  end
  local file_count = tonumber(preview.files) or 0
  local description = { ("%d %s will be restored."):format(file_count, file_count == 1 and "file" or "files"),
    "Git’s index will not change." }
  for _, warning in ipairs(preview.warnings or {}) do description[#description + 1] = warning end
  local choices = {
    { label = "Cancel", value = "cancel", detail = stopped_turn
      and "Keep files and history; the running turn was already stopped to compute this preview" or nil },
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
        if err then report_undo_failure(err) return end
        confirm_restore_preview(page, interaction, stopped_turn, selection)
      end)
      return
    end
    if state.restore_applying then return end
    -- A preview without an interaction undoes the last file restore and changes no conversation.
    local operation = { scope = interaction and "workspace" or "files" }
    state.restore_applying = operation
    render_status_hint(state)
    local composer_free = state.composer_buf and vim.api.nvim_buf_is_valid(state.composer_buf)
      and require("forge.views.harness.composer_draft").observe(state.composer_buf) or function() return false end
    client.request({ session_id = session_id, method = "session.exchange.rollback.apply", params = { preview_id = preview.preview_id } }, function(_, err)
      if state.restore_applying ~= operation then return end
      state.restore_applying = nil
      render_status_hint(state)
      if not (state.session and state.session.id == session_id and client.host_get_generation() == generation) then return end
      synchronize_state(nil, state)
      if harness_state() ~= state then
        if not err and interaction then
          M.with_state(state, function() M.restore_rolled_back_prompt(interaction, composer_free) end)
        end
        return
      end
      if err then
        report_undo_failure(err)
        if err:find("preview is stale", 1, true) and selection then
          client.request({ session_id = session_id, method = "session.exchange.rollback.prepare", params = selection }, function(refreshed, failure)
            if not current() then return end
            if failure then report_undo_failure(failure) return end
            confirm_restore_preview(refreshed, interaction, stopped_turn, selection)
          end)
        end
        return
      end
      if interaction then M.restore_rolled_back_prompt(interaction, composer_free) end
    end)
  end)
end

---Name the composer text that recreates a rolled-back exchange, or nil when the user wrote none.
---
---A goal or plan start returns as the command that started it, `/goal <objective>` or
---`/plan <request>`, which the host names as `task_command` because only it holds a plan's
---request. Other task starts and continuations carry Harness-authored instructions, which the
---user never typed and the next task transition regenerates.
---@param interaction ForgeRollbackInteraction
---@return string?
function M.rolled_back_prompt(interaction)
  if type(interaction.task_command) == "string" and interaction.task_command ~= "" then
    return interaction.task_command
  end
  if require("forge.render.harness.exchange_opening").notice(interaction.opening) then return nil end
  return interaction.prompt or ""
end

---Return a rolled-back exchange's input to the composer when the user wrote it.
---
---A native rewind can take seconds, so `composer_free` (from `composer_draft.observe` when the user
---confirmed the rollback) keeps the prompt out of a composer that now holds newer work.
---@param interaction ForgeRollbackInteraction
---@param composer_free? fun(): boolean
function M.restore_rolled_back_prompt(interaction, composer_free)
  local prompt = M.rolled_back_prompt(interaction)
  if not prompt then return end
  if not restore_composer_prompt(prompt, composer_free) and composer_free
    and prompt ~= "" then
    notifications.warn("The rolled-back prompt was not restored because the composer holds newer text",
      "Harness undo")
  end
end

---@param first_removed ForgeRollbackInteraction
---@param latest_exchange_id string
---@param retained_ordinal? integer
local function confirm_rollback(first_removed, latest_exchange_id, retained_ordinal)
  local state = harness_state()
  local session_id, generation = state.session.id, client.host_get_generation()
  if state.restore_applying then return end
  -- Both scopes collect active execution before they change anything, and leave the task paused.
  -- Conversation only collects on confirmation; a files rollback collects to compute its preview.
  local busy = activity.busy(state)
  local choices = {
    { label = "Cancel", value = "cancel" },
    { label = "Conversation only", detail = (busy and "Stops the running turn. " or "") .. "Keep all current files",
      value = "conversation" },
  }
  if first_removed.checkpoint_before and not state.no_checkpoint then
    choices[#choices + 1] = { label = "Conversation and files",
      detail = (busy and "Stops the running turn to preview the files. " or "")
        .. ("Restore files from before Exchange %s"):format(first_removed.ordinal),
      value = "workspace" }
  end
  local title = retained_ordinal and ("Roll back after Exchange %s"):format(retained_ordinal)
    or "Roll back to the start"
  local selection = { exchange_id = first_removed.id, latest_exchange_id = latest_exchange_id }
  open_choice_picker(state, title, "Choose what to roll back", choices, function(choice)
    if harness_state() ~= state or not state.session or state.session.id ~= session_id
      or client.host_get_generation() ~= generation then return end
    if not choice or choice == "cancel" or state.restore_applying then return end
    if choice == "workspace" then
      client.request({ session_id = session_id, method = "session.exchange.rollback.prepare", params = selection }, function(preview, err)
        if harness_state() ~= state or not state.session or state.session.id ~= session_id
          or client.host_get_generation() ~= generation then return end
        if err then report_undo_failure(err) return end
        confirm_restore_preview(preview, first_removed, busy, selection)
      end)
      return
    end
    if choice ~= "conversation" then return end
    local operation = { scope = "conversation" }
    state.restore_applying = operation
    render_status_hint(state)
    local composer_free = state.composer_buf and vim.api.nvim_buf_is_valid(state.composer_buf)
      and require("forge.views.harness.composer_draft").observe(state.composer_buf) or function() return false end
    local rollback = vim.tbl_extend("force", selection, { scope = "conversation" })
    M.task_transition({ action = "undo", rollback = rollback }, nil, function(applied, failure)
      if state.restore_applying ~= operation then return end
      state.restore_applying = nil
      render_status_hint(state)
      -- The task command already showed the failure as a notice; message history keeps it.
      if not applied and failure then
        pcall(vim.api.nvim_echo, { { "Harness undo: " .. tostring(failure), "ErrorMsg" } }, true, {})
      end
      if not applied or not state.session or state.session.id ~= session_id
        or client.host_get_generation() ~= generation then return end
      M.with_state(state, function() M.restore_rolled_back_prompt(first_removed, composer_free) end)
    end)
  end)
end

---@param edits? { file_count: integer, additions: integer, deletions: integer }
---@param file_width integer
---@param additions_width integer
---@return string, table[]?
local function undo_edit_cell(edits, file_width, additions_width)
  if type(edits) ~= "table" then return string.rep(" ", file_width - 1) .. "—" end
  if edits.file_count == 0 then return "" end
  local file_count = ("%" .. file_width .. "d"):format(edits.file_count)
  local additions = tostring(edits.additions)
  local deletions = "−" .. edits.deletions
  local separator = string.rep(" ", additions_width - #additions + 1)
  local prefix = file_count .. " · "
  local added = "+" .. additions
  return prefix .. added .. separator .. deletions, {
    { prefix }, { added, "ForgeAddRange" }, { separator }, { deletions, "ForgeDeleteRange" },
  }
end

---Open interaction history before choosing a rollback scope.
---
---Each exchange row names the last exchange to keep. The host takes the first exchange to remove,
---which also supplies the file checkpoint and the prompt to return to the composer.
---@param recovery? table restore journal status from `session.exchange.recovery`
---@param refused fun() puts the `/undo` command back when no list opens
local function open_exchange_undo_picker(recovery, refused)
  local state = harness_state()
  -- Every rollback rewinds the provider conversation natively; Harness never replays history.
  if not (state.capability and state.capability.native_conversation_rewind == true) then
    notifications.warn("Conversation rollback is unsupported: the current provider cannot rewind its conversation", "Harness undo")
    refused()
    return
  end
  local session_id, generation = state.session and state.session.id, client.host_get_generation()
  if not session_id then refused() return end
  client.request({ session_id = session_id, method = "session.exchange.list", params = {} }, function(interaction_list, request_error)
    if harness_state() ~= state or not state.session or state.session.id ~= session_id
      or client.host_get_generation() ~= generation then return end
    if request_error then
      report_undo_failure(request_error)
      refused()
      return
    end
    local current_exchange_list = {}
    local file_width, additions_width = 1, 1
    for _, interaction in ipairs(interaction_list or {}) do
      if interaction.disposition == "current" and interaction.state ~= "queued" then
        current_exchange_list[#current_exchange_list + 1] = interaction
        local edits = interaction.edits
        if type(edits) == "table" then
          file_width = math.max(file_width, #tostring(edits.file_count))
          additions_width = math.max(additions_width, #tostring(edits.additions))
        end
      end
    end
    local option_list = {}
    if recovery and not recovery.pending then
      option_list[#option_list + 1] = { id = "undo-file-restore", label = "Undo last file restore",
        columns = { "", "", "Undo last file restore" }, value = { undo_file_restore = true } }
    end
    local first_exchange = current_exchange_list[1]
    if first_exchange then
      local unavailable = first_exchange.rollback_unavailable
      option_list[#option_list + 1] = {
        id = "before-first-exchange",
        label = "Before first exchange" .. (unavailable and " · unavailable" or ""),
        columns = { "", "", unavailable and ("Before first exchange · " .. unavailable) or "Before first exchange" },
        value = { first_removed = first_exchange },
      }
    end
    for index, interaction in ipairs(current_exchange_list) do
      local first_removed = current_exchange_list[index + 1]
      local host_notice = require("forge.render.harness.exchange_opening").notice(interaction.opening)
      local prompt = host_notice or vim.trim(tostring(interaction.prompt or ""):gsub("%s+", " "))
      local unavailable = first_removed and first_removed.rollback_unavailable
      local label_suffix = not first_removed and " · latest" or unavailable and " · unavailable" or ""
      local detail = prompt ~= "" and prompt or "[empty prompt]"
      local elapsed = type(interaction.elapsed_ms) == "number" and ("%ds"):format(math.floor(interaction.elapsed_ms / 1000))
        or "—"
      local edit_summary, edit_segments = undo_edit_cell(interaction.edits, file_width, additions_width)
      option_list[#option_list + 1] = {
        id = interaction.id,
        label = "Exchange " .. tostring(interaction.ordinal) .. label_suffix,
        columns = { elapsed, edit_summary, unavailable and (unavailable .. " · " .. detail) or detail },
        column_segments = { { { elapsed, "ForgePickerDuration" } }, edit_segments },
        disabled_reason = not first_removed and "There are no later exchanges to roll back" or nil,
        value = { id = interaction.id, retained_ordinal = interaction.ordinal, first_removed = first_removed },
      }
    end
    open_choice_picker(
      state,
      "Undo",
      nil,
      option_list,
      function(destination)
        if harness_state() ~= state or not state.session or state.session.id ~= session_id
          or client.host_get_generation() ~= generation then return end
        if destination.undo_file_restore then
          client.request({ session_id = session_id, method = "session.exchange.recovery", params = { action = "undo" } }, function(preview, err)
            if harness_state() ~= state or not state.session or state.session.id ~= session_id
              or client.host_get_generation() ~= generation then return end
            if err then report_undo_failure(err, "Harness recovery") return end
            confirm_restore_preview(preview)
          end)
        elseif destination.first_removed then
          local removed = destination.first_removed
          if removed.rollback_unavailable then
            report_undo_failure(removed.rollback_unavailable)
          else
            confirm_rollback(removed, interaction_list[#interaction_list].id, destination.retained_ordinal)
          end
        end
      end,
      {
        empty_text = "This session has no exchanges available to undo.",
        selected_index = #option_list,
        wrap_selection = false,
        assign_choice_keys = false,
        height_ratio = 0.3,
        column_headers = { "Time", "Edits", "Details" },
        flexible_column = 3,
        preview = state.presentation and state.presentation.exchange_preview
          and state.presentation.exchange_preview(state.transcript_win),
      }
    )
  end)
end

---Open interaction history directly unless an unfinished file restore needs recovery.
---
---`refused`, from `composer_draft.take` when `/undo` was submitted, returns the command to the
---composer when no picker opens, so a refusal never consumes what the user typed.
---@param refused? fun()
function M.open_undo_picker(refused)
  refused = refused or function() end
  local state = harness_state()
  if state.restore_applying then
    notifications.warn("A rollback is already in progress", "Harness undo")
    refused()
    return
  end
  if not state.session then refused() return end
  local session_id, generation = state.session.id, client.host_get_generation()
  client.request({ session_id = session_id, method = "session.exchange.recovery", params = {} }, function(recovery, err)
    if harness_state() ~= state or not state.session or state.session.id ~= session_id
      or client.host_get_generation() ~= generation then return end
    if err then report_undo_failure(err, "Harness recovery") refused() return end
    if not recovery or recovery == vim.NIL then open_exchange_undo_picker(nil, refused) return end
    if recovery.applying then
      notifications.warn("A rollback is already in progress", "Harness undo")
      refused()
      return
    end
    if not recovery.pending then open_exchange_undo_picker(recovery, refused) return end
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
        if failure then report_undo_failure(failure, "Harness recovery") synchronize_state() return end
        if choice == "undo" then confirm_restore_preview(result) else synchronize_state() end
      end)
    end)
  end)
end

---Switch the transcript and composer target to one child-agent timeline or Main.
---@param selector string
function M.select_agent(selector)
  local state = harness_state()
  local previous_run_id = state.selected_agent_run_id
  local target_run_id
  if selector:lower() ~= "main" then
    local run, resolve_error = require("forge.views.harness.agent_catalog").resolve(state, selector)
    if not run then
      notifications.warn(resolve_error or ("No child agent matches " .. selector), "Harness agent")
      return
    end
    target_run_id = run.id
  end
  state.selected_agent_run_id = target_run_id
  -- Composer input goes to the selected timeline, so a selection the presentation failed to show
  -- reverts: otherwise Ctrl-S would address a child under a transcript still showing Main. The
  -- presentation reports that failure through this callback, as a failure string or as `false`
  -- followed by one, or by returning a failure string.
  local function selection_settled(outcome, detail)
    local failure = type(outcome) == "string" and outcome or (outcome == false and (detail or true)) or nil
    if not failure or state.selected_agent_run_id ~= target_run_id then return end
    state.selected_agent_run_id = previous_run_id
    M.with_state(state, M.render)
  end
  if state.presentation then
    local immediate = state.presentation.select_agent(target_run_id, selection_settled)
    if type(immediate) == "string" then selection_settled(immediate) end
  end
  M.render()
end

--- Admit a fresh child using a global profile and explicit task text.
---@param profile string|ForgeSpawnRequest
---@param text? string
---@param access? ForgeSpawnAccess
function M.spawn_agent(profile, text, access)
  local state = harness_state()
  local request = type(profile) == "table" and profile or { profile = profile, text = text, access = access }
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
---@param on_result? fun(failure_message: string?) Receives why the host could not carry the action out, or nil.
function M.agent_action(action, run_id, message, on_result)
  local state = harness_state()
  local params = { target = run_id, message = message }
  if action == "tools" then M.configure({ subagent_tools = state.subagent_tools ~= true }) return end
  if action == "message" or action == "followup" then
    if not message or message:match("^%s*$") then return end
  end
  local method = action == "message" and "session.agent.message.send" or "session.agent." .. action
  -- A message typed in the composer leaves it at submission, as a root prompt does, so a second
  -- submit before the reply finds an empty composer instead of sending the message again.
  local restore_draft = function() return false end
  local buffer = state.composer_buf
  if message and not state.queue_edit and buffer and vim.api.nvim_buf_is_valid(buffer)
    and composer_text(buffer) == message then
    dismiss_composer_completion(buffer)
    restore_draft = require("forge.views.harness.composer_draft").take(buffer,
      vim.api.nvim_buf_get_lines(buffer, 0, -1, false), attachment.capture(buffer).attachments,
      function() return state.composer_buf == buffer and not state.queue_edit end)
  end
  client.request({ session_id = state.session.id, method = method, params = params }, function(result, failure)
    if failure then
      restore_draft()
      notifications.error(failure, "Harness child " .. action)
      if on_result then on_result(type(failure) == "table" and failure.message or tostring(failure)) end
      return
    end
    -- An interrupt request succeeds as a whole and reports each child's outcome separately, so
    -- a child that could not be interrupted is named here rather than passing silently.
    local target_failure = require("forge.views.harness.agent_catalog").interrupt_failure(result)
    if target_failure then notifications.warn(target_failure, "Harness child " .. action) end
    synchronize_state(nil, state)
    if on_result then on_result(target_failure) end
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
---@param access? ForgeSpawnAccess
function M.open_spawn_picker(profile, access)
  local state = harness_state()
  local session_id = state.session and state.session.id
  client.request({ method = "agent.profile.get", params = {} }, function(snapshot, failure)
    if not state.session or state.session.id ~= session_id then return end
    if failure then notifications.error(failure, "Subagent profiles") return end
    if snapshot.error then notifications.warn(snapshot.error, "Subagent profiles") end
    require("forge.views.harness.spawn_picker").open({
      host = picker_host(state), profile = profile, access = access, document = snapshot.document, on_spawn = M.spawn_agent,
    })
  end)
end

function M.open_session_picker()
  require("forge.views.harness.session_picker").open(picker_host(harness_state()))
end

function M.open_config()
  local state = harness_state()
  local window = vim.api.nvim_get_current_win()
  local current_buffer = vim.api.nvim_win_get_buf(window)
  if not state.session or (current_buffer ~= state.transcript_buf and current_buffer ~= state.composer_buf) then
    require("forge.views.harness.global_settings").open({ window_list = { window }, control_win = window })
    return
  end
  require("forge.views.harness.settings").open(state, picker_host(state))
end

---List provider-owned background shells and terminate only the selected shell.
function M.open_background_picker()
  local state = harness_state()
  if state.capability and state.capability.background_terminals == false then
    notifications.warn("The current provider does not support background terminals", "ForgeHarness")
    return
  end
  if not state.capability or state.capability.background_terminals ~= true then
    notifications.warn("Background terminal capability is not available yet", "ForgeHarness")
    return
  end
  local session_id, generation = state.session.id, client.host_get_generation()
  local function current()
    return state.session and state.session.id == session_id and client.host_get_generation() == generation
  end
  client.request({ session_id = session_id, method = "session.document", params = { operation = "background_terminals" } }, function(inventory, failure)
    if not current() then return end
    if failure then notifications.error(failure, "Select Background Terminal") return end
    -- The picker lists this live read, so the status count adopts it rather than a stale push.
    if state.presentation and state.presentation.terminals then state.presentation.terminals.adopt(inventory) end
    if not inventory.supported then
      notifications.warn("The current provider does not support background terminals", "ForgeHarness")
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
    end, { empty_text = "No background terminals are running." })
  end)
end

---Explain how an edit ended when its message left the queue before the edit took hold.
---@param recovered ("sent"|"edited")?
---@param saved_draft? ForgePromptDraft
function M.report_recovered_edit(recovered, saved_draft)
  if recovered == "sent" then
    notifications.info("The queued message was sent before the edit took hold. The composer shows your earlier draft again.", "ForgeHarness")
  elseif recovered == "edited" then
    local message = "The queued message was sent before the edit took hold. Your changed text is now a normal draft."
    if saved_draft then message = message .. " Press Up to recall your earlier draft." end
    notifications.warn(message, "ForgeHarness")
  end
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
  local original, failure, attachments, recovered = queue_editor.finish(state, text, unqueue)
  if failure then notifications.warn(failure, "ForgeHarness") return end
  local saved_draft
  if recovered == "edited" and edit.original_draft:find("%S") then
    saved_draft = { text = edit.original_draft, attachments = edit.original_attachments }
    prompt_history.record(saved_draft)
  elseif unqueue and not recovered then
    -- The composer takes the unqueued message, so the draft it held before editing moves into prompt
    -- history, where Up recalls it.
    if original and original:find("%S") then
      saved_draft = { text = original, attachments = attachments or {} }
      prompt_history.record(saved_draft)
    end
    original, attachments = text, selected_attachments
  elseif text ~= "" and not recovered then prompt_history.record(text) end
  set_composer_text(state.composer_buf, original, attachments)
  if state.composer_win and vim.api.nvim_win_is_valid(state.composer_win) then
    layout.resize_composer(state.composer_buf, state.composer_win)
    if unqueue then vim.api.nvim_set_current_win(state.composer_win) end
  end
  M.refresh_winbar()
  M.report_recovered_edit(recovered, saved_draft)
  if saved_draft and not recovered then
    notifications.info("Your earlier draft was saved to prompt history; press Up to recall it.", "ForgeHarness")
  end
  if state.presentation and not recovered then state.presentation.follow_tail() end
end

---Recover an edit after delivery won its reservation, keeping edits on other messages.
---@param state table
function M.settle_delivered_edit(state)
  if queue_editor.delivered(state) then finish_queue_edit(state, composer_text(state.composer_buf), false) end
end

---Explain why no session is attached: a recorded startup failure, or startup still running.
---@param state table
---@param title string
function M.report_uninitialized(state, title)
  if state.start_failure then
    notifications.error("Harness failed to start: " .. state.start_failure .. ". Reopen Harness to retry.", title)
  else
    notifications.warn("Harness is still initializing", title)
  end
end

---Keep unowned startup drafts out of session history and the input pipeline.
---@param state ForgeHarnessPresentationState
---@return boolean
local function admit_composer_input(state)
  if state.switching_provider then
    -- The host this view will run on is still starting, whether for a provider switch or for a
    -- view returning to its own provider.
    notifications.warn((state.switch_notice or "Switching the Harness provider")
      .. ". The draft stays in the composer; submit it again once the switch finishes.", "ForgeHarness")
    return false
  end
  if state.host_error then
    -- A view whose host stopped or now serves another provider comes back on its own submit; the
    -- draft stays in the composer until the user submits it to the returned session.
    if not require("forge.views.harness").reconnect(state) then
      notifications.error(state.execution_notice or state.host_error, "ForgeHarness")
    end
    return false
  end
  if not state.session or type(state.session.id) ~= "string" or state.session.id == "" then
    M.report_uninitialized(state, "ForgeHarness")
    return false
  end
  return true
end

---Preserve drafts when the owner cannot execute them, while leaving navigation available.
---@param state ForgeHarnessPresentationState
---@return boolean
local function admit_execution(state)
  local unavailable = activity.unavailable_reason(state)
  if not unavailable then return true end
  notifications.error("Session unavailable: " .. unavailable, "ForgeHarness")
  return false
end

function M.submit()
  local state = harness_state()
  -- Enter submitted instead of accepting a completion, so the menu closes on every outcome,
  -- including a refused or rejected submit that leaves its text in the composer.
  dismiss_composer_completion(state.composer_buf)
  if not admit_composer_input(state) then return end
  require("forge.text").invalidate(state.composer_buf)
  if not attachments_ready(state.composer_buf) then return end
  state.execution_notice = nil
  local text = composer_text(state.composer_buf)
  if state.queue_edit then finish_queue_edit(state, text) return end
  if text == "/task refresh" then
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
  if require("forge.views.harness.completion.command_source").queues_execution(text)
    and not admit_execution(state) then return end
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
  if text == "/goal" then
    restore_composer_prompt("/goal ")
    notifications.warn("Describe the goal after /goal", "ForgeHarness")
    return
  end
  if text == "/btw" then
    restore_composer_prompt("/btw ")
    notifications.warn("Ask a question after /btw", "ForgeHarness")
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
  if text == "/provider" then
    set_composer_text(state.composer_buf, "")
    M.select_provider()
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
    M.fork_session(nil, { restore_draft = take_fork_command(state) })
    return
  end
  if text == "/new" or text == "/clear" then
    set_composer_text(state.composer_buf, "")
    require("forge.views.harness").new_session(nil, { open_mode = text == "/clear" and "current" or "tab" })
    return
  end
  if text == "/questions" then
    set_composer_text(state.composer_buf, "")
    M.reopen_question()
    return
  end
  local agent_tools_action = text:match("^/agents%s+(%S+)%s*$")
  if agent_tools_action == "on" or agent_tools_action == "off" then
    set_composer_text(state.composer_buf, "")
    M.configure({ subagent_tools = agent_tools_action == "on" })
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
  if text == "/sessions" or text == "/s" then
    set_composer_text(state.composer_buf, "")
    M.open_session_picker()
    return
  end
  if text == "/undo" then
    local composer = state.composer_buf
    local refused = require("forge.views.harness.composer_draft").take(composer,
      vim.api.nvim_buf_get_lines(composer, 0, -1, false), {},
      function() return harness_state() == state and state.composer_buf == composer end)
    M.open_undo_picker(function() refused() end)
    return
  end
  local spawn_arguments = text:match("^/agents%s+spawn%s+(.+)$")
  if spawn_arguments then
    local access, arguments = spawn_arguments:match("^%-%-(%S+)%s*(.*)$")
    if access ~= nil and access ~= "read" and access ~= "write" then
      notifications.warn("Use --read or --write for subagent access", "Harness agent")
      return
    end
    local spawn_profile, spawn_task = (arguments or spawn_arguments):match("^(%S+)%s*(.*)$")
    set_composer_text(state.composer_buf, "")
    if spawn_task and spawn_task ~= "" then M.spawn_agent(spawn_profile, spawn_task, access)
    else M.open_spawn_picker(spawn_profile, access) end
    return
  end
  local agent_selector = text:match("^/agents%s+(%S+)%s*$")
  if agent_selector then
    set_composer_text(state.composer_buf, "")
    M.select_agent(agent_selector)
    return
  elseif text:match("^/agents%s+") then
    set_composer_text(state.composer_buf, "")
    notifications.warn("Use /agents, /agents on|off, /agents edit [profile], or /agents spawn [--read|--write] [profile] [text]", "ForgeHarness")
    return
  end
  if text == "/compact" then
    set_composer_text(state.composer_buf, "")
    if state.capability.native_compact ~= true then
      notifications.warn("The current provider does not support manual context compaction", "ForgeHarness")
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
    M.fork_session(vim.trim(fork_name), { restore_draft = take_fork_command(state) })
    return
  end
  local aside_prompt = text:match("^/btw%s+(.+)$")
  if aside_prompt then
    M.fork_session(nil, { aside_prompt = vim.trim(aside_prompt), restore_draft = take_fork_command(state) })
    return
  end
  local new_name = text:match("^/new%s+(.+)$")
  if new_name then
    set_composer_text(state.composer_buf, "")
    require("forge.views.harness").new_session(vim.trim(new_name))
    return
  end
  local model_arguments = text:match("^/model%s+(%S.*)$")
  if model_arguments then
    if state.capability.model_selection ~= true then
      set_composer_text(state.composer_buf, "")
      notifications.warn("The current provider does not support model selection", "ForgeHarness")
      return
    end
    local session_id = state.session.id
    -- The catalog resolves the typed id or display name before anything is sent, so a name the
    -- provider does not offer is refused here and never enters the winbar's pending settings.
    require("forge.views.harness.provider_catalog").models(false, function(models, catalog_failure)
      if harness_state() ~= state or not state.session or state.session.id ~= session_id then return end
      if catalog_failure then
        report_configuration_error("the model catalog is unavailable: " .. catalog_failure)
        return
      end
      local next_config, _, failure = require("forge.views.harness.model_options").resolve_command(models or {},
        model_arguments, false)
      if not next_config then report_configuration_error(assert(failure)) return end
      if composer_text(state.composer_buf) == text then set_composer_text(state.composer_buf, "") end
      if next_config.effort and state.capability.effort_selection ~= true then
        notifications.warn("The current provider does not support reasoning effort selection", "ForgeHarness")
        return
      end
      local selected = selected_settings(state)
      if next_config.model == selected.model and (next_config.effort == nil or next_config.effort == selected.effort) then
        notifications.info(("The session already uses %s%s; nothing changed."):format(next_config.model,
          next_config.effort and (" with " .. next_config.effort .. " effort") or ""), "ForgeHarness")
        return
      end
      M.configure(next_config)
    end)
    return
  end
  local effort = text:match("^/effort%s+(%S+)$")
  if effort then
    set_composer_text(state.composer_buf, "")
    if state.capability.effort_selection ~= true then
      notifications.warn("The current provider does not support reasoning effort selection", "ForgeHarness")
      return
    end
    M.configure({ effort = effort })
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
  if activity.busy(state) and not state.checks_running and queue_only_commands[text:match("^%s*(%S+)")] then
    M.queue_submit()
    return
  end
  if text == "/config" or text == "/log" or text:match("^/log%s") then
    set_composer_text(state.composer_buf, "")
    local settings = require("forge.views.harness.settings")
    if text == "/config" then M.open_config()
    else settings.log(state.session.id, text:match("^/log%s+(.+)$")) end
    return
  end
  if not admit_execution(state) then return end
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
  if not admit_composer_input(state) then return end
  if not admit_execution(state) then return end
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
  local state = harness_state()
  -- As in `submit`, the menu closes on every outcome, including a refusal that keeps the draft.
  dismiss_composer_completion(state.composer_buf)
  if not admit_composer_input(state) then return end
  if not attachments_ready(state.composer_buf) then return end
  state.execution_notice = nil
  if state.queue_edit then finish_queue_edit(state, composer_text(state.composer_buf), true) return end
  local text = composer_text(state.composer_buf)
  if text == "" then return end
  local accepted, admission_failure = require("forge.views.harness.completion.command_source").accepts_prompt(text)
  if not accepted then notifications.warn(assert(admission_failure), "ForgeHarness") return end
  if not require("forge.views.harness.completion.command_source").queues_execution(text) then
    M.submit()
    return
  end
  if not admit_execution(state) then return end
  local captured_config, prompt, failure = model_command(text)
  if failure then report_configuration_error(failure) return end
  prompt_history.record(text)
  local attachments = attachment.capture(state.composer_buf).attachments
  local restore = require("forge.views.harness.composer_draft").take(state.composer_buf,
    vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false), attachments, function() return not state.queue_edit end)
  input_state.submit(state, prompt or text, attachments, "queue", captured_config, nil, restore)
  M.refresh_winbar()
end

function M.edit_queued(direction)
  local state = harness_state()
  if not attachments_ready(state.composer_buf) then return end
  if queue_editor.delivered(state) then M.settle_delivered_edit(state) return end
  local draft = composer_text(state.composer_buf)
  local text, failure, attachments, recovered = queue_editor.move(state, draft, direction)
  if failure then notifications.warn(failure, "ForgeHarness") return end
  if not text then return end
  vim.cmd.stopinsert()
  set_composer_text(state.composer_buf, text, attachments)
  if state.composer_win and vim.api.nvim_win_is_valid(state.composer_win) then
    layout.resize_composer(state.composer_buf, state.composer_win)
    vim.api.nvim_set_current_win(state.composer_win)
  end
  M.refresh_winbar()
  M.report_recovered_edit(recovered)
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
    if model_id == "default" then model_id = state.session.resolved_model or model_id end
    for _, model in ipairs(model_list or {}) do
      if model.id == model_id or (model_id == "default" and model.is_default) then
        local model_options = require("forge.views.harness.model_options")
        local choice = model_options.reasoning_choices(model)
        if #choice == 0 then return end
        local resolved = model_options.thinking(model, current)
        local index = 1
        for candidate_index, candidate in ipairs(choice) do if candidate == resolved then index = candidate_index end end
        index = math.max(1, math.min(#choice, index + direction))
        M.configure({ effort = choice[index] })
        return
      end
    end
  end)
end

function M.select_effort()
  if harness_state().capability.effort_selection ~= true then
    harness_state().configuration_error = "The current provider does not support reasoning effort selection"
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
  local loading = state.model_provider ~= (state.session and state.session.provider) or type(state.model_list) ~= "table"
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
    if model_id == "default" then model_id = state.session.resolved_model or model_id end
    local choice = {}
    for _, model in ipairs(model_list or {}) do
      if model.id == model_id or (model_id == "default" and model.is_default) then
        choice = require("forge.views.harness.model_options").reasoning_choices(model)
        break
      end
    end
    local options = vim.tbl_map(function(effort)
      return { label = effort, detail = detail_list[effort] or "Provider-advertised reasoning choice", value = effort }
    end, choice)
    open_choice_picker(state, "Select Reasoning Effort", nil, options, function(effort)
      M.configure({ effort = effort })
    end, { empty_text = "The selected model advertises no reasoning choices." })
  end)
end

---@param tier "default"|"fast"|"ultrafast"
function M.configure_service_tier(tier)
  local state = harness_state()
  if (tier == "fast" and state.capability.fast_mode ~= true)
    or (tier == "ultrafast" and state.capability.ultrafast_mode ~= true)
  then
    state.configuration_error = "The current provider does not support " .. tier .. " mode"
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
  if state.model_provider == state.session.provider and state.model_list then apply(state.model_list)
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
  local provider = state.session and state.session.provider or config.options.harness.provider
  local session_id = state.session and state.session.id
  local loading_owner = "harness-model-loading-" .. tostring(session_id)
  local loading = state.model_provider ~= provider or type(state.model_list) ~= "table"
  local function is_current()
    return harness_state() == state and state.session and state.session.id == session_id
      and state.session.provider == provider
      and (not loading or picker.is_open(loading_owner))
  end
  if state.capability.model_selection ~= true then
    notifications.warn("The current provider does not support model selection", "ForgeHarness")
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
    local state = harness_state()
    model_picker.open({
      host = picker_host(state),
      model_list = model_list,
      provider = provider,
      is_current = is_current,
      current_model = selected_setting(state, "model") == "default" and state.session.resolved_model
        or selected_setting(state, "model"),
      on_confirm = on_confirm,
    })
  end
  model_pins.refresh(provider, function(_, pin_error)
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
      open_model_picker(model_list or {})
    end)
  end)
end

---Use future defaults when the provider declares that it cannot edit an open binding.
local function resource_defaults_only(state, capability)
  local integration = state.capability.integration or {}
  return vim.iter(integration.limitations or {}):any(function(limitation)
    return limitation.id == capability and limitation.support == "unsupported"
  end)
end

function M.open_skill_picker()
  local state = harness_state()
  local session_id = state.session and state.session.id
  if not (state.capability.catalog and state.capability.catalog.skill) then
    notifications.warn("The current provider does not advertise provider skills", "Harness skills")
    return
  end
  provider_picker.open_skills({
    host = picker_host(state),
    is_current = function() return harness_state() == state and (state.session and state.session.id) == session_id end,
    future_defaults = resource_defaults_only(state, "skill_mutation"),
    workspace = state.session and state.session.workspace,
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
    notifications.warn("The current provider does not advertise MCP management", "Harness MCP")
    return
  end
  provider_picker.open_mcp({
    host = picker_host(state),
    is_current = owns_session,
    future_defaults = resource_defaults_only(state, "mcp_mutation"),
    workspace = state.session and state.session.workspace,
  })
end

function M.select_provider()
  local state = harness_state()
  local harness = require("forge.views.harness")
  local refusal = harness.provider_switch_refusal()
  if refusal then
    notifications.warn(refusal, "Harness provider")
    return
  end
  local source_session_id = state.session.id
  local function current_source()
    return session.harness == state and state.session and state.session.id == source_session_id
  end
  local current = state.session and state.session.provider or config.options.harness.provider
  local option_list = {}
  local provider_map = client.provider_map()
  for _, provider in ipairs(require("forge.views.harness.model_options").providers()) do
    local provider_config = provider_map[provider]
    option_list[#option_list + 1] = {
      id = provider,
      label = provider_config.label,
      detail = (provider_config.detail or provider_config.label) .. (provider == current and " (current)" or ""),
      value = provider,
    }
  end
  table.sort(option_list, function(left, right) return left.label < right.label end)
  open_choice_picker(state, "Select Harness", nil, option_list,
    function(provider)
      if provider == current or not current_source() then return end
      local function destination_picker()
        if not current_source() then return end
        open_choice_picker(state, "Select Chat", client.provider_map()[provider].label, {
          { id = "new", label = "New chat", value = "new" },
          { id = "resume", label = "Resume session…", value = "resume" },
        }, function(destination)
          if not current_source() then return end
          if destination == "new" then
            harness.switch_provider(provider, { kind = "new" })
          else
            require("forge.views.harness.session_picker").open(picker_host(state), {
              provider = provider,
              on_select = function(entry)
                if current_source() then harness.switch_provider(provider, { kind = "resume", session_id = entry.id }) end
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
  -- Warm the catalog on attach; the first runtime observation replaces workspace discovery.
  require("forge.views.harness.provider_catalog").models(true, function() end)
end

---@param action table
---@param submitted_text? string
---@param completed? fun(success: boolean)
---@param method? string
function M.task_transition(action, submitted_text, completed, method)
  local state = harness_state()
  -- Task requests flatten unknown fields into metadata, which resume and plan-review capture
  -- reject, so only a submission that actually selected attachments carries the field.
  if submitted_text then
    local selected = attachment.capture(state.composer_buf).attachments
    if #selected > 0 then action.attachments = selected end
  end
  if state.host_error then notifications.error(state.execution_notice or state.host_error, "Harness task") return end
  if not state.session then M.report_uninitialized(state, "Harness task") return end
  local restore_draft
  local buffer = state.composer_buf
  if submitted_text and composer_text(buffer) == submitted_text then
    dismiss_composer_completion(buffer)
    restore_draft = require("forge.views.harness.composer_draft").take(buffer,
      vim.api.nvim_buf_get_lines(buffer, 0, -1, false), attachment.capture(buffer).attachments,
      function() return harness_state() == state and state.composer_buf == buffer end)
  end
  task_control.transition(state, action, M.render, function(failure, _, detail)
    -- A task change that committed before its dispatch failed consumed the submission; the host
    -- reports the failure, and offering the text again would invite a second, duplicate change.
    local committed = detail and detail.code == "task_change_committed_dispatch_failed"
    if failure and restore_draft and not committed then restore_draft() end
    if completed then completed(not failure, failure) end
    if harness_state() == state then M.render() end
  end, method)
end

function M.toggle_write()
  M.task_transition({ action = "permission", change = { kind = "toggle_write_enabled" } })
end

---@param next_config table
---@param completed? fun(applied: boolean)
configure_now = function(next_config, completed)
  local state = harness_state()
  local session_id, generation = state.session.id, client.host_get_generation()
  -- The host answers once the change is applied, after any running turn stopped and restarted on
  -- it. Until then the winbar shows the change, with the waiting marker when a turn holds it back.
  -- The host applies changes in order, so a change sent behind a waiting one waits too. An idle
  -- change shows no marker even while the host's owner reads `working` to validate it.
  local waiting = activity.turn_pending(state)
  for _, earlier in ipairs(state.configuration_requests or {}) do
    if earlier.waiting and earlier.session_id == session_id and earlier.generation == generation then waiting = true end
  end
  local request = { change = vim.deepcopy(next_config), waiting = waiting,
    session_id = session_id, generation = generation }
  state.configuration_requests = state.configuration_requests or {}
  table.insert(state.configuration_requests, request)
  M.refresh_winbar()
  client.request({ session_id = session_id, method = "session.configure", params = vim.deepcopy(next_config) }, function(_, failure)
    for index, candidate in ipairs(state.configuration_requests or {}) do
      if candidate == request then table.remove(state.configuration_requests, index) break end
    end
    if not state.session or state.session.id ~= session_id or client.host_get_generation() ~= generation then
      if harness_state() == state then M.refresh_winbar() end
      if completed then completed(false) end
      return
    end
    -- The winbar rejection describes the latest settings attempt, so an accepted change retires it.
    if failure then report_configuration_error(failure) else state.configuration_error = nil end
    if completed then completed(not failure) end
    if harness_state() == state then M.render() end
  end)
end

---@param next_config table
---@param completed? fun(applied: boolean)
function M.configure(next_config, completed)
  configure_now(next_config, completed)
end

local function close()
  local state = harness_state()
  queue_editor.remember(state, composer_text(state.composer_buf))
  local workspace = require("forge.views.harness.workspace")
  workspace.release(state)
  if state.presentation then
    local ok, closed = pcall(state.presentation.close, { preserve_buffer = true })
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
---@param options? {present_questions?: boolean}
function M.activate_snapshot(result, options)
  local state = harness_state()
  local previous_session = state.session and state.session.id
  if not snapshot.apply(state, result) then
    state.initial_snapshot_pending = nil
    M.render()
    return
  end
  if state.input and state.input.session_id == state.session.id then input_state.refresh(state) end
  state.initial_snapshot_pending = nil
  if previous_session ~= state.session.id then state.queue, state.queue_edit = {}, nil end
  M.render()
  -- A previous host may have stopped mid-offer, leaving its outbox blocked behind an
  -- uncertain publication. Only the `session.state.get` reconciliation clears that
  -- block, so every attach reconciles instead of waiting for an event that cannot arrive.
  synchronize_state()
  M.resolve_runtime_model()
  if not options or options.present_questions ~= false then M.present_held_prompts() end
end

---Let a user retire a message whose delivery the host cannot confirm.
local function recover_unknown_input(state, entry)
  local owner, id, revision = state.input, entry.id, entry.revision
  local choices = {}
  if not entry.local_only then
    choices[#choices + 1] = { label = "Reconcile delivery",
      detail = "Retry only when delivery evidence confirms the provider did not receive the message", value = "reconcile" }
  end
  choices[#choices + 1] = { label = "Discard queue entry", detail = "Release the queue without sending this text again", value = "discard" }
  open_choice_picker(state, "Unconfirmed input", entry.error, choices, function(choice)
    if not choice then return end
    local current = owner and owner.entry_by_id[id]
    if state.input ~= owner or not current or current.revision ~= revision
      or (current.draft_state ~= "unknown" and current.delivery ~= "unknown") then
      notifications.warn("Input delivery changed. Check the current queue.", "Harness message")
      return
    end
    if choice == "reconcile" then input_state.retry(state, current)
    elseif current.local_only then input_state.discard_rejected(state, id)
    else input_state.perform(state, { kind = "remove", id = id, revision = revision }) end
  end, { empty_text = "No recovery choices" })
end

---Open the question or approval the selected session's snapshot holds, once its view is on screen.
function M.present_held_prompts()
  local state = harness_state()
  if state.active_elicitation and state.active_elicitation.elicitation then
    state.presented_question_key = nil
    vim.schedule(M.present_plan_question)
  end
  if #(state.approval or {}) > 0 then vim.schedule(function() M.present_approval(false, state) end) end
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
  -- Keys that read the draft wait for any paste that preceded them in the terminal stream.
  -- The queue control is captured at the key, so a send pressed before an interrupt that overtakes
  -- its deferred step still follows the pause the user saw rather than that interrupt.
  local function after_paste(action)
    return require("forge.views.harness.composer_draft").after_pending_paste(function()
      return harness_state().composer_buf
    end, function(origin)
      local state = harness_state()
      if state ~= origin.state or state.composer_buf ~= origin.buffer
        or (state.session and state.session.id) ~= origin.session_id or state.runtime_epoch ~= origin.runtime_epoch then
        notifications.warn("The Harness composer changed before the action ran. The draft was kept.", "ForgeHarness")
        return
      end
      input_state.under_fence(state, origin.fence, action)
    end, function()
      local state = harness_state()
      return { state = state, buffer = state.composer_buf, session_id = state.session and state.session.id,
        runtime_epoch = state.runtime_epoch, fence = input_state.control_fence(state) }
    end)
  end
  command_set.register(set, "edit_queued", after_paste(function() M.edit_queued(-1) end))
  command_set.register(set, "next_queued", after_paste(function() M.edit_queued(1) end))
  command_set.register(set, "submit", after_paste(M.submit))
  command_set.register(set, "queue", after_paste(M.queue_submit))
  command_set.register(set, "steer", after_paste(M.steer_submit))
  command_set.register(set, "retry_input", function()
    local state = harness_state()
    if state.queue_edit then
      notifications.error("Finish editing the queued message before retrying", "Harness message")
      return
    end
    for _, entry in ipairs(state.queue or {}) do
      if M.retries(entry) then
        if entry.draft_state == "unknown" or entry.delivery == "unknown" then recover_unknown_input(state, entry)
        else input_state.retry(state, entry) end
        return
      end
    end
    -- An interrupt pauses the queue; with no failed message to retry, the key resumes it.
    if state.input and state.input.suspended and #(state.queue or {}) > 0 then input_state.resume(state) end
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
    schedule_render()
    if queue_editor.delivered(state) then
      vim.schedule(function() M.with_state(state, function() M.settle_delivered_edit(state) end) end)
    end
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
  if client.host_is_accepting() then state.host_error = nil end
  state.command_set = M.command_set()
  M.attach_transcript(state.transcript_buf)
  keymaps.setup_view_keymaps(state.composer_buf, "harness", M.composer_command_set())
  require("forge.views.harness.composer_interrupt").attach(composer_buffer,
    function() return harness_state() == state and state.composer_buf == composer_buffer end,
    function() M.cancel_turn() end)
  require("forge.views.harness.workspace").attach(state)
  prompt_history.attach(state.composer_buf)
  if state.unsubscribe then state.unsubscribe() end
  state.unsubscribe = client.session_events_subscribe(function(event, payload, event_session_id)
    if state.switching_provider then return end
    if state.session and event_session_id and state.session.id ~= event_session_id then return end
    local active_state = session.harness
    session.activate_harness(state)
    local ok, failure = xpcall(function() on_event(event, payload) end, debug.traceback)
    if not ok then
      diagnostic.set(state, "sync_error", "Presentation failed: " .. tostring(failure),
        { owner = state.session and state.session.id or "presentation", category = "internal", severity = "error", recovery = "reconnect" })
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
