local M = {}

local client = require("diff_review.harness.client")
local command_set = require("diff_review.shared.view_command_set")
local config = require("diff_review.infra.config")
local keymaps = require("diff_review.shared.keymaps")
local notifications = require("diff_review.infra.notifications")
local renderer = require("diff_review.render.harness.interaction_tree")
local markdown = require("diff_review.render.harness.markdown")
local queue_renderer = require("diff_review.render.harness.queue")
local render_transaction = require("diff_review.render.harness.transaction")
local layout = require("diff_review.views.harness.layout")
local session = require("diff_review.session")
local interaction_state = require("diff_review.views.harness.interaction_state")

local namespace = vim.api.nvim_create_namespace("DiffReviewHarnessTranscript")
local queue_namespace = vim.api.nvim_create_namespace("DiffReviewHarnessQueue")
local choice_picker = {
  main = { current = false },
  layout = {
    preset = "select",
    preview = false,
    layout = { relative = "editor", row = 2 },
  },
}
local unsubscribe = nil
local state_sync_pending = false
local working_timer = nil
local working_started_ns = nil
local render_observer_for_test = nil

---@return table
local function harness_state() return session.harness end

local function render_queue()
  local state = harness_state()
  local buf = state.composer_buf
  local win = state.composer_win
  if not (buf and vim.api.nvim_buf_is_valid(buf) and win and vim.api.nvim_win_is_valid(win)) then return end
  vim.api.nvim_buf_clear_namespace(buf, queue_namespace, 0, -1)
  local virtual_line_list, row_count = queue_renderer.build(state.queue, vim.api.nvim_win_get_width(win))
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

---@return { text: string, group: string }[]
local function status_text()
  local state = harness_state()
  local active_session = state.session or {}
  local raw_mode = state.pending_mode or active_session.write_mode or "read"
  local mode = raw_mode:sub(1, 1):upper() .. raw_mode:sub(2):lower()
  if state.pending_mode then mode = mode .. "*" end
  local backend = active_session.backend or config.options.harness.backend
  local provider = active_session.provider_label
    or (backend == "codex" and "Codex CLI")
    or (backend == "acp" and "ACP agent")
    or backend
  local configured_model = active_session.model or config.options.harness.model
  local model = active_session.resolved_model or (configured_model == "default" and "resolving model" or configured_model)
  local effort = active_session.effort or config.options.harness.effort
  if state.pending_config and state.pending_config.model then model = state.pending_config.model .. "*" end
  if state.pending_config and state.pending_config.effort then effort = state.pending_config.effort .. "*" end
  local busy = state.busy and " • running" or (#state.queue > 0 and (" • queued " .. #state.queue) or "")
  local goal = nil
  if state.goal and state.goal.objective and state.goal.state ~= "cleared" then
    local goal_state = state.goal.state and state.goal.state ~= "active" and (" [" .. state.goal.state .. "]") or ""
    goal = " • Goal: " .. state.goal.objective .. goal_state
  end
  local permission_note = backend == "acp" and " • ACP permission best effort" or ""
  local fast = active_session.fast_mode and " fast" or ""
  local segment_list = {
    {
      text = mode,
      group = raw_mode == "write" and "DiffReviewHarnessWrite" or "DiffReviewHarnessRead",
    },
    {
      text = (" • %s • %s %s%s"):format(provider, model, effort, fast),
      group = "DiffReviewStatusLabel",
    },
  }
  if goal then
    segment_list[#segment_list + 1] = { text = goal, group = "DiffReviewHarnessGoal" }
  end
  if busy ~= "" or permission_note ~= "" then
    segment_list[#segment_list + 1] = { text = busy .. permission_note, group = "DiffReviewStatusLabel" }
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
    status_text()
  )
  if state.composer_win and vim.api.nvim_win_is_valid(state.composer_win) then
    vim.wo[state.composer_win].winbar = ""
  end
  render_queue()
end

---@return integer?
local function working_seconds()
  if not harness_state().busy or not working_started_ns then return nil end
  return math.floor((vim.uv.hrtime() - working_started_ns + 500000000) / 1000000000)
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
  local render = renderer.build(state.interaction, {
    working_seconds = working_seconds(),
    content_width = transcript_visible and vim.api.nvim_win_get_width(transcript_win) or nil,
    cwd = vim.uv.cwd(),
    on_diff_update = function() vim.schedule(M.render) end,
    expanded = state.activity_expanded,
    plan_progress = state.plan_progress,
    active_interaction_id = state.pending_interaction and state.pending_interaction.id
      or (state.interaction[#state.interaction] and state.interaction[#state.interaction].id),
  })
  state.prompt_line = render.prompt_lines
  state.activity_range = render.folds
  state.render_namespace = namespace
  local transaction = render_transaction.apply(state, render, {
    reset = reset == true,
    follow_tail = follow_tail,
  })
  markdown.render(buf, transcript_win, render.markdown_ranges)
  if render_observer_for_test then
    render_observer_for_test(vim.deepcopy(render.lines), vim.deepcopy(transaction))
  end
  M.refresh_winbar()
end

local function schedule_render()
  if vim.in_fast_event() then
    vim.schedule(M.render)
  else
    M.render()
  end
end

---@param busy boolean
local function set_busy(busy)
  local state = harness_state()
  state.busy = busy
  if busy then
    if working_started_ns then return end
    working_started_ns = vim.uv.hrtime()
    working_timer = vim.uv.new_timer()
    working_timer:start(1000, 1000, function()
      vim.schedule(function()
        if harness_state().busy then schedule_render() end
      end)
    end)
  else
    working_started_ns = nil
    if working_timer then
      working_timer:stop()
      working_timer:close()
      working_timer = nil
    end
  end
  schedule_render()
end

local function synchronize_state()
  if state_sync_pending then return end
  state_sync_pending = true
  client.request("state.get", {}, function(result, request_error)
    state_sync_pending = false
    if request_error or not result then return end
    local state = harness_state()
    state.session = result.session
    interaction_state.replace(state, result.interaction or {})
    state.capability = result.capability or {}
    state.no_checkpoint = result.no_checkpoint == true
    state.goal = result.goal
    state.active_plan = result.active_plan
    M.render(true)
  end)
end

---@param event string
---@param payload table
local function on_event(event, payload)
  local state = harness_state()
  if event == "backend_event" then
    if payload.kind == "timeline_active" then
      interaction_state.apply_active(state, payload.data or payload)
      schedule_render()
    elseif payload.kind == "timeline_thought_completed" then
      interaction_state.complete_thought(state, payload.data or payload)
      schedule_render()
    elseif payload.kind == "plan" and type(payload.plan_progress) == "table" then
      state.plan_progress = vim.deepcopy(payload.plan_progress)
      schedule_render()
    elseif payload.kind == "error" and type(payload.text) == "string" then
      notifications.warn(payload.text, "Harness")
    end
  elseif event == "plan_review" then
    state.active_plan = payload
    require("diff_review.views.plan_review").open(payload)
  elseif event == "plan_accepted" then
    state.active_plan = payload
    M.refresh_winbar()
  elseif event == "plan_cancelled" then
    state.active_plan = nil
    M.refresh_winbar()
  elseif event == "goal_changed" or event == "goal_continue_requested" then
    state.goal = payload.state == "cleared" and nil or payload
    M.refresh_winbar()
    if event == "goal_continue_requested" then vim.schedule(M.drain) end
  elseif event == "session_changed" or event == "session_configured" or event == "mode_changed" then
    local next_session = payload.session or payload
    if event == "session_changed" and state.session and next_session.id ~= state.session.id then
      interaction_state.replace(state, payload.interaction or {})
      state.queue = {}
      state.goal = nil
      state.active_plan = nil
      state.plan_progress = nil
    end
    state.session = next_session
    M.refresh_winbar()
  elseif event == "interaction_rolled_back" then
    synchronize_state()
  elseif event == "interaction_complete" or event == "interaction_updated" then
    interaction_state.complete_interaction(state, payload.interaction or payload)
    M.render()
  elseif event == "state_invalidated" then
    synchronize_state()
  end
end

---@param text string
local function begin_request(text)
  local state = harness_state()
  set_busy(true)
  local goal_objective = text:match("^/goal%s+(.+)$")
  if goal_objective and goal_objective ~= "pause" and goal_objective ~= "resume" and goal_objective ~= "clear" then
    state.goal = { objective = goal_objective, state = "active" }
  end
  interaction_state.begin(state, text)
  state.plan_progress = nil
  M.render()
  client.request("prompt.submit", { text = text }, function(result, request_error)
    set_busy(false)
    if request_error then
      interaction_state.fail_pending(state, request_error)
      notifications.error(request_error, "Harness")
      M.render()
      return
    elseif result then
      state.session = result.session or (result.id and result) or state.session
      state.capability = result.capability or state.capability
      if result.interaction then interaction_state.complete_interaction(state, result.interaction) end
    end
    M.render()
    vim.schedule(M.drain)
  end)
end

function M.drain()
  local state = harness_state()
  if state.busy then return end
  if state.active_plan and state.active_plan.state == "awaiting_review" then return end
  if state.pending_mode then
    local pending_mode = state.pending_mode
    state.pending_mode = nil
    M.set_mode(pending_mode)
    return
  end
  if state.pending_config then
    local pending_config = state.pending_config
    state.pending_config = nil
    M.configure(pending_config)
    return
  end
  local text = table.remove(state.queue, 1)
  if text then
    begin_request(text)
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

function M.submit()
  local state = harness_state()
  local text = composer_text(state.composer_buf)
  if text == "" then return end
  if text == "/write" or text == "/read" then
    set_composer_text(state.composer_buf, "")
    M.set_mode(text:sub(2))
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
  if text == "/rename" then
    set_composer_text(state.composer_buf, "")
    M.rename_session("")
    return
  end
  local session_name = text:match("^/rename%s+(.+)$")
  if session_name then
    set_composer_text(state.composer_buf, "")
    M.rename_session(vim.trim(session_name))
    return
  end
  local model = text:match("^/model%s+(.+)$")
  if model then
    set_composer_text(state.composer_buf, "")
    M.configure({ model = vim.trim(model) })
    return
  end
  local effort = text:match("^/effort%s+(%S+)$")
  if effort then
    set_composer_text(state.composer_buf, "")
    if not vim.tbl_contains({ "minimal", "low", "medium", "high", "xhigh" }, effort) then
      notifications.warn("Unknown reasoning effort: " .. effort, "Harness")
      return
    end
    M.configure({ effort = effort })
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
    state.queue[#state.queue + 1] = text
    M.refresh_winbar()
    return
  end
  begin_request(text)
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
    vim.cmd("startinsert")
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
  state.activity_expanded[expand_key] = not state.activity_expanded[expand_key]
  M.render()
end

---@param direction integer
function M.change_effort(direction)
  local state = harness_state()
  local effort_list = { "minimal", "low", "medium", "high", "xhigh" }
  local current = state.session and state.session.effort or config.options.harness.effort
  local index = 3
  for candidate_index, candidate in ipairs(effort_list) do if candidate == current then index = candidate_index end end
  index = math.max(1, math.min(#effort_list, index + direction))
  M.configure({ effort = effort_list[index] })
end

function M.select_effort()
  local effort_list = { "minimal", "low", "medium", "high", "xhigh" }
  vim.ui.select(effort_list, { prompt = "Harness reasoning effort", snacks = choice_picker }, function(effort)
    if effort then M.configure({ effort = effort }) end
  end)
end

---@param enabled boolean
function M.configure_fast_mode(enabled)
  local state = harness_state()
  local backend = state.session and state.session.backend or config.options.harness.backend
  if backend ~= "codex" then
    notifications.warn("Fast mode requires the Codex backend", "Harness")
    return
  end
  M.configure({ fast_mode = enabled })
end

function M.select_fast_mode()
  local state = harness_state()
  local enabled = state.session and state.session.fast_mode == true
  vim.ui.select({ enabled and "Disable fast mode" or "Enable fast mode", "Cancel" }, {
    prompt = "Codex fast mode",
    snacks = choice_picker,
  }, function(choice)
    if choice == "Enable fast mode" then M.configure_fast_mode(true) end
    if choice == "Disable fast mode" then M.configure_fast_mode(false) end
  end)
end

function M.select_model()
  client.request("backend.models", {}, function(model_list, request_error)
    if request_error then notifications.error(request_error, "Harness model") return end
    if type(model_list) ~= "table" or #model_list == 0 then
      local current = harness_state().session and harness_state().session.model or config.options.harness.model
      vim.ui.input({ prompt = "Harness model: ", default = current }, function(model)
        if model and vim.trim(model) ~= "" then M.configure({ model = vim.trim(model) }) end
      end)
      return
    end
    vim.ui.select(model_list, {
      prompt = "Harness model",
      snacks = choice_picker,
      format_item = function(model) return model.label .. "  " .. model.id end,
    }, function(model)
      if model then M.configure({ model = model.id }) end
    end)
  end)
end

function M.resolve_runtime_model()
  client.request("backend.models", {}, function(_, request_error)
    if request_error then notifications.error("Failed to resolve Harness model: " .. request_error, "Harness") end
  end)
end

---@param mode "read"|"write"
function M.set_mode(mode)
  local state = harness_state()
  local function apply_mode()
    if state.busy then
      state.pending_mode = mode
      M.refresh_winbar()
      notifications.info("Harness mode will change after the active request", "Harness")
      return
    end
    client.request("mode.set", { mode = mode }, function(result, request_error)
      if request_error then
        notifications.error("Failed to change Harness mode: " .. request_error, "Harness")
        M.refresh_winbar()
        return
      end
      state.session = result or state.session
      M.refresh_winbar()
      vim.schedule(M.drain)
    end)
  end

  if mode == "write" and state.no_checkpoint and config.options.harness.non_git_write_confirm then
    vim.ui.select({ "Cancel", "Enable Write without checkpoints" }, {
      prompt = "This is not a Git worktree. Harness cannot diff or roll back writes.",
      snacks = choice_picker,
    }, function(choice)
      if choice == "Enable Write without checkpoints" then apply_mode() end
    end)
    return
  end
  apply_mode()
end

function M.toggle_mode()
  local state = harness_state()
  local current_mode = state.pending_mode or (state.session and state.session.write_mode) or "read"
  M.set_mode(current_mode == "read" and "write" or "read")
end

---@param next_config table
function M.configure(next_config)
  local state = harness_state()
  if state.busy then
    state.pending_config = vim.tbl_extend("force", state.pending_config or {}, next_config)
    M.refresh_winbar()
    notifications.info("Harness configuration will apply at the next safe boundary", "Harness")
    return
  end
  client.request("session.configure", next_config, function(result, request_error)
    if request_error then notifications.error(request_error, "Harness") return end
    state.session = result
    M.refresh_winbar()
    if next_config.model and not result.resolved_model then M.resolve_runtime_model() end
    vim.schedule(M.drain)
  end)
end

local function close()
  local state = harness_state()
  local tab_count = vim.fn.tabpagenr("$")
  if tab_count > 1 then vim.cmd("tabclose") else vim.cmd("enew") end
  state.transcript_win = nil
  state.composer_win = nil
end

---@return DiffReviewViewCommandSet
function M.command_set()
  local set = command_set.new()
  command_set.register(set, "submit", M.submit)
  command_set.register(set, "edit_queued", M.edit_last_queued)
  command_set.register(set, "toggle_mode", M.toggle_mode)
  command_set.register(set, "previous_prompt", function() M.jump_prompt(-1) end)
  command_set.register(set, "next_prompt", function() M.jump_prompt(1) end)
  command_set.register(set, "toggle_activity", M.toggle_activity)
  command_set.register(set, "model", M.select_model)
  command_set.register(set, "effort_down", function() M.change_effort(-1) end)
  command_set.register(set, "effort_up", function() M.change_effort(1) end)
  command_set.register(set, "close", close)
  command_set.register(set, "help", function() keymaps.show_view_help("harness", set, "Harness") end)
  return set
end

function M.attach()
  local state = harness_state()
  state.command_set = M.command_set()
  keymaps.setup_view_keymaps(state.transcript_buf, "harness", state.command_set)
  local composer_command_set = M.command_set()
  command_set.unregister(composer_command_set, "close")
  keymaps.setup_view_keymaps(state.composer_buf, "harness", composer_command_set)
  if unsubscribe then unsubscribe() end
  unsubscribe = client.subscribe(on_event)
end

---@param observer? fun(lines: string[])
function M._set_render_observer_for_test(observer)
  render_observer_for_test = observer
end

return M
