local M = {}

local client = require("forge.client")
local layout = require("forge.views.harness.layout")
local notifications = require("forge.infra.notifications")
local session = require("forge.session")

local group = vim.api.nvim_create_augroup("ForgeHarnessSessionNavigation", { clear = false })
local pending_sequence = 0

---@type table<string, ForgeHarnessPresentationState>
local state_by_session_id = {}
---@type table<integer, ForgeHarnessPresentationState>
local state_by_buffer = {}

local function controller() return require("forge.views.harness.controller") end

---@param state ForgeHarnessPresentationState
local function valid_view(state)
  if state and not state.timeline_tab and state.transcript_win and vim.api.nvim_win_is_valid(state.transcript_win) then
    state.timeline_tab = vim.api.nvim_win_get_tabpage(state.transcript_win)
  end
  return state
    and state.timeline_tab
    and vim.api.nvim_tabpage_is_valid(state.timeline_tab)
    and state.transcript_buf
    and vim.api.nvim_buf_is_valid(state.transcript_buf)
end

---@param state ForgeHarnessPresentationState
local function activate_state(state)
  session.activate_harness(state)
end

---@param state ForgeHarnessPresentationState
local function focus_state(state)
  if not valid_view(state) then return false end
  vim.api.nvim_set_current_tabpage(state.timeline_tab)
  local target = state.composer_win
  if target and vim.api.nvim_win_is_valid(target) then vim.api.nvim_set_current_win(target) end
  activate_state(state)
  return true
end

---@param state ForgeHarnessPresentationState
local function attach_activation(state)
  for _, buf in ipairs({ state.transcript_buf, state.composer_buf }) do
    state_by_buffer[buf] = state
    vim.api.nvim_clear_autocmds({ group = group, buffer = buf })
    vim.api.nvim_create_autocmd({ "BufEnter", "WinEnter" }, {
      group = group,
      buffer = buf,
      callback = function() activate_state(state) end,
      desc = "Activate the Harness state owned by this timeline tab",
    })
  end
end

---@param timeline_key string
---@return ForgeHarnessPresentationState
local function create_view(timeline_key)
  local state = session.new_harness_state()
  state.transcript_buf, state.transcript_win, state.composer_buf, state.composer_win, state.timeline_tab =
    layout.open(timeline_key)
  layout.attach_auto_height(state.composer_buf, state.composer_win)
  layout.attach_scroll_boundary(state.transcript_buf, state.transcript_win)
  attach_activation(state)
  activate_state(state)
  return state
end

---@param state ForgeHarnessPresentationState
---@param session_id string
local function register(state, session_id)
  local previous_session_id = state.session and state.session.id
  if previous_session_id and state_by_session_id[previous_session_id] == state then
    state_by_session_id[previous_session_id] = nil
    session.harness_by_id[previous_session_id] = nil
  end
  state_by_session_id[session_id] = state
  session.register_harness(session_id, state)
  layout.rename_session_buffers(state.transcript_buf, state.composer_buf, session_id)
end

---@class ForgeHarnessSessionActivation
---@field state? ForgeHarnessPresentationState
---@field open_mode? "current"|"tab"
---@field interaction_mode? "reconcile"

---@param result table
---@param options? ForgeHarnessSessionActivation
function M.activate(result, options)
  options = options or {}
  local next_session = result and result.session
  if not (next_session and next_session.id) then
    notifications.error("Harness session activation returned no session", "ForgeHarness")
    return
  end
  local existing = state_by_session_id[next_session.id]
  local source_state = session.harness
  local state = options.state
  if existing and valid_view(existing) and existing ~= state then state = existing end
  if not state then
    state = options.open_mode == "current" and source_state or create_view("session-pending-" .. next_session.id)
  end
  attach_activation(state)
  register(state, next_session.id)
  activate_state(state)
  controller().activate_snapshot(result)
  controller().attach()
  controller().attach_transcript(state.transcript_buf)
  controller().refresh_winbar()
  focus_state(state)

  if options.open_mode == "current"
    and source_state ~= state
    and source_state.timeline_tab
    and vim.api.nvim_tabpage_is_valid(source_state.timeline_tab)
  then
    vim.api.nvim_tabpage_close(source_state.timeline_tab, false)
  end
end

---@class ForgeHarnessPendingSession
---@field state ForgeHarnessPresentationState
---@field source_session_id string
---@field source_session_name string
---@field child_name? string
---@field kind "fork"|"new"
---@field error? string

---@param source_session table
---@param child_name? string
---@return ForgeHarnessPendingSession
function M.begin_fork(source_session, child_name)
  local source_state = session.harness
  if source_session.id and state_by_session_id[source_session.id] ~= source_state then
    register(source_state, source_session.id)
  end
  if source_state.transcript_win and vim.api.nvim_win_is_valid(source_state.transcript_win) then
    vim.api.nvim_win_call(source_state.transcript_win, function() vim.cmd("normal! m'") end)
  end
  pending_sequence = pending_sequence + 1
  local pending = {
    state = create_view("fork-pending-" .. pending_sequence),
    source_session_id = source_session.id,
    source_session_name = source_session.name or "",
    child_name = child_name,
    kind = "fork",
  }
  M.render_pending(pending)
  return pending
end

---@param name? string
---@return ForgeHarnessPendingSession
function M.begin_new(name)
  pending_sequence = pending_sequence + 1
  local source_session_id = session.harness.session and session.harness.session.id or ""
  local state = create_view("new-pending-" .. pending_sequence)
  local pending = {
    state = state,
    source_session_id = source_session_id,
    source_session_name = "",
    child_name = name,
    kind = "new",
  }
  M.render_pending(pending)
  return pending
end

---@param pending ForgeHarnessPendingSession
function M.render_pending(pending)
  local state = pending.state
  local buf = state and state.transcript_buf
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  local line
  if pending.error then
    line = (pending.kind == "fork" and "Fork failed: " or "New session failed: ") .. pending.error
  elseif pending.kind == "new" then
    line = pending.child_name and pending.child_name ~= "" and ("Creating " .. pending.child_name .. "...")
      or "Creating a new session..."
  else
    local source = pending.source_session_id
    if pending.source_session_name ~= "" then source = source .. " (" .. pending.source_session_name .. ")" end
    local action = pending.child_name and pending.child_name ~= "" and ("Forking " .. pending.child_name .. " from ")
      or "Forking from "
    line = action .. source .. "..."
  end
  line = line:gsub("[\r\n]+", " ")
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, { "", "  " .. line })
  vim.bo[buf].modifiable = false
  vim.bo[buf].modified = false
end

---@param pending ForgeHarnessPendingSession
---@param result table
function M.complete_pending(pending, result)
  M.activate(result, { state = pending.state })
end

M.complete_fork = M.complete_pending

---@param pending ForgeHarnessPendingSession
---@param message string
function M.fail_fork(pending, message)
  pending.error = message
  M.render_pending(pending)
  notifications.error(message, "Harness fork")
end

---@param session_id string
---@param options? ForgeHarnessSessionActivation
function M.resume(session_id, options)
  local existing = state_by_session_id[session_id] or session.harness_by_id[session_id]
  if existing and focus_state(existing) then return end
  client.request("session.resume", { session_id = session_id }, function(result, request_error)
    if request_error then
      notifications.error(request_error, "Harness session")
      return
    end
    M.activate(result, options)
  end)
end

---@param session_id string
function M.open_parent(session_id)
  local state = session.harness
  if state.transcript_win and vim.api.nvim_win_is_valid(state.transcript_win) then
    vim.api.nvim_win_call(state.transcript_win, function() vim.cmd("normal! m'") end)
  end
  local parent_state = state_by_session_id[session_id] or session.harness_by_id[session_id]
  if parent_state then
    if valid_view(parent_state) then
      vim.api.nvim_set_current_tabpage(parent_state.timeline_tab)
      if parent_state.composer_win and vim.api.nvim_win_is_valid(parent_state.composer_win) then
        vim.api.nvim_set_current_win(parent_state.composer_win)
      end
    end
    activate_state(parent_state)
    return
  end
  M.resume(session_id, { open_mode = "tab" })
end

---@return table<string, ForgeHarnessPresentationState>, table<integer, ForgeHarnessPresentationState>
function M._registry_for_test()
  return state_by_session_id, state_by_buffer
end

return M
