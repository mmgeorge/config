local M = {}

local client = require("diff_review.harness.client")
local layout = require("diff_review.views.harness.layout")
local notifications = require("diff_review.infra.notifications")
local session = require("diff_review.session")

local group = vim.api.nvim_create_augroup("DiffReviewHarnessSessionNavigation", { clear = false })

---@type table<string, integer>
local buffer_by_session_id = {}
---@type table<integer, string>
local session_id_by_buffer = {}
local activation_session_id = nil
local suppress_buffer_enter = false
local pending_fork_sequence = 0
---@type table<integer, DiffReviewHarnessPendingFork>
local pending_fork_by_buffer = {}

---@class DiffReviewHarnessPendingFork
---@field buffer integer
---@field source_session_id string
---@field source_session_name string
---@field child_name? string
---@field error? string

local function controller() return require("diff_review.views.harness.controller") end

---@param win integer
---@param buf integer
local function switch_transcript_buffer(win, buf)
  suppress_buffer_enter = true
  vim.api.nvim_win_call(win, function()
    vim.cmd("buffer " .. tostring(buf))
    layout.configure_transcript_window(win)
  end)
  suppress_buffer_enter = false
end

---@param buf integer
---@param session_id string
function M.register(buf, session_id)
  buffer_by_session_id[session_id] = buf
  session_id_by_buffer[buf] = session_id
  vim.api.nvim_clear_autocmds({ group = group, buffer = buf })
  vim.api.nvim_create_autocmd("BufEnter", {
    group = group,
    buffer = buf,
    callback = function()
      if suppress_buffer_enter then return end
      local state = session.harness
      if state.session and state.session.id == session_id then
        state.transcript_buf = buf
        return
      end
      M.resume(session_id, { buffer = buf, switch_buffer = false })
    end,
    desc = "Activate the Harness session owned by this timeline",
  })
end

---@param session_id string
---@return integer
local function resolve_buffer(session_id)
  local buf = buffer_by_session_id[session_id]
  if buf and vim.api.nvim_buf_is_valid(buf) then return buf end
  buf = layout.create_transcript_buffer(session_id)
  M.register(buf, session_id)
  return buf
end

---@class DiffReviewHarnessSessionActivation
---@field buffer? integer
---@field switch_buffer? boolean
---@field interaction_mode? "reconcile"

---@param result table
---@param options? DiffReviewHarnessSessionActivation
function M.activate(result, options)
  options = options or {}
  local state = session.harness
  local next_session = result and result.session
  if not (next_session and next_session.id) then
    notifications.error("Harness session activation returned no session", "Harness")
    return
  end
  local buf = options.buffer or resolve_buffer(next_session.id)
  local promoted_pending_fork = pending_fork_by_buffer[buf] ~= nil
  pending_fork_by_buffer[buf] = nil
  if promoted_pending_fork then layout.rename_transcript_buffer(buf, next_session.id) end
  M.register(buf, next_session.id)
  if options.switch_buffer ~= false and state.transcript_win and vim.api.nvim_win_is_valid(state.transcript_win) then
    switch_transcript_buffer(state.transcript_win, buf)
  end
  state.transcript_buf = buf
  layout.attach_scroll_boundary(buf, state.transcript_win)
  controller().activate_snapshot(result, options.interaction_mode)
  controller().attach_transcript(buf)
end

---@param source_session table
---@param child_name? string
---@return DiffReviewHarnessPendingFork
function M.begin_fork(source_session, child_name)
  local state = session.harness
  pending_fork_sequence = pending_fork_sequence + 1
  local buf = layout.create_transcript_buffer("fork-pending-" .. pending_fork_sequence)
  local pending = {
    buffer = buf,
    source_session_id = source_session.id,
    source_session_name = source_session.name or "",
    child_name = child_name,
  }
  pending_fork_by_buffer[buf] = pending
  local win = state.transcript_win
  if win and vim.api.nvim_win_is_valid(win) then
    vim.api.nvim_win_call(win, function() vim.cmd("normal! m'") end)
    switch_transcript_buffer(win, buf)
  end
  state.transcript_buf = buf
  layout.attach_scroll_boundary(buf, win)
  M.render_pending(buf)
  controller().attach_transcript(buf)
  controller().refresh_winbar()
  return pending
end

---@param buf integer
---@return boolean
function M.render_pending(buf)
  local pending = pending_fork_by_buffer[buf]
  if not pending or not vim.api.nvim_buf_is_valid(buf) then return false end
  local source = pending.source_session_id
  if pending.source_session_name ~= "" then source = source .. " (" .. pending.source_session_name .. ")" end
  local action = pending.child_name and pending.child_name ~= "" and ("Forking " .. pending.child_name .. " from ")
    or "Forking from "
  local line = pending.error and ("Fork failed: " .. pending.error) or (action .. source .. "...")
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, { "", "  " .. line })
  vim.bo[buf].modifiable = false
  vim.bo[buf].modified = false
  return true
end

---@param pending DiffReviewHarnessPendingFork
---@param result table
function M.complete_fork(pending, result)
  local buf = pending.buffer
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then
    M.activate(result)
    return
  end
  M.activate(result, { buffer = buf })
end

---@param pending DiffReviewHarnessPendingFork
---@param message string
function M.fail_fork(pending, message)
  pending.error = message
  M.render_pending(pending.buffer)
  controller().refresh_winbar()
  notifications.error(message, "Harness fork")
end

---@param session_id string
---@param options? DiffReviewHarnessSessionActivation
function M.resume(session_id, options)
  if activation_session_id then return end
  activation_session_id = session_id
  client.request("session.resume", { session_id = session_id }, function(result, request_error)
    activation_session_id = nil
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
  local win = state.transcript_win
  if not (win and vim.api.nvim_win_is_valid(win)) then return end
  vim.api.nvim_win_call(win, function() vim.cmd("normal! m'") end)
  M.resume(session_id)
end

---@return table<string, integer>, table<integer, string>
function M._registry_for_test()
  return buffer_by_session_id, session_id_by_buffer
end

return M
