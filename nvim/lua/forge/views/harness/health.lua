local M = {}
local client = require("forge.client")
local diagnostic = require("forge.views.harness.diagnostic")

---@class HarnessStatusNotice
---@field text string
---@field animated boolean
---@field failed boolean?
---@field waiting boolean?
---@field hint string?
---@param state table
---@return HarnessStatusNotice?
function M.notice(state)
  local notice = state.presentation and diagnostic.get(state.presentation, "failure", "error")
    or diagnostic.get(state, "host_error", "error")
    or diagnostic.get(state, "queue_notice", "info")
    or diagnostic.get(state, "connection_error", "info")
    or diagnostic.get(state, "sync_error", "info")
    or state.presentation and diagnostic.get(state.presentation, "sync_failure", "info")
  if notice then return { text = notice.text, animated = notice.animated, failed = notice.failed, waiting = notice.waiting } end
  if not state.status or state.status.kind ~= "idle" or state.checks_running or state.queue_paused
    or state.task_operation or state.active_elicitation or state.aborting_plan then return end
  local submission = state.busy and state.prompt_submission
  local entry = submission and submission.queue_entry or (state.queue or {})[1]
  if not entry or state.queue_edit and state.queue_edit.barrier == entry then return end
  if entry.error or entry.reservation or state.input and state.input.suspended then return end
  return { text = "Working", animated = true, hint = "working" }
end

---@param state table
---@param refresh fun()
function M.watch(state, refresh)
  if not state.session then return end
  local identity = {}
  local generation = client.host_get_generation()
  local session_id = state.session.id
  state.health_owner = identity
  local last_response = vim.uv.now()
  local pending = false
  local connection_notice = "Connection unresponsive — task status unknown"
  local function current()
    return state.health_owner == identity and client.host_get_generation() == generation
      and not state.host_error and state.session and state.session.id == session_id
  end
  local function tick()
    if not current() then return end
    if vim.uv.now() - last_response >= 10000 then
      if not state.connection_error then state.connection_error = connection_notice refresh() end
    end
    if not pending then
      pending = true
      client.request({ session_id = session_id, method = "session.health.get", params = {} }, function(_, failure)
        if not current() then return end
        pending = false
        if not failure then
          last_response = vim.uv.now()
          if state.connection_error == connection_notice then state.connection_error = nil refresh() end
        end
      end)
    end
    vim.defer_fn(tick, 2000)
  end
  vim.defer_fn(tick, 2000)
end

return M
